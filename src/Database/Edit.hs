{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Database.Edit
Description : In-memory database editing primitives (copy, delete-by-selection)

Shared home for operations that produce a *new* loaded database from an
existing one without touching disk.

COPY is construction, not mutation. 'Database' is a pure, immutable value
(see "Types"), so duplicating it is just sharing the same persistent
structure under a second registry key: nothing observable can alias because
nothing is mutable. The only fresh allocation is the solver (it holds an
'MVar' factorization cache keyed by name); reusing the source solver would
let the two registry entries share a factorization cache, so the copy gets
its own.

DELETE is reconstruction, not mutation either. 'deleteActivities' drops a set
of activities and rebuilds every dependent structure (interning tables,
indexes, sparse matrices, product index) from the surviving activities. The
rebuild reuses the exact pure builders that back a freshly-loaded database
('buildInterningTables' / 'buildTechTriples' / 'buildBioTriples' /
'buildProductIndex' / 'buildIndexesWithProcessIds'), so a deleted-from
database is byte-for-byte indistinguishable from one that never carried the
removed rows. Exchanges in surviving activities that referenced a deleted
activity are UNLINKED (their activity link reset to nil), leaving the value
ready for relinking — never silently dropped.

Memory cost (copy): the copy keeps the source 'Database' alive for as long as
it is loaded. Structural sharing means we don't re-allocate the activity/flow
vectors, but a large database that would otherwise be unloaded stays resident
while any copy of it is loaded.
-}
module Database.Edit (
    copyDatabase,
    deleteActivities,
    deleteActivitiesWith,
    resolveDeleteSelection,
    DeleteSelection (..),
    DeleteRequest (..),
    DeleteOutcome (..),
    deleteActivitiesInDB,
    insertActivities,
    replaceActivities,
    MutationOutcome (..),
    mutateUploadedDatabase,
    WriteVerb (..),
    WriteRefusal (..),
    WriteReport (..),
    writeActivities,
    refusalMessage,
) where

import Control.Concurrent.STM (atomically, modifyTVar', readTVar, readTVarIO)
import Control.Exception (SomeException, finally, try)
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BL
import qualified Data.IntSet as IS
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import System.Directory (
    createDirectoryIfMissing,
    doesDirectoryExist,
    removePathForcibly,
    renameDirectory,
 )
import System.FilePath (makeRelative, splitDirectories, takeDirectory, (</>))

import Config (DatabaseConfig (..))
import Database (
    applyStructuredFilters,
    buildIndexesWithProcessIds,
    buildProductIndex,
    findActivitiesByFields,
 )
import Database.Author (
    AuthorContext (..),
    AuthoredActivity,
    ResolvedInsert (..),
    validateAuthored,
 )
import Database.CrossLinking (buildIndexedDatabaseFromDB)
import Database.Export (serializeDatabaseFiles)
import qualified Database.Loader as Loader
import Database.Manager (
    DatabaseManager (..),
    LoadedDatabase (..),
    clearMethodMappingCacheForDb,
    getDatabase,
    getMergedSynonymDB,
    getMergedUnitConfig,
    relinkDatabase,
 )
import Database.MatrixBuild (
    InterningTables (..),
    buildBioTriples,
    buildInterningTables,
    buildSupplierRefUnits,
    buildTechTriples,
    collectBioFlowOrder,
 )
import Database.Upload (DatabaseFormat (..), slugify)
import qualified Database.UploadedDatabase as UploadedDB
import Matrix (clearCachedSolver)
import qualified Search.BM25 as BM25
import Service (bm25Retrieve)
import SharedSolver (createSharedSolver)
import Types (
    Activity (..),
    BiosphereFlow (..),
    Database (..),
    Exchange (..),
    ProcessId,
    SparseTriple (..),
    TechnosphereFlow (..),
    UUID,
    findProcessId,
    findProcessIdByActivityUUID,
    initializeRuntimeFields,
    parseUUIDPair,
 )
import UnitConversion (UnitConfig, defaultUnitConfig)

{- | Copy a loaded database into the runtime registry under the slugified
@newName@.

Looks up the loaded source, builds an independent 'LoadedDatabase' (renamed
config + fresh solver — 'Database' is immutable, so the value itself is shared
safely) and inserts it into the loaded / available / indexed maps.

@newName@ is slugified to the same charset as uploaded databases: the copy is
registered as uploaded (see 'renameConfig'), and uploaded databases are later
deleted by name via 'removeDirectoryRecursive', so an unsanitised name (e.g.
@"../x"@ or @""@) would let the eventual delete escape the uploads directory.

Fails (Left) when the source is not loaded, when @newName@ slugifies to empty,
or when the name already designates a loaded, configured, or in-flight
database — a copy must never silently overwrite an existing entry. The name is
reserved atomically (in 'dmStagingDbs') across the slow solver build, so two
concurrent copies of the same name cannot both pass the existence check.
-}
copyDatabase :: DatabaseManager -> Text -> Text -> IO (Either Text ())
copyDatabase manager srcName newName = do
    let slug = slugify newName
    if T.null slug
        then pure $ Left $ "Invalid copy name (no usable characters): " <> newName
        else
            getDatabase manager srcName >>= \case
                Nothing -> pure $ Left $ "Database not loaded: " <> srcName
                Just src -> do
                    reserved <- atomically $ do
                        loadedDbs <- readTVar (dmLoadedDbs manager)
                        availableDbs <- readTVar (dmAvailableDbs manager)
                        stagingDbs <- readTVar (dmStagingDbs manager)
                        if M.member slug loadedDbs || M.member slug availableDbs || S.member slug stagingDbs
                            then pure (Left ("Database already exists: " <> slug))
                            else Right () <$ modifyTVar' (dmStagingDbs manager) (S.insert slug)
                    case reserved of
                        Left err -> pure (Left err)
                        Right () ->
                            finally
                                (registerCopy manager slug src)
                                (atomically $ modifyTVar' (dmStagingDbs manager) (S.delete slug))

{- | Build the copy's solver/index and insert it under @slug@. Caller holds the
'dmStagingDbs' reservation for @slug@.
-}
registerCopy :: DatabaseManager -> Text -> LoadedDatabase -> IO (Either Text ())
registerCopy manager slug src = do
    let copiedDb = ldDatabase src
        newConfig = renameConfig slug (ldConfig src)
        -- Fresh solver: a distinct name keys a distinct factorization cache.
        techTriplesInt =
            [ (fromIntegral i, fromIntegral j, v)
            | SparseTriple i j v <- U.toList (dbTechnosphereTriples copiedDb)
            ]
    solver <-
        createSharedSolver
            slug
            techTriplesInt
            (fromIntegral (dbActivityCount copiedDb))
    synonymDB <- getMergedSynonymDB manager
    let copied =
            LoadedDatabase
                { ldDatabase = copiedDb
                , ldSharedSolver = solver
                , ldConfig = newConfig
                }
        indexedDb = buildIndexedDatabaseFromDB slug synonymDB copiedDb
    atomically $ do
        modifyTVar' (dmLoadedDbs manager) (M.insert slug copied)
        modifyTVar' (dmAvailableDbs manager) (M.insert slug newConfig)
        modifyTVar' (dmIndexedDbs manager) (M.insert slug indexedDb)
    clearMethodMappingCacheForDb manager slug
    pure $ Right ()

{- | Rename a config for the copy: new internal name, derived display name, and
forced deletable/uploaded so the copy can be removed again via the normal
delete path (the source may be a TOML-pinned, non-deletable database).
-}
renameConfig :: Text -> DatabaseConfig -> DatabaseConfig
renameConfig newName cfg =
    cfg
        { dcName = newName
        , dcDisplayName = newName
        , dcIsUploaded = True
        , dcDeletable = True
        }

-- ---------------------------------------------------------------------------
-- Delete by selection
-- ---------------------------------------------------------------------------

{- | Remove a set of activities (by 'ProcessId') and rebuild every dependent
structure. Uses 'defaultUnitConfig'; effectful call sites that carry a merged
unit config (the only place where non-SI conversions matter) should call
'deleteActivitiesWith' instead so a coefficient is never silently dropped on
an unknown-unit conversion.
-}
deleteActivities :: [ProcessId] -> Database -> Either Text Database
deleteActivities = deleteActivitiesWith defaultUnitConfig

{- | 'deleteActivities' with an explicit 'UnitConfig'.

Steps, all pure:

  1. Resolve the delete set to the @(activityUUID, productUUID)@ keys it
     occupies in 'dbProcessIdTable', validating that every requested
     'ProcessId' exists (no silent skip).
  2. Drop those keys; for every surviving activity, UNLINK any technosphere /
     waste exchange whose @(activityLink, flow)@ pointed at a deleted key —
     reset its activity link to 'UUID.nil' and clear the stale process link.
  3. Rebuild interning tables, indexes, matrices and the product index from
     the surviving activity map via the shared loader builders.

Fails (Left) when an out-of-range 'ProcessId' is requested, when the result
would be empty, or when matrix construction reports an inconsistency (e.g. an
unknown unit conversion under the given config).
-}
deleteActivitiesWith :: UnitConfig -> [ProcessId] -> Database -> Either Text Database
deleteActivitiesWith unitConfig pids db = do
    deletedKeys <- resolveDeleteKeys db pids
    let survivors = surviving db deletedKeys
        survivingKeys = M.keysSet survivors
        unlinkedMap = M.map (unlinkActivity survivingKeys) survivors
    if M.null unlinkedMap
        then Left "Refusing to delete: the result would have no activities"
        else rebuildFromActivities unitConfig db unlinkedMap

{- | Resolve each requested 'ProcessId' to its @(activityUUID, productUUID)@ key.
Every id must be in range — an out-of-range id is a caller error, surfaced as
'Left' rather than silently ignored. The result is a 'Set' so membership tests
during unlinking are @O(log n)@.
-}
resolveDeleteKeys :: Database -> [ProcessId] -> Either Text (S.Set (UUID, UUID))
resolveDeleteKeys db = fmap S.fromList . traverse keyOf
  where
    table = dbProcessIdTable db
    n = V.length table
    keyOf pid
        | i >= 0 && i < n = Right (table V.! i)
        | otherwise = Left ("Delete: ProcessId out of range: " <> T.pack (show pid))
      where
        i = fromIntegral pid

-- | The activity map keyed by @(activityUUID, productUUID)@, minus the deleted keys.
surviving :: Database -> S.Set (UUID, UUID) -> M.Map (UUID, UUID) Activity
surviving db deletedKeys =
    M.fromList
        [ (key, dbActivities db V.! i)
        | i <- [0 .. V.length (dbActivities db) - 1]
        , let key = dbProcessIdTable db V.! i
        , not (S.member key deletedKeys)
        ]

{- | Reset technosphere / waste exchanges on a surviving activity whose producer
link no longer resolves to a surviving @(activityUUID, productUUID)@ key.

The link target is exactly that pair: 'findProducer' resolves
@(activityLink, flowId)@ against the interning table, so a multi-product
activity that keeps at least one product stays a valid target for exchanges
pointing at a *surviving* product, while exchanges pointing at a deleted
product are unlinked. A biosphere exchange has no producer link and is
returned unchanged. The stale 'ProcessId' link is always cleared because
deletion renumbers every 'ProcessId'. An already-orphan link
(@activityLink == nil@) stays orphan.
-}
unlinkActivity :: S.Set (UUID, UUID) -> Activity -> Activity
unlinkActivity survivingKeys act =
    act{exchanges = map unlinkExchange (exchanges act)}
  where
    dangling link flow = link /= UUID.nil && not (S.member (link, flow) survivingKeys)
    unlinkExchange ex = case ex of
        BiosphereExchange{} -> ex
        TechnosphereExchange{techActivityLinkId = link, techFlowId = flow}
            | dangling link flow ->
                ex{techActivityLinkId = UUID.nil, techProcessLinkId = Nothing}
            | otherwise -> ex{techProcessLinkId = Nothing}
        WasteExchange{waActivityLinkId = link, waFlowId = flow}
            | dangling link flow ->
                ex{waActivityLinkId = UUID.nil, waProcessLinkId = Nothing}
            | otherwise -> ex{waProcessLinkId = Nothing}

{- | Rebuild a 'Database' from a surviving activity map, reusing the exact pure
builders that back a freshly-loaded database. Flow / unit tables are carried
over unchanged: deletion removes activities, never the flow or unit
vocabulary, so a relink can still resolve the surviving links. Runtime fields
(synonym, flow-name, BM25 indexes) are reset to their unloaded state; the
effectful caller re-attaches them with the merged synonym DB.
-}
rebuildFromActivities :: UnitConfig -> Database -> M.Map (UUID, UUID) Activity -> Either Text Database
rebuildFromActivities unitConfig db activityMap =
    let tables = buildInterningTables activityMap
        supplierRefUnits = buildSupplierRefUnits (dbUnits db) (itActivities tables)
        indexes = buildIndexesWithProcessIds (itActivities tables) (itProcessIdTable tables)
        bioFlowUUIDs = collectBioFlowOrder (itActivities tables)
        bioTriples = buildBioTriples bioFlowUUIDs tables
        productIndex = buildProductIndex (itActivities tables) (itProcessIdTable tables) (dbTechFlows db)
     in case buildTechTriples unitConfig (dbUnits db) tables supplierRefUnits of
            Left err -> Left err
            Right (techTriples, _warnings) ->
                Right
                    db
                        { dbProcessIdTable = itProcessIdTable tables
                        , dbProcessIdLookup = itProcessIdLookup tables
                        , dbActivityUUIDIndex = itActivityUUIDIndex tables
                        , dbActivityProductsIndex = itActivityProductsIndex tables
                        , dbProductIndex = productIndex
                        , dbActivities = itActivities tables
                        , dbIndexes = indexes
                        , dbTechnosphereTriples = techTriples
                        , dbBiosphereTriples = bioTriples
                        , dbActivityIndex = V.generate (fromIntegral (itActivityCount tables)) fromIntegral
                        , dbBiosphereOrder = bioFlowUUIDs
                        , dbActivityCount = itActivityCount tables
                        , dbBiosphereCount = fromIntegral (V.length bioFlowUUIDs)
                        , -- Cross-DB links may now reference deleted activities; clear so a
                          -- subsequent relink rebuilds them against the surviving set.
                          dbCrossDBLinks = []
                        , -- Linking stats describe the pre-delete set (input count, completeness,
                          -- missing suppliers); reset to the fresh-load default so the setup/status
                          -- endpoint never reports stale numbers until the next relink.
                          dbLinkingStats = mempty
                        , -- Runtime-only indexes are reset; re-attached by the effectful caller.
                          dbSynonymDB = Nothing
                        , dbFlowsByName = M.empty
                        , dbFlowsByCAS = M.empty
                        , dbProductSearchIndex = M.empty
                        , dbBM25Index = Nothing
                        }

-- ---------------------------------------------------------------------------
-- Insert / replace
-- ---------------------------------------------------------------------------

{- | Whether a batch means to add rows the database does not have, or to
rewrite rows it does. There is deliberately no upsert: a mistyped identity
must fail, not quietly become a second copy of an activity the author thought
they were correcting.
-}
data WriteIntent = Insert | Replace

{- | Add authored activities to a database and rebuild everything that depends
on them.

Every key must be absent — 'Database.Author' mints identity from the activity's
own name, location, product and unit, so a key that already exists means the
author is re-describing something the database already holds and wants
'replaceActivities' instead.
-}
insertActivities :: UnitConfig -> [ResolvedInsert] -> Database -> Either Text Database
insertActivities = applyResolved Insert

{- | Rewrite activities the database already holds, keeping their identity.

Every key must be present. Replacing keeps the old version's flows in the
vocabulary, exactly as deletion does: a flow no activity uses any more is
inert, and dropping it would break a relink that still resolves through it.
-}
replaceActivities :: UnitConfig -> [ResolvedInsert] -> Database -> Either Text Database
replaceActivities = applyResolved Replace

{- | Shared write path. The vocabulary the batch brings (its product flows and
any new biosphere flows) lands in the same step as the activities that
reference it, so no intermediate value exists in which an exchange points at a
flow the database cannot name.

An empty batch returns the database untouched rather than rebuilding: a
rebuild resets cross-database links ('rebuildFromActivities'), so treating "no
activities to write" as a no-op is what keeps a caller's empty request from
silently unlinking the database.
-}
applyResolved :: WriteIntent -> UnitConfig -> [ResolvedInsert] -> Database -> Either Text Database
applyResolved intent unitConfig inserts db
    | null inserts = Right db
    | otherwise = do
        let existing = surviving db S.empty
        checkKeys intent existing (map riKey inserts)
        rebuildFromActivities
            unitConfig
            db
                { dbTechFlows = M.union (indexBy tfId (concatMap riNewTechFlows inserts)) (dbTechFlows db)
                , dbBioFlows = M.union (indexBy bfId (concatMap riNewBioFlows inserts)) (dbBioFlows db)
                }
            (M.union (M.fromList [(riKey i, riActivity i) | i <- inserts]) existing)
  where
    indexBy key xs = M.fromList [(key x, x) | x <- xs]

{- | Refuse a batch whose keys contradict the intent, naming every offending
identity at once so a long batch is not fixed one round-trip at a time.
-}
checkKeys :: WriteIntent -> M.Map (UUID, UUID) Activity -> [(UUID, UUID)] -> Either Text ()
checkKeys intent existing keys = case intent of
    Insert -> refuse "already exists" (filter (`M.member` existing) keys)
    Replace -> refuse "does not exist" (filter (not . (`M.member` existing)) keys)
  where
    refuse _ [] = Right ()
    refuse what offenders =
        Left $
            "Refusing to write: "
                <> T.intercalate ", " (map renderKey offenders)
                <> " "
                <> what
                <> " in this database"

-- ---------------------------------------------------------------------------
-- Writing authored activities
-- ---------------------------------------------------------------------------

{- | Which way a batch is meant to land: added to the collection, or written
over the one process the caller names.
-}
data WriteVerb = CreateActivities | ReplaceActivity Text

{- | Why a write did not happen, in the terms its caller has to answer in.

Each constructor exists because a different response is owed: an HTTP client
needs 404 apart from 409 apart from 400, and someone at a terminal needs to be
told which of "there is no such database", "that one is not yours to write"
and "your file has four problems" happened. A single error string would force
both to guess by reading it.
-}
data WriteRefusal
    = NotLoaded Text
    | NotWritable Text
    | Malformed [Text]
    | AlreadyPresent [Text]
    | NotPresent [Text]
    | WriteFailed Text

-- | What a write produced.
data WriteReport = WriteReport
    { wrWritten :: [Text]
    , wrPersisted :: Bool
    , wrWarnings :: [Text]
    }

{- | Validate a batch of authored activities against a loaded database and, if
nothing is wrong with it, write it.

This is the whole of authoring above the primitives, shared by the HTTP
endpoints and the command line so the two cannot drift on what is allowed.
Refusals are classified rather than stringly-typed, because the two callers
owe their users different answers to the same refusal.

A database the engine reads from its configuration is refused: that is
background data the whole installation shares, and a copy or an upload is
where authoring belongs.
-}
writeActivities ::
    DatabaseManager ->
    Text ->
    WriteVerb ->
    [AuthoredActivity] ->
    IO (Either WriteRefusal WriteReport)
writeActivities _ _ _ [] =
    -- Committing re-serializes the whole database and rebuilds its solver;
    -- an empty batch would pay all of that to write nothing.
    pure (Left (Malformed ["The batch is empty: there is nothing to write."]))
writeActivities manager dbName verb authored =
    getDatabase manager dbName >>= \case
        Nothing -> pure (Left (NotLoaded dbName))
        Just loaded
            | not (dcIsUploaded (ldConfig loaded)) -> pure (Left (NotWritable dbName))
            | otherwise -> do
                deps <- loadedDependencies manager (ldDatabase loaded)
                unitConfig <- getMergedUnitConfig manager
                let ctx =
                        AuthorContext
                            { acDb = ldDatabase loaded
                            , acDeps = deps
                            , acUnitConfig = unitConfig
                            }
                case validateAuthored ctx authored of
                    Left errs -> pure (Left (Malformed errs))
                    Right (resolved, warnings) ->
                        case presenceRefusal (ldDatabase loaded) verb resolved of
                            Just refusal -> pure (Left refusal)
                            Nothing -> commit deps unitConfig resolved warnings
  where
    commit deps unitConfig resolved warnings = do
        outcome <- mutateUploadedDatabase manager dbName (edit deps unitConfig)
        pure $ case outcome of
            Left err -> Left (WriteFailed err)
            Right done ->
                Right
                    WriteReport
                        { wrWritten = map (renderKey . riKey) resolved
                        , wrPersisted = moPersisted done
                        , wrWarnings = warnings <> moWarnings done
                        }
    -- Everything above judged a snapshot taken before the staging
    -- reservation; 'mutateReserved' re-reads the database under it. The edit
    -- therefore validates again against what is actually there, so a batch
    -- overtaken by a concurrent edit is refused rather than written with a
    -- supplier link that no longer resolves. In that rare interleaving the
    -- refusal degrades from a classified status to a 'WriteFailed' message —
    -- never to a dangling link. Identity minting is pure, so the keys cannot
    -- differ between the two runs.
    edit deps unitConfig db = do
        let ctx = AuthorContext{acDb = db, acDeps = deps, acUnitConfig = unitConfig}
        (resolved, _) <- first (T.intercalate "\n") (validateAuthored ctx authored)
        case verb of
            CreateActivities -> insertActivities unitConfig resolved db
            ReplaceActivity _ -> replaceActivities unitConfig resolved db

{- | The two refusals only a verb can name: creating over a process that is
already there, and rewriting one that is not. Checked before the mutation so
the caller learns which happened rather than reading it out of a message.

'ReplaceActivity' also checks that the body describes the process the caller
addressed. Identity is minted from the name, location, product and unit, so a
body that mints elsewhere would silently become a second row.
-}
presenceRefusal :: Database -> WriteVerb -> [ResolvedInsert] -> Maybe WriteRefusal
presenceRefusal db verb resolved = case verb of
    CreateActivities -> case filter present keys of
        [] -> Nothing
        clashes -> Just (AlreadyPresent (map renderKey clashes))
    -- The mismatch is checked before presence on purpose: a body that mints
    -- elsewhere describes a process the database may well not have, and
    -- answering "no such activity" would send the author looking for the wrong
    -- mistake.
    ReplaceActivity target -> case filter ((/= canonicalTarget db target) . renderKey) keys of
        elsewhere@(_ : _) ->
            Just . Malformed $
                [ "This activity's identity is "
                    <> renderKey wrong
                    <> ", not "
                    <> target
                    <> ". Identity comes from the name, location, product and unit, so\
                       \ writing this body here would address a different activity."
                | wrong <- elsewhere
                ]
        [] -> case filter (not . present) keys of
            [] -> Nothing
            absent -> Just (NotPresent (map renderKey absent))
  where
    keys = map riKey resolved
    present key = M.member key (dbProcessIdLookup db)

{- | The PUT target in the currency 'renderKey' speaks. A process is addressed
by the canonical @activityUUID_productUUID@ pair, or by the bare activity UUID
the read endpoints also accept, so the handle a caller got from a read works
here too. A target that resolves to nothing is kept as sent: the presence
check owns that refusal.
-}
canonicalTarget :: Database -> Text -> Text
canonicalTarget db target = case parseUUIDPair target of
    Just pair -> renderKey pair
    Nothing -> fromMaybe target $ do
        actUUID <- UUID.fromText target
        pid <- findProcessIdByActivityUUID db actUUID
        renderKey <$> dbProcessIdTable db V.!? fromIntegral pid

-- | The loaded databases a database draws suppliers from.
loadedDependencies :: DatabaseManager -> Database -> IO [Database]
loadedDependencies manager db = do
    loadedDbs <- readTVarIO (dmLoadedDbs manager)
    pure [ldDatabase ld | name <- dbDependsOn db, Just ld <- [M.lookup name loadedDbs]]

-- | @activityUUID_productUUID@, the identity a process is addressed by.
renderKey :: (UUID, UUID) -> Text
renderKey (actUUID, prodUUID) = UUID.toText actUUID <> "_" <> UUID.toText prodUUID

{- | Plain-language rendering, for callers with nowhere to put a status code.
An HTTP caller maps the constructors to codes instead.
-}
refusalMessage :: WriteRefusal -> Text
refusalMessage = \case
    NotLoaded name -> "Database not loaded: " <> name
    NotWritable name ->
        name
            <> " is a database this engine reads from its configuration, so it is shared\
               \ background data rather than yours to write. Copy it, or upload a database\
               \ of your own, and author there."
    Malformed errs -> T.intercalate "\n" errs
    AlreadyPresent keys -> "Already in this database: " <> T.intercalate ", " keys
    NotPresent keys -> "Not in this database: " <> T.intercalate ", " keys
    WriteFailed err -> err

-- ---------------------------------------------------------------------------
-- Persisting an edit
-- ---------------------------------------------------------------------------

{- | What a mutation did.

'moPersisted' is the honest half: an edit to a configured (TOML) database
lives in memory only and is gone at the next unload, and the caller has to be
able to say so rather than let the user assume otherwise.
-}
data MutationOutcome = MutationOutcome
    { moPersisted :: Bool
    , moWarnings :: [Text]
    }

{- | Apply a pure edit to a loaded database, then commit it to memory and — for
a database that owns its files — to disk.

The sequence is prepare-then-commit, because everything that can fail must
fail before anything is visible:

  1. refuse while another loaded database depends on this one (a rebuild
     renumbers processes, and the dependent's cross-database links would
     resolve to the wrong rows or silently drop at solve time);
  2. run the pure edit;
  3. serialize the result and write it into a staging directory beside the
     live one — nothing observable yet;
  4. build the runtime indexes and a fresh solver for the edited value;
  5. commit: swing the staging directory into place and swap the registry;
  6. relink across dependencies and save the matrix cache, so an unload and
     reload gives back what was just written rather than the pre-edit sources.

Steps 1-4 leave both memory and disk untouched on failure. Step 5 is two
renames rather than one atomic operation; a crash between them leaves the
previous sources beside the new ones under a @.old@ suffix rather than
losing them.
-}
mutateUploadedDatabase ::
    DatabaseManager ->
    Text ->
    (Database -> Either Text Database) ->
    IO (Either Text MutationOutcome)
mutateUploadedDatabase manager dbName edit = do
    -- Two concurrent edits of the same database would share one staging
    -- directory and interleave the renames over the live sources. Reserve the
    -- name (the same reservation copy and setup staging use) and refuse the
    -- second edit rather than queue it: edits are interactive and rare.
    reserved <- atomically $ do
        staging <- readTVar (dmStagingDbs manager)
        if S.member dbName staging
            then pure False
            else True <$ modifyTVar' (dmStagingDbs manager) (S.insert dbName)
    if not reserved
        then pure $ Left $ "An edit of " <> dbName <> " is already in progress. Retry when it finishes."
        else
            finally
                (mutateReserved manager dbName edit)
                (atomically $ modifyTVar' (dmStagingDbs manager) (S.delete dbName))

-- | The mutation proper. The caller holds the 'dmStagingDbs' reservation.
mutateReserved ::
    DatabaseManager ->
    Text ->
    (Database -> Either Text Database) ->
    IO (Either Text MutationOutcome)
mutateReserved manager dbName edit =
    getDatabase manager dbName >>= \case
        Nothing -> pure $ Left $ "Database not loaded: " <> dbName
        Just loaded -> do
            loadedDbs <- readTVarIO (dmLoadedDbs manager)
            case guardDependents dbName loadedDbs *> edit (ldDatabase loaded) of
                Left err -> pure (Left err)
                Right edited -> do
                    prepared <- prepareSources dbName (ldConfig loaded) edited
                    case prepared of
                        Left err -> pure (Left err)
                        Right (staging, warnings) ->
                            Right <$> commitMutation manager dbName loaded edited staging warnings

{- | Refuse an edit while another loaded database depends on this one. Shared
with the delete path, which has the same reason: rebuilding renumbers every
process, and a dependent's cross-database links would then resolve elsewhere
or drop silently at solve time.
-}
guardDependents :: Text -> M.Map Text LoadedDatabase -> Either Text ()
guardDependents dbName loadedDbs = case dependents of
    [] -> Right ()
    names ->
        Left $
            "Cannot edit "
                <> dbName
                <> ": still required by "
                <> T.intercalate ", " names
                <> ". Unload dependents first."
  where
    dependents =
        [ name
        | (name, ld) <- M.toList loadedDbs
        , name /= dbName
        , dbName `elem` dbDependsOn (ldDatabase ld)
        ]

{- | Sources written and waiting to be swung into place, or 'Nothing' when the
database has no files of its own to write.
-}
data StagedSources = StagedSources
    { ssDataDir :: FilePath
    -- ^ where the live sources are, or will be
    , ssStaging :: FilePath
    -- ^ the fully-written replacement, still invisible
    , ssHome :: Maybe (FilePath, DatabaseFormat)
    {- ^ set when this write gives the database a home it did not have; the
    upload root whose @meta.toml@ has to be written on commit
    -}
    }

{- | Serialize the edited database and stage it beside its live sources.

Three cases, in the order they are decided:

  * a configured (TOML) database writes nothing — it is a background database
    the engine reads and never owns, so its edits stay in memory;
  * a database with its own upload directory is rewritten in its own format,
    and refused when that format cannot record process identity
    ('Database.Export.serializeDatabaseFiles' says which and why);
  * a copy has no files at all — 'copyDatabase' shares the source's value
    without duplicating its directory — so the first write gives it one. It
    gets EcoSpold 2, the format that survives the round trip. The source's
    own files are never touched: writing through the shared path would edit a
    database nobody asked to edit.
-}
prepareSources ::
    Text ->
    DatabaseConfig ->
    Database ->
    IO (Either Text (Maybe StagedSources, [Text]))
prepareSources dbName config edited
    | not (dcIsUploaded config) = pure (Right (Nothing, []))
    | otherwise = do
        uploadsDir <- UploadedDB.getDatabaseUploadsDir
        let home = uploadsDir </> T.unpack dbName
            -- Judged on path components, not on text: a textual prefix would
            -- make a database named "agri" the owner of "agribalyse"'s files,
            -- and its first save would rewrite them.
            ownsItsFiles = splitDirectories home `isPrefixOf` splitDirectories (dcPath config)
            dataDir = if ownsItsFiles then dcPath config else home </> "data"
            format = if ownsItsFiles then fromMaybe UnknownFormat (dcFormat config) else EcoSpold2
        case serializeDatabaseFiles format edited of
            Left err -> pure (Left err)
            Right (entries, warnings) -> do
                written <- writeStaging (dataDir <> ".new") entries
                pure $ case written of
                    Left err -> Left err
                    Right staging ->
                        Right
                            ( Just
                                StagedSources
                                    { ssDataDir = dataDir
                                    , ssStaging = staging
                                    , ssHome = if ownsItsFiles then Nothing else Just (home, format)
                                    }
                            , warnings
                            )

{- | Write every entry into a fresh staging directory, replacing any leftover
from an interrupted write. Returns the directory on success; on failure the
partial directory is removed, so a retry never inherits half of a previous
attempt.
-}
writeStaging :: FilePath -> [(FilePath, BL.ByteString)] -> IO (Either Text FilePath)
writeStaging staging entries = do
    attempt <- try $ do
        removePathForcibly staging
        createDirectoryIfMissing True staging
        mapM_ writeEntry entries
    case attempt of
        Right () -> pure (Right staging)
        Left err -> do
            removePathForcibly staging
            pure $ Left $ "could not write the database sources: " <> T.pack (show (err :: SomeException))
  where
    writeEntry (relPath, bytes) = do
        let full = staging </> relPath
        createDirectoryIfMissing True (takeDirectory full)
        BL.writeFile full bytes

{- | Swing the staged sources into place, keeping the previous ones until the
new ones are live.
-}
commitSources :: StagedSources -> IO ()
commitSources staged = do
    let live = ssDataDir staged
        previous = live <> ".old"
    hadSources <- doesDirectoryExist live
    removePathForcibly previous
    if hadSources
        then renameDirectory live previous
        else createDirectoryIfMissing True (takeDirectory live)
    renameDirectory (ssStaging staged) live
    removePathForcibly previous

{- | Install the edited database: commit its sources, swap it into the
registry behind a fresh solver, then relink and re-cache so a reload returns
what was written.
-}
commitMutation ::
    DatabaseManager ->
    Text ->
    LoadedDatabase ->
    Database ->
    Maybe StagedSources ->
    [Text] ->
    IO MutationOutcome
commitMutation manager dbName loaded edited staged warnings = do
    synonymDB <- getMergedSynonymDB manager
    let withRuntime = BM25.addBM25Index (initializeRuntimeFields edited synonymDB)
        techTriplesInt =
            [ (fromIntegral i, fromIntegral j, v)
            | SparseTriple i j v <- U.toList (dbTechnosphereTriples withRuntime)
            ]
    -- The solver cached under this name has the pre-edit dimensions; drain and
    -- destroy it before installing the rebuilt one, as the delete path does.
    clearCachedSolver dbName
    solver <- createSharedSolver dbName techTriplesInt (fromIntegral (dbActivityCount withRuntime))
    mapM_ commitSources staged
    config <- maybe (pure (ldConfig loaded)) (recordHome manager dbName (ldConfig loaded) edited) staged
    let loaded' = loaded{ldDatabase = withRuntime, ldSharedSolver = solver, ldConfig = config}
        indexedDb = buildIndexedDatabaseFromDB dbName synonymDB withRuntime
    atomically $ do
        modifyTVar' (dmLoadedDbs manager) (M.insert dbName loaded')
        modifyTVar' (dmIndexedDbs manager) (M.insert dbName indexedDb)
    clearMethodMappingCacheForDb manager dbName
    relinkWarnings <- case staged of
        -- The transient path must not relink: 'relinkDatabase' saves the
        -- matrix cache when links change, which would half-persist an edit
        -- the caller is being told is memory-only. Say what that costs.
        Nothing ->
            pure
                [ "the edit cleared the links to "
                    <> T.intercalate ", " (dbDependsOn edited)
                    <> "; they return at the next load, and cross-database totals undercount until then"
                | not (null (dbDependsOn edited))
                , null (dbCrossDBLinks edited)
                ]
        Just source -> do
            -- Rebuilding cleared the cross-database links; rebuild them against
            -- the current dependency set before the cache records the result.
            relinked <- relinkDatabase manager dbName
            saved <- getDatabase manager dbName
            mapM_ (Loader.saveCachedDatabaseWithMatrices dbName (ssDataDir source) . ldDatabase) saved
            pure (either (\err -> ["the edit is saved, but relinking failed: " <> err]) (const []) relinked)
    pure
        MutationOutcome
            { moPersisted = isJust staged
            , moWarnings = warnings <> relinkWarnings
            }

{- | Give a database that had no files of its own a @meta.toml@ describing the
home it just acquired, and point its config at the new sources.
-}
recordHome :: DatabaseManager -> Text -> DatabaseConfig -> Database -> StagedSources -> IO DatabaseConfig
recordHome manager dbName config edited staged = case ssHome staged of
    Nothing -> pure config
    Just (home, format) -> do
        UploadedDB.writeUploadMeta
            home
            UploadedDB.UploadMeta
                { UploadedDB.umVersion = 1
                , UploadedDB.umDisplayName = dcDisplayName config
                , UploadedDB.umDescription = dcDescription config
                , UploadedDB.umFormat = format
                , UploadedDB.umDataPath = makeRelative home (ssDataDir staged)
                , UploadedDB.umDepends = dbDependsOn edited
                }
        let config' = config{dcPath = ssDataDir staged, dcFormat = Just format}
        atomically $ modifyTVar' (dmAvailableDbs manager) (M.insert dbName config')
        pure config'

-- ---------------------------------------------------------------------------
-- Selection resolver
-- ---------------------------------------------------------------------------

{- | A deletion request: the whole set matched by a filter, plus explicit
adjustments. @dsKeep@ rescues individuals the filter matched (checkbox
unticked); @dsExtra@ adds individuals the filter missed (checkbox ticked on a
row outside the current filter). 'ProcessId' lists, never paginated.
-}
data DeleteSelection = DeleteSelection
    { dsFiltered :: [ProcessId]
    -- ^ the full filtered set (pagination ignored)
    , dsKeep :: [ProcessId]
    -- ^ individuals to spare from deletion
    , dsExtra :: [ProcessId]
    -- ^ individuals to add to deletion
    }

{- | Final delete set = (filtered ∪ extra) \\ keep. Deduplicated; @keep@ wins
over both @filtered@ and @extra@ so an unticked checkbox always spares a row.
Order is not significant — the result is consumed as a set by
'deleteActivities'.
-}
resolveDeleteSelection :: DeleteSelection -> [ProcessId]
resolveDeleteSelection sel =
    map fromIntegral . IS.toList $
        IS.difference
            (IS.union (toSet (dsFiltered sel)) (toSet (dsExtra sel)))
            (toSet (dsKeep sel))
  where
    toSet = IS.fromList . map fromIntegral

{- | Resolve a filter to the full set of matching 'ProcessId's, ignoring
pagination. Mirrors the set 'Service.searchActivities' displays so that the
UI's "delete the whole filtered set" button removes exactly the rows the user
saw — no more, no fewer.

A non-exact name filter therefore takes the BM25 OR-over-tokens retrieval
(via 'bm25Retrieve') followed by the structured filters, exactly as
'searchActivities' does on its BM25 branch. Using the AND-over-token-groups
name lookup (the lex-sort fallback path) here would silently under-delete a
multi-word @--name@: it returns a subset of the displayed set, so the count
would be reported too low. We fall back to the structured field lookup only
when there is no name filter, the match is exact, or the query tokenizes to
nothing (in which case 'bm25Retrieve' yields 'Nothing' and there is no
displayed BM25 set to honour). Order is irrelevant — the result is consumed
as a set.
-}
filteredProcessIds ::
    Database ->
    Maybe Text -> -- name
    Maybe Text -> -- location
    Maybe Text -> -- product
    [(Text, Text, Bool)] -> -- classification (system, value, isExact)
    Bool -> -- exact name match
    [ProcessId]
filteredProcessIds db nameP geoP prodP classFilters exactMatch =
    map fst $ case bm25Candidates of
        Just ranked -> applyStructuredFilters db geoP prodP classFilters False ranked
        Nothing -> findActivitiesByFields db nameP geoP prodP classFilters exactMatch
  where
    bm25Candidates = do
        name <- nameP
        if exactMatch || T.null (T.strip name) then Nothing else bm25Retrieve db name

-- ---------------------------------------------------------------------------
-- Effectful entry point (registry swap)
-- ---------------------------------------------------------------------------

{- | Resolve a process-id string to its 'ProcessId'. Accepts the canonical
@activityUUID_productUUID@ form and the bare-activity-UUID fallback (when the
activity has a unique reference product), mirroring
'Service.resolveActivityAndProcessId'. The UI and CLI carry these textual ids,
so keep/extra adjustments are expressed in the same currency as the rows the
user sees — not the volatile integer matrix index. Unresolvable ids fail loudly
rather than being silently dropped.
-}
resolvePid :: Database -> Text -> Either Text ProcessId
resolvePid db queryText =
    maybe (Left ("Unknown process id: " <> queryText)) Right $ case parseUUIDPair queryText of
        Just (a, p) -> findProcessId db a p
        Nothing -> UUID.fromText queryText >>= findProcessIdByActivityUUID db

{- | One delete-by-selection request against a loaded database. Two exclusive
selection modes: the filter fields select the whole matching set (@drIds =
Nothing@), or @drIds@ names the set exactly — every filter field must then be
absent, so a request can never silently mean two things. @drKeep@ spares and
@drExtra@ adds explicit process ids in both modes.
-}
data DeleteRequest = DeleteRequest
    { drName :: Maybe Text
    , drLocation :: Maybe Text
    , drProduct :: Maybe Text
    , drClassifications :: [(Text, Text, Bool)]
    , drExactName :: Bool
    , drKeep :: [Text]
    , drExtra :: [Text]
    , drIds :: Maybe [Text]
    }

{- | What a delete-by-selection request did: how many activities went, and
whether the database it edited keeps the change past an unload.
-}
data DeleteOutcome = DeleteOutcome
    { doRemoved :: Int
    , doPersisted :: Bool
    , doWarnings :: [Text]
    }

{- | Delete a selection from a loaded database, in place under the same name.

Resolves the selection ('drIds' verbatim, else the filter's full matching
set), resolves the explicit keep/extra process-id strings, applies the
adjustments, then hands the deletion to 'mutateUploadedDatabase', which
rebuilds, rewrites the sources of a database that owns them, and swaps the
result in. Fails (Left) when the database is not loaded, an ids/keep/extra id
is unknown, 'drIds' is combined with filter fields, the format cannot record
what the edit produced, or the rebuild reports an inconsistency.

A deletion from a database with its own files now outlives the process: the
old behaviour left the sources and the matrix cache holding the pre-delete
set, so a restart quietly resurrected every removed activity. A configured
(TOML) database still edits in memory only, and 'doPersisted' says so.
-}
deleteActivitiesInDB ::
    DatabaseManager ->
    Text -> -- database name
    DeleteRequest ->
    IO (Either Text DeleteOutcome)
deleteActivitiesInDB manager dbName DeleteRequest{drName = nameP, drLocation = geoP, drProduct = prodP, drClassifications = classFilters, drExactName = exactMatch, drKeep = keep, drExtra = extra, drIds = mIds} =
    getDatabase manager dbName >>= \case
        Nothing -> pure $ Left $ "Database not loaded: " <> dbName
        Just loaded -> do
            let db = ldDatabase loaded
                -- The two selection modes are exclusive: ids name the set
                -- verbatim, filters compute it. A request carrying both is
                -- ambiguous, so it is refused rather than guessed at — exact
                -- included, since it only modifies name/classification matching.
                hasFilter =
                    any isJust [nameP, geoP, prodP] || not (null classFilters) || exactMatch
                selectionE = case mIds of
                    Just ids
                        | hasFilter -> Left "ids cannot be combined with name/location/product/classification/exact filters"
                        | otherwise -> traverse (resolvePid db) ids
                    Nothing -> Right (filteredProcessIds db nameP geoP prodP classFilters exactMatch)
            case (,,) <$> traverse (resolvePid db) keep <*> traverse (resolvePid db) extra <*> selectionE of
                Left err -> pure $ Left err
                Right (keepPids, extraPids, filtered) -> do
                    let toDelete =
                            resolveDeleteSelection
                                DeleteSelection{dsFiltered = filtered, dsKeep = keepPids, dsExtra = extraPids}
                    unitConfig <- getMergedUnitConfig manager
                    outcome <- mutateUploadedDatabase manager dbName (deleteActivitiesWith unitConfig toDelete)
                    pure $ flip fmap outcome $ \done ->
                        DeleteOutcome
                            { doRemoved = length toDelete
                            , doPersisted = moPersisted done
                            , doWarnings = moWarnings done
                            }
