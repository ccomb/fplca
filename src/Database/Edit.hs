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
    deleteActivitiesInDB,
) where

import Control.Concurrent.STM (atomically, modifyTVar', readTVar, readTVarIO)
import Control.Exception (finally)
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import Config (DatabaseConfig (..))
import Database (
    applyStructuredFilters,
    buildIndexesWithProcessIds,
    buildProductIndex,
    findActivitiesByFields,
 )
import Database.CrossLinking (buildIndexedDatabaseFromDB)
import Database.Manager (
    DatabaseManager (..),
    LoadedDatabase (..),
    clearMethodMappingCacheForDb,
    getDatabase,
    getMergedSynonymDB,
    getMergedUnitConfig,
 )
import Database.MatrixBuild (
    InterningTables (..),
    buildBioTriples,
    buildInterningTables,
    buildSupplierRefUnits,
    buildTechTriples,
    collectBioFlowOrder,
 )
import Database.Upload (slugify)
import Matrix (clearCachedSolver)
import qualified Search.BM25 as BM25
import Service (bm25Retrieve)
import SharedSolver (createSharedSolver)
import Types (
    Activity (..),
    Database (..),
    Exchange (..),
    ProcessId,
    SparseTriple (..),
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

{- | Delete a selection from a loaded database, in place under the same name.

Resolves the selection ('drIds' verbatim, else the filter's full matching
set), resolves the explicit keep/extra process-id strings, applies the
adjustments, deletes + rebuilds with the manager's merged unit config,
re-attaches runtime indexes with the merged synonym DB, swaps a fresh solver
in, and updates the loaded / indexed registry maps. Returns the number of
activities removed. Fails (Left) when the database is not loaded, an
ids/keep/extra id is unknown, 'drIds' is combined with filter fields, or the
rebuild reports an inconsistency.
-}
deleteActivitiesInDB ::
    DatabaseManager ->
    Text -> -- database name
    DeleteRequest ->
    IO (Either Text Int)
deleteActivitiesInDB manager dbName DeleteRequest{drName = nameP, drLocation = geoP, drProduct = prodP, drClassifications = classFilters, drExactName = exactMatch, drKeep = keep, drExtra = extra, drIds = mIds} =
    getDatabase manager dbName >>= \case
        Nothing -> pure $ Left $ "Database not loaded: " <> dbName
        Just loaded -> do
            -- Deleting activities renumbers/removes them, which would leave any loaded
            -- database that depends on this one holding cross-DB links that no longer
            -- resolve — and those silently drop at solve time, undercounting the
            -- dependent. Refuse while dependents are loaded (mirrors 'unloadDatabase').
            loadedDbs <- readTVarIO (dmLoadedDbs manager)
            let db = ldDatabase loaded
                dependents =
                    [ name
                    | (name, ld) <- M.toList loadedDbs
                    , name /= dbName
                    , dbName `elem` dbDependsOn (ldDatabase ld)
                    ]
                guardDeps
                    | null dependents = Right ()
                    | otherwise =
                        Left $
                            "Cannot delete from "
                                <> dbName
                                <> ": still required by "
                                <> T.intercalate ", " dependents
                                <> ". Unload dependents first."
                -- The two selection modes are exclusive: ids name the set
                -- verbatim, filters compute it. A request carrying both is
                -- ambiguous, so it is refused rather than guessed at.
                hasFilter =
                    any (/= Nothing) [nameP, geoP, prodP] || not (null classFilters)
                selectionE = case mIds of
                    Just ids
                        | hasFilter -> Left "ids cannot be combined with name/location/product/classification filters"
                        | otherwise -> traverse (resolvePid db) ids
                    Nothing -> Right (filteredProcessIds db nameP geoP prodP classFilters exactMatch)
            case guardDeps *> ((,,) <$> traverse (resolvePid db) keep <*> traverse (resolvePid db) extra <*> selectionE) of
                Left err -> pure $ Left err
                Right (keepPids, extraPids, filtered) -> do
                    let toDelete =
                            resolveDeleteSelection
                                DeleteSelection{dsFiltered = filtered, dsKeep = keepPids, dsExtra = extraPids}
                    unitConfig <- getMergedUnitConfig manager
                    case deleteActivitiesWith unitConfig toDelete db of
                        Left err -> pure $ Left err
                        Right rebuilt -> do
                            synonymDB <- getMergedSynonymDB manager
                            let withRuntime = BM25.addBM25Index (initializeRuntimeFields rebuilt synonymDB)
                                techTriplesInt =
                                    [ (fromIntegral i, fromIntegral j, v)
                                    | SparseTriple i j v <- U.toList (dbTechnosphereTriples withRuntime)
                                    ]
                            -- The MUMPS solver cached under this name is now stale (old
                            -- dimensions/factorization); drain + destroy it as unload/remove do,
                            -- before installing the rebuilt one — otherwise the first post-delete
                            -- solve overwrites the map entry and leaks a worker thread + native
                            -- instance.
                            clearCachedSolver dbName
                            solver <-
                                createSharedSolver
                                    dbName
                                    techTriplesInt
                                    (fromIntegral (dbActivityCount withRuntime))
                            let loaded' = loaded{ldDatabase = withRuntime, ldSharedSolver = solver}
                                indexedDb = buildIndexedDatabaseFromDB dbName synonymDB withRuntime
                            atomically $ do
                                modifyTVar' (dmLoadedDbs manager) (M.insert dbName loaded')
                                modifyTVar' (dmIndexedDbs manager) (M.insert dbName indexedDb)
                            clearMethodMappingCacheForDb manager dbName
                            -- Edits are transient by design. The on-disk matrix cache
                            -- and the source still hold the pre-delete set, so an
                            -- unload/reload deliberately restores the original
                            -- database; persisting an edited database is a separate,
                            -- explicit step (exporting it to a new file). We therefore
                            -- leave the cache in place rather than forcing the next
                            -- load to rebuild from source.
                            pure $ Right (length toDelete)
