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
    copyDatabaseAs,
    copyDatabase,
    deleteActivities,
    deleteActivitiesWith,
    resolveDeleteSelection,
    DeleteSelection (..),
    deleteActivitiesInDB,
) where

import Control.Concurrent.STM (atomically, modifyTVar', readTVarIO)
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
import Matrix (clearCachedSolver)
import qualified Search.BM25 as BM25
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

{- | Produce a copy of a loaded database under a new internal name.

The returned 'Database' is the source value unchanged — it carries no
self-name (the name lives in the registry 'DatabaseConfig'), and because the
value is immutable the copy is automatically independent of the source. The
@newName@ is the registry identity the copy will be inserted under; see
'copyDatabase' for the effectful registration that applies it.

Kept as a named, total function so the rename intent is explicit at call
sites and so future deep-edit primitives (which *will* transform the value)
share this entry point.
-}
copyDatabaseAs :: Text -> Database -> Database
copyDatabaseAs _newName = id

{- | Copy a loaded database into the runtime registry under @newName@.

Looks up the loaded source, builds an independent 'LoadedDatabase' (renamed
config + fresh solver, see module note), and inserts it into the loaded /
available / indexed maps. Fails (Left) when the source is not loaded or when
@newName@ already names a loaded or configured database — a copy must never
silently overwrite an existing entry.
-}
copyDatabase :: DatabaseManager -> Text -> Text -> IO (Either Text ())
copyDatabase manager srcName newName = do
    loadedDbs <- readTVarIO (dmLoadedDbs manager)
    availableDbs <- readTVarIO (dmAvailableDbs manager)
    if M.member newName loadedDbs || M.member newName availableDbs
        then pure $ Left $ "Database already exists: " <> newName
        else
            getDatabase manager srcName >>= \case
                Nothing -> pure $ Left $ "Database not loaded: " <> srcName
                Just src -> do
                    let copiedDb = copyDatabaseAs newName (ldDatabase src)
                        newConfig = renameConfig newName (ldConfig src)
                    -- Fresh solver: a distinct name keys a distinct factorization cache.
                    let techTriplesInt =
                            [ (fromIntegral i, fromIntegral j, v)
                            | SparseTriple i j v <- U.toList (dbTechnosphereTriples copiedDb)
                            ]
                    solver <-
                        createSharedSolver
                            newName
                            techTriplesInt
                            (fromIntegral (dbActivityCount copiedDb))
                    let copied =
                            LoadedDatabase
                                { ldDatabase = copiedDb
                                , ldSharedSolver = solver
                                , ldConfig = newConfig
                                }
                    synonymDB <- getMergedSynonymDB manager
                    let indexedDb = buildIndexedDatabaseFromDB newName synonymDB copiedDb
                    atomically $ do
                        modifyTVar' (dmLoadedDbs manager) (M.insert newName copied)
                        modifyTVar' (dmAvailableDbs manager) (M.insert newName newConfig)
                        modifyTVar' (dmIndexedDbs manager) (M.insert newName indexedDb)
                    clearMethodMappingCacheForDb manager newName
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
pagination. Mirrors the non-BM25 structured-filter path of
'Service.searchActivities' (name / location / product / classification), which
is the set the UI's "delete the whole filtered set" button acts on.
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
    map fst (findActivitiesByFields db nameP geoP prodP classFilters exactMatch)

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

{- | Delete a selection from a loaded database, in place under the same name.

Resolves the filter to its full matching set, resolves the explicit
keep/extra process-id strings, applies the adjustments, deletes + rebuilds
with the manager's merged unit config, re-attaches runtime indexes with the
merged synonym DB, swaps a fresh solver in, and updates the loaded / indexed
registry maps. Returns the number of activities removed. Fails (Left) when the
database is not loaded, a keep/extra id is unknown, or the rebuild reports an
inconsistency.
-}
deleteActivitiesInDB ::
    DatabaseManager ->
    Text -> -- database name
    Maybe Text -> -- filter: name
    Maybe Text -> -- filter: location
    Maybe Text -> -- filter: product
    [(Text, Text, Bool)] -> -- filter: classification
    Bool -> -- exact name match
    [Text] -> -- explicit keep (process-id strings)
    [Text] -> -- explicit extra (process-id strings)
    IO (Either Text Int)
deleteActivitiesInDB manager dbName nameP geoP prodP classFilters exactMatch keep extra =
    getDatabase manager dbName >>= \case
        Nothing -> pure $ Left $ "Database not loaded: " <> dbName
        Just loaded -> do
            let db = ldDatabase loaded
            case (,) <$> traverse (resolvePid db) keep <*> traverse (resolvePid db) extra of
                Left err -> pure $ Left err
                Right (keepPids, extraPids) -> do
                    let filtered = filteredProcessIds db nameP geoP prodP classFilters exactMatch
                        toDelete =
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
                            pure $ Right (length toDelete)
