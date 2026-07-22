{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Manager (
    -- * Types
    DatabaseManager (..),
    LoadedDatabase (..),
    DatabaseStatus (..),
    DatabaseLoadStatus (..),
    StagedDatabase (..),
    DatabaseSetupInfo (..),
    SetupError (..),
    setupErrorMessage,
    MissingSupplier (..),
    DependencyChoice (..),
    DependencyStatus (..),
    MethodCollectionStatus (..),
    RefDataStatus (..),
    DirectoryFormat (..),

    -- * Format detection
    detectDirectoryFormat,

    -- * Re-exports
    DepLoadResult (..),

    -- * Initialization
    initDatabaseManager,

    -- * Operations
    getDatabase,
    mkDepSolverLookup,
    listDatabases,
    clearMethodMappingCacheForDb,

    -- * Load/Unload
    loadDatabase,
    unloadDatabase,
    relinkDatabase,
    relinkDatabaseWithMapping,
    RelinkResult (..),
    addDatabase,
    removeDatabase,

    -- * Method Operations
    listMethodCollections,
    loadMethodCollection,
    loadMethodCollectionFromConfig,
    unloadMethodCollection,
    getLoadedMethods,
    getMethodCollection,
    addMethodCollection,
    removeMethodCollection,

    -- * Geography
    parseGeographiesCSV,

    -- * Reference Data Operations
    autoCreateFlowSynonyms,
    listFlowSynonyms,
    loadFlowSynonyms,
    unloadFlowSynonyms,
    addFlowSynonyms,
    removeFlowSynonyms,
    listCompartmentMappings,
    loadCompartmentMappings,
    unloadCompartmentMappings,
    addCompartmentMappings,
    removeCompartmentMappings,
    listUnitDefs,
    loadUnitDefs,
    unloadUnitDefs,
    addUnitDefs,
    removeUnitDefs,
    getFlowSynonymGroups,
    getMergedSynonymDB,
    getMergedCompartmentMap,
    getMergedEnergyDensities,
    getMergedUnitConfig,
    getMergedFlowMetadata,
    getLocationHierarchy,

    -- * Staged Database Operations
    getStagedDatabase,
    getDatabaseSetupInfo,
    buildLoadedSetupInfo,
    databaseGapReport,
    databaseQualityReport,
    databaseCoverageReport,
    addDependencyToStaged,
    removeDependencyFromStaged,
    setDataPath,
    finalizeDatabase,

    -- * Cached flow mapping
    mapMethodToFlowsCached,
    effectiveMethodMappings,
    mapMethodToTablesCached,
    mapMethodSetToTablesCached,
    mapMethodToIndexCached,

    -- * Internal (for Main.hs to load database)
    loadDatabaseFromConfig,

    -- * Internal (for tests: lowest-level loader, exposes the cache-hit flag)
    loadDatabaseRawWithCrossDB,

    -- * Internal (for tests: pure dependency-list builder)
    buildDependencyChoices,
) where

import API.JsonOptions (Stripped (..))
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (mapConcurrently, mapConcurrently_)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import qualified Control.Exception
import Control.Lens ((&), (?~))
import Control.Monad (forM, forM_, unless, void, when)
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.:?), (.=))
import qualified Data.Aeson as A
import Data.Bifunctor (first)
import Data.Char (toLower)
import Data.Either (lefts, partitionEithers, rights)
import Data.List (isPrefixOf, sort, sortOn, unsnoc)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import Data.OpenApi (NamedSchema (..), OpenApiType (..), ToSchema (..), enum_, type_)
import Data.Ord (Down (..))
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, removeDirectoryRecursive, removeFile)
import System.FilePath (isAbsolute, normalise, takeDirectory, takeExtension, takeFileName, (</>))
import System.Mem (performGC)

import Config
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Time (diffUTCTime, getCurrentTime)
import Database (buildDatabaseWithMatrices)
import qualified Database.Loader as Loader
import qualified Database.Quality as Quality
import EcoSpold.Parser2 (normalizeCAS)
import Matrix (clearCachedSolver)
import Method.ChemSynonyms (ChemSynonyms, emptyChemSynonyms, loadChemSynonyms)
import qualified Method.Coverage as Coverage
import Method.Mapping (
    MatchStrategy,
    MethodIndex,
    MethodSetTables,
    MethodTables,
    ProxyTargets (..),
    RegionalActivityWeights (..),
    buildMethodIndex,
    buildMethodSetTables,
    buildMethodTables,
    characterizedFlowIds,
    directionExcludedCFs,
    expandProxyEdges,
    expandSynonymMappings,
    fillBroadcastVector,
    fillRegionalActivityWeights,
    mapMethodToFlows,
    mtRegionalActivityWeights,
    mtRegionalizedCF,
    projectRegionalResourceFlows,
 )
import Method.Types (
    CompartmentMap,
    EnergyDensityMap,
    Location (..),
    Method (..),
    MethodCF (..),
    MethodCollection (..),
    ScoringSet (..),
    buildCompartmentMapFromCSV,
    buildEnergyDensityMapFromCSV,
    cfFamily,
    compartmentMapSize,
    energyDensityMapSize,
 )
import Progress (ProgressLevel (..), reportError, reportProgress, reportProgressWithTiming, withLogScope)
import qualified Search.BM25 as BM25
import SharedSolver (SharedSolver, createSharedSolver)
import qualified SharedSolver
import SubstanceRegistry (CASNumber (..), KeyNormalizers (..), NormName (..), SubstanceEdge, casBindingsFromEdges, parseSubstanceEdges)
import SynonymDB (BridgeDirection (..), SynEdge (..), SynonymDB (..), buildFromCSV, emptySynonymDB, excludeJunkSynonyms, excludeOverFrequentSynonyms, loadFromCSVFileWithCache, mergeSynonymDBs, normalizeName, oversizedClasses, reopenedBridges, synonymCount, uncoveredUnitSuffixes)
import Types (
    Activity (..),
    AttributeFallback (..),
    BioFlowDB,
    BiosphereFlow (..),
    CrossDBLink (..),
    CrossDBLinkingStats (..),
    Database (..),
    GeographyPolicy (..),
    LinkBlocker (..),
    LocationFallback (..),
    LocationUnresolved (..),
    SimpleDatabase (..),
    SparseTriple (..),
    UUID,
    Unit (..),
    UnitDB,
    bfCompartmentName,
    bfCompartmentSub,
    blockerReasonDetail,
    computeMinimalSelectedDeps,
    crossDBBySource,
    crossDBRedundantSources,
    deduplicateAttributeFallbacks,
    deduplicateFallbacks,
    deduplicateUnresolved,
    enrichBioFlowCAS,
    exchangeFlowId,
    exchangeIsReference,
    initializeRuntimeFields,
    toSimpleDatabase,
    unresolvedCount,
 )
import qualified UnitConversion

-- CrossDBLinkingStats is now in Types, re-exported from Database.Loader

import API.Types (DepLoadResult (..))
import qualified Data.Text.IO as TIO
import Database.CrossLinking (IndexedDatabase (..), LinkingContext (..), buildIndexedDatabaseFromDB, defaultLinkingThreshold)
import qualified Database.CrossLinking as CrossLinking
import Database.Upload (detectMethodFormat, detectedFormatLabel, findMethodDirectory, listDirectoryRecursive)
import qualified Database.Upload as Upload
import qualified Database.UploadedDatabase as UploadedDB
import Method.FlowResolver (ILCDFlowInfo)
import qualified Method.FlowResolver as FlowResolver
import qualified Method.Parser
import qualified Method.Parser.OlcaSchema as OlcaSchema
import Method.ParserCSV (parseMethodCSVBytes, stripBOM)
import Method.ParserSimaPro (isSimaProMethodCSV, parseSimaProMethodCSVBytes)
import qualified Method.Patch
import qualified SimaPro.Parser as SimaPro
import SynonymDB.Extract (extractFromEcoSpold2, extractFromILCDFlows, synonymPairsToCSV)

-- | A fully loaded database with solver ready for queries
data LoadedDatabase = LoadedDatabase
    { ldDatabase :: !Database
    , ldSharedSolver :: !SharedSolver
    , ldConfig :: !DatabaseConfig
    }

{- | A staged database awaiting dependency configuration
This is the intermediate state before building matrices
-}
data StagedDatabase = StagedDatabase
    { sdSimpleDB :: !SimpleDatabase
    -- ^ Parsed data (activities, flows, units)
    , sdConfig :: !DatabaseConfig
    -- ^ Configuration
    , sdUnlinkedCount :: !Int
    -- ^ Total unlinked exchanges
    , sdMissingProducts :: ![(Text, Int, LinkBlocker)]
    -- ^ (product name, count, reason)
    , sdSelectedDeps :: ![Text]
    -- ^ Selected dependency database names
    , sdCrossDBLinks :: ![CrossDBLink]
    -- ^ Cross-DB links found so far
    , sdLinkingStats :: !CrossDBLinkingStats
    -- ^ Linking statistics
    , sdCachedDB :: !(Maybe Database)
    -- ^ Pre-built DB from cache (skip rebuild)
    }

-- | Information about a missing supplier product
data MissingSupplier = MissingSupplier
    { msProductName :: !Text
    , msCount :: !Int
    -- ^ Number of activities needing this supplier
    , msLocation :: !(Maybe Text)
    -- ^ Most common location requested
    , msReason :: !Text
    -- ^ "unit_incompatible", "location_unavailable", "no_name_match"
    , msDetail :: !(Maybe Text)
    -- ^ e.g. "kg vs ton", "FR not available"
    }
    deriving (Show, Eq, Generic)
    deriving (ToJSON, ToSchema) via (Stripped MissingSupplier)

{- | Whether a candidate dependency is currently selected, merely available,
or redundant under the minimal cover (matches links but every link it wins
can be re-supplied by another selected DB at the same score).
-}
data DependencyStatus = SelectedDep | AvailableDep | RedundantDep
    deriving (Show, Eq, Generic)

instance ToJSON DependencyStatus where
    toJSON SelectedDep = A.String "selected"
    toJSON AvailableDep = A.String "available"
    toJSON RedundantDep = A.String "redundant"

{- | String-enum schema matching the lowercase wire codes from ToJSON above.
The previous default-Generic schema advertised the raw Haskell constructor
names (SelectedDep / AvailableDep / RedundantDep), which is what the schema
said but never what the wire emitted.
-}
instance ToSchema DependencyStatus where
    declareNamedSchema _ =
        pure $
            NamedSchema (Just "DependencyStatus") $
                mempty
                    & type_ ?~ OpenApiString
                    & enum_ ?~ [toJSON (c :: Text) | c <- ["selected", "available", "redundant"]]

-- | A candidate dependency database in one of three states
data DependencyChoice = DependencyChoice
    { dchStatus :: !DependencyStatus
    , dchDatabaseName :: !Text
    , dchDisplayName :: !Text
    , dchMatchCount :: !Int
    }
    deriving (Show, Eq, Generic)
    deriving (ToJSON, ToSchema) via (Stripped DependencyChoice)

{- | One of the candidate data directories inside an uploaded database's
upload root. Surfaces in @DatabaseSetupInfo.dsiAvailablePaths@ so the UI
can present a picker. The schema is now a proper named object instead
of a positional 3-tuple.
-}
data PathCandidate = PathCandidate
    { pcPath :: !Text
    -- ^ Relative path under the upload root
    , pcFormat :: !Text
    -- ^ Format label (e.g. "EcoSpold 2", "SimaPro CSV", "Unknown")
    , pcFileCount :: !Int
    -- ^ Number of data files detected in this directory
    }
    deriving (Show, Eq, Generic)
    deriving (ToJSON, ToSchema) via (Stripped PathCandidate)

-- | Setup info for a database (for the setup page)
data DatabaseSetupInfo = DatabaseSetupInfo
    { dsiName :: !Text
    , dsiDisplayName :: !Text
    , dsiActivityCount :: !Int
    , dsiInputCount :: !Int
    -- ^ Total technosphere inputs
    , dsiCompleteness :: !Double
    -- ^ Percentage of resolved links (0-100)
    , dsiInternalLinks :: !Int
    -- ^ Links resolved within this database
    , dsiCrossDBLinks :: !Int
    -- ^ Links resolved via dependencies
    , dsiUnresolvedLinks :: !Int
    -- ^ Still unresolved
    , dsiMissingSuppliers :: ![MissingSupplier]
    -- ^ Top missing suppliers
    , dsiDependencies :: ![DependencyChoice]
    {- ^ Candidate dependencies in one alpha-sorted list, each tagged as
    selected, available, or redundant under the minimal cover.
    -}
    , dsiIsReady :: !Bool
    -- ^ True if can be finalized
    , dsiUnknownUnits :: ![Text]
    -- ^ Unknown units from sdbUnits
    , dsiLocationFallbacks :: ![LocationFallback]
    -- ^ Accepted links with widened geography, tagged with 'LocationKind'
    , dsiLocationUnresolved :: ![LocationUnresolved]
    {- ^ Inputs that could not be linked because the database's
    'GeographyPolicy' rejected every candidate (or no candidate existed)
    -}
    , dsiAttributeFallbacks :: ![AttributeFallback]
    {- ^ Source-identity inputs (non-nil 'activityLinkId') matched by attributes
    because no loaded dependency shipped the exact activity — a likely
    cross-version stitch the consumer should verify against the source release.
    -}
    , dsiDataPath :: !Text
    -- ^ Current selected data path (relative)
    , dsiAvailablePaths :: ![PathCandidate]
    -- ^ Candidate data directories within the upload root
    , dsiIsLoaded :: !Bool
    -- ^ True if database is already loaded (read-only info)
    }
    deriving (Show, Eq, Generic)
    deriving (ToJSON) via (Stripped DatabaseSetupInfo)

-- | Errors from getDatabaseSetupInfo
data SetupError
    = SetupNotFound Text
    | -- | Configured (non-uploaded) database that must be loaded before setup.
      SetupNotLoaded Text
    | SetupFailed Text
    deriving (Show, Eq)

setupErrorMessage :: SetupError -> Text
setupErrorMessage (SetupNotFound msg) = msg
setupErrorMessage (SetupNotLoaded name) = "Database not loaded: " <> name
setupErrorMessage (SetupFailed msg) = msg

-- | Load status: derivable from TVar membership + linking stats
data DatabaseLoadStatus = Unloaded | PartiallyLinked | Loaded
    deriving (Show, Eq, Generic)

instance ToJSON DatabaseLoadStatus where
    toJSON Unloaded = A.String "unloaded"
    toJSON PartiallyLinked = A.String "partially_linked"
    toJSON Loaded = A.String "loaded"

instance FromJSON DatabaseLoadStatus where
    parseJSON = A.withText "DatabaseLoadStatus" $ \case
        "unloaded" -> pure Unloaded
        "partially_linked" -> pure PartiallyLinked
        "loaded" -> pure Loaded
        other -> fail $ "Unknown DatabaseLoadStatus: " <> T.unpack other

-- | Status of a database for API responses
data DatabaseStatus = DatabaseStatus
    { dsName :: !Text -- Internal identifier (slug)
    , dsDisplayName :: !Text -- Human-readable name for UI
    , dsDescription :: !(Maybe Text)
    , dsLoadAtStartup :: !Bool -- Configured to load at startup
    , dsStatus :: !DatabaseLoadStatus -- Derived from TVar membership + linking stats
    , dsIsUploaded :: !Bool -- True if path starts with "uploads/"
    , dsPath :: !Text -- Data path
    , dsFormat :: !(Maybe Upload.DatabaseFormat) -- Detected format
    , dsActivityCount :: !Int -- Number of activities (0 if unloaded)
    , dsDependsOn :: ![Text] -- Names of databases this one depends on (for cross-DB linking)
    }
    deriving (Show, Eq, Generic)

instance ToJSON DatabaseStatus where
    toJSON DatabaseStatus{..} =
        A.object
            [ "dsName" .= dsName
            , "dsDisplayName" .= dsDisplayName
            , "dsDescription" .= dsDescription
            , "dsLoadAtStartup" .= dsLoadAtStartup
            , "dsStatus" .= dsStatus
            , "dsIsUploaded" .= dsIsUploaded
            , "dsPath" .= dsPath
            , "dsFormat" .= dsFormat
            , "dsActivityCount" .= dsActivityCount
            , "dsDependsOn" .= dsDependsOn
            ]

instance FromJSON DatabaseStatus where
    parseJSON = A.withObject "DatabaseStatus" $ \v ->
        DatabaseStatus
            <$> v .: "dsName"
            <*> v .: "dsDisplayName"
            <*> v .:? "dsDescription"
            <*> v .: "dsLoadAtStartup"
            <*> v .: "dsStatus"
            <*> v .: "dsIsUploaded"
            <*> v .: "dsPath"
            <*> v .:? "dsFormat"
            <*> v .: "dsActivityCount"
            <*> v .:? "dsDependsOn" A..!= []

-- | Status of a method collection (e.g., EF-3.1) for API responses
data MethodCollectionStatus = MethodCollectionStatus
    { mcsName :: !Text -- Internal identifier
    , mcsDisplayName :: !Text -- Human-readable name
    , mcsDescription :: !(Maybe Text) -- Optional description
    , mcsStatus :: !DatabaseLoadStatus -- Loaded/Unloaded (reuse existing type)
    , mcsIsUploaded :: !Bool -- True if uploaded (vs. configured in TOML)
    , mcsPath :: !Text -- Path to method directory
    , mcsMethodCount :: !Int -- Number of impact categories (0 if unloaded)
    , mcsFormat :: !Text -- "SimaPro CSV", "ILCD", etc.
    }
    deriving (Show, Eq, Generic)

instance ToJSON MethodCollectionStatus where
    toJSON MethodCollectionStatus{..} =
        A.object
            [ "mcsName" .= mcsName
            , "mcsDisplayName" .= mcsDisplayName
            , "mcsDescription" .= mcsDescription
            , "mcsStatus" .= mcsStatus
            , "mcsIsUploaded" .= mcsIsUploaded
            , "mcsPath" .= mcsPath
            , "mcsMethodCount" .= mcsMethodCount
            , "mcsFormat" .= mcsFormat
            ]

instance FromJSON MethodCollectionStatus where
    parseJSON = A.withObject "MethodCollectionStatus" $ \v ->
        MethodCollectionStatus
            <$> v .: "mcsName"
            <*> v .: "mcsDisplayName"
            <*> v .:? "mcsDescription"
            <*> v .: "mcsStatus"
            <*> v .: "mcsIsUploaded"
            <*> v .: "mcsPath"
            <*> v .: "mcsMethodCount"
            <*> v .: "mcsFormat"

{- | The database manager maintains state for multiple databases
Databases with load=true are pre-loaded at startup for instant switching
-}
data DatabaseManager = DatabaseManager
    { dmLoadedDbs :: !(TVar (Map Text LoadedDatabase)) -- All loaded databases
    , dmStagedDbs :: !(TVar (Map Text StagedDatabase)) -- Staged databases (parsed but not finalized)
    , dmStagingDbs :: !(TVar (S.Set Text)) -- Databases currently being staged
    , dmIndexedDbs :: !(TVar (Map Text IndexedDatabase)) -- Pre-built indexes for cross-DB linking
    , dmAvailableDbs :: !(TVar (Map Text DatabaseConfig)) -- All configured databases
    , dmAvailableMethods :: !(TVar (Map Text MethodConfig)) -- All configured method collections
    , dmLoadedMethods :: !(TVar (Map Text MethodCollection)) -- name → parsed methods + NW data
    -- Reference data: flow synonyms
    , dmAvailableFlowSyns :: !(TVar (Map Text RefDataConfig))
    , dmLoadedFlowSyns :: !(TVar (Map Text SynonymDB))
    , -- Reference data: compartment mappings
      dmAvailableCompMaps :: !(TVar (Map Text RefDataConfig))
    , dmLoadedCompMaps :: !(TVar (Map Text CompartmentMap))
    , -- Reference data: unit definitions
      dmAvailableUnitDefs :: !(TVar (Map Text RefDataConfig))
    , dmLoadedUnitDefs :: !(TVar (Map Text UnitConversion.UnitConfig))
    , -- Reference data: energy densities (mass/volume → energy for energy-denominated CFs)
      dmAvailableEnergyDensities :: !(TVar (Map Text RefDataConfig))
    , dmLoadedEnergyDensities :: !(TVar (Map Text EnergyDensityMap))
    , dmNoCache :: !Bool -- Caching disabled flag
    , dmGeographies :: !(Map Text (Text, [Text])) -- code → (display_name, parent_codes)
    , dmMethodMappingCache :: !(TVar (Map (Text, Text, UUID) [(MethodCF, Maybe (BiosphereFlow, MatchStrategy))]))
    {- ^ Cached flow mappings: (dbName, collection, methodId) → mappings.
    The collection is part of the key because a method UUID is a UUIDv5 of the
    method name alone, so the same name in two collections collides on UUID
    while carrying different CF lists. Invalidated on database/method/synonym
    reload.
    -}
    , dmMethodTablesCache :: !(TVar (Map (Text, Text, UUID) MethodTables))
    {- ^ Cached LCIA-score lookup tables built from mappings.
    These depend only on (db, collection, method), so building them once per
    triple saves O(n log n) Map constructions on every LCIA call.
    -}
    , dmMethodTablesInflight :: !(TVar (Map (Text, Text, UUID) (TMVar (Either SomeException MethodTables))))
    {- ^ Single-flight slots guarding 'dmMethodTablesCache' builds. The first
    caller for a key installs an empty 'TMVar' and runs the (expensive) build;
    concurrent callers — including the load-time warm-up — await that slot
    instead of each rebuilding the same tables. The slot is removed when the
    build finishes, so a failed build is retried rather than cached.
    -}
    , dmMethodSetTablesCache :: !(TVar (Map (Text, Text, [UUID]) MethodSetTables))
    {- ^ Cached stacked CF tables for multi-method scoring.
    Key is (dbName, collection, sortedMethodIds) so subset-arbitrary requests
    share cache entries with named-collection ones whenever the method ids
    match within the same collection. Purged together with
    'dmMethodTablesCache' on any reload that invalidates the per-method cache
    (collection / synonym / DB load).
    -}
    , dmMethodIndexCache :: !(TVar (Map (Text, Text, UUID) MethodIndex))
    {- ^ Cached inverted indices over a method (CF tokens, by-medium, by-CAS).
    Used by the post-scoring suggester to surface candidate matches for
    uncharacterized flows. Keyed identically to the tables cache and
    invalidated on the same conditions.
    -}
    , dmChemSynonyms :: !ChemSynonyms
    {- ^ Vendored PubChem snapshot loaded once at startup. Drives the
    suggester's synonym-expansion signal. Empty when no path is configured
    or when the file is missing — suggester degrades to plain Jaccard.
    -}
    , dmSubstanceEdges :: ![SubstanceEdge]
    {- ^ Typed flow-correspondence edges loaded once at startup from
    @substance_edges.csv@. Empty when no path is configured. @ProxyFor@ edges
    feed the CF cascade ('expandProxyEdges'); @SameAs@ name↔CAS edges feed
    'dmCasBindings'.
    -}
    , dmCasBindings :: !(M.Map NormName CASNumber)
    {- ^ Name→CAS identities distilled from the @SameAs@ edges, applied to
    every database at load ('enrichBioFlowCAS') to fill empty @bfCAS@ so the
    native CAS bridge reaches flows a source left CAS-less (e.g. SimaPro
    exports). Empty when no edges bind a name to a CAS.
    -}
    , dmMergedFlowMetadataCache :: !(TVar (Maybe (BioFlowDB, UnitDB)))
    {- ^ Memoized 'M.unions' of every loaded DB's flows/units.
    Invalidated on any 'dmLoadedDbs' mutation; collision detection
    runs once per rebuild rather than per hot-path call.
    -}
    , dmMergedUnitConfigCache :: !(TVar (Maybe UnitConversion.UnitConfig))
    {- ^ Memoized merge of every loaded unit-definition set.
    Invalidated on 'dmLoadedUnitDefs' mutation.
    -}
    }

{- | Cached flow mapping: avoids re-matching method CFs to database flows on every LCIA call.
The mapping depends only on (database, method), not on the process being evaluated.
-}
mapMethodToFlowsCached :: DatabaseManager -> Text -> Text -> Database -> Method -> IO [(MethodCF, Maybe (BiosphereFlow, MatchStrategy))]
mapMethodToFlowsCached manager dbName collection db method = do
    let key = (dbName, collection, methodId method)
    cache <- readTVarIO (dmMethodMappingCache manager)
    case M.lookup key cache of
        Just cached -> return cached
        Nothing -> do
            result <- mapMethodToFlows db method
            atomically $ modifyTVar' (dmMethodMappingCache manager) (M.insert key result)
            return result

{- | The mappings scoring actually uses: the cached cascade result expanded
with the database's synonym fan-out and the configured substance edges.
Diagnostics (flow-mapping endpoints, coverage audits) must read THIS rather
than the raw cascade, or they under-report what the score tables contain.

Uses the database's frozen-at-load-time synonym DB, which holds the curated
registry plus any source the user had explicitly activated at load time
(auto-extracted candidates are persisted but never loaded by the engine).
-}
effectiveMethodMappings :: DatabaseManager -> Text -> Text -> Database -> Method -> IO [(MethodCF, Maybe (BiosphereFlow, MatchStrategy))]
effectiveMethodMappings manager dbName collection db method = do
    mappings <- mapMethodToFlowsCached manager dbName collection db method
    let synDB = fromMaybe emptySynonymDB (dbSynonymDB db)
        proxyTargets = ProxyTargets (dbFlowsByName db) (dbFlowsByCAS db) (dbBioFlows db)
    pure $
        expandProxyEdges proxyTargets (dmSubstanceEdges manager) $
            projectRegionalResourceFlows synDB (dbBioFlows db) $
                expandSynonymMappings synDB (dbFlowsByName db) mappings

-- | Cached prepared CF tables: built once per (db, method), reused across inventories.
mapMethodToTablesCached :: DatabaseManager -> Text -> Text -> Database -> Method -> IO MethodTables
mapMethodToTablesCached manager dbName collection db method = do
    hier <- getLocationHierarchy manager
    mapMethodToTablesCachedWithHier manager dbName collection db hier method

{- | Variant of 'mapMethodToTablesCached' that takes the location hierarchy as
an argument. Lets 'mapMethodSetToTablesCached' fetch it once per request
instead of once per method in the concurrent fan-out.
-}
mapMethodToTablesCachedWithHier ::
    DatabaseManager ->
    Text ->
    Text ->
    Database ->
    M.Map Location [Location] ->
    Method ->
    IO MethodTables
mapMethodToTablesCachedWithHier manager dbName collection db hier method = do
    let key = (dbName, collection, methodId method)
    cache <- readTVarIO (dmMethodTablesCache manager)
    case M.lookup key cache of
        Just tables -> pure tables
        Nothing ->
            -- Single-flight: the expensive build runs once per key even under a
            -- concurrent panel request + load-time warm-up racing on it.
            singleFlight
                (dmMethodTablesInflight manager)
                key
                (modifyTVar' (dmMethodTablesCache manager) . M.insert key)
                (buildMethodTablesFor manager dbName collection db hier method)

{- | Build the LCIA lookup tables for one method against a database: resolve the
CF→flow mappings, stack them into the broadcast/CAS/regional tables, and
precompute the regionalized per-activity weights. The deduplicated regional
coverage gaps are surfaced once here, at build time, rather than per-pid on the
scoring path. Caching and single-flighting are the caller's responsibility.
-}
buildMethodTablesFor ::
    DatabaseManager -> Text -> Text -> Database -> M.Map Location [Location] -> Method -> IO MethodTables
buildMethodTablesFor manager dbName collection db hier method = do
    expanded <- effectiveMethodMappings manager dbName collection db method
    -- A CF matchable through the union synonym tables but not through its own
    -- direction's view was excluded by the direction restriction alone — the
    -- usual cause is a method whose parser defaulted the direction (no
    -- metadata). Warn so the loss is distinguishable from a genuinely
    -- uncharacterized flow.
    let dirExcluded =
            directionExcludedCFs (fromMaybe emptySynonymDB (dbSynonymDB db)) (dbFlowsByName db) expanded
    unless (null dirExcluded) $
        reportProgress Warning $
            "[LCIA "
                <> T.unpack (methodName method)
                <> "] "
                <> show (length dirExcluded)
                <> " CF(s) match a synonym bridge only outside their flow direction "
                <> "(direction metadata may be missing from the method). Samples: "
                <> show (take 3 (map mcfFlowName dirExcluded))
    cmap <- getMergedCompartmentMap manager
    energyDensities <- getMergedEnergyDensities manager
    unitConfig <- getMergedUnitConfig manager
    (mFlows, mUnits) <- getMergedFlowMetadata manager
    -- A method listed in its collection's 'global-methods' is scored without
    -- regionalization: drop its located CFs so the broadcast (global) path — the
    -- method's own unlocated default CF — is the single answer, matching a
    -- reference distribution that flattened the spatial factors to a global value.
    -- This assumes the method carries such an unlocated default for the flows in
    -- question; a method whose CFs are all region-tagged would be left with none.
    -- The config loader warns when a 'global-methods' name matches no method.
    globalMethods <- maybe [] mcGlobalMethods . M.lookup collection <$> readTVarIO (dmAvailableMethods manager)
    let !raw0 = buildMethodTables (cfFamily (methodUnit method)) cmap energyDensities expanded
        !raw =
            if methodName method `elem` globalMethods
                then raw0{mtRegionalizedCF = M.empty}
                else raw0
        !withBroadcast = fillBroadcastVector unitConfig mUnits mFlows raw
        -- Precompute per-activity weights for regionalized methods so subsequent
        -- scoring is a dot product instead of one biosphere-triple walk per pid.
        !tables = fillRegionalActivityWeights unitConfig mUnits mFlows db hier withBroadcast
    case mtRegionalActivityWeights tables of
        Nothing -> pure ()
        Just raw' ->
            unless (null (rawMissingPairs raw')) $
                reportProgress Warning $
                    "[LCIA "
                        <> T.unpack (methodName method)
                        <> "] "
                        <> show (length (rawMissingPairs raw'))
                        <> " regionalized (flow, location) pair(s) without CF coverage "
                        <> "(after walking parent regions and universal broadcast). "
                        <> "Samples: "
                        <> show (take 3 [(show fid, T.unpack loc) | (fid, Location loc) <- rawMissingPairs raw'])
    pure tables

{- | Run @build@ at most once per @key@ across concurrent callers. The first
caller installs a slot and runs the build; others block on the same result
rather than duplicating the work. @onSuccess@ runs in the slot-clearing
transaction, so a built value lands in its cache atomically with the release. A
failed build clears the slot — the next caller retries — and re-throws.
-}
singleFlight ::
    (Ord k) =>
    TVar (Map k (TMVar (Either SomeException a))) ->
    k ->
    (a -> STM ()) ->
    IO a ->
    IO a
singleFlight inflightVar key onSuccess build = do
    (slot, owner) <- atomically $ do
        inflight <- readTVar inflightVar
        case M.lookup key inflight of
            Just s -> pure (s, False)
            Nothing -> do
                s <- newEmptyTMVar
                writeTVar inflightVar (M.insert key s inflight)
                pure (s, True)
    if not owner
        then either Control.Exception.throwIO pure =<< atomically (readTMVar slot)
        else do
            result <- try build
            atomically $ do
                modifyTVar' inflightVar (M.delete key)
                either (const (pure ())) onSuccess result
                putTMVar slot result
            either Control.Exception.throwIO pure result

{- | Kick off a background warm-up of every loaded method's lookup tables
against @dbName@, so the expensive (regional) build is paid once at load time
rather than on the first user score. Single-flighting means a request arriving
mid-warm joins the in-flight build instead of starting a second one.

Methods are built one at a time in a single background thread — deliberately
sequential rather than 'mapMethodSetToTablesCached''s concurrent fan-out: the
heavy regional builds (e.g. AWARE water use) thrash the GC when run in parallel,
so serial warming is both lower-peak-memory and faster wall-clock here. A
failure is logged, not fatal — the on-demand path rebuilds and surfaces it.
-}
warmMethodTables :: DatabaseManager -> Text -> Database -> IO ()
warmMethodTables manager dbName db = void $ forkIO $ withLogScope dbName $ do
    collections <- readTVarIO (dmLoadedMethods manager)
    let methods = [(collName, m) | (collName, mc) <- M.toList collections, m <- mcMethods mc]
    t0 <- getCurrentTime
    reportProgress Info $
        "[warm] " <> T.unpack dbName <> ": warming " <> show (length methods) <> " method table(s) in background…"
    forM_ methods $ \(collName, method) -> do
        r <- try (void (mapMethodToTablesCached manager dbName collName db method))
        case r of
            Right () -> pure ()
            Left (e :: SomeException) ->
                reportProgress Warning $
                    "[warm] "
                        <> T.unpack dbName
                        <> " / "
                        <> T.unpack collName
                        <> " / "
                        <> T.unpack (methodName method)
                        <> ": "
                        <> show e
    t1 <- getCurrentTime
    reportProgress Info $
        "[warm] "
            <> T.unpack dbName
            <> ": done ("
            <> show (round (realToFrac (diffUTCTime t1 t0) :: Double) :: Int)
            <> "s)"

{- | Cached stacked CF tables for multi-method scoring. Built once per
(dbName, sortedMethodIds), so two requests asking for the same method set
(in any order) share an entry. Per-method 'MethodTables' are sourced via
'mapMethodToTablesCached' and re-used; the only set-level work is stacking
broadcasts into a dense matrix when none of the methods are regionalized.
-}
mapMethodSetToTablesCached :: DatabaseManager -> Text -> Text -> Database -> [Method] -> IO MethodSetTables
mapMethodSetToTablesCached manager dbName collection db methods = do
    -- Canonical key = (dbName, collection, sorted methodIds). Stable regardless
    -- of input ordering so subset-arbitrary requests don't fragment the cache.
    let sortedMethods = sortOn methodId methods
        key = (dbName, collection, map methodId sortedMethods)
    cache <- readTVarIO (dmMethodSetTablesCache manager)
    case M.lookup key cache of
        Just mst -> pure mst
        Nothing -> do
            -- Fetch the location hierarchy once for the whole fan-out so the
            -- concurrent workers don't each rebuild the typed hierarchy
            -- under 'getLocationHierarchy'.
            hier <- getLocationHierarchy manager
            -- mapConcurrently here parallelizes the per-method 'MethodTables'
            -- build across the whole collection. On first request for a
            -- method set, this concretely parallelizes the expensive
            -- regionalized 'fillRegionalActivityWeights' walks (one per
            -- regio method × biosphere-triple stream). For EF 3.1 (5 regio
            -- methods over agribalyse) this trades a ~110s sequential warm-up
            -- for a ~25-30s parallel one. Concurrent cache writes on the
            -- per-method cache are idempotent under STM (last write wins,
            -- same value).
            tables <- mapConcurrently (mapMethodToTablesCachedWithHier manager dbName collection db hier) sortedMethods
            let !mst = buildMethodSetTables (zip sortedMethods tables)
            atomically $ modifyTVar' (dmMethodSetTablesCache manager) (M.insert key mst)
            pure mst

{- | Cached method index (CF tokens, by-medium, by-CAS): built once per
(db, method), reused by the post-scoring suggester. Doesn't depend on the
'Database' itself — only on the method's CF list — but keyed by (dbName,
methodId) to share lifetime semantics with the tables cache.
-}
mapMethodToIndexCached :: DatabaseManager -> Text -> Text -> Method -> IO MethodIndex
mapMethodToIndexCached manager dbName collection method = do
    let key = (dbName, collection, methodId method)
    cache <- readTVarIO (dmMethodIndexCache manager)
    case M.lookup key cache of
        Just idx -> pure idx
        Nothing -> do
            let !idx = buildMethodIndex method
            atomically $ modifyTVar' (dmMethodIndexCache manager) (M.insert key idx)
            pure idx

{- | Clear all cached flow mappings (call when databases, methods, or synonyms change).
Also drops the merged flow/unit snapshots — both caches depend on the loaded-DB set.
-}
clearMethodMappingCache :: DatabaseManager -> IO ()
clearMethodMappingCache manager = atomically $ do
    writeTVar (dmMethodMappingCache manager) M.empty
    writeTVar (dmMethodTablesCache manager) M.empty
    writeTVar (dmMethodTablesInflight manager) M.empty
    writeTVar (dmMethodSetTablesCache manager) M.empty
    writeTVar (dmMethodIndexCache manager) M.empty
    writeTVar (dmMergedFlowMetadataCache manager) Nothing
    writeTVar (dmMergedUnitConfigCache manager) Nothing

{- | Clear cached flow mappings for a specific database.
The merged flow/unit snapshots span every loaded DB, so a single-DB mutation
still invalidates them fully.
-}
clearMethodMappingCacheForDb :: DatabaseManager -> Text -> IO ()
clearMethodMappingCacheForDb manager dbName = atomically $ do
    modifyTVar' (dmMethodMappingCache manager) (M.filterWithKey (\(dn, _, _) _ -> dn /= dbName))
    modifyTVar' (dmMethodTablesCache manager) (M.filterWithKey (\(dn, _, _) _ -> dn /= dbName))
    modifyTVar' (dmMethodTablesInflight manager) (M.filterWithKey (\(dn, _, _) _ -> dn /= dbName))
    modifyTVar' (dmMethodSetTablesCache manager) (M.filterWithKey (\(dn, _, _) _ -> dn /= dbName))
    modifyTVar' (dmMethodIndexCache manager) (M.filterWithKey (\(dn, _, _) _ -> dn /= dbName))
    writeTVar (dmMergedFlowMetadataCache manager) Nothing
    writeTVar (dmMergedUnitConfigCache manager) Nothing

{- | Initialize database manager from config
Pre-loads databases with load=true at startup
Also discovers uploaded databases from uploads/ directory
-}
initDatabaseManager :: Config -> Bool -> Maybe FilePath -> IO DatabaseManager
initDatabaseManager config noCache configPath = do
    -- Resolve relative paths against the config file's directory
    let configDir = maybe "." takeDirectory configPath
        resolveRelative p = normalise $ if isAbsolute p then p else configDir </> p

    -- Get configured databases and detect their format
    configuredDbs <- forM (cfgDatabases config) $ \dbConfig -> do
        resolvedPath <- resolveDataPath (resolveRelative (dcPath dbConfig))
        format <- Upload.detectDatabaseFormat resolvedPath
        return dbConfig{dcPath = resolvedPath, dcFormat = Just format}

    -- Discover uploaded databases from uploads/ directory (self-describing with meta.toml)
    uploadedDbs <- discoverUploadedDatabases

    -- Merge configured + uploaded
    let allDbs = configuredDbs ++ uploadedDbs

    -- Create TVars
    loadedDbsVar <- newTVarIO M.empty
    stagedDbsVar <- newTVarIO M.empty
    stagingDbsVar <- newTVarIO S.empty
    indexedDbsVar <- newTVarIO M.empty
    availableDbsVar <- newTVarIO $ M.fromList [(dcName dc, dc) | dc <- allDbs]

    -- Discover uploaded methods
    uploadedMethodConfigs <- discoverUploadedMethodConfigs
    let allMethods = cfgMethods config ++ uploadedMethodConfigs
    availableMethodsVar <- newTVarIO $ M.fromList [(mcName mc, mc) | mc <- allMethods]
    loadedMethodsVar <- newTVarIO M.empty

    -- Reference data TVars (flow synonyms, compartment mappings, units)
    -- Discover uploaded reference data from uploads/<type>/ directories
    uploadedFlowSyns <- discoverUploadedRefData "uploads/flow-synonyms"
    uploadedCompMaps <- discoverUploadedRefData "uploads/compartment-mappings"
    uploadedUnitDefs <- discoverUploadedRefData "uploads/units"
    uploadedEnergyDensities <- discoverUploadedRefData "uploads/energy-densities"
    -- Resolve reference data paths relative to config directory
    let resolveRdPath rd = rd{rdPath = resolveRelative (rdPath rd)}
    let allFlowSyns = map resolveRdPath (cfgFlowSynonyms config) ++ uploadedFlowSyns
        allCompMaps = map resolveRdPath (cfgCompartmentMappings config) ++ uploadedCompMaps
        allUnitDefs = map resolveRdPath (cfgUnits config) ++ uploadedUnitDefs
        allEnergyDensities = map resolveRdPath (cfgEnergyDensities config) ++ uploadedEnergyDensities
    availableFlowSynsVar <- newTVarIO $ M.fromList [(rdName rd, rd) | rd <- allFlowSyns]
    loadedFlowSynsVar <- newTVarIO M.empty
    availableCompMapsVar <- newTVarIO $ M.fromList [(rdName rd, rd) | rd <- allCompMaps]
    loadedCompMapsVar <- newTVarIO M.empty
    availableUnitDefsVar <- newTVarIO $ M.fromList [(rdName rd, rd) | rd <- allUnitDefs]
    loadedUnitDefsVar <- newTVarIO M.empty
    availableEnergyDensitiesVar <- newTVarIO $ M.fromList [(rdName rd, rd) | rd <- allEnergyDensities]
    loadedEnergyDensitiesVar <- newTVarIO M.empty

    geographies <- case cfgGeographies config of
        Nothing -> return M.empty
        Just path -> parseGeographiesCSV (resolveRelative path)

    methodMappingCacheVar <- newTVarIO M.empty
    methodTablesCacheVar <- newTVarIO M.empty
    methodTablesInflightVar <- newTVarIO M.empty
    methodSetTablesCacheVar <- newTVarIO M.empty
    methodIndexCacheVar <- newTVarIO M.empty
    chemSyns <- case cfgChemSynonyms config of
        Nothing -> pure emptyChemSynonyms
        Just path -> do
            result <- loadChemSynonyms path
            case result of
                Right cs -> pure cs
                Left err -> do
                    putStrLn $ "warning: could not load chem synonyms from " <> path <> ": " <> err
                    pure emptyChemSynonyms
    substanceEdges <- case cfgSubstanceEdges config of
        Nothing -> pure []
        Just path -> do
            isFile <- doesFileExist path
            if not isFile
                then do
                    putStrLn $ "warning: substance edges file not found: " <> path
                    pure []
                else do
                    raw <- BL.readFile path
                    case parseSubstanceEdges (KeyNormalizers (NormName . normalizeName) (CASNumber . normalizeCAS)) raw of
                        Right es -> pure es
                        Left err -> do
                            putStrLn $ "warning: could not load substance edges from " <> path <> ": " <> T.unpack err
                            pure []
    let (substanceCasBindings, casBindingConflicts) = casBindingsFromEdges substanceEdges
    forM_ casBindingConflicts $ \(NormName n, (CASNumber c1, CASNumber c2)) ->
        putStrLn $
            "warning: substance_edges.csv binds flow name '"
                <> T.unpack n
                <> "' to two CAS ("
                <> T.unpack c1
                <> " kept, "
                <> T.unpack c2
                <> " ignored)"
    mergedFlowMetadataCacheVar <- newTVarIO Nothing
    mergedUnitConfigCacheVar <- newTVarIO Nothing

    let manager =
            DatabaseManager
                { dmLoadedDbs = loadedDbsVar
                , dmStagedDbs = stagedDbsVar
                , dmStagingDbs = stagingDbsVar
                , dmIndexedDbs = indexedDbsVar
                , dmAvailableDbs = availableDbsVar
                , dmAvailableMethods = availableMethodsVar
                , dmLoadedMethods = loadedMethodsVar
                , dmAvailableFlowSyns = availableFlowSynsVar
                , dmLoadedFlowSyns = loadedFlowSynsVar
                , dmAvailableCompMaps = availableCompMapsVar
                , dmLoadedCompMaps = loadedCompMapsVar
                , dmAvailableUnitDefs = availableUnitDefsVar
                , dmLoadedUnitDefs = loadedUnitDefsVar
                , dmAvailableEnergyDensities = availableEnergyDensitiesVar
                , dmLoadedEnergyDensities = loadedEnergyDensitiesVar
                , dmNoCache = noCache
                , dmGeographies = geographies
                , dmMethodMappingCache = methodMappingCacheVar
                , dmMethodTablesCache = methodTablesCacheVar
                , dmMethodTablesInflight = methodTablesInflightVar
                , dmMethodSetTablesCache = methodSetTablesCacheVar
                , dmMethodIndexCache = methodIndexCacheVar
                , dmChemSynonyms = chemSyns
                , dmSubstanceEdges = substanceEdges
                , dmCasBindings = substanceCasBindings
                , dmMergedFlowMetadataCache = mergedFlowMetadataCacheVar
                , dmMergedUnitConfigCache = mergedUnitConfigCacheVar
                }

    -- Auto-load active reference data (flow synonyms, compartment mappings, units)
    -- Flow synonyms use binary cache for fast loading (161K pairs → <1s vs 15s)
    reportProgress Info $
        "Loading reference data: "
            ++ show (length allUnitDefs)
            ++ " unit config(s), paths: "
            ++ unwords (map rdPath allUnitDefs)
    autoLoadFlowSynonyms loadedFlowSynsVar allFlowSyns
    autoLoadRefData compMapOps loadedCompMapsVar allCompMaps
    autoLoadRefData unitDefOps loadedUnitDefsVar allUnitDefs
    autoLoadRefData energyDensityOps loadedEnergyDensitiesVar allEnergyDensities

    totalStart <- getCurrentTime

    -- Load databases with level-based parallelism
    let allDbConfigs = allDbs
        configMap = M.fromList [(dcName c, c) | c <- allDbConfigs]
    case resolveLoadOrder allDbConfigs of
        Left err -> reportError $ "Dependency resolution failed: " <> T.unpack err
        Right loadOrder -> do
            synonymDB <- getMergedSynonymDB manager
            warnReopenedBridges synonymDB
            unitConfig <- getMergedUnitConfig manager
            let dbsToLoad = [configMap M.! name | name <- loadOrder, M.member name configMap]
                levels = computeDepLevels configMap loadOrder
            reportProgress Info $
                "Loading "
                    ++ show (length dbsToLoad)
                    ++ " database(s) in "
                    ++ show (length levels)
                    ++ " dependency levels: "
                    ++ T.unpack (T.intercalate " → " [T.intercalate "," names | names <- levels])
            forM_ (zip [1 :: Int ..] levels) $ \(levelNum, levelNames) -> do
                let levelConfigs = [configMap M.! name | name <- levelNames, M.member name configMap]
                reportProgress Info $
                    "  Level "
                        ++ show levelNum
                        ++ ": loading "
                        ++ show (length levelConfigs)
                        ++ " database(s) in parallel"
                currentIndexedDbs <- readTVarIO indexedDbsVar
                let otherIndexes = M.elems currentIndexedDbs
                mapConcurrently_ (loadOneDatabase synonymDB unitConfig noCache otherIndexes loadedDbsVar indexedDbsVar manager) levelConfigs
            loadedCount <- atomically $ M.size <$> readTVar loadedDbsVar
            reportProgress Info $ "Multi-database mode: " ++ show loadedCount ++ " database(s) loaded"

    -- Load method collections
    let activeMethods = filter mcActive (cfgMethods config)
    forM_ activeMethods $ \mc -> do
        result <- loadMethodCollectionFromConfig mc
        case result of
            Right (collection0, flowInfo) -> do
                let (collection, patchStats) = applyMethodConfig mc collection0
                atomically $ modifyTVar' loadedMethodsVar (M.insert (mcName mc) collection)
                reportProgress Info $
                    "  [OK] Loaded method: "
                        <> T.unpack (mcName mc)
                        <> " ("
                        <> show (length (mcMethods collection))
                        <> " impact categories)"
                warnZeroTouchPatches (mcName mc) patchStats
                -- Surface a 'global-methods' entry that matches no loaded method:
                -- the de-regionalization is keyed by method name, so a typo or a
                -- renamed method would otherwise be ignored in silence and the
                -- method would stay regionalized, diverging from the reference.
                let knownMethodNames = S.fromList (map methodName (mcMethods collection))
                    unknownGlobals = filter (`S.notMember` knownMethodNames) (Config.mcGlobalMethods mc)
                unless (null unknownGlobals) $
                    reportProgress Warning $
                        "  [global-methods] collection "
                            <> T.unpack (mcName mc)
                            <> ": no method named "
                            <> T.unpack (T.intercalate ", " unknownGlobals)
                            <> " — these stay regionalized; check for a typo."
                let !pairs = extractFromILCDFlows flowInfo
                autoCreateFlowSynonyms
                    manager
                    (mcName mc)
                    ("Auto-extracted from " <> mcName mc)
                    pairs
            Left err ->
                reportError $ "  [FAIL] Failed to load method " <> T.unpack (mcName mc) <> ": " <> T.unpack err

    totalEnd <- getCurrentTime
    let totalDuration = realToFrac (diffUTCTime totalEnd totalStart) :: Double
    reportProgressWithTiming Info "Total startup loading time" totalDuration

    return manager

-- | Load a single database with per-database timing, then register it
loadOneDatabase ::
    SynonymDB ->
    UnitConversion.UnitConfig ->
    Bool ->
    [IndexedDatabase] ->
    TVar (Map Text LoadedDatabase) ->
    TVar (Map Text IndexedDatabase) ->
    DatabaseManager ->
    DatabaseConfig ->
    IO ()
loadOneDatabase synonymDB unitConfig noCache otherIndexes loadedDbsVar indexedDbsVar manager dbConfig = withLogScope (dcName dbConfig) $ do
    dbStart <- getCurrentTime
    reportProgress Info $ "[STARTING] Loading database: " <> T.unpack (dcDisplayName dbConfig)
    result <- loadDatabaseFromConfigWithCrossDB dbConfig synonymDB unitConfig noCache otherIndexes (locationHierarchyOf manager)
    case result of
        Right (loaded0, _fromCache) -> do
            -- Backfill empty bfCAS from the registry's name↔CAS edges before
            -- indexing, so a CAS-less source (e.g. a SimaPro export) still
            -- reaches the native CAS bridge.
            let loaded = loaded0{ldDatabase = enrichBioFlowCAS (dmCasBindings manager) (ldDatabase loaded0)}
                indexedDb = buildIndexedDatabaseFromDB (dcName dbConfig) synonymDB (ldDatabase loaded)
            atomically $ do
                modifyTVar' loadedDbsVar (M.insert (dcName dbConfig) loaded)
                modifyTVar' indexedDbsVar (M.insert (dcName dbConfig) indexedDb)
            dbEnd <- getCurrentTime
            let !dbDuration = realToFrac (diffUTCTime dbEnd dbStart) :: Double
            reportProgressWithTiming Info ("  [OK] Loaded: " <> T.unpack (dcDisplayName dbConfig)) dbDuration
            -- Auto-extract synonyms from biosphere flows
            let db = ldDatabase loaded
                bioFlowDb = dbBioFlows db
                !pairs = extractFromEcoSpold2 bioFlowDb
                !bioFlowsWithSyns =
                    length
                        [ ()
                        | f <- M.elems bioFlowDb
                        , not (M.null (bfSynonyms f))
                        ]
            reportProgress Info $
                "  [EXTRACT] "
                    <> T.unpack (dcName dbConfig)
                    <> ": "
                    <> show (M.size bioFlowDb)
                    <> " bio flows, "
                    <> show bioFlowsWithSyns
                    <> " with synonyms, "
                    <> show (length pairs)
                    <> " pairs"
            -- A biosphere flow whose name carries a "/unit" suffix that
            -- 'normalizeName' does not strip silently misses its CF (the SimaPro
            -- unit-in-name convention; e.g. a "/MJ" absent from 'unitSuffixes').
            -- Surface it so the fix — add the unit to 'unitSuffixes' — is visible.
            let uncoveredUnits =
                    uncoveredUnitSuffixes
                        (UnitConversion.isKnownUnit unitConfig)
                        (map bfName (M.elems bioFlowDb))
            forM_ (M.toList uncoveredUnits) $ \(unit, egs) ->
                reportProgress Warning $
                    "  [UNIT] "
                        <> T.unpack (dcName dbConfig)
                        <> ": flow-name suffix /"
                        <> T.unpack unit
                        <> " not stripped on "
                        <> show (length egs)
                        <> " flows (add \"/"
                        <> T.unpack (T.toLower unit)
                        <> "\" to unitSuffixes); e.g. "
                        <> T.unpack (T.intercalate ", " (take 3 egs))
            autoCreateFlowSynonyms
                manager
                (dcName dbConfig)
                ("Auto-extracted from " <> dcDisplayName dbConfig)
                pairs
        Left err ->
            reportError $ "  [FAIL] Failed to load " <> T.unpack (dcName dbConfig) <> ": " <> T.unpack err

{- | Compute dependency levels from topo-sorted load order for parallel loading.
  Level 0 = no deps, level N = depends only on levels 0..N-1.
-}
computeDepLevels :: Map Text DatabaseConfig -> [Text] -> [[Text]]
computeDepLevels configMap loadOrder =
    let
        -- Compute level for each name: max(levels of deps) + 1, or 0 if no deps
        levelOf :: Map Text Int -> Text -> Int
        levelOf lvls name = case M.lookup name configMap of
            Nothing -> 0
            Just cfg -> case dcDepends cfg of
                [] -> 0
                deps -> 1 + maximum [M.findWithDefault 0 d lvls | d <- deps]
        -- Fold through topo-sorted order to assign levels
        levels' = foldl (\acc name -> M.insert name (levelOf acc name) acc) M.empty loadOrder
        -- Group by level
        maxLevel = if M.null levels' then 0 else maximum (M.elems levels')
     in
        [[name | name <- loadOrder, M.findWithDefault 0 name levels' == lvl] | lvl <- [0 .. maxLevel]]

{- | Discover uploaded databases from uploads/ directory
Reads meta.toml from each subdirectory and converts to DatabaseConfig
-}
discoverUploadedDatabases :: IO [DatabaseConfig]
discoverUploadedDatabases = do
    uploads <- UploadedDB.discoverUploadedDatabases
    forM uploads $ \(slug, dirPath, meta) -> do
        reportProgress Info $ "Discovered uploaded database: " <> T.unpack slug
        -- Always detect format from actual files (old uploads may have "unknown")
        let dataDir = dirPath </> UploadedDB.umDataPath meta
        format <- Upload.detectDatabaseFormat dataDir
        return $ uploadMetaToConfig slug dirPath meta{UploadedDB.umFormat = format}

-- | Convert UploadMeta to DatabaseConfig
uploadMetaToConfig :: Text -> FilePath -> UploadedDB.UploadMeta -> DatabaseConfig
uploadMetaToConfig slug dirPath meta =
    DatabaseConfig
        { dcName = slug
        , dcDisplayName = UploadedDB.umDisplayName meta
        , dcPath = dirPath </> UploadedDB.umDataPath meta -- Full path to data
        , dcDescription = UploadedDB.umDescription meta
        , dcLoad = False -- Never auto-load uploads
        , dcDefault = False
        , dcDepends = []
        , dcLocationAliases = M.empty
        , dcFormat = Just (UploadedDB.umFormat meta)
        , dcIsUploaded = True -- Discovered from uploads/ directory
        , dcDeletable = True
        , dcGeographyPolicy = GeoGlobal -- Uploads can't yet express policy; default to permissive
        }

{- | Discover uploaded methods from uploads/methods/ directory
Reads meta.toml from each subdirectory and converts to MethodConfig
| Convert a ScoringSetConfig to a ScoringSet
-}
configToScoringSet :: ScoringSetConfig -> ScoringSet
configToScoringSet ssc =
    ScoringSet
        { ssName = sscName ssc
        , ssUnit = sscUnit ssc
        , ssVariables = sscVariables ssc
        , ssComputed = sscComputed ssc
        , ssLabels = sscLabels ssc
        , ssNormalization = sscNormalization ssc
        , ssWeighting = sscWeighting ssc
        , ssScores = sscScores ssc
        , ssDisplayMultiplier = sscDisplayMultiplier ssc
        }

{- | Fold a 'MethodConfig's post-parse adjustments into a freshly parsed
collection: inject the configured scoring sets, then apply the declarative
CF patches ('Config.mcPatches'). Pure — reapplying the same config to the
same source file always yields the same result, so a reload never
compounds a patch. Also returns, per patch, how many CFs it touched (for
the zero-touch warning at the call site).
-}
applyMethodConfig :: MethodConfig -> MethodCollection -> (MethodCollection, [(Config.MethodPatch, Int)])
applyMethodConfig mc collection0 =
    let scoringSets = map configToScoringSet (Config.mcScoringSets mc)
        withScoring = collection0{Method.Types.mcScoringSets = scoringSets}
     in Method.Patch.applyMethodPatches (Config.mcPatches mc) withScoring

{- | Surface a patch that matched no characterization factor: the selector is
almost certainly wrong (a typo'd category or flow name), and staying silent
would leave the collection scoring as if the patch were never declared.
-}
warnZeroTouchPatches :: Text -> [(Config.MethodPatch, Int)] -> IO ()
warnZeroTouchPatches collName stats =
    forM_ [p | (p, n) <- stats, n == 0] $ \patch ->
        reportProgress Warning $
            "  [patch] collection "
                <> T.unpack collName
                <> ": \""
                <> T.unpack (Method.Patch.describePatch patch)
                <> "\" touched 0 characterization factors — check the selector."

discoverUploadedMethodConfigs :: IO [MethodConfig]
discoverUploadedMethodConfigs = do
    uploads <- UploadedDB.discoverUploadedMethods
    forM uploads $ \(slug, dirPath, meta) -> do
        reportProgress Info $ "Discovered uploaded method: " <> T.unpack slug
        -- Find the actual method XML directory (e.g., ILCD/lciamethods/)
        methodDir <- findMethodDirectory dirPath
        -- Read the format off the directory rather than meta.toml: the file on
        -- disk may predate method-aware detection, and can't drift this way.
        methodFormat <- detectMethodFormat methodDir
        return
            MethodConfig
                { mcName = UploadedDB.umDisplayName meta
                , mcPath = methodDir
                , mcActive = False -- Never auto-load uploaded methods
                , mcIsUploaded = True
                , mcDescription = UploadedDB.umDescription meta
                , mcFormat = detectedFormatLabel methodFormat
                , mcScoringSets = []
                , mcGlobalMethods = []
                , mcPatches = []
                }

-- | Get a database by name
getDatabase :: DatabaseManager -> Text -> IO (Maybe LoadedDatabase)
getDatabase manager dbName = do
    loadedDbs <- readTVarIO (dmLoadedDbs manager)
    return $ M.lookup dbName loadedDbs

{- | Build a 'DepSolverLookup' backed by the manager's loaded-databases map.
Passed to 'SharedSolver.computeInventoryMatrixBatchWithDepsCached' so it can
recurse into cross-database suppliers.
-}
mkDepSolverLookup :: DatabaseManager -> SharedSolver.DepSolverLookup
mkDepSolverLookup manager depDbName = do
    m <- getDatabase manager depDbName
    pure $ fmap (\ld -> (ldDatabase ld, ldSharedSolver ld)) m

-- | List all databases with their status
listDatabases :: DatabaseManager -> IO [DatabaseStatus]
listDatabases manager = do
    availableDbs <- readTVarIO (dmAvailableDbs manager)
    loadedDbs <- readTVarIO (dmLoadedDbs manager)

    forM (M.toList availableDbs) $ \(name, config) -> do
        let mLoaded = M.lookup name loadedDbs
            !status = case mLoaded of
                Nothing -> Unloaded
                Just ld
                    | unresolvedCount (dbLinkingStats (ldDatabase ld)) > 0 -> PartiallyLinked
                    | otherwise -> Loaded
            !actCount = maybe 0 (V.length . dbActivities . ldDatabase) mLoaded
        return
            DatabaseStatus
                { dsName = name
                , dsDisplayName = dcDisplayName config
                , dsDescription = dcDescription config
                , dsLoadAtStartup = dcLoad config
                , dsStatus = status
                , dsIsUploaded = dcIsUploaded config
                , dsPath = T.pack (dcPath config)
                , dsFormat = dcFormat config
                , dsActivityCount = actCount
                , dsDependsOn = dcDepends config
                }

{- | Load a database from its configuration (without cross-DB linking)
This is the original function, kept for backward compatibility
-}
loadDatabaseFromConfig :: DatabaseConfig -> SynonymDB -> Bool -> IO (Either Text LoadedDatabase)
loadDatabaseFromConfig dbConfig synonymDB noCache =
    fmap
        (fmap fst)
        (loadDatabaseFromConfigWithCrossDB dbConfig synonymDB UnitConversion.defaultUnitConfig noCache [] M.empty)

-- | File extensions 'resolveDataPath' knows how to extract as archives.
archiveExtensions :: [String]
archiveExtensions = [".zip", ".7z", ".gz", ".xz"]

{- | Resolve a database path: if it's an archive, extract it first.
Extracts to "{archivePath}.d/" and finds the actual data directory inside.
Plain files/directories pass through unchanged.
-}
resolveDataPath :: FilePath -> IO FilePath
resolveDataPath path = do
    isDir <- doesDirectoryExist path
    if isDir
        then return path
        else do
            isFile <- doesFileExist path
            if not isFile
                then return path -- missing: let caller handle
                else
                    let ext = map toLower (takeExtension path)
                     in if ext `elem` archiveExtensions
                            then extractAndFind path
                            else return path
  where
    extractAndFind archive = do
        let extractDir = archive ++ ".d"
        dirExists <- doesDirectoryExist extractDir
        alreadyExtracted <-
            if dirExists
                then not . null <$> listDirectory extractDir
                else return False
        if alreadyExtracted
            then do
                reportProgress Info $ "Using cached extraction: " <> extractDir
                Upload.findDataDirectory extractDir
            else do
                createDirectoryIfMissing True extractDir
                reportProgress Info $ "Extracting archive: " <> archive
                result <- Upload.extractArchiveFile archive extractDir
                case result of
                    Left err -> do
                        reportError $ "Archive extraction failed: " <> T.unpack err
                        return archive -- let caller report the meaningful error
                    Right () -> do
                        reportProgress Info "Extraction complete"
                        Upload.findDataDirectory extractDir

-- | Load a database from its configuration with cross-database linking support
loadDatabaseFromConfigWithCrossDB ::
    DatabaseConfig ->
    SynonymDB ->
    UnitConversion.UnitConfig ->
    Bool -> -- noCache
    [IndexedDatabase] -> -- Pre-built indexes from other databases for cross-DB linking
    M.Map Location [Location] -> -- Location hierarchy (empty = use built-in)
    IO (Either Text (LoadedDatabase, Bool))
loadDatabaseFromConfigWithCrossDB dbConfig synonymDB unitConfig noCache otherIndexes locationHier = do
    let sourcePath = dcPath dbConfig
        locationAliases = dcLocationAliases dbConfig
    reportProgress Info $ "Loading database from: " <> sourcePath
    dbResult <- loadDatabaseRawWithCrossDB (dcName dbConfig) locationAliases sourcePath noCache synonymDB unitConfig otherIndexes locationHier (dcGeographyPolicy dbConfig)

    case dbResult of
        Left err -> return $ Left err
        Right (dbRaw, fromCache) -> do
            -- Initialize runtime fields (synonym DB and flow name index)
            let database = BM25.addBM25Index (initializeRuntimeFields dbRaw synonymDB)

                -- Create shared solver with lazy factorization (deferred to first query)
                techTriples = dbTechnosphereTriples database
                activityCount = dbActivityCount database
                techTriplesInt = [(fromIntegral i, fromIntegral j, v) | SparseTriple i j v <- U.toList techTriples]
                activityCountInt = fromIntegral activityCount
            sharedSolver <- createSharedSolver (dcName dbConfig) techTriplesInt activityCountInt

            return $
                Right
                    ( LoadedDatabase
                        { ldDatabase = database
                        , ldSharedSolver = sharedSolver
                        , ldConfig = dbConfig
                        }
                    , fromCache
                    )

-- | Detected format of a database directory
data DirectoryFormat = FormatSpold | FormatXML | FormatCSV | FormatILCD | FormatUnknown
    deriving (Show, Eq)

-- | Detect the format of files in a directory
detectDirectoryFormat :: FilePath -> IO DirectoryFormat
detectDirectoryFormat path = do
    isDir <- doesDirectoryExist path
    isFile <- doesFileExist path
    if isFile
        then do
            -- Direct file: check extension
            let ext = map toLower (takeExtension path)
            return $ case ext of
                ".csv" -> FormatCSV
                ".spold" -> FormatSpold
                ".xml" -> FormatXML
                _ -> FormatUnknown
        else
            if isDir
                then do
                    -- Check for ILCD format first (has processes/ subdirectory)
                    hasProcesses <- doesDirectoryExist (path </> "processes")
                    if hasProcesses
                        then return FormatILCD
                        else do
                            -- EcoSpold packages keep their datasets in a
                            -- subdirectory (e.g. ecoinvent's datasets/*.spold),
                            -- so probe for .spold recursively. Otherwise a
                            -- sibling FilenameToActivityLookup.csv at the package
                            -- root masks them and the database misdetects as
                            -- SimaPro CSV, silently loading zero activities.
                            hasSpold <- containsExtensionDeep ".spold" path
                            if hasSpold
                                then return FormatSpold
                                else do
                                    files <- listDirectory path
                                    let extensions = map (map toLower . takeExtension) files
                                    -- Check for remaining formats (in order of preference)
                                    if ".csv" `elem` extensions
                                        then return FormatCSV
                                        else
                                            if ".xml" `elem` extensions
                                                then return FormatXML
                                                else return FormatUnknown
                else return FormatUnknown

{- | Recursively test whether the directory tree rooted at @path@ contains at
least one file with the given (lowercased) extension. Lets dataset files in a
subdirectory drive format detection even when an unrelated file sits at the
package root (e.g. ecoinvent's datasets/*.spold beside a root CSV).
-}
containsExtensionDeep :: String -> FilePath -> IO Bool
containsExtensionDeep ext =
    fmap (any ((== ext) . map toLower . takeExtension)) . listDirectoryRecursive

-- | Find CSV files in a directory
findCSVFiles :: FilePath -> IO [FilePath]
findCSVFiles path = do
    files <- listDirectory path
    let csvFiles = filter (\f -> map toLower (takeExtension f) == ".csv") files
    return $ map (path </>) csvFiles

{- | Build activity map from list of activities
Creates (activityUUID, productUUID) -> Activity mapping
-}
buildActivityMap :: [Activity] -> M.Map (UUID, UUID) Activity
buildActivityMap activities =
    M.fromList
        [ ((activityUUID, productUUID), activity)
        | activity <- activities
        , let activityUUID = SimaPro.generateActivityUUID activity
        , let refExchanges = filter exchangeIsReference (exchanges activity)
        , refExchange <- take 1 refExchanges -- Take first reference product
        , let productUUID = exchangeFlowId refExchange
        ]

{- | Load raw database from a configured source path, with cross-database linking.

The cache lives next to @sourcePath@ (see 'Loader.generateMatrixCacheFilename').
We probe it first using the unresolved @sourcePath@, so a deployment that ships
only the cache (no source archive on disk) still loads. On cache miss/stale we
'resolveDataPath' and parse, saving a fresh cache on success.
-}
loadDatabaseRawWithCrossDB ::
    -- | Database name
    T.Text ->
    -- | Location aliases
    M.Map T.Text T.Text ->
    -- | Source path (unresolved; cache is co-located with it)
    FilePath ->
    -- | noCache flag
    Bool ->
    -- | Synonym database
    SynonymDB ->
    -- | Unit configuration
    UnitConversion.UnitConfig ->
    -- | Pre-built indexes from other databases
    [IndexedDatabase] ->
    -- | Location hierarchy (empty = use built-in)
    M.Map Location [Location] ->
    -- | Geography policy for this database
    GeographyPolicy ->
    {- | (Database, fromCache): True iff the result came from the matrix cache
    as-is, i.e. cross-DB linking was NOT freshly run against 'otherIndexes'.
    Callers use this to decide whether a self-relink is needed.
    -}
    IO (Either Text (Database, Bool))
loadDatabaseRawWithCrossDB dbName locationAliases sourcePath noCache synonymDB unitConfig otherIndexes locationHier policy = do
    mCachedDb <-
        if noCache
            then return Nothing
            else Loader.loadCachedDatabaseWithMatrices dbName sourcePath
    let cacheUsable = case mCachedDb of
            Just db
                | unresolvedCount (dbLinkingStats db) > 0
                , not (null otherIndexes) ->
                    False -- stale: deps now available
            Just _ -> True
            Nothing -> False
    case (cacheUsable, mCachedDb) of
        (True, Just db) -> do
            Loader.reportCrossDBLinkingStats (fromIntegral (dbActivityCount db)) (dbLinkingStats db)
            return $ Right (db, True)
        _ -> do
            when (isJust mCachedDb && not cacheUsable) $
                reportProgress Info "Cache has unresolved links, rebuilding with available dependencies..."
            -- Cache miss/stale: now we need the source. Resolve archive if any.
            path <- resolveDataPath sourcePath
            isFile <- doesFileExist path
            isDir <- doesDirectoryExist path
            if not isFile && not isDir
                then return $ Left $ "Source path does not exist: " <> T.pack sourcePath
                else do
                    format <- detectDirectoryFormat path
                    case format of
                        FormatCSV -> loadCSV path
                        FormatUnknown ->
                            return $
                                Left $
                                    "No supported database files found in: "
                                        <> T.pack path
                                        <> ". Supported formats: EcoSpold v2 (.spold), EcoSpold v1 (.xml), SimaPro CSV (.csv), ILCD"
                        _ -> loadStructured path
  where
    loadCSV path = do
        mCsvFile <-
            doesFileExist path >>= \isFileCheck ->
                if isFileCheck
                    then return (Right path)
                    else do
                        csvFiles <- findCSVFiles path
                        case csvFiles of
                            [] -> return $ Left $ "No CSV files found in: " <> T.pack path
                            (f : _) -> return (Right f)
        case mCsvFile of
            Left err -> return $ Left err
            Right csvFile -> do
                reportProgress Info $ "Parsing SimaPro CSV: " <> csvFile
                (activities, techFlowDB, bioFlowDB, wasteFlowDB, unitDB) <- SimaPro.parseSimaProCSV unitConfig csvFile
                reportProgress Info $ "Building database from " <> show (length activities) <> " activities"
                let simpleDb = SimpleDatabase (buildActivityMap activities) techFlowDB bioFlowDB wasteFlowDB unitDB
                linkedDb <- Loader.fixSimaProActivityLinks unitConfig simpleDb
                dbResult <- buildDatabaseWithMatrices unitConfig (sdbActivities linkedDb) techFlowDB bioFlowDB (sdbWasteFlows linkedDb) unitDB
                case dbResult of
                    Left err -> return $ Left err
                    Right db -> do
                        unless noCache $
                            Loader.saveCachedDatabaseWithMatrices dbName sourcePath db
                        Loader.reportCrossDBLinkingStats (fromIntegral (dbActivityCount db)) (dbLinkingStats db)
                        return $ Right (db, False)

    loadStructured path = do
        loadResult <-
            Loader.loadDatabaseWithCrossDBLinking
                locationAliases
                otherIndexes
                synonymDB
                unitConfig
                locationHier
                policy
                path
        case loadResult of
            Left err -> return $ Left err
            Right (simpleDb, stats) -> do
                dbResult <-
                    buildDatabaseWithMatrices
                        unitConfig
                        (sdbActivities simpleDb)
                        (sdbTechFlows simpleDb)
                        (sdbBioFlows simpleDb)
                        (sdbWasteFlows simpleDb)
                        (sdbUnits simpleDb)
                case dbResult of
                    Left err -> return $ Left err
                    Right db -> do
                        let crossLinks = cdlLinks stats
                            depDbs = M.keys (crossDBBySource stats)
                            dbWithLinks =
                                db
                                    { dbCrossDBLinks = crossLinks
                                    , dbDependsOn = depDbs
                                    , dbLinkingStats = stats
                                    }
                        unless noCache $
                            Loader.saveCachedDatabaseWithMatrices dbName sourcePath dbWithLinks
                        return $ Right (dbWithLinks, False)

-- | Load a single database without auto-loading dependencies
loadDatabaseSingle :: DatabaseManager -> Text -> IO (Either Text LoadedDatabase)
loadDatabaseSingle manager dbName = do
    -- Check if already staged -> try to finalize, or clear stale staged entry
    stagedDbs <- readTVarIO (dmStagedDbs manager)
    case M.lookup dbName stagedDbs of
        Just staged
            -- Same readiness gate as finalize itself, so the shortcut and the
            -- gate can't disagree
            | isNothing (notReadyReason (stagedLinkCounts staged)) ->
                finalizeDatabase manager dbName
            | otherwise -> do
                -- Cannot finalize: clear staged entry, reload from config
                -- (loadDatabase pre-loaded deps, so fresh load should resolve links)
                atomically $ modifyTVar' (dmStagedDbs manager) (M.delete dbName)
                loadDatabaseSingleFromConfig manager dbName
        Nothing -> loadDatabaseSingleFromConfig manager dbName

-- | Load a database from config (not staged)
loadDatabaseSingleFromConfig :: DatabaseManager -> Text -> IO (Either Text LoadedDatabase)
loadDatabaseSingleFromConfig manager dbName = do
    -- Check if already loaded
    loadedDbs <- readTVarIO (dmLoadedDbs manager)
    case M.lookup dbName loadedDbs of
        Just loaded -> return $ Right loaded
        Nothing -> do
            -- Check if it's configured
            availableDbs <- readTVarIO (dmAvailableDbs manager)
            case M.lookup dbName availableDbs of
                Nothing -> return $ Left $ "Database not found: " <> dbName
                Just dbConfig -> do
                    reportProgress Info $ "[STARTING] Loading database: " <> T.unpack (dcDisplayName dbConfig)
                    -- Get currently loaded IndexedDatabases for cross-DB linking
                    currentIndexedDbs <- readTVarIO (dmIndexedDbs manager)
                    let otherIndexes = M.elems currentIndexedDbs
                    synonymDB <- getMergedSynonymDB manager
                    warnReopenedBridges synonymDB
                    unitConfig <- getMergedUnitConfig manager
                    eitherResult <-
                        try $
                            loadDatabaseFromConfigWithCrossDB
                                dbConfig
                                synonymDB
                                unitConfig
                                (dmNoCache manager)
                                otherIndexes
                                (locationHierarchyOf manager)
                    case eitherResult of
                        Left (ex :: SomeException) -> return $ Left $ "Exception loading database: " <> T.pack (show ex)
                        Right (Left err) -> return $ Left err
                        Right (Right (loaded, fromCache)) -> do
                            let indexedDb = buildIndexedDatabaseFromDB dbName synonymDB (ldDatabase loaded)
                            atomically $ do
                                modifyTVar' (dmLoadedDbs manager) (M.insert dbName loaded)
                                modifyTVar' (dmIndexedDbs manager) (M.insert dbName indexedDb)
                            clearMethodMappingCacheForDb manager dbName
                            reportProgress Info $ "  [OK] Loaded:" <> T.unpack (dcDisplayName dbConfig)
                            -- Auto-extract synonyms from biosphere flows
                            let db = ldDatabase loaded
                                pairs = extractFromEcoSpold2 (dbBioFlows db)
                            autoCreateFlowSynonyms
                                manager
                                dbName
                                ("Auto-extracted from " <> dcDisplayName dbConfig)
                                pairs
                            -- Self-relink only on cache hits. On a fresh
                            -- parse, 'loadDatabaseRawWithCrossDB' already ran
                            -- linking against the current 'otherIndexes' via
                            -- 'loadStructured' / 'loadCSV', so a follow-up
                            -- relink is guaranteed no-op work. On a cache
                            -- hit the cached DB carries links computed
                            -- against a previous dep set — possibly stale
                            -- versions of the same dep names — so a relink
                            -- is required to converge.
                            when fromCache $ do
                                result <- relinkDatabase manager dbName
                                case result of
                                    Right _ -> return ()
                                    Left err ->
                                        reportProgress Warning $
                                            "Self-relink of " <> T.unpack dbName <> " failed: " <> T.unpack err
                            relinkDependents manager dbName
                            return $ Right loaded

-- | Result of a relink operation (unresolved counts before/after).
data RelinkResult = RelinkResult
    { rresDbName :: !Text
    , rresUnresolvedBefore :: !Int
    , rresUnresolvedAfter :: !Int
    , rresCrossDBLinks :: !Int
    , rresDepsLoaded :: ![Text]
    , rresLinksChanged :: !Bool
    {- ^ True iff the relink actually changed 'dbCrossDBLinks' (as a set)
    versus the in-memory state before the call. Callers use this to skip
    redundant work — e.g. the explicit cache write in 'finalizeDatabase'
    is suppressed when the relink already saved.
    -}
    }
    deriving (Show, Eq)

{- | Order-insensitive equality for lists that are semantically sets
(cross-DB links, dependency names). Avoids spurious cache re-saves when
only the element order differs.
-}
sameSet :: (Ord a) => [a] -> [a] -> Bool
sameSet xs ys = S.fromList xs == S.fromList ys

{- | Re-run cross-DB linking for an already-loaded DB against its pinned
dependency set ('dbDependsOn'), not the full set of loaded DBs. Updates
'dbCrossDBLinks' and 'dbLinkingStats' in place in the LoadedDatabase record;
the dependency set itself is left untouched (strict pin — it changes only via
explicit add/remove-dependency). Does NOT rebuild the technosphere matrix or
invalidate the MUMPS factorization — cross-DB links are consumed only at
solve time.

Side-effect: persists the updated 'Database' back to its matrix-cache file
('Loader.saveCachedDatabaseWithMatrices') whenever the relink actually
changed 'dbCrossDBLinks'. Without this, the next startup
would re-load the stale cache and re-run cross-DB linking from scratch
even though we already know the answer. The save is skipped when the
relink is a no-op (no change vs. the in-memory state).
-}
relinkDatabase :: DatabaseManager -> Text -> IO (Either Text RelinkResult)
relinkDatabase manager dbName = relinkDatabaseWith manager dbName CrossLinking.emptyAliasMap Nothing

{- | Re-link a loaded DB across its full pinned dependency set, applying a
curated supplier-alias map. The aliases let a consumer's input flow name that
only matches a target supplier (typically in @depDb@) under the mapping still
link; links to the other pinned dependencies are re-resolved unchanged rather
than dropped. If @depDb@ is loaded but not yet in the database's declared
dependency set, it is pinned in-memory first — so an in-memory pipeline
(copy → delete → relink) composes without restaging (which would unload the
live database). Same persistence/no-op semantics as 'relinkDatabase'. Errors
(DB or dep not loaded) surface as 'Left'.
-}
relinkDatabaseWithMapping ::
    DatabaseManager ->
    -- | database to relink
    Text ->
    -- | dependency database to link against
    Text ->
    -- | consumer-flow → designated-supplier aliases
    CrossLinking.AliasMap ->
    IO (Either Text RelinkResult)
relinkDatabaseWithMapping manager dbName depDb aliases = withLogScope dbName $ do
    loadedDbs <- readTVarIO (dmLoadedDbs manager)
    case M.lookup dbName loadedDbs of
        Nothing -> relinkStaged manager dbName (Just depDb) aliases
        Just loaded
            | not (M.member depDb loadedDbs) ->
                return $ Left $ "Dependency database not loaded: " <> depDb <> " (load it first)"
            | otherwise -> do
                -- Declare the dependency in-memory if it isn't already pinned, so an
                -- in-memory pipeline (copy → delete → relink) composes in one pass
                -- without restaging — which would unload the live database. The pin
                -- set on disk is the current one *before* this in-memory addition;
                -- pass it so 'relinkDatabaseWith' persists the cache when the pin
                -- diverges from disk even if no new links are discovered.
                let persistedDeps = dbDependsOn (ldDatabase loaded)
                unless (depDb `elem` persistedDeps) $
                    atomically $
                        modifyTVar' (dmLoadedDbs manager) (M.adjust (addPinnedDep depDb) dbName)
                relinkDatabaseWith manager dbName aliases (Just persistedDeps)
  where
    -- Idempotent: a concurrent relink may have pinned the dep between the
    -- snapshot above and this transaction, so never prepend a duplicate.
    addPinnedDep dep ld =
        let db = ldDatabase ld
         in if dep `elem` dbDependsOn db
                then ld
                else ld{ldDatabase = db{dbDependsOn = dep : dbDependsOn db}}

{- | Relink a *staged* (parsed-but-not-finalized) database, mirroring
'relinkDatabaseWith' on the staged 'SimpleDatabase' via the shared
'Loader.relinkSimpleDatabase'. This lets the relink endpoint work from the setup
page before a database is finalized. @maybeDepDb@ pins a chosen dependency (a
mapping relink); @aliases@ feeds the supplier-alias map.
-}
relinkStaged :: DatabaseManager -> Text -> Maybe Text -> CrossLinking.AliasMap -> IO (Either Text RelinkResult)
relinkStaged manager dbName maybeDepDb aliases = withLogScope dbName $ do
    stagedDbs <- readTVarIO (dmStagedDbs manager)
    case M.lookup dbName stagedDbs of
        Nothing -> return $ Left $ "Database not loaded: " <> dbName
        Just staged -> do
            indexedDbs <- readTVarIO (dmIndexedDbs manager)
            case maybeDepDb of
                Just dep
                    | not (M.member dep indexedDbs) ->
                        return $ Left $ "Dependency database not loaded: " <> dep <> " (load it first)"
                _ -> do
                    synonymDB <- getMergedSynonymDB manager
                    unitConfig <- getMergedUnitConfig manager
                    let pinnedDeps = case maybeDepDb of
                            Just dep | dep `notElem` sdSelectedDeps staged -> dep : sdSelectedDeps staged
                            _ -> sdSelectedDeps staged
                        selectedIndexes = [idx | (n, idx) <- M.toList indexedDbs, n `elem` pinnedDeps]
                        beforeLinks = sdCrossDBLinks staged
                        newStats =
                            Loader.relinkSimpleDatabase
                                selectedIndexes
                                synonymDB
                                unitConfig
                                (locationHierarchyOf manager)
                                (dcGeographyPolicy (sdConfig staged))
                                aliases
                                (sdSimpleDB staged)
                        newLinks = Loader.cdlLinks newStats
                        updatedStaged =
                            staged
                                { sdSelectedDeps = pinnedDeps
                                , sdCrossDBLinks = newLinks
                                , sdLinkingStats = newStats
                                , sdMissingProducts = stagedMissingProducts (sdSimpleDB staged) newStats
                                }
                    atomically $ modifyTVar' (dmStagedDbs manager) (M.insert dbName updatedStaged)
                    return $
                        Right
                            RelinkResult
                                { rresDbName = dbName
                                , rresUnresolvedBefore = unresolvedCount (sdLinkingStats staged)
                                , rresUnresolvedAfter = unresolvedCount newStats
                                , rresCrossDBLinks = length newLinks
                                , rresDepsLoaded = pinnedDeps
                                , rresLinksChanged = not (sameSet newLinks beforeLinks)
                                }

{- | Shared relink core. Candidates are the database's full declared pin
('dbDependsOn'); relink recomputes the links within it but never grows or
shrinks the set. @aliases@ feeds 'lcSupplierAliases' — a mapping relink passes
the user's curated map (which retargets a chosen dependency without dropping
links to the others), a plain relink passes 'emptyAliasMap'. The dependency
set stored on the database is never mutated here.

@persistedDeps@ is the dependency set as it stands in the matrix cache on disk
('Just' when the caller pinned a new dep in-memory before calling). The cache
is the only durable store of 'dbDependsOn', so a pin that yields zero new links
must still be written; comparing the live pin against @persistedDeps@ surfaces
that divergence. 'Nothing' means the pin is unchanged from disk.
-}
relinkDatabaseWith ::
    DatabaseManager ->
    Text ->
    CrossLinking.AliasMap ->
    Maybe [Text] ->
    IO (Either Text RelinkResult)
relinkDatabaseWith manager dbName aliases persistedDeps = withLogScope dbName $ do
    loadedDbs <- readTVarIO (dmLoadedDbs manager)
    case M.lookup dbName loadedDbs of
        Nothing -> relinkStaged manager dbName Nothing aliases
        Just loaded -> do
            indexedDbs <- readTVarIO (dmIndexedDbs manager)
            -- Strict pin: candidates are restricted to the database's declared
            -- dependency set ('dbDependsOn'), never the full set of loaded DBs.
            -- This keeps the user's explicit selection authoritative — relink
            -- recomputes links *within* the whole pin (so a mapping relink against
            -- one dependency re-resolves the others unchanged instead of dropping
            -- them) but never expands or shrinks the set.
            let pinnedDeps = dbDependsOn (ldDatabase loaded)
                otherIndexes = [idb | (n, idb) <- M.toList indexedDbs, n /= dbName, n `elem` pinnedDeps]
            synonymDB <- getMergedSynonymDB manager
            unitConfig <- getMergedUnitConfig manager
            let db = ldDatabase loaded
                beforeUnresolved = unresolvedCount (dbLinkingStats db)
                beforeLinks = dbCrossDBLinks db
                beforeDeps = dbDependsOn db
                activityMap =
                    M.fromList
                        [ (dbProcessIdTable db V.! i, dbActivities db V.! i)
                        | i <- [0 .. V.length (dbActivities db) - 1]
                        ]
                ctx =
                    LinkingContext
                        { lcIndexedDatabases = otherIndexes
                        , lcSynonymDB = synonymDB
                        , lcUnitConfig = unitConfig
                        , lcThreshold = defaultLinkingThreshold
                        , lcLocationHierarchy = locationHierarchyOf manager
                        , lcGeographyPolicy = dcGeographyPolicy (ldConfig loaded)
                        , lcSupplierAliases = aliases
                        }
                !totalInputs = Loader.countTotalTechInputs (toSimpleDatabase db)
                rawStats =
                    Loader.findAllCrossDBLinks
                        ctx
                        (dbTechFlows db)
                        (dbWasteFlows db)
                        (dbUnits db)
                        activityMap
                newStats = rawStats{cdlTotalInputs = totalInputs}
                newLinks = cdlLinks newStats
                -- Strict pin: the dependency set is the user's selection,
                -- unchanged by relinking. Only the links within it are refreshed.
                newDeps = beforeDeps
                !db' =
                    db
                        { dbCrossDBLinks = newLinks
                        , dbDependsOn = newDeps
                        , dbLinkingStats = newStats
                        }
                !loaded' = loaded{ldDatabase = db'}
                afterUnresolved = unresolvedCount newStats
                -- The pin is invariant under relink (newDeps == beforeDeps), so a
                -- change can only be in the links. Compare as sets: link order is
                -- not significant and must not trigger a redundant cache write.
                linksChanged = not (sameSet newLinks beforeLinks)
                -- A caller may have pinned a new dependency in-memory before this
                -- call. The cache is the only durable store of 'dbDependsOn', so
                -- if the live pin diverges from what is on disk the cache must be
                -- rewritten even when no new links were discovered.
                depsChanged = maybe False (not . sameSet newDeps) persistedDeps
                cacheChanged = linksChanged || depsChanged
            atomically $ do
                modifyTVar' (dmLoadedDbs manager) (M.insert dbName loaded')
                modifyTVar'
                    (dmIndexedDbs manager)
                    (M.insert dbName (buildIndexedDatabaseFromDB dbName synonymDB db'))
            clearMethodMappingCacheForDb manager dbName
            -- Persist the relinked Database back to its matrix cache so the
            -- next startup doesn't have to re-discover the same links or lose
            -- the pin.
            when cacheChanged $
                Loader.saveCachedDatabaseWithMatrices
                    dbName
                    (dcPath (ldConfig loaded'))
                    db'
            -- Skip the log when the relink was a verification no-op: links
            -- and deps already matched the in-memory state. This is the
            -- common case for warm Loads after the previous commits and
            -- carries no information worth a log line.
            when cacheChanged $
                reportProgress Info $
                    "Re-linked "
                        <> T.unpack dbName
                        <> ": "
                        <> show beforeUnresolved
                        <> " \8594 "
                        <> show afterUnresolved
                        <> " unresolved products ("
                        <> show (length newLinks)
                        <> " cross-DB links)"
            return $
                Right
                    RelinkResult
                        { rresDbName = dbName
                        , rresUnresolvedBefore = beforeUnresolved
                        , rresUnresolvedAfter = afterUnresolved
                        , rresCrossDBLinks = length newLinks
                        , rresDepsLoaded = newDeps
                        , rresLinksChanged = linksChanged
                        }

{- | After a DB loads (or reloads), re-link every already-loaded DB that
declares it as a dependency. This makes cross-DB linking converge
automatically regardless of load order.
-}
relinkDependents :: DatabaseManager -> Text -> IO ()
relinkDependents manager newlyLoaded = do
    loadedDbs <- readTVarIO (dmLoadedDbs manager)
    let dependents =
            [ name
            | (name, ld) <- M.toList loadedDbs
            , name /= newlyLoaded
            , newlyLoaded `elem` dbDependsOn (ldDatabase ld)
            ]
    forM_ dependents $ \depName -> do
        result <- relinkDatabase manager depName
        case result of
            Right _ -> return ()
            Left err ->
                reportProgress Warning $
                    "Re-link of " <> T.unpack depName <> " failed: " <> T.unpack err

-- | Auto-load unloaded dependencies via loadDatabaseSingle
autoLoadDeps :: DatabaseManager -> [Text] -> IO [DepLoadResult]
autoLoadDeps manager deps =
    fmap catMaybes $ forM deps $ \depName -> do
        isLoaded <- M.member depName <$> readTVarIO (dmLoadedDbs manager)
        if isLoaded
            then return Nothing
            else do
                reportProgress Info $ "Auto-loading dependency: " <> T.unpack depName
                depResult <- loadDatabaseSingle manager depName
                case depResult of
                    Right _ -> do
                        reportProgress Info $ "  [OK] Auto-loaded: " <> T.unpack depName
                        return (Just (DepLoaded depName))
                    Left err -> do
                        reportProgress Error $ "  [FAIL] " <> T.unpack depName <> ": " <> T.unpack err
                        return (Just (DepLoadFailed depName err))

{- | Load a database on demand with automatic dependency loading
Pre-loads declared dependencies (from TOML config) so cross-DB linking works,
then loads the target database.
-}
loadDatabase :: DatabaseManager -> Text -> IO (Either Text (LoadedDatabase, [DepLoadResult]))
loadDatabase manager dbName = fmap flattenLoad (try (withLogScope dbName go))
  where
    -- A fresh load parses/reads from disk and can throw. Fold any exception
    -- into the Left this function already returns, so every surface (REST,
    -- MCP, CLI) gets one total Either to handle instead of each having to
    -- remember its own catch.
    flattenLoad :: Either SomeException (Either Text a) -> Either Text a
    flattenLoad = either (Left . T.pack . show) id
    go = do
        -- Pre-load declared dependencies so they're available for cross-DB linking
        availableDbs <- readTVarIO (dmAvailableDbs manager)
        let configDeps = maybe [] dcDepends (M.lookup dbName availableDbs)
        depResults1 <- autoLoadDeps manager configDeps

        result <- loadDatabaseSingle manager dbName
        case result of
            Left err -> return (Left err)
            Right loaded -> do
                -- Also auto-load any runtime-discovered dependencies
                depResults2 <- autoLoadDeps manager (dbDependsOn (ldDatabase loaded))
                -- Warm the method-table cache off the request path so the first
                -- score doesn't pay the (regional) build cost on demand.
                warmMethodTables manager dbName (ldDatabase loaded)
                return (Right (loaded, depResults1 ++ depResults2))

{- | Stage an uploaded database (parse + cross-DB link, no matrices yet)
When a valid cache exists, reconstructs staged state from the cached Database
without re-parsing, turning a ~90s operation into ~7s.
-}
stageUploadedDatabase :: DatabaseManager -> DatabaseConfig -> IO (Either Text ())
stageUploadedDatabase manager dbConfig = withLogScope (dcName dbConfig) $ do
    let dbName = dcName dbConfig
    reportProgress Info $ "[STARTING] Staging: " <> T.unpack (dcDisplayName dbConfig)

    -- Try cache first: if valid, reconstruct StagedDatabase without re-parsing
    mCachedDb <- Loader.loadCachedDatabaseWithMatrices dbName (dcPath dbConfig)

    case mCachedDb of
        Just cachedDb -> do
            -- Cache hit: auto-load dependencies so cross-DB solving works
            _ <- autoLoadDeps manager (dbDependsOn cachedDb)
            -- Recompute unknownUnits against the current unitConfig (cache may be stale)
            unitConfig <- getMergedUnitConfig manager
            let simpleDb = toSimpleDatabase cachedDb
                freshUnknownUnits =
                    S.fromList
                        [ unitName u
                        | u <- M.elems (sdbUnits simpleDb)
                        , not (UnitConversion.isKnownUnit unitConfig (unitName u))
                        , not (T.null (unitName u))
                        ]
                freshStats =
                    (dbLinkingStats cachedDb)
                        { cdlUnknownUnits = freshUnknownUnits
                        }
                staged =
                    StagedDatabase
                        { sdSimpleDB = simpleDb
                        , sdConfig = dbConfig
                        , sdUnlinkedCount = 0 -- was finalized successfully
                        , sdMissingProducts = []
                        , sdSelectedDeps = dbDependsOn cachedDb
                        , sdCrossDBLinks = dbCrossDBLinks cachedDb
                        , sdLinkingStats = freshStats
                        , sdCachedDB = Just cachedDb
                        }
            atomically $ modifyTVar' (dmStagedDbs manager) (M.insert dbName staged)
            reportProgress Info $ "  [OK] Staged from cache: " <> T.unpack (dcDisplayName dbConfig)
            return $ Right ()
        Nothing -> do
            -- Cache miss: parse and cross-DB link as before
            let locationAliases = dcLocationAliases dbConfig

            -- Resolve nested directory structure (e.g. ZIP extracts with multiple subdirs)
            path <- Upload.findDataDirectory (dcPath dbConfig)

            -- Look up indexes for cross-DB linking
            indexedDbs <- readTVarIO (dmIndexedDbs manager)
            let otherIndexes = M.elems indexedDbs

            -- Detect format to find the correct file path (CSV needs file, not directory)
            format <- detectDirectoryFormat path
            loadPath <- case format of
                FormatCSV -> do
                    isFile <- doesFileExist path
                    if isFile
                        then return path
                        else do
                            csvFiles <- findCSVFiles path
                            case csvFiles of
                                [] -> return path -- let loader produce the error
                                (f : _) -> return f
                _ -> return path

            -- Parse and run cross-DB linking (but don't build matrices)
            synonymDB <- getMergedSynonymDB manager
            unitConfig <- getMergedUnitConfig manager
            loadResult <-
                Loader.loadDatabaseWithCrossDBLinking
                    locationAliases
                    otherIndexes
                    synonymDB
                    unitConfig
                    (locationHierarchyOf manager)
                    (dcGeographyPolicy dbConfig)
                    loadPath

            case loadResult of
                Left err -> return $ Left err
                Right (simpleDb, stats) -> do
                    -- Minimal-cover pre-selection: drop DBs whose links are all
                    -- substitutable by another DB at the same score. If that
                    -- shrinks the dependency set, re-run linking restricted to
                    -- the chosen DBs so sdCrossDBLinks stays consistent with
                    -- sdSelectedDeps (no dangling supplier UUIDs at finalize).
                    let minimalDeps = computeMinimalSelectedDeps (Loader.cdlLinks stats)
                        contributingDeps = M.keys (Loader.crossDBBySource stats)
                    (finalStats, finalDB) <-
                        if S.fromList minimalDeps == S.fromList contributingDeps
                            then return (stats, simpleDb)
                            else do
                                let selectedSet = S.fromList minimalDeps
                                    restrictedIndexes =
                                        [idx | (n, idx) <- M.toList indexedDbs, S.member n selectedSet]
                                reportProgress Info $
                                    "  Minimal cover: dropping redundant deps "
                                        <> show (S.toList (S.fromList contributingDeps `S.difference` selectedSet))
                                        <> ", re-linking against "
                                        <> show minimalDeps
                                (simpleDb', stats') <-
                                    Loader.fixActivityLinksWithCrossDB
                                        restrictedIndexes
                                        synonymDB
                                        unitConfig
                                        (locationHierarchyOf manager)
                                        (dcGeographyPolicy dbConfig)
                                        simpleDb
                                return (stats', simpleDb')

                    let staged =
                            StagedDatabase
                                { sdSimpleDB = finalDB
                                , sdConfig = dbConfig
                                , sdUnlinkedCount = Loader.unresolvedCount finalStats
                                , sdMissingProducts = stagedMissingProducts finalDB finalStats
                                , sdSelectedDeps = minimalDeps
                                , sdCrossDBLinks = Loader.cdlLinks finalStats
                                , sdLinkingStats = finalStats
                                , sdCachedDB = Nothing
                                }

                    atomically $ modifyTVar' (dmStagedDbs manager) (M.insert dbName staged)
                    reportProgress Info $ "  [OK] Staged: " <> T.unpack (dcDisplayName dbConfig)
                    return $ Right ()

{- | Unload a database from memory (keeps config for reloading).
Refuses to unload if any currently-loaded database declares this one as a
dependency — unloading would leave the dependent's cross-DB links dangling.
-}
unloadDatabase :: DatabaseManager -> Text -> IO (Either Text ())
unloadDatabase manager dbName = withLogScope dbName $ do
    loadedDbs <- readTVarIO (dmLoadedDbs manager)

    case M.lookup dbName loadedDbs of
        Nothing -> return $ Left $ "Database not loaded: " <> dbName
        Just _ -> do
            let dependents =
                    [ name
                    | (name, ld) <- M.toList loadedDbs
                    , name /= dbName
                    , dbName `elem` dbDependsOn (ldDatabase ld)
                    ]
            if not (null dependents)
                then
                    return $
                        Left $
                            "Cannot unload "
                                <> dbName
                                <> ": still required by "
                                <> T.intercalate ", " dependents
                                <> ". Unload dependents first."
                else do
                    -- Remove from loaded databases and IndexedDatabases (for cross-DB linking)
                    atomically $ do
                        modifyTVar' (dmLoadedDbs manager) (M.delete dbName)
                        modifyTVar' (dmIndexedDbs manager) (M.delete dbName)

                    -- Clear cached solvers and flow mappings
                    clearCachedSolver dbName
                    clearMethodMappingCacheForDb manager dbName

                    -- Force garbage collection to release memory
                    performGC

                    reportProgress Info $ "Unloaded database: " <> T.unpack dbName
                    return $ Right ()

-- | Add a new database config to the manager (without loading)
addDatabase :: DatabaseManager -> DatabaseConfig -> IO ()
addDatabase manager dbConfig = do
    atomically $ modifyTVar' (dmAvailableDbs manager) (M.insert (dcName dbConfig) dbConfig)
    reportProgress Info $ "Added database config: " <> T.unpack (dcDisplayName dbConfig)

{- | Remove a database from the manager
Fails if database is loaded
-}
removeDatabase :: DatabaseManager -> Text -> IO (Either Text ())
removeDatabase manager dbName = do
    loadedDbs <- readTVarIO (dmLoadedDbs manager)
    availableDbs <- readTVarIO (dmAvailableDbs manager)

    case M.lookup dbName availableDbs of
        Nothing -> return $ Left $ "Database not found: " <> dbName
        Just dbConfig -> do
            -- Honor the per-config deletable policy (defaults to dcIsUploaded).
            if not (dcDeletable dbConfig)
                then return $ Left "Cannot delete configured database. Edit volca.toml to remove it."
                else
                    if M.member dbName loadedDbs
                        then return $ Left "Cannot delete loaded database. Close it first."
                        else do
                            -- Get the upload directory (uploads/<slug>/)
                            uploadsDir <- UploadedDB.getDatabaseUploadsDir
                            let uploadDir = uploadsDir </> T.unpack dbName
                            pathExists <- doesDirectoryExist uploadDir
                            if pathExists
                                then do
                                    -- Delete the database directory immediately
                                    result <- tryIO $ removeDirectoryRecursive uploadDir
                                    case result of
                                        Left (e :: SomeException) ->
                                            return $ Left $ "Failed to delete: " <> T.pack (show e)
                                        Right () -> do
                                            reportProgress Info $ "Deleted: " <> uploadDir
                                            deleteCacheFile dbName (dcPath dbConfig)
                                            removeFromMemory manager dbName
                                else do
                                    -- Directory already missing, just remove from memory
                                    reportProgress Info $ "Directory already missing: " <> uploadDir
                                    removeFromMemory manager dbName
  where
    tryIO :: IO a -> IO (Either SomeException a)
    tryIO = Control.Exception.try
    deleteCacheFile name sourcePath = do
        cacheFile <- Loader.generateMatrixCacheFilename name sourcePath
        let zstdFile = cacheFile ++ ".zst"
        cacheExists <- doesFileExist zstdFile
        when cacheExists $ do
            removeFile zstdFile
            reportProgress Info $ "Deleted cache: " ++ zstdFile

-- | Helper to remove database from in-memory maps only
removeFromMemory :: DatabaseManager -> Text -> IO (Either Text ())
removeFromMemory manager dbName = do
    atomically $ do
        modifyTVar' (dmAvailableDbs manager) (M.delete dbName)
        modifyTVar' (dmStagedDbs manager) (M.delete dbName)
        modifyTVar' (dmStagingDbs manager) (S.delete dbName)
    reportProgress Info $ "Removed database: " <> T.unpack dbName
    return $ Right ()

--------------------------------------------------------------------------------
-- Staged Database Operations
--------------------------------------------------------------------------------

-- | Get a staged database by name
getStagedDatabase :: DatabaseManager -> Text -> IO (Maybe StagedDatabase)
getStagedDatabase manager dbName = do
    stagedDbs <- readTVarIO (dmStagedDbs manager)
    return $ M.lookup dbName stagedDbs

{- | Supplier-gap report for a loaded or staged database — what is still
missing to fully supply its demands from the pinned dependencies, aggregated
per (product, location, unit) with the consumers that demand it.
-}
databaseGapReport :: DatabaseManager -> Text -> IO (Either Text Loader.GapReport)
databaseGapReport manager dbName = do
    loadedDbs <- readTVarIO (dmLoadedDbs manager)
    stagedDbs <- readTVarIO (dmStagedDbs manager)
    pure $ case (M.lookup dbName loadedDbs, M.lookup dbName stagedDbs) of
        (Just loaded, _) -> Right (Loader.gapReportForLoaded dbName (ldDatabase loaded))
        (Nothing, Just staged) ->
            Right (Loader.gapReportForStaged dbName (sdSimpleDB staged) (sdLinkingStats staged))
        (Nothing, Nothing) -> Left ("Database not loaded: " <> dbName)

{- | Dataset-soundness report for a loaded or staged database — the structural
defects a score can't reveal. Both phases reduce to the same pure scan, so a
maker gets the same answer before and after building the matrices.
-}
databaseQualityReport :: DatabaseManager -> Text -> IO (Either Text Quality.QualityReport)
databaseQualityReport manager dbName = do
    loadedDbs <- readTVarIO (dmLoadedDbs manager)
    stagedDbs <- readTVarIO (dmStagedDbs manager)
    pure $ case (M.lookup dbName loadedDbs, M.lookup dbName stagedDbs) of
        (Just loaded, _) -> Right (Quality.qualityReport dbName (toSimpleDatabase (ldDatabase loaded)))
        (Nothing, Just staged) -> Right (Quality.qualityReport dbName (sdSimpleDB staged))
        (Nothing, Nothing) -> Left ("Database not loaded: " <> dbName)

{- | Characterization-coverage report: the database flows a method collection
scores only through a name bridge (synonym/CAS), which an exact-name consumer
would score as zero. One entry per loaded collection when @mCollection@ is
'Nothing'; a single named collection otherwise (an error if it isn't loaded).

Needs a built database (the coverage probe reads the method tables), so unlike
the quality report it is loaded-only — no staged answer. Computed per request
on top of the per-method table and mapping caches; if it proves slow at
ecoinvent scale, the upgrade path is a @(db, collection)@-keyed cache beside
'mapMethodToTablesCached'.
-}
databaseCoverageReport :: DatabaseManager -> Text -> Maybe Text -> IO (Either Text Coverage.CoverageReport)
databaseCoverageReport manager dbName mCollection = do
    mLoaded <- getDatabase manager dbName
    loadedMethods <- readTVarIO (dmLoadedMethods manager)
    case mLoaded of
        Nothing -> pure (Left ("Database not loaded: " <> dbName))
        Just loaded -> do
            let db = ldDatabase loaded
            case collectionsToReport mCollection loadedMethods of
                Left err -> pure (Left err)
                Right cols -> do
                    hier <- getLocationHierarchy manager
                    bridges <- mapM (collectionBridgesFor db hier) cols
                    pure (Right (Coverage.CoverageReport dbName bridges))
  where
    -- The named collection (must be loaded) or every loaded one, by name.
    collectionsToReport sel loaded = case sel of
        Just name -> case M.lookup name loaded of
            Just mc -> Right [(name, mc)]
            Nothing -> Left ("Method collection not loaded: " <> name)
        Nothing -> Right (M.toList loaded)
    collectionBridgesFor db hier (collName, mc) = do
        let methods = mcMethods mc
        tables <- mapM (mapMethodToTablesCachedWithHier manager dbName collName db hier) methods
        mappings <- mapM (effectiveMethodMappings manager dbName collName db) methods
        let characterized = S.size (S.unions (map (`characterizedFlowIds` dbBioFlows db) tables))
            total = fromIntegral (dbBiosphereCount db)
        pure (Coverage.collectionBridges collName total characterized mappings)

{- | Outcome of the atomic staging decision; 'NeedToStage' carries the config
read inside the same transaction, so no later (racy) re-lookup is needed.
-}
data StageAction = AlreadyDone | NeedToStage DatabaseConfig

{- | Get setup info for a database (for the setup page)
Works for both staged and loaded databases
Auto-stages uploaded databases if they're not yet staged
Uses STM to prevent concurrent staging of the same database
-}
getDatabaseSetupInfo :: DatabaseManager -> Text -> IO (Either SetupError DatabaseSetupInfo)
getDatabaseSetupInfo manager dbName = do
    -- Atomic decision: already staged? already staging? need to stage?
    action <- atomically $ do
        stagedDbs <- readTVar (dmStagedDbs manager)
        loadedDbs <- readTVar (dmLoadedDbs manager)
        stagingDbs <- readTVar (dmStagingDbs manager)
        case M.lookup dbName stagedDbs of
            Just _ -> return $ Right AlreadyDone
            Nothing -> case M.lookup dbName loadedDbs of
                Just _ -> return $ Right AlreadyDone
                Nothing ->
                    if S.member dbName stagingDbs
                        then retry -- another thread is staging; STM blocks until done
                        else do
                            availableDbs <- readTVar (dmAvailableDbs manager)
                            case M.lookup dbName availableDbs of
                                Nothing -> return $ Left $ SetupNotFound $ "Database not found: " <> dbName
                                Just dbConfig
                                    | dcIsUploaded dbConfig -> do
                                        modifyTVar' (dmStagingDbs manager) (S.insert dbName)
                                        return $ Right (NeedToStage dbConfig)
                                    | otherwise ->
                                        return $ Left $ SetupNotLoaded dbName

    case action of
        Left err -> return $ Left err
        Right AlreadyDone -> buildSetupResult manager dbName
        Right (NeedToStage dbConfig) -> do
            -- Do the slow work, ensuring we always unmark on exception
            stageResult <-
                Control.Exception.finally
                    (stageUploadedDatabase manager dbConfig)
                    (atomically $ modifyTVar' (dmStagingDbs manager) (S.delete dbName))
            case stageResult of
                Left err -> do
                    reportProgress Error $ "Setup staging failed for " <> T.unpack dbName <> ": " <> T.unpack err
                    return $ Left $ SetupFailed err
                Right () -> buildSetupResult manager dbName

-- | Read current state and build setup info for a database
buildSetupResult :: DatabaseManager -> Text -> IO (Either SetupError DatabaseSetupInfo)
buildSetupResult manager dbName = do
    stagedDbs <- readTVarIO (dmStagedDbs manager)
    loadedDbs <- readTVarIO (dmLoadedDbs manager)
    availableDbs <- readTVarIO (dmAvailableDbs manager)
    indexedDbs <- readTVarIO (dmIndexedDbs manager)
    case M.lookup dbName stagedDbs of
        Just staged -> do
            let info = buildStagedSetupInfo staged availableDbs indexedDbs
            -- Populate available paths for uploaded databases
            if dcIsUploaded (sdConfig staged)
                then do
                    candidates <- discoverCandidatePaths (sdConfig staged)
                    return $ Right info{dsiAvailablePaths = candidates}
                else return $ Right info
        Nothing -> case M.lookup dbName loadedDbs of
            Just loaded ->
                return $ Right $ buildLoadedSetupInfo (ldConfig loaded) (ldDatabase loaded) availableDbs indexedDbs
            Nothing -> return $ Left $ SetupFailed $ "Failed to stage database: " <> dbName

{- | Link-resolution tally shared by the setup page and the finalize gate.
Both derive readiness from this one record, so "ready" and "can be finalized"
can never drift apart.
-}
data LinkCounts = LinkCounts
    { lcActivityCount :: !Int
    , lcTotalInputs :: !Int
    , lcUnlinked :: !Int
    , lcCrossDBLinks :: !Int
    }

-- | Inputs resolved inside the database itself.
lcInternalLinks :: LinkCounts -> Int
lcInternalLinks lc = max 0 (lcTotalInputs lc - lcUnlinked lc)

-- | Inputs no link, internal or cross-DB, resolves.
lcUnresolvedLinks :: LinkCounts -> Int
lcUnresolvedLinks lc = max 0 (lcUnlinked lc - lcCrossDBLinks lc)

-- | Tally for a staged database, from its parsed activities and linking stats.
stagedLinkCounts :: StagedDatabase -> LinkCounts
stagedLinkCounts staged =
    LinkCounts
        { lcActivityCount = M.size (sdbActivities sdb)
        , lcTotalInputs = Loader.countTotalTechInputs sdb
        , lcUnlinked = Loader.countUnlinkedExchanges sdb
        , lcCrossDBLinks = Loader.crossDBLinksCount (sdLinkingStats staged)
        }
  where
    sdb = sdSimpleDB staged

{- | Tally for a loaded database. Counts are recomputed from the activity set
via the same predicate as the staged path ('Loader.countUnlinkedExchanges'),
not read from 'dbLinkingStats': the stats only track nil-link / cross-DB
resolution and are blind to dangling internal links (non-nil 'activityLinkId'
pointing at an activity the database doesn't ship). A database bulk-loaded
from config bypasses the finalize gate, so without this it would report a
partial EcoSpold2 import as 100% ready while the matrix silently drops those
inputs.
-}
loadedLinkCounts :: Database -> LinkCounts
loadedLinkCounts db =
    LinkCounts
        { lcActivityCount = fromIntegral (dbActivityCount db)
        , lcTotalInputs = Loader.countTotalTechInputs sdb
        , lcUnlinked = Loader.countUnlinkedExchanges sdb
        , lcCrossDBLinks = length (dbCrossDBLinks db)
        }
  where
    sdb = toSimpleDatabase db

{- | Percentage of resolved inputs (0-100); an inputless database is complete.
Clamped: stats recording more cross-DB links than unlinked inputs must not
report above 100%.
-}
lcCompleteness :: LinkCounts -> Double
lcCompleteness lc
    | lcTotalInputs lc > 0 =
        min 100.0 $ 100.0 * fromIntegral (lcInternalLinks lc + lcCrossDBLinks lc) / fromIntegral (lcTotalInputs lc)
    | otherwise = 100.0

{- | Why a database cannot be finalized — 'Nothing' means ready. The setup
page's 'dsiIsReady' is the 'isNothing' of this, so the ready badge and the
finalize gate always agree.
-}
notReadyReason :: LinkCounts -> Maybe Text
notReadyReason lc
    | lcActivityCount lc == 0 =
        Just "database contains 0 activities. The data file may be corrupted or in an unsupported format."
    | lcUnresolvedLinks lc > 0 =
        Just $
            T.pack (show (lcUnresolvedLinks lc))
                <> " unresolved inputs. Add dependencies to resolve them first."
    | otherwise = Nothing

{- | Rank missing products by demanding-input count, descending. Nil-link gaps
carry the rich blockers the attribute matcher produced; dangling non-nil gaps
are tagged 'NoNameMatch'. The two sets are disjoint (nil vs non-nil), so the
concatenation never duplicates.
-}
rankMissingProducts :: Map Text (Int, LinkBlocker) -> Map Text Int -> [(Text, Int, LinkBlocker)]
rankMissingProducts blocked dangling =
    sortOn
        (\(_, cnt, _) -> Down cnt)
        ( [(name, cnt, blocker) | (name, (cnt, blocker)) <- M.toList blocked]
            <> [(name, cnt, NoNameMatch) | (name, cnt) <- M.toList dangling]
        )

-- | Project one ranked missing product onto its wire shape.
blockerToMissingSupplier :: (Text, Int, LinkBlocker) -> MissingSupplier
blockerToMissingSupplier (name, cnt, blocker) =
    let (reason, detail) = blockerReasonDetail blocker
     in MissingSupplier name cnt Nothing reason detail

{- | Missing-supplier list for a staged database: rich blockers from the
linking stats plus dangling background links a partial import leaves behind
('Loader.collectStagedDanglingProductNames'), ranked by demand.
-}
stagedMissingProducts :: SimpleDatabase -> CrossDBLinkingStats -> [(Text, Int, LinkBlocker)]
stagedMissingProducts sdb stats =
    rankMissingProducts
        (cdlUnresolvedProducts stats)
        (Loader.collectStagedDanglingProductNames sdb (cdlLinks stats))

{- | Assemble the wire record from the shared tally — the single place the
completeness, readiness, and linking-stats fields are filled, for both the
staged and the loaded builder. availablePaths is filled in by
'buildSetupResult' for uploaded databases (requires IO).
-}
setupInfoFrom :: DatabaseConfig -> LinkCounts -> CrossDBLinkingStats -> [(Text, Int, LinkBlocker)] -> [DependencyChoice] -> Bool -> DatabaseSetupInfo
setupInfoFrom config lc stats missing dependencies isLoaded =
    DatabaseSetupInfo
        { dsiName = dcName config
        , dsiDisplayName = dcDisplayName config
        , dsiActivityCount = lcActivityCount lc
        , dsiInputCount = lcTotalInputs lc
        , dsiCompleteness = lcCompleteness lc
        , dsiInternalLinks = lcInternalLinks lc
        , dsiCrossDBLinks = lcCrossDBLinks lc
        , dsiUnresolvedLinks = lcUnresolvedLinks lc
        , dsiMissingSuppliers = take 10 (map blockerToMissingSupplier missing)
        , dsiDependencies = dependencies
        , dsiIsReady = isNothing (notReadyReason lc)
        , dsiUnknownUnits = S.toList (cdlUnknownUnits stats)
        , dsiLocationFallbacks = deduplicateFallbacks (cdlLocationFallbacks stats)
        , dsiLocationUnresolved = deduplicateUnresolved (cdlLocationUnresolved stats)
        , dsiAttributeFallbacks = deduplicateAttributeFallbacks (cdlAttributeFallbacks stats)
        , dsiDataPath = T.pack (dcPath config)
        , dsiAvailablePaths = []
        , dsiIsLoaded = isLoaded
        }

-- | Build setup info from a staged database
buildStagedSetupInfo :: StagedDatabase -> Map Text DatabaseConfig -> Map Text IndexedDatabase -> DatabaseSetupInfo
buildStagedSetupInfo staged configs indexedDbs =
    let stats = sdLinkingStats staged
     in setupInfoFrom
            (sdConfig staged)
            (stagedLinkCounts staged)
            stats
            (sdMissingProducts staged)
            ( buildDependencyChoices
                (dcName (sdConfig staged))
                (sdSelectedDeps staged)
                (crossDBRedundantSources (cdlLinks stats) (sdSelectedDeps staged))
                configs
                indexedDbs
            )
            False

{- | Build setup info from a loaded database (already finalized). Counts come
from 'loadedLinkCounts' (see its note on recomputing rather than trusting
'dbLinkingStats'); rich blocker reasons still come from the stats, dangling
links are ranked in with them.
-}
buildLoadedSetupInfo :: DatabaseConfig -> Database -> Map Text DatabaseConfig -> Map Text IndexedDatabase -> DatabaseSetupInfo
buildLoadedSetupInfo config db configs indexedDbs =
    setupInfoFrom
        config
        (loadedLinkCounts db)
        (dbLinkingStats db)
        (rankMissingProducts (cdlUnresolvedProducts (dbLinkingStats db)) (Loader.collectDanglingProductNames db))
        (buildDependencyChoices (dcName config) (dbDependsOn db) [] configs indexedDbs)
        True

{- | Discover candidate data paths within an uploaded database's root directory.
Returns one 'PathCandidate' per candidate directory.
-}
discoverCandidatePaths :: DatabaseConfig -> IO [PathCandidate]
discoverCandidatePaths dbConfig = do
    uploadsDir <- UploadedDB.getDatabaseUploadsDir
    let uploadRoot = uploadsDir </> T.unpack (dcName dbConfig)
    candidates <- Upload.findAllDataDirectories uploadRoot
    forM candidates $ \dir -> do
        format <- Upload.detectDatabaseFormat dir
        count <- Upload.countDataFilesIn dir
        let rel = makeRelativePath uploadRoot dir
            label = case format of
                Upload.EcoSpold2 -> "EcoSpold 2"
                Upload.EcoSpold1 -> "EcoSpold 1"
                Upload.SimaProCSV -> "SimaPro CSV"
                Upload.ILCDProcess -> "ILCD"
                Upload.OpenLcaJsonLd -> "openLCA JSON-LD"
                Upload.BrightwayExcel -> "Brightway Excel"
                Upload.UnknownFormat -> "Unknown"
        return PathCandidate{pcPath = T.pack rel, pcFormat = label, pcFileCount = count}
  where
    -- Simple relative path: strip upload root prefix
    makeRelativePath base path
        | base `isPrefixOf` path =
            let r = drop (length base + 1) path
             in if null r then "." else r
        | otherwise = path

{- | Change the data path for an uploaded (staged) database.
Validates path, updates config + meta.toml, clears staged DB to force re-stage.
-}
setDataPath :: DatabaseManager -> Text -> Text -> IO (Either Text DatabaseSetupInfo)
setDataPath manager dbName newRelPath = do
    availableDbs <- readTVarIO (dmAvailableDbs manager)
    case M.lookup dbName availableDbs of
        Nothing -> return $ Left $ "Database not found: " <> dbName
        Just dbConfig
            | not (dcIsUploaded dbConfig) ->
                return $ Left "Cannot change data path for configured databases"
            | otherwise -> do
                -- Resolve full path
                uploadsDir <- UploadedDB.getDatabaseUploadsDir
                let uploadRoot = uploadsDir </> T.unpack dbName
                    newFullPath = uploadRoot </> T.unpack newRelPath

                -- Validate that path exists and has data
                hasData <- Upload.anyDataFilesIn newFullPath
                if not hasData
                    then return $ Left $ "No data files found in: " <> newRelPath
                    else do
                        -- Detect format for the new path
                        newFormat <- Upload.detectDatabaseFormat newFullPath

                        -- Update config
                        let updatedConfig =
                                dbConfig
                                    { dcPath = newFullPath
                                    , dcFormat = Just newFormat
                                    }
                        atomically $ modifyTVar' (dmAvailableDbs manager) (M.insert dbName updatedConfig)

                        -- Update meta.toml
                        mMeta <- UploadedDB.readUploadMeta uploadRoot
                        case mMeta of
                            Just meta ->
                                UploadedDB.writeUploadMeta
                                    uploadRoot
                                    meta
                                        { UploadedDB.umDataPath = T.unpack newRelPath
                                        , UploadedDB.umFormat = newFormat
                                        }
                            Nothing -> return ()

                        -- Clear staged DB to force re-staging with new path
                        atomically $ modifyTVar' (dmStagedDbs manager) (M.delete dbName)

                        -- Re-stage and return fresh setup info
                        result <- getDatabaseSetupInfo manager dbName
                        case result of
                            Left err -> return $ Left $ setupErrorMessage err
                            Right info -> return $ Right info

{- | Build the combined list of dependency choices.
Excludes the current database, tags each remaining DB as selected,
redundant, or available, and sorts the result alphabetically.
Selected takes precedence over redundant if a name appears in both sets.
-}
buildDependencyChoices ::
    -- | Current database name (excluded from the result)
    Text ->
    -- | Names currently selected as dependencies
    [Text] ->
    -- | Names that match links but are redundant under the minimal cover
    [Text] ->
    Map Text DatabaseConfig ->
    Map Text IndexedDatabase ->
    [DependencyChoice]
buildDependencyChoices currentName selected redundant configs indexedDbs =
    let selectedSet = S.fromList selected
        redundantSet = S.fromList redundant
        statusOf name
            | S.member name selectedSet = SelectedDep
            | S.member name redundantSet = RedundantDep
            | otherwise = AvailableDep
        mkChoice (name, idx) =
            DependencyChoice
                { dchStatus = statusOf name
                , dchDatabaseName = name
                , dchDisplayName = maybe name dcDisplayName (M.lookup name configs)
                , dchMatchCount = M.size (Database.CrossLinking.idbByProductName idx)
                }
     in sortOn
            dchDatabaseName
            [ mkChoice (name, idx)
            | (name, idx) <- M.toList indexedDbs
            , name /= currentName
            ]

{- | Re-stage a loaded database for dependency editing
Moves from dmLoadedDbs → dmStagedDbs, cleans up solver
-}
restageLoadedDatabase :: DatabaseManager -> Text -> LoadedDatabase -> IO StagedDatabase
restageLoadedDatabase manager dbName ld = do
    let db = ldDatabase ld
        stats = dbLinkingStats db
        staged =
            StagedDatabase
                { sdSimpleDB = toSimpleDatabase db
                , sdConfig = ldConfig ld
                , sdUnlinkedCount = unresolvedCount stats
                , sdMissingProducts = stagedMissingProducts (toSimpleDatabase db) stats
                , sdSelectedDeps = dbDependsOn db
                , sdCrossDBLinks = dbCrossDBLinks db
                , sdLinkingStats = stats
                , sdCachedDB = Nothing
                }
    atomically $ do
        modifyTVar' (dmLoadedDbs manager) (M.delete dbName)
        modifyTVar' (dmStagedDbs manager) (M.insert dbName staged)
    clearCachedSolver dbName
    clearMethodMappingCacheForDb manager dbName
    return staged

-- | Get or create staged database (re-stages loaded DBs on the fly)
getOrStageDatabase :: DatabaseManager -> Text -> IO (Either Text StagedDatabase)
getOrStageDatabase manager dbName = do
    stagedDbs <- readTVarIO (dmStagedDbs manager)
    case M.lookup dbName stagedDbs of
        Just staged -> return $ Right staged
        Nothing -> do
            loadedDbs <- readTVarIO (dmLoadedDbs manager)
            case M.lookup dbName loadedDbs of
                Just ld -> Right <$> restageLoadedDatabase manager dbName ld
                Nothing -> return $ Left $ "Database not found: " <> dbName

{- | Add a dependency to a staged (or partially-linked loaded) database
Runs cross-DB linking against the new dependency
-}
addDependencyToStaged :: DatabaseManager -> Text -> Text -> IO (Either Text DatabaseSetupInfo)
addDependencyToStaged manager dbName depName = do
    indexedDbs <- readTVarIO (dmIndexedDbs manager)
    stagedResult <- getOrStageDatabase manager dbName

    case stagedResult of
        Left err -> return $ Left err
        Right staged -> case M.lookup depName indexedDbs of
            Nothing -> return $ Left $ "Dependency database not loaded: " <> depName
            Just _depIdx -> do
                -- Compute new dependency list, then link only against selected deps
                let newDeps =
                        if depName `elem` sdSelectedDeps staged
                            then sdSelectedDeps staged
                            else depName : sdSelectedDeps staged
                    selectedIndexes = [idx | (name, idx) <- M.toList indexedDbs, name `elem` newDeps]
                synonymDB <- getMergedSynonymDB manager
                unitConfig <- getMergedUnitConfig manager
                (_, newStats) <-
                    Loader.fixActivityLinksWithCrossDB
                        selectedIndexes
                        synonymDB
                        unitConfig
                        (locationHierarchyOf manager)
                        (dcGeographyPolicy (sdConfig staged))
                        (sdSimpleDB staged)

                -- Update staged database with new stats and dependency
                let updatedStaged =
                        staged
                            { sdSelectedDeps = newDeps
                            , sdCrossDBLinks = Loader.cdlLinks newStats
                            , sdLinkingStats = newStats
                            , sdMissingProducts = stagedMissingProducts (sdSimpleDB staged) newStats
                            }

                -- Save updated staged database
                atomically $ modifyTVar' (dmStagedDbs manager) (M.insert dbName updatedStaged)

                -- Return updated setup info
                first setupErrorMessage <$> getDatabaseSetupInfo manager dbName

-- | Remove a dependency from a staged (or partially-linked loaded) database
removeDependencyFromStaged :: DatabaseManager -> Text -> Text -> IO (Either Text DatabaseSetupInfo)
removeDependencyFromStaged manager dbName depName = do
    stagedResult <- getOrStageDatabase manager dbName

    case stagedResult of
        Left err -> return $ Left err
        Right staged -> do
            let newDeps = filter (/= depName) (sdSelectedDeps staged)

            -- Re-run cross-DB linking without the removed dependency
            indexedDbs <- readTVarIO (dmIndexedDbs manager)
            let remainingIndexes = [idx | (name, idx) <- M.toList indexedDbs, name `elem` newDeps]
            synonymDB <- getMergedSynonymDB manager
            unitConfig <- getMergedUnitConfig manager
            (_, newStats) <-
                Loader.fixActivityLinksWithCrossDB
                    remainingIndexes
                    synonymDB
                    unitConfig
                    (locationHierarchyOf manager)
                    (dcGeographyPolicy (sdConfig staged))
                    (sdSimpleDB staged)

            -- Update staged database
            let updatedStaged =
                    staged
                        { sdSelectedDeps = newDeps
                        , sdCrossDBLinks = Loader.cdlLinks newStats
                        , sdLinkingStats = newStats
                        , sdMissingProducts = stagedMissingProducts (sdSimpleDB staged) newStats
                        }

            atomically $ modifyTVar' (dmStagedDbs manager) (M.insert dbName updatedStaged)
            first setupErrorMessage <$> getDatabaseSetupInfo manager dbName

-- | Finalize a staged database (build matrices and make it ready for queries)
finalizeDatabase :: DatabaseManager -> Text -> IO (Either Text LoadedDatabase)
finalizeDatabase manager dbName = withLogScope dbName $ do
    stagedDbs <- readTVarIO (dmStagedDbs manager)

    case M.lookup dbName stagedDbs of
        Nothing -> do
            -- Not staged — an already-loaded database finalizes as a no-op,
            -- but only through the same readiness gate the setup page reports:
            -- a partial import bulk-loaded from config must not get a success
            -- where the setup says not ready.
            loadedDbs <- readTVarIO (dmLoadedDbs manager)
            case M.lookup dbName loadedDbs of
                Just loaded ->
                    return $ case notReadyReason (loadedLinkCounts (ldDatabase loaded)) of
                        Just reason -> Left ("Cannot finalize: " <> reason)
                        Nothing -> Right loaded
                Nothing -> return $ Left $ "Staged database not found: " <> dbName
        Just staged ->
            case notReadyReason (stagedLinkCounts staged) of
                Just reason -> return $ Left ("Cannot finalize: " <> reason)
                Nothing -> do
                    reportProgress Info $ "[STARTING] Finalizing database: " <> T.unpack dbName

                    synonymDB <- getMergedSynonymDB manager

                    -- Use pre-built database from cache, or build matrices from scratch
                    buildResult <- case sdCachedDB staged of
                        Just cachedDb -> do
                            -- The on-disk cache == cachedDb. Carry the
                            -- (possibly edited) staged dependency pin and
                            -- its recomputed links onto the loaded DB so
                            -- the pin is authoritative; flag a re-save
                            -- when either diverges from what's on disk.
                            let pinned =
                                    cachedDb
                                        { dbCrossDBLinks = sdCrossDBLinks staged
                                        , dbDependsOn = sdSelectedDeps staged
                                        , dbLinkingStats = sdLinkingStats staged
                                        }
                                needsSave =
                                    not (sameSet (dbDependsOn cachedDb) (sdSelectedDeps staged))
                                        || not (sameSet (dbCrossDBLinks cachedDb) (sdCrossDBLinks staged))
                            return $ Right (BM25.addBM25Index (initializeRuntimeFields pinned synonymDB), needsSave)
                        Nothing -> do
                            unitConfig <- getMergedUnitConfig manager
                            dbResult <-
                                buildDatabaseWithMatrices
                                    unitConfig
                                    (sdbActivities (sdSimpleDB staged))
                                    (sdbTechFlows (sdSimpleDB staged))
                                    (sdbBioFlows (sdSimpleDB staged))
                                    (sdbWasteFlows (sdSimpleDB staged))
                                    (sdbUnits (sdSimpleDB staged))
                            case dbResult of
                                Left err -> return $ Left err
                                Right db -> do
                                    let dbWithLinks =
                                            db
                                                { dbCrossDBLinks = sdCrossDBLinks staged
                                                , dbDependsOn = sdSelectedDeps staged
                                                , dbLinkingStats = sdLinkingStats staged
                                                }
                                    -- Freshly built matrices: always persist.
                                    return $ Right (BM25.addBM25Index (initializeRuntimeFields dbWithLinks synonymDB), True)

                    case buildResult of
                        Left err -> return $ Left err
                        Right (dbWithRuntime, needsSave) -> do
                            -- Create shared solver with lazy factorization (deferred to first query)
                            let techTriplesInt = [(fromIntegral i, fromIntegral j, v) | SparseTriple i j v <- U.toList (dbTechnosphereTriples dbWithRuntime)]
                                activityCountInt = fromIntegral $ dbActivityCount dbWithRuntime
                            sharedSolver <- createSharedSolver dbName techTriplesInt activityCountInt

                            let loaded =
                                    LoadedDatabase
                                        { ldDatabase = dbWithRuntime
                                        , ldSharedSolver = sharedSolver
                                        , ldConfig = sdConfig staged
                                        }

                            -- Move from staged to loaded
                            let indexedDb = buildIndexedDatabaseFromDB dbName synonymDB dbWithRuntime
                            atomically $ do
                                modifyTVar' (dmStagedDbs manager) (M.delete dbName)
                                modifyTVar' (dmLoadedDbs manager) (M.insert dbName loaded)
                                modifyTVar' (dmIndexedDbs manager) (M.insert dbName indexedDb)
                            clearMethodMappingCacheForDb manager dbName

                            -- Self-relink first against the current
                            -- dep set: a cached or staged build can
                            -- carry cross-DB links that don't match
                            -- the deps now in 'dmIndexedDbs'.
                            -- 'relinkDatabase' rewrites both the
                            -- in-memory state and (when 'linksChanged'
                            -- is True) the matrix cache.
                            relinkOutcome <- relinkDatabase manager dbName

                            -- 'needsSave' marks a finalize that changed
                            -- something on disk (fresh build, or an
                            -- edited dependency pin on a cache hit). The
                            -- 'Left' fallback treats a failed relink as
                            -- "no relink write happened", so the explicit
                            -- save below still fires when needed.
                            linksChangedAfter <- case relinkOutcome of
                                Right rr -> return (rresLinksChanged rr)
                                Left err -> do
                                    reportProgress Warning $
                                        "Self-relink of " <> T.unpack dbName <> " failed: " <> T.unpack err
                                    return False
                            -- Persist when this finalize introduced a
                            -- change (fresh build, or an edited pin on a
                            -- cache hit) that the relink didn't already
                            -- write. relink owns the save whenever it
                            -- actually changed the in-memory links.
                            when (needsSave && not linksChangedAfter) $
                                Loader.saveCachedDatabaseWithMatrices dbName (dcPath (sdConfig staged)) dbWithRuntime

                            reportProgress Info $ "  [OK] Finalized: " <> T.unpack dbName
                            return $ Right loaded

--------------------------------------------------------------------------------
-- Method Collection Management
--------------------------------------------------------------------------------

{- | Load methods from a MethodConfig path (directory or archive).
Handles ZIP/7z archives via resolveDataPath, finds method XMLs,
and enriches CFs from ILCD flow XMLs when available.
-}
loadMethodCollectionFromConfig :: MethodConfig -> IO (Either Text (MethodCollection, M.Map UUID ILCDFlowInfo))
loadMethodCollectionFromConfig mc = do
    -- Resolve archives (ZIP → extracted directory). Single .json (openLCA
    -- JSON-LD ImpactCategory) and .csv (SimaPro method export) files are
    -- accepted directly without a wrapping directory or archive.
    resolvedPath <- resolveDataPath (mcPath mc)
    isDir <- doesDirectoryExist resolvedPath
    isFile <- doesFileExist resolvedPath
    let ext = map toLower (takeExtension resolvedPath)
        isSingleJson = isFile && ext == ".json"
        isSingleCsv = isFile && ext == ".csv"
        isBareFile = isSingleJson || isSingleCsv
    if not isDir && not isBareFile
        then
            return . Left $
                if not isFile
                    then "Method path not found: " <> T.pack (mcPath mc)
                    else
                        if ext `elem` archiveExtensions
                            -- resolveDataPath returns the archive path unchanged when
                            -- extraction failed, so an archive reaching here means that.
                            then "Archive could not be extracted (see log above): " <> T.pack (mcPath mc)
                            else "Unsupported method file type (expected a directory, archive, .csv, or .json): " <> T.pack (mcPath mc)
        else do
            (dir, xmlFiles, csvFiles, jsonFiles) <-
                if isBareFile
                    then
                        return
                            ( takeDirectory resolvedPath
                            , []
                            , [takeFileName resolvedPath | isSingleCsv]
                            , [takeFileName resolvedPath | isSingleJson]
                            )
                    else do
                        -- Find method directory (handles nested ILCD structures)
                        d <- findMethodDirectory resolvedPath
                        -- listDirectory order is filesystem-dependent; sort so a
                        -- collection loads its methods in the same order on every
                        -- machine (and a re-export of it is byte-stable).
                        fs <- sort <$> listDirectory d
                        let xs = filter (\f -> map toLower (takeExtension f) == ".xml") fs
                            cs = filter (\f -> map toLower (takeExtension f) == ".csv") fs
                            js = filter (\f -> map toLower (takeExtension f) == ".json") fs
                        return (d, xs, cs, js)
            if null xmlFiles && null csvFiles && null jsonFiles
                then return $ Left $ "No method files (.xml/.csv/.json) found in: " <> T.pack dir
                else do
                    -- A bare method file (.csv/.json) carries its own CFs and has no
                    -- ILCD flows/ sibling; only a real ILCD directory does. Scanning a
                    -- coincidental neighbouring flows/ would parse unrelated flow XMLs
                    -- and register foreign synonyms under this collection's name.
                    mFlowsDir <-
                        if isBareFile
                            then return Nothing
                            else FlowResolver.resolveFlowDirectory dir
                    flowInfo <- case mFlowsDir of
                        Nothing -> do
                            reportProgress Info "  No flows/ directory found, using shortDescription fallback"
                            return M.empty
                        Just flowsDir -> do
                            reportProgress Info $ "  Loading ILCD flow XMLs from: " <> flowsDir
                            info <- FlowResolver.parseFlowDirectory flowsDir
                            reportProgress Info $ "  Loaded " <> show (M.size info) <> " flow definitions"
                            return info
                    -- Parse method files with flow enrichment
                    xmlResults <- forM xmlFiles $ \f ->
                        Method.Parser.parseMethodFileWithFlows flowInfo (dir </> f)
                    -- Split CSV files into SimaPro method exports and tabular CSVs
                    csvParsed <- forM csvFiles $ \f -> do
                        bytes <- stripBOM <$> BS.readFile (dir </> f)
                        if isSimaProMethodCSV bytes
                            then return $ fmap Left (parseSimaProMethodCSVBytes bytes)
                            else return $ fmap Right (parseMethodCSVBytes bytes)
                    -- openLCA JSON-LD ImpactCategory files (carries optional regionalized CFs).
                    -- Only files that actually carry @type=ImpactCategory are parsed; others
                    -- are skipped silently since arbitrary .json files can sit alongside
                    -- method data (e.g. metadata or other openLCA entity types).
                    jsonResults <- forM jsonFiles $ \f -> do
                        bytes <- BS.readFile (dir </> f)
                        if OlcaSchema.isOlcaImpactCategoryJson bytes
                            then return $ Just $ OlcaSchema.parseOlcaImpactCategoryBytes bytes
                            else return Nothing
                    let (xmlErrs, xmlMethods) = partitionEithers xmlResults
                        (csvErrs, csvOks) = partitionEithers csvParsed
                        (jsonErrs, jsonMethods) = partitionEithers (catMaybes jsonResults)
                        -- Merge: SimaPro CSVs are MethodCollections, tabular CSVs are [Method]
                        spCollections = lefts csvOks
                        tabularMethods = concat (rights csvOks)
                        allMethods =
                            xmlMethods
                                ++ tabularMethods
                                ++ jsonMethods
                                ++ concatMap mcMethods spCollections
                        -- Merge NW data from all SimaPro CSV sources
                        allDamageCats = concatMap mcDamageCategories spCollections
                        allNWSets = concatMap mcNormWeightSets spCollections
                        collection = MethodCollection allMethods allDamageCats allNWSets []
                        errs = xmlErrs ++ csvErrs ++ jsonErrs
                    case (null allMethods, errs) of
                        (True, firstErr : _) ->
                            return $ Left $ "All method files failed to parse: " <> T.pack firstErr
                        _ -> do
                            let xmlOk = length xmlMethods
                                csvOk = length csvOks
                                jsonOk = length jsonMethods
                            reportProgress Info $ "  Parsed " <> show xmlOk <> " XML, " <> show csvOk <> " CSV, " <> show jsonOk <> " JSON file(s)"
                            unless (null allDamageCats) $
                                reportProgress Info $
                                    "  "
                                        <> show (length allDamageCats)
                                        <> " damage categories, "
                                        <> show (length allNWSets)
                                        <> " normalization-weighting set(s)"
                            unless (null errs) $
                                reportProgress Warning $
                                    "  " <> show (length errs) <> " method file(s) failed to parse"
                            return $ Right (collection, flowInfo)

-- | List all method collections with their status
listMethodCollections :: DatabaseManager -> IO [MethodCollectionStatus]
listMethodCollections manager = do
    available <- readTVarIO (dmAvailableMethods manager)
    loaded <- readTVarIO (dmLoadedMethods manager)
    return
        [ MethodCollectionStatus
            { mcsName = name
            , mcsDisplayName = mcName mc
            , mcsDescription = mcDescription mc
            , mcsStatus = if M.member name loaded then Loaded else Unloaded
            , mcsIsUploaded = mcIsUploaded mc
            , mcsPath = T.pack (mcPath mc)
            , mcsMethodCount = maybe 0 (length . mcMethods) (M.lookup name loaded)
            , mcsFormat = fromMaybe (detectFormatFromPath (mcPath mc)) (mcFormat mc)
            }
        | (name, mc) <- M.toList available
        ]
  where
    detectFormatFromPath :: FilePath -> Text
    detectFormatFromPath p
        | T.isInfixOf ".csv" (T.toLower (T.pack p)) = "SimaPro CSV"
        | T.isInfixOf ".json" (T.toLower (T.pack p)) = "Regionalized LCIA JSON"
        | otherwise = "ILCD"

-- | Load a method collection on demand
loadMethodCollection :: DatabaseManager -> Text -> IO (Either Text ())
loadMethodCollection manager name = do
    available <- readTVarIO (dmAvailableMethods manager)
    case M.lookup name available of
        Nothing -> return $ Left $ "Method collection not found: " <> name
        Just mc -> do
            already <- M.member name <$> readTVarIO (dmLoadedMethods manager)
            if already
                then return $ Right ()
                else do
                    reportProgress Info $ "[STARTING] Loading method: " <> T.unpack name
                    result <- loadMethodCollectionFromConfig mc
                    case result of
                        Left err -> do
                            reportProgress Error $ "  [FAIL] " <> T.unpack name <> ": " <> T.unpack err
                            return $ Left err
                        Right (collection0, flowInfo) -> do
                            -- Inject scoring sets from TOML config, apply declarative CF patches
                            let (collection, patchStats) = applyMethodConfig mc collection0
                            atomically $ modifyTVar' (dmLoadedMethods manager) (M.insert name collection)
                            clearMethodMappingCache manager
                            let methods = mcMethods collection
                                totalCFs = sum $ map (length . methodFactors) methods
                            reportProgress Info $
                                "  [OK] Loaded: "
                                    <> T.unpack name
                                    <> " ("
                                    <> show (length methods)
                                    <> " impact categories, "
                                    <> show totalCFs
                                    <> " characterization factors)"
                            warnZeroTouchPatches name patchStats
                            -- Auto-extract synonyms from ILCD flow definitions
                            let pairs = extractFromILCDFlows flowInfo
                            autoCreateFlowSynonyms
                                manager
                                name
                                ("Auto-extracted from " <> name)
                                pairs
                            return $ Right ()

-- | Unload a method collection from memory
unloadMethodCollection :: DatabaseManager -> Text -> IO (Either Text ())
unloadMethodCollection manager name = do
    loaded <- readTVarIO (dmLoadedMethods manager)
    if M.member name loaded
        then do
            atomically $ modifyTVar' (dmLoadedMethods manager) (M.delete name)
            clearMethodMappingCache manager
            reportProgress Info $ "Unloaded method: " <> T.unpack name
            return $ Right ()
        else return $ Left $ "Method collection not loaded: " <> name

-- | Get all loaded methods (flattened across all collections)
getLoadedMethods :: DatabaseManager -> IO [(Text, Method)]
getLoadedMethods manager = do
    loaded <- readTVarIO (dmLoadedMethods manager)
    return [(collName, m) | (collName, coll) <- M.toList loaded, m <- mcMethods coll]

-- | Look up one loaded method collection by name.
getMethodCollection :: DatabaseManager -> Text -> IO (Maybe MethodCollection)
getMethodCollection manager name = M.lookup name <$> readTVarIO (dmLoadedMethods manager)

-- | Add a new method collection to the available list
addMethodCollection :: DatabaseManager -> MethodConfig -> IO ()
addMethodCollection manager mc =
    atomically $ modifyTVar' (dmAvailableMethods manager) (M.insert (mcName mc) mc)

-- | Remove an uploaded method collection (delete files + remove from memory)
removeMethodCollection :: DatabaseManager -> Text -> IO (Either Text ())
removeMethodCollection manager name = do
    available <- readTVarIO (dmAvailableMethods manager)
    loaded <- readTVarIO (dmLoadedMethods manager)
    case M.lookup name available of
        Nothing -> return $ Left $ "Method collection not found: " <> name
        Just mc
            | not (mcIsUploaded mc) ->
                return $ Left "Cannot delete configured method. Edit volca.toml to remove it."
            | M.member name loaded ->
                return $ Left "Cannot delete loaded method. Close it first."
            | otherwise -> do
                -- Find and delete the upload directory
                methodUploadsDir <- UploadedDB.getMethodUploadsDir
                -- The slug is derived from the directory name; search for it
                let slug = Upload.slugify name
                    uploadDir = methodUploadsDir </> T.unpack slug
                pathExists <- doesDirectoryExist uploadDir
                if pathExists
                    then do
                        result <- Control.Exception.try $ removeDirectoryRecursive uploadDir
                        case result of
                            Left (e :: SomeException) ->
                                return $ Left $ "Failed to delete: " <> T.pack (show e)
                            Right () -> do
                                reportProgress Info $ "Deleted method: " <> uploadDir
                                atomically $ modifyTVar' (dmAvailableMethods manager) (M.delete name)
                                return $ Right ()
                    else do
                        -- Directory already missing, just remove from memory
                        atomically $ modifyTVar' (dmAvailableMethods manager) (M.delete name)
                        return $ Right ()

--------------------------------------------------------------------------------
-- Merged reference data helpers
--------------------------------------------------------------------------------

-- | Get the merged SynonymDB from all loaded synonym databases.
getMergedSynonymDB :: DatabaseManager -> IO SynonymDB
getMergedSynonymDB manager = do
    loaded <- readTVarIO (dmLoadedFlowSyns manager)
    return $
        if M.null loaded
            then emptySynonymDB
            else mergeSynonymDBs (M.elems loaded)

{- | Surface one-way synonym bridges whose direction constraint is void in the
(merged) set — re-linked in the opposite view by an untyped transitive chain or
a contradictory row ('reopenedBridges'). 'demoteDuplicates' only drops the exact
duplicate pair, so this residue would otherwise silently widen a curated
one-way bridge back to both directions. Called where the merged set is about to
drive a database load, not on the request-path getters, so it fires once per
load rather than per query.
-}
warnReopenedBridges :: SynonymDB -> IO ()
warnReopenedBridges synDB =
    forM_ (reopenedBridges synDB) $ \e ->
        reportProgress Warning $
            "Flow synonyms: one-way bridge "
                <> show (seA e)
                <> " = "
                <> show (seB e)
                <> " ("
                <> dirLabel (seDir e)
                <> ") is re-linked in the opposite direction's view by other rows; its direction restriction is void"
  where
    dirLabel BridgeBoth = "both"
    dirLabel BridgeInput = "input"
    dirLabel BridgeOutput = "output"

-- | Get the merged CompartmentMap from all loaded compartment mappings.
getMergedCompartmentMap :: DatabaseManager -> IO CompartmentMap
getMergedCompartmentMap manager = do
    loaded <- readTVarIO (dmLoadedCompMaps manager)
    return $ M.unions (M.elems loaded)

{- | Get the merged 'EnergyDensityMap' from all loaded energy-density sets.
First-wins union over active CSVs, mirroring 'getMergedCompartmentMap'.
-}
getMergedEnergyDensities :: DatabaseManager -> IO EnergyDensityMap
getMergedEnergyDensities manager = do
    loaded <- readTVarIO (dmLoadedEnergyDensities manager)
    return $ M.unions (M.elems loaded)

{- | Get the merged UnitConfig from all loaded unit definitions.
Memoized: pure over the loaded-unit-def set, invalidated on mutation.
-}
getMergedUnitConfig :: DatabaseManager -> IO UnitConversion.UnitConfig
getMergedUnitConfig manager = do
    cached <- readTVarIO (dmMergedUnitConfigCache manager)
    case cached of
        Just cfg -> pure cfg
        Nothing -> do
            loaded <- readTVarIO (dmLoadedUnitDefs manager)
            let !cfg =
                    if M.null loaded
                        then UnitConversion.defaultUnitConfig
                        else UnitConversion.mergeUnitConfigs (M.elems loaded)
            atomically $ writeTVar (dmMergedUnitConfigCache manager) (Just cfg)
            pure cfg

{- | Snapshot of flow + unit metadata across every currently-loaded DB.
Used to characterize or display a cross-DB-merged 'Inventory', whose
flow UUIDs can come from any loaded DB. Without the merge, root-DB-only
metadata silently drops every dep-DB flow during LCIA characterization
(CF lookup falls off the end of the fallback chain) and inventory export.

Memoized on 'dmMergedFlowMetadataCache': the merged Maps are pure over
the loaded-DB set, so the expensive 'M.unions' + UUID collision scan
runs once per DB-set mutation instead of per LCIA call (previously the
dominant source of garbage in 27-wide 'mapConcurrently' characterization).

Detects UUID collisions with divergent metadata. 'M.unions' is first-wins;
collisions should never happen (same UUID ⇒ same flow by construction),
but if data drift produces them, surface via log rather than hide.
-}

{- | Location hierarchy as a 'Map ChildLocation [ParentLocation]', sourced from
'data/geographies.csv' (or the hardcoded fallback). Reused across the LCIA
regionalized scoring path (see 'Method.Mapping.computeRegionalizedLCIAScore').
-}
getLocationHierarchy :: DatabaseManager -> IO (M.Map Location [Location])
getLocationHierarchy = pure . locationHierarchyOf

-- | Pure form of 'getLocationHierarchy', shared by the loading paths.
locationHierarchyOf :: DatabaseManager -> M.Map Location [Location]
locationHierarchyOf manager = M.map (map Location . snd) (M.mapKeysMonotonic Location (dmGeographies manager))

{- | Merged biosphere flow metadata + units across all loaded DBs. Technosphere
flows are not merged here because characterization (the only consumer of
this cache) targets biosphere flows exclusively.
-}
getMergedFlowMetadata :: DatabaseManager -> IO (BioFlowDB, UnitDB)
getMergedFlowMetadata manager = do
    cached <- readTVarIO (dmMergedFlowMetadataCache manager)
    case cached of
        Just snap -> pure snap
        Nothing -> do
            loaded <- readTVarIO (dmLoadedDbs manager)
            let dbs = map ldDatabase (M.elems loaded)
                bioMaps = map dbBioFlows dbs
                unitMaps = map dbUnits dbs
                !mergedBios = M.unions bioMaps
                !mergedUnits = M.unions unitMaps
                bioHits = collisions bioFingerprint bioMaps
                unitHits = collisions unitFingerprint unitMaps
            unless (null bioHits) $
                reportProgress Warning $
                    "[merged BioFlowDB] "
                        <> show (length bioHits)
                        <> " UUID collision(s) with divergent biosphere flow metadata; keeping first. Samples: "
                        <> show (take 3 bioHits)
            unless (null unitHits) $
                reportProgress Warning $
                    "[merged UnitDB] "
                        <> show (length unitHits)
                        <> " UUID collision(s) with divergent unit metadata; keeping first. Samples: "
                        <> show (take 3 unitHits)
            let !snap = (mergedBios, mergedUnits)
            atomically $ writeTVar (dmMergedFlowMetadataCache manager) (Just snap)
            pure snap
  where
    bioFingerprint f = (bfName f, bfCompartmentName f, bfCompartmentSub f)
    unitFingerprint = unitName

    collisions :: (Ord fp) => (v -> fp) -> [Map UUID v] -> [UUID]
    collisions fp ms =
        let step = M.foldlWithKey' (insertFp fp)
            insertFp f acc k v = M.insertWith S.union k (S.singleton (f v)) acc
            merged = foldl step (M.empty :: Map UUID (S.Set fp)) ms
         in [u | (u, fps) <- M.toList merged, S.size fps > 1]

-- | Status of a reference data resource for API responses
data RefDataStatus = RefDataStatus
    { rdsName :: !Text
    , rdsDisplayName :: !Text
    , rdsDescription :: !(Maybe Text)
    , rdsStatus :: !DatabaseLoadStatus
    , rdsIsUploaded :: !Bool
    , rdsIsAuto :: !Bool
    , rdsEntryCount :: !Int
    }
    deriving (Show, Eq, Generic)

instance ToJSON RefDataStatus where
    toJSON RefDataStatus{..} =
        A.object
            [ "rdsName" .= rdsName
            , "rdsDisplayName" .= rdsDisplayName
            , "rdsDescription" .= rdsDescription
            , "rdsStatus" .= rdsStatus
            , "rdsIsUploaded" .= rdsIsUploaded
            , "rdsIsAuto" .= rdsIsAuto
            , "rdsEntryCount" .= rdsEntryCount
            ]

instance FromJSON RefDataStatus where
    parseJSON = A.withObject "RefDataStatus" $ \v ->
        RefDataStatus
            <$> v .: "rdsName"
            <*> v .: "rdsDisplayName"
            <*> v .:? "rdsDescription"
            <*> v .: "rdsStatus"
            <*> v .: "rdsIsUploaded"
            <*> v .: "rdsIsAuto"
            <*> v .: "rdsEntryCount"

--------------------------------------------------------------------------------
-- Generic ref-data operations (shared by flow synonyms, compartment maps, units)
--------------------------------------------------------------------------------

-- | Operations for a ref-data kind — everything that varies between the three.
data RefDataOps a = RefDataOps
    { rdoAvailableVar :: !(DatabaseManager -> TVar (Map Text RefDataConfig))
    , rdoLoadedVar :: !(DatabaseManager -> TVar (Map Text a))
    , rdoParse :: !(BL.ByteString -> Either Text a)
    , rdoCount :: !(a -> Int)
    , rdoLabel :: !String
    , rdoUploadDir :: !FilePath
    , rdoCanDelete :: !(RefDataConfig -> Bool)
    }

flowSynOps :: RefDataOps SynonymDB
flowSynOps =
    RefDataOps
        dmAvailableFlowSyns
        dmLoadedFlowSyns
        (first T.pack . buildFromCSV)
        synonymCount
        "flow synonyms"
        "uploads/flow-synonyms"
        (\rd -> rdIsUploaded rd || rdIsAuto rd)

compMapOps :: RefDataOps CompartmentMap
compMapOps =
    RefDataOps
        dmAvailableCompMaps
        dmLoadedCompMaps
        (first T.pack . buildCompartmentMapFromCSV)
        compartmentMapSize
        "compartment mapping"
        "uploads/compartment-mappings"
        rdIsUploaded

unitDefOps :: RefDataOps UnitConversion.UnitConfig
unitDefOps =
    RefDataOps
        dmAvailableUnitDefs
        dmLoadedUnitDefs
        UnitConversion.buildFromCSV
        UnitConversion.unitCount
        "units"
        "uploads/units"
        rdIsUploaded

energyDensityOps :: RefDataOps EnergyDensityMap
energyDensityOps =
    RefDataOps
        dmAvailableEnergyDensities
        dmLoadedEnergyDensities
        (first T.pack . buildEnergyDensityMapFromCSV)
        energyDensityMapSize
        "energy densities"
        "uploads/energy-densities"
        rdIsUploaded

listRefDataG :: RefDataOps a -> DatabaseManager -> IO [RefDataStatus]
listRefDataG ops manager = do
    available <- readTVarIO (rdoAvailableVar ops manager)
    loaded <- readTVarIO (rdoLoadedVar ops manager)
    return
        [ RefDataStatus
            { rdsName = rdName rd
            , rdsDisplayName = rdName rd
            , rdsDescription = rdDescription rd
            , rdsStatus = if M.member (rdName rd) loaded then Loaded else Unloaded
            , rdsIsUploaded = rdIsUploaded rd
            , rdsIsAuto = rdIsAuto rd
            , rdsEntryCount = maybe 0 (rdoCount ops) (M.lookup (rdName rd) loaded)
            }
        | rd <- M.elems available
        ]

loadRefDataG :: RefDataOps a -> DatabaseManager -> Text -> IO (Either Text ())
loadRefDataG ops manager name = do
    available <- readTVarIO (rdoAvailableVar ops manager)
    case M.lookup name available of
        Nothing -> return $ Left $ T.pack (rdoLabel ops) <> " not found: " <> name
        Just rd -> do
            loaded <- readTVarIO (rdoLoadedVar ops manager)
            if M.member name loaded
                then return $ Right ()
                else do
                    result <- loadRefDataCSV (rdPath rd)
                    case result of
                        Left err -> return $ Left err
                        Right csvData -> case rdoParse ops csvData of
                            Left err -> return $ Left err
                            Right val -> do
                                atomically $ do
                                    modifyTVar' (rdoLoadedVar ops manager) (M.insert name val)
                                    invalidateMergedRefCaches manager
                                reportProgress Info $ "Loaded " <> rdoLabel ops <> ": " <> T.unpack name
                                return $ Right ()

unloadRefDataG :: RefDataOps a -> DatabaseManager -> Text -> IO (Either Text ())
unloadRefDataG ops manager name = do
    loaded <- readTVarIO (rdoLoadedVar ops manager)
    if M.member name loaded
        then do
            atomically $ do
                modifyTVar' (rdoLoadedVar ops manager) (M.delete name)
                invalidateMergedRefCaches manager
            reportProgress Info $ "Unloaded " <> rdoLabel ops <> ": " <> T.unpack name
            return $ Right ()
        else return $ Left $ T.pack (rdoLabel ops) <> " not loaded: " <> name

{- | Drop the merged-ref-data caches. Conservatively clears both — the
flow-metadata and unit-config snapshots are cheap to rebuild lazily, and
ref-data changes (units, flow synonyms, compartment maps) are rare enough
that per-kind dispatch adds no observable value.
-}
invalidateMergedRefCaches :: DatabaseManager -> STM ()
invalidateMergedRefCaches manager = do
    writeTVar (dmMergedFlowMetadataCache manager) Nothing
    writeTVar (dmMergedUnitConfigCache manager) Nothing

addRefDataG :: RefDataOps a -> DatabaseManager -> RefDataConfig -> IO ()
addRefDataG ops manager rd =
    atomically $ modifyTVar' (rdoAvailableVar ops manager) (M.insert (rdName rd) rd)

removeRefDataG :: RefDataOps a -> DatabaseManager -> Text -> IO (Either Text ())
removeRefDataG ops manager name = do
    available <- readTVarIO (rdoAvailableVar ops manager)
    case M.lookup name available of
        Nothing -> return $ Left $ T.pack (rdoLabel ops) <> " not found: " <> name
        Just rd | not (rdoCanDelete ops rd) -> return $ Left $ "Cannot delete preinstalled " <> T.pack (rdoLabel ops)
        Just _ -> do
            loaded <- readTVarIO (rdoLoadedVar ops manager)
            if M.member name loaded
                then return $ Left "Unload before deleting"
                else do
                    removeUploadedRefData (rdoUploadDir ops) name
                    atomically $ modifyTVar' (rdoAvailableVar ops manager) (M.delete name)
                    return $ Right ()

-- | Auto-load active flow synonyms using binary cache for speed
autoLoadFlowSynonyms :: TVar (Map Text SynonymDB) -> [RefDataConfig] -> IO ()
autoLoadFlowSynonyms loadedVar configs =
    forM_ (filter rdActive configs) $ \rd -> do
        result <- loadFromCSVFileWithCache (rdPath rd)
        case result of
            Right synDB -> do
                atomically $ modifyTVar' loadedVar (M.insert (rdName rd) synDB)
                reportProgress Info $
                    "  [OK] Loaded flow synonyms: "
                        <> T.unpack (rdName rd)
                        <> " ("
                        <> show (synonymCount synDB)
                        <> " entries)"
            Left err ->
                reportError $ "  [FAIL] Failed to load flow synonyms " <> T.unpack (rdName rd) <> ": " <> err

-- | Auto-load active reference data at startup
autoLoadRefData :: RefDataOps a -> TVar (Map Text a) -> [RefDataConfig] -> IO ()
autoLoadRefData ops loadedVar configs =
    forM_ (filter rdActive configs) $ \rd -> do
        result <- loadRefDataCSV (rdPath rd)
        case result of
            Right csvData -> case rdoParse ops csvData of
                Right val -> do
                    atomically $ modifyTVar' loadedVar (M.insert (rdName rd) val)
                    reportProgress Info $
                        "  [OK] Loaded "
                            <> rdoLabel ops
                            <> ": "
                            <> T.unpack (rdName rd)
                            <> " ("
                            <> show (rdoCount ops val)
                            <> " entries)"
                Left err ->
                    reportError $ "  [FAIL] Failed to parse " <> rdoLabel ops <> " " <> T.unpack (rdName rd) <> ": " <> T.unpack err
            Left err -> reportError $ "  [FAIL] Failed to read " <> T.unpack (rdName rd) <> ": " <> T.unpack err

-- Public API: delegates to generic ops

listFlowSynonyms :: DatabaseManager -> IO [RefDataStatus]
listFlowSynonyms = listRefDataG flowSynOps

loadFlowSynonyms :: DatabaseManager -> Text -> IO (Either Text ())
loadFlowSynonyms = loadRefDataG flowSynOps

unloadFlowSynonyms :: DatabaseManager -> Text -> IO (Either Text ())
unloadFlowSynonyms = unloadRefDataG flowSynOps

addFlowSynonyms :: DatabaseManager -> RefDataConfig -> IO ()
addFlowSynonyms = addRefDataG flowSynOps

removeFlowSynonyms :: DatabaseManager -> Text -> IO (Either Text ())
removeFlowSynonyms = removeRefDataG flowSynOps

-- | Get synonym groups for a specific loaded flow synonyms resource.
getFlowSynonymGroups :: DatabaseManager -> Text -> IO (Either Text [[Text]])
getFlowSynonymGroups manager name = do
    loaded <- readTVarIO (dmLoadedFlowSyns manager)
    case M.lookup name loaded of
        Nothing -> return $ Left $ "Flow synonyms not loaded: " <> name
        Just synDB -> return $ Right $ M.elems (synIdToNames synDB)

listCompartmentMappings :: DatabaseManager -> IO [RefDataStatus]
listCompartmentMappings = listRefDataG compMapOps

loadCompartmentMappings :: DatabaseManager -> Text -> IO (Either Text ())
loadCompartmentMappings = loadRefDataG compMapOps

unloadCompartmentMappings :: DatabaseManager -> Text -> IO (Either Text ())
unloadCompartmentMappings = unloadRefDataG compMapOps

addCompartmentMappings :: DatabaseManager -> RefDataConfig -> IO ()
addCompartmentMappings = addRefDataG compMapOps

removeCompartmentMappings :: DatabaseManager -> Text -> IO (Either Text ())
removeCompartmentMappings = removeRefDataG compMapOps

listUnitDefs :: DatabaseManager -> IO [RefDataStatus]
listUnitDefs = listRefDataG unitDefOps

loadUnitDefs :: DatabaseManager -> Text -> IO (Either Text ())
loadUnitDefs = loadRefDataG unitDefOps

unloadUnitDefs :: DatabaseManager -> Text -> IO (Either Text ())
unloadUnitDefs = unloadRefDataG unitDefOps

addUnitDefs :: DatabaseManager -> RefDataConfig -> IO ()
addUnitDefs = addRefDataG unitDefOps

removeUnitDefs :: DatabaseManager -> Text -> IO (Either Text ())
removeUnitDefs = removeRefDataG unitDefOps

--------------------------------------------------------------------------------
-- Reference data helpers
--------------------------------------------------------------------------------

{- | Parse a geographies CSV file (code,display_name,parents) into a lookup map.
Parents field uses '|' as separator. display_name is optional (falls back to code).
Lines starting with "code" are treated as headers and skipped.
-}
parseGeographiesCSV :: FilePath -> IO (Map Text (Text, [Text]))
parseGeographiesCSV path = do
    exists <- doesFileExist path
    if not exists
        then do
            reportProgress Info $ "Geographies file not found: " <> path <> " (using built-in hierarchy)"
            return M.empty
        else do
            content <- TIO.readFile path
            let ls = T.lines content
                parsed = concatMap parseLine ls
            reportProgress Info $ "Loaded " <> show (length parsed) <> " geographies from " <> path
            return $ M.fromList parsed
  where
    parseLine line
        | T.null (T.strip line) = []
        | "code" `T.isPrefixOf` line = [] -- header row
        | "#" `T.isPrefixOf` T.strip line = [] -- comment
        | otherwise = case T.splitOn "," line of
            [] -> []
            [_] -> []
            parts@(codeRaw : _) -> case unsnoc parts of
                Nothing -> [] -- unreachable: parts is non-empty by pattern
                Just (initPart, parentsRaw) ->
                    let code = T.strip codeRaw
                        parentsStr = T.strip parentsRaw
                        displayRaw = T.intercalate "," (drop 1 initPart)
                        displayName = let d = T.strip displayRaw in if T.null d then code else d
                        parents = if T.null parentsStr then [] else T.splitOn "|" parentsStr
                     in [(code, (displayName, parents))]

-- | Load CSV file content from path.
loadRefDataCSV :: FilePath -> IO (Either Text BL.ByteString)
loadRefDataCSV path = do
    exists <- doesFileExist path
    if not exists
        then return $ Left $ "File not found: " <> T.pack path
        else Right <$> BL.readFile path

{- | Discover uploaded reference data from a directory.
Each subdirectory should contain a data.csv and optional meta.toml.
-}
discoverUploadedRefData :: FilePath -> IO [RefDataConfig]
discoverUploadedRefData baseDir = do
    exists <- doesDirectoryExist baseDir
    if not exists
        then return []
        else do
            entries <- listDirectory baseDir
            fmap catMaybes $ forM entries $ \entry -> do
                let dirPath = baseDir </> entry
                    csvPath = dirPath </> "data.csv"
                csvExists <- doesFileExist csvPath
                if csvExists
                    then do
                        let name = T.pack entry
                            isAuto = "auto-" `T.isPrefixOf` name
                        reportProgress Info $ "Discovered uploaded ref data: " <> T.unpack name
                        return $
                            Just
                                RefDataConfig
                                    { rdName = name
                                    , rdPath = csvPath
                                    , rdActive = not isAuto -- Auto-extracted synonyms inactive by default (noisy); curated data/flows.csv preferred
                                    , rdIsUploaded = True
                                    , rdIsAuto = isAuto
                                    , rdDescription = Nothing
                                    }
                    else return Nothing

-- | Remove uploaded reference data directory.
removeUploadedRefData :: FilePath -> Text -> IO ()
removeUploadedRefData baseDir name = do
    let uploadDir = baseDir </> T.unpack name
    exists <- doesDirectoryExist uploadDir
    when exists $ do
        result <- try $ removeDirectoryRecursive uploadDir
        case result of
            Left (e :: SomeException) ->
                reportError $ "Failed to delete " <> uploadDir <> ": " <> show e
            Right () ->
                reportProgress Info $ "Deleted: " <> uploadDir

{- | A synonym carried by more distinct flows than this is a classification label
or stop-word (e.g. @"organic"@), not a true synonym — 'excludeOverFrequentSynonyms'
drops it. The bound sits in the gap between the class-label hubs (≥187 flows in
EF 3.1) and the first genuine flow name used as a synonym (~17 flows).
-}
maxSynonymFlowFrequency :: Int
maxSynonymFlowFrequency = 25

{- | Persist auto-extracted synonym pairs as an opt-in candidate set.
Writes CSV to uploads/flow-synonyms/auto-{source}/data.csv and registers it
inactive. The pairs never enter the matching: flow matching trusts only the
curated registry (data/flows.csv) plus sources the user explicitly activates
(activation lasts for the session and only reaches databases loaded after it) —
DB-embedded synonyms are a bootstrap input for offline curation, not a runtime
one. To regenerate a stale candidate, remove the source and reload.
-}
autoCreateFlowSynonyms :: DatabaseManager -> Text -> Text -> [(Text, Text)] -> IO ()
autoCreateFlowSynonyms _ _ _ [] = return ()
autoCreateFlowSynonyms manager sourceName description pairs = do
    let slug = "auto-" <> sourceName
    -- Skip if already registered (persisted candidate from a previous run,
    -- discovered at startup, or extracted earlier this session)
    alreadyExtracted <- atomically $ M.member slug <$> readTVar (dmAvailableFlowSyns manager)
    if alreadyExtracted
        then reportProgress Info $ "  [AUTO] " <> T.unpack slug <> ": candidate already extracted"
        else do
            let (nonJunkPairs, junkTokens) = excludeJunkSynonyms pairs
                (keptPairs, excludedSyns) =
                    excludeOverFrequentSynonyms maxSynonymFlowFrequency nonJunkPairs
            unless (null junkTokens) $
                reportProgress Info $
                    "  [AUTO] "
                        <> T.unpack slug
                        <> ": dropped "
                        <> show (length junkTokens)
                        <> " placeholder/non-substance synonym tokens: "
                        <> T.unpack (T.intercalate ", " (take 8 junkTokens))
            unless (null excludedSyns) $
                reportProgress Info $
                    "  [AUTO] "
                        <> T.unpack slug
                        <> ": excluded "
                        <> show (length excludedSyns)
                        <> " over-frequent synonym tokens (class labels/stop-words): "
                        <> T.unpack
                            ( T.intercalate ", " $
                                map (\(tok, n) -> tok <> "(" <> T.pack (show n) <> ")") (take 8 excludedSyns)
                            )
            let dir = "uploads/flow-synonyms" </> T.unpack slug
                path = dir </> "data.csv"
            createDirectoryIfMissing True dir
            BL.writeFile path (synonymPairsToCSV keptPairs)
            let rd =
                    RefDataConfig
                        { rdName = slug
                        , rdPath = path
                        , rdActive = False -- auto-extracted synonyms inactive by default (noisy, use curated data/flows.csv)
                        , rdIsUploaded = True
                        , rdIsAuto = True
                        , rdDescription = Just description
                        }
            addFlowSynonyms manager rd
            -- Close the candidate set only to audit its quality: an oversized
            -- class means the transitive closure fused unrelated substances
            -- through an ambiguous bridge (a junk hub) — surface it so the
            -- curator sees it before ever activating the source.
            forM_ (oversizedClasses 100 keptPairs) $ \cls ->
                reportProgress Warning $
                    "  [AUTO] "
                        <> T.unpack slug
                        <> ": synonym closure fused "
                        <> show (length cls)
                        <> " names into one class (possible junk hub); e.g. "
                        <> T.unpack (T.intercalate ", " (take 5 cls))
            reportProgress Info $
                "  [AUTO] "
                    <> T.unpack slug
                    <> ": "
                    <> show (length keptPairs)
                    <> " candidate synonym pairs (opt-in, not loaded)"
