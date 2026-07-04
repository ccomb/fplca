{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.Routes where

import API.DatabaseHandlers (simpleAction)
import qualified API.DatabaseHandlers as DBHandlers
import qualified API.OpenApi
import API.Types (ActivateResponse (..), ActivityContribution (..), ActivityInfo (..), ActivitySummary (..), Aggregation (..), BatchImpactsEntry (..), BatchImpactsRequest (..), BatchImpactsResponse (..), BinaryContent (..), CharacterizationEntry (..), CharacterizationResult (..), ClassificationEntryInfo (..), ClassificationPresetInfo (..), ClassificationSystem (..), ConsumersResponse (..), ContributingActivitiesResult (..), ContributingFlowsResult (..), CutoffWasteFlow (..), DatabaseListResponse (..), DeleteSelectionRequest (..), DeleteSelectionResponse (..), ExchangeDetail (..), ExportRequest (..), ExportResponse (..), FlowCFEntry (..), FlowCFMapping (..), FlowContributionEntry (..), FlowDetail (..), FlowSearchResult (..), FlowSummary (..), GraphExport (..), InventoryExport (..), LCIABatchResult (..), LCIAResult (..), LoadDatabaseResponse (..), MappingStatus (..), MethodCollectionListResponse (..), MethodCollectionStatusAPI (..), MethodDetail (..), MethodFactorAPI (..), MethodSummary (..), PerturbedEntry (..), RefDataListResponse (..), RelinkRequest (..), RelinkResponse (..), ScoringIndicator (..), SearchResults (..), SensitivityRequest (..), SensitivityResponse (..), SubstitutionRequest (..), SupplyChainResponse (..), SynonymGroupsResponse (..), TreeExport (..), UnmappedFlowAPI (..), UploadChunk (..), UploadResponse (..), apiFlowOfKind)
import App.Env (AppEnv (..), AppM, runApp)
import qualified Config
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.STM (readTVarIO)
import Control.Exception (evaluate)
import Control.Monad (forM, forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (asum)
import Data.List (find, intercalate, sortBy, sortOn)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import Data.OpenApi (OpenApi, ToSchema)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (diffUTCTime, getCurrentTime)
import qualified Data.UUID as UUID
import qualified Data.Validation as V
import qualified Data.Vector as V
import Database
import Database.Manager (DatabaseManager (..), DatabaseSetupInfo (..), LoadedDatabase (..), MethodCollectionStatus (..), getDatabase, getMergedUnitConfig)
import qualified Database.Manager as DM
import qualified Expr
import GHC.Generics
import qualified GHC.Stats
import Matrix (Inventory, Vector)
import Method.Mapping (LCIAOutcome (..), LongTermMode (..), MappingStats (..), MatchStrategy (..), MethodTables (..), applyLongTermMode, computeLCIAScoreFromTables, computeLCIAScoreSetFromTables, computeMappingStats, inventoryContributions, longTermModeFromExclude, lookupCFForFlow, strategyPriority, sumRegionalizedLCIAScoreCrossDB)
import qualified Method.Mapping
import Method.Types (DamageCategory (..), Method (..), MethodCF (..), MethodCollection (..), NormWeightSet (..), ScoringEvaluation (..), ScoringSet (..), computeFormulaScores)
import qualified Method.Types as MT
import Numeric (showFFloat)
import Plugin.Types (AnalyzeContext (..), AnalyzeHandle (..), PluginRegistry (..))
import Progress (ProgressLevel (Info, Warning), getLogLines, reportProgress)
import Servant
import Servant.OpenApi (toOpenApi)
import qualified Service
import qualified Service.Aggregate as Agg
import SharedSolver (SharedSolver)
import qualified SharedSolver
import Tree (buildLoopAwareTree)
import Types
import qualified Version

-- | API type definition - RESTful design with focused endpoints
type LCAAPI =
    "api"
        :> "v1"
        :> ( "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> Get '[JSON] ActivityInfo
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "flows" :> Get '[JSON] [FlowSummary]
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "inputs" :> Get '[JSON] [ExchangeDetail]
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "outputs" :> Get '[JSON] [ExchangeDetail]
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "reference-product" :> Get '[JSON] FlowDetail
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "tree" :> Get '[JSON] TreeExport
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "inventory" :> Get '[JSON] InventoryExport
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "graph" :> QueryParam "cutoff" Double :> Get '[JSON] GraphExport
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "supply-chain" :> QueryParam "name" Text :> QueryParam "limit" Int :> QueryParam "min-quantity" Double :> QueryParam "offset" Int :> QueryParam "max-depth" Int :> QueryParam "location" Text :> QueryParam "product" Text :> QueryParam "preset" Text :> QueryParams "classification" Text :> QueryParams "classification-value" Text :> QueryParams "classification-mode" Text :> QueryParam "sort" Text :> QueryParam "order" Text :> QueryParam "include-edges" Bool :> Get '[JSON] SupplyChainResponse
                :<|> "db"
                    :> Capture "dbName" Text
                    :> "activity"
                    :> Capture "processId" Text
                    :> "aggregate"
                    :> QueryParam "scope" Text
                    :> QueryParam "is_input" Bool
                    :> QueryParam "max_depth" Int
                    :> QueryParam "filter_name" Text
                    :> QueryParam "filter_name_not" Text
                    :> QueryParam "filter_unit" Text
                    :> QueryParam "preset" Text
                    :> QueryParams "filter_classification" Text
                    :> QueryParam "filter_target_name" Text
                    :> QueryParam "filter_exchange_type" Text
                    :> QueryParam "filter_is_reference" Bool
                    :> QueryParam "group_by" Text
                    :> QueryParam "aggregate" Text
                    :> Get '[JSON] Aggregation
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "impacts" :> Capture "collection" Text :> QueryParam "exclude-long-term" Bool :> Get '[JSON] LCIABatchResult
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "impacts" :> Capture "collection" Text :> QueryParam "exclude-long-term" Bool :> ReqBody '[JSON] SubstitutionRequest :> Post '[JSON] LCIABatchResult
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "impacts" :> Capture "collection" Text :> Capture "methodId" Text :> QueryParam "top-flows" Int :> Get '[JSON] LCIAResult
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "impacts" :> Capture "collection" Text :> Capture "methodId" Text :> ReqBody '[JSON] SubstitutionRequest :> Post '[JSON] LCIAResult
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "sensitivity" :> Capture "collection" Text :> Capture "methodId" Text :> ReqBody '[JSON] SensitivityRequest :> Post '[JSON] SensitivityResponse
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "inventory" :> ReqBody '[JSON] SubstitutionRequest :> Post '[JSON] InventoryExport
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "supply-chain" :> QueryParam "name" Text :> QueryParam "limit" Int :> QueryParam "min-quantity" Double :> QueryParam "offset" Int :> QueryParam "max-depth" Int :> QueryParam "location" Text :> QueryParam "product" Text :> QueryParam "preset" Text :> QueryParams "classification" Text :> QueryParams "classification-value" Text :> QueryParams "classification-mode" Text :> QueryParam "sort" Text :> QueryParam "order" Text :> QueryParam "include-edges" Bool :> ReqBody '[JSON] SubstitutionRequest :> Post '[JSON] SupplyChainResponse
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "consumers" :> QueryParam "name" Text :> QueryParam "location" Text :> QueryParam "product" Text :> QueryParam "preset" Text :> QueryParams "classification" Text :> QueryParams "classification-value" Text :> QueryParams "classification-mode" Text :> QueryParam "limit" Int :> QueryParam "offset" Int :> QueryParam "max-depth" Int :> QueryParam "sort" Text :> QueryParam "order" Text :> QueryParam "include-edges" Bool :> Get '[JSON] ConsumersResponse
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "path-to" :> QueryParam "target" Text :> Get '[JSON] Value
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "analyze" :> Capture "analyzerName" Text :> Get '[JSON] Value
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "contributing-flows" :> Capture "collection" Text :> Capture "methodId" Text :> QueryParam "limit" Int :> QueryParam "exclude-long-term" Bool :> Get '[JSON] ContributingFlowsResult
                :<|> "db" :> Capture "dbName" Text :> "activity" :> Capture "processId" Text :> "contributing-activities" :> Capture "collection" Text :> Capture "methodId" Text :> QueryParam "limit" Int :> QueryParam "exclude-long-term" Bool :> Get '[JSON] ContributingActivitiesResult
                :<|> "db" :> Capture "dbName" Text :> "flow" :> Capture "flowId" Text :> Get '[JSON] FlowDetail
                :<|> "db" :> Capture "dbName" Text :> "flow" :> Capture "flowId" Text :> "activities" :> Get '[JSON] [ActivitySummary]
                :<|> "methods" :> Get '[JSON] [MethodSummary]
                :<|> "method" :> Capture "methodId" Text :> Get '[JSON] MethodDetail
                :<|> "method" :> Capture "methodId" Text :> "factors" :> Get '[JSON] [MethodFactorAPI]
                :<|> "db" :> Capture "dbName" Text :> "method" :> Capture "methodId" Text :> "mapping" :> Get '[JSON] MappingStatus
                :<|> "db" :> Capture "dbName" Text :> "method" :> Capture "methodId" Text :> "flow-mapping" :> Get '[JSON] FlowCFMapping
                :<|> "db" :> Capture "dbName" Text :> "method" :> Capture "methodId" Text :> "characterization" :> QueryParam "flow" Text :> QueryParam "limit" Int :> Get '[JSON] CharacterizationResult
                :<|> "db" :> Capture "dbName" Text :> "flows" :> QueryParam "q" Text :> QueryParam "lang" Text :> QueryParam "limit" Int :> QueryParam "offset" Int :> QueryParam "sort" Text :> QueryParam "order" Text :> Get '[JSON] (SearchResults FlowSearchResult)
                :<|> "db" :> Capture "dbName" Text :> "activities" :> QueryParam "name" Text :> QueryParam "geo" Text :> QueryParam "product" Text :> QueryParam "exact" Bool :> QueryParam "preset" Text :> QueryParams "classification" Text :> QueryParams "classification-value" Text :> QueryParams "classification-mode" Text :> QueryParam "limit" Int :> QueryParam "offset" Int :> QueryParam "sort" Text :> QueryParam "order" Text :> Get '[JSON] (SearchResults ActivitySummary)
                :<|> "db" :> Capture "dbName" Text :> "classifications" :> Get '[JSON] [ClassificationSystem]
                :<|> "db" :> Capture "dbName" Text :> "impacts" :> Capture "collection" Text :> QueryParam "top-flows" Int :> ReqBody '[JSON] BatchImpactsRequest :> Post '[JSON] BatchImpactsResponse
                -- Database management endpoints
                :<|> "db" :> Get '[JSON] DatabaseListResponse
                -- Load/Unload/Delete endpoints
                :<|> "db" :> Capture "dbName" Text :> "load" :> Post '[JSON] LoadDatabaseResponse
                :<|> "db" :> Capture "dbName" Text :> "unload" :> Post '[JSON] ActivateResponse
                -- Relink: empty {} body = plain relink; {depDb,mappingCsv} = mapping relink
                :<|> "db" :> Capture "dbName" Text :> "relink" :> ReqBody '[JSON] RelinkRequest :> Post '[JSON] RelinkResponse
                :<|> "db" :> Capture "dbName" Text :> "copy" :> Capture "newName" Text :> Post '[JSON] ActivateResponse
                :<|> "db" :> Capture "dbName" Text :> Delete '[JSON] ActivateResponse
                -- Delete the whole filtered set of activities (selection in JSON body)
                :<|> "db" :> Capture "dbName" Text :> "delete" :> ReqBody '[JSON] DeleteSelectionRequest :> Post '[JSON] DeleteSelectionResponse
                -- Export a loaded database to a serialized (base64) file in the requested format
                :<|> "db" :> Capture "dbName" Text :> "export" :> ReqBody '[JSON] ExportRequest :> Post '[JSON] ExportResponse
                -- Upload endpoint (streamed octet-stream body; metadata in query params)
                :<|> "db" :> "upload" :> QueryParam "name" Text :> QueryParam "description" Text :> StreamBody NoFraming OctetStream (SourceIO UploadChunk) :> Post '[JSON] UploadResponse
                -- Database setup endpoints (for cross-DB linking configuration)
                :<|> "db" :> Capture "dbName" Text :> "setup" :> Get '[JSON] DatabaseSetupInfo
                :<|> "db" :> Capture "dbName" Text :> "add-dependency" :> Capture "depName" Text :> Post '[JSON] DatabaseSetupInfo
                :<|> "db" :> Capture "dbName" Text :> "remove-dependency" :> Capture "depName" Text :> Post '[JSON] DatabaseSetupInfo
                :<|> "db" :> Capture "dbName" Text :> "set-data-path" :> ReqBody '[JSON] Value :> Post '[JSON] DatabaseSetupInfo
                :<|> "db" :> Capture "dbName" Text :> "finalize" :> Post '[JSON] ActivateResponse
                -- Method collection endpoints
                :<|> "method-collections" :> Get '[JSON] MethodCollectionListResponse
                :<|> "method-collections" :> Capture "name" Text :> "load" :> Post '[JSON] ActivateResponse
                :<|> "method-collections" :> Capture "name" Text :> "unload" :> Post '[JSON] ActivateResponse
                :<|> "method-collections" :> Capture "name" Text :> Delete '[JSON] ActivateResponse
                :<|> "method-collections" :> "upload" :> QueryParam "name" Text :> QueryParam "description" Text :> StreamBody NoFraming OctetStream (SourceIO UploadChunk) :> Post '[JSON] UploadResponse
                -- Reference data endpoints (flow synonyms, compartment mappings, units)
                :<|> "flow-synonyms" :> Get '[JSON] RefDataListResponse
                :<|> "flow-synonyms" :> Capture "name" Text :> "load" :> Post '[JSON] ActivateResponse
                :<|> "flow-synonyms" :> Capture "name" Text :> "unload" :> Post '[JSON] ActivateResponse
                :<|> "flow-synonyms" :> Capture "name" Text :> Delete '[JSON] ActivateResponse
                :<|> "flow-synonyms" :> "upload" :> QueryParam "name" Text :> QueryParam "description" Text :> StreamBody NoFraming OctetStream (SourceIO UploadChunk) :> Post '[JSON] UploadResponse
                :<|> "flow-synonyms" :> Capture "name" Text :> "groups" :> Get '[JSON] SynonymGroupsResponse
                :<|> "flow-synonyms" :> Capture "name" Text :> "download" :> Get '[OctetStream] (Headers '[Header "Content-Disposition" Text] BinaryContent)
                :<|> "compartment-mappings" :> Get '[JSON] RefDataListResponse
                :<|> "compartment-mappings" :> Capture "name" Text :> "load" :> Post '[JSON] ActivateResponse
                :<|> "compartment-mappings" :> Capture "name" Text :> "unload" :> Post '[JSON] ActivateResponse
                :<|> "compartment-mappings" :> Capture "name" Text :> Delete '[JSON] ActivateResponse
                :<|> "compartment-mappings" :> "upload" :> QueryParam "name" Text :> QueryParam "description" Text :> StreamBody NoFraming OctetStream (SourceIO UploadChunk) :> Post '[JSON] UploadResponse
                :<|> "units" :> Get '[JSON] RefDataListResponse
                :<|> "units" :> Capture "name" Text :> "load" :> Post '[JSON] ActivateResponse
                :<|> "units" :> Capture "name" Text :> "unload" :> Post '[JSON] ActivateResponse
                :<|> "units" :> Capture "name" Text :> Delete '[JSON] ActivateResponse
                :<|> "units" :> "upload" :> QueryParam "name" Text :> QueryParam "description" Text :> StreamBody NoFraming OctetStream (SourceIO UploadChunk) :> Post '[JSON] UploadResponse
                -- Log endpoint
                :<|> "logs" :> QueryParam "since" Int :> Get '[JSON] Value
                -- Auth endpoint (login)
                :<|> "auth" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] (Headers '[Header "Set-Cookie" String] Value)
                -- Version endpoint
                :<|> "version" :> Get '[JSON] Value
                -- Hosting config (for managed instances)
                :<|> "hosting" :> Get '[JSON] Value
                -- Runtime stats (memory usage)
                :<|> "stats" :> Get '[JSON] Value
                -- Classification presets (from TOML config)
                :<|> "classification-presets" :> Get '[JSON] [ClassificationPresetInfo]
                -- OpenAPI spec, enriched with operationId/description from API.Resources.
                -- pyvolca's runtime dispatcher reads this to route operation_id → HTTP.
                :<|> "openapi.json" :> Get '[JSON] Value
           )

{- | Standard prefix for "database not loaded" 404 bodies. Exported so that
'API.BatchImpacts.translateError' can recover a typed 'DatabaseNotLoaded'
from the wire body without the two ends silently drifting apart.
-}
databaseNotLoadedPrefix :: Text
databaseNotLoadedPrefix = "Database not loaded: "

-- | Same idea for "collection not loaded" 404 bodies.
collectionNotLoadedPrefix :: Text
collectionNotLoadedPrefix = "Collection not loaded: "

-- | Build the 404 body for a not-loaded database / collection by name.
notLoadedBody :: Text -> Text -> BSL.ByteString
notLoadedBody prefix name = BSL.fromStrict (T.encodeUtf8 (prefix <> name))

-- | Get database by name, throw 404 if not loaded
requireDatabaseByName :: Text -> AppM (Database, SharedSolver)
requireDatabaseByName dbName = do
    dbManager <- asks aeDbManager
    maybeLoaded <- liftIO $ getDatabase dbManager dbName
    case maybeLoaded of
        Just loaded -> return (ldDatabase loaded, ldSharedSolver loaded)
        Nothing -> throwError err404{errBody = notLoadedBody databaseNotLoadedPrefix dbName}

{- | Refuse LCIA when the DB still has unresolved cross-DB products. Forces
the user to load the missing dep DBs (or POST {} to /relink) rather than
silently undercounting impacts.
-}
requireFullyLinked :: Text -> Database -> AppM ()
requireFullyLinked dbName db =
    let n = unresolvedCount (dbLinkingStats db)
     in when (n > 0) $
            throwError
                err422
                    { errBody =
                        BSL.fromStrict $
                            T.encodeUtf8 $
                                "Database \""
                                    <> dbName
                                    <> "\" has "
                                    <> T.pack (show n)
                                    <> " unresolved cross-DB products. Load the missing dependency "
                                    <> "databases (see GET /api/v1/db/"
                                    <> dbName
                                    <> "/setup) then POST {} to /api/v1/db/"
                                    <> dbName
                                    <> "/relink."
                    }

-- | Inventory with cross-DB back-substitution; maps unit-conversion errors to 422.
inventoryWithDeps :: Text -> Database -> SharedSolver -> ProcessId -> AppM Inventory
inventoryWithDeps dbName db solver pid =
    SharedSolver.csInventory <$> solutionWithDeps dbName db solver pid

{- | Cross-DB inventory + per-DB scaling vectors. The scalings are needed by
the regionalized LCIA path (per-DB dot products summed across all DBs
reached at request time); the inventory alone is enough for non-regional
methods.
-}
solutionWithDeps :: Text -> Database -> SharedSolver -> ProcessId -> AppM SharedSolver.CrossDBSolution
solutionWithDeps dbName db solver pid = do
    dbManager <- asks aeDbManager
    requireFullyLinked dbName db
    unitCfg <- liftIO $ getMergedUnitConfig dbManager
    res <-
        liftIO $
            SharedSolver.computeInventoryMatrixWithDepsCached
                unitCfg
                (DM.mkDepSolverLookup dbManager)
                db
                dbName
                solver
                pid
    case res of
        Right sol -> pure sol
        Left err -> throwError err422{errBody = BSL.fromStrict $ T.encodeUtf8 err}

-- | Batch inventory with cross-DB back-substitution; maps unit-conversion errors to 422.
inventoriesWithDeps :: Text -> Database -> SharedSolver -> [ProcessId] -> AppM [Inventory]
inventoriesWithDeps dbName db solver pids =
    map SharedSolver.csInventory <$> solutionsWithDeps dbName db solver pids

-- | Batch variant of 'solutionWithDeps'.
solutionsWithDeps :: Text -> Database -> SharedSolver -> [ProcessId] -> AppM [SharedSolver.CrossDBSolution]
solutionsWithDeps dbName db solver pids = do
    dbManager <- asks aeDbManager
    requireFullyLinked dbName db
    unitCfg <- liftIO $ getMergedUnitConfig dbManager
    res <-
        liftIO $
            SharedSolver.computeInventoryMatrixBatchWithDepsCached
                unitCfg
                (DM.mkDepSolverLookup dbManager)
                db
                dbName
                solver
                pids
    case res of
        Right sols -> pure sols
        Left err -> throwError err422{errBody = BSL.fromStrict $ T.encodeUtf8 err}

{- | Per-method static context, prepared ONCE per batch request and reused
across every pid in that request. Carries the precomputed mapping stats
and total mapped-flow count; combined with the precomputed broadcast
(and regionalized activity weights) on 'MethodTables', this is enough
to build an LCIAResult per pid in O(1) per method — no TVar reads, no
per-pid cache lookups, no inventoryContributions walk.
-}
data MethodCtx = MethodCtx
    { mctxMethod :: !Method
    , mctxMappedFlows :: !Int
    }

{- | Build an 'ActivityContribution' row from a cross-DB contribution key
@(depDbName, pid)@. For dep-DB rows the process ID is qualified as
@"dbName::actUUID_prodUUID"@ — same convention as
'Service.hs' (activity-detail endpoint) so the UI's existing
cross-DB navigation handles it. Root-DB rows keep the bare @pid@ form.
-}
mkCrossDBContrib ::
    DatabaseManager ->
    -- | root DB name
    Text ->
    -- | merged biosphere flow DB
    BioFlowDB ->
    -- | merged unitDB
    UnitDB ->
    -- | total score (for share %)
    Double ->
    ((Text, ProcessId), Double) ->
    IO ActivityContribution
mkCrossDBContrib dbManager rootDbName _flowDB unitDB score ((depDbName, pid), c) = do
    mLd <- DM.getDatabase dbManager depDbName
    pure $ case mLd of
        Just ld ->
            let d = ldDatabase ld
                mAct = Service.findActivityByProcessId d pid
                pidText =
                    if depDbName == rootDbName
                        then processIdToText d pid
                        else depDbName <> "::" <> processIdToText d pid
                -- Reference products are technosphere; pull from the dep DB's tech flows.
                (prodName, _, _) = maybe ("", 0, "") (Service.getReferenceProductInfo (dbTechFlows d) unitDB) mAct
             in ActivityContribution
                    { acProcessId = pidText
                    , acActivityName = maybe "" activityName mAct
                    , acProductName = prodName
                    , acLocation = maybe "" activityLocation mAct
                    , acContribution = c
                    , acSharePct = if score /= 0 then c / score * 100 else 0
                    }
        Nothing ->
            ActivityContribution
                { acProcessId = depDbName <> "::<unloaded>"
                , acActivityName = ""
                , acProductName = ""
                , acLocation = ""
                , acContribution = c
                , acSharePct = if score /= 0 then c / score * 100 else 0
                }

-- | Helper function to validate ProcessId and lookup activity
withValidatedActivity :: Database -> Text -> (Activity -> AppM a) -> AppM a
withValidatedActivity db processId action =
    either throwServiceError action (Service.resolveActivityByProcessId db processId)

{- | Helper function to validate UUID and lookup flow. Returns a tagged sum
so callers can dispatch on tech vs bio.
-}
withValidatedFlow :: Database -> Text -> (FlowKind -> AppM a) -> AppM a
withValidatedFlow db uuid action = do
    validUuid <- either throwServiceError pure (Service.validateUUID uuid)
    let lookups =
            [ TechKind <$> M.lookup validUuid (dbTechFlows db)
            , BioKind <$> M.lookup validUuid (dbBioFlows db)
            , WasteKind <$> M.lookup validUuid (dbWasteFlows db)
            ]
    case asum lookups of
        Just flow -> action flow
        Nothing -> throwError err404{errBody = "Flow not found"}

-- | Login request body
newtype LoginRequest = LoginRequest
    { lrCode :: Text
    }
    deriving (Generic)

instance FromJSON LoginRequest where
    parseJSON = withObject "LoginRequest" $ \v ->
        LoginRequest <$> v .: "code"

-- ToSchema orphan for the login request — lives here (not in API.OpenApi)
-- to avoid a circular dependency.
instance ToSchema LoginRequest

{- | The complete OpenAPI 3.0 specification for the VoLCA REST API.

Built in two steps:
  1. 'toOpenApi' derives the structural spec from the 'LCAAPI' Servant type.
  2. 'API.OpenApi.enrichWithResources' stamps @operationId@, @summary@, and
     the long @description@ onto each operation with a matching entry in
     'API.Resources'. This makes pyvolca's runtime dispatcher able to key
     on @operationId@ (e.g. @"get_impacts"@).
-}
volcaOpenApi :: OpenApi
volcaOpenApi = API.OpenApi.stampInfo (API.OpenApi.enrichWithResources (toOpenApi (Proxy :: Proxy LCAAPI)))

{- | Expand a named classification preset from config into the
(system, value, exact) triples used by the Service/Aggregate layers.
An unknown preset name yields the empty list (same behavior as the
previous inline implementations in searchActivitiesWithCount and
getActivityConsumers).
-}
expandPreset :: [Config.ClassificationPreset] -> Maybe Text -> [(Text, Text, Bool)]
expandPreset _ Nothing = []
expandPreset presets (Just pn) = case find (\p -> Config.cpName p == pn) presets of
    Just p -> [(Config.ceSystem e, Config.ceValue e, Config.ceMode e == "exact") | e <- Config.cpFilters p]
    Nothing -> []

-- ============================================================================
-- Hoisted helpers — previously in lcaServer's `where`. Lifted to top level so
-- non-Servant callers (notably src/API/BatchImpacts.hs and any client of the
-- LCIA batch pipeline outside the Servant AppM stack) can reuse them.
--
-- Behavior is byte-identical to the original where-bound versions.
-- ============================================================================

-- | Enrich a raw LCIA result with damage category mapping and NW scores. Pure.
enrichWithNW :: M.Map Text Text -> Maybe NormWeightSet -> LCIAResult -> LCIAResult
enrichWithNW dcLookup mNW result =
    let dmgCat = M.findWithDefault (lrCategory result) (lrCategory result) dcLookup
        (normScore, weightScore) = case mNW of
            Just nw ->
                let mNorm = M.lookup dmgCat (nwNormalization nw)
                    mWeight = M.lookup dmgCat (nwWeighting nw)
                 in case (mNorm, mWeight) of
                        (Just n, Just w) ->
                            let ns = lrScore result * n
                             in (Just ns, Just (ns * w))
                        _ -> (Nothing, Nothing)
            Nothing -> (Nothing, Nothing)
     in result
            { lrDamageCategory = dmgCat
            , lrNormalizedScore = normScore
            , lrWeightedScore = weightScore
            }

-- | Assemble an LCIABatchResult from the post-characterization parts. Pure.
mkLCIABatchResult ::
    [LCIAResult] ->
    Maybe NormWeightSet ->
    [NormWeightSet] ->
    M.Map Text (M.Map Text Double) ->
    [ScoringSet] ->
    M.Map Text (M.Map Text ScoringIndicator) ->
    [CutoffWasteFlow] ->
    LCIABatchResult
mkLCIABatchResult results mNW nwSets scoringResults scoringSets scoringIndicators cutoffWaste =
    LCIABatchResult
        { lbrResults = results
        , lbrSingleScore = Nothing
        , lbrSingleScoreUnit = Nothing
        , lbrNormWeightSetName = nwName <$> mNW
        , lbrAvailableNWsets = map nwName nwSets
        , lbrScoringResults = scoringResults
        , lbrScoringUnits = M.fromList [(ssName ss, ssUnit ss) | ss <- scoringSets]
        , lbrScoringIndicators = scoringIndicators
        , lbrCutoffWaste = cutoffWaste
        }

-- | Per-category single-line log within a batch.
logBatchCategory :: Int -> LCIAResult -> IO ()
logBatchCategory _invSize result = do
    let scoreTxt = showFFloat (Just 4) (lrScore result) ""
    reportProgress Info $
        "  "
            <> T.unpack (lrMethodName result)
            <> ": "
            <> scoreTxt
            <> " "
            <> T.unpack (lrUnit result)
            <> " ("
            <> show (lrMappedFlows result)
            <> " CFs mapped)"

-- | Single-method LCIA log line.
logLCIAResult :: LCIAResult -> Method -> IO ()
logLCIAResult result method = do
    let mapped = lrMappedFlows result
    reportProgress Info $
        "[LCIA] "
            <> T.unpack (methodName method)
            <> ": "
            <> showFFloat (Just 4) (lrScore result) ""
            <> " "
            <> T.unpack (methodUnit method)
    reportProgress Info $ "  Flow mapping: " <> show mapped <> " CFs mapped"

{- | Resolve a process_id text against a database, throwing the appropriate
HTTP status. Validates the resolved ProcessId against the technosphere
matrix index too — see Service.validateProcessIdInMatrixIndex.
-}
resolveOrThrow :: Database -> Text -> AppM (ProcessId, Activity)
resolveOrThrow db processIdText = do
    (pid, act) <- either throwServiceError pure (Service.resolveActivityAndProcessId db processIdText)
    either throwServiceError pure (Service.validateProcessIdInMatrixIndex db pid)
    pure (pid, act)

{- | Pure mapping from a domain 'Service.ServiceError' to the HTTP error it
surfaces as. Every constructor is a client-supplied invariant breakage, so all
map to 4xx/422 — never 5xx. Kept pure (and separate from 'throwServiceError')
so the status contract is unit-testable without booting a server.
-}
serviceErrorToServerError :: Service.ServiceError -> ServerError
serviceErrorToServerError = \case
    Service.InvalidUUID msg -> err400{errBody = utf8Body msg}
    Service.InvalidProcessId msg -> err400{errBody = utf8Body msg}
    Service.ActivityNotFound _ -> err404{errBody = "Activity not found"}
    Service.FlowNotFound _ -> err404{errBody = "Flow not found"}
    -- MatrixError covers singular Sherman-Morrison, missing technosphere links,
    -- and cross-DB unit-conversion failures — all client-submitted invariant
    -- breakages. Surface as 422 like the rest of the cross-DB pipeline.
    Service.MatrixError msg -> err422{errBody = utf8Body msg}
  where
    utf8Body = BSL.fromStrict . T.encodeUtf8

throwServiceError :: Service.ServiceError -> AppM a
throwServiceError = throwError . serviceErrorToServerError

-- | Load a method collection by name from the live DatabaseManager state.
loadCollection :: Text -> AppM ([Method], [DamageCategory], [NormWeightSet], [ScoringSet])
loadCollection collectionName = do
    dbManager <- asks aeDbManager
    loadedCollections <- liftIO $ readTVarIO (dmLoadedMethods dbManager)
    case M.lookup collectionName loadedCollections of
        Just mc -> return (mcMethods mc, mcDamageCategories mc, mcNormWeightSets mc, mcScoringSets mc)
        Nothing -> throwError err404{errBody = notLoadedBody collectionNotLoadedPrefix collectionName}

{- | Cross-DB inventory solution for an activity. 'Nothing' takes the cached
no-substitution path ('requireFullyLinked' runs inside 'solutionWithDeps');
'Just' applies the substitutions through the uncached path.
-}
crossDBSolutionFor :: Text -> Database -> SharedSolver -> ProcessId -> Maybe SubstitutionRequest -> AppM SharedSolver.CrossDBSolution
crossDBSolutionFor dbName db solver pid mSub = case mSub of
    Nothing -> solutionWithDeps dbName db solver pid
    Just subReq -> do
        dbManager <- asks aeDbManager
        requireFullyLinked dbName db
        unitCfg <- liftIO $ getMergedUnitConfig dbManager
        eSol <-
            liftIO $
                Service.inventoryWithSubsAndDeps
                    unitCfg
                    (DM.mkDepSolverLookup dbManager)
                    db
                    dbName
                    solver
                    pid
                    (srSubstitutions subReq)
        either throwServiceError pure eSol

{- | Apply the request's long-term policy to a freshly solved inventory. Drops
the delayed long-term emission flows when excluding, and is a no-op (with no
extra work) when including. Every downstream scoring path reads @csInventory@,
so filtering here once keeps scores and top-contributor breakdowns consistent.
-}
applyLongTermToSolution :: DatabaseManager -> LongTermMode -> SharedSolver.CrossDBSolution -> IO SharedSolver.CrossDBSolution
applyLongTermToSolution _ IncludeLongTerm sol = pure sol
applyLongTermToSolution dbManager ExcludeLongTerm sol = do
    (mFlows, _) <- DM.getMergedFlowMetadata dbManager
    pure sol{SharedSolver.csInventory = applyLongTermMode mFlows ExcludeLongTerm (SharedSolver.csInventory sol)}

{- | Look up MethodSetTables for every (DB, scaling) pair in the cross-DB
solution. Cache-keyed by (dbName, methods).
-}
buildPerDbSetTables ::
    DatabaseManager ->
    Text ->
    NE.NonEmpty (Text, Database, Vector) ->
    [Method] ->
    IO (NE.NonEmpty (Database, Vector, Method.Mapping.MethodSetTables))
buildPerDbSetTables dbManager collection scalings methods =
    traverse
        ( \(n, d, sv) -> do
            mst <- DM.mapMethodSetToTablesCached dbManager n collection d methods
            pure (d, sv, mst)
        )
        scalings

{- | Score every method in 'methods' against the inventory in 'sol' in a
single dense matvec (non-regional) plus per-method passes (regional).
-}
batchedScoresFor ::
    DatabaseManager ->
    Text ->
    Text ->
    Database ->
    SharedSolver.CrossDBSolution ->
    [Method] ->
    IO (M.Map UUID (Either Text Double))
batchedScoresFor dbManager _dbName collection _db sol methods = do
    perDb <- buildPerDbSetTables dbManager collection (SharedSolver.csScalings sol) methods
    unitCfg <- getMergedUnitConfig dbManager
    (mFlows, mUnits) <- DM.getMergedFlowMetadata dbManager
    hier <- DM.getLocationHierarchy dbManager
    pure $
        M.fromList $
            computeLCIAScoreSetFromTables
                unitCfg
                mUnits
                mFlows
                (SharedSolver.csInventory sol)
                hier
                perDb

{- | Pre-warm per-(db, method) cached tables so subsequent 'batchedScoresFor'
and 'inventoryContributions' calls hit a warm cache.
-}
prepMethodCtx :: DatabaseManager -> Text -> Text -> Database -> Method -> IO MethodCtx
prepMethodCtx dbManager dbName collection db method = do
    mappings <- DM.mapMethodToFlowsCached dbManager dbName collection db method
    _ <- DM.mapMethodToTablesCached dbManager dbName collection db method
    let stats = computeMappingStats mappings
    pure MethodCtx{mctxMethod = method, mctxMappedFlows = msTotal stats - msUnmatched stats}

{- | Compute LCIA result for a single method against a cross-DB inventory
solution. 'precomputedScore' short-circuits the per-method scoring loop
when a batched matvec result is already available.
-}
computeCategoryResult ::
    DatabaseManager ->
    Text ->
    Text ->
    Database ->
    SharedSolver.CrossDBSolution ->
    Activity ->
    Int ->
    Maybe (Either Text Double) ->
    Method ->
    IO LCIAResult
computeCategoryResult dbManager dbName collection db sol activity topFlows precomputedScore method = do
    unitCfg <- getMergedUnitConfig dbManager
    (mFlows, mUnits) <- DM.getMergedFlowMetadata dbManager
    mappings <- DM.mapMethodToFlowsCached dbManager dbName collection db method
    tables <- DM.mapMethodToTablesCached dbManager dbName collection db method
    let inventory = SharedSolver.csInventory sol
    let stats = computeMappingStats mappings
    score <- case precomputedScore of
        Just (Right s) -> evaluate s
        Just (Left err) -> do
            reportProgress Warning $
                "[LCIA " <> T.unpack (methodName method) <> "] " <> T.unpack err
            evaluate (0 :: Double)
        Nothing ->
            if M.null (mtRegionalizedCF tables)
                then evaluate $ loScore (computeLCIAScoreFromTables unitCfg mUnits mFlows inventory tables)
                else do
                    hier <- DM.getLocationHierarchy dbManager
                    perDb <-
                        forM (NE.toList (SharedSolver.csScalings sol)) $ \(n, d, sv) -> do
                            tbls <- DM.mapMethodToTablesCached dbManager n collection d method
                            pure (d, sv, tbls)
                    case sumRegionalizedLCIAScoreCrossDB unitCfg mUnits mFlows hier perDb of
                        Right s -> evaluate s
                        Left err -> do
                            reportProgress Warning $
                                "[LCIA " <> T.unpack (methodName method) <> "] " <> T.unpack err
                            evaluate (0 :: Double)
    let (prodName, prodAmount, prodUnit) = Service.getReferenceProductInfo (dbTechFlows db) mUnits activity
        functionalUnit = T.pack (showFFloat (Just 2) prodAmount "") <> " " <> prodUnit <> " of " <> prodName
        (rawContribs, unknownUuids) = inventoryContributions unitCfg mUnits mFlows inventory tables
        contribs = sortOn (\(_, _, c) -> negate (abs c)) rawContribs
        topContribs = take topFlows contribs
        topContributors =
            [ FlowContributionEntry
                { fcoFlowName = bfName f
                , fcoContribution = c
                , fcoSharePct = if score /= 0 then c / score * 100 else 0
                , fcoFlowId = UUID.toText (bfId f)
                , fcoCategory = bfCompartmentName f
                , fcoCompartment = bfCompartmentSub f
                , fcoCfValue = cfVal
                }
            | (f, cfVal, c) <- topContribs
            ]
    unless (null unknownUuids) $
        reportProgress Warning $
            "[LCIA "
                <> T.unpack (methodName method)
                <> "] "
                <> show (length unknownUuids)
                <> " inventory flow UUID(s) absent from merged FlowDB — characterization incomplete. Samples: "
                <> show (take 3 unknownUuids)
    pure
        LCIAResult
            { lrMethodId = methodId method
            , lrMethodName = methodName method
            , lrCategory = methodCategory method
            , lrDamageCategory = methodCategory method
            , lrScore = score
            , lrUnit = methodUnit method
            , lrNormalizedScore = Nothing
            , lrWeightedScore = Nothing
            , lrMappedFlows = msTotal stats - msUnmatched stats
            , lrFunctionalUnit = functionalUnit
            , lrTopContributors = topContributors
            }

{- | Batch fast path: scores via 'batchedScoresFor', then build LCIAResult
records straight from the precomputed contexts. Skips the per-pid
'mapConcurrently' over methods entirely. When 'topFlows' is 0
(the default) 'lrTopContributors' is left empty and the contribution
walk is skipped; when >0, runs 'inventoryContributions' per method.
-}
buildLCIABatchResultCached ::
    DatabaseManager ->
    Text ->
    Text ->
    Database ->
    ProcessId ->
    Activity ->
    MethodCollection ->
    SharedSolver.CrossDBSolution ->
    [MethodCtx] ->
    Int ->
    IO LCIABatchResult
buildLCIABatchResultCached dbManager dbName collectionName db actPid activity collection sol ctxs topFlows = do
    let damageCats = mcDamageCategories collection
        nwSets = mcNormWeightSets collection
        dcLookup =
            M.fromList
                [ (subName, dcName dc)
                | dc <- damageCats
                , (subName, _) <- dcImpacts dc
                ]
        mNW = case nwSets of (nw : _) -> Just nw; [] -> Nothing
        methods = map mctxMethod ctxs
        inventory = SharedSolver.csInventory sol
    scoreMap <- batchedScoresFor dbManager dbName collectionName db sol methods
    (mFlows, mUnits) <- DM.getMergedFlowMetadata dbManager
    let unknownUuids =
            [ fid
            | (fid, qty) <- M.toList inventory
            , qty /= 0
            , not (M.member fid mFlows)
            ]
    unless (null unknownUuids) $
        reportProgress Warning $
            "[LCIA batch] pid="
                <> show actPid
                <> ": "
                <> show (length unknownUuids)
                <> " inventory flow UUID(s) absent from merged FlowDB — characterization incomplete. Samples: "
                <> show (take 3 unknownUuids)
    mUnitCfg <-
        if topFlows > 0
            then Just <$> getMergedUnitConfig dbManager
            else pure Nothing
    let (prodName, prodAmount, prodUnit) = Service.getReferenceProductInfo (dbTechFlows db) mUnits activity
        functionalUnit = T.pack (showFFloat (Just 2) prodAmount "") <> " " <> prodUnit <> " of " <> prodName
        mkResultIO ctx = do
            let method = mctxMethod ctx
            score <- case M.lookup (methodId method) scoreMap of
                Just (Right s) -> pure s
                Just (Left err) -> do
                    reportProgress Warning $
                        "[LCIA "
                            <> T.unpack (methodName method)
                            <> "] pid="
                            <> show actPid
                            <> ": "
                            <> T.unpack err
                    pure 0
                Nothing -> pure 0
            topContributors <- case mUnitCfg of
                Nothing -> pure []
                Just unitCfg -> do
                    tables <- DM.mapMethodToTablesCached dbManager dbName collectionName db method
                    let (rawContribs, _unknownUuids) =
                            inventoryContributions unitCfg mUnits mFlows inventory tables
                        sorted = sortOn (\(_, _, c) -> negate (abs c)) rawContribs
                        top = take topFlows sorted
                    pure
                        [ FlowContributionEntry
                            { fcoFlowName = bfName f
                            , fcoContribution = c
                            , fcoSharePct = if score /= 0 then c / score * 100 else 0
                            , fcoFlowId = UUID.toText (bfId f)
                            , fcoCategory = bfCompartmentName f
                            , fcoCompartment = bfCompartmentSub f
                            , fcoCfValue = cfVal
                            }
                        | (f, cfVal, c) <- top
                        ]
            pure $
                enrichWithNW dcLookup mNW $
                    LCIAResult
                        { lrMethodId = methodId method
                        , lrMethodName = methodName method
                        , lrCategory = methodCategory method
                        , lrDamageCategory = methodCategory method
                        , lrScore = score
                        , lrUnit = methodUnit method
                        , lrNormalizedScore = Nothing
                        , lrWeightedScore = Nothing
                        , lrMappedFlows = mctxMappedFlows ctx
                        , lrFunctionalUnit = functionalUnit
                        , lrTopContributors = topContributors
                        }
    results <- traverse mkResultIO ctxs
    let rawScoreMap = rawScoreMapByName results
    (scoringResults, scoringIndicators) <-
        computeAllScoringSets (mcScoringSets collection) rawScoreMap
    pure (mkLCIABatchResult results mNW nwSets scoringResults (mcScoringSets collection) scoringIndicators (Service.buildCutoffWaste db activity))

{- | Top-level LCIA batch entry point — AppM-returning. Used by the Servant
routes (via thin where-aliases) and by API.BatchImpacts.
-}
activityLCIABatchH ::
    Text ->
    Text ->
    Text ->
    Maybe SubstitutionRequest ->
    LongTermMode ->
    AppM LCIABatchResult
activityLCIABatchH dbName processIdText collectionName mSub ltMode = do
    dbManager <- asks aeDbManager
    (db, sharedSolver) <- requireDatabaseByName dbName
    (actProcessId, activity) <- resolveOrThrow db processIdText
    (methods, damageCats, nwSets, scoringSets) <- loadCollection collectionName
    let dcLookup = M.fromList [(subName, dcName dc) | dc <- damageCats, (subName, _) <- dcImpacts dc]
        mNW = case nwSets of (nw : _) -> Just nw; [] -> Nothing
    t0 <- liftIO getCurrentTime
    sol <- crossDBSolutionFor dbName db sharedSolver actProcessId mSub >>= liftIO . applyLongTermToSolution dbManager ltMode
    t1 <- liftIO getCurrentTime
    let inventory = SharedSolver.csInventory sol
        !invSize = M.size inventory
    when (isNothing mSub) $
        liftIO $ do
            reportProgress Info $
                "[LCIA batch] " <> T.unpack collectionName <> " for " <> T.unpack (activityName activity)
            reportProgress Info $
                "  Inventory: "
                    <> show invSize
                    <> " flows ("
                    <> showFFloat (Just 2) (realToFrac (diffUTCTime t1 t0) :: Double) ""
                    <> "s)"
            when (invSize == 0) $
                reportProgress Info "  WARNING: inventory is empty — check matrix computation"
            when (invSize > 0 && invSize <= 5) $
                reportProgress Info $
                    "  Inventory UUIDs: " <> intercalate ", " (map UUID.toString $ M.keys inventory)
    scoreMap <- liftIO $ batchedScoresFor dbManager dbName collectionName db sol methods
    rawResults <-
        liftIO $
            mapConcurrently
                (\m -> computeCategoryResult dbManager dbName collectionName db sol activity 5 (M.lookup (methodId m) scoreMap) m)
                methods
    let results = map (enrichWithNW dcLookup mNW) rawResults
        rawScoreMap = rawScoreMapByName rawResults
    (scoringResults, scoringIndicators) <- liftIO $ computeAllScoringSets scoringSets rawScoreMap
    when (isNothing mSub) $
        liftIO $ do
            t2 <- getCurrentTime
            mapM_ (logBatchCategory invSize) results
            reportProgress Info $
                "  Total: "
                    <> show (length results)
                    <> " categories ("
                    <> showFFloat (Just 2) (realToFrac (diffUTCTime t2 t0) :: Double) ""
                    <> "s)"
            forM_ (M.toList scoringResults) $ \(name, scores) ->
                reportProgress Info $
                    "  Scoring '"
                        <> T.unpack name
                        <> "': "
                        <> intercalate ", " [T.unpack k <> "=" <> showFFloat (Just 6) v "" | (k, v) <- M.toList scores]
    pure (mkLCIABatchResult results mNW nwSets scoringResults scoringSets scoringIndicators (Service.buildCutoffWaste db activity))

{- | Top-level multi-activity batch impacts. One MUMPS multi-RHS solve for all
valid PIDs, parallel characterization. Used by the Servant POST route and
by API.BatchImpacts.
-}
batchImpactsH ::
    Text ->
    Text ->
    Maybe Int ->
    BatchImpactsRequest ->
    AppM BatchImpactsResponse
batchImpactsH dbName collectionName topFlowsParam req = do
    dbManager <- asks aeDbManager
    (db, sharedSolver) <- requireDatabaseByName dbName
    loadedCollections <- liftIO $ readTVarIO (dmLoadedMethods dbManager)
    collection <- case M.lookup collectionName loadedCollections of
        Just mc -> pure mc
        Nothing -> throwError err404{errBody = notLoadedBody collectionNotLoadedPrefix collectionName}
    let resolved =
            [ (pidText, Service.resolveActivityAndProcessId db pidText)
            | pidText <- birProcessIds req
            ]
        valid = [(pidText, pidNum, act) | (pidText, Right (pidNum, act)) <- resolved]
        notFound = [pidText | (pidText, Left (Service.ActivityNotFound _)) <- resolved]
        invalid = [pidText | (pidText, Left (Service.InvalidProcessId _)) <- resolved]
        validPidNums = [pidNum | (_, pidNum, _) <- valid]
    t0 <- liftIO getCurrentTime
    sols <- solutionsWithDeps dbName db sharedSolver validPidNums
    t1 <- liftIO getCurrentTime
    ctxs <- liftIO $ mapConcurrently (prepMethodCtx dbManager dbName collectionName db) (mcMethods collection)
    let topFlows = max 0 (fromMaybe 0 topFlowsParam)
    let mkEntry ((pidText, pidNum, activity), sol) = do
            impacts <- buildLCIABatchResultCached dbManager dbName collectionName db pidNum activity collection sol ctxs topFlows
            pure
                BatchImpactsEntry
                    { bieProcessId = pidText
                    , bieActivityName = activityName activity
                    , bieImpacts = impacts
                    }
    entries <- liftIO $ mapM mkEntry (zip valid sols)
    t2 <- liftIO getCurrentTime
    liftIO $
        reportProgress Info $
            "[batch-impacts] "
                <> T.unpack dbName
                <> " / "
                <> T.unpack collectionName
                <> ": "
                <> show (length valid)
                <> " activities"
                <> ( if null notFound && null invalid
                        then ""
                        else
                            " ("
                                <> show (length notFound)
                                <> " not_found, "
                                <> show (length invalid)
                                <> " invalid)"
                   )
                <> " — solve "
                <> showFFloat (Just 2) (realToFrac (diffUTCTime t1 t0) :: Double) ""
                <> "s, "
                <> "total "
                <> showFFloat (Just 2) (realToFrac (diffUTCTime t2 t0) :: Double) ""
                <> "s"
    pure
        BatchImpactsResponse
            { birResults = entries
            , birNotFound = notFound
            , birInvalid = invalid
            }

-- ---------------------------------------------------------------------------
-- Pure helpers shared by handlers
-- ---------------------------------------------------------------------------

-- | Parse "System=Value[:exact]" into (system, value, isExact).
parseClassFilter :: Text -> Maybe (Text, Text, Bool)
parseClassFilter raw =
    let (sys, rest) = T.breakOn "=" raw
     in if T.null rest
            then Nothing
            else
                let valAndMode = T.drop 1 rest
                    (val, mode) = T.breakOn ":" valAndMode
                    isExact = T.drop 1 mode == "exact"
                 in Just (T.strip sys, T.strip val, isExact)

-- | Merge preset-derived and explicit (system, value, exact) classification filters.
mergeClassFilters ::
    [Config.ClassificationPreset] ->
    Maybe Text ->
    [Text] ->
    [Text] ->
    [Text] ->
    [(Text, Text, Bool)]
mergeClassFilters presets presetParam systems values modes =
    expandPreset presets presetParam
        ++ zipWith3
            (\s v m -> (s, v, m == "exact"))
            systems
            values
            (modes ++ repeat "contains")

-- | Build a 'Service.SupplyChainFilter' shared by GET and POST handlers.
buildSupplyChainFilter ::
    [Config.ClassificationPreset] ->
    Maybe Text ->
    Maybe Int ->
    Maybe Double ->
    Maybe Int ->
    Maybe Int ->
    Maybe Text ->
    Maybe Text ->
    Maybe Text ->
    [Text] ->
    [Text] ->
    [Text] ->
    Maybe Text ->
    Maybe Text ->
    Service.SupplyChainFilter
buildSupplyChainFilter presets nameFilter limitParam minQuantity offsetParam maxDepthParam locationFilter productFilter presetParam classSystems classValues classModes sortParam orderParam =
    Service.SupplyChainFilter
        { Service.scfCore =
            Service.ActivityFilterCore
                { Service.afcName = nameFilter
                , Service.afcLocation = locationFilter
                , Service.afcProduct = productFilter
                , Service.afcClassifications = mergeClassFilters presets presetParam classSystems classValues classModes
                , Service.afcLimit = limitParam
                , Service.afcOffset = offsetParam
                , Service.afcSort = sortParam
                , Service.afcOrder = orderParam
                }
        , Service.scfMaxDepth = maxDepthParam
        , Service.scfMinQuantity = minQuantity
        }

buildFlowEntry :: Database -> MethodTables -> M.Map UUID (MethodCF, MatchStrategy) -> UUID -> FlowCFEntry
buildFlowEntry db tables reverseIndex uuid =
    let mFlow = M.lookup uuid (dbBioFlows db)
        mMatch = M.lookup uuid reverseIndex
     in FlowCFEntry
            { fceFlowId = uuid
            , fceFlowName = maybe "" bfName mFlow
            , fceFlowCategory = maybe "" bfCompartmentName mFlow
            , -- The CF this flow actually scores with: the read-side lookup, so
              -- a flow reached via a medium-level or CAS-bridge fallback (no
              -- single build-side CF resolved to it) still reports as covered —
              -- exactly what scoring sees. mMatch only annotates how a direct
              -- match resolved; it is absent for fallback-covered flows.
              fceCfValue = fmap fst (mFlow >>= lookupCFForFlow tables uuid . Just)
            , fceCfFlowName = fmap (mcfFlowName . fst) mMatch
            , fceMatchStrategy = fmap (strategyToText . snd) mMatch
            }

strategyToText :: MatchStrategy -> Text
strategyToText ByUUID = "uuid"
strategyToText ByCAS = "cas"
strategyToText ByName = "name"
strategyToText BySynonym = "synonym"
strategyToText ByFuzzy = "fuzzy"
strategyToText ByProxy = "proxy"
strategyToText NoMatch = "none"

matchesQuery :: Maybe Text -> Text -> Text -> Bool
matchesQuery Nothing _ _ = True
matchesQuery (Just q) cfName dbFlowName =
    T.isInfixOf q (T.toLower cfName) || T.isInfixOf q (T.toLower dbFlowName)

cfToAPI :: MethodCF -> MethodFactorAPI
cfToAPI cf =
    MethodFactorAPI
        { mfaFlowRef = mcfFlowRef cf
        , mfaFlowName = mcfFlowName cf
        , mfaDirection = case mcfDirection cf of
            MT.Input -> "Input"
            MT.Output -> "Output"
        , mfaValue = mcfValue cf
        }

-- ---------------------------------------------------------------------------
-- AppM helpers
-- ---------------------------------------------------------------------------

{- | Lookup a method by UUID across all loaded collections, returning the
collection name it was found in alongside the method. The collection name is
needed to key the per-method CF caches (a UUID alone collides across
collections that share a method name). First-match on ambiguity, mirroring
'API.MCP.resolveMethod'.
-}
loadMethodByUUID :: Text -> AppM (Text, Method)
loadMethodByUUID uuidText = do
    dbManager <- asks aeDbManager
    loadedMethods <- liftIO $ DM.getLoadedMethods dbManager
    case UUID.fromText uuidText of
        Nothing -> throwError err400{errBody = "Invalid method UUID format"}
        Just uuid ->
            case filter (\(_, m) -> methodId m == uuid) loadedMethods of
                ((col, m) : _) -> return (col, m)
                [] -> throwError err404{errBody = "Method not found"}

{- | Resolve a method by UUID *within a named collection*. Unlike
'loadMethodByUUID' (first-match across all collections), this guarantees the
method's CFs belong to @collectionName@ — required wherever the result keys a
collection-scoped cache, so a method's factors and its cache slot never disagree.
-}
loadMethodInCollection :: Text -> Text -> AppM Method
loadMethodInCollection collectionName uuidText = do
    (methods, _, _, _) <- loadCollection collectionName
    case UUID.fromText uuidText of
        Nothing -> throwError err400{errBody = "Invalid method UUID format"}
        Just uuid -> case filter ((== uuid) . methodId) methods of
            (m : _) -> pure m
            [] -> throwError err404{errBody = "Method not found in collection"}

-- | Resolve (db, solver, ProcessId, Activity, Method) within @collectionName@ and dispatch.
withActivityAndMethod ::
    Text ->
    Text ->
    Text ->
    Text ->
    (Database -> SharedSolver -> ProcessId -> Activity -> Method -> AppM a) ->
    AppM a
withActivityAndMethod dbName collectionName processIdText methodIdText k = do
    (db, sharedSolver) <- requireDatabaseByName dbName
    method <- loadMethodInCollection collectionName methodIdText
    case Service.resolveActivityAndProcessId db processIdText of
        Left (Service.ActivityNotFound _) -> throwError err404{errBody = "Activity not found"}
        Left (Service.InvalidProcessId _) -> throwError err400{errBody = "Invalid ProcessId format"}
        Left err -> throwError err500{errBody = BSL.fromStrict $ T.encodeUtf8 $ T.pack $ show err}
        Right (actProcessId, activity) -> k db sharedSolver actProcessId activity method

-- ---------------------------------------------------------------------------
-- Servant handlers (top-level AppM actions)
-- ---------------------------------------------------------------------------

getOpenApiSpec :: AppM Value
getOpenApiSpec = return $ toJSON volcaOpenApi

{- | Wire-format revision advertised on /api/v1/version. BUMP this whenever a
breaking change to the JSON wire shape lands (field rename/removal, type
narrowing, newly-required field). Clients compare it to decide compatibility.
-}
currentWireVersion :: Int
currentWireVersion = 1

getVersion :: AppM Value
getVersion =
    return $
        object
            [ "version" .= Version.version
            , "gitHash" .= Version.gitHash
            , "gitTag" .= Version.gitTag
            , "buildTarget" .= Version.buildTarget
            , "wireVersion" .= currentWireVersion
            ]

getHosting :: AppM Value
getHosting = do
    hostingConfig <- asks aeHostingConfig
    return $ case hostingConfig of
        Just hc ->
            object
                [ "is_hosted" .= True
                , "max_uploads" .= Config.hcMaxUploads hc
                , "max_upload_mb" .= Config.hcMaxUploadMb hc
                , "api_access" .= Config.hcApiAccess hc
                , "upgrade_upload" .= Config.hcUpgradeUpload hc
                , "upgrade_api" .= Config.hcUpgradeApi hc
                , "upgrade_vm_size" .= Config.hcUpgradeVmSize hc
                ]
        Nothing ->
            object
                [ "is_hosted" .= False
                , "max_uploads" .= (-1 :: Int)
                , "max_upload_mb" .= (-1 :: Int)
                , "api_access" .= True
                , "upgrade_upload" .= ("" :: Text)
                , "upgrade_api" .= ("" :: Text)
                , "upgrade_vm_size" .= ("" :: Text)
                ]

getStats :: AppM Value
getStats = liftIO $ do
    enabled <- GHC.Stats.getRTSStatsEnabled
    if enabled
        then do
            stats <- GHC.Stats.getRTSStats
            return $
                object
                    [ "memory_used_bytes" .= GHC.Stats.gcdetails_live_bytes (GHC.Stats.gc stats)
                    , "memory_allocated_bytes" .= GHC.Stats.allocated_bytes stats
                    , "gc_count" .= GHC.Stats.gcs stats
                    ]
        else
            return $
                object
                    ["error" .= ("RTS stats not enabled. Run with +RTS -T to enable." :: Text)]

getClassificationPresets :: AppM [ClassificationPresetInfo]
getClassificationPresets = do
    presets <- asks aeClassificationPresets
    return $ map toInfo presets
  where
    toInfo p =
        ClassificationPresetInfo
            { cpiName = Config.cpName p
            , cpiLabel = Config.cpLabel p
            , cpiDescription = Config.cpDescription p
            , cpiFilters = map (\e -> ClassificationEntryInfo (Config.ceSystem e) (Config.ceValue e) (Config.ceMode e)) (Config.cpFilters p)
            }

getLogsHandler :: Maybe Int -> AppM Value
getLogsHandler sinceMaybe = do
    let since = fromMaybe 0 sinceMaybe
    (nextIndex, logLines) <- liftIO $ getLogLines since
    return $
        object
            [ "lines" .= logLines
            , "nextIndex" .= nextIndex
            ]

postAuth :: LoginRequest -> AppM (Headers '[Header "Set-Cookie" String] Value)
postAuth loginReq = do
    password <- asks aePassword
    case password of
        Nothing ->
            return $ noHeader $ object ["ok" .= True]
        Just pwd ->
            if T.unpack (lrCode loginReq) == pwd
                then
                    let cookieValue = "volca_session=" ++ pwd ++ "; Path=/; HttpOnly; SameSite=Strict"
                     in return $ addHeader cookieValue $ object ["ok" .= True]
                else
                    throwError err401{errBody = "{\"error\":\"invalid code\"}"}

getActivityInfo :: Text -> Text -> AppM ActivityInfo
getActivityInfo dbName processId = do
    dbManager <- asks aeDbManager
    (db, _) <- requireDatabaseByName dbName
    unitCfg <- liftIO $ getMergedUnitConfig dbManager
    result <- either throwServiceError pure (Service.getActivityInfo unitCfg db processId)
    case fromJSON result of
        Success activityInfo -> return activityInfo
        Error err -> throwError err500{errBody = BSL.fromStrict $ T.encodeUtf8 $ T.pack err}

getActivityFlows :: Text -> Text -> AppM [FlowSummary]
getActivityFlows dbName processId = do
    (db, _) <- requireDatabaseByName dbName
    withValidatedActivity db processId $ \activity ->
        return $ Service.getActivityFlowSummaries db activity

getActivityInputs :: Text -> Text -> AppM [ExchangeDetail]
getActivityInputs dbName processId = do
    (db, _) <- requireDatabaseByName dbName
    withValidatedActivity db processId $ \activity ->
        return $ Service.getActivityInputDetails db activity

getActivityOutputs :: Text -> Text -> AppM [ExchangeDetail]
getActivityOutputs dbName processId = do
    (db, _) <- requireDatabaseByName dbName
    withValidatedActivity db processId $ \activity ->
        return $ Service.getActivityOutputDetails db activity

getActivityReferenceProduct :: Text -> Text -> AppM FlowDetail
getActivityReferenceProduct dbName processId = do
    (db, _) <- requireDatabaseByName dbName
    withValidatedActivity db processId $ \activity ->
        case Service.getActivityReferenceProductDetail db activity of
            Nothing -> throwError err404{errBody = "No reference product found"}
            Just refProduct -> return refProduct

getActivityTree :: Text -> Text -> AppM TreeExport
getActivityTree dbName processId = do
    dbManager <- asks aeDbManager
    maxTreeDepth <- asks aeMaxTreeDepth
    (db, _) <- requireDatabaseByName dbName
    withValidatedActivity db processId $ \_activity -> do
        let activityUuidText = case T.splitOn "_" processId of
                (uuid : _) -> uuid
                [] -> processId
        case UUID.fromText activityUuidText of
            Nothing -> throwError err400{errBody = "Invalid activity UUID format"}
            Just activityUuid -> do
                unitCfg <- liftIO $ getMergedUnitConfig dbManager
                let loopAwareTree = buildLoopAwareTree unitCfg db activityUuid maxTreeDepth
                return $ Service.convertToTreeExport db processId maxTreeDepth loopAwareTree

{- | Inventory with optional substitutions; goes through the cross-DB
back-substitution path so dep-DB inventories merge into the response.
-}
activityInventoryCore :: Text -> Text -> Maybe SubstitutionRequest -> AppM InventoryExport
activityInventoryCore dbName processIdText mSub = do
    dbManager <- asks aeDbManager
    (db, sharedSolver) <- requireDatabaseByName dbName
    (processId, activity) <- resolveOrThrow db processIdText
    sol <- crossDBSolutionFor dbName db sharedSolver processId mSub
    (mFlows, mUnits) <- liftIO $ DM.getMergedFlowMetadata dbManager
    pure $ Service.convertToInventoryExport db mFlows mUnits processId activity (SharedSolver.csInventory sol)

getActivityInventory :: Text -> Text -> AppM InventoryExport
getActivityInventory dbName processIdText = activityInventoryCore dbName processIdText Nothing

getActivityGraph :: Text -> Text -> Maybe Double -> AppM GraphExport
getActivityGraph dbName processId maybeCutoff = do
    (db, sharedSolver) <- requireDatabaseByName dbName
    let cutoffPercent = fromMaybe 1.0 maybeCutoff
    result <- liftIO $ Service.buildActivityGraph db sharedSolver processId cutoffPercent
    either throwServiceError pure result

{- | Supply-chain core (scaling-vector based). 'Nothing' takes the cached
solve; 'Just' applies substitutions via the cross-DB resolver.
-}
activitySupplyChainCore ::
    Text ->
    Text ->
    Maybe Text ->
    Maybe Int ->
    Maybe Double ->
    Maybe Int ->
    Maybe Int ->
    Maybe Text ->
    Maybe Text ->
    Maybe Text ->
    [Text] ->
    [Text] ->
    [Text] ->
    Maybe Text ->
    Maybe Text ->
    Maybe Bool ->
    Maybe SubstitutionRequest ->
    AppM SupplyChainResponse
activitySupplyChainCore dbName processIdText nameFilter limitParam minQuantity offsetParam maxDepthParam locationFilter productFilter presetParam classSystems classValues classModes sortParam orderParam includeEdgesParam mSub = do
    dbManager <- asks aeDbManager
    presets <- asks aeClassificationPresets
    (db, sharedSolver) <- requireDatabaseByName dbName
    let includeEdges = fromMaybe False includeEdgesParam
        scf =
            buildSupplyChainFilter
                presets
                nameFilter
                limitParam
                minQuantity
                offsetParam
                maxDepthParam
                locationFilter
                productFilter
                presetParam
                classSystems
                classValues
                classModes
                sortParam
                orderParam
    case mSub of
        Nothing -> do
            unitCfg <- liftIO $ DM.getMergedUnitConfig dbManager
            result <- liftIO $ Service.getSupplyChain unitCfg (DM.mkDepSolverLookup dbManager) db dbName sharedSolver processIdText scf includeEdges
            either throwServiceError pure result
        Just subReq -> do
            unitCfg <- liftIO $ DM.getMergedUnitConfig dbManager
            (processId, _) <- resolveOrThrow db processIdText
            scalingResult <-
                liftIO $
                    Service.computeScalingVectorWithSubstitutionsCrossDB
                        unitCfg
                        (DM.mkDepSolverLookup dbManager)
                        db
                        dbName
                        sharedSolver
                        processId
                        (srSubstitutions subReq)
            case scalingResult of
                Left err -> throwServiceError err
                Right (scalingVec, virtualLinks) -> do
                    eResp <-
                        liftIO $
                            Service.buildSupplyChainFromScalingVectorCrossDB
                                unitCfg
                                (DM.mkDepSolverLookup dbManager)
                                db
                                dbName
                                processId
                                scalingVec
                                virtualLinks
                                scf
                                includeEdges
                    either throwServiceError pure eResp

getActivitySupplyChain ::
    Text ->
    Text ->
    Maybe Text ->
    Maybe Int ->
    Maybe Double ->
    Maybe Int ->
    Maybe Int ->
    Maybe Text ->
    Maybe Text ->
    Maybe Text ->
    [Text] ->
    [Text] ->
    [Text] ->
    Maybe Text ->
    Maybe Text ->
    Maybe Bool ->
    AppM SupplyChainResponse
getActivitySupplyChain dbName processIdText nameFilter limitParam minQuantity offsetParam maxDepthParam locationFilter productFilter presetParam classSystems classValues classModes sortParam orderParam includeEdgesParam =
    activitySupplyChainCore dbName processIdText nameFilter limitParam minQuantity offsetParam maxDepthParam locationFilter productFilter presetParam classSystems classValues classModes sortParam orderParam includeEdgesParam Nothing

{- | Aggregate endpoint with accumulating field-level validation (a single
request can report invalid `scope` and invalid `aggregate` together).
-}
getActivityAggregate ::
    Text ->
    Text ->
    Maybe Text ->
    Maybe Bool ->
    Maybe Int ->
    Maybe Text ->
    Maybe Text ->
    Maybe Text ->
    Maybe Text ->
    [Text] ->
    Maybe Text ->
    Maybe Text ->
    Maybe Bool ->
    Maybe Text ->
    Maybe Text ->
    AppM Aggregation
getActivityAggregate dbName processId scopeParam isInputParam maxDepthParam fnameParam fnameNotParam funitParam presetParam fclassParams ftargetParam fexchangeTypeParam freferenceParam groupByParam aggregateParam = do
    dbManager <- asks aeDbManager
    presets <- asks aeClassificationPresets
    (db, sharedSolver) <- requireDatabaseByName dbName
    let parseScope = \case
            Just "direct" -> V.Success Agg.ScopeDirect
            Just "supply_chain" -> V.Success Agg.ScopeSupplyChain
            Just "biosphere" -> V.Success Agg.ScopeBiosphere
            _ -> V.failure "scope must be one of: direct | supply_chain | biosphere"
        parseExType = \case
            Nothing -> V.Success Nothing
            Just "technosphere" -> V.Success (Just Agg.KindTechnosphere)
            Just "biosphere" -> V.Success (Just Agg.KindBiosphere)
            Just "waste" -> V.Success (Just Agg.KindWaste)
            Just _ -> V.failure "filter_exchange_type must be one of: technosphere | biosphere | waste"
        parseAgg = \case
            Nothing -> V.Success Agg.AggSum
            Just "sum_quantity" -> V.Success Agg.AggSum
            Just "count" -> V.Success Agg.AggCount
            Just "share" -> V.Success Agg.AggShare
            Just other -> V.failure ("aggregate must be one of: sum_quantity | count | share (got " <> other <> ")")
    (scope, exchangeType, aggFn) <-
        case V.toEither $ (,,) <$> parseScope scopeParam <*> parseExType fexchangeTypeParam <*> parseAgg aggregateParam of
            Left errs -> throwError err400{errBody = BSL.fromStrict (T.encodeUtf8 (T.intercalate "; " (NE.toList errs)))}
            Right v -> pure v
    case (exchangeType, scope) of
        (Just _, Agg.ScopeBiosphere) ->
            throwError err400{errBody = "filter_exchange_type is redundant with scope=biosphere"}
        (Just _, Agg.ScopeSupplyChain) ->
            throwError err400{errBody = "filter_exchange_type is not supported with scope=supply_chain (all entries are technosphere)"}
        _ -> return ()
    let presetFilters = expandPreset presets presetParam
        explicitFilters = mapMaybe parseClassFilter fclassParams
        params =
            Agg.AggregateParams
                { Agg.apScope = scope
                , Agg.apIsInput = isInputParam
                , Agg.apMaxDepth = maxDepthParam
                , Agg.apFilterName = fnameParam
                , Agg.apFilterNameNot = maybe [] (map T.strip . T.splitOn ",") fnameNotParam
                , Agg.apFilterUnit = funitParam
                , Agg.apFilterClassifications = presetFilters ++ explicitFilters
                , Agg.apFilterTargetName = ftargetParam
                , Agg.apFilterExchangeType = exchangeType
                , Agg.apFilterIsReference = freferenceParam
                , Agg.apGroupBy = groupByParam
                , Agg.apAggregate = aggFn
                }
    unitCfg <- liftIO $ getMergedUnitConfig dbManager
    (mFlows, mUnits) <- liftIO $ DM.getMergedFlowMetadata dbManager
    result <- liftIO $ Agg.aggregate unitCfg mFlows mUnits db dbName sharedSolver (DM.mkDepSolverLookup dbManager) processId params
    either throwServiceError pure result

{- | LCIA single-method core. GET passes a top-flows param and logs;
POST carries substitutions instead and skips logging.
-}
activityLCIACore :: Text -> Text -> Text -> Text -> Maybe Int -> Maybe SubstitutionRequest -> AppM LCIAResult
activityLCIACore dbName processIdText collectionName methodIdText topFlowsParam mSub = do
    dbManager <- asks aeDbManager
    (db, sharedSolver) <- requireDatabaseByName dbName
    method <- loadMethodInCollection collectionName methodIdText
    (processId, activity) <- resolveOrThrow db processIdText
    sol <- crossDBSolutionFor dbName db sharedSolver processId mSub
    result <- liftIO $ computeCategoryResult dbManager dbName collectionName db sol activity (fromMaybe 5 topFlowsParam) Nothing method
    when (isNothing mSub) $ liftIO $ logLCIAResult result method
    pure result

getActivityLCIA :: Text -> Text -> Text -> Text -> Maybe Int -> AppM LCIAResult
getActivityLCIA dbName processIdText collectionName methodIdText topFlowsParam =
    activityLCIACore dbName processIdText collectionName methodIdText topFlowsParam Nothing

postActivityLCIA :: Text -> Text -> Text -> Text -> SubstitutionRequest -> AppM LCIAResult
postActivityLCIA dbName processIdText collectionName methodIdText subReq =
    activityLCIACore dbName processIdText collectionName methodIdText Nothing (Just subReq)

{- | Sensitivity sweep: rank-1 perturbations on the root scaling, scored
through the cross-DB graph (regional CFs on dep DBs still apply).
-}
postActivitySensitivity :: Text -> Text -> Text -> Text -> SensitivityRequest -> AppM SensitivityResponse
postActivitySensitivity dbName processIdText collectionName methodIdText senReq = do
    dbManager <- asks aeDbManager
    (db, sharedSolver) <- requireDatabaseByName dbName
    requireFullyLinked dbName db
    method <- loadMethodInCollection collectionName methodIdText
    (processId, activity) <- resolveOrThrow db processIdText
    eRes <- liftIO $ Service.computeSensitivities db sharedSolver processId (srPerturbations senReq)
    (baselineX, perResults) <- either throwServiceError pure eRes
    unitCfg <- liftIO $ getMergedUnitConfig dbManager
    let depLookup = DM.mkDepSolverLookup dbManager
        scaleToSolution x = do
            eSol <-
                SharedSolver.goWithDepsFromScalings
                    unitCfg
                    depLookup
                    db
                    dbName
                    []
                    [x]
                    0
            pure $ case eSol of
                Left err -> Left err
                Right (sol : _) -> Right sol
                Right [] -> Left "cross-DB propagation returned empty result"
        buildEntry baselineLcia (p, eitherX) = case eitherX of
            Left err -> pure (PerturbedEntry p (Left err))
            Right x' -> do
                eSol <- scaleToSolution x'
                case eSol of
                    Left err -> pure (PerturbedEntry p (Left err))
                    Right sol -> do
                        lcia <- computeCategoryResult dbManager dbName collectionName db sol activity 5 Nothing method
                        pure (PerturbedEntry p (Right (lcia, lrScore lcia - lrScore baselineLcia)))
    eBaselineSol <- liftIO $ scaleToSolution baselineX
    baselineSol <-
        either
            (\err -> throwError err422{errBody = BSL.fromStrict $ T.encodeUtf8 err})
            pure
            eBaselineSol
    baselineLcia <-
        liftIO $
            computeCategoryResult dbManager dbName collectionName db baselineSol activity 5 Nothing method
    perturbed <-
        liftIO $
            mapConcurrently (buildEntry baselineLcia) perResults
    pure SensitivityResponse{srBaseline = baselineLcia, srPerturbed = perturbed}

getActivityLCIABatch :: Text -> Text -> Text -> Maybe Bool -> AppM LCIABatchResult
getActivityLCIABatch dbName processIdText collectionName mExcludeLT =
    activityLCIABatchH dbName processIdText collectionName Nothing (longTermModeFromExclude (fromMaybe False mExcludeLT))

postActivityLCIABatch :: Text -> Text -> Text -> Maybe Bool -> SubstitutionRequest -> AppM LCIABatchResult
postActivityLCIABatch dbName processIdText collectionName mExcludeLT subReq =
    activityLCIABatchH dbName processIdText collectionName (Just subReq) (longTermModeFromExclude (fromMaybe False mExcludeLT))

postActivityInventory :: Text -> Text -> SubstitutionRequest -> AppM InventoryExport
postActivityInventory dbName processIdText subReq = activityInventoryCore dbName processIdText (Just subReq)

postActivitySupplyChain ::
    Text ->
    Text ->
    Maybe Text ->
    Maybe Int ->
    Maybe Double ->
    Maybe Int ->
    Maybe Int ->
    Maybe Text ->
    Maybe Text ->
    Maybe Text ->
    [Text] ->
    [Text] ->
    [Text] ->
    Maybe Text ->
    Maybe Text ->
    Maybe Bool ->
    SubstitutionRequest ->
    AppM SupplyChainResponse
postActivitySupplyChain dbName processIdText nameFilter limitParam minQuantity offsetParam maxDepthParam locationFilter productFilter presetParam classSystems classValues classModes sortParam orderParam includeEdgesParam subReq =
    activitySupplyChainCore dbName processIdText nameFilter limitParam minQuantity offsetParam maxDepthParam locationFilter productFilter presetParam classSystems classValues classModes sortParam orderParam includeEdgesParam (Just subReq)

getActivityConsumers ::
    Text ->
    Text ->
    Maybe Text ->
    Maybe Text ->
    Maybe Text ->
    Maybe Text ->
    [Text] ->
    [Text] ->
    [Text] ->
    Maybe Int ->
    Maybe Int ->
    Maybe Int ->
    Maybe Text ->
    Maybe Text ->
    Maybe Bool ->
    AppM ConsumersResponse
getActivityConsumers dbName processIdText nameFilter locationFilter productFilter presetParam classSystems classValues classModes limitParam offsetParam maxDepthParam sortParam orderParam includeEdgesParam = do
    presets <- asks aeClassificationPresets
    (db, _) <- requireDatabaseByName dbName
    let cnf =
            Service.ConsumerFilter
                { Service.cnfCore =
                    Service.ActivityFilterCore
                        { Service.afcName = nameFilter
                        , Service.afcLocation = locationFilter
                        , Service.afcProduct = productFilter
                        , Service.afcClassifications = mergeClassFilters presets presetParam classSystems classValues classModes
                        , Service.afcLimit = limitParam
                        , Service.afcOffset = offsetParam
                        , Service.afcSort = sortParam
                        , Service.afcOrder = orderParam
                        }
                , Service.cnfMaxDepth = maxDepthParam
                , Service.cnfIncludeEdges = fromMaybe False includeEdgesParam
                }
    either throwServiceError pure (Service.getConsumers db dbName processIdText cnf)

getActivityPathTo :: Text -> Text -> Maybe Text -> AppM Value
getActivityPathTo dbName processIdText targetParam = do
    (db, solver) <- requireDatabaseByName dbName
    target <-
        maybe
            (throwError err400{errBody = "Missing required 'target' query parameter"})
            pure
            targetParam
    result <- liftIO $ Service.getPathTo db solver processIdText target
    case result of
        Left (Service.ActivityNotFound msg) ->
            throwError err404{errBody = BSL.fromStrict $ T.encodeUtf8 msg}
        Left (Service.InvalidProcessId msg) ->
            throwError err400{errBody = BSL.fromStrict $ T.encodeUtf8 msg}
        Left err ->
            throwError err500{errBody = BSL.fromStrict $ T.encodeUtf8 $ T.pack $ show err}
        Right val -> return val

getActivityAnalyze :: Text -> Text -> Text -> AppM Value
getActivityAnalyze dbName processIdText analyzerName = do
    dbManager <- asks aeDbManager
    (db, sharedSolver) <- requireDatabaseByName dbName
    case M.lookup analyzerName (prAnalyzers (dmPlugins dbManager)) of
        Nothing -> throwError err404{errBody = "Analyzer not found: " <> BSL.fromStrict (T.encodeUtf8 analyzerName)}
        Just analyzer -> do
            case Service.resolveActivityAndProcessId db processIdText of
                Left (Service.ActivityNotFound _) -> throwError err404{errBody = "Activity not found"}
                Left (Service.InvalidProcessId _) -> throwError err400{errBody = "Invalid ProcessId format"}
                Left err -> throwError err500{errBody = BSL.fromStrict $ T.encodeUtf8 $ T.pack $ show err}
                Right (actProcessId, _) -> do
                    inventory <- inventoryWithDeps dbName db sharedSolver actProcessId
                    (mFlows, mUnits) <- liftIO $ DM.getMergedFlowMetadata dbManager
                    loadedMethods <- liftIO $ DM.getLoadedMethods dbManager
                    let methods = map snd loadedMethods
                        ctx =
                            AnalyzeContext
                                { acDatabase = db
                                , acInventory = inventory
                                , acMethods = methods
                                , acParameters = M.empty
                                , acTechFlowDB = M.empty
                                , acBioFlowDB = mFlows
                                , acUnitDB = mUnits
                                }
                    liftIO $ ahAnalyze analyzer ctx

getContributingFlows :: Text -> Text -> Text -> Text -> Maybe Int -> Maybe Bool -> AppM ContributingFlowsResult
getContributingFlows dbName processIdText collectionName methodIdText limitParam mExcludeLT =
    withActivityAndMethod dbName collectionName processIdText methodIdText $ \db sharedSolver actProcessId _ method -> do
        dbManager <- asks aeDbManager
        let lim = fromMaybe 20 limitParam
            ltMode = longTermModeFromExclude (fromMaybe False mExcludeLT)
        unitCfg <- liftIO $ getMergedUnitConfig dbManager
        (mFlows, mUnits) <- liftIO $ DM.getMergedFlowMetadata dbManager
        inventory <- applyLongTermMode mFlows ltMode <$> inventoryWithDeps dbName db sharedSolver actProcessId
        tables <- liftIO $ DM.mapMethodToTablesCached dbManager dbName collectionName db method
        let score = loScore (computeLCIAScoreFromTables unitCfg mUnits mFlows inventory tables)
            (rawContribs, unknownUuids) = inventoryContributions unitCfg mUnits mFlows inventory tables
            contribs = sortOn (\(_, _, c) -> negate (abs c)) rawContribs
            topFlows =
                [ FlowContributionEntry
                    { fcoFlowName = bfName f
                    , fcoContribution = c
                    , fcoSharePct = if score /= 0 then c / score * 100 else 0
                    , fcoFlowId = UUID.toText (bfId f)
                    , fcoCategory = bfCompartmentName f
                    , fcoCompartment = bfCompartmentSub f
                    , fcoCfValue = cfVal
                    }
                | (f, cfVal, c) <- take lim contribs
                ]
        liftIO $
            unless (null unknownUuids) $
                reportProgress Warning $
                    "[contributing-flows "
                        <> T.unpack (methodName method)
                        <> "] "
                        <> show (length unknownUuids)
                        <> " inventory flow UUID(s) absent from merged FlowDB. Samples: "
                        <> show (take 3 unknownUuids)
        return
            ContributingFlowsResult
                { cfrMethod = methodName method
                , cfrUnit = methodUnit method
                , cfrTotalScore = score
                , cfrTopFlows = topFlows
                }

getContributingActivities :: Text -> Text -> Text -> Text -> Maybe Int -> Maybe Bool -> AppM ContributingActivitiesResult
getContributingActivities dbName processIdText collectionName methodIdText limitParam mExcludeLT =
    withActivityAndMethod dbName collectionName processIdText methodIdText $ \db sharedSolver actProcessId _ method -> do
        dbManager <- asks aeDbManager
        let lim = fromMaybe 10 limitParam
            ltMode = longTermModeFromExclude (fromMaybe False mExcludeLT)
        requireFullyLinked dbName db
        unitCfg <- liftIO $ getMergedUnitConfig dbManager
        (mFlows, mUnits) <- liftIO $ DM.getMergedFlowMetadata dbManager
        tables <- liftIO $ DM.mapMethodToTablesCached dbManager dbName collectionName db method
        eContribs <-
            liftIO $
                SharedSolver.crossDBProcessContributions
                    unitCfg
                    mUnits
                    mFlows
                    (DM.mkDepSolverLookup dbManager)
                    db
                    dbName
                    sharedSolver
                    actProcessId
                    tables
                    ltMode
        case eContribs of
            Left err -> throwError err422{errBody = BSL.fromStrict $ T.encodeUtf8 err}
            Right contributions -> do
                let score = sum (M.elems contributions)
                    sorted = sortOn (\(_, c) -> negate (abs c)) (M.toList contributions)
                    top = take lim sorted
                rows <- liftIO $ mapM (mkCrossDBContrib dbManager dbName mFlows mUnits score) top
                return
                    ContributingActivitiesResult
                        { carMethod = methodName method
                        , carUnit = methodUnit method
                        , carTotalScore = score
                        , carActivities = rows
                        }

getFlowDetail :: Text -> Text -> AppM FlowDetail
getFlowDetail dbName flowIdText = do
    (db, _) <- requireDatabaseByName dbName
    withValidatedFlow db flowIdText $ \flow -> do
        let fid = flowKindId flow
            unitName' = flowKindUnitName (dbUnits db) flow
            usageCount = Service.getFlowUsageCount db fid
        return $ FlowDetail (apiFlowOfKind flow) unitName' usageCount

getFlowActivities :: Text -> Text -> AppM [ActivitySummary]
getFlowActivities dbName flowIdText = do
    (db, _) <- requireDatabaseByName dbName
    withValidatedFlow db flowIdText $ \flow ->
        return $ Service.getActivitiesUsingFlow db (flowKindId flow)

getMethods :: AppM [MethodSummary]
getMethods = do
    dbManager <- asks aeDbManager
    loadedMethods <- liftIO $ DM.getLoadedMethods dbManager
    return
        [ MethodSummary
            { msmId = methodId m
            , msmName = methodName m
            , msmCategory = methodCategory m
            , msmUnit = methodUnit m
            , msmFactorCount = length (methodFactors m)
            , msmCollection = collName
            }
        | (collName, m) <- loadedMethods
        ]

getMethodDetail :: Text -> AppM MethodDetail
getMethodDetail methodIdText = do
    (_, method) <- loadMethodByUUID methodIdText
    return $
        MethodDetail
            { mdId = methodId method
            , mdName = methodName method
            , mdDescription = methodDescription method
            , mdUnit = methodUnit method
            , mdCategory = methodCategory method
            , mdMethodology = methodMethodology method
            , mdFactorCount = length (methodFactors method)
            }

getMethodFactors :: Text -> AppM [MethodFactorAPI]
getMethodFactors methodIdText = do
    (_, method) <- loadMethodByUUID methodIdText
    return $ map cfToAPI (methodFactors method)

getMethodMapping :: Text -> Text -> AppM MappingStatus
getMethodMapping dbName methodIdText = do
    dbManager <- asks aeDbManager
    (db, _) <- requireDatabaseByName dbName
    (collectionName, method) <- loadMethodByUUID methodIdText
    mappings <- liftIO $ DM.mapMethodToFlowsCached dbManager dbName collectionName db method
    let stats = computeMappingStats mappings
        totalFactors = length mappings
        coverage =
            if totalFactors > 0
                then fromIntegral (totalFactors - msUnmatched stats) / fromIntegral totalFactors * 100
                else 0.0
        unmappedFlows =
            take
                50
                [ UnmappedFlowAPI
                    { ufaFlowRef = mcfFlowRef cf
                    , ufaFlowName = mcfFlowName cf
                    , ufaDirection = case mcfDirection cf of
                        MT.Input -> "Input"
                        MT.Output -> "Output"
                    }
                | (cf, Nothing) <- mappings
                ]
        uniqueDbFlows = S.size $ S.fromList [bfId f | (_, Just (f, _)) <- mappings]
    return
        MappingStatus
            { mstMethodId = methodId method
            , mstMethodName = methodName method
            , mstTotalFactors = msTotal stats
            , mstMappedByUUID = msByUUID stats
            , mstMappedByCAS = msByCAS stats
            , mstMappedByName = msByName stats
            , mstMappedBySynonym = msBySynonym stats
            , mstUnmapped = msUnmatched stats
            , mstCoverage = coverage
            , mstDbBiosphereCount = fromIntegral (dbBiosphereCount db)
            , mstUniqueDbFlowsMatched = uniqueDbFlows
            , mstUnmappedFlows = unmappedFlows
            }

getFlowCFMapping :: Text -> Text -> AppM FlowCFMapping
getFlowCFMapping dbName methodIdText = do
    dbManager <- asks aeDbManager
    (db, _) <- requireDatabaseByName dbName
    (collectionName, method) <- loadMethodByUUID methodIdText
    mappings <- liftIO $ DM.effectiveMethodMappings dbManager dbName collectionName db method
    tables <- liftIO $ DM.mapMethodToTablesCached dbManager dbName collectionName db method
    -- Effective mappings include the synonym fan-out and substance-edge
    -- expansions, appended after the cascade entries; several may target one
    -- flow, so keep the most discriminating per flow (the score tables dedup
    -- the same way via 'preferBetter'). This index only annotates how a direct
    -- match resolved; coverage itself is read from 'tables' below.
    let reverseIndex =
            M.fromListWith
                (\a b -> if strategyPriority (snd a) <= strategyPriority (snd b) then a else b)
                [(bfId f, (cf, strat)) | (cf, Just (f, strat)) <- mappings]
        entries = map (buildFlowEntry db tables reverseIndex) (V.toList (dbBiosphereOrder db))
        matchedCount = length [() | e <- entries, isJust (fceCfValue e)]
    return
        FlowCFMapping
            { fcmMethodName = methodName method
            , fcmMethodUnit = methodUnit method
            , fcmTotalFlows = fromIntegral (dbBiosphereCount db)
            , fcmMatchedFlows = matchedCount
            , fcmFlows = entries
            }

getCharacterization :: Text -> Text -> Maybe Text -> Maybe Int -> AppM CharacterizationResult
getCharacterization dbName methodIdText flowFilter limitParam = do
    dbManager <- asks aeDbManager
    (db, _) <- requireDatabaseByName dbName
    (collectionName, method) <- loadMethodByUUID methodIdText
    let lim = fromMaybe 50 limitParam
        queryLower = fmap T.toLower flowFilter
    mappings <- liftIO $ DM.effectiveMethodMappings dbManager dbName collectionName db method
    let matched =
            [ (cf, f, strat)
            | (cf, Just (f, strat)) <- mappings
            , matchesQuery queryLower (mcfFlowName cf) (bfName f)
            ]
        sorted = sortOn (\(cf, _, _) -> negate (abs (mcfValue cf))) matched
        top = take lim sorted
        mkEntry (cf, f, strat) =
            CharacterizationEntry
                { cheMethodFlowName = mcfFlowName cf
                , cheCfValue = mcfValue cf
                , cheCfUnit = mcfUnit cf
                , cheDirection = case mcfDirection cf of
                    MT.Input -> "Input"
                    MT.Output -> "Output"
                , cheDbFlowName = bfName f
                , cheFlowId = UUID.toText (bfId f)
                , cheFlowUnit = getUnitNameForBioFlow (dbUnits db) f
                , cheCategory = bfCompartmentName f
                , cheCompartment = bfCompartmentSub f
                , cheMatchStrategy = strategyToText strat
                }
    return
        CharacterizationResult
            { chrMethod = methodName method
            , chrUnit = methodUnit method
            , chrMatches = length matched
            , chrShown = length top
            , chrFactors = map mkEntry top
            }

getMethodCollections :: AppM MethodCollectionListResponse
getMethodCollections = do
    dbManager <- asks aeDbManager
    statuses <- liftIO $ DM.listMethodCollections dbManager
    return $
        MethodCollectionListResponse
            [ MethodCollectionStatusAPI
                { mcaName = mcsName s
                , mcaDisplayName = mcsDisplayName s
                , mcaDescription = mcsDescription s
                , mcaStatus = case mcsStatus s of
                    DM.Loaded -> "loaded"
                    _ -> "unloaded"
                , mcaIsUploaded = mcsIsUploaded s
                , mcaPath = mcsPath s
                , mcaMethodCount = mcsMethodCount s
                , mcaFormat = Just (mcsFormat s)
                }
            | s <- statuses
            ]

loadMethodCollectionHandler :: Text -> AppM ActivateResponse
loadMethodCollectionHandler name = do
    dbManager <- asks aeDbManager
    simpleAction (DM.loadMethodCollection dbManager name) ("Loaded method: " <> name)

unloadMethodCollectionHandler :: Text -> AppM ActivateResponse
unloadMethodCollectionHandler name = do
    dbManager <- asks aeDbManager
    simpleAction (DM.unloadMethodCollection dbManager name) ("Unloaded method: " <> name)

searchFlows :: Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> AppM (SearchResults FlowSearchResult)
searchFlows dbName queryParam langParam limitParam offsetParam sortParam orderParam = do
    (db, _) <- requireDatabaseByName dbName
    case queryParam of
        Nothing -> return (SearchResults [] 0 0 50 False 0.0)
        Just query -> do
            let ff =
                    Service.FlowFilter
                        { Service.ffQuery = query
                        , Service.ffLang = langParam
                        , Service.ffLimit = limitParam
                        , Service.ffOffset = offsetParam
                        , Service.ffSort = sortParam
                        , Service.ffOrder = orderParam
                        }
            searchFlowsInternal db ff

searchActivitiesWithCount :: Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Bool -> Maybe Text -> [Text] -> [Text] -> [Text] -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> AppM (SearchResults ActivitySummary)
searchActivitiesWithCount dbName nameParam geoParam productParam exactParam presetParam classSystems classValues classModes limitParam offsetParam sortParam orderParam = do
    presets <- asks aeClassificationPresets
    (db, _) <- requireDatabaseByName dbName
    let exactMatch = fromMaybe False exactParam
        sf =
            Service.SearchFilter
                { Service.sfCore =
                    Service.ActivityFilterCore
                        { Service.afcName = nameParam
                        , Service.afcLocation = geoParam
                        , Service.afcProduct = productParam
                        , Service.afcClassifications = mergeClassFilters presets presetParam classSystems classValues classModes
                        , Service.afcLimit = limitParam
                        , Service.afcOffset = offsetParam
                        , Service.afcSort = sortParam
                        , Service.afcOrder = orderParam
                        }
                , Service.sfExactMatch = exactMatch
                }
    result <- liftIO $ Service.searchActivities db sf
    case result of
        Left err -> throwError err500{errBody = BSL.fromStrict $ T.encodeUtf8 $ T.pack $ show err}
        Right jsonValue -> case fromJSON jsonValue of
            Success searchResults -> return searchResults
            Error parseErr -> throwError err500{errBody = BSL.fromStrict $ T.encodeUtf8 $ T.pack parseErr}

getClassifications :: Text -> AppM [ClassificationSystem]
getClassifications dbName = do
    (db, _) <- requireDatabaseByName dbName
    return $ Service.getClassifications db

postImpactsBatch :: Text -> Text -> Maybe Int -> BatchImpactsRequest -> AppM BatchImpactsResponse
postImpactsBatch = batchImpactsH

-- ---------------------------------------------------------------------------
-- Servant server
-- ---------------------------------------------------------------------------

lcaServer :: AppEnv -> Server LCAAPI
lcaServer env = hoistServer lcaAPI (runApp env) handlers
  where
    handlers =
        getActivityInfo
            :<|> getActivityFlows
            :<|> getActivityInputs
            :<|> getActivityOutputs
            :<|> getActivityReferenceProduct
            :<|> getActivityTree
            :<|> getActivityInventory
            :<|> getActivityGraph
            :<|> getActivitySupplyChain
            :<|> getActivityAggregate
            :<|> getActivityLCIABatch
            :<|> postActivityLCIABatch
            :<|> getActivityLCIA
            :<|> postActivityLCIA
            :<|> postActivitySensitivity
            :<|> postActivityInventory
            :<|> postActivitySupplyChain
            :<|> getActivityConsumers
            :<|> getActivityPathTo
            :<|> getActivityAnalyze
            :<|> getContributingFlows
            :<|> getContributingActivities
            :<|> getFlowDetail
            :<|> getFlowActivities
            :<|> getMethods
            :<|> getMethodDetail
            :<|> getMethodFactors
            :<|> getMethodMapping
            :<|> getFlowCFMapping
            :<|> getCharacterization
            :<|> searchFlows
            :<|> searchActivitiesWithCount
            :<|> getClassifications
            :<|> postImpactsBatch
            :<|> DBHandlers.getDatabases
            :<|> DBHandlers.loadDatabaseHandler
            :<|> DBHandlers.unloadDatabaseHandler
            :<|> DBHandlers.relinkDatabaseHandler
            :<|> DBHandlers.copyDatabaseHandler
            :<|> DBHandlers.deleteDatabaseHandler
            :<|> DBHandlers.deleteActivitiesHandler
            :<|> DBHandlers.exportDatabaseHandler
            :<|> DBHandlers.uploadDatabaseHandler
            :<|> DBHandlers.getDatabaseSetupHandler
            :<|> DBHandlers.addDependencyHandler
            :<|> DBHandlers.removeDependencyHandler
            :<|> DBHandlers.setDataPathHandler
            :<|> DBHandlers.finalizeDatabaseHandler
            :<|> getMethodCollections
            :<|> loadMethodCollectionHandler
            :<|> unloadMethodCollectionHandler
            :<|> DBHandlers.deleteMethodHandler
            :<|> DBHandlers.uploadMethodHandler
            :<|> DBHandlers.listRefData DBHandlers.FlowSynonyms
            :<|> DBHandlers.loadRefData DBHandlers.FlowSynonyms
            :<|> DBHandlers.unloadRefData DBHandlers.FlowSynonyms
            :<|> DBHandlers.deleteRefData DBHandlers.FlowSynonyms
            :<|> DBHandlers.uploadRefData DBHandlers.FlowSynonyms
            :<|> DBHandlers.getFlowSynonymGroupsHandler
            :<|> DBHandlers.downloadRefDataHandler DBHandlers.FlowSynonyms
            :<|> DBHandlers.listRefData DBHandlers.CompartmentMappings
            :<|> DBHandlers.loadRefData DBHandlers.CompartmentMappings
            :<|> DBHandlers.unloadRefData DBHandlers.CompartmentMappings
            :<|> DBHandlers.deleteRefData DBHandlers.CompartmentMappings
            :<|> DBHandlers.uploadRefData DBHandlers.CompartmentMappings
            :<|> DBHandlers.listRefData DBHandlers.UnitDefs
            :<|> DBHandlers.loadRefData DBHandlers.UnitDefs
            :<|> DBHandlers.unloadRefData DBHandlers.UnitDefs
            :<|> DBHandlers.deleteRefData DBHandlers.UnitDefs
            :<|> DBHandlers.uploadRefData DBHandlers.UnitDefs
            :<|> getLogsHandler
            :<|> postAuth
            :<|> getVersion
            :<|> getHosting
            :<|> getStats
            :<|> getClassificationPresets
            :<|> getOpenApiSpec

{- | Build the scoring input map (impact method name → raw score) from LCIA
results. Keyed by method NAME, which is unique per collection — not by
'lrCategory', which for ILCD methods is the coarse damage class (e.g. all four
"Climate change-*" methods share category "Climate change"; the three freshwater
ecotoxicity methods share "Aquatic eco-toxicity"). Keying by category collapses
such methods and breaks single-score resolution ("Unknown variable: …"). For
SimaPro-adapted methods name == category, so their scoring is unchanged.
-}
rawScoreMapByName :: [LCIAResult] -> M.Map Text Double
rawScoreMapByName results = M.fromList [(lrMethodName r, lrScore r) | r <- results]

{- | Evaluate every scoring set against the raw impact score map.
Returns (setName → scoreName → value, setName → varName → ScoringIndicator).
Scoring sets that fail to evaluate are logged as warnings and omitted.
Values are pre-multiplied by each set's displayMultiplier (default 1.0).
-}
computeAllScoringSets ::
    [ScoringSet] ->
    M.Map Text Double ->
    IO (M.Map Text (M.Map Text Double), M.Map Text (M.Map Text ScoringIndicator))
computeAllScoringSets scoringSets rawScoreMap = do
    evaluations <- forM scoringSets $ \ss ->
        case computeFormulaScores ss rawScoreMap of
            Right eval -> pure $ Just (ss, eval)
            Left err -> do
                reportProgress Warning $
                    "  Scoring set '"
                        <> T.unpack (ssName ss)
                        <> "' failed: "
                        <> err
                pure Nothing
    let ok = [(ss, e) | Just (ss, e) <- evaluations]
        scores = M.fromList [(ssName ss, seScores e) | (ss, e) <- ok]
        indicators = M.fromList [(ssName ss, toIndicators ss e) | (ss, e) <- ok]
    pure (scores, indicators)
  where
    -- Only emit rows for variables that actually contribute to a score formula.
    -- Intermediate helpers (consumed by `computed` but not referenced in any
    -- `scores.*` formula) are hidden from the breakdown.
    toIndicators ss e =
        let displayed = S.fromList (concatMap (Expr.collectIdentifiers '.') (M.elems (ssScores ss)))
            names = ssLabels ss <> ssVariables ss
         in M.mapWithKey
                ( \var val ->
                    ScoringIndicator
                        { siCategory = M.findWithDefault var var names
                        , siValue = val
                        }
                )
                (M.filterWithKey (\var _ -> S.member var displayed) (seNwEnv e))

-- | Helper function to apply pagination to search results
paginateResults :: [a] -> Maybe Int -> Maybe Int -> IO (SearchResults a)
paginateResults results limitParam offsetParam = do
    startTime <- getCurrentTime
    let totalCount = length results
        limit = fromMaybe totalCount limitParam -- Default: return all results
        offset = fromMaybe 0 offsetParam -- Default offset: 0
        paginatedResults = take limit $ drop offset results
        hasMore = offset + length paginatedResults < totalCount
    endTime <- getCurrentTime
    let searchTimeMs = realToFrac (diffUTCTime endTime startTime) * 1000 :: Double
    return $ SearchResults paginatedResults totalCount offset limit hasMore searchTimeMs

{- | Internal helper for flow search with optional language filtering.
The 'ffQuery' is always present (callers short-circuit on the no-query
case); language filtering is not yet implemented.
-}
searchFlowsInternal :: Database -> Service.FlowFilter -> AppM (SearchResults FlowSearchResult)
searchFlowsInternal db Service.FlowFilter{Service.ffQuery = query, Service.ffLimit = limitParam, Service.ffOffset = offsetParam, Service.ffSort = sortParam, Service.ffOrder = orderParam} = do
    -- Language filtering not yet implemented, search all synonyms
    let flows = findFlowsBySynonym db query
        unitOf = flowKindUnitName (dbUnits db)
        allResults = [FlowSearchResult (flowKindId flow) (flowKindName flow) (flowKindCategory flow) (unitOf flow) (M.map S.toList (flowKindSynonyms flow)) | flow <- flows]
        isDesc = orderParam == Just "desc"
        fsCmp = case sortParam of
            Just "category" -> \a b -> compare (fsrCategory a) (fsrCategory b)
            Just "unit" -> \a b -> compare (fsrUnitName a) (fsrUnitName b)
            _ -> \a b -> compare (fsrName a) (fsrName b)
        sorted = sortBy (if isDesc then flip fsCmp else fsCmp) allResults
    liftIO $ paginateResults sorted limitParam offsetParam

-- | Proxy for the API
lcaAPI :: Proxy LCAAPI
lcaAPI = Proxy
