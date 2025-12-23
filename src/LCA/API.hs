{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module LCA.API where

import qualified LCA.Matrix
import LCA.Matrix (Inventory)
import LCA.Matrix.SharedSolver (SharedSolver)
import LCA.LCIA (computeLCIAScore)
import LCA.Method.Mapping (mapMethodFlows, MatchStrategy(..), MappingStats(..), computeMappingStats)
import LCA.Method.Parser (parseMethodFile)
import LCA.Method.Types (Method(..), MethodCF(..), FlowDirection(..))
import LCA.SynonymDB (SynonymDB, emptySynonymDB)
import LCA.DatabaseManager (DatabaseManager(..), LoadedDatabase(..), DatabaseStatus(..), getCurrentDatabase, getCurrentDatabaseName, listDatabases, activateDatabase)
import qualified LCA.Config
import LCA.Query
import qualified LCA.Service
import LCA.Tree (buildLoopAwareTree)
import LCA.Types
import System.Directory (listDirectory, doesFileExist)
import System.FilePath ((</>), takeExtension)
import LCA.Types.API (ActivityForAPI (..), ActivityInfo (..), ActivityLinks (..), ActivityMetadata (..), ActivityStats (..), ActivitySummary (..), ExchangeDetail (..), ExchangeWithUnit (..), ExportNode (..), FlowDetail (..), FlowInfo (..), FlowRole (..), FlowSearchResult (..), FlowSummary (..), GraphExport (..), InventoryExport (..), InventoryFlowDetail (..), InventoryMetadata (..), InventoryStatistics (..), LCIARequest (..), LCIAResult (..), MappingStatus (..), MethodDetail (..), MethodFactorAPI (..), MethodSummary (..), NodeType (..), SearchResults (..), TreeEdge (..), TreeExport (..), TreeMetadata (..), UnmappedFlowAPI (..), DatabaseListResponse(..), DatabaseStatusAPI(..), ActivateResponse(..))
import Data.Aeson
import Data.Aeson.Types (Result (..), fromJSON)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import qualified Data.UUID as UUID
import GHC.Generics
import Servant
import Control.Monad.IO.Class (liftIO)

-- | API type definition - RESTful design with focused endpoints
type LCAAPI =
    "api"
        :> "v1"
        :> ( "activity" :> Capture "processId" Text :> Get '[JSON] ActivityInfo
                :<|> "activity" :> Capture "processId" Text :> "flows" :> Get '[JSON] [FlowSummary]
                :<|> "activity" :> Capture "processId" Text :> "inputs" :> Get '[JSON] [ExchangeDetail]
                :<|> "activity" :> Capture "processId" Text :> "outputs" :> Get '[JSON] [ExchangeDetail]
                :<|> "activity" :> Capture "processId" Text :> "reference-product" :> Get '[JSON] FlowDetail
                :<|> "activity" :> Capture "processId" Text :> "tree" :> Get '[JSON] TreeExport
                :<|> "activity" :> Capture "processId" Text :> "inventory" :> Get '[JSON] InventoryExport
                :<|> "activity" :> Capture "processId" Text :> "graph" :> QueryParam "cutoff" Double :> Get '[JSON] GraphExport
                :<|> "activity" :> Capture "processId" Text :> "lcia" :> Capture "methodId" Text :> Get '[JSON] LCIAResult
                :<|> "flow" :> Capture "flowId" Text :> Get '[JSON] FlowDetail
                :<|> "flow" :> Capture "flowId" Text :> "activities" :> Get '[JSON] [ActivitySummary]
                :<|> "methods" :> Get '[JSON] [MethodSummary]
                :<|> "method" :> Capture "methodId" Text :> Get '[JSON] MethodDetail
                :<|> "method" :> Capture "methodId" Text :> "factors" :> Get '[JSON] [MethodFactorAPI]
                :<|> "method" :> Capture "methodId" Text :> "mapping" :> Get '[JSON] MappingStatus
                :<|> "search" :> "flows" :> QueryParam "q" Text :> QueryParam "lang" Text :> QueryParam "limit" Int :> QueryParam "offset" Int :> Get '[JSON] (SearchResults FlowSearchResult)
                :<|> "search" :> "activities" :> QueryParam "name" Text :> QueryParam "geo" Text :> QueryParam "product" Text :> QueryParam "limit" Int :> QueryParam "offset" Int :> Get '[JSON] (SearchResults ActivitySummary)
                :<|> "lcia" :> Capture "processId" Text :> ReqBody '[JSON] LCIARequest :> Post '[JSON] Value
                -- Database management endpoints
                :<|> "databases" :> Get '[JSON] DatabaseListResponse
                :<|> "databases" :> Capture "dbName" Text :> "activate" :> Post '[JSON] ActivateResponse
                :<|> "databases" :> "current" :> Get '[JSON] (Maybe DatabaseStatusAPI)
           )

-- | Get current database and solver from DatabaseManager
-- Throws 500 error if no database is loaded
requireCurrentDatabase :: DatabaseManager -> Handler (Database, SharedSolver)
requireCurrentDatabase dbManager = do
    maybeLoaded <- liftIO $ getCurrentDatabase dbManager
    case maybeLoaded of
        Nothing -> throwError err500{errBody = "No database loaded"}
        Just loaded -> return (ldDatabase loaded, ldSharedSolver loaded)

-- | Helper function to validate ProcessId and lookup activity
withValidatedActivity :: Database -> Text -> (Activity -> Handler a) -> Handler a
withValidatedActivity db processId action = do
    case LCA.Service.resolveActivityByProcessId db processId of
        Left (LCA.Service.InvalidProcessId errorMsg) -> throwError err400{errBody = BSL.fromStrict $ T.encodeUtf8 errorMsg}
        Left (LCA.Service.ActivityNotFound _) -> throwError err404{errBody = "Activity not found"}
        Left _ -> throwError err400{errBody = "Invalid request"}
        Right activity -> action activity

-- | Helper function to validate UUID and lookup flow
withValidatedFlow :: Database -> Text -> (Flow -> Handler a) -> Handler a
withValidatedFlow db uuid action = do
    case LCA.Service.validateUUID uuid of
        Left (LCA.Service.InvalidUUID errorMsg) -> throwError err400{errBody = BSL.fromStrict $ T.encodeUtf8 errorMsg}
        Left _ -> throwError err400{errBody = "Invalid request"}
        Right validUuidText ->
            case UUID.fromText validUuidText of
                Nothing -> throwError err400{errBody = "Invalid UUID format"}
                Just validUuid ->
                    case M.lookup validUuid (dbFlows db) of
                        Nothing -> throwError err404{errBody = "Flow not found"}
                        Just flow -> action flow

-- | API server implementation
-- DatabaseManager is used to dynamically fetch current database on each request
lcaServer :: DatabaseManager -> Int -> Maybe FilePath -> Server LCAAPI
lcaServer dbManager maxTreeDepth methodsDir =
    getActivityInfo
        :<|> getActivityFlows
        :<|> getActivityInputs
        :<|> getActivityOutputs
        :<|> getActivityReferenceProduct
        :<|> getActivityTree
        :<|> getActivityInventory
        :<|> getActivityGraph
        :<|> getActivityLCIA
        :<|> getFlowDetail
        :<|> getFlowActivities
        :<|> getMethods
        :<|> getMethodDetail
        :<|> getMethodFactors
        :<|> getMethodMapping
        :<|> searchFlows
        :<|> searchActivitiesWithCount
        :<|> postLCIA
        :<|> getDatabases
        :<|> activateDatabaseHandler
        :<|> getCurrentDatabaseHandler
  where
    -- Core activity endpoint - streamlined data
    getActivityInfo :: Text -> Handler ActivityInfo
    getActivityInfo processId = do
        (db, _) <- requireCurrentDatabase dbManager
        case LCA.Service.getActivityInfo db processId of
            Left (LCA.Service.ActivityNotFound _) -> throwError err404{errBody = "Activity not found"}
            Left (LCA.Service.InvalidProcessId _) -> throwError err400{errBody = "Invalid ProcessId format"}
            Right result -> case fromJSON result of
                Success activityInfo -> return activityInfo
                Error err -> throwError err500{errBody = BSL.fromStrict $ T.encodeUtf8 $ T.pack err}

    -- Activity flows sub-resource
    getActivityFlows :: Text -> Handler [FlowSummary]
    getActivityFlows processId = do
        (db, _) <- requireCurrentDatabase dbManager
        withValidatedActivity db processId $ \activity ->
            return $ LCA.Service.getActivityFlowSummaries db activity

    -- Activity inputs sub-resource
    getActivityInputs :: Text -> Handler [ExchangeDetail]
    getActivityInputs processId = do
        (db, _) <- requireCurrentDatabase dbManager
        withValidatedActivity db processId $ \activity ->
            return $ LCA.Service.getActivityInputDetails db activity

    -- Activity outputs sub-resource
    getActivityOutputs :: Text -> Handler [ExchangeDetail]
    getActivityOutputs processId = do
        (db, _) <- requireCurrentDatabase dbManager
        withValidatedActivity db processId $ \activity ->
            return $ LCA.Service.getActivityOutputDetails db activity

    -- Activity reference product sub-resource
    getActivityReferenceProduct :: Text -> Handler FlowDetail
    getActivityReferenceProduct processId = do
        (db, _) <- requireCurrentDatabase dbManager
        withValidatedActivity db processId $ \activity -> do
            case LCA.Service.getActivityReferenceProductDetail db activity of
                Nothing -> throwError err404{errBody = "No reference product found"}
                Just refProduct -> return refProduct

    -- Activity tree export for visualization (configurable depth)
    getActivityTree :: Text -> Handler TreeExport
    getActivityTree processId = do
        (db, _) <- requireCurrentDatabase dbManager
        withValidatedActivity db processId $ \activity -> do
            -- Use CLI --tree-depth option for configurable depth
            -- Default depth limit prevents DOS attacks via deep tree requests
            -- Extract activity UUID from processId (format: activityUUID_productUUID)
            let activityUuidText = case T.splitOn "_" processId of
                    (uuid:_) -> uuid
                    [] -> processId  -- Fallback
            case UUID.fromText activityUuidText of
                Nothing -> throwError err400{errBody = "Invalid activity UUID format"}
                Just activityUuid -> do
                    let loopAwareTree = buildLoopAwareTree db activityUuid maxTreeDepth
                    return $ LCA.Service.convertToTreeExport db processId maxTreeDepth loopAwareTree

    -- Activity inventory calculation (full supply chain LCI)
    getActivityInventory :: Text -> Handler InventoryExport
    getActivityInventory processId = do
        (db, sharedSolver) <- requireCurrentDatabase dbManager
        result <- liftIO $ LCA.Service.getActivityInventoryWithSharedSolver sharedSolver db processId
        case result of
            Left (LCA.Service.ActivityNotFound _) -> throwError err404{errBody = "Activity not found"}
            Left (LCA.Service.InvalidProcessId _) -> throwError err400{errBody = "Invalid ProcessId format"}
            Left _ -> throwError err500{errBody = "Internal server error"}
            Right inventoryExport -> return inventoryExport

    -- Activity graph endpoint for network visualization
    getActivityGraph :: Text -> Maybe Double -> Handler GraphExport
    getActivityGraph processId maybeCutoff = do
        (db, sharedSolver) <- requireCurrentDatabase dbManager
        let cutoffPercent = fromMaybe 1.0 maybeCutoff  -- Default to 1% cutoff
        result <- liftIO $ LCA.Service.buildActivityGraph db sharedSolver processId cutoffPercent
        case result of
            Left (LCA.Service.ActivityNotFound _) -> throwError err404{errBody = "Activity not found"}
            Left (LCA.Service.InvalidProcessId _) -> throwError err400{errBody = "Invalid ProcessId format"}
            Left (LCA.Service.MatrixError msg) -> throwError err500{errBody = BSL.fromStrict $ T.encodeUtf8 msg}
            Left _ -> throwError err500{errBody = "Internal server error"}
            Right graphExport -> return graphExport

    -- Activity LCIA endpoint
    getActivityLCIA :: Text -> Text -> Handler LCIAResult
    getActivityLCIA processIdText methodIdText = do
        (db, _) <- requireCurrentDatabase dbManager
        -- Load the method
        method <- loadMethodByUUID methodIdText

        -- Resolve activity and ProcessId from text
        case LCA.Service.resolveActivityAndProcessId db processIdText of
            Left (LCA.Service.ActivityNotFound _) -> throwError err404{errBody = "Activity not found"}
            Left (LCA.Service.InvalidProcessId _) -> throwError err400{errBody = "Invalid ProcessId format"}
            Left err -> throwError err500{errBody = BSL.fromStrict $ T.encodeUtf8 $ T.pack $ show err}
            Right (actProcessId, _activity) -> do
                -- Compute inventory using matrix solver
                let inventory = LCA.Matrix.computeInventoryMatrix db actProcessId

                -- Get synonym DB and flow indexes
                let synDB = fromMaybe emptySynonymDB (dbSynonymDB db)
                    flowsByUUID = dbFlows db
                    flowsByName = dbFlowsByName db

                -- Map method flows to database flows
                let mappings = mapMethodFlows synDB flowsByUUID flowsByName method
                    stats = computeMappingStats mappings

                -- Compute LCIA score: sum of (inventory quantity * CF value) for mapped flows
                let score = computeLCIAScore inventory mappings

                return $ LCIAResult
                    { lrMethodId = methodId method
                    , lrMethodName = methodName method
                    , lrCategory = methodCategory method
                    , lrScore = score
                    , lrUnit = methodUnit method
                    , lrMappedFlows = msTotal stats - msUnmatched stats
                    , lrUnmappedFlows = msUnmatched stats
                    }

    -- Flow detail endpoint
    getFlowDetail :: Text -> Handler FlowDetail
    getFlowDetail flowIdText = do
        (db, _) <- requireCurrentDatabase dbManager
        withValidatedFlow db flowIdText $ \flow -> do
            let usageCount = LCA.Service.getFlowUsageCount db (flowId flow)
            let unitName = getUnitNameForFlow (dbUnits db) flow
            return $ FlowDetail flow unitName usageCount

    -- Activities using a specific flow
    getFlowActivities :: Text -> Handler [ActivitySummary]
    getFlowActivities flowIdText = do
        (db, _) <- requireCurrentDatabase dbManager
        withValidatedFlow db flowIdText $ \flow ->
            return $ LCA.Service.getActivitiesUsingFlow db (flowId flow)

    -- List all available methods
    getMethods :: Handler [MethodSummary]
    getMethods = case methodsDir of
        Nothing -> return []
        Just dir -> do
            files <- liftIO $ listDirectory dir
            let xmlFiles = filter (\f -> takeExtension f == ".xml") files
            methods <- liftIO $ mapM (parseAndSummarize dir) xmlFiles
            return $ [m | Just m <- methods]

    -- Get method details
    getMethodDetail :: Text -> Handler MethodDetail
    getMethodDetail methodIdText = do
        method <- loadMethodByUUID methodIdText
        return $ MethodDetail
            { mdId = methodId method
            , mdName = methodName method
            , mdDescription = methodDescription method
            , mdUnit = methodUnit method
            , mdCategory = methodCategory method
            , mdMethodology = methodMethodology method
            , mdFactorCount = length (methodFactors method)
            }

    -- Get method characterization factors
    getMethodFactors :: Text -> Handler [MethodFactorAPI]
    getMethodFactors methodIdText = do
        method <- loadMethodByUUID methodIdText
        return $ map cfToAPI (methodFactors method)

    -- Get method flow mapping status
    getMethodMapping :: Text -> Handler MappingStatus
    getMethodMapping methodIdText = do
        (db, _) <- requireCurrentDatabase dbManager
        method <- loadMethodByUUID methodIdText
        -- Get SynonymDB and flow name index from database
        let synDB = fromMaybe emptySynonymDB (dbSynonymDB db)
            flowsByUUID = dbFlows db
            flowsByName = dbFlowsByName db
        -- Run the mapping
        let mappings = mapMethodFlows synDB flowsByUUID flowsByName method
            stats = computeMappingStats mappings
            totalFactors = length mappings
            coverage = if totalFactors > 0
                       then fromIntegral (totalFactors - msUnmatched stats) / fromIntegral totalFactors * 100
                       else 0.0
            -- Get unmapped flows (limit to first 50 for API response)
            unmappedFlows = take 50 [ UnmappedFlowAPI
                { ufaFlowRef = mcfFlowRef cf
                , ufaFlowName = mcfFlowName cf
                , ufaDirection = case mcfDirection cf of
                    Input -> "Input"
                    Output -> "Output"
                }
                | (cf, Nothing) <- mappings
                ]
        return MappingStatus
            { mstMethodId = methodId method
            , mstMethodName = methodName method
            , mstTotalFactors = msTotal stats
            , mstMappedByUUID = msByUUID stats
            , mstMappedByName = msByName stats
            , mstMappedBySynonym = msBySynonym stats
            , mstUnmapped = msUnmatched stats
            , mstCoverage = coverage
            , mstUnmappedFlows = unmappedFlows
            }

    -- Helper to parse a method file and create a summary
    parseAndSummarize :: FilePath -> FilePath -> IO (Maybe MethodSummary)
    parseAndSummarize dir fileName = do
        result <- parseMethodFile (dir </> fileName)
        case result of
            Left _ -> return Nothing
            Right m -> return $ Just $ MethodSummary
                { msmId = methodId m
                , msmName = methodName m
                , msmCategory = methodCategory m
                , msmUnit = methodUnit m
                , msmFactorCount = length (methodFactors m)
                }

    -- Helper to load a method by UUID
    loadMethodByUUID :: Text -> Handler Method
    loadMethodByUUID uuidText = case methodsDir of
        Nothing -> throwError err404{errBody = "No methods directory configured"}
        Just dir -> do
            -- Try to find the file matching the UUID
            files <- liftIO $ listDirectory dir
            let targetFile = T.unpack uuidText ++ ".xml"
            if targetFile `elem` files
                then do
                    result <- liftIO $ parseMethodFile (dir </> targetFile)
                    case result of
                        Left err -> throwError err500{errBody = BSL.fromStrict $ T.encodeUtf8 $ T.pack err}
                        Right m -> return m
                else throwError err404{errBody = "Method not found"}

    -- Helper to convert MethodCF to API type
    cfToAPI :: MethodCF -> MethodFactorAPI
    cfToAPI cf = MethodFactorAPI
        { mfaFlowRef = mcfFlowRef cf
        , mfaFlowName = mcfFlowName cf
        , mfaDirection = case mcfDirection cf of
            Input -> "Input"
            Output -> "Output"
        , mfaValue = mcfValue cf
        }

    -- Search flows by name or synonym with optional language filtering and pagination
    searchFlows :: Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Handler (SearchResults FlowSearchResult)
    searchFlows queryParam langParam limitParam offsetParam = do
        (db, _) <- requireCurrentDatabase dbManager
        searchFlowsInternal db queryParam langParam limitParam offsetParam

    -- Search activities by specific fields with pagination and count
    searchActivitiesWithCount :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Handler (SearchResults ActivitySummary)
    searchActivitiesWithCount nameParam geoParam productParam limitParam offsetParam = do
        (db, _) <- requireCurrentDatabase dbManager
        -- Use Service.searchActivities which paginates BEFORE calling findProcessIdForActivity
        -- This avoids O(n*m) performance issue where n=results, m=total activities
        result <- liftIO $ LCA.Service.searchActivities db nameParam geoParam productParam limitParam offsetParam
        case result of
            Left err -> throwError err500{errBody = BSL.fromStrict $ T.encodeUtf8 $ T.pack $ show err}
            Right jsonValue -> case fromJSON jsonValue of
                Success searchResults -> return searchResults
                Error parseErr -> throwError err500{errBody = BSL.fromStrict $ T.encodeUtf8 $ T.pack parseErr}

    -- LCIA computation
    postLCIA :: Text -> LCIARequest -> Handler Value
    postLCIA processId lciaReq = do
        (db, _) <- requireCurrentDatabase dbManager
        withValidatedActivity db processId $ \_ -> do
            -- This would implement LCIA computation with the provided method
            -- For now, return a placeholder
            return $ object ["status" .= ("not_implemented" :: Text), "processId" .= processId, "method" .= lciaMethod lciaReq]

    -- Database management handlers
    getDatabases :: Handler DatabaseListResponse
    getDatabases = do
        dbStatuses <- liftIO $ listDatabases dbManager
        currentName <- liftIO $ getCurrentDatabaseName dbManager
        let statusList = map convertDbStatus dbStatuses
        return $ DatabaseListResponse statusList currentName

    activateDatabaseHandler :: Text -> Handler ActivateResponse
    activateDatabaseHandler dbName = do
        result <- liftIO $ activateDatabase dbManager dbName
        case result of
            Left err -> return $ ActivateResponse False err Nothing
            Right loadedDb -> do
                let config = ldConfig loadedDb
                    status = DatabaseStatusAPI
                        { dsaName = LCA.Config.dcName config
                        , dsaDisplayName = LCA.Config.dcDisplayName config
                        , dsaDescription = LCA.Config.dcDescription config
                        , dsaActive = LCA.Config.dcActive config
                        , dsaLoaded = True
                        , dsaCached = True
                        , dsaPath = T.pack (LCA.Config.dcPath config)
                        }
                return $ ActivateResponse True ("Activated database: " <> LCA.Config.dcDisplayName config) (Just status)

    getCurrentDatabaseHandler :: Handler (Maybe DatabaseStatusAPI)
    getCurrentDatabaseHandler = do
        maybeLoaded <- liftIO $ getCurrentDatabase dbManager
        case maybeLoaded of
            Nothing -> return Nothing
            Just loaded -> return $ Just $ convertLoadedDbToStatus loaded

    -- Helper to convert DatabaseManager.DatabaseStatus to API.DatabaseStatusAPI
    convertDbStatus :: DatabaseStatus -> DatabaseStatusAPI
    convertDbStatus ds = DatabaseStatusAPI
        { dsaName = dsName ds
        , dsaDisplayName = dsDisplayName ds
        , dsaDescription = dsDescription ds
        , dsaActive = dsActive ds
        , dsaLoaded = dsLoaded ds
        , dsaCached = dsCached ds
        , dsaPath = dsPath ds
        }

    -- Helper to convert LoadedDatabase to DatabaseStatusAPI
    convertLoadedDbToStatus :: LoadedDatabase -> DatabaseStatusAPI
    convertLoadedDbToStatus loaded =
        let config = ldConfig loaded
        in DatabaseStatusAPI
            { dsaName = LCA.Config.dcName config
            , dsaDisplayName = LCA.Config.dcDisplayName config
            , dsaDescription = LCA.Config.dcDescription config
            , dsaActive = LCA.Config.dcActive config
            , dsaLoaded = True
            , dsaCached = True
            , dsaPath = T.pack (LCA.Config.dcPath config)
            }

-- | Helper function to apply pagination to search results
paginateResults :: [a] -> Maybe Int -> Maybe Int -> IO (SearchResults a)
paginateResults results limitParam offsetParam = do
    startTime <- getCurrentTime
    let totalCount = length results
        limit = min 1000 (maybe 50 id limitParam) -- Default limit: 50, max: 1000
        offset = maybe 0 id offsetParam -- Default offset: 0
        paginatedResults = take limit $ drop offset results
        hasMore = offset + length paginatedResults < totalCount
    endTime <- getCurrentTime
    let searchTimeMs = realToFrac (diffUTCTime endTime startTime) * 1000 :: Double
    return $ SearchResults paginatedResults totalCount offset limit hasMore searchTimeMs

-- | Internal helper for flow search with optional language filtering
searchFlowsInternal :: Database -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Handler (SearchResults FlowSearchResult)
searchFlowsInternal _ Nothing _ _ _ = return $ SearchResults [] 0 0 50 False 0.0
searchFlowsInternal db (Just query) _langParam limitParam offsetParam = do
    -- Language filtering not yet implemented, search all synonyms
    let flows = findFlowsBySynonym db query
        flowSearchResults = [FlowSearchResult (flowId flow) (flowName flow) (flowCategory flow) (getUnitNameForFlow (dbUnits db) flow) (M.map S.toList (flowSynonyms flow)) | flow <- flows]
    liftIO $ paginateResults flowSearchResults limitParam offsetParam

-- | Proxy for the API
lcaAPI :: Proxy LCAAPI
lcaAPI = Proxy
