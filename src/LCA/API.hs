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
import LCA.DatabaseManager (DatabaseManager(..), LoadedDatabase(..), DatabaseSetupInfo(..), getDatabase)
import LCA.Upload (DatabaseFormat(..))
import qualified LCA.API.DatabaseHandlers as DBHandlers
import LCA.Progress (getLogLines)
import LCA.Query
import qualified LCA.Service
import LCA.Tree (buildLoopAwareTree)
import LCA.Types
import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension)
import LCA.Types.API (ActivityForAPI (..), ActivityInfo (..), ActivityLinks (..), ActivityMetadata (..), ActivityStats (..), ActivitySummary (..), ExchangeDetail (..), ExchangeWithUnit (..), ExportNode (..), FlowDetail (..), FlowInfo (..), FlowRole (..), FlowSearchResult (..), FlowSummary (..), GraphExport (..), InventoryExport (..), InventoryFlowDetail (..), InventoryMetadata (..), InventoryStatistics (..), LCIARequest (..), LCIAResult (..), MappingStatus (..), MethodDetail (..), MethodFactorAPI (..), MethodSummary (..), NodeType (..), SearchResults (..), TreeEdge (..), TreeExport (..), TreeMetadata (..), UnmappedFlowAPI (..), DatabaseListResponse(..), DatabaseStatusAPI(..), ActivateResponse(..), UploadRequest(..), UploadResponse(..))
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
import Control.Concurrent.STM (readTVarIO)
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Types.Header (hSetCookie)

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
                :<|> "flow" :> Capture "flowId" Text :> QueryParam "db" Text :> Get '[JSON] FlowDetail
                :<|> "flow" :> Capture "flowId" Text :> "activities" :> QueryParam "db" Text :> Get '[JSON] [ActivitySummary]
                :<|> "methods" :> Get '[JSON] [MethodSummary]
                :<|> "method" :> Capture "methodId" Text :> Get '[JSON] MethodDetail
                :<|> "method" :> Capture "methodId" Text :> "factors" :> Get '[JSON] [MethodFactorAPI]
                :<|> "method" :> Capture "methodId" Text :> "mapping" :> QueryParam "db" Text :> Get '[JSON] MappingStatus
                :<|> "search" :> "flows" :> QueryParam "db" Text :> QueryParam "q" Text :> QueryParam "lang" Text :> QueryParam "limit" Int :> QueryParam "offset" Int :> Get '[JSON] (SearchResults FlowSearchResult)
                :<|> "search" :> "activities" :> QueryParam "db" Text :> QueryParam "name" Text :> QueryParam "geo" Text :> QueryParam "product" Text :> QueryParam "limit" Int :> QueryParam "offset" Int :> Get '[JSON] (SearchResults ActivitySummary)
                :<|> "lcia" :> Capture "processId" Text :> ReqBody '[JSON] LCIARequest :> Post '[JSON] Value
                -- Database management endpoints
                :<|> "databases" :> Get '[JSON] DatabaseListResponse
                -- Load/Unload/Delete endpoints
                :<|> "databases" :> Capture "dbName" Text :> "load" :> Post '[JSON] ActivateResponse
                :<|> "databases" :> Capture "dbName" Text :> "unload" :> Post '[JSON] ActivateResponse
                :<|> "databases" :> Capture "dbName" Text :> Delete '[JSON] ActivateResponse
                -- Upload endpoint (base64-encoded ZIP in JSON body)
                :<|> "databases" :> "upload" :> ReqBody '[JSON] UploadRequest :> Post '[JSON] UploadResponse
                -- Database setup endpoints (for cross-DB linking configuration)
                :<|> "databases" :> Capture "dbName" Text :> "setup" :> Get '[JSON] DatabaseSetupInfo
                :<|> "databases" :> Capture "dbName" Text :> "add-dependency" :> Capture "depName" Text :> Post '[JSON] DatabaseSetupInfo
                :<|> "databases" :> Capture "dbName" Text :> "remove-dependency" :> Capture "depName" Text :> Post '[JSON] DatabaseSetupInfo
                :<|> "databases" :> Capture "dbName" Text :> "finalize" :> Post '[JSON] ActivateResponse
                -- Log endpoint
                :<|> "logs" :> QueryParam "since" Int :> Get '[JSON] Value
                -- Auth endpoint (login)
                :<|> "auth" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] (Headers '[Header "Set-Cookie" String] Value)
           )

-- | Get database by name, throw 404 if not loaded
requireDatabaseByName :: DatabaseManager -> Text -> Handler (Database, SharedSolver)
requireDatabaseByName dbManager dbName = do
    maybeLoaded <- liftIO $ getDatabase dbManager dbName
    case maybeLoaded of
        Just loaded -> return (ldDatabase loaded, ldSharedSolver loaded)
        Nothing -> throwError err404{errBody = "Database not loaded: " <> BSL.fromStrict (T.encodeUtf8 dbName)}

-- | Get database for a processId, searching all loaded databases
requireDatabaseForProcessId :: DatabaseManager -> Text -> Handler (Database, SharedSolver)
requireDatabaseForProcessId dbManager processIdText = do
    allDbs <- liftIO $ readTVarIO (dmLoadedDbs dbManager)
    case [ld | ld <- M.elems allDbs, isValidProcessId (ldDatabase ld)] of
        (ld:_) -> return (ldDatabase ld, ldSharedSolver ld)
        [] -> throwError err404{errBody = "No loaded database contains process: " <> BSL.fromStrict (T.encodeUtf8 processIdText)}
  where
    isValidProcessId db = case parseProcessId db processIdText of
        Just _  -> True
        Nothing -> False

-- | Get database from explicit db query param, falling back to first loaded database
requireDatabaseByParam :: DatabaseManager -> Maybe Text -> Handler (Database, SharedSolver)
requireDatabaseByParam dbManager (Just dbName) = requireDatabaseByName dbManager dbName
requireDatabaseByParam dbManager Nothing = do
    allDbs <- liftIO $ readTVarIO (dmLoadedDbs dbManager)
    case M.elems allDbs of
        (ld:_) -> return (ldDatabase ld, ldSharedSolver ld)
        [] -> throwError err404{errBody = "No database loaded"}

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

-- | Login request body
data LoginRequest = LoginRequest
    { lrCode :: Text
    } deriving (Generic)

instance FromJSON LoginRequest where
    parseJSON = withObject "LoginRequest" $ \v ->
        LoginRequest <$> v .: "code"

-- | API server implementation
-- DatabaseManager is used to dynamically fetch current database on each request
lcaServer :: DatabaseManager -> Int -> Maybe FilePath -> Maybe String -> Server LCAAPI
lcaServer dbManager maxTreeDepth methodsDir password =
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
        :<|> DBHandlers.getDatabases dbManager
        :<|> DBHandlers.loadDatabaseHandler dbManager
        :<|> DBHandlers.unloadDatabaseHandler dbManager
        :<|> DBHandlers.deleteDatabaseHandler dbManager
        :<|> DBHandlers.uploadDatabaseHandler dbManager
        :<|> DBHandlers.getDatabaseSetupHandler dbManager
        :<|> DBHandlers.addDependencyHandler dbManager
        :<|> DBHandlers.removeDependencyHandler dbManager
        :<|> DBHandlers.finalizeDatabaseHandler dbManager
        :<|> getLogsHandler
        :<|> postAuth
  where
    getLogsHandler :: Maybe Int -> Handler Value
    getLogsHandler sinceMaybe = do
        let since = fromMaybe 0 sinceMaybe
        (nextIndex, lines) <- liftIO $ getLogLines since
        return $ object
            [ "lines" .= lines
            , "nextIndex" .= nextIndex
            ]

    postAuth :: LoginRequest -> Handler (Headers '[Header "Set-Cookie" String] Value)
    postAuth loginReq =
        case password of
            Nothing ->
                -- No password configured, auth always succeeds
                return $ noHeader $ object ["ok" .= True]
            Just pwd ->
                if T.unpack (lrCode loginReq) == pwd
                then
                    let cookieValue = "fplca_session=" ++ pwd ++ "; Path=/; HttpOnly; SameSite=Strict"
                    in return $ addHeader cookieValue $ object ["ok" .= True]
                else
                    throwError err401{errBody = "{\"error\":\"invalid code\"}"}

    -- Core activity endpoint - streamlined data
    getActivityInfo :: Text -> Handler ActivityInfo
    getActivityInfo processId = do
        (db, _) <- requireDatabaseForProcessId dbManager processId
        let unitCfg = dmUnitConfig dbManager
        case LCA.Service.getActivityInfo unitCfg db processId of
            Left (LCA.Service.ActivityNotFound _) -> throwError err404{errBody = "Activity not found"}
            Left (LCA.Service.InvalidProcessId _) -> throwError err400{errBody = "Invalid ProcessId format"}
            Right result -> case fromJSON result of
                Success activityInfo -> return activityInfo
                Error err -> throwError err500{errBody = BSL.fromStrict $ T.encodeUtf8 $ T.pack err}

    -- Activity flows sub-resource
    getActivityFlows :: Text -> Handler [FlowSummary]
    getActivityFlows processId = do
        (db, _) <- requireDatabaseForProcessId dbManager processId
        withValidatedActivity db processId $ \activity ->
            return $ LCA.Service.getActivityFlowSummaries db activity

    -- Activity inputs sub-resource
    getActivityInputs :: Text -> Handler [ExchangeDetail]
    getActivityInputs processId = do
        (db, _) <- requireDatabaseForProcessId dbManager processId
        withValidatedActivity db processId $ \activity ->
            return $ LCA.Service.getActivityInputDetails db activity

    -- Activity outputs sub-resource
    getActivityOutputs :: Text -> Handler [ExchangeDetail]
    getActivityOutputs processId = do
        (db, _) <- requireDatabaseForProcessId dbManager processId
        withValidatedActivity db processId $ \activity ->
            return $ LCA.Service.getActivityOutputDetails db activity

    -- Activity reference product sub-resource
    getActivityReferenceProduct :: Text -> Handler FlowDetail
    getActivityReferenceProduct processId = do
        (db, _) <- requireDatabaseForProcessId dbManager processId
        withValidatedActivity db processId $ \activity -> do
            case LCA.Service.getActivityReferenceProductDetail db activity of
                Nothing -> throwError err404{errBody = "No reference product found"}
                Just refProduct -> return refProduct

    -- Activity tree export for visualization (configurable depth)
    getActivityTree :: Text -> Handler TreeExport
    getActivityTree processId = do
        (db, _) <- requireDatabaseForProcessId dbManager processId
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
        (db, sharedSolver) <- requireDatabaseForProcessId dbManager processId
        result <- liftIO $ LCA.Service.getActivityInventoryWithSharedSolver sharedSolver db processId
        case result of
            Left (LCA.Service.ActivityNotFound _) -> throwError err404{errBody = "Activity not found"}
            Left (LCA.Service.InvalidProcessId _) -> throwError err400{errBody = "Invalid ProcessId format"}
            Left _ -> throwError err500{errBody = "Internal server error"}
            Right inventoryExport -> return inventoryExport

    -- Activity graph endpoint for network visualization
    getActivityGraph :: Text -> Maybe Double -> Handler GraphExport
    getActivityGraph processId maybeCutoff = do
        (db, sharedSolver) <- requireDatabaseForProcessId dbManager processId
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
        (db, _) <- requireDatabaseForProcessId dbManager processIdText
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
    getFlowDetail :: Text -> Maybe Text -> Handler FlowDetail
    getFlowDetail flowIdText dbParam = do
        (db, _) <- requireDatabaseByParam dbManager dbParam
        withValidatedFlow db flowIdText $ \flow -> do
            let usageCount = LCA.Service.getFlowUsageCount db (flowId flow)
            let unitName = getUnitNameForFlow (dbUnits db) flow
            return $ FlowDetail flow unitName usageCount

    -- Activities using a specific flow
    getFlowActivities :: Text -> Maybe Text -> Handler [ActivitySummary]
    getFlowActivities flowIdText dbParam = do
        (db, _) <- requireDatabaseByParam dbManager dbParam
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
    getMethodMapping :: Text -> Maybe Text -> Handler MappingStatus
    getMethodMapping methodIdText dbParam = do
        (db, _) <- requireDatabaseByParam dbManager dbParam
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
    searchFlows :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Handler (SearchResults FlowSearchResult)
    searchFlows dbParam queryParam langParam limitParam offsetParam = do
        (db, _) <- requireDatabaseByParam dbManager dbParam
        searchFlowsInternal db queryParam langParam limitParam offsetParam

    -- Search activities by specific fields with pagination and count
    searchActivitiesWithCount :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Handler (SearchResults ActivitySummary)
    searchActivitiesWithCount dbParam nameParam geoParam productParam limitParam offsetParam = do
        (db, _) <- requireDatabaseByParam dbManager dbParam
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
        (db, _) <- requireDatabaseForProcessId dbManager processId
        withValidatedActivity db processId $ \_ -> do
            -- This would implement LCIA computation with the provided method
            -- For now, return a placeholder
            return $ object ["status" .= ("not_implemented" :: Text), "processId" .= processId, "method" .= lciaMethod lciaReq]

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
