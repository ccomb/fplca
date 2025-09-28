{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module ACV.API where

import ACV.Inventory (Inventory, computeInventoryFromLoopAwareTree, computeInventoryWithFlows)
import ACV.Query
import qualified ACV.Service
import ACV.Tree (buildActivityTreeWithDatabase, buildCutoffLoopAwareTree, buildLoopAwareTree)
import ACV.Types
import ACV.Types.API (SearchResults(..), ActivitySummary(..), FlowSearchResult(..), InventoryExport(..), InventoryMetadata(..), InventoryFlowDetail(..), InventoryStatistics(..), TreeExport(..), TreeMetadata(..), ExportNode(..), NodeType(..), TreeEdge(..), FlowInfo(..), FlowSummary(..), FlowRole(..), ActivityInfo(..), ActivityForAPI(..), ActivityMetadata(..), ActivityLinks(..), ActivityStats(..), ExchangeWithUnit(..), FlowDetail(..), ExchangeDetail(..), LCIARequest(..))
import Data.Aeson
import Data.Aeson.Types (Result(..), fromJSON)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as BSL
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics
import Servant
import qualified Data.UUID as UUID

-- | API type definition - RESTful design with focused endpoints
type ACVAPI =
    "api"
        :> "v1"
        :> ( "activity" :> Capture "uuid" Text :> Get '[JSON] ActivityInfo
                :<|> "activity" :> Capture "uuid" Text :> "flows" :> Get '[JSON] [FlowSummary]
                :<|> "activity" :> Capture "uuid" Text :> "inputs" :> Get '[JSON] [ExchangeDetail]
                :<|> "activity" :> Capture "uuid" Text :> "outputs" :> Get '[JSON] [ExchangeDetail]
                :<|> "activity" :> Capture "uuid" Text :> "reference-product" :> Get '[JSON] FlowDetail
                :<|> "activity" :> Capture "uuid" Text :> "tree" :> Get '[JSON] TreeExport
                :<|> "activity" :> Capture "uuid" Text :> "inventory" :> Get '[JSON] InventoryExport
                :<|> "flow" :> Capture "flowId" Text :> Get '[JSON] FlowDetail
                :<|> "flow" :> Capture "flowId" Text :> "activities" :> Get '[JSON] [ActivitySummary]
                :<|> "search" :> "flows" :> QueryParam "q" Text :> QueryParam "lang" Text :> QueryParam "limit" Int :> QueryParam "offset" Int :> Get '[JSON] (SearchResults FlowSearchResult)
                :<|> "search" :> "activities" :> QueryParam "name" Text :> QueryParam "geo" Text :> QueryParam "product" Text :> QueryParam "limit" Int :> QueryParam "offset" Int :> Get '[JSON] (SearchResults ActivitySummary)
                :<|> "lcia" :> Capture "uuid" Text :> ReqBody '[JSON] LCIARequest :> Post '[JSON] Value
           )


-- JSON instances
instance ToJSON ACV.Query.SynonymStats




-- | Helper function to validate UUID and lookup activity
withValidatedActivity :: Database -> Text -> (Activity -> Handler a) -> Handler a
withValidatedActivity db uuid action = do
    case ACV.Service.validateUUID uuid of
        Left (ACV.Service.InvalidUUID errorMsg) -> throwError err400{errBody = BSL.fromStrict $ T.encodeUtf8 errorMsg}
        Left _ -> throwError err400{errBody = "Invalid request"}
        Right validUuid -> 
            case M.lookup validUuid (dbActivities db) of
                Nothing -> throwError err404{errBody = "Activity not found"}
                Just activity -> action activity

-- | Helper function to validate UUID and lookup flow
withValidatedFlow :: Database -> Text -> (Flow -> Handler a) -> Handler a
withValidatedFlow db uuid action = do
    case ACV.Service.validateUUID uuid of
        Left (ACV.Service.InvalidUUID errorMsg) -> throwError err400{errBody = BSL.fromStrict $ T.encodeUtf8 errorMsg}
        Left _ -> throwError err400{errBody = "Invalid request"}
        Right validUuid -> 
            case M.lookup validUuid (dbFlows db) of
                Nothing -> throwError err404{errBody = "Flow not found"}
                Just flow -> action flow

-- | API server implementation with multiple focused endpoints
acvServer :: Database -> Int -> Server ACVAPI
acvServer db maxTreeDepth =
    getActivityInfo
        :<|> getActivityFlows
        :<|> getActivityInputs
        :<|> getActivityOutputs
        :<|> getActivityReferenceProduct
        :<|> getActivityTree
        :<|> getActivityInventory
        :<|> getFlowDetail
        :<|> getFlowActivities
        :<|> searchFlows
        :<|> searchActivitiesWithCount
        :<|> postLCIA
  where
    -- Core activity endpoint - streamlined data
    getActivityInfo :: Text -> Handler ActivityInfo
    getActivityInfo uuid = do
        case ACV.Service.getActivityInfo db uuid of
            Left (ACV.Service.ActivityNotFound _) -> throwError err404{errBody = "Activity not found"}
            Left (ACV.Service.InvalidUUID _) -> throwError err400{errBody = "Invalid UUID format"}
            Right result -> case fromJSON result of
                Success activityInfo -> return activityInfo
                Error err -> throwError err500{errBody = BSL.fromStrict $ T.encodeUtf8 $ T.pack err}

    -- Activity flows sub-resource
    getActivityFlows :: Text -> Handler [FlowSummary]
    getActivityFlows uuid = withValidatedActivity db uuid $ \activity ->
        return $ ACV.Service.getActivityFlowSummaries db activity

    -- Activity inputs sub-resource
    getActivityInputs :: Text -> Handler [ExchangeDetail]
    getActivityInputs uuid = withValidatedActivity db uuid $ \activity ->
        return $ ACV.Service.getActivityInputDetails db activity

    -- Activity outputs sub-resource
    getActivityOutputs :: Text -> Handler [ExchangeDetail]
    getActivityOutputs uuid = withValidatedActivity db uuid $ \activity ->
        return $ ACV.Service.getActivityOutputDetails db activity

    -- Activity reference product sub-resource
    getActivityReferenceProduct :: Text -> Handler FlowDetail
    getActivityReferenceProduct uuid = withValidatedActivity db uuid $ \activity -> do
        case ACV.Service.getActivityReferenceProductDetail db activity of
            Nothing -> throwError err404{errBody = "No reference product found"}
            Just refProduct -> return refProduct

    -- Activity tree export for visualization (configurable depth)
    getActivityTree :: Text -> Handler TreeExport
    getActivityTree uuid = withValidatedActivity db uuid $ \_ -> do
        -- Use CLI --tree-depth option for configurable depth
        -- Default depth limit prevents DOS attacks via deep tree requests
        let loopAwareTree = buildLoopAwareTree db uuid maxTreeDepth
        return $ ACV.Service.convertToTreeExport db uuid maxTreeDepth loopAwareTree

    -- Activity inventory calculation (full supply chain LCI)
    getActivityInventory :: Text -> Handler InventoryExport
    getActivityInventory uuid = withValidatedActivity db uuid $ \activity -> do
        case ACV.Service.computeActivityInventory db uuid of
            Left (ACV.Service.ActivityNotFound _) -> throwError err404{errBody = "Activity not found"}
            Left (ACV.Service.InvalidUUID _) -> throwError err400{errBody = "Invalid UUID format"}
            Right inventory -> return $ ACV.Service.convertToInventoryExport db activity inventory 35

    -- Flow detail endpoint
    getFlowDetail :: Text -> Handler FlowDetail
    getFlowDetail flowId = withValidatedFlow db flowId $ \flow -> do
        let usageCount = ACV.Service.getFlowUsageCount db flowId
        let unitName = getUnitNameForFlow (dbUnits db) flow
        return $ FlowDetail flow unitName usageCount

    -- Activities using a specific flow
    getFlowActivities :: Text -> Handler [ActivitySummary]
    getFlowActivities flowId = withValidatedFlow db flowId $ \_ ->
        return $ ACV.Service.getActivitiesUsingFlow db flowId

    -- Search flows by name or synonym with optional language filtering and pagination
    searchFlows :: Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Handler (SearchResults FlowSearchResult)
    searchFlows queryParam langParam limitParam offsetParam = 
        searchFlowsInternal db queryParam langParam limitParam offsetParam

    -- Search activities by specific fields with pagination and count
    searchActivitiesWithCount :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Handler (SearchResults ActivitySummary)
    searchActivitiesWithCount nameParam geoParam productParam limitParam offsetParam = do
        let activities = findActivitiesByFields db nameParam geoParam productParam
            activitySummaries = [ActivitySummary (activityId activity) (activityName activity) (activityLocation activity) | activity <- activities]
        return $ paginateResults activitySummaries limitParam offsetParam


    -- LCIA computation
    postLCIA :: Text -> LCIARequest -> Handler Value
    postLCIA uuid lciaReq = withValidatedActivity db uuid $ \_ -> do
        -- This would implement LCIA computation with the provided method
        -- For now, return a placeholder
        return $ object ["status" .= ("not_implemented" :: Text), "uuid" .= uuid, "method" .= lciaMethod lciaReq]

-- | Helper function to apply pagination to search results
paginateResults :: [a] -> Maybe Int -> Maybe Int -> SearchResults a
paginateResults results limitParam offsetParam =
    let totalCount = length results
        limit = min 1000 (maybe 50 id limitParam) -- Default limit: 50, max: 1000
        offset = maybe 0 id offsetParam -- Default offset: 0
        paginatedResults = take limit $ drop offset results
        hasMore = offset + length paginatedResults < totalCount
    in SearchResults paginatedResults totalCount offset limit hasMore

-- | Internal helper for flow search with optional language filtering
searchFlowsInternal :: Database -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Handler (SearchResults FlowSearchResult)
searchFlowsInternal _ Nothing _ _ _ = return $ SearchResults [] 0 0 50 False
searchFlowsInternal db (Just query) langParam limitParam offsetParam = do
    let flows = case langParam of
            Nothing -> findFlowsBySynonym db query
            Just lang -> findFlowsBySynonymInLanguage db lang query
        flowSearchResults = [FlowSearchResult (flowId flow) (flowName flow) (flowCategory flow) (getUnitNameForFlow (dbUnits db) flow) (M.map S.toList (flowSynonyms flow)) | flow <- flows]
    return $ paginateResults flowSearchResults limitParam offsetParam

-- | Proxy for the API
acvAPI :: Proxy ACVAPI
acvAPI = Proxy
