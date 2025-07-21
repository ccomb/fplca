{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module ACV.API where

import ACV.Types
import ACV.Query
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S
import GHC.Generics
import Servant

-- | API type definition - RESTful design with focused endpoints
type ACVAPI = "api" :> "v1" :> (
       "activity" :> Capture "uuid" Text :> Get '[JSON] ActivityInfo
  :<|> "activity" :> Capture "uuid" Text :> "flows" :> Get '[JSON] [FlowSummary]
  :<|> "activity" :> Capture "uuid" Text :> "inputs" :> Get '[JSON] [ExchangeDetail]
  :<|> "activity" :> Capture "uuid" Text :> "outputs" :> Get '[JSON] [ExchangeDetail]
  :<|> "activity" :> Capture "uuid" Text :> "reference-product" :> Get '[JSON] FlowDetail
  :<|> "flows" :> Capture "flowId" Text :> Get '[JSON] FlowDetail
  :<|> "flows" :> Capture "flowId" Text :> "activities" :> Get '[JSON] [ActivitySummary]
  :<|> "search" :> "flows" :> QueryParam "q" Text :> QueryParam "limit" Int :> QueryParam "offset" Int :> Get '[JSON] [FlowDetail]
  :<|> "search" :> "flows" :> QueryParam "q" Text :> QueryParam "lang" Text :> QueryParam "limit" Int :> QueryParam "offset" Int :> Get '[JSON] [FlowDetail]
  :<|> "search" :> "flows" :> "count" :> QueryParam "q" Text :> Get '[JSON] Int
  :<|> "search" :> "flows" :> "count" :> QueryParam "q" Text :> QueryParam "lang" Text :> Get '[JSON] Int
  :<|> "search" :> "activities" :> QueryParam "name" Text :> QueryParam "geo" Text :> QueryParam "product" Text :> QueryParam "limit" Int :> QueryParam "offset" Int :> Get '[JSON] [ActivitySummary]
  :<|> "search" :> "activities" :> "count" :> QueryParam "name" Text :> QueryParam "geo" Text :> QueryParam "product" Text :> Get '[JSON] Int
  :<|> "synonyms" :> "languages" :> Get '[JSON] [Text]
  :<|> "synonyms" :> "stats" :> Get '[JSON] SynonymStats
  )

-- | Enhanced exchange with unit name for API responses
data ExchangeWithUnit = ExchangeWithUnit
    { ewuExchange :: Exchange
    , ewuUnitName :: Text               -- Unit name for the exchange
    } deriving (Generic, Show)

-- | Activity information optimized for API responses
data ActivityForAPI = ActivityForAPI
    { pfaId :: UUID
    , pfaName :: Text
    , pfaDescription :: [Text] -- Description par paragraphes
    , pfaSynonyms :: M.Map Text (S.Set Text) -- Synonymes par langue
    , pfaClassifications :: M.Map Text Text -- Classifications (ISIC, CPC, etc.)
    , pfaLocation :: Text
    , pfaUnit :: Text -- Unité de référence
    , pfaExchanges :: [ExchangeWithUnit] -- Exchanges with unit names
    } deriving (Generic, Show)

-- | Streamlined activity information - core data only
data ActivityInfo = ActivityInfo
    { piActivity :: ActivityForAPI        -- Enhanced activity with unit names
    , piMetadata :: ActivityMetadata     -- Extended metadata
    , piStatistics :: ActivityStats      -- Usage statistics
    , piLinks :: ActivityLinks           -- Links to sub-resources
    } deriving (Generic, Show)

-- | Extended activity metadata
data ActivityMetadata = ActivityMetadata
    { pmTotalFlows :: Int              -- Number of unique flows used
    , pmTechnosphereInputs :: Int      -- Count of technosphere inputs
    , pmBiosphereExchanges :: Int      -- Count of biosphere exchanges  
    , pmHasReferenceProduct :: Bool    -- Whether activity has reference product
    , pmReferenceProductFlow :: Maybe UUID -- Flow ID of reference product
    } deriving (Generic, Show)

-- | Links to related resources
data ActivityLinks = ActivityLinks
    { plFlowsUrl :: Text              -- URL to flows endpoint
    , plInputsUrl :: Text             -- URL to inputs endpoint  
    , plOutputsUrl :: Text            -- URL to outputs endpoint
    , plReferenceProductUrl :: Maybe Text -- URL to reference product (if exists)
    } deriving (Generic, Show)

-- | Lightweight flow information for lists
data FlowSummary = FlowSummary
    { fsFlow :: Flow                  -- Core flow data
    , fsUnitName :: Text              -- Unit name for the flow
    , fsUsageCount :: Int             -- How many activities use this flow
    , fsRole :: FlowRole              -- Role in this specific activity
    } deriving (Generic, Show)

-- | Role of a flow in a specific activity context
data FlowRole = InputFlow | OutputFlow | ReferenceProductFlow
    deriving (Eq, Show, Generic)

-- | Flow with additional metadata
data FlowDetail = FlowDetail
    { fdFlow :: Flow
    , fdUnitName :: Text      -- Unit name for the flow
    , fdUsageCount :: Int     -- How many activities use this flow
    } deriving (Generic, Show)

-- | Exchange with flow, unit, and target activity information
data ExchangeDetail = ExchangeDetail
    { edExchange :: Exchange
    , edFlow :: Flow
    , edFlowUnitName :: Text                   -- Unit name for the flow's default unit
    , edUnit :: Unit                           -- Unit information for the exchange
    , edExchangeUnitName :: Text               -- Unit name for the exchange's specific unit
    , edTargetActivity :: Maybe ActivitySummary  -- Target activity for technosphere inputs
    } deriving (Generic, Show)

-- | Minimal activity information for navigation
data ActivitySummary = ActivitySummary
    { prsId :: UUID
    , prsName :: Text
    , prsLocation :: Text
    } deriving (Generic, Show)

-- | Activity statistics
data ActivityStats = ActivityStats
    { psInputCount :: Int
    , psOutputCount :: Int
    , psTotalExchanges :: Int
    , psLocation :: Text
    } deriving (Generic, Show)

-- JSON instances
instance ToJSON ActivityInfo
instance ToJSON ActivityForAPI
instance ToJSON ExchangeWithUnit
instance ToJSON ActivityMetadata
instance ToJSON ActivityLinks
instance ToJSON FlowSummary
instance ToJSON FlowRole
instance ToJSON FlowDetail  
instance ToJSON ExchangeDetail
instance ToJSON ActivityStats
instance ToJSON ActivitySummary
instance ToJSON Activity
instance ToJSON Exchange
instance ToJSON Flow
instance ToJSON FlowType
instance ToJSON Unit
instance ToJSON SynonymStats

-- | API server implementation with multiple focused endpoints
acvServer :: Database -> Server ACVAPI
acvServer db = getActivityInfo
         :<|> getActivityFlows  
         :<|> getActivityInputs
         :<|> getActivityOutputs
         :<|> getActivityReferenceProduct
         :<|> getFlowDetail
         :<|> getFlowActivities
         :<|> searchFlows
         :<|> searchFlowsInLanguage
         :<|> countFlows
         :<|> countFlowsInLanguage
         :<|> searchActivities
         :<|> countActivities
         :<|> getAvailableLanguagesAPI
         :<|> getSynonymStatsAPI
  where
    -- Core activity endpoint - streamlined data
    getActivityInfo :: Text -> Handler ActivityInfo
    getActivityInfo uuid = do
      case M.lookup uuid (dbActivities db) of
        Nothing -> throwError err404 { errBody = "Activity not found" }
        Just activity -> do
          let activityForAPI = convertActivityForAPI db activity
          let metadata = calculateActivityMetadata db activity
          let stats = calculateActivityStats activity
          let links = generateActivityLinks uuid
          
          return $ ActivityInfo
            { piActivity = activityForAPI
            , piMetadata = metadata  
            , piStatistics = stats
            , piLinks = links
            }
    
    -- Activity flows sub-resource
    getActivityFlows :: Text -> Handler [FlowSummary]
    getActivityFlows uuid = do
      case M.lookup uuid (dbActivities db) of
        Nothing -> throwError err404 { errBody = "Activity not found" }
        Just activity -> return $ getActivityFlowSummaries db activity
    
    -- Activity inputs sub-resource  
    getActivityInputs :: Text -> Handler [ExchangeDetail]
    getActivityInputs uuid = do
      case M.lookup uuid (dbActivities db) of
        Nothing -> throwError err404 { errBody = "Activity not found" }
        Just activity -> return $ getActivityInputDetails db activity
    
    -- Activity outputs sub-resource
    getActivityOutputs :: Text -> Handler [ExchangeDetail] 
    getActivityOutputs uuid = do
      case M.lookup uuid (dbActivities db) of
        Nothing -> throwError err404 { errBody = "Activity not found" }
        Just activity -> return $ getActivityOutputDetails db activity
    
    -- Activity reference product sub-resource
    getActivityReferenceProduct :: Text -> Handler FlowDetail
    getActivityReferenceProduct uuid = do
      case M.lookup uuid (dbActivities db) of
        Nothing -> throwError err404 { errBody = "Activity not found" }
        Just activity -> do
          case getActivityReferenceProductDetail db activity of
            Nothing -> throwError err404 { errBody = "No reference product found" }
            Just refProduct -> return refProduct
    
    -- Flow detail endpoint
    getFlowDetail :: Text -> Handler FlowDetail
    getFlowDetail flowId = do
      case M.lookup flowId (dbFlows db) of
        Nothing -> throwError err404 { errBody = "Flow not found" }
        Just flow -> do
          let usageCount = getFlowUsageCount db flowId
          let unitName = getUnitNameForFlow (dbUnits db) flow
          return $ FlowDetail flow unitName usageCount
    
    -- Activities using a specific flow
    getFlowActivities :: Text -> Handler [ActivitySummary]
    getFlowActivities flowId = do
      case M.lookup flowId (dbFlows db) of
        Nothing -> throwError err404 { errBody = "Flow not found" }
        Just _ -> return $ getActivitiesUsingFlow db flowId
    
    -- Search flows by name or synonym with pagination
    searchFlows :: Maybe Text -> Maybe Int -> Maybe Int -> Handler [FlowDetail]
    searchFlows Nothing _ _ = return []
    searchFlows (Just query) limitParam offsetParam = do
      let flows = findFlowsBySynonym db query
          flowDetails = [FlowDetail flow (getUnitNameForFlow (dbUnits db) flow) (getFlowUsageCount db (flowId flow)) | flow <- flows]
          limit = min 1000 (maybe 50 id limitParam)  -- Default limit: 50, max: 1000
          offset = maybe 0 id offsetParam -- Default offset: 0
          paginatedResults = take limit $ drop offset flowDetails
      return paginatedResults
    
    -- Search flows by synonym in specific language with pagination
    searchFlowsInLanguage :: Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Handler [FlowDetail]
    searchFlowsInLanguage Nothing _ _ _ = return []
    searchFlowsInLanguage _ Nothing _ _ = return []
    searchFlowsInLanguage (Just query) (Just lang) limitParam offsetParam = do
      let flows = findFlowsBySynonymInLanguage db lang query
          flowDetails = [FlowDetail flow (getUnitNameForFlow (dbUnits db) flow) (getFlowUsageCount db (flowId flow)) | flow <- flows]
          limit = min 1000 (maybe 50 id limitParam)  -- Default limit: 50, max: 1000
          offset = maybe 0 id offsetParam -- Default offset: 0
          paginatedResults = take limit $ drop offset flowDetails
      return paginatedResults
    
    -- Search activities by specific fields with pagination
    searchActivities :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Handler [ActivitySummary]
    searchActivities nameParam geoParam productParam limitParam offsetParam = do
      let activities = findActivitiesByFields db nameParam geoParam productParam
          activitySummaries = [ActivitySummary (activityId activity) (activityName activity) (activityLocation activity) | activity <- activities]
          limit = min 1000 (maybe 50 id limitParam)  -- Default limit: 50, max: 1000
          offset = maybe 0 id offsetParam -- Default offset: 0
          paginatedResults = take limit $ drop offset activitySummaries
      return paginatedResults
    
    -- Count flows by name or synonym
    countFlows :: Maybe Text -> Handler Int
    countFlows Nothing = return 0
    countFlows (Just query) = do
      let flows = findFlowsBySynonym db query
      return $ length flows
    
    -- Count flows by synonym in specific language
    countFlowsInLanguage :: Maybe Text -> Maybe Text -> Handler Int
    countFlowsInLanguage Nothing _ = return 0
    countFlowsInLanguage _ Nothing = return 0
    countFlowsInLanguage (Just query) (Just lang) = do
      let flows = findFlowsBySynonymInLanguage db lang query
      return $ length flows
    
    -- Count activities by specific fields
    countActivities :: Maybe Text -> Maybe Text -> Maybe Text -> Handler Int
    countActivities nameParam geoParam productParam = do
      let activities = findActivitiesByFields db nameParam geoParam productParam
      return $ length activities
    
    -- Get available languages
    getAvailableLanguagesAPI :: Handler [Text]
    getAvailableLanguagesAPI = return $ getAvailableLanguages db
    
    -- Get synonym statistics
    getSynonymStatsAPI :: Handler SynonymStats
    getSynonymStatsAPI = return $ getSynonymStats db

-- | Calculate extended metadata for a activity  
calculateActivityMetadata :: Database -> Activity -> ActivityMetadata
calculateActivityMetadata db activity =
    let allExchanges = exchanges activity
        uniqueFlows = length $ M.fromList [(exchangeFlowId ex, ()) | ex <- allExchanges]
        techInputs = length [ex | ex <- allExchanges, isTechnosphereExchange ex, exchangeIsInput ex, not (exchangeIsReference ex)]
        bioExchanges = length [ex | ex <- allExchanges, isBiosphereExchange ex]
        refProduct = case [ex | ex <- allExchanges, exchangeIsReference ex] of
                       [] -> Nothing
                       (ex:_) -> Just (exchangeFlowId ex)
    in ActivityMetadata
        { pmTotalFlows = uniqueFlows
        , pmTechnosphereInputs = techInputs  
        , pmBiosphereExchanges = bioExchanges
        , pmHasReferenceProduct = refProduct /= Nothing
        , pmReferenceProductFlow = refProduct
        }

-- | Generate links to sub-resources for a activity
generateActivityLinks :: Text -> ActivityLinks
generateActivityLinks uuid = ActivityLinks
    { plFlowsUrl = "/api/v1/activity/" <> uuid <> "/flows"
    , plInputsUrl = "/api/v1/activity/" <> uuid <> "/inputs"
    , plOutputsUrl = "/api/v1/activity/" <> uuid <> "/outputs"  
    , plReferenceProductUrl = Just ("/api/v1/activity/" <> uuid <> "/reference-product")
    }

-- | Get flows used by a activity as lightweight summaries
getActivityFlowSummaries :: Database -> Activity -> [FlowSummary]
getActivityFlowSummaries db activity =
    [ FlowSummary flow (getUnitNameForFlow (dbUnits db) flow) (getFlowUsageCount db (flowId flow)) (determineFlowRole exchange)
    | exchange <- exchanges activity
    , Just flow <- [M.lookup (exchangeFlowId exchange) (dbFlows db)]
    ]
  where
    determineFlowRole ex
        | exchangeIsReference ex = ReferenceProductFlow
        | exchangeIsInput ex = InputFlow  
        | otherwise = OutputFlow

-- | Get reference product as FlowDetail (if exists)
getActivityReferenceProductDetail :: Database -> Activity -> Maybe FlowDetail
getActivityReferenceProductDetail db activity = do
    refExchange <- case filter exchangeIsReference (exchanges activity) of
                     [] -> Nothing
                     (ex:_) -> Just ex
    flow <- M.lookup (exchangeFlowId refExchange) (dbFlows db)
    let usageCount = getFlowUsageCount db (flowId flow)
    let unitName = getUnitNameForFlow (dbUnits db) flow
    return $ FlowDetail flow unitName usageCount

-- | Get activities that use a specific flow as ActivitySummary list
getActivitiesUsingFlow :: Database -> UUID -> [ActivitySummary]  
getActivitiesUsingFlow db flowUUID =
    case M.lookup flowUUID (idxByFlow $ dbIndexes db) of
        Nothing -> []
        Just activityUUIDs -> 
            let uniqueUUIDs = S.toList $ S.fromList activityUUIDs  -- Deduplicate activity UUIDs
            in [ ActivitySummary (activityId proc) (activityName proc) (activityLocation proc)
               | procUUID <- uniqueUUIDs
               , Just proc <- [M.lookup procUUID (dbActivities db)]
               ]

-- | Get all flows used by a activity with usage statistics (legacy function - kept for compatibility)
getActivityFlows :: Database -> Activity -> [FlowDetail]
getActivityFlows db activity = 
    [ FlowDetail flow (getUnitNameForFlow (dbUnits db) flow) (getFlowUsageCount db (flowId flow))
    | exchange <- exchanges activity
    , Just flow <- [M.lookup (exchangeFlowId exchange) (dbFlows db)]
    ]

-- | Get flow usage count across all activities
getFlowUsageCount :: Database -> UUID -> Int
getFlowUsageCount db flowUUID = 
    case M.lookup flowUUID (idxByFlow $ dbIndexes db) of
        Nothing -> 0
        Just activityUUIDs -> length activityUUIDs

-- | Get detailed input exchanges
getActivityInputDetails :: Database -> Activity -> [ExchangeDetail]
getActivityInputDetails db activity =
    [ ExchangeDetail exchange flow (getUnitNameForFlow (dbUnits db) flow) unit (getUnitNameForExchange (dbUnits db) exchange) (getTargetActivity db exchange)
    | exchange <- exchanges activity
    , exchangeIsInput exchange
    , Just flow <- [M.lookup (exchangeFlowId exchange) (dbFlows db)]
    , Just unit <- [M.lookup (exchangeUnitId exchange) (dbUnits db)]
    ]

-- | Get detailed output exchanges  
getActivityOutputDetails :: Database -> Activity -> [ExchangeDetail]
getActivityOutputDetails db activity =
    [ ExchangeDetail exchange flow (getUnitNameForFlow (dbUnits db) flow) unit (getUnitNameForExchange (dbUnits db) exchange) (getTargetActivity db exchange)
    | exchange <- exchanges activity
    , not (exchangeIsInput exchange)
    , Just flow <- [M.lookup (exchangeFlowId exchange) (dbFlows db)]
    , Just unit <- [M.lookup (exchangeUnitId exchange) (dbUnits db)]
    ]


-- | Get target activity for technosphere navigation
getTargetActivity :: Database -> Exchange -> Maybe ActivitySummary
getTargetActivity db exchange = do
    targetId <- exchangeActivityLinkId exchange
    targetActivity <- M.lookup targetId (dbActivities db)
    return $ ActivitySummary
        { prsId = activityId targetActivity
        , prsName = activityName targetActivity
        , prsLocation = activityLocation targetActivity
        }

-- | Calculate activity statistics
calculateActivityStats :: Activity -> ActivityStats
calculateActivityStats activity = ActivityStats
    { psInputCount = length $ filter exchangeIsInput (exchanges activity)
    , psOutputCount = length $ filter (not . exchangeIsInput) (exchanges activity)  
    , psTotalExchanges = length (exchanges activity)
    , psLocation = activityLocation activity
    }

-- | Convert Activity to ActivityForAPI with unit names
convertActivityForAPI :: Database -> Activity -> ActivityForAPI
convertActivityForAPI db activity = ActivityForAPI
    { pfaId = activityId activity
    , pfaName = activityName activity
    , pfaDescription = activityDescription activity
    , pfaSynonyms = activitySynonyms activity
    , pfaClassifications = activityClassification activity
    , pfaLocation = activityLocation activity
    , pfaUnit = activityUnit activity
    , pfaExchanges = map convertExchangeWithUnit (exchanges activity)
    }
  where
    convertExchangeWithUnit exchange = ExchangeWithUnit
        { ewuExchange = exchange
        , ewuUnitName = getUnitNameForExchange (dbUnits db) exchange
        }

-- | Proxy for the API
acvAPI :: Proxy ACVAPI
acvAPI = Proxy
