{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module ACV.API where

import ACV.Query
import ACV.Tree (buildLoopAwareTree, buildActivityTreeWithDatabase)
import ACV.Inventory (computeInventoryWithFlows, Inventory)
import ACV.Types
import Data.Aeson
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics
import Servant

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
    , ewuUnitName :: Text -- Unit name for the exchange
    }
    deriving (Generic, Show)

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
    }
    deriving (Generic, Show)

-- | Streamlined activity information - core data only
data ActivityInfo = ActivityInfo
    { piActivity :: ActivityForAPI -- Enhanced activity with unit names
    , piMetadata :: ActivityMetadata -- Extended metadata
    , piStatistics :: ActivityStats -- Usage statistics
    , piLinks :: ActivityLinks -- Links to sub-resources
    }
    deriving (Generic, Show)

-- | Extended activity metadata
data ActivityMetadata = ActivityMetadata
    { pmTotalFlows :: Int -- Number of unique flows used
    , pmTechnosphereInputs :: Int -- Count of technosphere inputs
    , pmBiosphereExchanges :: Int -- Count of biosphere exchanges
    , pmHasReferenceProduct :: Bool -- Whether activity has reference product
    , pmReferenceProductFlow :: Maybe UUID -- Flow ID of reference product
    }
    deriving (Generic, Show)

-- | Links to related resources
data ActivityLinks = ActivityLinks
    { plFlowsUrl :: Text -- URL to flows endpoint
    , plInputsUrl :: Text -- URL to inputs endpoint
    , plOutputsUrl :: Text -- URL to outputs endpoint
    , plReferenceProductUrl :: Maybe Text -- URL to reference product (if exists)
    }
    deriving (Generic, Show)

-- | Lightweight flow information for lists
data FlowSummary = FlowSummary
    { fsFlow :: Flow -- Core flow data
    , fsUnitName :: Text -- Unit name for the flow
    , fsUsageCount :: Int -- How many activities use this flow
    , fsRole :: FlowRole -- Role in this specific activity
    }
    deriving (Generic, Show)

-- | Role of a flow in a specific activity context
data FlowRole = InputFlow | OutputFlow | ReferenceProductFlow
    deriving (Eq, Show, Generic)

-- | Flow with additional metadata
data FlowDetail = FlowDetail
    { fdFlow :: Flow
    , fdUnitName :: Text -- Unit name for the flow
    , fdUsageCount :: Int -- How many activities use this flow
    }
    deriving (Generic, Show)

-- | Exchange with flow, unit, and target activity information
data ExchangeDetail = ExchangeDetail
    { edExchange :: Exchange
    , edFlow :: Flow
    , edFlowUnitName :: Text -- Unit name for the flow's default unit
    , edUnit :: Unit -- Unit information for the exchange
    , edExchangeUnitName :: Text -- Unit name for the exchange's specific unit
    , edTargetActivity :: Maybe ActivitySummary -- Target activity for technosphere inputs
    }
    deriving (Generic, Show)

-- | Minimal activity information for navigation
data ActivitySummary = ActivitySummary
    { prsId :: UUID
    , prsName :: Text
    , prsLocation :: Text
    }
    deriving (Generic, Show)

-- | Activity statistics
data ActivityStats = ActivityStats
    { psInputCount :: Int
    , psOutputCount :: Int
    , psTotalExchanges :: Int
    , psLocation :: Text
    }
    deriving (Generic, Show)

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

-- | Tree export data structures for Elm frontend
data TreeExport = TreeExport
    { teTree :: TreeMetadata
    , teNodes :: M.Map UUID ExportNode
    , teEdges :: [TreeEdge]
    }
    deriving (Generic, Show)

data TreeMetadata = TreeMetadata
    { tmRootId :: UUID
    , tmMaxDepth :: Int
    , tmTotalNodes :: Int
    , tmLoopNodes :: Int
    , tmLeafNodes :: Int
    , tmExpandableNodes :: Int  -- Nodes that could expand further
    }
    deriving (Generic, Show)

data ExportNode = ExportNode
    { enId :: UUID
    , enName :: Text
    , enDescription :: [Text]
    , enLocation :: Text
    , enUnit :: Text
    , enNodeType :: NodeType
    , enDepth :: Int
    , enLoopTarget :: Maybe UUID
    , enParentId :: Maybe UUID      -- For navigation back up
    , enChildrenCount :: Int        -- Number of potential children for expandability
    }
    deriving (Generic, Show)

data NodeType = ActivityNode | LoopNode
    deriving (Eq, Generic, Show)

data TreeEdge = TreeEdge
    { teFrom :: UUID
    , teTo :: UUID
    , teFlow :: FlowInfo
    , teQuantity :: Double
    , teUnit :: Text
    }
    deriving (Generic, Show)

data FlowInfo = FlowInfo
    { fiId :: UUID
    , fiName :: Text
    , fiCategory :: Text
    }
    deriving (Generic, Show)

-- JSON instances for tree export
instance ToJSON TreeExport
instance ToJSON TreeMetadata
instance ToJSON ExportNode
instance ToJSON NodeType
instance ToJSON TreeEdge
instance ToJSON FlowInfo

-- | Inventory export data structures
data InventoryExport = InventoryExport
    { ieMetadata :: InventoryMetadata
    , ieFlows :: [InventoryFlowDetail]
    , ieStatistics :: InventoryStatistics
    } deriving (Generic, Show)

data InventoryMetadata = InventoryMetadata
    { imRootActivity :: ActivitySummary
    , imCalculationDepth :: Int
    , imTotalFlows :: Int
    , imEmissionFlows :: Int      -- Biosphere outputs (negative environmental impact)
    , imResourceFlows :: Int      -- Biosphere inputs (resource extraction)  
    } deriving (Generic, Show)

data InventoryFlowDetail = InventoryFlowDetail
    { ifdFlow :: Flow
    , ifdQuantity :: Double
    , ifdUnitName :: Text
    , ifdIsEmission :: Bool       -- True for emissions, False for resource extraction
    , ifdCategory :: Text         -- Flow category for grouping
    } deriving (Generic, Show)

data InventoryStatistics = InventoryStatistics  
    { isTotalQuantity :: Double   -- Sum of absolute values
    , isEmissionQuantity :: Double -- Sum of emissions (should be positive)
    , isResourceQuantity :: Double -- Sum of resource extraction (should be positive)
    , isTopCategories :: [(Text, Int)] -- Top flow categories by count
    } deriving (Generic, Show)

-- JSON instances for inventory export
instance ToJSON InventoryExport
instance ToJSON InventoryMetadata  
instance ToJSON InventoryFlowDetail
instance ToJSON InventoryStatistics

-- | API server implementation with multiple focused endpoints
acvServer :: Database -> Server ACVAPI
acvServer db =
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
            Nothing -> throwError err404{errBody = "Activity not found"}
            Just activity -> do
                let activityForAPI = convertActivityForAPI db activity
                let metadata = calculateActivityMetadata db activity
                let stats = calculateActivityStats activity
                let links = generateActivityLinks uuid

                return $
                    ActivityInfo
                        { piActivity = activityForAPI
                        , piMetadata = metadata
                        , piStatistics = stats
                        , piLinks = links
                        }

    -- Activity flows sub-resource
    getActivityFlows :: Text -> Handler [FlowSummary]
    getActivityFlows uuid = do
        case M.lookup uuid (dbActivities db) of
            Nothing -> throwError err404{errBody = "Activity not found"}
            Just activity -> return $ getActivityFlowSummaries db activity

    -- Activity inputs sub-resource
    getActivityInputs :: Text -> Handler [ExchangeDetail]
    getActivityInputs uuid = do
        case M.lookup uuid (dbActivities db) of
            Nothing -> throwError err404{errBody = "Activity not found"}
            Just activity -> return $ getActivityInputDetails db activity

    -- Activity outputs sub-resource
    getActivityOutputs :: Text -> Handler [ExchangeDetail]
    getActivityOutputs uuid = do
        case M.lookup uuid (dbActivities db) of
            Nothing -> throwError err404{errBody = "Activity not found"}
            Just activity -> return $ getActivityOutputDetails db activity

    -- Activity reference product sub-resource
    getActivityReferenceProduct :: Text -> Handler FlowDetail
    getActivityReferenceProduct uuid = do
        case M.lookup uuid (dbActivities db) of
            Nothing -> throwError err404{errBody = "Activity not found"}
            Just activity -> do
                case getActivityReferenceProductDetail db activity of
                    Nothing -> throwError err404{errBody = "No reference product found"}
                    Just refProduct -> return refProduct

    -- Activity tree export for visualization (fixed depth=2)
    getActivityTree :: Text -> Handler TreeExport
    getActivityTree uuid = do
        case M.lookup uuid (dbActivities db) of
            Nothing -> throwError err404{errBody = "Activity not found"}
            Just _ -> do
                let maxDepth = 2 -- Fixed depth for interactive navigation
                let loopAwareTree = buildLoopAwareTree db uuid maxDepth
                return $ convertToTreeExport db uuid maxDepth loopAwareTree

    -- Activity inventory calculation (full supply chain LCI)
    getActivityInventory :: Text -> Handler InventoryExport
    getActivityInventory uuid = do
        case M.lookup uuid (dbActivities db) of
            Nothing -> throwError err404{errBody = "Activity not found"}
            Just activity -> do
                -- Build full activity tree for inventory calculation
                let activityTree = buildActivityTreeWithDatabase db uuid
                let inventory = computeInventoryWithFlows (dbFlows db) activityTree
                return $ convertToInventoryExport db activity inventory

    -- Flow detail endpoint
    getFlowDetail :: Text -> Handler FlowDetail
    getFlowDetail flowId = do
        case M.lookup flowId (dbFlows db) of
            Nothing -> throwError err404{errBody = "Flow not found"}
            Just flow -> do
                let usageCount = getFlowUsageCount db flowId
                let unitName = getUnitNameForFlow (dbUnits db) flow
                return $ FlowDetail flow unitName usageCount

    -- Activities using a specific flow
    getFlowActivities :: Text -> Handler [ActivitySummary]
    getFlowActivities flowId = do
        case M.lookup flowId (dbFlows db) of
            Nothing -> throwError err404{errBody = "Flow not found"}
            Just _ -> return $ getActivitiesUsingFlow db flowId

    -- Search flows by name or synonym with pagination
    searchFlows :: Maybe Text -> Maybe Int -> Maybe Int -> Handler [FlowDetail]
    searchFlows Nothing _ _ = return []
    searchFlows (Just query) limitParam offsetParam = do
        let flows = findFlowsBySynonym db query
            flowDetails = [FlowDetail flow (getUnitNameForFlow (dbUnits db) flow) (getFlowUsageCount db (flowId flow)) | flow <- flows]
            limit = min 1000 (maybe 50 id limitParam) -- Default limit: 50, max: 1000
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
            limit = min 1000 (maybe 50 id limitParam) -- Default limit: 50, max: 1000
            offset = maybe 0 id offsetParam -- Default offset: 0
            paginatedResults = take limit $ drop offset flowDetails
        return paginatedResults

    -- Search activities by specific fields with pagination
    searchActivities :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Handler [ActivitySummary]
    searchActivities nameParam geoParam productParam limitParam offsetParam = do
        let activities = findActivitiesByFields db nameParam geoParam productParam
            activitySummaries = [ActivitySummary (activityId activity) (activityName activity) (activityLocation activity) | activity <- activities]
            limit = min 1000 (maybe 50 id limitParam) -- Default limit: 50, max: 1000
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
            (ex : _) -> Just (exchangeFlowId ex)
     in ActivityMetadata
            { pmTotalFlows = uniqueFlows
            , pmTechnosphereInputs = techInputs
            , pmBiosphereExchanges = bioExchanges
            , pmHasReferenceProduct = refProduct /= Nothing
            , pmReferenceProductFlow = refProduct
            }

-- | Generate links to sub-resources for a activity
generateActivityLinks :: Text -> ActivityLinks
generateActivityLinks uuid =
    ActivityLinks
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
        (ex : _) -> Just ex
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
            let uniqueUUIDs = S.toList $ S.fromList activityUUIDs -- Deduplicate activity UUIDs
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
    return $
        ActivitySummary
            { prsId = activityId targetActivity
            , prsName = activityName targetActivity
            , prsLocation = activityLocation targetActivity
            }

-- | Calculate activity statistics
calculateActivityStats :: Activity -> ActivityStats
calculateActivityStats activity =
    ActivityStats
        { psInputCount = length $ filter exchangeIsInput (exchanges activity)
        , psOutputCount = length $ filter (not . exchangeIsInput) (exchanges activity)
        , psTotalExchanges = length (exchanges activity)
        , psLocation = activityLocation activity
        }

-- | Convert Activity to ActivityForAPI with unit names
convertActivityForAPI :: Database -> Activity -> ActivityForAPI
convertActivityForAPI db activity =
    ActivityForAPI
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
    convertExchangeWithUnit exchange =
        ExchangeWithUnit
            { ewuExchange = exchange
            , ewuUnitName = getUnitNameForExchange (dbUnits db) exchange
            }

-- | Convert LoopAwareTree to TreeExport format for JSON serialization
convertToTreeExport :: Database -> UUID -> Int -> LoopAwareTree -> TreeExport
convertToTreeExport db rootUUID maxDepth tree =
    let (nodes, edges, stats) = extractNodesAndEdges db tree 0 Nothing M.empty []
        metadata =
            TreeMetadata
                { tmRootId = rootUUID
                , tmMaxDepth = maxDepth
                , tmTotalNodes = M.size nodes
                , tmLoopNodes = length [() | (_, node) <- M.toList nodes, enNodeType node == LoopNode]
                , tmLeafNodes = length [() | (_, node) <- M.toList nodes, null [e | e <- edges, teFrom e == enId node]]
                , tmExpandableNodes = length [() | (_, node) <- M.toList nodes, enChildrenCount node > 0]
                }
     in TreeExport metadata nodes edges

-- | Extract nodes and edges from LoopAwareTree
extractNodesAndEdges :: Database -> LoopAwareTree -> Int -> Maybe UUID -> M.Map UUID ExportNode -> [TreeEdge] -> (M.Map UUID ExportNode, [TreeEdge], TreeStats)
extractNodesAndEdges db tree depth parentId nodeAcc edgeAcc = case tree of
    TreeLeaf activity ->
        let childrenCount = countPotentialChildren db activity
            node =
                ExportNode
                    { enId = activityId activity
                    , enName = activityName activity
                    , enDescription = activityDescription activity
                    , enLocation = activityLocation activity
                    , enUnit = activityUnit activity
                    , enNodeType = ActivityNode
                    , enDepth = depth
                    , enLoopTarget = Nothing
                    , enParentId = parentId
                    , enChildrenCount = childrenCount
                    }
            nodes' = M.insert (activityId activity) node nodeAcc
         in (nodes', edgeAcc, TreeStats 1 0 1)
    TreeLoop uuid name depth ->
        let node =
                ExportNode
                    { enId = uuid
                    , enName = name
                    , enDescription = ["Loop reference"]
                    , enLocation = "N/A"
                    , enUnit = "N/A"
                    , enNodeType = LoopNode
                    , enDepth = depth
                    , enLoopTarget = Just uuid
                    , enParentId = parentId
                    , enChildrenCount = 0  -- Loops don't expand
                    }
            nodes' = M.insert uuid node nodeAcc
         in (nodes', edgeAcc, TreeStats 1 1 0)
    TreeNode activity children ->
        let childrenCount = countPotentialChildren db activity
            parentNode =
                ExportNode
                    { enId = activityId activity
                    , enName = activityName activity
                    , enDescription = activityDescription activity
                    , enLocation = activityLocation activity
                    , enUnit = activityUnit activity
                    , enNodeType = ActivityNode
                    , enDepth = depth
                    , enLoopTarget = Nothing
                    , enParentId = parentId
                    , enChildrenCount = childrenCount
                    }
            nodes' = M.insert (activityId activity) parentNode nodeAcc
            currentId = activityId activity
            processChild (quantity, flow, subtree) (nodeAcc, edgeAcc, statsAcc) =
                let (childNodes, childEdges, childStats) = extractNodesAndEdges db subtree (depth + 1) (Just currentId) nodeAcc edgeAcc
                    edge =
                        TreeEdge
                            { teFrom = currentId
                            , teTo = getTreeNodeId subtree
                            , teFlow = FlowInfo (flowId flow) (flowName flow) (flowCategory flow)
                            , teQuantity = quantity
                            , teUnit = getUnitNameForFlow (dbUnits db) flow
                            }
                    newStats = combineStats statsAcc childStats
                 in (childNodes, edge : childEdges, newStats)
            (finalNodes, finalEdges, childStats) = foldr processChild (nodes', edgeAcc, TreeStats 1 0 0) children
         in (finalNodes, finalEdges, childStats)

-- | Helper to get node ID from LoopAwareTree
getTreeNodeId :: LoopAwareTree -> UUID
getTreeNodeId (TreeLeaf activity) = activityId activity
getTreeNodeId (TreeLoop uuid _ _) = uuid
getTreeNodeId (TreeNode activity _) = activityId activity

-- | Count potential children for navigation (technosphere inputs that could be expanded)
countPotentialChildren :: Database -> Activity -> Int
countPotentialChildren db activity = 
    length [ ex | ex <- exchanges activity
               , isTechnosphereExchange ex
               , exchangeIsInput ex
               , not (exchangeIsReference ex)
               , Just targetUUID <- [exchangeActivityLinkId ex]
               , M.member targetUUID (dbActivities db)
               ]

-- | Convert raw inventory to structured export format
convertToInventoryExport :: Database -> Activity -> Inventory -> InventoryExport
convertToInventoryExport db rootActivity inventory =
    let flowDetails = [ InventoryFlowDetail flow quantity (getUnitNameForFlow (dbUnits db) flow) isEmission (flowCategory flow)
                      | (flowUUID, quantity) <- M.toList inventory
                      , Just flow <- [M.lookup flowUUID (dbFlows db)]
                      , let isEmission = not (isResourceExtraction flow quantity)
                      ]
        
        emissionFlows = length [f | f <- flowDetails, ifdIsEmission f]
        resourceFlows = length [f | f <- flowDetails, not (ifdIsEmission f)]
        
        totalQuantity = sum [abs (ifdQuantity f) | f <- flowDetails]
        emissionQuantity = sum [ifdQuantity f | f <- flowDetails, ifdIsEmission f, ifdQuantity f > 0]
        resourceQuantity = sum [abs (ifdQuantity f) | f <- flowDetails, not (ifdIsEmission f)]
        
        categoryStats = take 10 $ 
            M.toList $ M.fromListWith (+) [(ifdCategory f, 1) | f <- flowDetails]
        
        metadata = InventoryMetadata
            { imRootActivity = ActivitySummary (activityId rootActivity) (activityName rootActivity) (activityLocation rootActivity)
            , imCalculationDepth = -1  -- Full tree depth (unknown beforehand)
            , imTotalFlows = length flowDetails
            , imEmissionFlows = emissionFlows
            , imResourceFlows = resourceFlows
            }
        
        statistics = InventoryStatistics
            { isTotalQuantity = totalQuantity
            , isEmissionQuantity = emissionQuantity  
            , isResourceQuantity = resourceQuantity
            , isTopCategories = categoryStats
            }
    
    in InventoryExport metadata flowDetails statistics

-- | Determine if a flow represents resource extraction (negative quantity = extraction from environment)
isResourceExtraction :: Flow -> Double -> Bool
isResourceExtraction flow quantity = quantity < 0 && flowType flow == Biosphere

-- | Simple stats tracking
data TreeStats = TreeStats Int Int Int -- total, loops, leaves

combineStats :: TreeStats -> TreeStats -> TreeStats
combineStats (TreeStats t1 l1 v1) (TreeStats t2 l2 v2) = TreeStats (t1 + t2) (l1 + l2) (v1 + v2)

-- | Proxy for the API
acvAPI :: Proxy ACVAPI
acvAPI = Proxy
