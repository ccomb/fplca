{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module ACV.Service where

import ACV.Inventory (Inventory, computeInventoryFromLoopAwareTree, computeInventoryWithFlows)
import ACV.Matrix (computeInventoryMatrix)
import ACV.Query (findActivitiesByFields, findFlowsBySynonym)
import ACV.Tree (buildActivityTreeWithDatabase, buildCutoffLoopAwareTree, buildLoopAwareTree)
import ACV.Types
import ACV.Types.API (ActivityForAPI (..), ActivityInfo (..), ActivityLinks (..), ActivityMetadata (..), ActivityStats (..), ActivitySummary (..), ExchangeDetail (..), ExchangeWithUnit (..), ExportNode (..), FlowDetail (..), FlowInfo (..), FlowRole (..), FlowSearchResult (..), FlowSummary (..), InventoryExport (..), InventoryFlowDetail (..), InventoryMetadata (..), InventoryStatistics (..), NodeType (..), SearchResults (..), TreeEdge (..), TreeExport (..), TreeMetadata (..))
import Data.Aeson (Value, toJSON)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Debug.Trace (trace)

-- | Domain service errors
data ServiceError
    = InvalidUUID Text
    | ActivityNotFound Text
    | FlowNotFound Text
    deriving (Show)

-- | Validate UUID format
validateUUID :: Text -> Either ServiceError Text
validateUUID uuidText
    | Just _ <- UUID.fromText uuidText = Right uuidText
    | otherwise = Left $ InvalidUUID $ "Invalid UUID format: " <> uuidText

-- | Rich activity info (returns same format as API)
getActivityInfo :: Database -> Text -> Either ServiceError Value
getActivityInfo db uuid = do
    validUuid <- validateUUID uuid
    case M.lookup validUuid (dbActivities db) of
        Nothing -> Left $ ActivityNotFound validUuid
        Just activity ->
            let activityForAPI = convertActivityForAPI db activity
                metadata = calculateActivityMetadata db activity
                stats = calculateActivityStats activity
                links = generateActivityLinks uuid
                activityInfo =
                    ActivityInfo
                        { piActivity = activityForAPI
                        , piMetadata = metadata
                        , piStatistics = stats
                        , piLinks = links
                        }
             in Right $ toJSON activityInfo

-- | Core inventory calculation logic (pure domain function)
computeActivityInventory :: Database -> Text -> Either ServiceError Inventory
computeActivityInventory db uuid = do
    let _ = trace ("SERVICE: computeActivityInventory starting for UUID: " ++ T.unpack uuid) ()
    let _ = trace ("SERVICE: About to validate UUID") ()
    validUuid <- validateUUID uuid
    let _ = trace ("SERVICE: UUID validation returned: " ++ show validUuid) ()
    let _ = trace ("SERVICE: computeActivityInventory UUID validation completed") ()
    
    case M.lookup validUuid (dbActivities db) of
        Nothing -> 
            let _ = trace ("SERVICE: computeActivityInventory - Activity not found") ()
            in Left $ ActivityNotFound validUuid
        Just _ ->
            let _ = trace ("SERVICE: computeActivityInventory - Activity found, calling computeInventoryMatrix") ()
                _ = trace ("SERVICE: About to call computeInventoryMatrix with UUID: " ++ show validUuid) ()
                -- Use matrix-based calculation for proper cycle resolution
                !inventory = computeInventoryMatrix db validUuid
                _ = trace ("SERVICE: computeInventoryMatrix returned, inventory size: " ++ show (M.size inventory)) ()
                _ = trace ("SERVICE: computeInventoryMatrix call completed successfully") ()
             in Right inventory

-- | Convert raw inventory to structured export format
convertToInventoryExport :: Database -> Activity -> Inventory -> Int -> InventoryExport
convertToInventoryExport db rootActivity inventory calculationDepth =
    let _ = trace ("EXPORT: Starting conversion with " ++ show (M.size inventory) ++ " inventory flows") ()
        inventoryList = take 10 $ M.toList inventory  -- Limit inventory to first 10 items
        _ = trace ("EXPORT: Limited inventory to " ++ show (length inventoryList) ++ " items") ()
        
        !flowDetails = 
            [ let _ = trace ("EXPORT: Processing flow " ++ show i ++ "/" ++ show (length inventoryList)) ()
                  !flow = f
                  _ = trace ("EXPORT: Flow lookup successful") ()
                  !unitName = getUnitNameForFlow (dbUnits db) flow
                  _ = trace ("EXPORT: Unit name retrieved") ()  
                  !isEmission = not (isResourceExtraction flow quantity)
                  _ = trace ("EXPORT: Emission status computed") ()
                  !category = flowCategory flow
                  _ = trace ("EXPORT: Category retrieved") ()
              in InventoryFlowDetail flow quantity unitName isEmission category
            | ((flowUUID, quantity), i) <- zip inventoryList [1..]
            , Just f <- [M.lookup flowUUID (dbFlows db)]
            ]
        _ = trace ("EXPORT: Created " ++ show (length flowDetails) ++ " flow details") ()

        !emissionFlows = length [f | f <- flowDetails, ifdIsEmission f]
        !resourceFlows = length [f | f <- flowDetails, not (ifdIsEmission f)]

        !totalQuantity = sum [abs (ifdQuantity f) | f <- flowDetails]
        !emissionQuantity = sum [ifdQuantity f | f <- flowDetails, ifdIsEmission f, ifdQuantity f > 0]
        !resourceQuantity = sum [abs (ifdQuantity f) | f <- flowDetails, not (ifdIsEmission f)]

        !categoryStats =
            take 10 $
                M.toList $
                    M.fromListWith (+) [(ifdCategory f, 1) | f <- flowDetails]

        !metadata =
            InventoryMetadata
                { imRootActivity = ActivitySummary (activityId rootActivity) (activityName rootActivity) (activityLocation rootActivity)
                , imCalculationDepth = calculationDepth -- Actual calculation depth used
                , imTotalFlows = length flowDetails
                , imEmissionFlows = emissionFlows
                , imResourceFlows = resourceFlows
                }

        !statistics =
            InventoryStatistics
                { isTotalQuantity = totalQuantity
                , isEmissionQuantity = emissionQuantity
                , isResourceQuantity = resourceQuantity
                , isTopCategories = categoryStats
                }
        _ = trace ("EXPORT: Completed all calculations, returning InventoryExport") ()
     in InventoryExport metadata flowDetails statistics

-- | Determine if a flow represents resource extraction (negative quantity = extraction from environment)
isResourceExtraction :: Flow -> Double -> Bool
isResourceExtraction flow quantity = quantity < 0 && flowType flow == Biosphere

-- | Get activity inventory as rich InventoryExport (same as API)
getActivityInventory :: Database -> Text -> Either ServiceError Value
getActivityInventory db uuid = 
    let _ = trace ("SERVICE: getActivityInventory ENTRY POINT - function called with UUID: " ++ T.unpack uuid) ()
        _ = trace ("SERVICE: Testing UUID validation only") ()
    in case validateUUID uuid of
        Left err -> 
            let _ = trace ("SERVICE: UUID validation failed") ()
            in Left err
        Right validUuid ->
            let _ = trace ("SERVICE: UUID validation succeeded") ()
                _ = trace ("SERVICE: Testing database lookup") ()
            in case M.lookup validUuid (dbActivities db) of
                Nothing -> 
                    let _ = trace ("SERVICE: Activity not found in database") ()
                    in Left $ ActivityNotFound validUuid
                Just activity ->
                    let _ = trace ("SERVICE: Activity found in database, starting matrix computation") ()
                        !inventory = computeInventoryMatrix db validUuid
                        _ = trace ("SERVICE: Matrix computation completed successfully") ()
                        _ = trace ("SERVICE: Inventory size: " ++ show (M.size inventory)) ()
                        _ = trace ("SERVICE: Converting to export format...") ()
                        !inventoryExport = convertToInventoryExport db activity inventory 35
                        _ = trace ("SERVICE: Export format conversion completed") ()
                    in Right $ toJSON inventoryExport

-- | Simple stats tracking for tree processing
data TreeStats = TreeStats Int Int Int -- total, loops, leaves

combineStats :: TreeStats -> TreeStats -> TreeStats
combineStats (TreeStats t1 l1 v1) (TreeStats t2 l2 v2) = TreeStats (t1 + t2) (l1 + l2) (v1 + v2)

-- | Helper to get node ID from LoopAwareTree
getTreeNodeId :: LoopAwareTree -> UUID
getTreeNodeId (TreeLeaf activity) = activityId activity
getTreeNodeId (TreeLoop uuid _ _) = uuid
getTreeNodeId (TreeNode activity _) = activityId activity

-- | Count potential children for navigation (technosphere inputs that could be expanded)
countPotentialChildren :: Database -> Activity -> Int
countPotentialChildren db activity =
    length
        [ ex | ex <- exchanges activity, isTechnosphereExchange ex, exchangeIsInput ex, not (exchangeIsReference ex), Just targetUUID <- [exchangeActivityLinkId ex], M.member targetUUID (dbActivities db)
        ]

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
                    , enChildrenCount = 0 -- Loops don't expand
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

-- | Get activity tree as rich TreeExport (same as API)
getActivityTree :: Database -> Text -> Either ServiceError Value
getActivityTree db uuid = do
    validUuid <- validateUUID uuid
    case M.lookup validUuid (dbActivities db) of
        Nothing -> Left $ ActivityNotFound validUuid
        Just _ ->
            let maxDepth = 3 -- Match API depth
                loopAwareTree = buildLoopAwareTree db validUuid maxDepth
                treeExport = convertToTreeExport db validUuid maxDepth loopAwareTree
             in Right $ toJSON treeExport

-- | Get flow usage count across all activities
getFlowUsageCount :: Database -> UUID -> Int
getFlowUsageCount db flowUUID =
    case M.lookup flowUUID (idxByFlow $ dbIndexes db) of
        Nothing -> 0
        Just activityUUIDs -> length activityUUIDs

-- | Get flows used by an activity as lightweight summaries
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

-- | Get activity flows as JSON (for CLI)
getActivityFlows :: Database -> Text -> Either ServiceError Value
getActivityFlows db uuid = do
    validUuid <- validateUUID uuid
    case M.lookup validUuid (dbActivities db) of
        Nothing -> Left $ ActivityNotFound validUuid
        Just activity ->
            let flowSummaries = getActivityFlowSummaries db activity
             in Right $ toJSON flowSummaries

-- | Search flows (returns same format as API)
searchFlows :: Database -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Either ServiceError Value
searchFlows _ Nothing _ _ _ = Right $ toJSON $ SearchResults ([] :: [FlowSearchResult]) 0 0 50 False
searchFlows db (Just query) langParam limitParam offsetParam =
    let lang = maybe "en" id langParam
        limit = maybe 50 (min 1000) limitParam
        offset = maybe 0 (max 0) offsetParam
        allResults = findFlowsBySynonym db query
        total = length allResults
        pagedResults = take limit $ drop offset allResults
        hasMore = offset + limit < total
        flowResults = map (\flow -> FlowSearchResult (flowId flow) (flowName flow) (flowCategory flow) (getUnitNameForFlow (dbUnits db) flow)) pagedResults
     in Right $ toJSON $ SearchResults flowResults total offset limit hasMore

-- | Search activities (returns same format as API)
searchActivities :: Database -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Either ServiceError Value
searchActivities db nameParam geoParam productParam limitParam offsetParam =
    let limit = maybe 50 (min 1000) limitParam
        offset = maybe 0 (max 0) offsetParam
        allResults = findActivitiesByFields db nameParam geoParam productParam
        total = length allResults
        pagedResults = take limit $ drop offset allResults
        hasMore = offset + limit < total
        activityResults = map (\activity -> ActivitySummary (activityId activity) (activityName activity) (activityLocation activity)) pagedResults
     in Right $ toJSON $ SearchResults activityResults total offset limit hasMore

-- | Calculate extended metadata for an activity
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

-- | Generate links to sub-resources for an activity
generateActivityLinks :: Text -> ActivityLinks
generateActivityLinks uuid =
    ActivityLinks
        { plFlowsUrl = "/api/v1/activity/" <> uuid <> "/flows"
        , plInputsUrl = "/api/v1/activity/" <> uuid <> "/inputs"
        , plOutputsUrl = "/api/v1/activity/" <> uuid <> "/outputs"
        , plReferenceProductUrl = Just ("/api/v1/activity/" <> uuid <> "/reference-product")
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
        let flowInfo = M.lookup (exchangeFlowId exchange) (dbFlows db)
            targetActivityInfo = case exchange of
                TechnosphereExchange _ _ _ _ _ linkId ->
                    case M.lookup linkId (dbActivities db) of
                        Just targetActivity -> Just (activityName targetActivity)
                        Nothing -> Nothing
                BiosphereExchange _ _ _ _ -> Nothing
         in ExchangeWithUnit
                { ewuExchange = exchange
                , ewuUnitName = getUnitNameForExchange (dbUnits db) exchange
                , ewuFlowName = maybe "unknown" flowName flowInfo
                , ewuFlowCategory = case flowInfo of
                    Just flow -> case isTechnosphereExchange exchange of
                        True -> "technosphere"
                        False -> flowCategory flow -- This will now include compartment info like "water/ground-, long-term"
                    Nothing -> "unknown"
                , ewuTargetActivity = targetActivityInfo
                }

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

-- | Helper function to get detailed exchanges with filtering
getActivityExchangeDetails :: Database -> Activity -> (Exchange -> Bool) -> [ExchangeDetail]
getActivityExchangeDetails db activity filterFn =
    [ ExchangeDetail exchange flow (getUnitNameForFlow (dbUnits db) flow) unit (getUnitNameForExchange (dbUnits db) exchange) (getTargetActivity db exchange)
    | exchange <- exchanges activity
    , filterFn exchange
    , Just flow <- [M.lookup (exchangeFlowId exchange) (dbFlows db)]
    , Just unit <- [M.lookup (exchangeUnitId exchange) (dbUnits db)]
    ]

-- | Get detailed input exchanges
getActivityInputDetails :: Database -> Activity -> [ExchangeDetail]
getActivityInputDetails db activity = getActivityExchangeDetails db activity exchangeIsInput

-- | Get detailed output exchanges
getActivityOutputDetails :: Database -> Activity -> [ExchangeDetail]
getActivityOutputDetails db activity = getActivityExchangeDetails db activity (not . exchangeIsInput)
