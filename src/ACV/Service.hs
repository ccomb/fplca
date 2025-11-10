{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module ACV.Service where

import ACV.CLI.Types (DebugMatricesOptions (..))
import ACV.Matrix (Inventory, applySparseMatrix, buildDemandVectorFromIndex, computeInventoryMatrix, solveSparseLinearSystem, toList, fromList)
import ACV.Matrix.SharedSolver (SharedSolver, solveWithSharedSolver)
import ACV.Progress
import ACV.Query (findActivitiesByFields, findFlowsBySynonym)
import ACV.Tree (buildLoopAwareTree)
import ACV.Types
import ACV.Types.API (ActivityForAPI (..), ActivityInfo (..), ActivityLinks (..), ActivityMetadata (..), ActivityStats (..), ActivitySummary (..), ExchangeDetail (..), ExchangeWithUnit (..), ExportNode (..), FlowDetail (..), FlowInfo (..), FlowRole (..), FlowSearchResult (..), FlowSummary (..), InventoryExport (..), InventoryFlowDetail (..), InventoryMetadata (..), InventoryStatistics (..), NodeType (..), SearchResults (..), TreeEdge (..), TreeExport (..), TreeMetadata (..))
import Data.Aeson (Value, toJSON)
import Data.Int (Int32)
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import System.IO

-- | Domain service errors
data ServiceError
    = InvalidUUID Text
    | InvalidProcessId Text
    | ActivityNotFound Text
    | FlowNotFound Text
    | MatrixError Text  -- Generic error from matrix computations
    deriving (Show)

-- | Validate UUID format
validateUUID :: Text -> Either ServiceError Text
validateUUID uuidText
    | Just _ <- UUID.fromText uuidText = Right uuidText
    | otherwise = Left $ InvalidUUID $ "Invalid UUID format: " <> uuidText

-- | Parse ProcessId from text (activity_uuid_product_uuid format)
parseProcessIdFromText :: Database -> Text -> Either ServiceError ProcessId
parseProcessIdFromText db text =
    case parseProcessId db text of
        Just processId -> Right processId
        Nothing -> Left $ InvalidProcessId $ "Invalid ProcessId format (expected activity_uuid_product_uuid): " <> text

-- | Find activity by ProcessId using direct Vector access
findActivityByProcessId :: Database -> ProcessId -> Maybe Activity
findActivityByProcessId db processId =
    if processId >= 0 && fromIntegral processId < V.length (dbActivities db)
    then Just $ dbActivities db V.! fromIntegral processId
    else Nothing

-- | Resolve activity query using ProcessId format with UUID fallback for compatibility
resolveActivityByProcessId :: Database -> Text -> Either ServiceError Activity
resolveActivityByProcessId db queryText =
    case resolveActivityAndProcessId db queryText of
        Right (_processId, activity) -> Right activity
        Left err -> Left err

-- | Resolve activity and get both ProcessId and Activity
-- This is the preferred function when you need the ProcessId (e.g., for matrix operations)
resolveActivityAndProcessId :: Database -> Text -> Either ServiceError (ProcessId, Activity)
resolveActivityAndProcessId db queryText =
    case parseProcessIdFromText db queryText of
        Right processId ->
            case findActivityByProcessId db processId of
                Just activity -> Right (processId, activity)
                Nothing -> Left $ ActivityNotFound queryText
        Left _ ->
            -- Fallback: try as bare UUID for ECOINVENT data compatibility
            case UUID.fromText queryText of
                Just uuid ->
                    case findProcessIdByActivityUUID db uuid of
                        Just processId ->
                            case findActivityByProcessId db processId of
                                Just activity -> Right (processId, activity)
                                Nothing -> Left $ ActivityNotFound queryText
                        Nothing -> Left $ InvalidProcessId $ "Query must be ProcessId format (activity_uuid_product_uuid) or valid UUID: " <> queryText
                Nothing -> Left $ InvalidProcessId $ "Invalid UUID format: " <> queryText

-- | Validate that a ProcessId exists in the matrix activity index
-- This check ensures we fail fast with clear error messages before expensive matrix operations
-- The activity index is required for building demand vectors and performing inventory calculations
validateProcessIdInMatrixIndex :: Database -> ProcessId -> Either ServiceError ()
validateProcessIdInMatrixIndex db processId =
    if processId >= 0 && fromIntegral processId < V.length (dbActivityIndex db)
    then Right ()
    else Left $ MatrixError $
        "ProcessId not available for matrix calculations: " <>
        T.pack (show processId) <>
        ". This activity may exist in the database but is not indexed for inventory calculations."


-- | Rich activity info (returns same format as API)
getActivityInfo :: Database -> Text -> Either ServiceError Value
getActivityInfo db queryText = do
    (processId, activity) <- resolveActivityAndProcessId db queryText
    let activityForAPI = convertActivityForAPI db processId activity
        metadata = calculateActivityMetadata db activity
        stats = calculateActivityStats activity
        -- Use ProcessId (which encodes both activityUUID and productUUID) for links
        activityIdForLinks = processIdToText db processId
        links = generateActivityLinks activityIdForLinks
        activityInfo =
            ActivityInfo
                { piActivity = activityForAPI
                , piMetadata = metadata
                , piStatistics = stats
                , piLinks = links
                }
     in Right $ toJSON activityInfo

-- | Core inventory calculation logic using matrix-based LCA calculations
-- | Convert raw inventory to structured export format
convertToInventoryExport :: Database -> ProcessId -> Activity -> Inventory -> InventoryExport
convertToInventoryExport db processId rootActivity inventory =
    let
        -- Filter out flows with zero quantities to reduce noise in the results
        inventoryList = M.toList inventory

        !flowDetails =
            [ InventoryFlowDetail flow quantity unitName isEmission category
            | (flowUUID, quantity) <- inventoryList
            , quantity /= 0  -- Exclude flows with zero quantities
            , Just flow <- [M.lookup flowUUID (dbFlows db)]
            , let !unitName = getUnitNameForFlow (dbUnits db) flow
                  !isEmission = not (isResourceExtraction flow quantity)
                  !category = flowCategory flow
            ]

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
                { imRootActivity =
                    ActivitySummary
                        (processIdToText db processId)
                        (activityName rootActivity)
                        (activityLocation rootActivity)
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
     in
        InventoryExport metadata flowDetails statistics

-- | Determine if a flow represents resource extraction based on flow category
-- Since B matrix now stores all flows as positive (Ecoinvent convention), we use category instead of sign
-- Resource extractions have category starting with "natural resource" (e.g., "natural resource/in ground", "natural resource/in water")
isResourceExtraction :: Flow -> Double -> Bool
isResourceExtraction flow _ =
    flowType flow == Biosphere &&
    ("natural resource" `T.isPrefixOf` T.toLower (flowCategory flow))

-- | Get activity inventory as rich InventoryExport (same as API)
getActivityInventory :: Database -> Text -> Either ServiceError Value
getActivityInventory db processIdText = do
    (processId, activity) <- resolveActivityAndProcessId db processIdText
    -- Validate ProcessId exists in matrix index before expensive computation
    validateProcessIdInMatrixIndex db processId
    -- Matrix computation (will not fail if validation passed)
    let inventory = computeInventoryMatrix db processId
        !inventoryExport = convertToInventoryExport db processId activity inventory
    Right $ toJSON inventoryExport

-- | Shared solver-aware activity inventory export for concurrent processing
getActivityInventoryWithSharedSolver :: SharedSolver -> Database -> Text -> IO (Either ServiceError InventoryExport)
getActivityInventoryWithSharedSolver sharedSolver db processIdText = do
    case resolveActivityAndProcessId db processIdText of
        Left err -> return $ Left err
        Right (processId, activity) -> do
            -- Validate ProcessId exists in matrix index before expensive computation
            case validateProcessIdInMatrixIndex db processId of
                Left validationErr -> return $ Left validationErr
                Right () -> do
                    -- Inline matrix calculation with shared solver to avoid circular dependency
                    let activityCount = dbActivityCount db
                        bioFlowCount = dbBiosphereCount db
                        techTriples = dbTechnosphereTriples db
                        bioTriples = dbBiosphereTriples db
                        activityIndex = dbActivityIndex db
                        bioFlowUUIDs = dbBiosphereFlows db
                        -- Build demand vector (will not fail after validation)
                        demandVec = buildDemandVectorFromIndex activityIndex processId

                    -- Use shared solver with MVar synchronization - this is thread-safe
                    -- The shared solver uses the cached factorization from the database
                    supplyVec <- case dbCachedFactorization db of
                        Just _ -> do
                            -- Use shared solver with cached factorization - thread-safe and fast
                            solveWithSharedSolver sharedSolver demandVec
                        Nothing -> do
                            -- Fallback: use direct matrix computation if no cached factorization
                            -- Convert Int32 to Int for solveSparseLinearSystem
                            let techTriplesInt = [(fromIntegral i, fromIntegral j, v) | SparseTriple i j v <- U.toList techTriples]
                                activityCountInt = fromIntegral activityCount
                            return $ solveSparseLinearSystem techTriplesInt activityCountInt demandVec

                    -- Calculate inventory using sparse biosphere matrix: g = B * supply
                    -- Convert Int32 to Int for applySparseMatrix
                    let bioTriplesInt = [(fromIntegral i, fromIntegral j, v) | SparseTriple i j v <- U.toList bioTriples]
                        bioFlowCountInt = fromIntegral bioFlowCount
                        inventoryVec = applySparseMatrix bioTriplesInt bioFlowCountInt supplyVec
                        inventory = M.fromList $ zip (V.toList bioFlowUUIDs) (toList inventoryVec)
                        inventoryExport = convertToInventoryExport db processId activity inventory
                    return $ Right inventoryExport

-- | Simple stats tracking for tree processing
data TreeStats = TreeStats Int Int Int -- total, loops, leaves

combineStats :: TreeStats -> TreeStats -> TreeStats
combineStats (TreeStats t1 l1 v1) (TreeStats t2 l2 v2) = TreeStats (t1 + t2) (l1 + l2) (v1 + v2)

-- | Helper to find ProcessId for an activity by searching the database
-- This is needed because activities don't store their own ProcessId/UUID
-- Strategy: match activities by name, location, unit, and first reference product flow
findProcessIdForActivity :: Database -> Activity -> Maybe ProcessId
findProcessIdForActivity db activity =
    let actName = activityName activity
        actLoc = activityLocation activity
        actUnit = activityUnit activity
        refFlowId = case [exchangeFlowId ex | ex <- exchanges activity, exchangeIsReference ex] of
            (fid:_) -> Just fid
            [] -> Nothing

        matchesActivity dbActivity =
            let dbRefFlowId = case [exchangeFlowId ex | ex <- exchanges dbActivity, exchangeIsReference ex] of
                    (dbFid:_) -> Just dbFid
                    [] -> Nothing
            in activityName dbActivity == actName &&
               activityLocation dbActivity == actLoc &&
               activityUnit dbActivity == actUnit &&
               dbRefFlowId == refFlowId

        matchingIndex = V.findIndex matchesActivity (dbActivities db)
    in fmap fromIntegral matchingIndex

-- | Helper to get node ID from LoopAwareTree (returns ProcessId format)
-- For activities, we look up their ProcessId; for loops, we use the bare UUID
getTreeNodeId :: Database -> LoopAwareTree -> Text
getTreeNodeId db (TreeLeaf activity) =
    case findProcessIdForActivity db activity of
        Just processId -> processIdToText db processId
        Nothing -> "unknown-activity"  -- Fallback
getTreeNodeId _ (TreeLoop uuid _ _) = UUID.toText uuid -- Loop references converted to Text
getTreeNodeId db (TreeNode activity _) =
    case findProcessIdForActivity db activity of
        Just processId -> processIdToText db processId
        Nothing -> "unknown-activity"  -- Fallback

-- | Count potential children for navigation (technosphere inputs that could be expanded)
countPotentialChildren :: Database -> Activity -> Int
countPotentialChildren db activity =
    length
        [ ex
        | ex <- exchanges activity
        , isTechnosphereExchange ex
        , exchangeIsInput ex
        , not (exchangeIsReference ex)
        , Just targetUUID <- [exchangeActivityLinkId ex]
        , Just _ <- [findProcessIdByActivityUUID db targetUUID]
        ]

-- | Extract nodes and edges from LoopAwareTree
extractNodesAndEdges :: Database -> LoopAwareTree -> Int -> Maybe Text -> M.Map Text ExportNode -> [TreeEdge] -> (M.Map Text ExportNode, [TreeEdge], TreeStats)
extractNodesAndEdges db tree depth parentId nodeAcc edgeAcc = case tree of
    TreeLeaf activity ->
        let childrenCount = countPotentialChildren db activity
            processIdText = getTreeNodeId db tree
            node =
                ExportNode
                    { enId = processIdText -- Now ProcessId format
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
            nodes' = M.insert processIdText node nodeAcc -- Use ProcessId as key
         in (nodes', edgeAcc, TreeStats 1 0 1)
    TreeLoop uuid name depth ->
        let uuidText = UUID.toText uuid
            node =
                ExportNode
                    { enId = uuidText -- Convert UUID to Text for API
                    , enName = name
                    , enDescription = ["Loop reference"]
                    , enLocation = "N/A"
                    , enUnit = "N/A"
                    , enNodeType = LoopNode
                    , enDepth = depth
                    , enLoopTarget = Just uuidText
                    , enParentId = parentId
                    , enChildrenCount = 0 -- Loops don't expand
                    }
            nodes' = M.insert uuidText node nodeAcc
         in (nodes', edgeAcc, TreeStats 1 1 0)
    TreeNode activity children ->
        let childrenCount = countPotentialChildren db activity
            currentProcessId = getTreeNodeId db tree
            parentNode =
                ExportNode
                    { enId = currentProcessId -- Now ProcessId format
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
            nodes' = M.insert currentProcessId parentNode nodeAcc -- Use ProcessId as key
            processChild (quantity, flow, subtree) (nodeAcc, edgeAcc, statsAcc) =
                let (childNodes, childEdges, childStats) = extractNodesAndEdges db subtree (depth + 1) (Just currentProcessId) nodeAcc edgeAcc
                    edge =
                        TreeEdge
                            { teFrom = currentProcessId -- Now ProcessId format
                            , teTo = getTreeNodeId db subtree -- This now returns ProcessId format
                            , teFlow = FlowInfo (flowId flow) (flowName flow) (flowCategory flow)
                            , teQuantity = quantity
                            , teUnit = getUnitNameForFlow (dbUnits db) flow
                            }
                    newStats = combineStats statsAcc childStats
                 in (childNodes, edge : childEdges, newStats)
            (finalNodes, finalEdges, childStats) = foldr processChild (nodes', edgeAcc, TreeStats 1 0 0) children
         in (finalNodes, finalEdges, childStats)

-- | Convert LoopAwareTree to TreeExport format for JSON serialization
convertToTreeExport :: Database -> Text -> Int -> LoopAwareTree -> TreeExport
convertToTreeExport db rootProcessId maxDepth tree =
    let (nodes, edges, stats) = extractNodesAndEdges db tree 0 Nothing M.empty []
        metadata =
            TreeMetadata
                { tmRootId = rootProcessId -- Now ProcessId format
                , tmMaxDepth = maxDepth
                , tmTotalNodes = M.size nodes
                , tmLoopNodes = length [() | (_, node) <- M.toList nodes, enNodeType node == LoopNode]
                , tmLeafNodes = length [() | (_, node) <- M.toList nodes, null [e | e <- edges, teFrom e == enId node]]
                , tmExpandableNodes = length [() | (_, node) <- M.toList nodes, enChildrenCount node > 0]
                }
     in TreeExport metadata nodes edges

-- | Get activity tree as rich TreeExport with configurable depth
getActivityTree :: Database -> Text -> Int -> Either ServiceError Value
getActivityTree db queryText maxDepth = do
    (processId, activity) <- resolveActivityAndProcessId db queryText
    -- Get the activity UUID from the processIdText (which is activityUUID_productUUID)
    let activityUuidText = case T.splitOn "_" queryText of
            (uuid:_) -> uuid
            [] -> queryText  -- Fallback
    case UUID.fromText activityUuidText of
        Just activityUuid ->
            let loopAwareTree = buildLoopAwareTree db activityUuid maxDepth
                treeExport = convertToTreeExport db queryText maxDepth loopAwareTree
             in Right $ toJSON treeExport
        Nothing -> Left $ InvalidUUID $ "Invalid activity UUID: " <> activityUuidText

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
        flowResults = map (\flow -> FlowSearchResult (flowId flow) (flowName flow) (flowCategory flow) (getUnitNameForFlow (dbUnits db) flow) (M.map S.toList (flowSynonyms flow))) pagedResults
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
        activityResults =
            map
                ( \activity ->
                    case findProcessIdForActivity db activity of
                        Just processId ->
                            ActivitySummary
                                (processIdToText db processId)
                                (activityName activity)
                                (activityLocation activity)
                        Nothing ->
                            -- Fallback if ProcessId not found
                            ActivitySummary
                                "unknown"
                                (activityName activity)
                                (activityLocation activity)
                )
                pagedResults
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
-- Note: This function requires the ProcessId to get the activity UUID
convertActivityForAPI :: Database -> ProcessId -> Activity -> ActivityForAPI
convertActivityForAPI db processId activity =
    ActivityForAPI
        { pfaProcessId = processIdToText db processId
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
                TechnosphereExchange _ _ _ _ _ linkId _ ->
                    case findActivityByActivityUUID db linkId of
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
    targetActivity <- findActivityByActivityUUID db targetId
    processId <- findProcessIdForActivity db targetActivity
    return $
        ActivitySummary
            { prsId = processIdToText db processId
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
             in [ ActivitySummary
                    (processIdToText db processId)
                    (activityName proc)
                    (activityLocation proc)
                | procUUID <- uniqueUUIDs
                , Just proc <- [findActivityByActivityUUID db procUUID]
                , Just processId <- [findProcessIdForActivity db proc]
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

-- | Get flow info as JSON (for CLI)
getFlowInfo :: Database -> Text -> Either ServiceError Value
getFlowInfo db flowIdText = do
    case UUID.fromText flowIdText of
        Nothing -> Left $ InvalidUUID $ "Invalid flow UUID: " <> flowIdText
        Just flowId ->
            case M.lookup flowId (dbFlows db) of
                Nothing -> Left $ FlowNotFound flowIdText
                Just flow ->
                    let usageCount = getFlowUsageCount db flowId
                        unitName = getUnitNameForFlow (dbUnits db) flow
                        flowDetail = FlowDetail flow unitName usageCount
                     in Right $ toJSON flowDetail

-- | Get activities that use a specific flow as JSON (for CLI)
getFlowActivities :: Database -> Text -> Either ServiceError Value
getFlowActivities db flowIdText = do
    case UUID.fromText flowIdText of
        Nothing -> Left $ InvalidUUID $ "Invalid flow UUID: " <> flowIdText
        Just flowId ->
            case M.lookup flowId (dbFlows db) of
                Nothing -> Left $ FlowNotFound flowIdText
                Just _ ->
                    let activities = getActivitiesUsingFlow db flowId
                     in Right $ toJSON activities

-- | Get available synonym languages (placeholder)
-- | Compute LCIA scores (placeholder)
computeLCIA :: Database -> Text -> FilePath -> Either ServiceError Value
computeLCIA db queryText methodFile = do
    (processId, activity) <- resolveActivityAndProcessId db queryText
    let placeholder = M.fromList [("method" :: Text, T.pack methodFile), ("activity", processIdToText db processId)]
     in Right $ toJSON placeholder

-- | Export LCIA results as XML (placeholder)
exportLCIAAsXML :: Value -> FilePath -> Either ServiceError ()
exportLCIAAsXML _ _ = Right ()

-- | Export LCIA results as CSV (placeholder)
exportLCIAAsCSV :: Value -> FilePath -> Either ServiceError ()
exportLCIAAsCSV _ _ = Right ()

-- | Export matrix debug data
exportMatrixDebugData :: Database -> Text -> DebugMatricesOptions -> IO (Either ServiceError Value)
exportMatrixDebugData database processIdText opts = do
    case resolveActivityAndProcessId database processIdText of
        Left err -> return $ Left err
        Right (processId, targetActivity) -> do
            -- Extract the activityUUID from processIdText (format: activityUUID_productUUID)
            let activityUuidText = case T.splitOn "_" processIdText of
                    (uuid:_) -> uuid
                    [] -> processIdText  -- Fallback
            case UUID.fromText activityUuidText of
                Nothing -> return $ Left $ InvalidUUID $ "Invalid activity UUID: " <> activityUuidText
                Just activityUuid -> do
                    -- Extract matrix debugging information (includes all computations)
                    let matrixData = extractMatrixDebugInfo database activityUuid (debugFlowFilter opts)

                    -- Get inventory from matrixData (already computed)
                    let inventoryList = mdInventoryVector matrixData
                    let bioFlowUUIDs = mdBioFlowUUIDs matrixData
                    let inventory = M.fromList $ zip (V.toList bioFlowUUIDs) inventoryList

                    -- Proper IO for CSV export
                    putStrLn $ "DEBUG: Starting CSV export to " ++ debugOutput opts
                    exportMatrixDebugCSVs (debugOutput opts) matrixData
                    putStrLn $ "DEBUG: CSV export completed"

                    let summary =
                            M.fromList
                                [ ("activity_uuid" :: Text, UUID.toText activityUuid)
                                , ("activity_name" :: Text, activityName targetActivity)
                                , ("total_inventory_flows" :: Text, T.pack $ show $ M.size inventory)
                                , ("matrix_debug_exported" :: Text, "CSV_EXPORTED")
                                , ("supply_chain_file" :: Text, T.pack $ debugOutput opts ++ "_supply_chain.csv")
                                , ("biosphere_matrix_file" :: Text, T.pack $ debugOutput opts ++ "_biosphere_matrix.csv")
                                ]
                    return $ Right $ toJSON summary

-- | Extract matrix debug information from Database
extractMatrixDebugInfo :: Database -> UUID -> Maybe Text -> MatrixDebugInfo
extractMatrixDebugInfo database targetUUID flowFilter =
    let activities = dbActivities database
        flows = dbFlows database
        techTriples = dbTechnosphereTriples database
        bioTriples = dbBiosphereTriples database
        activityIndexVec = dbActivityIndex database
        bioFlowUUIDs = dbBiosphereFlows database
        activityCount = dbActivityCount database
        bioFlowCount = dbBiosphereCount database

        -- Build demand vector for target activity (f[i] = 1 for target, 0 elsewhere)
        -- Need to find ProcessId from activity UUID - never fabricate
        targetProcessId = case findProcessIdByActivityUUID database targetUUID of
            Just pid -> pid
            Nothing -> error $ "Activity " <> show targetUUID <> " has no ProcessId in database for debug-matrices"
        demandVec = buildDemandVectorFromIndex activityIndexVec targetProcessId
        demandList = toList demandVec

        -- Solve (I - A) * supply = demand using PETSc (same as in computeInventoryMatrix)
        -- Convert Int32 to Int for solveSparseLinearSystem
        techTriplesInt = [(fromIntegral i, fromIntegral j, v) | SparseTriple i j v <- U.toList techTriples]
        activityCountInt = fromIntegral activityCount
        supplyVec = solveSparseLinearSystem techTriplesInt activityCountInt demandVec
        supplyList = toList supplyVec

        -- Calculate inventory using biosphere matrix: g = B * supply
        -- Convert Int32 to Int for applySparseMatrix
        bioTriplesInt = [(fromIntegral i, fromIntegral j, v) | SparseTriple i j v <- U.toList bioTriples]
        bioFlowCountInt = fromIntegral bioFlowCount
        inventoryVec = applySparseMatrix bioTriplesInt bioFlowCountInt supplyVec
        inventoryList = toList inventoryVec

        -- Filter biosphere flows if specified
        filteredBioTriples = case flowFilter of
            Nothing -> bioTriples
            Just filterText ->
                let matchingFlowIndices =
                        [ idx | (uuid, idx) <- zip (V.toList bioFlowUUIDs) [0 ..], Just flow <- [M.lookup uuid flows], T.toLower filterText `T.isInfixOf` T.toLower (flowName flow)
                        ]
                    matchingFlowIndicesInt32 = map fromIntegral matchingFlowIndices :: [Int32]
                 in U.filter (\(SparseTriple row _ _) -> row `elem` matchingFlowIndicesInt32) bioTriples
     in MatrixDebugInfo
            { mdActivities = activities
            , mdFlows = flows
            , mdTechTriples = techTriples
            , mdBioTriples = filteredBioTriples
            , mdActivityIndex = activityIndexVec
            , mdBioFlowUUIDs = bioFlowUUIDs
            , mdTargetUUID = targetUUID
            , mdTargetProcessId = targetProcessId
            , mdDatabase = database
            , mdSupplyVector = supplyList
            , mdDemandVector = demandList
            , mdInventoryVector = inventoryList
            }

-- | Matrix debug information container
data MatrixDebugInfo = MatrixDebugInfo
    { mdActivities :: ActivityDB  -- V.Vector Activity indexed by ProcessId
    , mdFlows :: M.Map UUID Flow
    , mdTechTriples :: U.Vector SparseTriple
    , mdBioTriples :: U.Vector SparseTriple
    , mdActivityIndex :: V.Vector Int32  -- ProcessId → matrix column index
    , mdBioFlowUUIDs :: V.Vector UUID
    , mdTargetUUID :: UUID
    , mdTargetProcessId :: ProcessId  -- The ProcessId for the target activity
    , mdDatabase :: Database  -- Database reference for lookups
    , mdSupplyVector :: [Double] -- Real supply amounts from PETSc solver
    , mdDemandVector :: [Double] -- Demand vector (f)
    , mdInventoryVector :: [Double] -- Final inventory vector (g)
    }

-- | Export matrix debug CSVs
exportMatrixDebugCSVs :: FilePath -> MatrixDebugInfo -> IO ()
exportMatrixDebugCSVs basePath debugInfo = do
    let supplyChainPath = basePath ++ "_supply_chain.csv"
        biosphereMatrixPath = basePath ++ "_biosphere_matrix.csv"

    putStrLn $ "DEBUG: exportMatrixDebugCSVs called with basePath: " ++ basePath
    putStrLn $ "DEBUG: Will create files: " ++ supplyChainPath ++ " and " ++ biosphereMatrixPath

    -- Debug demand vector
    let demandVector = mdDemandVector debugInfo
        targetProcessId = mdTargetProcessId debugInfo
        activityIndexVec = mdActivityIndex debugInfo
        targetIndex = fromIntegral (activityIndexVec V.! fromEnum targetProcessId) :: Int
        targetDemand =
            if targetIndex >= 0 && targetIndex < length demandVector
                then demandVector !! targetIndex
                else -999.0
    putStrLn $ "DEBUG: Target activity index: " ++ show targetIndex
    putStrLn $ "DEBUG: Target demand value: " ++ show targetDemand
    putStrLn $ "DEBUG: Demand vector length: " ++ show (length demandVector)
    putStrLn $ "DEBUG: Non-zero demand entries: " ++ show (length $ filter (/= 0.0) demandVector)

    -- Export supply chain activities (from technosphere matrix)
    putStrLn "DEBUG: Calling exportSupplyChainData"
    exportSupplyChainData supplyChainPath debugInfo

    -- Export biosphere matrix contributions
    putStrLn "DEBUG: Calling exportBiosphereMatrixData"
    exportBiosphereMatrixData biosphereMatrixPath debugInfo

    putStrLn "DEBUG: Both CSV exports completed"

-- | Export supply chain data showing which activities contribute to target
exportSupplyChainData :: FilePath -> MatrixDebugInfo -> IO ()
exportSupplyChainData filePath debugInfo = do
    let database = mdDatabase debugInfo
        activities = mdActivities debugInfo
        activityIndexVec = mdActivityIndex debugInfo
        supplyVector = mdSupplyVector debugInfo
        targetUUID = mdTargetUUID debugInfo

        -- Create supply chain data with REAL supply amounts from solver
        supplyChainRows =
            [ csvRow processId activity idx supply
            | processId <- [toEnum 0 .. toEnum (V.length activities - 1)]
            , let activity = activities V.! fromEnum processId
            , let idx = fromIntegral (activityIndexVec V.! fromEnum processId) :: Int
            , let supply = if idx < length supplyVector then supplyVector !! idx else 0.0
            ]

        csvRow processId activity idx supply =
            [ T.unpack (processIdToText database processId)
            , T.unpack (activityName activity)
            , T.unpack (activityLocation activity)
            , show supply -- REAL supply amount from PETSc solver
            , show idx
            ]

        csvHeader = ["activity_id", "activity_name", "location", "supply_amount", "col_idx"]
        allRows = csvHeader : supplyChainRows
        csvContent = L.intercalate "\n" (map (L.intercalate ",") allRows)

    putStrLn $ "DEBUG: Writing supply chain data with " ++ show (length supplyChainRows) ++ " activities"
    putStrLn $ "DEBUG: Supply vector length: " ++ show (length supplyVector)
    writeFile filePath csvContent

-- | Export biosphere matrix contributions
exportBiosphereMatrixData :: FilePath -> MatrixDebugInfo -> IO ()
exportBiosphereMatrixData filePath debugInfo = do
    let database = mdDatabase debugInfo
        flows = mdFlows debugInfo
        activities = mdActivities debugInfo
        bioTriples = mdBioTriples debugInfo
        bioFlowUUIDs = mdBioFlowUUIDs debugInfo
        activityIndex = mdActivityIndex debugInfo
        supplyVector = mdSupplyVector debugInfo

        -- Convert matrix triplets to CSV rows with REAL contributions
        matrixRows =
            [ csvRow bioTriple
            | bioTriple@(SparseTriple row col value) <- U.toList bioTriples
            , abs value > 1e-20 -- Filter very small values
            ]

        csvRow (SparseTriple row col value) =
            let rowInt = fromIntegral row :: Int
                colInt = fromIntegral col :: Int
             in [ maybe "unknown" show (getFlowUUID rowInt)
                , maybe "unknown" (T.unpack . flowName) (getFlow rowInt)
                , maybe "unknown" T.unpack (getFlowUnit rowInt)
                , maybe "unknown" (T.unpack . processIdToText database) (getActivityProcessId colInt)
                , maybe "unknown" (T.unpack . activityName) (getActivity colInt)
                , show value
                , show (realContribution colInt) -- REAL contribution = matrix_value × supply_amount
                ]
          where
            getFlowUUID :: Int -> Maybe UUID
            getFlowUUID rowIdx =
                if rowIdx < V.length bioFlowUUIDs
                    then Just (bioFlowUUIDs V.! rowIdx)
                    else Nothing
            getFlow :: Int -> Maybe Flow
            getFlow rowIdx = getFlowUUID rowIdx >>= flip M.lookup flows
            getFlowUnit :: Int -> Maybe Text
            getFlowUnit rowIdx = fmap (getUnitNameForFlow (dbUnits database)) (getFlow rowIdx)
            -- Find ProcessId by searching for matching matrix column index
            getActivityProcessId :: Int -> Maybe ProcessId
            getActivityProcessId colIdx =
                L.find (\pid -> fromIntegral (activityIndex V.! fromEnum pid) == colIdx)
                       [toEnum 0 .. toEnum (V.length activities - 1)]
            getActivity :: Int -> Maybe Activity
            getActivity colIdx = do
                processId <- getActivityProcessId colIdx
                if fromEnum processId < V.length activities
                    then Just (activities V.! fromEnum processId)
                    else Nothing
            -- Calculate REAL contribution: B[row,col] × supply[col]
            realContribution :: Int -> Double
            realContribution colIdx =
                if colIdx < length supplyVector
                    then value * (supplyVector !! colIdx)
                    else 0.0

        csvHeader = ["flow_id", "flow_name", "unit", "activity_id", "activity_name", "matrix_value", "contribution"]
        allRows = csvHeader : matrixRows
        csvContent = L.intercalate "\n" (map (L.intercalate ",") allRows)

    putStrLn $ "DEBUG: Writing biosphere matrix with " ++ show (length matrixRows) ++ " entries"
    writeFile filePath csvContent
-- | Export matrices in universal matrix format (Ecoinvent-compatible)
-- Creates 4 CSV files: ie_index.csv, ee_index.csv, A_public.csv, B_public.csv
exportUniversalMatrixFormat :: FilePath -> Database -> IO ()
exportUniversalMatrixFormat outputDir db = do
    putStrLn $ "Exporting matrices to universal format in: " ++ outputDir

    -- Export the 4 required files
    exportIEIndex (outputDir ++ "/ie_index.csv") db
    exportEEIndex (outputDir ++ "/ee_index.csv") db
    exportAMatrix (outputDir ++ "/A_public.csv") db
    exportBMatrix (outputDir ++ "/B_public.csv") db

    putStrLn "Universal matrix export completed"

-- | Escape text for CSV output (semicolon delimiter)
-- Wraps fields containing semicolons or quotes in quotes, doubles internal quotes
escapeCsvField :: Text -> Text
escapeCsvField text
    | T.any (\c -> c == ';' || c == '"' || c == '\n' || c == '\r') text =
        "\"" <> T.replace "\"" "\"\"" text <> "\""
    | otherwise = text

-- | Export ie_index.csv (Intermediate Exchanges - processes/activities)
-- Format: activityName;geography;product;unitName;index
exportIEIndex :: FilePath -> Database -> IO ()
exportIEIndex filePath db = do
    let activities = dbActivities db
        processIdTable = dbProcessIdTable db
        flows = dbFlows db

        -- Create index entries for each activity
        rows = V.toList $ V.imap (\idx (actUuid, prodUuid) ->
                let activity = activities V.! idx
                    -- Find reference product name
                    refProduct = case [ex | ex <- exchanges activity, exchangeIsReference ex] of
                        (ex:_) -> case M.lookup (exchangeFlowId ex) flows of
                            Just flow -> flowName flow
                            Nothing -> T.pack (show prodUuid)
                        [] -> T.pack (show prodUuid)
                    unit = activityUnit activity
                in escapeCsvField (activityName activity) <> ";" <>
                   escapeCsvField (activityLocation activity) <> ";" <>
                   escapeCsvField refProduct <> ";" <>
                   escapeCsvField unit <> ";" <>
                   T.pack (show idx)
            ) processIdTable

        header = "activityName;geography;product;unitName;index"
        content = T.unlines (header : rows)

    TIO.writeFile filePath content
    putStrLn $ "Exported " ++ show (length rows) ++ " activities to " ++ filePath

-- | Export ee_index.csv (Elementary Exchanges - biosphere flows)
-- Format: name;compartment;subcompartment;unitName;index
exportEEIndex :: FilePath -> Database -> IO ()
exportEEIndex filePath db = do
    let bioFlowUUIDs = dbBiosphereFlows db
        flows = dbFlows db

        -- Create index entries for each biosphere flow
        rows = zipWith (\flowUuid idx ->
                case M.lookup flowUuid flows of
                    Just flow ->
                        let category = flowCategory flow
                            -- Parse category into compartment/subcompartment
                            -- Format is typically "compartment/subcompartment" or just "compartment"
                            (compartment, subcompartment) = case T.splitOn "/" category of
                                [] -> ("unspecified", "")
                                [c] -> (T.strip c, "")
                                (c:s:_) -> (T.strip c, T.strip s)
                            unit = UUID.toText $ flowUnitId flow
                        in escapeCsvField (flowName flow) <> ";" <>
                           escapeCsvField compartment <> ";" <>
                           escapeCsvField subcompartment <> ";" <>
                           escapeCsvField unit <> ";" <>
                           T.pack (show idx)
                    Nothing ->
                        escapeCsvField (T.pack (show flowUuid)) <> ";unknown;;;" <> T.pack (show idx)
            ) (V.toList bioFlowUUIDs) [0..]

        header = "name;compartment;subcompartment;unitName;index"
        content = T.unlines (header : rows)

    TIO.writeFile filePath content
    putStrLn $ "Exported " ++ show (length rows) ++ " biosphere flows to " ++ filePath

-- | Export A_public.csv (Technosphere Matrix)
-- Format: row;column;coefficient;uncertainty type;varianceWithPedigreeUncertainty;minValue;mostLikelyValue;maxValue
exportAMatrix :: FilePath -> Database -> IO ()
exportAMatrix filePath db = do
    let techTriples = dbTechnosphereTriples db
        activityCount = dbActivityCount db

        -- Add diagonal entries (identity matrix part)
        diagonalEntries = [T.pack $ show i ++ ";" ++ show i ++ ";1.0;;;;;"
                          | i <- [0..fromIntegral activityCount - 1]]

        -- Convert off-diagonal triplets to CSV rows
        -- Ecoinvent format exports (I-A), not A, so negate the positive triplets
        -- techTriples contain positive input coefficients, negate to get (I-A) format
        offDiagonalRows = U.foldr (\(SparseTriple row col value) acc ->
                let rowStr = T.pack $ show row ++ ";" ++ show col ++ ";" ++
                            show (-value) ++ ";;;;;"  -- Negate to export (I-A) format
                in rowStr : acc
            ) [] techTriples

        header = "row;column;coefficient;uncertainty type;varianceWithPedigreeUncertainty;minValue;mostLikelyValue;maxValue"
        -- Combine diagonal and off-diagonal entries
        allRows = diagonalEntries ++ offDiagonalRows
        content = T.unlines (header : allRows)

    TIO.writeFile filePath content
    putStrLn $ "Exported technosphere matrix with " ++ show (length diagonalEntries) ++
               " diagonal + " ++ show (U.length techTriples) ++ " off-diagonal entries to " ++ filePath

-- | Export B_public.csv (Biosphere Matrix)
-- Format: row;column;coefficient;uncertainty type;varianceWithPedigreeUncertainty;minValue;mostLikelyValue;maxValue
exportBMatrix :: FilePath -> Database -> IO ()
exportBMatrix filePath db = do
    let bioTriples = dbBiosphereTriples db

        -- Convert triplets to CSV rows
        -- Keep original signs (no negation needed for biosphere matrix)
        rows = U.foldr (\(SparseTriple row col value) acc ->
                let rowStr = T.pack $ show row ++ ";" ++ show col ++ ";" ++
                            show value ++ ";;;;;"
                in rowStr : acc
            ) [] bioTriples

        header = "row;column;coefficient;uncertainty type;varianceWithPedigreeUncertainty;minValue;mostLikelyValue;maxValue"
        content = T.unlines (header : rows)

    TIO.writeFile filePath content
    putStrLn $ "Exported biosphere matrix with " ++ show (U.length bioTriples) ++ " entries to " ++ filePath
