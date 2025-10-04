{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module ACV.Service where

import ACV.CLI.Types (DebugMatricesOptions (..))
import ACV.Matrix (Inventory, applySparseMatrix, buildDemandVectorFromIndex, computeInventoryMatrix, solveSparseLinearSystem, toList)
import ACV.Progress
import ACV.Query (findActivitiesByFields, findFlowsBySynonym)
import ACV.Tree (buildActivityTreeWithDatabase, buildCutoffLoopAwareTree, buildLoopAwareTree)
import ACV.Types
import ACV.Types.API (ActivityForAPI (..), ActivityInfo (..), ActivityLinks (..), ActivityMetadata (..), ActivityStats (..), ActivitySummary (..), ExchangeDetail (..), ExchangeWithUnit (..), ExportNode (..), FlowDetail (..), FlowInfo (..), FlowRole (..), FlowSearchResult (..), FlowSummary (..), InventoryExport (..), InventoryFlowDetail (..), InventoryMetadata (..), InventoryStatistics (..), NodeType (..), SearchResults (..), TreeEdge (..), TreeExport (..), TreeMetadata (..))
import Data.Aeson (Value, toJSON)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.Vector.Unboxed as U
import System.IO
import System.IO.Unsafe (unsafePerformIO)

-- | Domain service errors
data ServiceError
    = InvalidUUID Text
    | InvalidProcessId Text
    | ActivityNotFound Text
    | FlowNotFound Text
    deriving (Show)

-- | Validate UUID format
validateUUID :: Text -> Either ServiceError Text
validateUUID uuidText
    | Just _ <- UUID.fromText uuidText = Right uuidText
    | otherwise = Left $ InvalidUUID $ "Invalid UUID format: " <> uuidText

-- | Parse ProcessId from text (activity_uuid_product_uuid format)
parseProcessIdFromText :: Text -> Either ServiceError ProcessId
parseProcessIdFromText text =
    case parseProcessId text of
        Just processId -> Right processId
        Nothing -> Left $ InvalidProcessId $ "Invalid ProcessId format (expected activity_uuid_product_uuid): " <> text

-- | Find activity by ProcessId by searching through all activities
findActivityByProcessId :: Database -> ProcessId -> Maybe Activity
findActivityByProcessId db processId =
    let allActivities = M.elems (dbActivities db)
        matchingActivities = filter (\activity -> activityProcessId activity == Just processId) allActivities
    in case matchingActivities of
        (activity:_) -> Just activity
        [] -> Nothing

-- | Resolve query input - either UUID or ProcessId format
resolveActivityQuery :: Database -> Text -> Either ServiceError Activity
resolveActivityQuery db queryText
    -- First try as UUID (backward compatibility)
    | Right validUuid <- validateUUID queryText =
        case M.lookup validUuid (dbActivities db) of
            Just activity -> Right activity
            Nothing -> Left $ ActivityNotFound queryText
    -- If not a valid UUID, try as ProcessId
    | Right processId <- parseProcessIdFromText queryText =
        case findActivityByProcessId db processId of
            Just activity -> Right activity
            Nothing -> Left $ ActivityNotFound queryText
    -- If neither UUID nor ProcessId format, return error
    | otherwise = Left $ InvalidUUID $ "Query must be either a valid UUID or ProcessId (activity_uuid_product_uuid): " <> queryText

-- | Rich activity info (returns same format as API)
getActivityInfo :: Database -> Text -> Either ServiceError Value
getActivityInfo db queryText = do
    activity <- resolveActivityQuery db queryText
    let activityForAPI = convertActivityForAPI db activity
        metadata = calculateActivityMetadata db activity
        stats = calculateActivityStats activity
        links = generateActivityLinks (activityId activity)  -- Use actual activity UUID for links
        activityInfo =
            ActivityInfo
                { piActivity = activityForAPI
                , piMetadata = metadata
                , piStatistics = stats
                , piLinks = links
                }
     in Right $ toJSON activityInfo

-- | Core inventory calculation logic using matrix-based LCA calculations
computeActivityInventory :: Database -> Text -> Either ServiceError Inventory
computeActivityInventory db queryText = do
    activity <- resolveActivityQuery db queryText
    let !inventory = computeInventoryMatrix db (activityId activity)
     in Right inventory

-- | Convert raw inventory to structured export format
convertToInventoryExport :: Database -> Activity -> Inventory -> Int -> InventoryExport
convertToInventoryExport db rootActivity inventory calculationDepth =
    let
        -- Include all flows from inventory calculation (no filtering)
        inventoryList = M.toList inventory

        !flowDetails =
            [ InventoryFlowDetail flow quantity unitName isEmission category
            | (flowUUID, quantity) <- inventoryList
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
     in
        InventoryExport metadata flowDetails statistics

-- | Determine if a flow represents resource extraction (negative quantity = extraction from environment)
isResourceExtraction :: Flow -> Double -> Bool
isResourceExtraction flow quantity = quantity < 0 && flowType flow == Biosphere

-- | Get activity inventory as rich InventoryExport (same as API)
getActivityInventory :: Database -> Text -> Either ServiceError Value
getActivityInventory db uuid = do
    validUuid <- validateUUID uuid
    case M.lookup validUuid (dbActivities db) of
        Nothing -> Left $ ActivityNotFound validUuid
        Just activity ->
            let !inventory = computeInventoryMatrix db validUuid
                !inventoryExport = convertToInventoryExport db activity inventory 35
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

-- | Get activity tree as rich TreeExport with configurable depth
getActivityTree :: Database -> Text -> Int -> Either ServiceError Value
getActivityTree db queryText maxDepth = do
    activity <- resolveActivityQuery db queryText
    let activityUuid = activityId activity
        loopAwareTree = buildLoopAwareTree db activityUuid maxDepth
        treeExport = convertToTreeExport db activityUuid maxDepth loopAwareTree
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
getActivityFlows db queryText = do
    activity <- resolveActivityQuery db queryText
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
                TechnosphereExchange _ _ _ _ _ linkId _ ->
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

-- | Get activity inputs as JSON (for CLI)
getActivityInputs :: Database -> Text -> Either ServiceError Value
getActivityInputs db queryText = do
    activity <- resolveActivityQuery db queryText
    let inputDetails = getActivityInputDetails db activity
     in Right $ toJSON inputDetails

-- | Get activity outputs as JSON (for CLI)
getActivityOutputs :: Database -> Text -> Either ServiceError Value
getActivityOutputs db queryText = do
    activity <- resolveActivityQuery db queryText
    let outputDetails = getActivityOutputDetails db activity
     in Right $ toJSON outputDetails

-- | Get activity reference product as JSON (for CLI)
getActivityReferenceProduct :: Database -> Text -> Either ServiceError Value
getActivityReferenceProduct db queryText = do
    activity <- resolveActivityQuery db queryText
    case getActivityReferenceProductDetail db activity of
        Nothing -> Right $ toJSON (Nothing :: Maybe FlowDetail)
        Just refProduct -> Right $ toJSON refProduct

-- | Get flow info as JSON (for CLI)
getFlowInfo :: Database -> Text -> Either ServiceError Value
getFlowInfo db flowId = do
    case M.lookup flowId (dbFlows db) of
        Nothing -> Left $ FlowNotFound flowId
        Just flow ->
            let usageCount = getFlowUsageCount db flowId
                unitName = getUnitNameForFlow (dbUnits db) flow
                flowDetail = FlowDetail flow unitName usageCount
             in Right $ toJSON flowDetail

-- | Get activities that use a specific flow as JSON (for CLI)
getFlowActivities :: Database -> Text -> Either ServiceError Value
getFlowActivities db flowId = do
    case M.lookup flowId (dbFlows db) of
        Nothing -> Left $ FlowNotFound flowId
        Just _ ->
            let activities = getActivitiesUsingFlow db flowId
             in Right $ toJSON activities

-- | Get available synonym languages (placeholder)
getSynonymLanguages :: Database -> Either ServiceError Value
getSynonymLanguages _ =
    let languages = ["en", "fr", "de", "it", "es"] :: [Text]
     in Right $ toJSON languages

-- | Get synonym statistics (placeholder)
getSynonymStats :: Database -> Either ServiceError Value
getSynonymStats _ =
    let stats = M.fromList [("total_synonyms" :: Text, 1000 :: Int), ("languages", 5 :: Int)]
     in Right $ toJSON stats

-- | Compute LCIA scores (placeholder)
computeLCIA :: Database -> Text -> FilePath -> Either ServiceError Value
computeLCIA db queryText methodFile = do
    activity <- resolveActivityQuery db queryText
    let placeholder = M.fromList [("method" :: Text, T.pack methodFile), ("activity", activityId activity)]
     in Right $ toJSON placeholder

-- | Export LCIA results as XML (placeholder)
exportLCIAAsXML :: Value -> FilePath -> Either ServiceError ()
exportLCIAAsXML _ _ = Right ()

-- | Export LCIA results as CSV (placeholder)
exportLCIAAsCSV :: Value -> FilePath -> Either ServiceError ()
exportLCIAAsCSV _ _ = Right ()

-- | Export matrix debug data
exportMatrixDebugData :: Database -> Text -> DebugMatricesOptions -> Either ServiceError Value
exportMatrixDebugData database uuid opts = do
    validUuid <- validateUUID uuid
    case M.lookup validUuid (dbActivities database) of
        Nothing -> Left $ ActivityNotFound validUuid
        Just targetActivity -> do
            -- Extract matrix debugging information (includes all computations)
            let matrixData = extractMatrixDebugInfo database validUuid (debugFlowFilter opts)

            -- Get inventory from matrixData (already computed)
            let inventoryList = mdInventoryVector matrixData
            let bioFlowUUIDs = mdBioFlowUUIDs matrixData
            let inventory = M.fromList $ zip bioFlowUUIDs inventoryList

            -- FORCE CSV export execution - use deepseq to ensure it runs
            let !csvResult = unsafePerformIO $ do
                    putStrLn $ "DEBUG: Starting CSV export to " ++ debugOutput opts
                    exportMatrixDebugCSVs (debugOutput opts) matrixData
                    putStrLn $ "DEBUG: CSV export completed"
                    return "CSV_EXPORTED"

            let summary =
                    M.fromList
                        [ ("activity_uuid" :: Text, uuid)
                        , ("activity_name" :: Text, activityName targetActivity)
                        , ("total_inventory_flows" :: Text, T.pack $ show $ M.size inventory)
                        , ("matrix_debug_exported" :: Text, T.pack csvResult)
                        , ("supply_chain_file" :: Text, T.pack $ debugOutput opts ++ "_supply_chain.csv")
                        , ("biosphere_matrix_file" :: Text, T.pack $ debugOutput opts ++ "_biosphere_matrix.csv")
                        ]
            Right $ toJSON summary

-- | Extract matrix debug information from Database
extractMatrixDebugInfo :: Database -> UUID -> Maybe Text -> MatrixDebugInfo
extractMatrixDebugInfo database targetUUID flowFilter =
    let activities = dbActivities database
        flows = dbFlows database
        techTriples = dbTechnosphereTriples database
        bioTriples = dbBiosphereTriples database
        activityIndex = dbActivityIndex database
        bioFlowUUIDs = dbBiosphereFlows database
        activityCount = dbActivityCount database
        bioFlowCount = dbBiosphereCount database

        -- Build demand vector for target activity (f[i] = 1 for target, 0 elsewhere)
        demandVec = buildDemandVectorFromIndex activityIndex targetUUID
        demandList = toList demandVec

        -- Solve (I - A) * supply = demand using PETSc (same as in computeInventoryMatrix)
        supplyVec = solveSparseLinearSystem techTriples activityCount demandVec
        supplyList = toList supplyVec

        -- Calculate inventory using biosphere matrix: g = B * supply
        inventoryVec = applySparseMatrix bioTriples bioFlowCount supplyVec
        inventoryList = toList inventoryVec

        -- Filter biosphere flows if specified
        filteredBioTriples = case flowFilter of
            Nothing -> bioTriples
            Just filterText ->
                let matchingFlowIndices =
                        [ idx | (uuid, idx) <- zip bioFlowUUIDs [0 ..], Just flow <- [M.lookup uuid flows], T.toLower filterText `T.isInfixOf` T.toLower (flowName flow)
                        ]
                 in filter (\(row, _, _) -> row `elem` matchingFlowIndices) bioTriples
     in MatrixDebugInfo
            { mdActivities = activities
            , mdFlows = flows
            , mdTechTriples = techTriples
            , mdBioTriples = filteredBioTriples
            , mdActivityIndex = activityIndex
            , mdBioFlowUUIDs = bioFlowUUIDs
            , mdTargetUUID = targetUUID
            , mdSupplyVector = supplyList
            , mdDemandVector = demandList
            , mdInventoryVector = inventoryList
            }

-- | Matrix debug information container
data MatrixDebugInfo = MatrixDebugInfo
    { mdActivities :: M.Map UUID Activity
    , mdFlows :: M.Map UUID Flow
    , mdTechTriples :: [SparseTriple]
    , mdBioTriples :: [SparseTriple]
    , mdActivityIndex :: M.Map UUID Int
    , mdBioFlowUUIDs :: [UUID]
    , mdTargetUUID :: UUID
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
        targetIndex = case M.lookup (mdTargetUUID debugInfo) (mdActivityIndex debugInfo) of
            Just idx -> idx
            Nothing -> -1
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
    let activities = mdActivities debugInfo
        activityIndex = mdActivityIndex debugInfo
        supplyVector = mdSupplyVector debugInfo
        targetUUID = mdTargetUUID debugInfo

        -- Create supply chain data with REAL supply amounts from solver
        supplyChainRows =
            [ csvRow uuid activity idx supply
            | (uuid, idx) <- M.toList activityIndex
            , Just activity <- [M.lookup uuid activities]
            , let supply = if idx < length supplyVector then supplyVector !! idx else 0.0
            ]

        csvRow uuid activity idx supply =
            [ show uuid
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
    let flows = mdFlows debugInfo
        activities = mdActivities debugInfo
        bioTriples = mdBioTriples debugInfo
        bioFlowUUIDs = mdBioFlowUUIDs debugInfo
        activityIndex = mdActivityIndex debugInfo
        supplyVector = mdSupplyVector debugInfo

        -- Convert matrix triplets to CSV rows with REAL contributions
        matrixRows =
            [ csvRow bioTriple
            | bioTriple@(row, col, value) <- bioTriples
            , abs value > 1e-20 -- Filter very small values
            ]

        csvRow (row, col, value) =
            [ maybe "unknown" show (getFlowUUID row)
            , maybe "unknown" (T.unpack . flowName) (getFlow row)
            , maybe "unknown" T.unpack (getFlowUnit row)
            , maybe "unknown" show (getActivityUUID col)
            , maybe "unknown" (T.unpack . activityName) (getActivity col)
            , show value
            , show realContribution -- REAL contribution = matrix_value × supply_amount
            ]
          where
            getFlowUUID rowIdx =
                if rowIdx < length bioFlowUUIDs
                    then Just (bioFlowUUIDs !! rowIdx)
                    else Nothing
            getFlow rowIdx = getFlowUUID rowIdx >>= flip M.lookup flows
            getFlowUnit rowIdx = fmap (T.pack . show . flowUnitId) (getFlow rowIdx)
            getActivityUUID colIdx = fmap fst $ L.find ((== colIdx) . snd) (M.toList activityIndex)
            getActivity colIdx = getActivityUUID colIdx >>= flip M.lookup activities
            -- Calculate REAL contribution: B[row,col] × supply[col]
            realContribution =
                if col < length supplyVector
                    then value * (supplyVector !! col)
                    else 0.0

        csvHeader = ["flow_id", "flow_name", "unit", "activity_id", "activity_name", "matrix_value", "contribution"]
        allRows = csvHeader : matrixRows
        csvContent = L.intercalate "\n" (map (L.intercalate ",") allRows)

    putStrLn $ "DEBUG: Writing biosphere matrix with " ++ show (length matrixRows) ++ " entries"
    writeFile filePath csvContent
