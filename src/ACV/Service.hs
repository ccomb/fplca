{-# LANGUAGE OverloadedStrings #-}

module ACV.Service where

import ACV.Inventory (computeInventoryWithFlows, computeInventoryFromLoopAwareTree, Inventory)
import ACV.Query (findFlowsBySynonym, findActivitiesByFields)
import ACV.Tree (buildActivityTreeWithDatabase, buildLoopAwareTree, buildCutoffLoopAwareTree)
import ACV.Types
import ACV.API (SearchResults(..), ActivitySummary(..), FlowSearchResult(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.UUID as UUID
import Data.Aeson (Value, toJSON)

-- | Domain service errors
data ServiceError 
    = InvalidUUID Text
    | ActivityNotFound Text
    | FlowNotFound Text
    deriving (Show, Eq)

-- | Validate UUID format
validateUUID :: Text -> Either ServiceError Text
validateUUID uuidText
    | Just _ <- UUID.fromText uuidText = Right uuidText
    | otherwise = Left $ InvalidUUID $ "Invalid UUID format: " <> uuidText

-- | Simple activity info for CLI (no complex API types)
getActivityInfo :: Database -> Text -> Either ServiceError Value
getActivityInfo db uuid = do
    validUuid <- validateUUID uuid
    case M.lookup validUuid (dbActivities db) of
        Nothing -> Left $ ActivityNotFound validUuid
        Just activity -> Right $ toJSON $ activityName activity

-- | Get activity inventory (pure domain logic)
getActivityInventory :: Database -> Text -> Either ServiceError Value
getActivityInventory db uuid = do
    validUuid <- validateUUID uuid
    case M.lookup validUuid (dbActivities db) of
        Nothing -> Left $ ActivityNotFound validUuid
        Just _ -> 
            -- Use same logic as API: cutoff-based loop-aware tree for proper supply chain calculation
            let maxDepth = 35 -- Higher depth since cutoffs will control tree size
                cutoffThreshold = 1e-8 -- 0.01% cutoff - more permissive to capture full supply chain
                loopAwareTree = buildCutoffLoopAwareTree db validUuid maxDepth cutoffThreshold
                inventory = computeInventoryFromLoopAwareTree (dbFlows db) loopAwareTree
            in Right $ toJSON inventory

-- | Get activity tree (pure domain logic)
getActivityTree :: Database -> Text -> Either ServiceError Value
getActivityTree db uuid = do
    validUuid <- validateUUID uuid
    case M.lookup validUuid (dbActivities db) of
        Nothing -> Left $ ActivityNotFound validUuid
        Just _ -> 
            let tree = buildLoopAwareTree db validUuid 2
                nodeCount :: Int
                nodeCount = countTreeNodes tree
            in Right $ toJSON nodeCount
  where
    countTreeNodes (TreeLeaf _) = 1
    countTreeNodes (TreeLoop _ _ _) = 1  
    countTreeNodes (TreeNode _ children) = 1 + sum [countTreeNodes subtree | (_, _, subtree) <- children]

-- | Search flows (returns same format as API)
searchFlows :: Database -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Either ServiceError Value
searchFlows _  Nothing _ _ _ = Right $ toJSON $ SearchResults ([] :: [FlowSearchResult]) 0 0 50 False
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

