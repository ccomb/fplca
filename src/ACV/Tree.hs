{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module ACV.Tree (buildActivityTreeWithDatabase, buildLoopAwareTree, buildCutoffLoopAwareTree) where

import ACV.Types
import ACV.UnitConversion (convertExchangeAmount)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V
import Data.Text (Text)

type VisitedSet = S.Set UUID



-- | Nouvelle version optimisée avec les variants Exchange
isTechnosphereInput :: FlowDB -> Exchange -> Bool
isTechnosphereInput _ ex =
    case ex of
        TechnosphereExchange _ _ _ isInput isRef _ _ -> isInput && not isRef
        BiosphereExchange _ _ _ _ -> False

-- | Dummy activity to indicate recursion stop
placeholder :: Activity
placeholder = Activity "Loop detected" ["Loop detected"] M.empty M.empty "N/A" "unit" []

-- | Get converted exchange amount ensuring unit compatibility
-- Converts exchange amount to the target activity's reference unit for proper scaling
getConvertedExchangeAmount :: Database -> Exchange -> UUID -> Double
getConvertedExchangeAmount db exchange targetActivityUUID =
    let originalAmount = exchangeAmount exchange
        -- Get exchange unit name
        exchangeUnitName = case M.lookup (exchangeUnitId exchange) (dbUnits db) of
            Just unit -> unitName unit
            Nothing -> "unknown"
        -- Get target activity's reference unit (lookup by activity UUID for backward compat)
        targetReferenceUnit = case findActivityByActivityUUID db targetActivityUUID of
            Just targetActivity -> activityUnit targetActivity
            Nothing -> "unknown"
    in if exchangeUnitName == "unknown" || targetReferenceUnit == "unknown"
       then originalAmount  -- No conversion possible, use original
       else convertExchangeAmount exchangeUnitName targetReferenceUnit originalAmount


-- | Version optimisée avec Database complet (avec index)
-- Now uses ProcessId instead of UUID to support multi-product activities
buildActivityTreeWithDatabase :: Database -> UUID -> ActivityTree
buildActivityTreeWithDatabase db rootUUID =
    -- Find the ProcessId for the root activity UUID
    case findProcessIdByActivityUUID db rootUUID of
        Nothing -> Leaf placeholder
        Just rootProcessId -> go S.empty rootProcessId
  where
    go :: S.Set ProcessId -> ProcessId -> ActivityTree
    go seen processId
        | S.member processId seen = Leaf placeholder -- Prevent infinite loop
        | otherwise =
            case getActivity db processId of
                Nothing -> Leaf placeholder
                Just proc ->
                    let newSeen = S.insert processId seen
                        children =
                            [ (exchangeAmount ex, go newSeen targetProcessId)
                            | ex <- exchanges proc
                            , isTechnosphereInput (dbFlows db) ex
                            -- Try to get ProcessId from exchange, fallback to finding by activity UUID
                            , Just targetProcessId <- [exchangeProcessLinkId ex `orElse`
                                (exchangeActivityLinkId ex >>= findProcessIdByActivityUUID db)]
                            , isValidProcessId db targetProcessId
                            ]
                        -- Force evaluation of children list to avoid thunk buildup in recursive tree
                        !children' = children
                     in if null children'
                            then Leaf proc
                            else Node proc children'

    -- Helper for Maybe chaining
    orElse :: Maybe a -> Maybe a -> Maybe a
    orElse (Just x) _ = Just x
    orElse Nothing y = y

    -- Helper to check if ProcessId is valid
    isValidProcessId :: Database -> ProcessId -> Bool
    isValidProcessId database pid =
        pid >= 0 && fromIntegral pid < V.length (dbActivities database)


-- | Build loop-aware tree for SVG export with maximum depth limit
buildLoopAwareTree :: Database -> UUID -> Int -> LoopAwareTree
buildLoopAwareTree db rootUUID maxDepth = buildLoopAwareTreeWithVisited db rootUUID S.empty 0 maxDepth


-- | Build loop-aware tree with cutoff-based pruning (professional LCA approach)
buildCutoffLoopAwareTree :: Database -> UUID -> Int -> Double -> LoopAwareTree
buildCutoffLoopAwareTree db rootUUID maxDepth cutoffThreshold =
    buildCutoffTreeWithVisited db rootUUID S.empty 0 maxDepth 1.0 cutoffThreshold

-- | Helper function with visited set and depth tracking
buildLoopAwareTreeWithVisited :: Database -> UUID -> S.Set UUID -> Int -> Int -> LoopAwareTree
buildLoopAwareTreeWithVisited db activityUUID visited depth maxDepth
    -- Stop at maximum depth
    | depth >= maxDepth =
        case findActivityByActivityUUID db activityUUID of
            Nothing -> TreeLoop activityUUID "Missing Activity" depth
            Just activity -> TreeLoop activityUUID (activityName activity) depth
    -- Detect loop
    | activityUUID `S.member` visited =
        case findActivityByActivityUUID db activityUUID of
            Nothing -> TreeLoop activityUUID "Missing Activity" depth
            Just activity -> TreeLoop activityUUID (activityName activity) depth
    -- Build subtree
    | otherwise =
        case findActivityByActivityUUID db activityUUID of
            Nothing -> TreeLoop activityUUID "Missing Activity" depth
            Just activity ->
                let visited' = S.insert activityUUID visited
                    techInputs = [ ex | ex <- exchanges activity
                                     , isTechnosphereInput (dbFlows db) ex ]
                    children = [ (convertedAmount, flow, subtree)
                               | ex <- techInputs
                               , Just targetUUID <- [exchangeActivityLinkId ex]
                               , Just flow <- [M.lookup (exchangeFlowId ex) (dbFlows db)]
                               , let convertedAmount = getConvertedExchangeAmount db ex targetUUID
                               , let subtree = buildLoopAwareTreeWithVisited db targetUUID visited' (depth + 1) maxDepth
                               ]
                in if null children
                   then TreeLeaf activity
                   else TreeNode activity children


-- | Cutoff-based tree building with contribution tracking
buildCutoffTreeWithVisited :: Database -> UUID -> S.Set UUID -> Int -> Int -> Double -> Double -> LoopAwareTree
buildCutoffTreeWithVisited db activityUUID visited depth maxDepth scaleFactor cutoffThreshold
    -- Stop if contribution becomes too small (cutoff pruning)
    | scaleFactor < cutoffThreshold =
        case findActivityByActivityUUID db activityUUID of
            Nothing -> TreeLoop activityUUID "Below cutoff threshold" depth
            Just activity -> TreeLoop activityUUID ("Cutoff: " <> activityName activity) depth
    -- Stop at maximum depth
    | depth >= maxDepth =
        case findActivityByActivityUUID db activityUUID of
            Nothing -> TreeLoop activityUUID "Missing Activity" depth
            Just activity -> TreeLoop activityUUID (activityName activity) depth
    -- Detect loop
    | activityUUID `S.member` visited =
        case findActivityByActivityUUID db activityUUID of
            Nothing -> TreeLoop activityUUID "Missing Activity" depth
            Just activity -> TreeLoop activityUUID (activityName activity) depth
    -- Build subtree
    | otherwise =
        case findActivityByActivityUUID db activityUUID of
            Nothing -> TreeLoop activityUUID "Missing Activity" depth
            Just activity ->
                let visited' = S.insert activityUUID visited
                    techInputs = [ ex | ex <- exchanges activity
                                     , isTechnosphereInput (dbFlows db) ex ]
                    children = [ (convertedAmount, flow, subtree)
                               | ex <- techInputs
                               , Just targetUUID <- [exchangeActivityLinkId ex]
                               , Just flow <- [M.lookup (exchangeFlowId ex) (dbFlows db)]
                               , let convertedAmount = getConvertedExchangeAmount db ex targetUUID
                               , let childScaleFactor = scaleFactor * abs convertedAmount
                               , childScaleFactor >= cutoffThreshold  -- Cutoff check before building subtree
                               , let subtree = buildCutoffTreeWithVisited db targetUUID visited' (depth + 1) maxDepth childScaleFactor cutoffThreshold
                               ]
                in if null children
                   then TreeLeaf activity
                   else TreeNode activity children
