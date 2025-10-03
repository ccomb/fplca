{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module ACV.Tree (buildActivityTreeWithDatabase, buildLoopAwareTree, buildCutoffLoopAwareTree) where

import ACV.Types
import ACV.UnitConversion (convertExchangeAmount)
import qualified Data.Map as M
import qualified Data.Set as S
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
placeholder = Activity "loop-detected" Nothing "Loop detected" ["Loop detected"] M.empty M.empty "N/A" "unit" []

-- | Get converted exchange amount ensuring unit compatibility
-- Converts exchange amount to the target activity's reference unit for proper scaling
getConvertedExchangeAmount :: Database -> Exchange -> UUID -> Double
getConvertedExchangeAmount db exchange targetActivityUUID = 
    let originalAmount = exchangeAmount exchange
        -- Get exchange unit name
        exchangeUnitName = case M.lookup (exchangeUnitId exchange) (dbUnits db) of
            Just unit -> unitName unit
            Nothing -> "unknown"
        -- Get target activity's reference unit
        targetReferenceUnit = case M.lookup targetActivityUUID (dbActivities db) of
            Just targetActivity -> activityUnit targetActivity
            Nothing -> "unknown"
    in if exchangeUnitName == "unknown" || targetReferenceUnit == "unknown"
       then originalAmount  -- No conversion possible, use original
       else convertExchangeAmount exchangeUnitName targetReferenceUnit originalAmount


-- | Version optimisée avec Database complet (avec index)
buildActivityTreeWithDatabase :: Database -> UUID -> ActivityTree
buildActivityTreeWithDatabase db = buildActivityTreeWithFlowDBs (dbActivities db) (dbFlows db)

-- | Version optimisée avec FlowDB - Implémentation interne
buildActivityTreeWithFlowDBs :: ActivityDB -> FlowDB -> UUID -> ActivityTree
buildActivityTreeWithFlowDBs procDB flowDB = go S.empty
  where
    go :: VisitedSet -> UUID -> ActivityTree
    go seen pid
        | S.member pid seen = Leaf placeholder -- Prevent infinite loop
        | otherwise =
            case M.lookup pid procDB of
                Nothing -> Leaf placeholder
                Just proc ->
                    let newSeen = S.insert pid seen
                        children =
                            [ (exchangeAmount ex, go newSeen targetUUID)
                            | ex <- exchanges proc
                            , isTechnosphereInput flowDB ex
                            , Just targetUUID <- [exchangeActivityLinkId ex]
                            , M.member targetUUID procDB
                            ]
                        -- Force evaluation of children list to avoid thunk buildup in recursive tree
                        !children' = children
                     in if null children'
                            then Leaf proc
                            else Node proc children'


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
        case M.lookup activityUUID (dbActivities db) of
            Nothing -> TreeLoop activityUUID "Missing Activity" depth
            Just activity -> TreeLoop activityUUID (activityName activity) depth
    -- Detect loop
    | activityUUID `S.member` visited = 
        case M.lookup activityUUID (dbActivities db) of
            Nothing -> TreeLoop activityUUID "Missing Activity" depth  
            Just activity -> TreeLoop activityUUID (activityName activity) depth
    -- Build subtree
    | otherwise = 
        case M.lookup activityUUID (dbActivities db) of
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
        case M.lookup activityUUID (dbActivities db) of
            Nothing -> TreeLoop activityUUID "Below cutoff threshold" depth
            Just activity -> TreeLoop activityUUID ("Cutoff: " <> activityName activity) depth
    -- Stop at maximum depth
    | depth >= maxDepth = 
        case M.lookup activityUUID (dbActivities db) of
            Nothing -> TreeLoop activityUUID "Missing Activity" depth
            Just activity -> TreeLoop activityUUID (activityName activity) depth
    -- Detect loop
    | activityUUID `S.member` visited = 
        case M.lookup activityUUID (dbActivities db) of
            Nothing -> TreeLoop activityUUID "Missing Activity" depth  
            Just activity -> TreeLoop activityUUID (activityName activity) depth
    -- Build subtree
    | otherwise = 
        case M.lookup activityUUID (dbActivities db) of
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
