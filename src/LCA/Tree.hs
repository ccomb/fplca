{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module LCA.Tree (buildLoopAwareTree) where

import LCA.Types
import LCA.UnitConversion (convertExchangeAmount)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V
import Data.Text (Text)

type VisitedSet = S.Set UUID



-- | Nouvelle version optimisée avec les variants Exchange
isTechnosphereInput :: FlowDB -> Exchange -> Bool
isTechnosphereInput _ ex =
    case ex of
        TechnosphereExchange _ _ _ isInput isRef _ _ _ -> isInput && not isRef
        BiosphereExchange _ _ _ _ _ -> False

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

-- | Build loop-aware tree for SVG export with maximum depth limit
buildLoopAwareTree :: Database -> UUID -> Int -> LoopAwareTree
buildLoopAwareTree db rootUUID maxDepth =
    let maxNodes = 300  -- Maximum total nodes to prevent performance issues
        (tree, _) = buildLoopAwareTreeWithVisited db rootUUID S.empty 0 maxDepth maxNodes
    in tree

-- | Helper function with visited set, depth tracking, and node count limit
-- Returns (tree, remainingNodeBudget)
buildLoopAwareTreeWithVisited :: Database -> UUID -> S.Set UUID -> Int -> Int -> Int -> (LoopAwareTree, Int)
buildLoopAwareTreeWithVisited db activityUUID visited depth maxDepth remainingNodes
    -- Stop if node budget exhausted
    | remainingNodes <= 0 =
        case findActivityByActivityUUID db activityUUID of
            Nothing -> (TreeLoop activityUUID "Missing Activity (node limit)" depth, 0)
            Just activity -> (TreeLoop activityUUID (activityName activity) depth, 0)
    -- Stop at maximum depth
    | depth >= maxDepth =
        case findActivityByActivityUUID db activityUUID of
            Nothing -> (TreeLoop activityUUID "Missing Activity" depth, remainingNodes - 1)
            Just activity -> (TreeLoop activityUUID (activityName activity) depth, remainingNodes - 1)
    -- Detect loop
    | activityUUID `S.member` visited =
        case findActivityByActivityUUID db activityUUID of
            Nothing -> (TreeLoop activityUUID "Missing Activity" depth, remainingNodes - 1)
            Just activity -> (TreeLoop activityUUID (activityName activity) depth, remainingNodes - 1)
    -- Build subtree
    | otherwise =
        case findActivityByActivityUUID db activityUUID of
            Nothing -> (TreeLoop activityUUID "Missing Activity" depth, remainingNodes - 1)
            Just activity ->
                let visited' = S.insert activityUUID visited
                    techInputs = [ ex | ex <- exchanges activity
                                     , isTechnosphereInput (dbFlows db) ex ]
                    -- Build children with node budget tracking
                    (children, finalBudget) = buildChildrenWithBudget techInputs visited' (depth + 1) maxDepth (remainingNodes - 1)
                    buildChildrenWithBudget :: [Exchange] -> S.Set UUID -> Int -> Int -> Int -> ([(Double, Flow, LoopAwareTree)], Int)
                    buildChildrenWithBudget [] _ _ _ budget = ([], budget)
                    buildChildrenWithBudget (ex:exs) vis d maxD budget
                        | budget <= 0 = ([], 0)  -- Stop if budget exhausted
                        | otherwise =
                            case (exchangeActivityLinkId ex, M.lookup (exchangeFlowId ex) (dbFlows db)) of
                                (Just targetUUID, Just flow) ->
                                    let convertedAmount = getConvertedExchangeAmount db ex targetUUID
                                        (subtree, budget') = buildLoopAwareTreeWithVisited db targetUUID vis d maxD budget
                                        (restChildren, finalBudget') = buildChildrenWithBudget exs vis d maxD budget'
                                    in ((convertedAmount, flow, subtree) : restChildren, finalBudget')
                                _ -> buildChildrenWithBudget exs vis d maxD budget
                in if null children
                   then (TreeLeaf activity, finalBudget)
                   else (TreeNode activity children, finalBudget)
