{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module ACV.Tree (buildLoopAwareTree) where

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
buildLoopAwareTree db rootUUID maxDepth = buildLoopAwareTreeWithVisited db rootUUID S.empty 0 maxDepth

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
