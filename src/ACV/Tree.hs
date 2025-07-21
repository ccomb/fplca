{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module ACV.Tree (buildActivityTree, buildActivityTreeWithFlows, buildActivityTreeWithDatabase, buildLoopAwareTree, buildSafeLoopAwareTree, buildCutoffLoopAwareTree) where

import ACV.Types
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)

type VisitedSet = S.Set UUID

-- | Version originale - fonctionne avec l'ancienne structure
buildActivityTree :: ActivityDB -> UUID -> ActivityTree
buildActivityTree db = go S.empty
  where
    go :: VisitedSet -> UUID -> ActivityTree
    go seen pid
        | S.member pid seen = Leaf placeholder -- Prevent infinite loop
        | otherwise =
            case M.lookup pid db of
                Nothing -> Leaf placeholder
                Just proc ->
                    let newSeen = S.insert pid seen
                        children =
                            [ (exchangeAmount ex, go newSeen (exchangeFlowId ex))
                            | ex <- exchanges proc
                            , isTechnosphereInputOld ex
                            , M.member (exchangeFlowId ex) db
                            ]
                        -- Force evaluation of children list to avoid thunk buildup in recursive tree
                        !children' = children
                     in if null children'
                            then Leaf proc
                            else Node proc children'

-- | Version originale pour compatibilité (ne devrait plus être utilisée)
isTechnosphereInputOld :: Exchange -> Bool
isTechnosphereInputOld ex =
    -- Cette fonction ne peut plus fonctionner avec la nouvelle structure!
    -- Elle est gardée temporairement pour la compatibilité
    error "isTechnosphereInputOld: Cannot determine flow type without FlowDB access"

-- | Nouvelle version optimisée avec les variants Exchange
isTechnosphereInput :: FlowDB -> Exchange -> Bool
isTechnosphereInput _ ex =
    case ex of
        TechnosphereExchange _ _ _ isInput isRef _ -> isInput && not isRef
        BiosphereExchange _ _ _ _ -> False

-- | Dummy activity to indicate recursion stop
placeholder :: Activity
placeholder = Activity "loop-detected" "Loop detected" ["Loop detected"] M.empty M.empty "N/A" "unit" []

-- | Version optimisée avec FlowDB - Compatible avec SimpleDatabase et Database
buildActivityTreeWithFlows :: SimpleDatabase -> UUID -> ActivityTree
buildActivityTreeWithFlows (SimpleDatabase procDB flowDB _) = buildActivityTreeWithFlowDBs procDB flowDB

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
                            [ (exchangeAmount ex, go newSeen (exchangeFlowId ex))
                            | ex <- exchanges proc
                            , isTechnosphereInput flowDB ex
                            , M.member (exchangeFlowId ex) procDB
                            ]
                        -- Force evaluation of children list to avoid thunk buildup in recursive tree
                        !children' = children
                     in if null children'
                            then Leaf proc
                            else Node proc children'

-- | Fonction récursive : construit un nœud de l'arbre (version originale)
buildNode :: ActivityDB -> Activity -> ActivityTree
buildNode db proc =
    let subExchanges = [] -- filter isTechnosphereInputOld (exchanges proc) -- Désactivé car non fonctionnel
        subTrees =
            [ (exchangeAmount ex, buildActivityTree db (exchangeFlowId ex))
            | ex <- subExchanges
            , M.member (exchangeFlowId ex) db
            ]
     in if null subTrees
            then Leaf proc
            else Node proc subTrees

-- | Build loop-aware tree for SVG export with maximum depth limit
buildLoopAwareTree :: Database -> UUID -> Int -> LoopAwareTree
buildLoopAwareTree db rootUUID maxDepth = buildLoopAwareTreeWithVisited db rootUUID S.empty 0 maxDepth

-- | Build loop-aware tree with both depth and node count limits for safer inventory calculation
buildSafeLoopAwareTree :: Database -> UUID -> Int -> Int -> LoopAwareTree  
buildSafeLoopAwareTree db rootUUID maxDepth maxNodes = 
    buildSafeTreeWithLimits db rootUUID S.empty 0 maxDepth (0, maxNodes)

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
                    children = [ (exchangeAmount ex, flow, subtree)
                               | ex <- techInputs
                               , Just targetUUID <- [exchangeActivityLinkId ex]
                               , Just flow <- [M.lookup (exchangeFlowId ex) (dbFlows db)]
                               , let subtree = buildLoopAwareTreeWithVisited db targetUUID visited' (depth + 1) maxDepth
                               ]
                in if null children
                   then TreeLeaf activity
                   else TreeNode activity children

-- | Safe tree building with node count limits (nodeCount, maxNodes)
buildSafeTreeWithLimits :: Database -> UUID -> S.Set UUID -> Int -> Int -> (Int, Int) -> LoopAwareTree
buildSafeTreeWithLimits db activityUUID visited depth maxDepth (nodeCount, maxNodes)
    -- Stop if too many nodes created
    | nodeCount >= maxNodes =
        case M.lookup activityUUID (dbActivities db) of
            Nothing -> TreeLoop activityUUID "Node limit reached" depth
            Just activity -> TreeLoop activityUUID (activityName activity) depth
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
                    (children, finalNodeCount) = buildChildrenWithLimit db techInputs visited' depth maxDepth (nodeCount + 1) maxNodes
                in if null children
                   then TreeLeaf activity
                   else TreeNode activity children
  where
    buildChildrenWithLimit :: Database -> [Exchange] -> S.Set UUID -> Int -> Int -> Int -> Int -> ([(Double, Flow, LoopAwareTree)], Int)
    buildChildrenWithLimit _ [] _ _ _ currentCount _ = ([], currentCount)
    buildChildrenWithLimit db (ex:exs) visited depth maxDepth currentCount maxNodes
        | currentCount >= maxNodes = ([], currentCount)  -- Stop building children
        | otherwise = 
            case (exchangeActivityLinkId ex, M.lookup (exchangeFlowId ex) (dbFlows db)) of
                (Just targetUUID, Just flow) ->
                    let subtree = buildSafeTreeWithLimits db targetUUID visited (depth + 1) maxDepth (currentCount, maxNodes)
                        child = (exchangeAmount ex, flow, subtree)
                        (restChildren, restCount) = buildChildrenWithLimit db exs visited depth maxDepth (currentCount + 1) maxNodes
                    in (child : restChildren, restCount)
                _ -> buildChildrenWithLimit db exs visited depth maxDepth currentCount maxNodes

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
                    children = [ (exchangeAmount ex, flow, subtree)
                               | ex <- techInputs
                               , Just targetUUID <- [exchangeActivityLinkId ex]
                               , Just flow <- [M.lookup (exchangeFlowId ex) (dbFlows db)]
                               , let childScaleFactor = scaleFactor * abs (exchangeAmount ex)
                               , childScaleFactor >= cutoffThreshold  -- Cutoff check before building subtree
                               , let subtree = buildCutoffTreeWithVisited db targetUUID visited' (depth + 1) maxDepth childScaleFactor cutoffThreshold
                               ]
                in if null children
                   then TreeLeaf activity
                   else TreeNode activity children
