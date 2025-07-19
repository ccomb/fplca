{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module ACV.Tree (buildProcessTree, buildProcessTreeWithFlows) where

import ACV.Types
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)

type VisitedSet = S.Set UUID

-- | Version originale - fonctionne avec l'ancienne structure
buildProcessTree :: ProcessDB -> UUID -> ProcessTree
buildProcessTree db = go S.empty
  where
    go :: VisitedSet -> UUID -> ProcessTree
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

-- | Nouvelle version qui nécessite accès à FlowDB
isTechnosphereInput :: FlowDB -> Exchange -> Bool
isTechnosphereInput flowDB ex =
    case M.lookup (exchangeFlowId ex) flowDB of
        Nothing -> False
        Just flow -> flowType flow == Technosphere
                  && exchangeIsInput ex
                  && not (exchangeIsReference ex)

-- | Dummy process to indicate recursion stop
placeholder :: Process
placeholder = Process "loop-detected" "Loop detected" "N/A" []

-- | Version optimisée avec FlowDB
buildProcessTreeWithFlows :: Database -> UUID -> ProcessTree
buildProcessTreeWithFlows (Database procDB flowDB) = go S.empty
  where
    go :: VisitedSet -> UUID -> ProcessTree
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
buildNode :: ProcessDB -> Process -> ProcessTree
buildNode db proc =
    let subExchanges = [] -- filter isTechnosphereInputOld (exchanges proc) -- Désactivé car non fonctionnel
        subTrees =
            [ (exchangeAmount ex, buildProcessTree db (exchangeFlowId ex))
            | ex <- subExchanges
            , M.member (exchangeFlowId ex) db
            ]
     in if null subTrees
            then Leaf proc
            else Node proc subTrees
