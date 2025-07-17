module ACV.Tree (buildProcessTree) where

import ACV.Types
import qualified Data.Map as M
import qualified Data.Set as S

type ProcessDB = M.Map UUID Process
type VisitedSet = S.Set UUID

-- | Public entry point - Memory optimized version
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
                            [ (exchangeAmount ex, go newSeen (flowId $ exchangeFlow ex))
                            | ex <- exchanges proc
                            , isTechnosphereInput ex
                            , M.member (flowId $ exchangeFlow ex) db
                            ]
                        -- Force evaluation of children to avoid thunk buildup
                        children' = map (\(amt, tree) -> amt `seq` tree `seq` (amt, tree)) children
                     in if null children'
                            then Leaf proc
                            else Node proc children'

-- | Helper for technosphere input check
isTechnosphereInput :: Exchange -> Bool
isTechnosphereInput ex =
    flowType (exchangeFlow ex) == Technosphere
        && exchangeIsInput ex
        && not (exchangeIsReference ex)

-- | Dummy process to indicate recursion stop
placeholder :: Process
placeholder = Process "loop-detected" "Loop detected" "N/A" []

-- | Fonction récursive : construit un nœud de l'arbre
buildNode :: ProcessDB -> Process -> ProcessTree
buildNode db proc =
    let subExchanges = filter isTechnosphereInput (exchanges proc)
        subTrees =
            [ (exchangeAmount ex, buildProcessTree db (flowId $ exchangeFlow ex))
            | ex <- subExchanges
            , M.member (flowId $ exchangeFlow ex) db
            ]
     in if null subTrees
            then Leaf proc
            else Node proc subTrees
