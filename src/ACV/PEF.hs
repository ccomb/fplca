module ACV.PEF where

import qualified Data.Map as M
import ACV.Types
import ACV.Inventory (Inventory)

-- | Résultat d'impact : somme des impacts par catégorie
type ImpactResult = M.Map ImpactCategory Double

-- | Applique une méthode de caractérisation (PEF ou autre) à un inventaire
applyCharacterization :: Inventory -> [CF] -> ImpactResult
applyCharacterization inv method =
  foldl accumulate M.empty method
  where
    accumulate acc cf =
      let fid     = cfFlowId cf
          factor  = cfFactor cf
          cat     = cfCategory cf
          amount  = M.findWithDefault 0 fid inv
      in M.insertWith (+) cat (factor * amount) acc

