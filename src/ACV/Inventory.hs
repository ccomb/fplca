module ACV.Inventory where

import qualified Data.Map as M
import ACV.Types

-- | Un inventaire est une agrégation de flux biosphère : UUID -> quantité
type Inventory = M.Map UUID Double

-- | Calcule l'inventaire global à partir de l'arbre de procédés
computeInventory :: ProcessTree -> Inventory
computeInventory = computeInventoryWithWeight 1.0

-- | Calcule l'inventaire en tenant compte d'un facteur de pondération
computeInventoryWithWeight :: Double -> ProcessTree -> Inventory
computeInventoryWithWeight weight (Leaf proc) =
  biosphereInventory weight proc
computeInventoryWithWeight weight (Node proc children) =
  let localInv = biosphereInventory weight proc
      childrenInvs = [ computeInventoryWithWeight (weight * qty) subtree
                     | (qty, subtree) <- children
                     ]
  in foldl (M.unionWith (+)) localInv childrenInvs

-- | Extrait les flux biosphère d’un procédé et les pondère
biosphereInventory :: Double -> Process -> Inventory
biosphereInventory w proc =
  M.fromListWith (+)
    [ (flowId (exchangeFlow ex), w * exchangeAmount ex)
    | ex <- exchanges proc
    , flowType (exchangeFlow ex) == Biosphere
    ]

