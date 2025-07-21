module ACV.Inventory where

import qualified Data.Map as M
import ACV.Types
import Data.Maybe (mapMaybe)

-- | Un inventaire est une agrégation de flux biosphère : UUID -> quantité
type Inventory = M.Map UUID Double

-- | Calcule l'inventaire global à partir de l'arbre de activités - Version optimisée avec FlowDB
computeInventoryWithFlows :: FlowDB -> ActivityTree -> Inventory
computeInventoryWithFlows flowDB tree = computeInventoryWithWeightAndFlows flowDB 1.0 tree

-- | Calcule l'inventaire en tenant compte d'un facteur de pondération - Version optimisée avec FlowDB
computeInventoryWithWeightAndFlows :: FlowDB -> Double -> ActivityTree -> Inventory
computeInventoryWithWeightAndFlows flowDB weight (Leaf proc) =
  biosphereInventoryWithFlows flowDB weight proc
computeInventoryWithWeightAndFlows flowDB weight (Node proc children) =
  let localInv = biosphereInventoryWithFlows flowDB weight proc
      childrenInvs = [ computeInventoryWithWeightAndFlows flowDB (weight * qty) subtree
                     | (qty, subtree) <- children
                     ]
  in foldl (M.unionWith (+)) localInv childrenInvs

-- | Calcule l'inventaire global à partir de l'arbre de activités (version originale)
computeInventory :: ActivityTree -> Inventory
computeInventory = error "computeInventory: Use computeInventoryWithFlows instead"

-- | Calcule l'inventaire en tenant compte d'un facteur de pondération (version originale)
computeInventoryWithWeight :: Double -> ActivityTree -> Inventory
computeInventoryWithWeight weight (Leaf proc) =
  error "computeInventoryWithWeight: Use computeInventoryWithWeightAndFlows instead"
computeInventoryWithWeight weight (Node proc children) =
  error "computeInventoryWithWeight: Use computeInventoryWithWeightAndFlows instead"

-- | Extrait les flux biosphère d'un activité et les pondère - Version optimisée avec FlowDB
biosphereInventoryWithFlows :: FlowDB -> Double -> Activity -> Inventory
biosphereInventoryWithFlows flowDB w proc =
  M.fromListWith (+)
    [ (exchangeFlowId ex, w * exchangeAmount ex)
    | ex <- exchanges proc
    , isBiosphereFlow flowDB ex
    ]

-- | Vérifie si un échange est un flux biosphère (optimisé avec variants Exchange)
isBiosphereFlow :: FlowDB -> Exchange -> Bool
isBiosphereFlow _ ex =
    case ex of
        BiosphereExchange _ _ _ _ -> True
        TechnosphereExchange _ _ _ _ _ _ -> False

-- | Extrait les flux biosphère d'un activité et les pondère (version originale)
biosphereInventory :: Double -> Activity -> Inventory
biosphereInventory w proc =
  error "biosphereInventory: Use biosphereInventoryWithFlows instead"

-- | Calcule l'inventaire global à partir d'un arbre LoopAwareTree (avec détection de boucles)
computeInventoryFromLoopAwareTree :: FlowDB -> LoopAwareTree -> Inventory
computeInventoryFromLoopAwareTree flowDB tree = computeLoopAwareInventoryWithWeight flowDB 1.0 tree

-- | Calcule l'inventaire en tenant compte d'un facteur de pondération - Version LoopAwareTree
computeLoopAwareInventoryWithWeight :: FlowDB -> Double -> LoopAwareTree -> Inventory
computeLoopAwareInventoryWithWeight flowDB weight (TreeLeaf activity) =
  biosphereInventoryWithFlows flowDB weight activity
computeLoopAwareInventoryWithWeight flowDB weight (TreeLoop _ _ _) =
  M.empty  -- Les boucles ne contribuent pas à l'inventaire (déjà comptées)
computeLoopAwareInventoryWithWeight flowDB weight (TreeNode activity children) =
  let localInv = biosphereInventoryWithFlows flowDB weight activity
      childrenInvs = [ computeLoopAwareInventoryWithWeight flowDB (weight * quantity) subtree
                     | (quantity, _, subtree) <- children  -- Ignore flow info, use quantity and subtree
                     ]
  in foldl (M.unionWith (+)) localInv childrenInvs

