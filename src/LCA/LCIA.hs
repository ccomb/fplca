{-# LANGUAGE OverloadedStrings #-}

-- | LCIA (Life Cycle Impact Assessment) computation functions
module LCA.LCIA
    ( computeLCIAScore
    ) where

import qualified Data.Map.Strict as M
import Data.UUID (UUID)

import LCA.Matrix (Inventory)
import LCA.Method.Mapping (MatchStrategy)
import LCA.Method.Types (MethodCF(..), FlowDirection(..))
import LCA.Types (Flow(..))

-- | Compute LCIA score from inventory and flow mappings
--
-- For each mapped flow:
--   score += inventory[flow_uuid] * CF_value
--
-- Uses ecoinvent sign convention:
-- - Emissions (outputs) have positive inventory values
-- - Resources (inputs) have negative inventory values
-- - Both multiply by positive CF values
computeLCIAScore :: Inventory -> [(MethodCF, Maybe (Flow, MatchStrategy))] -> Double
computeLCIAScore inventory mappings =
    sum [qty * mcfValue cf | (cf, Just (flow, _)) <- mappings
                           , Just qty <- [M.lookup (flowId flow) inventory]]
