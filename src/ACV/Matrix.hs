{-# LANGUAGE OverloadedStrings #-}

module ACV.Matrix 
    ( computeInventoryMatrix
    , buildCompleteTechnosphereMatrix
    , buildCompleteBiosphereMatrix
    , buildMatrixFromTriples
    , buildDemandVector
    , getAllBiosphereFlowsFromDB
    ) where

import ACV.Types
import ACV.UnitConversion (convertExchangeAmount)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (elemIndex, sortOn)
import Data.Maybe (fromMaybe, mapMaybe, fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace (trace)
import Numeric.LinearAlgebra

-- | Matrix-based inventory calculation using Brightway approach
-- Builds complete matrices for entire database, then solves: (I - A)^-1 * f
-- Where: I = identity, A = technosphere matrix, f = demand vector
computeInventoryMatrix :: Database -> UUID -> M.Map UUID Double
computeInventoryMatrix db rootUUID = 
    let allActivities = dbActivities db
        activityUUIDs = M.keys allActivities
        activityCount = length activityUUIDs
        
        _ = trace ("Complete matrix calculation for " ++ show activityCount ++ " activities") ()
        
        -- Build complete sparse matrices for entire database
        (techMatrix, activityIndex) = buildCompleteTechnosphereMatrix db
        bioMatrix = buildCompleteBiosphereMatrix db
        
        -- Build demand vector for root activity
        demandVec = buildDemandVector allActivities activityUUIDs rootUUID
        
        -- Solve standard LCA equation: (I - A)^-1 * f
        identityMatrix = ident activityCount
        supplyVec = (identityMatrix - techMatrix) <\> demandVec
        
        -- Calculate inventory: g = B * supply
        inventoryVec = bioMatrix #> supplyVec
        
        -- Convert back to Map with biosphere flow UUIDs
        bioFlowUUIDs = getAllBiosphereFlowsFromDB db
        
    in M.fromList $ zip bioFlowUUIDs (toList inventoryVec)


-- | Build complete technosphere matrix for entire database (Brightway approach)
-- Returns matrix and activity index mapping for efficient lookup
buildCompleteTechnosphereMatrix :: Database -> (Matrix Double, M.Map UUID Int)
buildCompleteTechnosphereMatrix db =
    let allActivities = dbActivities db
        activityUUIDs = M.keys allActivities
        activityCount = length activityUUIDs
        activityIndex = M.fromList $ zip activityUUIDs [0..]
        
        -- Build sparse triplets for entire database
        techTriples = [ (i, j, value)
                      | (j, consumerUUID) <- zip [0..] activityUUIDs
                      , Just consumerActivity <- [M.lookup consumerUUID allActivities]
                      , ex <- exchanges consumerActivity
                      , isTechnosphereExchange ex
                      , let (i, value) = 
                              if exchangeIsReference ex
                              then (j, exchangeAmount ex)  -- Diagonal: production output
                              else case exchangeActivityLinkId ex of
                                     Just inputActivityUUID -> 
                                         case M.lookup inputActivityUUID activityIndex of
                                             Just producerIdx -> (producerIdx, -(exchangeAmount ex))  -- Input consumption  
                                             Nothing -> (-1, 0.0)  -- Invalid link
                                     Nothing -> (-1, 0.0)  -- Invalid exchange
                      , i >= 0 && abs value > 1e-15  -- Only significant non-zero values
                      ]
        
        -- Convert to dense matrix
        techMatrix = buildMatrixFromTriples activityCount activityCount techTriples
        
    in (techMatrix, activityIndex)

-- | Build complete biosphere matrix for entire database (Brightway approach) 
-- Returns matrix mapping biosphere flows × activities 
buildCompleteBiosphereMatrix :: Database -> Matrix Double
buildCompleteBiosphereMatrix db =
    let allActivities = dbActivities db
        activityUUIDs = M.keys allActivities
        activityCount = length activityUUIDs
        activityIndex = M.fromList $ zip activityUUIDs [0..]
        
        -- Get all unique biosphere flows in database
        bioFlowUUIDs = getAllBiosphereFlowsFromDB db
        bioFlowCount = length bioFlowUUIDs
        bioFlowIndex = M.fromList $ zip bioFlowUUIDs [0..]
        
        -- Build sparse triplets for all biosphere exchanges
        bioTriples = [ (i, j, amount)
                     | (j, actUUID) <- zip [0..] activityUUIDs  -- j = activity index
                     , Just activity <- [M.lookup actUUID allActivities]
                     , ex <- exchanges activity
                     , isBiosphereExchange ex
                     , Just i <- [M.lookup (exchangeFlowId ex) bioFlowIndex]  -- i = biosphere flow index
                     , let amount = if exchangeIsInput ex 
                                   then -(exchangeAmount ex)  -- Resource consumption (negative)
                                   else exchangeAmount ex     -- Emission (positive)
                     , abs amount > 1e-15  -- Only significant flows
                     ]
        
        -- Convert to dense matrix
        bioMatrix = buildMatrixFromTriples bioFlowCount activityCount bioTriples
        
    in bioMatrix

-- | Build biosphere matrix triplets (sparse coordinate format)
buildSparseBiosphereTriples :: ActivityDB -> [UUID] -> M.Map UUID Int -> [(Int, Int, Double)]
buildSparseBiosphereTriples activities actUUIDs activityIndex =
    let bioFlows = getAllBiosphereFlows activities
        bioFlowIndex = M.fromList $ zip bioFlows [0..]
    in [ (i, j, amount)
       | (j, actUUID) <- zip [0..] actUUIDs  -- j = activity index
       , Just activity <- [M.lookup actUUID activities]
       , ex <- exchanges activity
       , isBiosphereExchange ex
       , Just i <- [M.lookup (exchangeFlowId ex) bioFlowIndex]  -- i = biosphere flow index
       , let amount = if exchangeIsInput ex 
                     then -(exchangeAmount ex)  -- Resource consumption (negative)
                     else exchangeAmount ex     -- Emission (positive)
       , abs amount > 1e-15  -- Only significant flows
       ]

-- | Build dense matrix from sparse triplets (efficient for final solving)
buildMatrixFromTriples :: Int -> Int -> [(Int, Int, Double)] -> Matrix Double
buildMatrixFromTriples nRows nCols triplets =
    let elemMap = M.fromList [((i, j), val) | (i, j, val) <- triplets]
        -- Convert to row-major list format for hmatrix
        elements = [ M.findWithDefault 0.0 (i, j) elemMap | i <- [0..nRows-1], j <- [0..nCols-1] ]
    in (nRows><nCols) elements


-- | Build final demand vector f 
-- f[i] = external demand for product from activity i
buildDemandVector :: ActivityDB -> [UUID] -> UUID -> Vector Double
buildDemandVector activities actUUIDs rootUUID =
    let n = length actUUIDs
        indexMap = M.fromList $ zip actUUIDs [0..]
        rootIndex = fromMaybe 0 (M.lookup rootUUID indexMap)
    in fromList [ if i == rootIndex then 1.0 else 0.0 | i <- [0..n-1] ]

-- | Get all unique biosphere flows from entire database
getAllBiosphereFlowsFromDB :: Database -> [UUID]
getAllBiosphereFlowsFromDB db = 
    let bioFlows = S.fromList 
            [ exchangeFlowId ex 
            | activity <- M.elems (dbActivities db)
            , ex <- exchanges activity
            , isBiosphereExchange ex
            ]
    in S.toList bioFlows

-- | Get all unique biosphere flows from activities (legacy function)
getAllBiosphereFlows :: ActivityDB -> [UUID]
getAllBiosphereFlows activities = 
    let bioFlows = S.fromList 
            [ exchangeFlowId ex 
            | activity <- M.elems activities
            , ex <- exchanges activity
            , isBiosphereExchange ex
            ]
    in S.toList bioFlows

-- | Check if exchange is technosphere input (not reference product)
isTechnosphereInput :: FlowDB -> Exchange -> Bool  
isTechnosphereInput _ ex =
    case ex of
        TechnosphereExchange _ _ _ isInput isRef _ -> isInput && not isRef
        BiosphereExchange _ _ _ _ -> False