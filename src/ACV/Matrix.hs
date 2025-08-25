{-# LANGUAGE OverloadedStrings #-}

module ACV.Matrix 
    ( computeInventoryMatrix
    , buildTechnosphereMatrix
    , buildBiosphereMatrix
    , buildDemandVector
    ) where

import ACV.Types
import ACV.UnitConversion (convertExchangeAmount)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (elemIndex, sortOn)
import Data.Maybe (fromMaybe, mapMaybe, fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import Numeric.LinearAlgebra

-- | Matrix-based inventory calculation following standard LCA methodology
-- Solves: A * supply = demand, then g = B * supply  
-- Where: A = technosphere matrix, B = biosphere matrix, supply = scaling factors
computeInventoryMatrix :: Database -> UUID -> M.Map UUID Double
computeInventoryMatrix db rootUUID = 
    let activities = getAllReachableActivities db rootUUID
        activityList = M.elems activities
        activityUUIDs = M.keys activities
        
        -- Debug: check what we found
        _ = if length activities == 0 
            then error $ "No reachable activities found from root: " ++ show rootUUID
            else ()
        
        -- Build matrices using standard LCA approach
        techMatrix = buildTechnosphereMatrix activities activityUUIDs
        bioMatrix = buildBiosphereMatrix activities activityUUIDs  
        demandVec = buildDemandVector activities activityUUIDs rootUUID
        
        -- Debug: check matrix dimensions
        _ = if rows techMatrix == 0 || cols techMatrix == 0
            then error $ "Empty technosphere matrix: " ++ show (rows techMatrix) ++ "x" ++ show (cols techMatrix)
            else ()
        
        -- Solve linear system: A * supply = demand
        -- Supply vector contains scaling factors for each process
        supplyVec = techMatrix <\> demandVec
        
        -- Note: supplyVec contains scaling factors, including cycle resolution
            
        -- Calculate inventory: g = B * supply (standard LCA calculation)
        inventoryVec = bioMatrix #> supplyVec
        
        -- Get biosphere flow UUIDs
        bioFlowUUIDs = getAllBiosphereFlows activities
        
    in M.fromList $ zip bioFlowUUIDs (toList inventoryVec)

-- | Get all activities reachable from root (using technosphere inputs only)
getAllReachableActivities :: Database -> UUID -> ActivityDB
getAllReachableActivities db rootUUID = 
    let go visited actUUID
            | S.member actUUID visited = M.empty
            | otherwise = 
                case M.lookup actUUID (dbActivities db) of
                    Nothing -> M.empty
                    Just activity ->
                        let visited' = S.insert actUUID visited
                            -- Get technosphere inputs
                            techInputs = [ ex | ex <- exchanges activity
                                         , isTechnosphereInput (dbFlows db) ex ]
                            -- Recursively get connected activities
                            childActivities = M.unions 
                                [ go visited' targetUUID 
                                | ex <- techInputs
                                , Just targetUUID <- [exchangeActivityLinkId ex]
                                , M.member targetUUID (dbActivities db)
                                ]
                        in M.insert actUUID activity childActivities
    in go S.empty rootUUID

-- | Build technosphere matrix A (activities × activities)
-- A[i,j] = net flow from activity i to activity j
--        = positive for outputs (what i produces for j)
--        = negative for inputs (what i consumes from j)
buildTechnosphereMatrix :: ActivityDB -> [UUID] -> Matrix Double  
buildTechnosphereMatrix activities actUUIDs =
    let n = length actUUIDs
        indexMap = M.fromList $ zip actUUIDs [0..]
        
        -- Build matrix directly from exchange data
        matrixElements i j =
            let producerUUID = actUUIDs !! i  -- Activity that could be producing
                consumerUUID = actUUIDs !! j  -- Activity that could be consuming
            in case M.lookup consumerUUID activities of
                Nothing -> 0.0
                Just consumerActivity ->
                    -- Sum all flows between producer i and consumer j
                    sum [ exchangeFlow
                        | ex <- exchanges consumerActivity
                        , isTechnosphereExchange ex
                        , let exchangeFlow = 
                                if exchangeIsReference ex && consumerUUID == producerUUID
                                then exchangeAmount ex  -- Positive: what this activity produces
                                else case exchangeActivityLinkId ex of
                                       Just inputActivityUUID | inputActivityUUID == producerUUID ->
                                           -(exchangeAmount ex)  -- Negative: what this activity consumes from producer
                                       _ -> 0.0
                        , exchangeFlow /= 0.0
                        ]
        
    in (n><n) [ matrixElements i j | i <- [0..n-1], j <- [0..n-1] ]

-- | Build biosphere matrix B (elementary flows × activities)  
-- B[i,j] = amount of elementary flow i emitted by 1 unit of activity j
buildBiosphereMatrix :: ActivityDB -> [UUID] -> Matrix Double
buildBiosphereMatrix activities actUUIDs =
    let bioFlows = getAllBiosphereFlows activities
        m = length bioFlows
        n = length actUUIDs
        
        -- Build matrix using element function
        matrixElements i j =
            case M.lookup (actUUIDs !! j) activities of
                Nothing -> 0.0
                Just activity ->
                    -- Sum all biosphere exchanges of flow i from activity j
                    sum [ if exchangeIsInput ex 
                          then -(exchangeAmount ex)  -- Resource consumption (negative)
                          else exchangeAmount ex     -- Emission (positive)
                        | ex <- exchanges activity
                        , isBiosphereExchange ex
                        , exchangeFlowId ex == (bioFlows !! i)
                        ]
        
    in (m><n) [ matrixElements i j | i <- [0..m-1], j <- [0..n-1] ]

-- | Build final demand vector f 
-- f[i] = external demand for product from activity i
buildDemandVector :: ActivityDB -> [UUID] -> UUID -> Vector Double
buildDemandVector activities actUUIDs rootUUID =
    let n = length actUUIDs
        indexMap = M.fromList $ zip actUUIDs [0..]
        rootIndex = fromMaybe 0 (M.lookup rootUUID indexMap)
    in fromList [ if i == rootIndex then 1.0 else 0.0 | i <- [0..n-1] ]

-- | Get all unique biosphere flows from activities
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