{-# LANGUAGE OverloadedStrings #-}

module ACV.Matrix 
    ( computeInventoryMatrix
    , buildDemandVector
    , buildDemandVectorFromIndex
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
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST
import Control.Monad (forM_)

-- | Compressed Row Storage (CRS) format for efficient sparse operations
data CRSMatrix = CRSMatrix
    { crsValues :: !(V.Vector Double)     -- Non-zero values
    , crsColIndices :: !(V.Vector Int)    -- Column indices for each value
    , crsRowPointers :: !(V.Vector Int)   -- Start index of each row in values array
    , crsNumRows :: !Int                  -- Number of rows
    , crsNumCols :: !Int                  -- Number of columns
    } deriving (Show)

-- | Convert sparse triplets to CRS format
buildCRSMatrix :: [(Int, Int, Double)] -> Int -> Int -> CRSMatrix
buildCRSMatrix triplets nRows nCols = 
    let -- Sort triplets by row first, then by column
        sortedTriplets = sortOn (\(i, j, _) -> (i, j)) triplets
        -- Extract components
        (rows, cols, vals) = unzip3 sortedTriplets
        
        -- Build row pointers array: rowPtrs[i] = start index of row i in vals array
        rowPtrs = V.create $ do
            ptrs <- MV.new (nRows + 1)
            MV.set ptrs 0  -- Initialize all to 0
            
            -- Count elements per row first
            rowCounts <- MV.new nRows
            MV.set rowCounts 0
            forM_ rows $ \row -> do
                count <- MV.read rowCounts row
                MV.write rowCounts row (count + 1)
            
            -- Build cumulative row pointers
            MV.write ptrs 0 0
            forM_ [1..nRows] $ \i -> do
                prevPtr <- MV.read ptrs (i-1)
                rowCount <- MV.read rowCounts (i-1)
                MV.write ptrs i (prevPtr + rowCount)
            
            return ptrs
            
    in CRSMatrix
        { crsValues = V.fromList vals
        , crsColIndices = V.fromList cols
        , crsRowPointers = rowPtrs
        , crsNumRows = nRows
        , crsNumCols = nCols
        }

-- | Efficient sparse matrix-vector multiplication: y = A * x
sparseMatVec :: CRSMatrix -> Vector Double -> Vector Double
sparseMatVec crs x =
    let n = crsNumRows crs
        xVec = V.fromList (toList x)
    in fromList $ V.toList $ V.create $ do
        result <- MV.new n
        MV.set result 0.0  -- Initialize to zero
        
        forM_ [0..n-1] $ \i -> do
            let rowStart = crsRowPointers crs V.! i
            let rowEnd = crsRowPointers crs V.! (i + 1)
            
            -- Compute dot product for row i
            let rowSum = sum [ (crsValues crs V.! k) * (xVec V.! (crsColIndices crs V.! k))
                             | k <- [rowStart..rowEnd-1] ]
            MV.write result i rowSum
            
        return result

-- | Extract diagonal elements for preconditioning
extractDiagonal :: CRSMatrix -> Vector Double
extractDiagonal crs = 
    let n = crsNumRows crs
    in fromList $ map getDiagElement [0..n-1]
  where
    getDiagElement i =
        let rowStart = crsRowPointers crs V.! i
            rowEnd = crsRowPointers crs V.! (i + 1)
            findDiag k
                | k >= rowEnd = 1.0  -- Default to 1.0 if no diagonal found
                | (crsColIndices crs V.! k) == i = crsValues crs V.! k
                | otherwise = findDiag (k + 1)
        in findDiag rowStart

-- | Apply diagonal preconditioning: x_new = D^(-1) * x
applyDiagonalPreconditioning :: Vector Double -> Vector Double -> Vector Double
applyDiagonalPreconditioning diag x = 
    let diagInv = cmap (\d -> if abs d > 1e-14 then 1.0/d else 1.0) diag
    in diagInv * x

-- | BiCGSTAB solver with diagonal preconditioning for sparse linear systems Ax = b
-- More efficient than Gauss-Seidel, suitable for non-symmetric matrices
solveBiCGSTAB :: CRSMatrix -> Vector Double -> Vector Double -> Double -> Int -> Either String (Vector Double)
solveBiCGSTAB a b x0 tol maxIter =
    let n = crsNumRows a
        -- Extract diagonal for preconditioning
        diag = extractDiagonal a
        _ = trace ("BiCGSTAB: Starting solver for " ++ show n ++ "x" ++ show n ++ " system") ()
        
        -- Initial guess and residual
        r0 = b - sparseMatVec a x0
        r0Hat = r0  -- Arbitrary vector, we choose r0Hat = r0
        
        -- BiCGSTAB iteration with progress tracking
        bicgstabIter x r v p rho alpha omega s t iter
            | iter >= maxIter = 
                let _ = trace ("BiCGSTAB: Failed to converge in " ++ show maxIter ++ " iterations, residual=" ++ show (norm_2 r)) ()
                in Left $ "BiCGSTAB failed to converge in " ++ show maxIter ++ " iterations"
            | norm_2 r < tol = 
                let _ = trace ("BiCGSTAB: Converged in " ++ show iter ++ " iterations, residual=" ++ show (norm_2 r)) ()
                in Right x  -- Converged!
            | otherwise =
                let -- Progress reporting every 10 iterations
                    _ = if iter `mod` 10 == 0 
                        then trace ("BiCGSTAB: iter " ++ show iter ++ ", residual=" ++ show (norm_2 r)) ()
                        else ()
                        
                    -- Compute rho_i = <r0Hat, r_i>
                    rhoNew = r0Hat <.> r
                    
                    -- Check for breakdown
                    _ = if abs rhoNew < 1e-14 then error "BiCGSTAB breakdown: rho too small" else ()
                    
                    -- Update p
                    beta = (rhoNew / rho) * (alpha / omega)
                    pNew = r + scale beta (p - scale omega v)
                    
                    -- Compute v = A * p
                    vNew = sparseMatVec a pNew
                    
                    -- Compute alpha
                    alphaNew = rhoNew / (r0Hat <.> vNew)
                    
                    -- Update s and check for convergence
                    sNew = r - scale alphaNew vNew
                    
                in if norm_2 sNew < tol
                   then Right (x + scale alphaNew pNew)  -- Early convergence
                   else 
                       let -- Compute t = A * s
                           tNew = sparseMatVec a sNew
                           
                           -- Compute omega
                           omegaNew = (tNew <.> sNew) / (tNew <.> tNew)
                           
                           -- Update solution and residual
                           xNew = x + scale alphaNew pNew + scale omegaNew sNew
                           rNew = sNew - scale omegaNew tNew
                           
                       in bicgstabIter xNew rNew vNew pNew rhoNew alphaNew omegaNew sNew tNew (iter + 1)
        
    in if norm_2 r0 < tol
       then Right x0  -- Already converged
       else bicgstabIter x0 r0 (fromList $ replicate n 0) r0 1.0 1.0 1.0 
                         (fromList $ replicate n 0) (fromList $ replicate n 0) 0

-- | Matrix-based inventory calculation using sparse approach
-- Uses pre-built sparse triplets, then solves: (I - A)^-1 * f
-- Where: I = identity, A = technosphere matrix, f = demand vector
computeInventoryMatrix :: Database -> UUID -> M.Map UUID Double
computeInventoryMatrix db rootUUID = 
    let activityCount = dbActivityCount db
        bioFlowCount = dbBiosphereCount db
        
        _ = trace ("=== MATRIX INVENTORY CALCULATION START ===") ()
        _ = trace ("Sparse matrix calculation for " ++ show activityCount ++ " activities, " ++ show bioFlowCount ++ " biosphere flows") ()
        
        -- Use pre-built sparse coordinate lists from database
        _ = trace ("Extracting pre-built sparse matrices from database...") ()
        techTriples = dbTechnosphereTriples db
        bioTriples = dbBiosphereTriples db
        activityIndex = dbActivityIndex db  
        bioFlowUUIDs = dbBiosphereFlows db
        
        _ = trace ("Tech matrix has " ++ show (length techTriples) ++ " non-zero elements") ()
        _ = trace ("Bio matrix has " ++ show (length bioTriples) ++ " non-zero elements") ()
        
        -- Build demand vector for root activity
        _ = trace ("Building demand vector for root activity...") ()
        demandVec = buildDemandVectorFromIndex activityIndex rootUUID
        
        -- Solve sparse LCA equation: (I - A) * supply = demand
        _ = trace ("Starting sparse linear system solving...") ()
        supplyVec = solveSparseLinearSystem techTriples activityCount demandVec
        
        -- Calculate inventory using sparse biosphere matrix: g = B * supply
        _ = trace ("Applying biosphere matrix multiplication...") ()
        inventoryVec = applySparseMatrix bioTriples bioFlowCount supplyVec
        
        _ = trace ("Converting result to Map format...") ()
        result = M.fromList $ zip bioFlowUUIDs (toList inventoryVec)
        _ = trace ("=== MATRIX INVENTORY CALCULATION COMPLETE ===") ()
        
    in result

-- | Solve sparse linear system (I - A) * x = b 
-- Uses dense solving for small matrices, efficient sparse BiCGSTAB for large matrices
solveSparseLinearSystem :: [(Int, Int, Double)] -> Int -> Vector Double -> Vector Double
solveSparseLinearSystem techTriples n demandVec =
    let -- Build (I - A) system from sparse triplets
        -- Add identity matrix: I[i,i] = 1.0
        identityTriples = [(i, i, 1.0) | i <- [0..n-1]]
        -- Subtract technosphere: (I - A)[i,j] = I[i,j] - A[i,j]
        negTechTriples = [(i, j, -value) | (i, j, value) <- techTriples]
        allTriples = identityTriples ++ negTechTriples
        
    in if n <= 1000  -- Conservative threshold - sparse solving is still challenging for very large systems
       then -- Small matrices: use dense solving (fast and stable)
           let systemMatrix = buildMatrixFromTriples n n allTriples
               supplyVec = systemMatrix <\> demandVec
           in supplyVec
       else -- Large matrices: use efficient sparse BiCGSTAB solver
           let _ = trace ("Building CRS matrix for " ++ show n ++ " x " ++ show n ++ " system with " ++ show (length allTriples) ++ " non-zeros") ()
               -- Convert to CRS format for efficient operations
               systemCRS = buildCRSMatrix allTriples n n
               _ = trace ("CRS matrix built, starting BiCGSTAB solver") ()
               -- Better initial guess: start with zero vector
               initialGuess = fromList (replicate n 0.0)
               -- Solver parameters
               maxIter = 100  -- Reduce max iterations for faster testing
               tolerance = 1e-8  -- Slightly relaxed tolerance
               
           in case solveBiCGSTAB systemCRS demandVec initialGuess tolerance maxIter of
                Left errorMsg -> 
                    let _ = trace ("BiCGSTAB solver error: " ++ errorMsg ++ ", falling back to dense solver") ()
                        systemMatrix = buildMatrixFromTriples n n allTriples
                    in systemMatrix <\> demandVec  -- Fallback to dense
                Right solution -> solution

-- | Efficient sparse matrix multiplication using CRS format: result = A * vector
applySparseMatrix :: [(Int, Int, Double)] -> Int -> Vector Double -> Vector Double  
applySparseMatrix sparseTriples nRows inputVec =
    let -- Convert to CRS format for efficient multiplication
        crs = buildCRSMatrix sparseTriples nRows nRows
    in sparseMatVec crs inputVec

-- | Build dense matrix from sparse triplets (helper function)
buildMatrixFromTriples :: Int -> Int -> [(Int, Int, Double)] -> Matrix Double
buildMatrixFromTriples nRows nCols triplets =
    let elemMap = M.fromList [((i, j), val) | (i, j, val) <- triplets]
        -- Convert to row-major list format for hmatrix
        elements = [ M.findWithDefault 0.0 (i, j) elemMap | i <- [0..nRows-1], j <- [0..nCols-1] ]
    in (nRows><nCols) elements



-- | Build final demand vector f using pre-built activity index
-- f[i] = external demand for product from activity i  
buildDemandVectorFromIndex :: M.Map UUID Int -> UUID -> Vector Double
buildDemandVectorFromIndex activityIndex rootUUID =
    let n = M.size activityIndex
        rootIndex = fromMaybe 0 (M.lookup rootUUID activityIndex)
    in fromList [ if i == rootIndex then 1.0 else 0.0 | i <- [0..n-1] ]

-- | Build final demand vector f (legacy function)
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