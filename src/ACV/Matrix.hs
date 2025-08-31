{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module ACV.Matrix (
    computeInventoryMatrix,
    buildDemandVector,
    buildDemandVectorFromIndex,
) where

import ACV.Types
import ACV.UnitConversion (convertExchangeAmount)
import Control.Monad (forM_)
import Control.Monad.ST
import Data.List (elemIndex, sortOn)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import qualified Data.Vector.Storable as VS
import Debug.Trace (trace)
import System.IO (hPutStrLn, hFlush, stderr)
import System.IO.Unsafe (unsafePerformIO)
import Data.Time (getCurrentTime, diffUTCTime)

-- PETSc imports
import Numerical.PETSc.Internal

-- | Simple vector operations (replacing hmatrix dependency)
type Vector = U.Vector Double

-- | Immediate trace with stderr flush - ensures output appears immediately
traceFlush :: String -> a -> a
traceFlush msg x = unsafePerformIO $ do
    hPutStrLn stderr msg
    hFlush stderr
    return x

-- Vector operations
norm2 :: Vector -> Double
norm2 v = sqrt $ U.sum $ U.map (^ 2) v

dot :: Vector -> Vector -> Double
dot v1 v2 = U.sum $ U.zipWith (*) v1 v2

scale :: Double -> Vector -> Vector
scale s v = U.map (* s) v

vectorAdd :: Vector -> Vector -> Vector
vectorAdd = U.zipWith (+)

vectorSub :: Vector -> Vector -> Vector
vectorSub = U.zipWith (-)

fromList :: [Double] -> Vector
fromList = U.fromList

toList :: Vector -> [Double]
toList = U.toList

-- | Convert sparse triplets to petsc-hs format 
buildPetscMatrixData :: [(Int, Int, Double)] -> Int -> (V.Vector (Int, Int, PetscScalar_))
buildPetscMatrixData triplets n = 
    let -- Convert to PETSc CDouble format
        petscTriplets = [(i, j, realToFrac val) | (i, j, val) <- triplets]
     in V.fromList petscTriplets

-- | Solve linear system (I - A) * x = b using PETSc direct solver
solveSparseLinearSystemPETSc :: [(Int, Int, Double)] -> Int -> Vector -> Vector
solveSparseLinearSystemPETSc techTriples n demandVec = unsafePerformIO $ do
    let _ = traceFlush ("🔧🔧🔧 PETSc DIRECT SOLVER ENTRY 🔧🔧🔧") ()
    let _ = traceFlush ("Building " ++ show n ++ "x" ++ show n ++ " system matrix for PETSc") ()

    -- Build (I - A) system from sparse triplets
    -- Add identity matrix: I[i,i] = 1.0
    let identityTriples = [(i, i, 1.0) | i <- [0 .. n - 1]]
    -- Subtract technosphere: (I - A)[i,j] = I[i,j] - A[i,j]
    let negTechTriples = [(i, j, -value) | (i, j, value) <- techTriples]
    -- Add small positive shift to the diagonal for numerical stability
    let shift = 1e-12
    let shiftedIdentity = [(i, i, shift) | i <- [0 .. n - 1]]
    let allTriples = identityTriples ++ shiftedIdentity ++ negTechTriples

    let _ = traceFlush ("System has " ++ show (length allTriples) ++ " non-zero entries") ()

    -- Convert to PETSc format
    let matrixData = buildPetscMatrixData allTriples n
    let rhsData = V.fromList $ map realToFrac $ toList demandVec
    let nzPattern = ConstNZPR (fromIntegral $ length allTriples, fromIntegral $ length allTriples)

    -- Use PETSc within the context
    result <- withPetsc0 $ do
        let _ = traceFlush ("Creating PETSc matrix and solving...") ()
        let comm = commWorld
        
        withPetscMatrix comm n n MatAij matrixData nzPattern InsertValues $ \mat ->
            withVecNew comm rhsData $ \rhs -> do
                let (_, _, _, matMutable) = fromPetscMatrix mat
                let _ = traceFlush ("🚀 Starting PETSc direct solve...") ()
                startTime <- getCurrentTime
                
                withKspSetupSolveAlloc comm KspRichardson matMutable matMutable rhs $ \ksp solution -> do
                    endTime <- getCurrentTime
                    let solveTime = realToFrac $ diffUTCTime endTime startTime
                    let _ = traceFlush ("✅ PETSc solve completed in " ++ show solveTime ++ "s") ()
                    
                    -- Extract solution as list
                    solutionData <- vecGetVS solution
                    let solutionList = VS.toList solutionData
                    return $ map realToFrac solutionList

    let _ = traceFlush ("🎉 PETSc direct solver completed successfully") ()
    return $ fromList result

-- | Solve linear system (I - A) * x = b using PETSc direct solver (wrapper)
solveSparseLinearSystem :: [(Int, Int, Double)] -> Int -> Vector -> Vector
solveSparseLinearSystem = solveSparseLinearSystemPETSc

-- | Efficient sparse matrix multiplication: result = A * vector
applySparseMatrix :: [(Int, Int, Double)] -> Int -> Vector -> Vector
applySparseMatrix sparseTriples nRows inputVec =
    let resultVec = U.create $ do
            result <- MU.new nRows
            -- Initialize to zero
            forM_ [0..nRows-1] $ \i -> MU.write result i 0.0
            
            -- Apply sparse matrix
            forM_ sparseTriples $ \(i, j, val) -> do
                if j < U.length inputVec then do
                    oldVal <- MU.read result i
                    let newVal = oldVal + val * (inputVec U.! j)
                    MU.write result i newVal
                else return ()
            
            return result
     in resultVec

{- | Matrix-based inventory calculation using sparse approach with PETSc
Uses pre-built sparse triplets, then solves: (I - A)^-1 * f
Where: I = identity, A = technosphere matrix, f = demand vector
-}
computeInventoryMatrix :: Database -> UUID -> M.Map UUID Double
computeInventoryMatrix db rootUUID =
    traceFlush ("🚀🚀🚀 MATRIX COMPUTATION ENTRY POINT REACHED 🚀🚀🚀") $
    traceFlush ("=== MATRIX INVENTORY CALCULATION START (PETSc) ===") $
    let activityCount = dbActivityCount db
        bioFlowCount = dbBiosphereCount db
        _ = traceFlush ("PETSc matrix calculation for " ++ show activityCount ++ " activities, " ++ show bioFlowCount ++ " biosphere flows") ()

        -- Use pre-built sparse coordinate lists from database
        _ = traceFlush ("Extracting pre-built sparse matrices from database...") ()
        techTriples = dbTechnosphereTriples db
        bioTriples = dbBiosphereTriples db
        activityIndex = dbActivityIndex db
        bioFlowUUIDs = dbBiosphereFlows db
        
        _ = traceFlush ("Matrix data extracted successfully") ()
        _ = traceFlush ("Tech matrix has " ++ show (length techTriples) ++ " non-zero elements") ()
        _ = traceFlush ("Bio matrix has " ++ show (length bioTriples) ++ " non-zero elements") ()

        -- Build demand vector for root activity
        _ = traceFlush ("Building demand vector for root activity...") ()
        demandVec = buildDemandVectorFromIndex activityIndex rootUUID

        -- Check matrix size before proceeding
        _ = if activityCount > 5000 
            then traceFlush ("ℹ️  Large matrix (" ++ show activityCount ++ "x" ++ show activityCount ++ "). Using PETSc direct solver.") ()
            else traceFlush ("✅ Matrix size is reasonable for direct solving") ()

        -- Solve sparse LCA equation: (I - A) * supply = demand using PETSc
        _ = traceFlush ("Starting PETSc direct linear system solving...") ()
        supplyVec = solveSparseLinearSystem techTriples activityCount demandVec
        _ = traceFlush ("PETSc solve completed") ()

        -- Calculate inventory using sparse biosphere matrix: g = B * supply
        _ = traceFlush ("Applying biosphere matrix multiplication...") ()
        inventoryVec = applySparseMatrix bioTriples bioFlowCount supplyVec

        _ = traceFlush ("Converting result to Map format...") ()
        result = M.fromList $ zip bioFlowUUIDs (toList inventoryVec)
        _ = traceFlush ("=== MATRIX INVENTORY CALCULATION COMPLETE (PETSc) ===") ()
     in result

{- | Build final demand vector f using pre-built activity index
f[i] = external demand for product from activity i
-}
buildDemandVectorFromIndex :: M.Map UUID Int -> UUID -> Vector
buildDemandVectorFromIndex activityIndex rootUUID =
    let n = M.size activityIndex
        rootIndex = fromMaybe 0 (M.lookup rootUUID activityIndex)
     in fromList [if i == rootIndex then 1.0 else 0.0 | i <- [0 .. n - 1]]

{- | Build final demand vector f (legacy function)
f[i] = external demand for product from activity i
-}
buildDemandVector :: ActivityDB -> [UUID] -> UUID -> Vector
buildDemandVector activities actUUIDs rootUUID =
    let n = length actUUIDs
        indexMap = M.fromList $ zip actUUIDs [0 ..]
        rootIndex = fromMaybe 0 (M.lookup rootUUID indexMap)
     in fromList [if i == rootIndex then 1.0 else 0.0 | i <- [0 .. n - 1]]