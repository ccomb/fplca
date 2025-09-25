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
import Data.Time (diffUTCTime, getCurrentTime)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Debug.Trace (trace)
import System.IO (hFlush, hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)

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

-- | Aggregate duplicate matrix entries by summing values for same (i,j) coordinates
aggregateMatrixEntries :: [(Int, Int, Double)] -> [(Int, Int, Double)]
aggregateMatrixEntries entries =
    let groupedEntries = M.fromListWith (+) [((i, j), val) | (i, j, val) <- entries]
     in [(i, j, val) | ((i, j), val) <- M.toList groupedEntries]

-- Vector operations
norm2 :: Vector -> Double
norm2 v = sqrt $ U.sum $ U.map (^ 2) v

dot :: Vector -> Vector -> Double
dot v1 v2 = U.sum $ U.zipWith (*) v1 v2

scale :: Double -> Vector -> Vector
scale s = U.map (* s)

vectorAdd :: Vector -> Vector -> Vector
vectorAdd = U.zipWith (+)

vectorSub :: Vector -> Vector -> Vector
vectorSub = U.zipWith (-)

fromList :: [Double] -> Vector
fromList = U.fromList

toList :: Vector -> [Double]
toList = U.toList

-- | Convert sparse triplets to petsc-hs format
buildPetscMatrixData :: [(Int, Int, Double)] -> Int -> V.Vector (Int, Int, PetscScalar_)
buildPetscMatrixData triplets n =
    let
        -- Convert to PETSc CDouble format
        petscTriplets = [(i, j, realToFrac val) | (i, j, val) <- triplets]
     in
        V.fromList petscTriplets

-- | Solve linear system (I - A) * x = b using PETSc MUMPS direct solver
solveSparseLinearSystemPETSc :: [(Int, Int, Double)] -> Int -> Vector -> Vector
solveSparseLinearSystemPETSc techTriples n demandVec = unsafePerformIO $ do
    -- Build (I - A) system from sparse triplets
    -- Add identity matrix: I[i,i] = 1.0
    let identityTriples = [(i, i, 1.0) | i <- [0 .. n - 1]]
    -- Subtract technosphere: (I - A)[i,j] = I[i,j] - A[i,j]
    let systemTechTriples = [(i, j, -value) | (i, j, value) <- techTriples]
    let allTriples = identityTriples ++ systemTechTriples

    -- Aggregate duplicate entries for proper matrix assembly
    let aggregatedTriples = aggregateMatrixEntries allTriples

    let !_ = traceFlush ("Matrix construction completed. Starting PETSc direct solve for " ++ show n ++ " activities...") ()

    -- Convert to PETSc format
    let matrixData = buildPetscMatrixData aggregatedTriples n
    let rhsData = V.fromList $ map realToFrac $ toList demandVec
    let nzPattern = ConstNZPR (fromIntegral $ length aggregatedTriples, fromIntegral $ length aggregatedTriples)

    let !_ = traceFlush ("Starting PETSc direct solve for " ++ show n ++ " activities...") ()

    -- Use PETSc within the context
    result <- withPetsc0 $ do
        let comm = commWorld

        withPetscMatrix comm n n MatAij matrixData nzPattern InsertValues $ \mat ->
            withVecNew comm rhsData $ \rhs -> do
                let (_, _, _, matMutable) = fromPetscMatrix mat
                startTime <- getCurrentTime

                -- Use PETSC_OPTIONS for solver configuration (supports MUMPS, SuperLU, etc.)
                withKspSetupSolveAllocFromOptions comm matMutable matMutable False rhs $ \ksp solution -> do
                    endTime <- getCurrentTime
                    let solveTime = realToFrac $ diffUTCTime endTime startTime
                    let !_ = traceFlush ("MUMPS direct solve completed in " ++ show solveTime ++ " seconds") ()

                    -- Extract solution as list
                    solutionData <- vecGetVS solution
                    let solutionList = VS.toList solutionData
                    let solutionResult = map realToFrac solutionList
                    return solutionResult

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
            forM_ [0 .. nRows - 1] $ \i -> MU.write result i 0.0

            -- Apply sparse matrix
            forM_ sparseTriples $ \(i, j, val) -> do
                if j < U.length inputVec
                    then do
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
    let activityCount = dbActivityCount db
        bioFlowCount = dbBiosphereCount db

        -- Use pre-built sparse coordinate lists from database
        techTriples = dbTechnosphereTriples db
        bioTriples = dbBiosphereTriples db
        activityIndex = dbActivityIndex db
        bioFlowUUIDs = dbBiosphereFlows db

        -- Build demand vector for root activity
        demandVec = buildDemandVectorFromIndex activityIndex rootUUID

        -- Solve sparse LCA equation: (I - A) * supply = demand using PETSc
        supplyVec = solveSparseLinearSystem techTriples activityCount demandVec

        -- Calculate inventory using sparse biosphere matrix: g = B * supply
        inventoryVec = applySparseMatrix bioTriples bioFlowCount supplyVec

        result = M.fromList $ zip bioFlowUUIDs (toList inventoryVec)
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
