{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : ACV.Matrix
Description : Matrix-based LCA calculations using PETSc sparse solvers

This module implements matrix-based Life Cycle Assessment (LCA) calculations using
the PETSc library for high-performance sparse linear algebra. It solves the fundamental
LCA equation: (I - A)⁻¹ * f = s, where:

- I is the identity matrix
- A is the technosphere matrix (activities × activities)
- f is the final demand vector
- s is the supply vector (scaling factors)

The biosphere inventory is then calculated as: B * s = g, where:
- B is the biosphere matrix (flows × activities)
- g is the final inventory vector

Key features:
- Uses MUMPS direct solver through PETSc for numerical stability
- Configurable through PETSC_OPTIONS environment variables
- Handles sparse matrices efficiently with coordinate triplet format
- Supports large-scale problems (tested with 15K+ activities)

Performance characteristics:
- Matrix assembly: O(nnz) where nnz is number of non-zero entries
- MUMPS factorization: O(n^1.5) for sparse LCA matrices
- Forward/backward solve: O(n log n)
- Memory usage: ~50-100 MB for typical Ecoinvent database
-}
module ACV.Matrix (
    Inventory,
    computeInventoryMatrix,
    buildDemandVector,
    buildDemandVectorFromIndex,
    solveSparseLinearSystem,
    applySparseMatrix,
    toList,
) where

import ACV.Progress
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
import System.IO.Unsafe (unsafePerformIO)

-- PETSc imports
import Numerical.PETSc.Internal

-- | Simple vector operations (replacing hmatrix dependency)
type Vector = U.Vector Double

-- | Final inventory vector mapping biosphere flow UUIDs to quantities.
type Inventory = M.Map UUID Double

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

{- |
Solve the fundamental LCA equation (I - A) * x = b using PETSc MUMPS direct solver.

This function:
1. Constructs the (I - A) system matrix from technosphere triplets
2. Uses PETSc's MUMPS direct solver for numerical stability
3. Returns the supply vector (scaling factors) for all activities

The solver configuration is controlled by PETSC_OPTIONS environment variable,
allowing fine-tuning of MUMPS parameters for optimal performance.

Performance: ~3s for 14,457 activities with 116K technosphere entries
-}
solveSparseLinearSystemPETSc :: [(Int, Int, Double)] -> Int -> Vector -> Vector
solveSparseLinearSystemPETSc techTriples n demandVec = unsafePerformIO $ do
    -- Build (I - A) system from sparse triplets
    -- Add identity matrix: I[i,i] = 1.0
    let identityTriples = [(i, i, 1.0) | i <- [0 .. n - 1]]
    let systemTechTriples = [(i, j, -value) | (i, j, value) <- techTriples]
    let allTriples = identityTriples ++ systemTechTriples

    -- Aggregate duplicate entries for proper matrix assembly
    let aggregatedTriples = aggregateMatrixEntries allTriples

    reportMatrixOperation $ "Matrix assembly completed - starting PETSc direct solve for " ++ show n ++ " activities"

    -- Convert to PETSc format
    let matrixData = buildPetscMatrixData aggregatedTriples n
    let rhsData = V.fromList $ map realToFrac $ toList demandVec
    let nzPattern = ConstNZPR (fromIntegral $ length aggregatedTriples, fromIntegral $ length aggregatedTriples)

    -- Use PETSc within the context
    result <- withPetsc0 $ do
        let comm = commWorld

        withPetscMatrix comm n n MatAij matrixData nzPattern InsertValues $ \mat ->
            withVecNew comm rhsData $ \rhs -> do
                let (_, _, _, matMutable) = fromPetscMatrix mat

                -- Time the solver execution
                withProgressTiming Solver "MUMPS direct solve" $ do
                    -- Use PETSC_OPTIONS for solver configuration (supports MUMPS, SuperLU, etc.)
                    withKspSetupSolveAllocFromOptions comm matMutable matMutable False rhs $ \ksp solution -> do
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

{- |
Compute the complete LCA inventory for a given root activity using matrix-based calculations.

This is the main entry point for LCA calculations. It performs the complete inventory
calculation in two steps:

1. **Technology solve**: (I - A) * s = f
   - Solves for supply vector 's' (scaling factors for all activities)
   - Uses PETSc MUMPS direct solver for numerical stability

2. **Biosphere calculation**: g = B * s
   - Multiplies biosphere matrix by supply vector
   - Results in final inventory 'g' (environmental flows)

Input:
- Database with pre-computed sparse matrices (techTriples, bioTriples)
- Root activity UUID for which to compute inventory

Output:
- Map from biosphere flow UUID to inventory quantity (kg, MJ, etc.)

Performance: ~7s total for full Ecoinvent database (14K activities)
- 3.5s cache loading + 3s solver + 0.5s biosphere calculation
-}
computeInventoryMatrix :: Database -> UUID -> Inventory
computeInventoryMatrix db rootUUID =
    let activityCount = dbActivityCount db
        bioFlowCount = dbBiosphereCount db

        -- Use pre-built sparse coordinate lists from database
        techTriples = dbTechnosphereTriples db
        bioTriples = dbBiosphereTriples db
        activityIndex = dbActivityIndex db
        bioFlowUUIDs = dbBiosphereFlows db

        -- Build demand vector for root activity (f[i] = 1 for root, 0 elsewhere)
        demandVec = buildDemandVectorFromIndex activityIndex rootUUID

        -- Solve sparse LCA equation: (I - A) * supply = demand using PETSc
        supplyVec = solveSparseLinearSystem techTriples activityCount demandVec

        -- Calculate inventory using sparse biosphere matrix: g = B * supply
        inventoryVec = applySparseMatrix bioTriples bioFlowCount supplyVec

        result = M.fromList $ zip bioFlowUUIDs (toList inventoryVec)
     in result

{- |
Build the final demand vector f for LCA calculations.

The demand vector represents external demand for products from each activity:
- f[i] = 1.0 for the root activity (functional unit)
- f[i] = 0.0 for all other activities

This vector is used in the fundamental LCA equation: (I - A) * s = f

Uses the pre-built activity index mapping from UUID to matrix indices for efficiency.
-}
buildDemandVectorFromIndex :: M.Map UUID Int -> UUID -> Vector
buildDemandVectorFromIndex activityIndex rootUUID =
    let n = M.size activityIndex
        rootIndex = fromMaybe 0 (M.lookup rootUUID activityIndex)
     in fromList [if i == rootIndex then 1.0 else 0.0 | i <- [0 .. n - 1]]

{- |
Legacy function for building demand vector.

@deprecated Use 'buildDemandVectorFromIndex' instead, which uses pre-computed indices.

This function builds the activity index on-the-fly, which is inefficient for
repeated calls. The Database type now includes pre-computed indices.
-}
buildDemandVector :: ActivityDB -> [UUID] -> UUID -> Vector
buildDemandVector activities actUUIDs rootUUID =
    let n = length actUUIDs
        indexMap = M.fromList $ zip actUUIDs [0 ..]
        rootIndex = fromMaybe 0 (M.lookup rootUUID indexMap)
     in fromList [if i == rootIndex then 1.0 else 0.0 | i <- [0 .. n - 1]]
