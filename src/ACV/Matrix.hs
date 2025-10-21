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
    Vector,
    Inventory,
    computeInventoryMatrix,
    buildDemandVector,
    buildDemandVectorFromIndex,
    solveSparseLinearSystem,
    applySparseMatrix,
    fromList,
    toList,
    initializePetscForServer,
    finalizePetscForServer,
    precomputeMatrixFactorization,
    addFactorizationToDatabase,
    solveSparseLinearSystemWithFactorization,
    -- Worker-specific functions for thread pool
    initializePetscForWorker,
    finalizePetscForWorker,
    solveSparseLinearSystemForWorker,
    createFactorizedKspForWorker,
) where

import ACV.Progress
import ACV.Types
import ACV.UnitConversion (convertExchangeAmount)
import Control.Exception (catch, SomeException)
import Control.Monad (forM_, when, unless)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
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
import Data.IORef
import Control.Concurrent.MVar
import System.IO.Unsafe (unsafePerformIO)

-- | Simple vector operations (replacing hmatrix dependency)
type Vector = U.Vector Double

-- | Final inventory vector mapping biosphere flow UUIDs to quantities.
type Inventory = M.Map UUID Double

-- | Global PETSc initialization state to avoid re-initialization issues
{-# NOINLINE petscInitialized #-}
petscInitialized :: IORef Bool
petscInitialized = unsafePerformIO $ newIORef False

-- | Global cache for pre-factorized KSP solver with thread synchronization
{-# NOINLINE cachedKspSolver #-}
cachedKspSolver :: MVar (Maybe (KSP, PetscMatrix PetscScalar_, Int))
cachedKspSolver = unsafePerformIO $ newMVar Nothing

-- Global mutex to serialize all PETSc operations (matrix assembly, solving, etc.)
-- This prevents concurrent PETSc operations that can cause stack corruption
{-# NOINLINE petscGlobalMutex #-}
petscGlobalMutex :: MVar ()
petscGlobalMutex = unsafePerformIO $ newMVar ()

-- | Global MPI/PETSc initialization - call once and keep alive
{-# NOINLINE petscGlobalInit #-}
petscGlobalInit :: IO ()
petscGlobalInit = do
    initialized <- readIORef petscInitialized
    unless initialized $ do
        petscInit0  -- Initialize PETSc/MPI without automatic finalization
        writeIORef petscInitialized True

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
Fast solver using the globally cached pre-factorized KSP solver.

This function uses the cached KSP solver with pre-computed MUMPS factorization,
eliminating both matrix assembly and factorization time for concurrent requests.
Achieves sub-second inventory calculations after server startup.
-}
solveSparseLinearSystemWithFactorization :: MatrixFactorization -> Vector -> Vector
solveSparseLinearSystemWithFactorization factorization demandVec = unsafePerformIO $ do
    -- Check for globally cached pre-factorized solver with thread synchronization
    cachedSolver <- readMVar cachedKspSolver
    case cachedSolver of
        Nothing -> do
            -- Fallback to standard solver if no cached factorization available
            reportMatrixOperation "No cached factorization found, falling back to matrix assembly"
            let systemMatrix = mfSystemMatrix factorization
                n = mfActivityCount factorization
                -- Convert system matrix back to technosphere triplets (remove identity entries)
                techTriples = [(i, j, -val) | (i, j, val) <- systemMatrix, i /= j]
            return $ solveSparseLinearSystemPETSc techTriples n demandVec

        Just (ksp, petscMat, n) -> do
            reportMatrixOperation $ "Using globally cached pre-factorized solver for " ++ show n ++ " activities - ultra-fast solve"

            -- Time the ultra-fast solve with thread synchronization and error handling
            result <- withProgressTiming Solver "PETSc thread-safe cached solve" $ do
                -- Use withMVar to ensure thread-safe access to the cached KSP solver
                result <- catch
                    (withMVar petscGlobalMutex $ \_ -> do
                        cachedSolver <- readMVar cachedKspSolver
                        case cachedSolver of
                            Nothing -> error "Cached solver disappeared during solve"
                            Just (kspSolver, _, _) -> do
                                -- Create fresh vectors for each request to avoid shared state
                                let rhsData = V.fromList $ map realToFrac $ toList demandVec
                                let comm = commWorld

                                withVecNew comm rhsData $ \rhs -> do
                                    withVecDuplicate rhs $ \solution -> do
                                        -- Use the pre-factorized solver with thread safety
                                        kspSolve kspSolver rhs solution
                                        solutionData <- vecGetVS solution
                                        let solutionList = VS.toList solutionData
                                        return $ Just $ map realToFrac solutionList)
                    (\e -> do
                        reportMatrixOperation $ "MUMPS cached solver failed (broken pipe/concurrency issue): " ++ show (e :: SomeException)
                        reportMatrixOperation "Falling back to fresh solver for this request"
                        return Nothing)

                case result of
                    Just solutionList -> return solutionList
                    Nothing -> do
                        -- Fallback to standard solver if cached solver fails
                        let systemMatrix = mfSystemMatrix factorization
                            techTriples = [(i, j, -val) | (i, j, val) <- systemMatrix, i /= j]
                        return $ toList $ solveSparseLinearSystemPETSc techTriples n demandVec

            return $ fromList result

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
    -- Serialize ALL PETSc operations to prevent concurrent matrix assembly/solving
    withMVar petscGlobalMutex $ \_ -> do
        -- Ensure PETSc is initialized globally (once and persistent)
        petscGlobalInit

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

        -- Use PETSc within the already initialized context (no re-initialization)
        let comm = commWorld
        result <- withPetscMatrix comm n n MatAij matrixData nzPattern InsertValues $ \mat ->
            withVecNew comm rhsData $ \rhs -> do
                let (_, _, _, matMutable) = fromPetscMatrix mat

                -- Time the solver execution with enhanced error handling
                withProgressTiming Solver "PETSc solve with environment options" $ do
                    -- Use explicit KSP setup to ensure PETSC_OPTIONS are consumed properly
                    withKsp comm $ \ksp -> do
                        -- Set up operators first
                        kspSetOperators ksp matMutable matMutable

                        -- Explicitly consume PETSC_OPTIONS environment variables
                        kspSetFromOptions ksp
                        pc <- kspGetPC ksp
                        pcSetFromOptions pc

                        -- Debug: Report that options were applied
                        reportSolverOperation "Applied PETSC_OPTIONS to KSP and PC"

                        -- Configure solver
                        kspSetInitialGuessNonzero ksp False
                        kspSetUp ksp

                        -- Allocate solution vector and solve
                        withVecDuplicate rhs $ \solution -> do
                            kspSolve ksp rhs solution

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
        -- Try to use cached factorization for faster solving
        supplyVec = case dbCachedFactorization db of
            Just factorization -> solveSparseLinearSystemWithFactorization factorization demandVec
            Nothing -> solveSparseLinearSystem techTriples activityCount demandVec

        -- Calculate inventory using sparse biosphere matrix: g = B * supply
        inventoryVec = applySparseMatrix bioTriples bioFlowCount supplyVec

        result = M.fromList $ zip bioFlowUUIDs (toList inventoryVec)
     in result

{- |
Compute inventory matrix using worker pool for thread-safe concurrent processing.

This function uses the worker pool instead of direct PETSc calls to avoid MUMPS
thread-safety issues. Each request is submitted to an available worker thread
which has its own isolated PETSc context.

Key differences from computeInventoryMatrix:
- Thread-safe: Uses worker pool with isolated contexts per worker
- Concurrent: Multiple inventory calculations can run simultaneously
- No shared state: Each worker operates independently
- Performance: Leverages parallel processing capabilities

The computation is identical to the direct version:
1. Extract matrix data from database
2. Build demand vector for target activity
3. Submit work to worker pool (thread-safe MUMPS solver)
4. Apply biosphere matrix to calculate inventory
-}

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

{- |
Initialize PETSc once for the entire server lifetime.
This prevents the MPI re-initialization issue by keeping the PETSc/MPI context alive.
Should be called once during server startup.
-}
initializePetscForServer :: IO ()
initializePetscForServer = petscGlobalInit

{- |
Pre-compute matrix factorization for concurrent inventory calculations.

This function builds the (I - A) system matrix from technosphere triplets and
pre-computes the factorization during server startup. The resulting
MatrixFactorization can be stored in the Database for fast concurrent solves.

Performance: ~3s factorization time for full Ecoinvent, saves 2.9s per inventory request
-}
precomputeMatrixFactorization :: [(Int, Int, Double)] -> Int -> IO MatrixFactorization
precomputeMatrixFactorization techTriples n = do
    -- Serialize ALL PETSc operations to prevent concurrent matrix assembly/factorization
    withMVar petscGlobalMutex $ \_ -> do
        -- Ensure PETSc is initialized
        petscGlobalInit

        -- Build (I - A) system matrix
        let identityTriples = [(i, i, 1.0) | i <- [0 .. n - 1]]
        let systemTechTriples = [(i, j, -value) | (i, j, value) <- techTriples]
        let systemMatrix = aggregateMatrixEntries (identityTriples ++ systemTechTriples)

        reportMatrixOperation $ "Pre-computing factorization for " ++ show n ++ " activities with " ++ show (length systemMatrix) ++ " entries"

        -- Create and cache the actual PETSc factorized solver
        let matrixData = buildPetscMatrixData systemMatrix n
        let nzPattern = ConstNZPR (fromIntegral $ length systemMatrix, fromIntegral $ length systemMatrix)
        let comm = commWorld

        -- Create the PETSc matrix and factorize it
        mat <- petscMatrixCreate comm n n MatAij matrixData nzPattern InsertValues
        ksp <- kspCreate comm
        let (_, _, _, matMutable) = fromPetscMatrix mat

        kspSetOperators ksp matMutable matMutable
        kspSetFromOptions ksp
        pc <- kspGetPC ksp
        pcSetFromOptions pc
        kspSetInitialGuessNonzero ksp False
        kspSetUp ksp  -- This performs the actual factorization

        -- Cache the factorized solver globally with thread safety
        swapMVar cachedKspSolver (Just (ksp, mat, n))

        reportMatrixOperation "PETSc solver factorized and cached in memory for reuse"

        -- Also store the system matrix for backward compatibility
        let factorization = MatrixFactorization
                { mfSystemMatrix = systemMatrix
                , mfActivityCount = n
                }

        return factorization

{- |
Add a pre-computed matrix factorization to the database.
This enables fast concurrent inventory calculations by avoiding repeated factorization.
-}
addFactorizationToDatabase :: Database -> MatrixFactorization -> Database
addFactorizationToDatabase db factorization = db { dbCachedFactorization = Just factorization }

{- |
Finalize PETSc when the server shuts down.
This should be called once during server cleanup.
-}
finalizePetscForServer :: IO ()
finalizePetscForServer = do
    initialized <- readIORef petscInitialized
    when initialized $ do
        petscFin  -- Explicitly finalize PETSc/MPI
        writeIORef petscInitialized False

-- ===== WORKER THREAD POOL FUNCTIONS =====

{- |
Initialize PETSc context for individual worker thread.

Each worker thread gets its own isolated PETSc context to avoid MUMPS thread-safety issues.
This is the key innovation that enables true concurrent processing while keeping MUMPS.
-}
initializePetscForWorker :: Int -> IO ()
initializePetscForWorker workerId = do
    reportProgress Solver $ "Worker " ++ show workerId ++ " initializing dedicated PETSc/MUMPS context"
    -- Each worker uses the same global PETSc initialization but operates independently
    -- The key insight is that each worker thread gets its own matrix/solver instances
    petscGlobalInit
    reportProgress Solver $ "Worker " ++ show workerId ++ " PETSc context ready"

{- |
Cleanup PETSc context for worker thread.
-}
finalizePetscForWorker :: Int -> IO ()
finalizePetscForWorker workerId = do
    reportProgress Solver $ "Worker " ++ show workerId ++ " cleaning up PETSc context"
    -- Worker cleanup - individual workers don't call petscFin (global resource)

{- |
Solve sparse linear system using worker's isolated PETSc context.

This is the core function that enables concurrent MUMPS processing. Each worker
thread creates its own PETSc matrix and solver instances, avoiding the thread-safety
issues that cause crashes when multiple threads access the same MUMPS solver.
-}
solveSparseLinearSystemForWorker :: Int -> [(Int, Int, Double)] -> Int -> Vector -> IO Vector
solveSparseLinearSystemForWorker workerId techTriples n demandVec = do
    reportProgress Solver $ "Worker " ++ show workerId ++ " solving system for "
                          ++ show n ++ " activities with isolated MUMPS instance"

    -- Each worker creates its own matrix/solver instances - this is the key to thread safety
    -- No shared state = no thread-safety issues with MUMPS

    -- Build (I - A) system from sparse triplets
    let identityTriples = [(i, i, 1.0) | i <- [0 .. n - 1]]
    let systemTechTriples = [(i, j, -value) | (i, j, value) <- techTriples]
    let allTriples = identityTriples ++ systemTechTriples

    -- Aggregate duplicate entries for proper matrix assembly
    let aggregatedTriples = aggregateMatrixEntries allTriples

    reportProgress Solver $ "Worker " ++ show workerId ++ " created isolated matrix with "
                          ++ show (length aggregatedTriples) ++ " entries"

    -- Convert to PETSc format
    let matrixData = buildPetscMatrixData aggregatedTriples n
    let rhsData = V.fromList $ map realToFrac $ toList demandVec
    let nzPattern = ConstNZPR (fromIntegral $ length aggregatedTriples, fromIntegral $ length aggregatedTriples)

    -- Create worker-specific PETSc objects (isolated from other workers)
    let comm = commWorld
    result <- withPetscMatrix comm n n MatAij matrixData nzPattern InsertValues $ \mat ->
        withVecNew comm rhsData $ \rhs -> do
            let (_, _, _, matMutable) = fromPetscMatrix mat

            -- Each worker gets its own KSP solver instance
            withProgressTiming Solver ("Worker " ++ show workerId ++ " isolated MUMPS solve") $ do
                withKsp comm $ \ksp -> do
                    -- Worker-specific solver setup
                    kspSetOperators ksp matMutable matMutable
                    kspSetFromOptions ksp
                    pc <- kspGetPC ksp
                    pcSetFromOptions pc
                    kspSetInitialGuessNonzero ksp False
                    kspSetUp ksp

                    -- Solve with worker's isolated MUMPS instance
                    withVecDuplicate rhs $ \solution -> do
                        kspSolve ksp rhs solution
                        solutionData <- vecGetVS solution
                        let solutionList = map realToFrac $ VS.toList solutionData
                        return solutionList

    reportProgress Solver $ "Worker " ++ show workerId ++ " completed isolated solve"
    return $ fromList result

{- |
Create a factorized KSP solver for a worker from technosphere triplets.

This function creates a worker-specific factorized KSP that can be reused for
multiple solve operations, providing the performance benefits of cached factorization
while maintaining thread safety through per-worker instances.

Each worker gets its own complete KSP object with pre-computed MUMPS factorization,
avoiding the thread-safety issues of sharing factorized objects between threads.
-}
createFactorizedKspForWorker :: Int -> [(Int, Int, Double)] -> Int -> IO KSP
createFactorizedKspForWorker workerId techTriples n = do
    reportProgress Solver $ "Worker " ++ show workerId ++ " creating isolated factorized KSP for "
                          ++ show n ++ " activities"

    -- Build (I - A) system from sparse triplets (same as other functions)
    let identityTriples = [(i, i, 1.0) | i <- [0 .. n - 1]]
    let systemTechTriples = [(i, j, -value) | (i, j, value) <- techTriples]
    let allTriples = identityTriples ++ systemTechTriples
    let aggregatedTriples = aggregateMatrixEntries allTriples

    -- Convert to PETSc format
    let matrixData = buildPetscMatrixData aggregatedTriples n
    let nzPattern = ConstNZPR (fromIntegral $ length aggregatedTriples, fromIntegral $ length aggregatedTriples)
    let comm = commWorld

    -- Create PETSc matrix for this worker
    mat <- petscMatrixCreate comm n n MatAij matrixData nzPattern InsertValues
    let (_, _, _, matMutable) = fromPetscMatrix mat

    -- Create and setup KSP solver for this worker
    ksp <- kspCreate comm
    kspSetOperators ksp matMutable matMutable
    kspSetFromOptions ksp  -- Read PETSC_OPTIONS for solver configuration
    pc <- kspGetPC ksp
    pcSetFromOptions pc    -- Configure preconditioner (MUMPS options)
    kspSetInitialGuessNonzero ksp False

    -- Perform factorization during setup (this is the expensive operation)
    withProgressTiming Solver ("Worker " ++ show workerId ++ " MUMPS factorization") $ do
        kspSetUp ksp  -- This performs the actual MUMPS factorization

    reportProgress Solver $ "Worker " ++ show workerId ++ " created isolated factorized KSP - ready for fast solves"
    return ksp
