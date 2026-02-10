{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : LCA.Matrix
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
module LCA.Matrix (
    Vector,
    Inventory,
    computeInventoryMatrix,
    computeInventoryWithDependencies,
    buildDemandVectorFromIndex,
    solveSparseLinearSystem,
    applySparseMatrix,
    fromList,
    toList,
    initializePetscForServer,
    precomputeMatrixFactorization,
    addFactorizationToDatabase,
    solveSparseLinearSystemWithFactorization,
    clearCachedKspSolver,
) where

import LCA.Progress
import LCA.Types
import LCA.UnitConversion (convertExchangeAmount)
import Control.Exception (catch, SomeException)
import Control.Monad (forM_, when, unless)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Monad.ST
import Data.Int (Int32)
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

-- | Global cache for pre-factorized KSP solvers per database with thread synchronization
-- Maps database name to (KSP solver, PETSc matrix, activity count)
{-# NOINLINE cachedKspSolver #-}
cachedKspSolver :: MVar (M.Map Text (KSP, PetscMatrix PetscScalar_, Int))
cachedKspSolver = unsafePerformIO $ newMVar M.empty

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

-- | Clear cached KSP solver for a database (call when unloading)
-- Destroys the PETSc KSP solver and matrix to release memory
clearCachedKspSolver :: Text -> IO ()
clearCachedKspSolver dbName = do
    modifyMVar_ cachedKspSolver $ \cache -> do
        case M.lookup dbName cache of
            Just (ksp, mat, _) -> do
                -- Destroy PETSc objects to release C-level memory
                kspDestroy ksp
                petscMatrixDestroy mat
                return $ M.delete dbName cache
            Nothing -> return cache

-- | Aggregate duplicate matrix entries by summing values for same (i,j) coordinates
aggregateMatrixEntries :: [(Int, Int, Double)] -> [(Int, Int, Double)]
aggregateMatrixEntries entries =
    let groupedEntries = M.fromListWith (+) [((i, j), val) | (i, j, val) <- entries]
     in [(i, j, val) | ((i, j), val) <- M.toList groupedEntries]

-- Vector operations
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

The solver is looked up by database ID from the per-database cache, enabling
instant switching between databases without re-factorization.
-}
solveSparseLinearSystemWithFactorization :: MatrixFactorization -> Vector -> Vector
solveSparseLinearSystemWithFactorization factorization demandVec = unsafePerformIO $ do
    -- Look up pre-factorized solver for this specific database
    let dbId = mfDatabaseId factorization
    cachedSolvers <- readMVar cachedKspSolver
    case M.lookup dbId cachedSolvers of
        Nothing -> do
            -- Fallback to standard solver if no cached factorization available for this database
            reportMatrixOperation $ "No cached factorization found for database '" ++ T.unpack dbId ++ "', falling back to matrix assembly"
            let systemMatrix = mfSystemMatrix factorization
                n = mfActivityCount factorization
                -- Convert system matrix back to technosphere triplets (remove identity entries)
                techTriples = U.toList $ U.filter (\(SparseTriple i j _) -> i /= j) $ U.map (\(SparseTriple i j val) -> SparseTriple i j (-val)) systemMatrix
            return $ solveSparseLinearSystemPETSc [(fromIntegral i, fromIntegral j, v) | SparseTriple i j v <- techTriples] (fromIntegral n) demandVec

        Just (ksp, petscMat, n) -> do
            reportMatrixOperation $ "Using cached solver for database '" ++ T.unpack dbId ++ "' (" ++ show n ++ " activities) - ultra-fast solve"

            -- Time the ultra-fast solve with thread synchronization and error handling
            result <- withProgressTiming Solver "PETSc thread-safe cached solve" $ do
                -- Use withMVar to ensure thread-safe access to the cached KSP solver
                result <- catch
                    (withMVar petscGlobalMutex $ \_ -> do
                        cachedSolvers' <- readMVar cachedKspSolver
                        case M.lookup dbId cachedSolvers' of
                            Nothing -> error $ "Cached solver for database '" ++ T.unpack dbId ++ "' disappeared during solve"
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
                            techTriples = U.toList $ U.filter (\(SparseTriple i j _) -> i /= j) $ U.map (\(SparseTriple i j val) -> SparseTriple i j (-val)) systemMatrix
                        return $ toList $ solveSparseLinearSystemPETSc [(fromIntegral i, fromIntegral j, v) | SparseTriple i j v <- techTriples] (fromIntegral n) demandVec

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
                        pc <- kspGetPc ksp
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

                            -- Check for infinity/NaN in solution (indicates singular/ill-conditioned matrix)
                            let hasInfinity = any (\x -> isInfinite x || isNaN x) solutionResult
                            when hasInfinity $ do
                                reportMatrixOperation "ERROR: Solution contains infinity or NaN values"
                                reportMatrixOperation "Matrix is singular - likely missing reference flows or treatment activities"
                                reportMatrixOperation "Check: (1) all activities have reference flows, (2) treatment inputs included in matrix"

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
- Root activity ProcessId for which to compute inventory

Output:
- Map from biosphere flow UUID to inventory quantity (kg, MJ, etc.)

Performance: ~7s total for full Ecoinvent database (25K products)
- 3.5s cache loading + 3s solver + 0.5s biosphere calculation
-}
computeInventoryMatrix :: Database -> ProcessId -> Inventory
computeInventoryMatrix db rootProcessId =
    let activityCount = dbActivityCount db
        bioFlowCount = dbBiosphereCount db

        -- Use pre-built sparse coordinate lists from database
        techTriples = dbTechnosphereTriples db
        bioTriples = dbBiosphereTriples db
        activityIndex = dbActivityIndex db
        -- CRITICAL FIX: Rebuild bioFlowIndex from dbBiosphereFlows to ensure consistency
        -- with the B matrix which was built using indices from the same vector.
        -- Storing both the Map and the vector is redundant and error-prone.
        bioFlowIndex = M.fromList $ zip (V.toList $ dbBiosphereFlows db) [0..]

        -- Build demand vector (will error if ProcessId not in index - validation happens at service layer)
        demandVec = buildDemandVectorFromIndex activityIndex rootProcessId

        -- Solve sparse LCA equation: (I - A) * supply = demand using PETSc
        -- Try to use cached factorization for faster solving
        supplyVec = case dbCachedFactorization db of
            Just factorization -> solveSparseLinearSystemWithFactorization factorization demandVec
            Nothing -> solveSparseLinearSystem [(fromIntegral i, fromIntegral j, v) | SparseTriple i j v <- U.toList techTriples] (fromIntegral activityCount) demandVec

        -- Calculate inventory using sparse biosphere matrix: g = B * supply
        inventoryVec = applySparseMatrix [(fromIntegral i, fromIntegral j, v) | SparseTriple i j v <- U.toList bioTriples] (fromIntegral bioFlowCount) supplyVec

        -- Build result map using the stored bioFlowIndex (ensures consistency with bioTriples)
        result = M.fromList [(uuid, inventoryVec U.! idx)
                           | (uuid, idx) <- M.toList bioFlowIndex
                           , idx < U.length inventoryVec]
     in result

{- |
Compute inventory with cross-database dependencies.

This function implements the block matrix back-substitution algorithm:
1. Solve the local database system: (I - A_local) × s_local = f
2. For each cross-database link, accumulate demand on the supplier
3. Group demand by dependency database
4. Recursively solve each dependency database
5. Sum inventories from all databases

Arguments:
- lookupDb: Function to look up a database by supplier UUID pair
- db: The current database (e.g., Ginko)
- rootProcessId: The root activity to solve for

Returns:
- Combined inventory from current database and all dependencies

Note: Cross-DB links store raw exchange amounts. We need to multiply by the
consumer's scaling factor (supply vector value) to get actual demand.
-}
computeInventoryWithDependencies
    :: (UUID -> UUID -> Maybe Database)  -- ^ Lookup dependency DB by supplier (actUUID, prodUUID)
    -> Database                          -- ^ Current database
    -> ProcessId                         -- ^ Root activity
    -> Inventory
computeInventoryWithDependencies lookupDb db rootProcessId =
    let -- Step 1: Compute local inventory (same as computeInventoryMatrix)
        activityCount = dbActivityCount db
        bioFlowCount = dbBiosphereCount db
        techTriples = dbTechnosphereTriples db
        bioTriples = dbBiosphereTriples db
        activityIndex = dbActivityIndex db
        bioFlowIndex = M.fromList $ zip (V.toList $ dbBiosphereFlows db) [0..]

        demandVec = buildDemandVectorFromIndex activityIndex rootProcessId

        -- Solve local system
        localSupplyVec = case dbCachedFactorization db of
            Just factorization -> solveSparseLinearSystemWithFactorization factorization demandVec
            Nothing -> solveSparseLinearSystem
                [(fromIntegral i, fromIntegral j, v) | SparseTriple i j v <- U.toList techTriples]
                (fromIntegral activityCount)
                demandVec

        -- Calculate local inventory
        localInventoryVec = applySparseMatrix
            [(fromIntegral i, fromIntegral j, v) | SparseTriple i j v <- U.toList bioTriples]
            (fromIntegral bioFlowCount)
            localSupplyVec
        localInventory = M.fromList
            [(uuid, localInventoryVec U.! idx) | (uuid, idx) <- M.toList bioFlowIndex, idx < U.length localInventoryVec]

        -- Step 2: Compute demand for cross-DB links
        -- For each cross-DB link, the demand on the supplier is:
        --   demand = coefficient × supply[consumer]
        -- where supply[consumer] is the scaling factor for the consumer activity
        crossLinks = dbCrossDBLinks db
        processIdLookup = dbProcessIdLookup db

        -- Group links by supplier database (determined by looking up each supplier)
        -- and accumulate demand
        crossDBDemands :: M.Map (UUID, UUID) Double  -- (supplierActUUID, supplierProdUUID) -> total demand
        crossDBDemands = foldr accumulateDemand M.empty crossLinks
          where
            accumulateDemand :: CrossDBLink -> M.Map (UUID, UUID) Double -> M.Map (UUID, UUID) Double
            accumulateDemand link acc =
                let consumerKey = (cdlConsumerActUUID link, cdlConsumerProdUUID link)
                    supplierKey = (cdlSupplierActUUID link, cdlSupplierProdUUID link)
                in case M.lookup consumerKey processIdLookup of
                    Nothing -> acc  -- Consumer not found (shouldn't happen)
                    Just consumerPid ->
                        let consumerIdx = fromIntegral $ activityIndex V.! fromIntegral consumerPid
                            consumerScale = localSupplyVec U.! consumerIdx
                            -- Demand = amount per unit output × scaling factor
                            demand = cdlCoefficient link * consumerScale
                        in M.insertWith (+) supplierKey demand acc

        -- Step 3: For each supplier, find its database and compute its inventory
        -- Then sum all inventories
        crossDBInventory :: Inventory
        crossDBInventory = M.foldrWithKey processSupplier M.empty crossDBDemands
          where
            processSupplier :: (UUID, UUID) -> Double -> Inventory -> Inventory
            processSupplier (supplierActUUID, supplierProdUUID) demand acc =
                case lookupDb supplierActUUID supplierProdUUID of
                    Nothing -> acc  -- Supplier database not loaded (warning could be issued)
                    Just depDb ->
                        case M.lookup (supplierActUUID, supplierProdUUID) (dbProcessIdLookup depDb) of
                            Nothing -> acc  -- Supplier not found in its database
                            Just supplierPid ->
                                -- Recursively compute inventory (could recurse further if depDb has dependencies)
                                let depInventory = computeInventoryWithDependencies lookupDb depDb supplierPid
                                    -- Scale by demand
                                    scaledInventory = M.map (* demand) depInventory
                                in M.unionWith (+) acc scaledInventory

        -- Step 4: Sum local and cross-DB inventories
        totalInventory = M.unionWith (+) localInventory crossDBInventory

     in totalInventory

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

Uses the pre-built activity index mapping from ProcessId to matrix indices for efficiency.
-}
buildDemandVectorFromIndex :: V.Vector Int32 -> ProcessId -> Vector
buildDemandVectorFromIndex activityIndex rootProcessId =
    let n = V.length activityIndex
        -- The activityIndex is a direct mapping: ProcessId -> matrix index
        -- So we just look up the ProcessId (which is an index) in the vector
        rootIndex = if fromIntegral rootProcessId >= 0 && fromIntegral rootProcessId < n
                    then fromIntegral $ activityIndex V.! fromIntegral rootProcessId
                    else error $ "FATAL: ProcessId not found in activity index: " ++ show rootProcessId
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

The database name is used as the key in the per-database solver cache, enabling
instant switching between databases without re-factorization.

Performance: ~3s factorization time for full Ecoinvent, saves 2.9s per inventory request
-}
precomputeMatrixFactorization :: Text -> [(Int, Int, Double)] -> Int -> IO MatrixFactorization
precomputeMatrixFactorization dbName techTriples n = do
    -- Serialize ALL PETSc operations to prevent concurrent matrix assembly/factorization
    withMVar petscGlobalMutex $ \_ -> do
        -- Ensure PETSc is initialized
        petscGlobalInit

        -- Build (I - A) system matrix
        let identityTriples = [(i, i, 1.0) | i <- [0 .. n - 1]]
        let systemTechTriples = [(i, j, -value) | (i, j, value) <- techTriples]
        let systemMatrix = aggregateMatrixEntries (identityTriples ++ systemTechTriples)

        reportMatrixOperation $ "Pre-computing factorization for database '" ++ T.unpack dbName ++ "' (" ++ show n ++ " activities, " ++ show (length systemMatrix) ++ " entries)"

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
        pc <- kspGetPc ksp
        pcSetFromOptions pc
        kspSetInitialGuessNonzero ksp False
        kspSetUp ksp  -- This performs the actual factorization

        -- Cache the factorized solver by database name for per-database lookup
        modifyMVar_ cachedKspSolver $ \solvers -> return $ M.insert dbName (ksp, mat, n) solvers

        reportMatrixOperation $ "PETSc solver for database '" ++ T.unpack dbName ++ "' factorized and cached"

        -- Store the system matrix for fallback when cache miss or solver failure
        let factorization = MatrixFactorization
                { mfSystemMatrix = U.fromList [SparseTriple (fromIntegral i) (fromIntegral j) v | (i, j, v) <- systemMatrix]
                , mfActivityCount = fromIntegral n
                , mfDatabaseId = dbName
                }

        return factorization

{- |
Add a pre-computed matrix factorization to the database.
This enables fast concurrent inventory calculations by avoiding repeated factorization.
-}
addFactorizationToDatabase :: Database -> MatrixFactorization -> Database
addFactorizationToDatabase db factorization = db { dbCachedFactorization = Just factorization }
