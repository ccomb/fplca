{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : SharedSolver
Description : Shared PETSc solver with thread synchronization for concurrent MUMPS processing

This module implements a simple shared solver approach where a single PETSc context
and factorized KSP are protected by MVar synchronization. This eliminates MUMPS
thread-safety issues while maintaining fast factorized solves.

Key features:
- Single shared PETSc/MUMPS context (no multi-context issues)
- MVar-based thread synchronization (serialize access to MUMPS)
- Pre-factorized KSP for sub-second performance
- Simple and reliable architecture
-}

module SharedSolver (
    -- * Shared solver types
    SharedSolver,

    -- * Solver management
    createSharedSolver,

    -- * Concurrent solving
    solveWithSharedSolver
) where

import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar)
import Control.Exception (bracket, catch, SomeException)
import Progress
import Types
import Matrix (Vector, fromList, solveSparseLinearSystem, solveSparseLinearSystemWithFactorization)

-- | Shared solver with single PETSc context and thread synchronization
data SharedSolver = SharedSolver
    { solverLock :: MVar ()                      -- ^ Serialize access to solver
    , solverCachedFactorization :: Maybe MatrixFactorization -- ^ Pre-computed factorization for fast solves
    , solverTechTriples :: [(Int, Int, Double)]   -- ^ Technosphere matrix data for fallback
    , solverActivityCount :: Int                 -- ^ Number of activities for matrix sizing
    }

-- | Create a shared solver with thread synchronization
createSharedSolver :: Maybe MatrixFactorization -> [(Int, Int, Double)] -> Int -> IO SharedSolver
createSharedSolver maybeFactorization techTriples activityCount = do
    reportProgress Info "Creating shared solver with thread synchronization"
    lock <- newMVar ()
    case maybeFactorization of
        Just _ -> reportProgress Info "Shared solver using cached matrix factorization for fast concurrent solves"
        Nothing -> reportProgress Info "Shared solver will use fallback standard solver"
    return $ SharedSolver lock maybeFactorization techTriples activityCount

-- | Solve inventory calculation using shared solver with thread synchronization
solveWithSharedSolver :: SharedSolver -> Vector -> IO Vector
solveWithSharedSolver solver demandVector = do
    -- Use MVar to serialize access to the shared PETSc/MUMPS solver
    bracket (takeMVar (solverLock solver)) (putMVar (solverLock solver)) $ \_ -> do
        case solverCachedFactorization solver of
            Just factorization -> do
                reportProgress Solver "Using cached factorization for sub-second solve"
                solveSparseLinearSystemWithFactorization factorization demandVector
            Nothing -> do
                reportProgress Solver "Using fallback standard sparse solver (no cached factorization)"
                solveSparseLinearSystem (solverTechTriples solver) (solverActivityCount solver) demandVector

