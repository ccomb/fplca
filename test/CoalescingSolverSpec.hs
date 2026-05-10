{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : CoalescingSolverSpec
Description : Concurrency / batching tests for the per-database coalescing solver.

Exercises the worker-thread that owns the MUMPS handle and merges concurrent
solve requests into one 'mumpsSolveMulti' call per round. Uses the SAMPLE.min3
fixture (3 activities, linear supply chain) so the oracle path
('solveSparseLinearSystem') stays in milliseconds.
-}
module CoalescingSolverSpec (spec) where

import Control.Concurrent.Async (concurrently, mapConcurrently)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar)
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as U
import Matrix (
    Vector,
    buildDemandVectorFromIndex,
    clearCachedSolver,
    fromList,
    inspectCoalesceBatchCount,
    solveSparseLinearSystem,
    solveSparseLinearSystemWithFactorization,
    solveSparseLinearSystemWithFactorizationMulti,
 )
import qualified SharedSolver as SS
import Test.Hspec
import TestHelpers (loadSampleDatabase, mkSolverFromDb)
import Types

spec :: Spec
spec = do
    describe "concurrent correctness on the cached path" $ do
        it "50 concurrent single-solves all match the oracle" $ do
            (_solver, fact, db) <- min3CachedSolver
            let demands = take 50 (cycle (basicDemands db))
            expected <- mapM (oracleSolve db) demands
            actual <-
                mapConcurrently
                    (solveSparseLinearSystemWithFactorization fact)
                    demands
            actual `shouldSatisfy` allCloseTo expected

    describe "coalescence (the actual point of the refactor)" $ do
        it "K concurrent requests collapse into < K solver calls" $ do
            (_solver, fact, db) <- min3CachedSolver
            let k = 200
                demands = take k (cycle (basicDemands db))
            before <- inspectCoalesceBatchCount cacheKey
            -- Synchronize the K threads on a single start gate so they all
            -- hit submitBatch within the same scheduling window, otherwise
            -- a fast solve on a 3x3 matrix could finish before the next
            -- request is even forked, hiding the coalescing.
            gate <- newEmptyMVar
            _ <- concurrently
                (mapConcurrently (\d -> readMVar gate >> solveSparseLinearSystemWithFactorization fact d) demands)
                (putMVar gate ())
            after <- inspectCoalesceBatchCount cacheKey
            case (before, after) of
                (Just b, Just a) -> (a - b) `shouldSatisfy` (< k)
                _ -> expectationFailure "coalescing solver counter not present"

    describe "single-RHS regression (batch of 1)" $ do
        it "submitOne path produces the oracle result" $ do
            (_solver, fact, db) <- min3CachedSolver
            let demand = buildDemandVectorFromIndex (dbActivityIndex db) 0
            expected <- oracleSolve db demand
            actual <- solveSparseLinearSystemWithFactorization fact demand
            U.toList actual `shouldSatisfy` closeTo (U.toList expected)

    describe "multi-RHS demultiplexing" $ do
        it "k=10 distinct demands return matching oracle solutions in order" $ do
            (_solver, fact, db) <- min3CachedSolver
            let demands = take 10 (cycle (basicDemands db))
            expected <- mapM (oracleSolve db) demands
            actual <- solveSparseLinearSystemWithFactorizationMulti fact demands
            length actual `shouldBe` length expected
            zip actual expected `shouldSatisfy` all (\(a, e) -> closeTo (U.toList e) (U.toList a))

    describe "mixed single + batch concurrency" $ do
        it "single and multi-RHS callers interleaved still match the oracle" $ do
            (_solver, fact, db) <- min3CachedSolver
            let singles = take 20 (cycle (basicDemands db))
                multiBatch = take 6 (cycle (basicDemands db))
            expectedSingles <- mapM (oracleSolve db) singles
            expectedMulti <- mapM (oracleSolve db) multiBatch
            (actualSingles, actualMulti) <- concurrently
                (mapConcurrently (solveSparseLinearSystemWithFactorization fact) singles)
                (solveSparseLinearSystemWithFactorizationMulti fact multiBatch)
            actualSingles `shouldSatisfy` allCloseTo expectedSingles
            actualMulti `shouldSatisfy` allCloseTo expectedMulti

    describe "clean shutdown" $ do
        it "clearCachedSolver lets the next request fall back without deadlock" $ do
            (_solver, fact, db) <- min3CachedSolver
            let demand = buildDemandVectorFromIndex (dbActivityIndex db) 0
            expected <- oracleSolve db demand
            clearCachedSolver cacheKey
            -- After clear, the cache lookup misses and we fall back to a fresh
            -- assemble+factorize+solve. The result must still be correct, and
            -- no deadlock on the worker MVar.
            actual <- solveSparseLinearSystemWithFactorization fact demand
            U.toList actual `shouldSatisfy` closeTo (U.toList expected)

-- ---------------------------------------------------------------------------
-- Fixtures and helpers
-- ---------------------------------------------------------------------------

-- | Cache key shared by every test in this spec. Distinct from the keys used
-- by other specs so they don't clobber each other.
cacheKey :: T.Text
cacheKey = "SAMPLE.min3.coalescing"

-- | Build a SharedSolver on SAMPLE.min3, trigger factorization (so the
-- coalescing worker is running), and return the bits the tests need.
min3CachedSolver :: IO (SS.SharedSolver, MatrixFactorization, Database)
min3CachedSolver = do
    db <- loadSampleDatabase "SAMPLE.min3"
    solver <- mkSolverFromDb db cacheKey
    let demand = buildDemandVectorFromIndex (dbActivityIndex db) 0
    _ <- SS.solveWithSharedSolver solver demand
    Just fact <- SS.getFactorization solver
    pure (solver, fact, db)

-- | A small set of distinct demand vectors that exercise different RHS shapes.
-- Repeating these via 'cycle' gives us many varied (but reproducible) inputs.
basicDemands :: Database -> [Vector]
basicDemands db =
    let n = U.length (buildDemandVectorFromIndex (dbActivityIndex db) 0)
     in [ unitOn n 0
        , unitOn n 1
        , unitOn n 2
        , scaleVec 2.5 (unitOn n 0)
        , scaleVec 0.7 (unitOn n 1)
        , addVec (unitOn n 0) (unitOn n 2)
        ]

unitOn :: Int -> Int -> Vector
unitOn n i = fromList [if k == i then 1.0 else 0.0 | k <- [0 .. n - 1]]

scaleVec :: Double -> Vector -> Vector
scaleVec c = U.map (* c)

addVec :: Vector -> Vector -> Vector
addVec = U.zipWith (+)

-- | Cold-path oracle: assemble (I-A) and solve from scratch via MUMPS.
-- Bypasses the worker entirely — what we compare against.
oracleSolve :: Database -> Vector -> IO Vector
oracleSolve db demand =
    let triples =
            [ (fromIntegral i, fromIntegral j, v)
            | SparseTriple i j v <- U.toList (dbTechnosphereTriples db)
            ]
        n = fromIntegral (dbActivityCount db)
     in solveSparseLinearSystem triples n demand

-- | Numerical tolerance used throughout the spec (matches GoldenData).
solverTolerance :: Double
solverTolerance = 1.0e-9

closeTo :: [Double] -> [Double] -> Bool
closeTo expected actual =
    length expected == length actual
        && and (zipWith (\e a -> abs (e - a) < solverTolerance) expected actual)

allCloseTo :: [Vector] -> [Vector] -> Bool
allCloseTo expected actual =
    length expected == length actual
        && and (zipWith (\e a -> closeTo (U.toList e) (U.toList a)) expected actual)

