{-# LANGUAGE OverloadedStrings #-}

{- | LCI matrix-solve benchmarks.

The technosphere matrix is sparse and shared across all queries on a
database, so production caches a MUMPS factorisation per DB. The benches
here take that production setup as a given:

  * 'solve.scaling_vector' — single linear solve @(I − A) x = d@ for one
    product, building the factorisation lazily as 'computeScalingVector'
    does today.

  * 'solve.inventory_matrix' — same solve plus the @B · x@ matvec that
    converts the supply vector into a biosphere inventory. This is the
    « LCI » step a user pays for every analysed product.

  * 'solve.batch_multi_rhs' — multi-RHS solve over N products in one MUMPS
    call (chunked internally), using a precomputed factorisation. Mirrors
    what the cached coalescing solver does in production.

The fixture is loaded once at registration time; each iteration only
re-runs the solve / matvec.
-}
module Bench.Solve (
    register,
) where

import Control.Exception (evaluate)
import Criterion.Main (Benchmarkable, nfIO)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as VU

import qualified Matrix
import Types (
    Database (..),
    MatrixFactorization,
    ProcessId,
    SparseTriple (..),
 )

import qualified Bench.Helpers as H
import Bench.Json (BenchSpec (..), UnitOfWork (..))
import qualified Bench.Json as J
import qualified Fixtures as F

-- ---------------------------------------------------------------------------
-- Public registration
-- ---------------------------------------------------------------------------

nBatchProducts :: Int
nBatchProducts = 100

register :: IO [BenchSpec]
register = do
    mFx <- pickSolveFixture
    case mFx of
        Nothing -> do
            putStrLn "[bench] solve.*: no fixture available, skipping"
            pure []
        Just (src, path) -> do
            putStrLn $ "[bench] solve.*: loading database from " <> path <> " ..."
            res <- H.loadFullDatabase path
            case res of
                Left err -> do
                    putStrLn $ "[bench] solve.*: load failed (" <> T.unpack err <> "), skipping"
                    pure []
                Right db -> case pickProcessIds db nBatchProducts of
                    [] -> do
                        putStrLn "[bench] solve.*: loaded database has zero activities, skipping"
                        pure []
                    pids@(firstPid : _) -> do
                        let !nProcesses = fromIntegral (dbActivityCount db) :: Int
                        -- Pre-compute factorisation for the batched bench.
                        putStrLn "[bench] solve.batch_multi_rhs: precomputing matrix factorisation..."
                        fact <- buildFactorization src db
                        pure
                            [ BenchSpec
                                { bsCapability = "solve.scaling_vector"
                                , bsLabel = T.pack ("Solve Ax=b for one product on a " <> show nProcesses <> "-process matrix")
                                , bsDescription =
                                    "Solves the technosphere linear system (I − A) x = d for one demand vector, \
                                    \where A is the sparse activity-by-activity matrix and d is a unit demand for \
                                    \one product. The supply vector x tells us how much of every upstream activity \
                                    \is needed. This is the core LCA matrix step."
                                , bsUnitOfWork = UnitOfWork{uowKind = "matrix_processes", uowN = nProcesses}
                                , bsMetric = "milliseconds"
                                , bsFixture = J.Fixture{J.fSource = F.fixtureSourceLabel src, J.fSlice = "whole database matrix"}
                                , bsAction = scalingBench db firstPid
                                }
                            , BenchSpec
                                { bsCapability = "solve.inventory_matrix"
                                , bsLabel = T.pack ("Compute the full biosphere inventory on a " <> show nProcesses <> "-process matrix")
                                , bsDescription =
                                    "Solves the technosphere system (I − A) x = d, then applies the biosphere \
                                    \matrix g = B · x to get the environmental flow vector for one product. \
                                    \This is the full LCI step — what an analyst sees as « run the analysis » \
                                    \before any LCIA scoring."
                                , bsUnitOfWork = UnitOfWork{uowKind = "matrix_processes", uowN = nProcesses}
                                , bsMetric = "milliseconds"
                                , bsFixture = J.Fixture{J.fSource = F.fixtureSourceLabel src, J.fSlice = "whole database matrix"}
                                , bsAction = inventoryBench db firstPid
                                }
                            , BenchSpec
                                { bsCapability = "solve.batch_multi_rhs"
                                , bsLabel = T.pack ("Batch LCI solve for " <> show nBatchProducts <> " products in parallel (multi-RHS)")
                                , bsDescription =
                                    "Solves the technosphere system for many products in one MUMPS call, sharing \
                                    \the symbolic + numeric factorisation across right-hand sides. This is what \
                                    \the cached coalescing solver does in production for batch analyses (e.g. \
                                    \scoring an entire shopping list at once). The factorisation is precomputed \
                                    \outside the timed region; the bench measures the multi-RHS substitution + \
                                    \biosphere matvec per product."
                                , bsUnitOfWork = UnitOfWork{uowKind = "products_batched", uowN = length pids}
                                , bsMetric = "milliseconds"
                                , bsFixture = J.Fixture{J.fSource = F.fixtureSourceLabel src, J.fSlice = T.pack ("first " <> show nBatchProducts <> " activities by ProcessId")}
                                , bsAction = batchBench db fact pids
                                }
                            ]

-- ---------------------------------------------------------------------------
-- Bench actions
-- ---------------------------------------------------------------------------

scalingBench :: Database -> ProcessId -> Benchmarkable
scalingBench db pid = nfIO $ do
    v <- Matrix.computeScalingVector db pid
    evaluate (VU.length v)

inventoryBench :: Database -> ProcessId -> Benchmarkable
inventoryBench db pid = nfIO $ do
    inv <- Matrix.computeInventoryMatrix db pid
    evaluate (M.size inv)

batchBench :: Database -> MatrixFactorization -> [ProcessId] -> Benchmarkable
batchBench db fact pids = nfIO $ do
    invs <- Matrix.computeInventoryMatrixBatch db fact pids
    evaluate (sum (map M.size invs))

-- ---------------------------------------------------------------------------
-- Fixture helpers
-- ---------------------------------------------------------------------------

pickSolveFixture :: IO (Maybe (F.FixtureSource, FilePath))
pickSolveFixture = go [F.Agribalyse, F.Bafu, F.Ecoinvent]
  where
    go [] = pure Nothing
    go (s : ss) = do
        m <- F.lookupFixture s
        case m of
            Just p -> pure (Just (s, p))
            Nothing -> go ss

{- | First N ProcessIds in the database (deterministic by matrix ordering).
Returns at most as many as the database has; falls back gracefully on
small fixtures.
-}
pickProcessIds :: Database -> Int -> [ProcessId]
pickProcessIds db n =
    let total = fromIntegral (dbActivityCount db) :: Int
        cap = min n total
     in [fromIntegral i | i <- [0 .. cap - 1]]

-- | Build a MUMPS factorisation from the database's technosphere triples.
buildFactorization :: F.FixtureSource -> Database -> IO MatrixFactorization
buildFactorization src db = do
    let n = fromIntegral (dbActivityCount db)
        triples =
            [ (fromIntegral i, fromIntegral j, v)
            | SparseTriple i j v <- VU.toList (dbTechnosphereTriples db)
            ]
    Matrix.precomputeMatrixFactorization (F.fixtureSourceLabel src) triples n
