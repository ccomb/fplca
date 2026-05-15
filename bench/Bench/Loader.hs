{-# LANGUAGE OverloadedStrings #-}

{- | Loader benchmarks.

  * 'loader.single_db' — time to take a database from on-disk format to
    fully-indexed in-memory @SimpleDatabase@: parse + activity-link
    resolution + indexing. This is what an instance pays the first time a
    user loads a DB (or after a cache invalidation).

  * 'loader.multi_db_cross_link' — same path plus cross-DB supplier
    resolution against a separately loaded « background » DB. This is the
    canonical setup that lets foreground bases (Agribalyse, BAFU) borrow
    their upstream from a shared core like Ecoinvent.

Both benches are heavy (tens of seconds per iteration on real fixtures),
so the orchestrator's sample count is intentionally low.
-}
module Bench.Loader (
    register,
) where

import Control.Exception (evaluate)
import Criterion.Main (Benchmarkable, nfIO)
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import qualified Database.CrossLinking as CL
import qualified Database.Loader as Loader
import qualified SynonymDB.Types as Syn
import Types (cdlTotalInputs, sdbActivities)
import qualified UnitConversion as UC

import qualified Bench.Json as J
import Bench.Json (BenchSpec (..), UnitOfWork (..))
import qualified Fixtures as F

register :: IO [BenchSpec]
register = do
    single <- registerSingleDb
    cross <- registerCrossDbLinking
    pure (single ++ cross)

-- ---------------------------------------------------------------------------
-- loader.single_db
-- ---------------------------------------------------------------------------

{- | Bench loading one database end-to-end. Picks the first available
fixture among (Agribalyse → Bafu → Ecoinvent) so the bench runs on
whatever happens to be on disk. Reports the actual process count.
-}
registerSingleDb :: IO [BenchSpec]
registerSingleDb = do
    mFx <- pickFirstAvailable [F.Agribalyse, F.Bafu, F.Ecoinvent]
    case mFx of
        Nothing -> do
            putStrLn "[bench] loader.single_db: no fixture available, skipping"
            pure []
        Just (src, path) -> do
            -- One probe load to learn the process count for the unit_of_work.
            res <- Loader.loadDatabase UC.defaultUnitConfig path
            case res of
                Left err -> do
                    putStrLn $
                        "[bench] loader.single_db: probe load failed ("
                            <> T.unpack err
                            <> "), skipping"
                    pure []
                Right sdb -> do
                    let !n = M.size (sdbActivities sdb)
                    pure
                        [ BenchSpec
                            { bsCapability = "loader.single_db"
                            , bsLabel = T.pack ("Load a database of " <> show n <> " processes (parse + link + index)")
                            , bsDescription =
                                "Reads a database from disk, parses every process and exchange, resolves \
                                \technosphere supplier links inside the database, and builds the in-memory \
                                \indexes. This is the cost paid the first time a user loads a database in \
                                \VoLCA (subsequent loads hit the matrix cache and skip most of this work)."
                            , bsUnitOfWork = UnitOfWork{uowKind = "processes", uowN = n}
                            , bsMetric = "seconds"
                            , bsFixture = J.Fixture{J.fSource = F.fixtureSourceLabel src, J.fSlice = "whole database"}
                            , bsAction = loadBench path
                            }
                        ]

loadBench :: FilePath -> Benchmarkable
loadBench path = nfIO $ do
    r <- Loader.loadDatabase UC.defaultUnitConfig path
    case r of
        Left err -> evaluate (T.length err)
        Right sdb -> evaluate (M.size (sdbActivities sdb))

-- ---------------------------------------------------------------------------
-- loader.multi_db_cross_link
-- ---------------------------------------------------------------------------

{- | Bench cross-DB linking: load Agribalyse with Ecoinvent already indexed
in the background, so unlinked technosphere inputs resolve against the
Ecoinvent supplier index. Requires both fixtures.
-}
registerCrossDbLinking :: IO [BenchSpec]
registerCrossDbLinking = do
    mFg <- F.lookupFixture F.Agribalyse
    mBg <- F.lookupFixture F.Ecoinvent
    case (mFg, mBg) of
        (Just fgPath, Just bgPath) -> do
            -- Pre-load the background DB once: this is the canonical pattern
            -- (Ecoinvent loaded once, then any number of foreground bases
            -- attach to it). The bench iteration only re-runs the
            -- foreground load + linking step.
            putStrLn "[bench] loader.multi_db_cross_link: pre-loading background DB..."
            bgRes <- Loader.loadDatabase UC.defaultUnitConfig bgPath
            case bgRes of
                Left err -> do
                    putStrLn $
                        "[bench] loader.multi_db_cross_link: background load failed ("
                            <> T.unpack err
                            <> "), skipping"
                    pure []
                Right bgSdb -> do
                    let !bgIndex =
                            CL.buildIndexedDatabase
                                (F.fixtureSourceLabel F.Ecoinvent)
                                Syn.emptySynonymDB
                                bgSdb
                    -- Probe load of the foreground DB to learn the supplier-link count.
                    putStrLn "[bench] loader.multi_db_cross_link: probing foreground load..."
                    probeRes <-
                        Loader.loadDatabaseWithCrossDBLinking
                            M.empty
                            [bgIndex]
                            Syn.emptySynonymDB
                            UC.defaultUnitConfig
                            M.empty
                            fgPath
                    case probeRes of
                        Left err -> do
                            putStrLn $
                                "[bench] loader.multi_db_cross_link: probe failed ("
                                    <> T.unpack err
                                    <> "), skipping"
                            pure []
                        Right (_, stats) -> do
                            -- 'cdlTotalInputs' is what cross-DB resolution had to traverse.
                            let !n = cdlTotalInputs stats
                            pure
                                [ BenchSpec
                                    { bsCapability = "loader.multi_db_cross_link"
                                    , bsLabel = T.pack ("Reload a foreground DB and cross-link " <> show n <> " supplier inputs against a background DB")
                                    , bsDescription =
                                        "Each iteration re-parses the foreground database (Agribalyse-shaped) end-to-end \
                                        \AND resolves every unlinked technosphere input against a separately loaded, \
                                        \pre-indexed background database (Ecoinvent-shaped). The background DB is loaded \
                                        \once before measurement; the foreground load + linking pass is re-run each \
                                        \iteration. Wall-time is therefore dominated by the foreground parse — readers \
                                        \should treat it as a combined « reload + cross-link » cost, not a pure linking \
                                        \cost. This is the canonical setup that lets sector-specific bases borrow generic \
                                        \background data from a shared core."
                                    , bsUnitOfWork = UnitOfWork{uowKind = "supplier_inputs", uowN = n}
                                    , bsMetric = "seconds"
                                    , bsFixture =
                                        J.Fixture
                                            { J.fSource = T.pack "agribalyse + ecoinvent"
                                            , J.fSlice = "whole databases (foreground reload each iteration)"
                                            }
                                    , bsAction = crossLinkBench bgIndex fgPath
                                    }
                                ]
        _ -> do
            putStrLn "[bench] loader.multi_db_cross_link: need both VOLCA_BENCH_AGRIBALYSE and VOLCA_BENCH_ECOINVENT, skipping"
            pure []

crossLinkBench :: CL.IndexedDatabase -> FilePath -> Benchmarkable
crossLinkBench bgIndex fgPath = nfIO $ do
    r <-
        Loader.loadDatabaseWithCrossDBLinking
            M.empty
            [bgIndex]
            Syn.emptySynonymDB
            UC.defaultUnitConfig
            M.empty
            fgPath
    case r of
        Left err -> evaluate (T.length err)
        Right (sdb, stats) ->
            evaluate (M.size (sdbActivities sdb) + cdlTotalInputs stats)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

pickFirstAvailable :: [F.FixtureSource] -> IO (Maybe (F.FixtureSource, FilePath))
pickFirstAvailable [] = pure Nothing
pickFirstAvailable (s : ss) = do
    m <- F.lookupFixture s
    case m of
        Just p -> pure (Just (s, p))
        Nothing -> pickFirstAvailable ss
