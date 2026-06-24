{-# LANGUAGE OverloadedStrings #-}

{- | LCIA scoring benchmarks.

The synthetic 27-method × 5000-CF fixture matches what 'fanout' (per-method
HTTP loop) and 'set-batched' (single matvec over a stacked broadcast) would
hit on a PEF-shaped multi-method scoring request. The two configurations
emit one bench each so the consumer can compute speedup directly.

The cascade-vs-broadcast comparison from the original MultiMethodBench was
removed because the empty-broadcast configuration is unreachable in
production ('Database.Manager.spawnSolverGroup' always fills the broadcast
vector at build time). The cascade fallback itself remains in the source,
since it is the cross-DB safety net that keeps inventories beyond the
broadcast's flow domain from being silently dropped.

Real-fixture LCIA benches (`lcia.real.set_batched`, `lcia.real.regional`)
are deferred until the loader benchs land; they need a fully loaded
'Database' value plus a cached 'MethodSetTables', and the cleanest place to
build that is in 'Bench.Loader'.
-}
module Bench.Lcia (
    register,
) where

import Control.Exception (evaluate)
import Criterion.Main (nf)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import System.Random (mkStdGen, randoms)

import Matrix (Inventory)
import qualified Matrix
import Method.Mapping
import qualified Method.Parser as MP
import Method.Types (Method (..), MethodCF (..))
import qualified Method.Types as MT
import qualified Plugin.Builtin as Builtin
import Types (
    BiosphereFlow (..),
    Compartment (..),
    Database (..),
    Indexes (..),
    ProductIndex (..),
    Unit (..),
 )
import qualified UnitConversion as UC

import qualified Bench.Helpers as H
import Bench.Json (BenchSpec (..), UnitOfWork (..))
import qualified Bench.Json as J
import qualified Fixtures as F

-- ---------------------------------------------------------------------------
-- Fixture sizes (PEF-shaped)
-- ---------------------------------------------------------------------------

nFlowsTotal :: Int
nFlowsTotal = 10_000

nCFsPerMethod :: Int
nCFsPerMethod = 5_000

nMethods :: Int
nMethods = 27

nInvNonzero :: Int
nInvNonzero = 500

-- ---------------------------------------------------------------------------
-- Synthetic fixture
-- ---------------------------------------------------------------------------

mkUuid :: Int -> UUID
mkUuid n = UUID.fromWords (fromIntegral n) 0xBEEF 0xCAFE 0xDEAD

mkUnit :: UUID -> Text -> Unit
mkUnit uid name =
    Unit
        { unitId = uid
        , unitName = name
        , unitSymbol = name
        , unitComment = ""
        }

mkFlow :: UUID -> UUID -> Int -> BiosphereFlow
mkFlow fid uid i =
    BiosphereFlow
        { bfId = fid
        , bfName = T.pack ("flow-" <> show i)
        , bfUnitId = uid
        , bfSynonyms = M.empty
        , bfCAS = Nothing
        , bfSubstanceId = Nothing
        , bfCompartment = Just (Compartment "air" Nothing)
        }

mkCF :: UUID -> Double -> MethodCF
mkCF flowRef val =
    MethodCF
        { mcfFlowRef = flowRef
        , mcfFlowName = "stub"
        , mcfDirection = MT.Output
        , mcfValue = val
        , mcfCompartment = Nothing
        , mcfCAS = Nothing
        , mcfUnit = "kg"
        , mcfConsumerLocation = Nothing
        }

mkMethod :: Int -> [MethodCF] -> Method
mkMethod n factors =
    Method
        { methodId = mkUuid (1_000_000 + n)
        , methodName = T.pack ("M-" <> show n)
        , methodDescription = Nothing
        , methodUnit = "kg eq"
        , methodCategory = T.pack ("Cat-" <> show n)
        , methodMethodology = Nothing
        , methodFactors = factors
        }

{- | Empty 'Database' for the non-regional synthetic bench.

'scoreRegionalCrossDB' threads a 'Database' through 'computeLCIAScoreSetFromTables'
but never inspects it when the method has no regionalised CFs. We still pass a
real value (rather than 'error' / 'undefined') so that an accidental forcing
during a future refactor degrades to a 0-result instead of a runtime crash.
-}
emptyDatabase :: Database
emptyDatabase =
    Database
        { dbProcessIdTable = V.empty
        , dbProcessIdLookup = M.empty
        , dbActivityUUIDIndex = M.empty
        , dbActivityProductsIndex = M.empty
        , dbProductIndex = ProductIndex M.empty M.empty M.empty
        , dbActivities = V.empty
        , dbTechFlows = M.empty
        , dbBioFlows = M.empty
        , dbWasteFlows = M.empty
        , dbUnits = M.empty
        , dbIndexes = Indexes M.empty M.empty M.empty M.empty
        , dbTechnosphereTriples = U.empty
        , dbBiosphereTriples = U.empty
        , dbActivityIndex = V.empty
        , dbBiosphereOrder = V.empty
        , dbActivityCount = 0
        , dbBiosphereCount = 0
        , dbCrossDBLinks = []
        , dbDependsOn = []
        , dbLinkingStats = mempty
        , dbSynonymDB = Nothing
        , dbFlowsByName = M.empty
        , dbFlowsByCAS = M.empty
        , dbProductSearchIndex = M.empty
        , dbBM25Index = Nothing
        }

data SynFixture = SynFixture
    { fxFlowDB :: !(M.Map UUID BiosphereFlow)
    , fxUnitDB :: !(M.Map UUID Unit)
    , fxUnitCfg :: !UC.UnitConfig
    , fxInventory :: !Inventory
    , fxFilledTables :: ![MethodTables]
    , fxSetTables :: !MethodSetTables
    }

buildFixture :: SynFixture
buildFixture =
    let unitCfg = UC.defaultUnitConfig
        kgUid = mkUuid 99_999
        unitDB = M.singleton kgUid (mkUnit kgUid "kg")
        flows =
            [ (fid, mkFlow fid kgUid i)
            | i <- [0 .. nFlowsTotal - 1]
            , let fid = mkUuid i
            ]
        flowDB = M.fromList flows

        cfValuesFor :: Int -> [Double]
        cfValuesFor seed =
            map
                (\r -> 0.01 + r * 99.99)
                (take nCFsPerMethod (randoms (mkStdGen seed) :: [Double]))

        mappingsFor seed =
            [ (mkCF fid v, Just (snd (flows !! i), ByUUID))
            | (i, v) <- zip [0 ..] (cfValuesFor seed)
            , let fid = mkUuid i
            ]

        methods = [mkMethod m [] | m <- [1 .. nMethods]]
        rawTables = [buildMethodTables M.empty M.empty (mappingsFor s) | s <- [1 .. nMethods]]
        filledTables = map (fillBroadcastVector unitCfg unitDB flowDB) rawTables
        setTables = buildMethodSetTables (zip methods filledTables)

        inventory =
            M.fromList
                [ (mkUuid (i * (nFlowsTotal `div` nInvNonzero)), 1.0 + fromIntegral i * 0.1)
                | i <- [0 .. nInvNonzero - 1]
                ]
     in SynFixture
            { fxFlowDB = flowDB
            , fxUnitDB = unitDB
            , fxUnitCfg = unitCfg
            , fxInventory = inventory
            , fxFilledTables = filledTables
            , fxSetTables = setTables
            }

{- | Score one inventory against each of the 27 methods, sequentially —
mirrors the per-method HTTP route loop without parallelism overhead.
-}
benchFanout :: SynFixture -> [Double]
benchFanout fx =
    [ loScore $
        computeLCIAScoreFromTables
            (fxUnitCfg fx)
            (fxUnitDB fx)
            (fxFlowDB fx)
            (fxInventory fx)
            t
    | t <- fxFilledTables fx
    ]

-- | Score all 27 methods at once via the stacked-broadcast matvec.
benchSetBatched :: SynFixture -> [(UUID, Either Text Double)]
benchSetBatched fx =
    computeLCIAScoreSetFromTables
        (fxUnitCfg fx)
        (fxUnitDB fx)
        (fxFlowDB fx)
        (fxInventory fx)
        M.empty
        ((emptyDatabase, U.empty, fxSetTables fx) :| [])

-- ---------------------------------------------------------------------------
-- Public registration
-- ---------------------------------------------------------------------------

register :: IO [BenchSpec]
register = do
    syn <- registerSynthetic
    real <- registerReal
    pure (syn ++ real)

-- ---------------------------------------------------------------------------
-- Synthetic benches (cascade vs broadcast removed; live comparison only)
-- ---------------------------------------------------------------------------

registerSynthetic :: IO [BenchSpec]
registerSynthetic = do
    let !fx = buildFixture
    -- Force the lazy parts of the fixture so their build cost is paid once
    -- here rather than in the first bench iteration. The MethodTables /
    -- MethodSetTables records carry strict fields, so WHNF of each list
    -- spine + the set tables is enough.
    let !_ = length (fxFilledTables fx)
        !_ = fxSetTables fx
    let synFixture =
            J.Fixture
                { J.fSource = "synthetic"
                , J.fSlice =
                    T.pack
                        ( show nMethods
                            <> " methods × "
                            <> show nCFsPerMethod
                            <> " CFs, "
                            <> show nInvNonzero
                            <> "-flow inventory"
                        )
                }
        uow =
            UnitOfWork
                { uowKind = "lcia_methods"
                , uowN = nMethods
                }
    pure
        [ BenchSpec
            { bsCapability = "lcia.synthetic.fanout"
            , bsLabel = "Score 27 LCIA methods on one inventory (per-method loop)"
            , bsDescription =
                "Sequential scoring of one inventory against 27 LCIA methods, one method per call. \
                \This is the path the HTTP API takes when a user requests several methods and the \
                \results aren't pre-batched. Compare against `lcia.synthetic.set_batched` to see \
                \what batching saves."
            , bsUnitOfWork = uow
            , bsMetric = "milliseconds"
            , bsFixture = synFixture
            , bsAction = nf benchFanout fx
            }
        , BenchSpec
            { bsCapability = "lcia.synthetic.set_batched"
            , bsLabel = "Score 27 LCIA methods on one inventory (batched matvec)"
            , bsDescription =
                "Single matvec over a stacked broadcast that scores all 27 methods at once. \
                \Walks the inventory exactly once and writes into 27 accumulators per non-zero. \
                \Used by the batched HTTP route to amortise the inventory walk across methods."
            , bsUnitOfWork = uow
            , bsMetric = "milliseconds"
            , bsFixture = synFixture
            , bsAction = nf benchSetBatched fx
            }
        ]

-- ---------------------------------------------------------------------------
-- Real-data benches (need both a loaded Database and a parsed Method)
-- ---------------------------------------------------------------------------

registerReal :: IO [BenchSpec]
registerReal = do
    mDb <- pickDbFixture
    mMethod <- F.lookupFixture F.MethodEFIlcd
    case (mDb, mMethod) of
        (Just (src, dbPath), Just methodPath) -> do
            putStrLn "[bench] lcia.real.score_method: loading database..."
            dbRes <- H.loadFullDatabase dbPath
            case dbRes of
                Left err -> do
                    putStrLn $ "[bench] lcia.real.score_method: load failed (" <> show err <> "), skipping"
                    pure []
                Right db -> do
                    putStrLn "[bench] lcia.real.score_method: parsing method file..."
                    methodRes <- MP.parseMethodFile methodPath
                    case methodRes of
                        Left err -> do
                            putStrLn $ "[bench] lcia.real.score_method: parse method failed (" <> err <> "), skipping"
                            pure []
                        Right method -> do
                            putStrLn "[bench] lcia.real.score_method: mapping CFs to flows + filling broadcast..."
                            mappings <- mapMethodToFlows Builtin.defaultMappers db method
                            let unitDB = dbUnits db
                                flowDB = dbBioFlows db
                                tables0 = buildMethodTables M.empty M.empty mappings
                                !tables = fillBroadcastVector UC.defaultUnitConfig unitDB flowDB tables0
                                !nCFs = length (methodFactors method)
                            putStrLn "[bench] lcia.real.score_method: computing inventory for first product..."
                            inventory <- Matrix.computeInventoryMatrix db 0
                            -- Force inventory size so build cost is paid here.
                            _ <- evaluate (M.size inventory)
                            pure
                                [ BenchSpec
                                    { bsCapability = "lcia.real.score_method"
                                    , bsLabel = T.pack ("Score one product against an LCIA method (" <> show nCFs <> " CFs)")
                                    , bsDescription =
                                        "Scores one product's biosphere inventory against one full LCIA method, \
                                        \using the cached broadcast vector path. The inventory is computed once \
                                        \(at registration time) and reused; the bench measures only the \
                                        \scoring step. This is the per-method per-product cost a user pays \
                                        \when asking for a single indicator on a real product."
                                    , bsUnitOfWork = UnitOfWork{uowKind = "characterization_factors", uowN = nCFs}
                                    , bsMetric = "milliseconds"
                                    , bsFixture =
                                        J.Fixture
                                            { J.fSource =
                                                F.fixtureSourceLabel src
                                                    <> " + "
                                                    <> F.fixtureSourceLabel F.MethodEFIlcd
                                            , J.fSlice = "one product, whole method"
                                            }
                                    , bsAction =
                                        nf
                                            (\inv -> loScore (computeLCIAScoreFromTables UC.defaultUnitConfig unitDB flowDB inv tables))
                                            inventory
                                    }
                                ]
        _ -> do
            putStrLn "[bench] lcia.real.score_method: need both a DB fixture and VOLCA_BENCH_METHOD_EF_ILCD, skipping"
            pure []

pickDbFixture :: IO (Maybe (F.FixtureSource, FilePath))
pickDbFixture = go [F.Agribalyse, F.Bafu, F.Ecoinvent]
  where
    go [] = pure Nothing
    go (s : ss) = do
        m <- F.lookupFixture s
        case m of
            Just p -> pure (Just (s, p))
            Nothing -> go ss
