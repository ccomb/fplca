{-# LANGUAGE OverloadedStrings #-}

{- | Microbenchmark for the LCIA scoring fast paths.

Three configurations, all on the same synthetic fixture (10K biosphere flows,
~5K CF matches per method, 27 PEF-sized methods, 500-flow sparse inventory):

  * @cascade-mono@: per-method scoring with the legacy 3-level CF cascade
    + per-flow @convertUnit@ (mtBroadcast empty). Matches pre-Phase-1 behaviour.
  * @broadcast-mono@: per-method scoring with mtBroadcast filled. Phase 1.
  * @broadcast-fanout@: simulates the per-method HTTP loop — score the same
    inventory 27 times in sequence, each call hitting the broadcast Map.
  * @set-batched@: 'computeLCIAScoreSetFromTables' on the same set, single
    inventory walk + dense matvec. Phase 2.

Speedup ratios:
  * Phase 1: cascade-mono / broadcast-mono.
  * Phase 2: broadcast-fanout / set-batched.
-}
module Main (main) where

import Criterion.Main
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.Vector.Unboxed as U
import System.Random (mkStdGen, randoms)

import Matrix (Inventory)
import Method.Mapping
import Method.Types (Method (..), MethodCF (..))
import qualified Method.Types as MT
import Types (Database, Flow (..), FlowType (..), Unit (..))
import qualified UnitConversion

-- ---------------------------------------------------------------------------
-- Fixture sizes (PEF-shaped)
-- ---------------------------------------------------------------------------

nFlowsTotal :: Int
nFlowsTotal = 10_000 -- biosphere flows in the merged DB

nCFsPerMethod :: Int
nCFsPerMethod = 5_000 -- CF matches per method (half of flows)

nMethods :: Int
nMethods = 27 -- PEF-shaped

nInvNonzero :: Int
nInvNonzero = 500 -- non-zero entries in the supply-vector inventory

-- ---------------------------------------------------------------------------
-- Fixture builders
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

mkFlow :: UUID -> UUID -> Int -> Flow
mkFlow fid uid i =
    Flow
        { flowId = fid
        , flowName = T.pack ("flow-" <> show i)
        , flowCategory = "air"
        , flowSubcompartment = Nothing
        , flowUnitId = uid
        , flowType = Biosphere
        , flowSynonyms = M.empty
        , flowCAS = Nothing
        , flowSubstanceId = Nothing
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

-- | A CF with a different unit than the matched flow, forcing the legacy
-- 'convertUnit' branch to run on every scored flow. Phase 1's win is most
-- visible against this kind of CF (the broadcast path absorbs the
-- conversion factor at build time).
mkCFGramme :: UUID -> Double -> MethodCF
mkCFGramme flowRef val =
    (mkCF flowRef val){mcfUnit = "g"}

-- | Fresh UnitConfig that knows both kg and g, so the convertUnit calls in
-- the legacy fixture actually succeed (default config only knows kg).
unitConfigKgG :: UnitConversion.UnitConfig
unitConfigKgG =
    UnitConversion.UnitConfig
        { UnitConversion.ucDimensionOrder = []
        , UnitConversion.ucUnits =
            M.fromList
                [ ("kg", UnitConversion.UnitDef [1, 0, 0, 0, 0, 0, 0, 0] 1.0)
                , ("g", UnitConversion.UnitDef [1, 0, 0, 0, 0, 0, 0, 0] 1.0e-3)
                ]
        , UnitConversion.ucOriginalKeys = M.fromList [("kg", "kg"), ("g", "g")]
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

-- The non-regio scoring path never inspects the Database argument.
unusedDatabase :: Database
unusedDatabase = error "Database not used in non-regio bench"

-- ---------------------------------------------------------------------------
-- Build the fixture once
-- ---------------------------------------------------------------------------

data Fixture = Fixture
    { fxFlowDB :: !(M.Map UUID Flow)
    , fxUnitDB :: !(M.Map UUID Unit)
    , fxUnitCfg :: !UnitConversion.UnitConfig
    , fxInventory :: !Inventory
    , fxMethods :: ![Method]
    , fxRawTables :: ![MethodTables] -- mtBroadcast empty (legacy cascade)
    , fxFilledTables :: ![MethodTables] -- mtBroadcast filled (Phase 1)
    , fxSetTables :: !MethodSetTables -- stacked broadcast (Phase 2)
    }

buildFixture :: (UUID -> Double -> MethodCF) -> UnitConversion.UnitConfig -> Fixture
buildFixture mkCfFn unitCfg =
    let kgUid = mkUuid 99_999
        unitDB = M.singleton kgUid (mkUnit kgUid "kg")
        flows =
            [ (fid, mkFlow fid kgUid i)
            | i <- [0 .. nFlowsTotal - 1]
            , let fid = mkUuid i
            ]
        flowDB = M.fromList flows

        -- Pseudo-random CF values, deterministic per-method seed.
        cfValuesFor :: Int -> [Double]
        cfValuesFor seed =
            -- Map [0,1) doubles to a [0.01, 100) log-ish range; keep them non-zero.
            map (\r -> 0.01 + r * 99.99) (take nCFsPerMethod (randoms (mkStdGen seed) :: [Double]))

        -- Each method matches the first nCFsPerMethod flows by UUID.
        mappingsFor seed =
            [ (mkCfFn fid v, Just (snd (flows !! i), ByUUID))
            | (i, v) <- zip [0 ..] (cfValuesFor seed)
            , let fid = mkUuid i
            ]

        methods = [mkMethod m [] | m <- [1 .. nMethods]] -- factors don't matter, mappings drive build
        rawTables = [buildMethodTables M.empty (mappingsFor s) | s <- [1 .. nMethods]]
        filledTables = map (fillBroadcastVector unitCfg unitDB flowDB) rawTables
        setTables = buildMethodSetTables (zip methods filledTables)

        -- Sparse inventory: pick 500 flows out of 10_000 (one per 20).
        inventory =
            M.fromList
                [ (mkUuid (i * (nFlowsTotal `div` nInvNonzero)), 1.0 + fromIntegral i * 0.1)
                | i <- [0 .. nInvNonzero - 1]
                ]
     in Fixture
            { fxFlowDB = flowDB
            , fxUnitDB = unitDB
            , fxUnitCfg = unitCfg
            , fxInventory = inventory
            , fxMethods = methods
            , fxRawTables = rawTables
            , fxFilledTables = filledTables
            , fxSetTables = setTables
            }

-- ---------------------------------------------------------------------------
-- Bench groups
-- ---------------------------------------------------------------------------

-- | Score one inventory against ONE method (the 1st in the list), legacy cascade.
benchMonoCascade :: Fixture -> Double
benchMonoCascade fx =
    loScore $
        computeLCIAScoreFromTables
            (fxUnitCfg fx)
            (fxUnitDB fx)
            (fxFlowDB fx)
            (fxInventory fx)
            (head (fxRawTables fx))

-- | Score one inventory against ONE method, broadcast filled (Phase 1).
benchMonoBroadcast :: Fixture -> Double
benchMonoBroadcast fx =
    loScore $
        computeLCIAScoreFromTables
            (fxUnitCfg fx)
            (fxUnitDB fx)
            (fxFlowDB fx)
            (fxInventory fx)
            (head (fxFilledTables fx))

-- | Score the same inventory once per method, sequential — what the HTTP route
-- does with mapConcurrently sans the parallelism overhead.
benchMonoFanout :: Fixture -> [Double]
benchMonoFanout fx =
    [ loScore $
        computeLCIAScoreFromTables
            (fxUnitCfg fx)
            (fxUnitDB fx)
            (fxFlowDB fx)
            (fxInventory fx)
            t
    | t <- fxFilledTables fx
    ]

-- | Score all methods at once via stacked-broadcast matvec (Phase 2).
-- No regional methods → 'scoreRegionalCrossDB' never inspects the triple,
-- so the unused Database / empty scaling vector are never forced.
benchSetBatched :: Fixture -> [(UUID, Either Text Double)]
benchSetBatched fx =
    computeLCIAScoreSetFromTables
        (fxUnitCfg fx)
        (fxUnitDB fx)
        (fxFlowDB fx)
        (fxInventory fx)
        M.empty
        ((unusedDatabase, U.empty, fxSetTables fx) :| [])

main :: IO ()
main = do
    -- Same-units fixture: flow is "kg", CF is "kg" → legacy short-circuits
    -- the convertUnit call. Phase 1's win is small here; Phase 2 wins big.
    let fxSameUnit = buildFixture mkCF UnitConversion.defaultUnitConfig
    -- Cross-units fixture: flow is "kg", CF is "g" → legacy runs convertUnit
    -- per scored flow. This is where Phase 1's pre-multiplication shines.
    let fxXUnit = buildFixture mkCFGramme unitConfigKgG
    fxSameUnit `seq` fxFilledTables fxSameUnit `seq` fxSetTables fxSameUnit `seq` pure ()
    fxXUnit `seq` fxFilledTables fxXUnit `seq` fxSetTables fxXUnit `seq` pure ()
    defaultMain
        [ bgroup
            "single-method (same units, legacy short-circuits)"
            [ bench "cascade (legacy)" $ nf benchMonoCascade fxSameUnit
            , bench "broadcast (Phase 1)" $ nf benchMonoBroadcast fxSameUnit
            ]
        , bgroup
            "single-method (cross units, legacy must convert)"
            [ bench "cascade (legacy)" $ nf benchMonoCascade fxXUnit
            , bench "broadcast (Phase 1)" $ nf benchMonoBroadcast fxXUnit
            ]
        , bgroup
            ("multi-method " <> show nMethods <> " (same units)")
            [ bench "fanout broadcast (per-method loop)" $ nf benchMonoFanout fxSameUnit
            , bench "set batched (Phase 2 matvec)" $ nf benchSetBatched fxSameUnit
            ]
        , bgroup
            ("multi-method " <> show nMethods <> " (cross units)")
            [ bench "fanout broadcast (per-method loop)" $ nf benchMonoFanout fxXUnit
            , bench "set batched (Phase 2 matvec)" $ nf benchSetBatched fxXUnit
            ]
        ]
