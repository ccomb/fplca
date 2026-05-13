{-# LANGUAGE OverloadedStrings #-}

{- | Tests for the multi-method scoring path: 'MethodSetTables' +
'computeLCIAScoreSetFromTables'. Focused on correctness of the batched
matvec vs per-method dispatch and on cache-key canonicality at the pure
data-structure level.

Cache lifecycle (TVar in 'DatabaseManager') is exercised indirectly by the
existing route tests; here we keep dependencies minimal and test the pure
functions directly.
-}
module MethodSetTablesSpec (spec) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Test.Hspec

import Matrix (Inventory)
import Method.Mapping
import Method.Types (Method (..), MethodCF (..))
import qualified Method.Types as MT
import Types (Database, Flow (..), FlowType (..), Unit (..))
import qualified UnitConversion

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

mkUuid :: Int -> UUID
mkUuid n = UUID.fromWords (fromIntegral n) 0 0 0

mkUnit :: UUID -> Text -> Unit
mkUnit uid name = Unit{unitId = uid, unitName = name, unitSymbol = name, unitComment = ""}

mkFlow :: UUID -> Text -> UUID -> Flow
mkFlow fid name uId =
    Flow
        { flowId = fid
        , flowName = name
        , flowCategory = "air"
        , flowSubcompartment = Nothing
        , flowUnitId = uId
        , flowType = Biosphere
        , flowSynonyms = M.empty
        , flowCAS = Nothing
        , flowSubstanceId = Nothing
        }

mkCF :: UUID -> Double -> MethodCF
mkCF flowRef val =
    MethodCF
        { mcfFlowRef = flowRef
        , mcfFlowName = "co2"
        , mcfDirection = MT.Output
        , mcfValue = val
        , mcfCompartment = Nothing
        , mcfCAS = Nothing
        , mcfUnit = "kg"
        , mcfConsumerLocation = Nothing
        }

mkMethod :: Int -> Text -> [MethodCF] -> Method
mkMethod n name factors =
    Method
        { methodId = mkUuid n
        , methodName = name
        , methodDescription = Nothing
        , methodUnit = "kg eq"
        , methodCategory = name
        , methodMethodology = Nothing
        , methodFactors = factors
        }

-- The non-regio batched path never reads from the 'Database' argument; it
-- lives only on the per-method (regio) branch. We pass 'undefined' to avoid
-- wiring a heavyweight stub. This is safe under Haskell's laziness for the
-- non-regio tests below.
unusedDatabase :: Database
unusedDatabase = error "Database value not used in non-regio scoring"

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
    describe "buildMethodSetTables" $ do
        it "puts non-regional methods in msBatched, leaves msRegional empty" $ do
            let fid = mkUuid 100
                uidKg = mkUuid 200
                cf = mkCF fid 1.0
                m1 = mkMethod 1 "m1" [cf]
                tables0 = buildMethodTables M.empty [(cf, Just (mkFlow fid "co2" uidKg, ByUUID))]
                fdb = M.singleton fid (mkFlow fid "co2" uidKg)
                udb = M.singleton uidKg (mkUnit uidKg "kg")
                filled = fillBroadcastVector UnitConversion.defaultUnitConfig udb fdb tables0
                mst = buildMethodSetTables [(m1, filled)]
                bt = msBatched mst
            V.length (msRegional mst) `shouldBe` 0
            btNMethods bt `shouldBe` 1
            btNFlows bt `shouldBe` 1
            U.length (btMat bt) `shouldBe` 1

        it "puts regional methods in msRegional, keeps msBatched non-regional only" $ do
            let fid = mkUuid 100
                uidKg = mkUuid 200
                cf = mkCF fid 1.0
                tables0 =
                    (buildMethodTables M.empty [(cf, Just (mkFlow fid "co2" uidKg, ByUUID))])
                        { mtRegionalizedCF = M.singleton (fid, "FR") (2.0, "kg")
                        }
                m1 = mkMethod 1 "m1" [cf]
                fdb = M.singleton fid (mkFlow fid "co2" uidKg)
                udb = M.singleton uidKg (mkUnit uidKg "kg")
                filled = fillBroadcastVector UnitConversion.defaultUnitConfig udb fdb tables0
                mst = buildMethodSetTables [(m1, filled)]
                bt = msBatched mst
            V.length (msRegional mst) `shouldBe` 1
            btNMethods bt `shouldBe` 0
            btNFlows bt `shouldBe` 0
            U.null (btMat bt) `shouldBe` True

    describe "computeLCIAScoreSetFromTables (non-regio batched matvec)" $ do
        it "matches per-method computeLCIAScoreFromTables on a 3-method set" $ do
            let fid1 = mkUuid 100
                fid2 = mkUuid 101
                uidKg = mkUuid 200
                flow1 = mkFlow fid1 "co2" uidKg
                flow2 = mkFlow fid2 "ch4" uidKg
                fdb = M.fromList [(fid1, flow1), (fid2, flow2)]
                udb = M.singleton uidKg (mkUnit uidKg "kg")

                cfA1 = mkCF fid1 2.0 -- method A: co2=2, ch4 absent
                cfB1 = mkCF fid1 1.0 -- method B: co2=1, ch4=25
                cfB2 = (mkCF fid2 25.0){mcfFlowName = "ch4"}
                cfC1 = mkCF fid1 0.5 -- method C: co2=0.5, ch4 absent
                mA = mkMethod 1 "A" [cfA1]
                mB = mkMethod 2 "B" [cfB1, cfB2]
                mC = mkMethod 3 "C" [cfC1]

                fill ts = fillBroadcastVector UnitConversion.defaultUnitConfig udb fdb ts
                tA = fill (buildMethodTables M.empty [(cfA1, Just (flow1, ByUUID))])
                tB = fill (buildMethodTables M.empty [(cfB1, Just (flow1, ByUUID)), (cfB2, Just (flow2, ByUUID))])
                tC = fill (buildMethodTables M.empty [(cfC1, Just (flow1, ByUUID))])

                mst = buildMethodSetTables [(mA, tA), (mB, tB), (mC, tC)]

                inv :: Inventory
                inv = M.fromList [(fid1, 4.0), (fid2, 0.1)]

                -- Per-method legacy scores (golden)
                sA = loScore (computeLCIAScoreFromTables UnitConversion.defaultUnitConfig udb fdb inv tA)
                sB = loScore (computeLCIAScoreFromTables UnitConversion.defaultUnitConfig udb fdb inv tB)
                sC = loScore (computeLCIAScoreFromTables UnitConversion.defaultUnitConfig udb fdb inv tC)

                -- Batched scores via the set path
                results =
                    computeLCIAScoreSetFromTables
                        UnitConversion.defaultUnitConfig
                        udb
                        fdb
                        inv
                        M.empty -- hier unused for non-regio
                        (NE.singleton (unusedDatabase, U.empty, mst)) -- scalingVec unused for non-regio
                resultMap = M.fromList results
            -- Sanity: explicit numbers
            sA `shouldBe` 8.0 -- 4*2 + 0.1*0 (no ch4 cf)
            sB `shouldBe` 6.5 -- 4*1 + 0.1*25
            sC `shouldBe` 2.0 -- 4*0.5 + 0
            -- Set scoring matches per-method to the bit
            M.lookup (methodId mA) resultMap `shouldBe` Just (Right sA)
            M.lookup (methodId mB) resultMap `shouldBe` Just (Right sB)
            M.lookup (methodId mC) resultMap `shouldBe` Just (Right sC)

        it "empty inventory scores all methods to 0" $ do
            let fid = mkUuid 100
                uidKg = mkUuid 200
                cf = mkCF fid 1.0
                m1 = mkMethod 1 "m1" [cf]
                m2 = mkMethod 2 "m2" [cf]
                fdb = M.singleton fid (mkFlow fid "co2" uidKg)
                udb = M.singleton uidKg (mkUnit uidKg "kg")
                fill ts = fillBroadcastVector UnitConversion.defaultUnitConfig udb fdb ts
                t = fill (buildMethodTables M.empty [(cf, Just (mkFlow fid "co2" uidKg, ByUUID))])
                mst = buildMethodSetTables [(m1, t), (m2, t)]
                results =
                    computeLCIAScoreSetFromTables
                        UnitConversion.defaultUnitConfig
                        udb
                        fdb
                        M.empty
                        M.empty
                        (NE.singleton (unusedDatabase, U.empty, mst))
            map snd results `shouldBe` [Right 0.0, Right 0.0]

        it "inventory UUIDs absent from both broadcast and flowDB contribute 0" $ do
            -- A UUID that exists nowhere — no broadcast row, no flowDB entry —
            -- still scores zero because the per-method cascade fallback also
            -- misses it (nothing for 'lookupCascadeCF' to anchor against).
            let fidIn = mkUuid 100
                fidOut = mkUuid 999 -- in inventory only, unreachable
                uidKg = mkUuid 200
                cf = mkCF fidIn 3.0
                m1 = mkMethod 1 "m1" [cf]
                fdb = M.singleton fidIn (mkFlow fidIn "co2" uidKg)
                udb = M.singleton uidKg (mkUnit uidKg "kg")
                t =
                    fillBroadcastVector UnitConversion.defaultUnitConfig udb fdb $
                        buildMethodTables M.empty [(cf, Just (mkFlow fidIn "co2" uidKg, ByUUID))]
                mst = buildMethodSetTables [(m1, t)]
                inv = M.fromList [(fidIn, 2.0), (fidOut, 100.0)]
                results =
                    computeLCIAScoreSetFromTables
                        UnitConversion.defaultUnitConfig
                        udb
                        fdb
                        inv
                        M.empty
                        (NE.singleton (unusedDatabase, U.empty, mst))
            -- Only the matched flow contributes: 2 × 3 = 6.
            map snd results `shouldBe` [Right 6.0]

        it "out-of-broadcast UUID resolved via mtUuidCF contributes via cascade fallback" $ do
            -- Regression gate: a merged inventory carries UUIDs whose flows
            -- were not in the root flowDB at 'fillBroadcastVector' time, so
            -- they have no row in 'btMat' and miss 'btUuidIndex'.
            -- The CF table itself ('mtUuidCF', built from the full mapping
            -- set) still resolves them — that's the per-method 'fastScore'
            -- fallback ('lookupCascadeCF'). Before the cascade fallback in
            -- the batched walker, those flows silently scored zero. This
            -- test pins the equivalence.
            let fidBuild = mkUuid 100 -- in build flowDB → in broadcast
                fidCrossDB = mkUuid 999 -- in CF table + scoring flowDB, NOT in build flowDB
                uidKg = mkUuid 200
                cfBuild = mkCF fidBuild 3.0
                cfCross = mkCF fidCrossDB 3.0
                m1 = mkMethod 1 "m1" [cfBuild, cfCross]
                -- flowDB at build time: only fidBuild. 'fillBroadcastVector'
                -- walks this, so 'mtBroadcast' / 'btUuidIndex' = {fidBuild}.
                buildFlowDB = M.singleton fidBuild (mkFlow fidBuild "co2" uidKg)
                -- flowDB at scoring time (merged inventories carry more
                -- flows than the root DB had at table-build time).
                scoringFlowDB =
                    M.fromList
                        [ (fidBuild, mkFlow fidBuild "co2" uidKg)
                        , (fidCrossDB, mkFlow fidCrossDB "co2" uidKg)
                        ]
                udb = M.singleton uidKg (mkUnit uidKg "kg")
                t =
                    fillBroadcastVector UnitConversion.defaultUnitConfig udb buildFlowDB $
                        buildMethodTables
                            M.empty
                            [ (cfBuild, Just (mkFlow fidBuild "co2" uidKg, ByUUID))
                            , (cfCross, Just (mkFlow fidCrossDB "co2" uidKg, ByUUID))
                            ]
                mst = buildMethodSetTables [(m1, t)]
                inv = M.fromList [(fidBuild, 2.0), (fidCrossDB, 4.0)]
                results =
                    computeLCIAScoreSetFromTables
                        UnitConversion.defaultUnitConfig
                        udb
                        scoringFlowDB
                        inv
                        M.empty
                        (NE.singleton (unusedDatabase, U.empty, mst))
            -- Both contribute against CF=3: (2 + 4) × 3 = 18.
            map snd results `shouldBe` [Right 18.0]

    describe "msAllMethods preserves caller-given order" $ do
        it "preserves the order methods were passed in" $ do
            let fid = mkUuid 100
                uidKg = mkUuid 200
                cf = mkCF fid 1.0
                fdb = M.singleton fid (mkFlow fid "co2" uidKg)
                udb = M.singleton uidKg (mkUnit uidKg "kg")
                t =
                    fillBroadcastVector UnitConversion.defaultUnitConfig udb fdb $
                        buildMethodTables M.empty [(cf, Just (mkFlow fid "co2" uidKg, ByUUID))]
                mB = mkMethod 2 "B" [cf]
                mA = mkMethod 1 "A" [cf]
                mC = mkMethod 3 "C" [cf]
                mst = buildMethodSetTables [(mB, t), (mA, t), (mC, t)]
                ids = V.toList $ V.map mseMethodId (msAllMethods mst)
            ids `shouldBe` [methodId mB, methodId mA, methodId mC]

    describe "mixed regional + non-regional set" $ do
        -- This is the failure mode the partition fixes. Pre-PR, a single
        -- regional method anywhere in the set flipped 'msAnyRegional' to
        -- True and forced every method (regional or not) down the slow
        -- per-method walk. The partition restores the batched matvec for
        -- the non-regional half while keeping per-method dispatch for the
        -- regional half — and crucially, the merged result list must come
        -- back in caller order, not partition order.
        it "merges batched + regional scores in caller order, bit-identical to mono-method" $ do
            let fid1 = mkUuid 100
                fid2 = mkUuid 101
                uidKg = mkUuid 200
                flow1 = mkFlow fid1 "co2" uidKg
                flow2 = mkFlow fid2 "ch4" uidKg
                fdb = M.fromList [(fid1, flow1), (fid2, flow2)]
                udb = M.singleton uidKg (mkUnit uidKg "kg")
                fill ts = fillBroadcastVector UnitConversion.defaultUnitConfig udb fdb ts
                -- Two non-regional CFs (m1 on fid1; m3 on fid1+fid2) and a
                -- regional one (m2 with a per-location override on fid1).
                cf1a = mkCF fid1 2.0
                cf2a = mkCF fid1 5.0
                cf3a = mkCF fid1 1.0
                cf3b = (mkCF fid2 25.0){mcfFlowName = "ch4"}
                tNonRegio1 =
                    fill (buildMethodTables M.empty [(cf1a, Just (flow1, ByUUID))])
                tRegio =
                    fill
                        ( (buildMethodTables M.empty [(cf2a, Just (flow1, ByUUID))])
                            { mtRegionalizedCF = M.singleton (fid1, "FR") (7.0, "kg")
                            }
                        )
                tNonRegio2 =
                    fill
                        ( buildMethodTables
                            M.empty
                            [ (cf3a, Just (flow1, ByUUID))
                            , (cf3b, Just (flow2, ByUUID))
                            ]
                        )
                m1 = mkMethod 1 "non-regio A" [cf1a]
                m2 = mkMethod 2 "regio" [cf2a]
                m3 = mkMethod 3 "non-regio B" [cf3a, cf3b]
                -- Interleave: [non-regio, regio, non-regio]. If the merge
                -- pulled batched-first or regional-first, this order would
                -- not survive.
                mst =
                    buildMethodSetTables
                        [(m1, tNonRegio1), (m2, tRegio), (m3, tNonRegio2)]
                inv :: Inventory
                inv = M.fromList [(fid1, 4.0), (fid2, 0.1)]
                results =
                    computeLCIAScoreSetFromTables
                        UnitConversion.defaultUnitConfig
                        udb
                        fdb
                        inv
                        M.empty
                        (NE.singleton (unusedDatabase, U.empty, mst))
            -- Caller order preserved on the result keys.
            map fst results
                `shouldBe` [methodId m1, methodId m2, methodId m3]
            -- Non-regional scores are bit-identical to scoring the same
            -- method alone via computeLCIAScoreFromTables.
            let s1 = loScore (computeLCIAScoreFromTables UnitConversion.defaultUnitConfig udb fdb inv tNonRegio1)
                s3 = loScore (computeLCIAScoreFromTables UnitConversion.defaultUnitConfig udb fdb inv tNonRegio2)
            map snd results !! 0 `shouldBe` Right s1
            map snd results !! 2 `shouldBe` Right s3
