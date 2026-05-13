{-# LANGUAGE OverloadedStrings #-}

{- | Regression: regional LCIA must score dep-DB biosphere emissions, not
just root-DB ones.

The 'computeRegionalizedLCIAScore' fast path is a dot product
@rawWeights · scalingVec@. Both vectors are built from a single 'Database':
'rawWeights' from that DB's biosphere triples, 'scalingVec' from that DB's
activity columns. So when a root-DB activity consumes a dep-DB activity,
the dep DB's emissions are present in the merged 'Inventory' but invisible
to the regional path — its regional CFs were never queried.

This spec uses the synthetic two-DB fixture from
'CrossDBRegionalLCIAFixture'. The "broken" case shows the root-only score
is 0 (the bug); the "fixed" case shows that summing per-DB regional dot
products recovers the right answer.
-}
module CrossDBRegionalLCIASpec (spec) where

import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as U

import Test.Hspec

import CrossDBRegionalLCIAFixture
import Method.Mapping
import qualified SharedSolver as SS
import TestHelpers (mkSolverFromDb)
import Types

spec :: Spec
spec = describe "cross-DB regional LCIA" $ do
    -- Fixture used by both expectations:
    --   * Root DB R: 1 activity at FR. No biosphere emission. Cross-DB link
    --     to dep DB D's activity #1 (the one at DE), coefficient 1.0.
    --   * Dep DB D: 3 activities at FR, DE, GLO. D's activity #1 (DE) emits
    --     1.0 kg of biosphere flow F.
    --   * Method M: one regional CF table on F: FR=1, DE=5, GLO=0.5.
    --
    -- Demanding R's activity #0 forces 1 unit of D's activity #1 (DE).
    --   * Dep scaling x_D = [0, 1, 0].
    --   * Dep regional weights w_D = [0, 1·CF[F,DE], 0] = [0, 5, 0].
    --   * Root weights w_R: empty (R has no biosphere triples).
    --   * Cross-DB regional score = w_R·x_R + w_D·x_D = 0 + 5 = 5.
    let fix = mkRegionalFixture
        rootDb = rfRootDb fix
        depDb = rfDepDb fix
        rootTables = rfRootTables fix
        depTables = rfDepTables fix

    it "OLD path: root-only regional score silently misses dep-DB emissions" $ do
        -- Today's contract: computeLCIAScoreAuto is handed the ROOT db's
        -- scaling vector and the ROOT db's MethodTables. The merged
        -- Inventory containing F=1 kg is passed too, but the regional path
        -- ignores it — score = rawWeights_root · scaling_root = 0.
        rootSolver <- mkSolverFromDb rootDb "root"
        depSolver <- mkSolverFromDb depDb "dep"
        let depLookup name =
                pure $
                    if name == "dep" then Just (depDb, depSolver) else Nothing
        eRes <-
            SS.computeInventoryMatrixWithDepsCached
                kgUnitConfig
                depLookup
                rootDb
                "root"
                rootSolver
                0 -- root's only activity column
        case eRes of
            Left err -> expectationFailure ("solve failed: " <> show err)
            Right sol -> do
                let inv = SS.csInventory sol
                -- Sanity: dep DB's emission shows up in the merged inventory.
                M.lookup flowUUID inv `shouldBe` Just 1.0
                -- The bug surface: the regional path scored against root
                -- tables/scaling alone returns 0 (or, depending on how the
                -- caller invokes it, the wrong number — never 5).
                let rootScaling = case [s | (n, _, s) <- SS.csScalings sol, n == "root"] of
                        (s : _) -> s
                        [] -> U.empty
                computeRegionalizedLCIAScore
                    kgUnitConfig
                    (dbUnits rootDb)
                    (dbFlows rootDb)
                    rootDb
                    rootScaling
                    M.empty
                    rootTables
                    `shouldBe` Right 0.0

    it "NEW path: per-DB regional sum recovers the dep-DB contribution" $ do
        rootSolver <- mkSolverFromDb rootDb "root"
        depSolver <- mkSolverFromDb depDb "dep"
        let depLookup name =
                pure $
                    if name == "dep" then Just (depDb, depSolver) else Nothing
        eRes <-
            SS.computeInventoryMatrixWithDepsCached
                kgUnitConfig
                depLookup
                rootDb
                "root"
                rootSolver
                0
        case eRes of
            Left err -> expectationFailure ("solve failed: " <> show err)
            Right sol -> do
                let perDb =
                        [ (db, s, tablesFor n)
                        | (n, db, s) <- SS.csScalings sol
                        ]
                    tablesFor n = case n of
                        "root" -> rootTables
                        "dep" -> depTables
                        other -> error ("unexpected dbName in csScalings: " <> show other)
                sumRegionalizedLCIAScoreCrossDB
                    kgUnitConfig
                    (dbUnits depDb)
                    (dbFlows depDb)
                    M.empty
                    perDb
                    `shouldBe` Right 5.0

    it "NEW path: tainted dep-DB drops to 0 contribution; root contribution survives" $ do
        -- Same DBs, but the method only has CF[F, FR]. The dep DB's DE
        -- activity is regionalized in the method (F appears in regional
        -- CFs) but no CF resolves at DE / parents / broadcast — that's a
        -- tainted column, and it carries scaling 1. Per-DB scoring Lefts
        -- on dep. The cross-DB sum tolerates it: drops the dep DB to a 0
        -- contribution and keeps the root DB's Right. The build-time WARN
        -- already names the gap (per-(flow, location) pair); score-time
        -- loudness would regress users who used to see partial scores.
        let strictMappings = regionalMappings [("FR", 1)]
            depTablesStrict = buildTables depDb strictMappings
            rootTablesStrict = buildTables rootDb strictMappings
        rootSolver <- mkSolverFromDb rootDb "root"
        depSolver <- mkSolverFromDb depDb "dep"
        let depLookup name =
                pure $
                    if name == "dep" then Just (depDb, depSolver) else Nothing
        eRes <-
            SS.computeInventoryMatrixWithDepsCached
                kgUnitConfig
                depLookup
                rootDb
                "root"
                rootSolver
                0
        case eRes of
            Left err -> expectationFailure ("solve failed: " <> show err)
            Right sol -> do
                let perDb =
                        [ (db, s, tablesFor n)
                        | (n, db, s) <- SS.csScalings sol
                        ]
                    tablesFor n = case n of
                        "root" -> rootTablesStrict
                        "dep" -> depTablesStrict
                        other -> error ("unexpected dbName in csScalings: " <> show other)
                -- Root has no biosphere triples → contributes Right 0.
                -- Dep biosphere triple at DE has no CF → Left (tainted).
                -- Tolerant sum: Right 0 (root) + 0 dropped (dep) = Right 0.
                sumRegionalizedLCIAScoreCrossDB
                    kgUnitConfig
                    (dbUnits depDb)
                    (dbFlows depDb)
                    M.empty
                    perDb
                    `shouldBe` Right 0.0

    it "NEW path: all-Left case (every participating DB tainted) surfaces Left" $ do
        -- Every DB Lefts (no Right to fall back to). The sum has nothing
        -- to sum, so it Lefts — preserves no-silent-errors when there is
        -- genuinely no recoverable contribution.
        let strictMappings = regionalMappings [("FR", 1)]
            depTablesStrict = buildTables depDb strictMappings
            -- Root variant with a tainted biosphere triple at DE so both
            -- root and dep tables Left on the precomputed dot product.
            taintedRoot = mkDB 100 ["DE"] [(0, 1.0)]
            taintedRootTables = buildTables taintedRoot strictMappings
        rootSolver <- mkSolverFromDb taintedRoot "root"
        depSolver <- mkSolverFromDb depDb "dep"
        let depLookup name =
                pure $
                    if name == "dep" then Just (depDb, depSolver) else Nothing
        eRes <-
            SS.computeInventoryMatrixWithDepsCached
                kgUnitConfig
                depLookup
                taintedRoot
                "root"
                rootSolver
                0
        case eRes of
            Left err -> expectationFailure ("solve failed: " <> show err)
            Right sol -> do
                let perDb =
                        [ (db, s, tablesFor n)
                        | (n, db, s) <- SS.csScalings sol
                        ]
                    tablesFor n = case n of
                        "root" -> taintedRootTables
                        "dep" -> depTablesStrict
                        other -> error ("unexpected dbName in csScalings: " <> show other)
                case sumRegionalizedLCIAScoreCrossDB
                    kgUnitConfig
                    (dbUnits depDb)
                    (dbFlows depDb)
                    M.empty
                    perDb of
                    Left _ -> pure ()
                    Right v ->
                        expectationFailure
                            ( "expected Left when every DB Lefts, got Right "
                                <> show v
                            )
