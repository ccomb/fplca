{-# LANGUAGE OverloadedStrings #-}

-- | Cross-database back-substitution correctness tests.
--
-- Exercises the WithDeps path that includes biosphere contributions reached
-- through 'dbCrossDBLinks'. Since the test suite has no multi-DB fixture,
-- we test:
--
-- 1. pure helpers on a single loaded sample DB;
-- 2. that 'computeInventoryMatrixBatchWithDepsCached' on a DB with no
--    cross-DB links reduces exactly to the local-only batch variant.
-- 3. that 'depDemandsToVector' honours the supplier's reference unit and
--    fails hard on unknown unit pairs.
module CrossDBInventorySpec (spec) where

import Test.Hspec
import TestHelpers (loadSampleDatabase)
import Types
import Matrix (accumulateDepDemands, depDemandsToVector)
import SharedSolver
    ( createSharedSolver
    , computeInventoryMatrixBatchCached
    , computeInventoryMatrixBatchWithDepsCached
    )
import UnitConversion (defaultUnitConfig)
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.UUID as UUID
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T

spec :: Spec
spec = do
    describe "accumulateDepDemands" $ do

        it "returns empty map when database has no cross-DB links" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let n = fromIntegral (dbActivityCount db) :: Int
                scalingVec = U.replicate n 1.0
            accumulateDepDemands db scalingVec `shouldBe` M.empty

        it "returns empty map when scaling vector is all zeros" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let n = fromIntegral (dbActivityCount db) :: Int
                scalingVec = U.replicate n 0.0
            accumulateDepDemands db scalingVec `shouldBe` M.empty

    describe "depDemandsToVector" $ do

        it "returns zero vector for empty demand map" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let n = fromIntegral (dbActivityCount db) :: Int
            case depDemandsToVector defaultUnitConfig "SAMPLE.min3" db M.empty of
                Right vec -> do
                    U.length vec `shouldBe` n
                    U.all (== 0.0) vec `shouldBe` True
                Left err -> expectationFailure (T.unpack err)

        it "silently drops suppliers not present in the dep DB" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let n = fromIntegral (dbActivityCount db) :: Int
                fakeSupplier = (UUID.nil, UUID.nil)
                demands = M.singleton fakeSupplier (42.0, "kg")
            case depDemandsToVector defaultUnitConfig "SAMPLE.min3" db demands of
                Right vec -> do
                    U.length vec `shouldBe` n
                    U.all (== 0.0) vec `shouldBe` True
                Left err -> expectationFailure (T.unpack err)

        it "passes amount through unchanged when exchange unit matches supplier refUnit" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            case firstActivityWithRefUnit db of
                Nothing -> pendingWith "SAMPLE.min3 has no activity with a reference output unit"
                Just (supplierKey, supplierIdx, refUnit) -> do
                    let demands = M.singleton supplierKey (7.5, refUnit)
                    case depDemandsToVector defaultUnitConfig "SAMPLE.min3" db demands of
                        Right vec -> vec U.! supplierIdx `shouldBe` 7.5
                        Left err  -> expectationFailure (T.unpack err)

        it "fails hard when the unit pair is unknown" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            case firstActivityWithRefUnit db of
                Nothing -> pendingWith "SAMPLE.min3 has no activity with a reference output unit"
                Just (supplierKey, _, refUnit) -> do
                    let bogusUnit = refUnit <> "__no_conversion__"
                        demands   = M.singleton supplierKey (1.0, bogusUnit)
                    case depDemandsToVector defaultUnitConfig "dep-test" db demands of
                        Right _  -> expectationFailure "expected Left for unknown unit pair"
                        Left err -> do
                            err `shouldSatisfy` T.isInfixOf "Unknown unit conversion"
                            err `shouldSatisfy` T.isInfixOf "dep-test"

    describe "computeInventoryMatrixBatchWithDepsCached" $ do

        it "matches local-only batch for a DB with no cross-DB links" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let techTriples = [ (fromIntegral i, fromIntegral j, v)
                              | SparseTriple i j v <- U.toList (dbTechnosphereTriples db) ]
                actCount    = fromIntegral (dbActivityCount db)
            solver <- createSharedSolver "SAMPLE.min3" techTriples actCount
            let pids = [0]
                noDeps _ = pure Nothing

            localInvs <- computeInventoryMatrixBatchCached db solver pids
            withDepsE <- computeInventoryMatrixBatchWithDepsCached defaultUnitConfig noDeps db solver pids

            case withDepsE of
                Left err         -> expectationFailure (T.unpack err)
                Right withDepsInvs -> do
                    length withDepsInvs `shouldBe` length localInvs
                    case (localInvs, withDepsInvs) of
                        ([a], [b]) -> M.toList a `shouldBe` M.toList b
                        _          -> expectationFailure "expected one inventory per pid"

        it "empty pid list returns empty result without solving" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let techTriples = [ (fromIntegral i, fromIntegral j, v)
                              | SparseTriple i j v <- U.toList (dbTechnosphereTriples db) ]
                actCount    = fromIntegral (dbActivityCount db)
            solver <- createSharedSolver "SAMPLE.min3-empty" techTriples actCount
            let noDeps _ = pure Nothing
            res <- computeInventoryMatrixBatchWithDepsCached defaultUnitConfig noDeps db solver []
            res `shouldBe` Right []


-- | Pick the first activity that has a reference output exchange with a
-- known unit, returning ((actUUID, prodUUID), matrixIndex, refUnit).
firstActivityWithRefUnit :: Database -> Maybe ((UUID, UUID), Int, Text)
firstActivityWithRefUnit db =
    listToMaybe
        [ (procId, fromIntegral (dbActivityIndex db V.! idx), unit)
        | (idx, procId) <- zip [0 ..] (V.toList (dbProcessIdTable db))
        , let act   = dbActivities db V.! idx
              refExs = [ex | ex <- exchanges act, exchangeIsReference ex, not (exchangeIsInput ex)]
        , ex <- take 1 refExs
        , let unit = getUnitNameForExchange (dbUnits db) ex
        , not (T.null unit), unit /= "unknown"
        ]
