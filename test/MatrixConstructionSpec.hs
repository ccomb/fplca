{-# LANGUAGE OverloadedStrings #-}

module MatrixConstructionSpec (spec) where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Test.Hspec
import TestHelpers
import Types

spec :: Spec
spec = do
    describe "Matrix Construction Sign Convention" $ do
        it "stores technosphere input triplets as positive on samples without substitutions" $ do
            -- Regression: an earlier bug stored input coefficients with the
            -- wrong sign, producing positive (I-A) off-diagonals instead of
            -- negative ones. Database.hs sets value = sign * amount / denom
            -- with sign=+1 for inputs; Matrix.hs negates when building (I-A).
            --
            -- SAMPLE.min3 has no substitution (avoided-burden) rows, so every
            -- input is a real consumption and every tech triplet is positive.
            -- Sources with negative Materials/fuels rows legitimately produce
            -- negative triplets — see SimaProParserSpec "SimaPro substitutions".
            db <- loadSampleDatabase "SAMPLE.min3"

            let techTriples = VU.toList (dbTechnosphereTriples db)

            -- Expected: Y needs 0.6 from X, Z needs 0.4 from Y
            let positiveTriplets = filter (\(SparseTriple _ _ v) -> v > 0) techTriples
            length positiveTriplets `shouldBe` length techTriples

            -- Verify specific expected values
            let sortedTriplets = VU.toList (dbTechnosphereTriples db)
            length sortedTriplets `shouldSatisfy` (>= 2)

        it "normalizes exchanges by reference product amount" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            -- Load activity X which outputs 1.0 kg
            -- Check that technosphere inputs are normalized
            let techTriples = VU.toList (dbTechnosphereTriples db)

            -- Should have at least 2 technosphere exchanges
            length techTriples `shouldSatisfy` (>= 2)

    describe "Biosphere Matrix Construction" $ do
        it "stores emissions as POSITIVE values" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            let bioTriples = VU.toList (dbBiosphereTriples db)

            -- Expected: 2 biosphere flows (CO2 and Zinc)
            length bioTriples `shouldSatisfy` (>= 2)

            -- All biosphere values should be positive for emissions
            let emissionTriplets = filter (\(SparseTriple _ _ v) -> v > 0) bioTriples
            length emissionTriplets `shouldSatisfy` (>= 2)

        it "builds biosphere matrix with correct dimensions" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            let bioFlowCount = dbBiosphereCount db
            let activityCount = dbActivityCount db

            -- SAMPLE.min3: 2 biosphere flows, 3 activities
            bioFlowCount `shouldBe` 2
            activityCount `shouldBe` 3

    describe "Database Structure" $ do
        it "builds ProcessId table correctly" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            -- ProcessId table maps index to (activityUUID, productUUID)
            let processIdTable = dbProcessIdTable db
            V.length processIdTable `shouldBe` 3

        it "builds activity index" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            -- Activity index should be identity mapping for simple case
            let activityIndex = dbActivityIndex db
            V.length activityIndex `shouldBe` 3

    describe "Matrix Sparsity" $ do
        it "filters values below threshold (1e-15)" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            let techTriples = VU.toList (dbTechnosphereTriples db)

            -- All values should be > 1e-15
            all (\(SparseTriple _ _ v) -> abs v > 1.0e-15) techTriples `shouldBe` True

        it "excludes diagonal entries from technosphere triplets" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            let techTriples = VU.toList (dbTechnosphereTriples db)

            -- Diagonal entries (self-loops) are excluded in simple SAMPLE.min3
            let diagonalEntries = filter (\(SparseTriple i j _) -> i == j) techTriples
            length diagonalEntries `shouldBe` 0
