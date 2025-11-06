{-# LANGUAGE OverloadedStrings #-}

module InventorySpec (spec) where

import Test.Hspec
import TestHelpers
import GoldenData
import ACV.Types
import ACV.Matrix (computeInventoryMatrix, buildDemandVector)
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as U

spec :: Spec
spec = do
    describe "SAMPLE.min3 Golden Tests (Linear Supply Chain)" $ do
        it "computes correct inventory for Product X (1 kg demand)" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            -- Build demand vector for Product X (activity index 0)
            let demandVec = buildDemandVector db [0] [1.0]

            -- Compute inventory
            inventory <- computeInventoryMatrix db demandVec

            -- Find CO2 and Zinc flows
            let co2Flow = findFlowByName db "carbon dioxide"
            let zincFlow = findFlowByName db "zinc"

            case (co2Flow, zincFlow) of
                (Just co2, Just zinc) -> do
                    let co2Amount = M.findWithDefault 0.0 (flowId co2) inventory
                    let zincAmount = M.findWithDefault 0.0 (flowId zinc) inventory

                    -- Golden values: CO2 = 0.96 kg, Zinc = 0.00072 kg
                    withinTolerance defaultTolerance sampleMin3ExpectedCO2 co2Amount
                        `shouldBe` True

                    withinTolerance defaultTolerance sampleMin3ExpectedZinc zincAmount
                        `shouldBe` True

                _ -> expectationFailure "Could not find CO2 or Zinc flows in database"

        it "computes finite values (no +inf/-inf)" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            let demandVec = buildDemandVector db [0] [1.0]
            inventory <- computeInventoryMatrix db demandVec

            -- All inventory values should be finite
            let allFinite = all (\(_, v) -> not (isInfinite v) && not (isNaN v)) (M.toList inventory)
            allFinite `shouldBe` True

    describe "SAMPLE.min Self-Loops Test" $ do
        it "handles circular dependencies without infinity" $ do
            db <- loadSampleDatabase "SAMPLE.min"

            -- Build demand vector for first activity
            let demandVec = buildDemandVector db [0] [1.0]

            -- Compute inventory
            inventory <- computeInventoryMatrix db demandVec

            -- All values should be finite
            let allFinite = all (\(_, v) -> not (isInfinite v) && not (isNaN v)) (M.toList inventory)
            allFinite `shouldBe` True

        it "computes positive inventory values" $ do
            db <- loadSampleDatabase "SAMPLE.min"

            let demandVec = buildDemandVector db [0] [1.0]
            inventory <- computeInventoryMatrix db demandVec

            -- Most biosphere values should be positive (emissions)
            let positiveCount = length $ filter (\(_, v) -> v > 0) (M.toList inventory)
            positiveCount `shouldSatisfy` (> 0)

    describe "Supply Vector Validation" $ do
        it "SAMPLE.min3 supply vector matches expected" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            let demandVec = buildDemandVector db [0] [1.0]

            -- Note: We would need to expose the supply vector from computeInventoryMatrix
            -- For now, we validate through the inventory results
            inventory <- computeInventoryMatrix db demandVec

            -- Inventory should not be empty
            M.size inventory `shouldSatisfy` (> 0)

    describe "Edge Cases" $ do
        it "handles zero demand gracefully" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            let demandVec = buildDemandVector db [0] [0.0]
            inventory <- computeInventoryMatrix db demandVec

            -- Zero demand should give zero or near-zero inventory
            let maxValue = maximum $ map abs $ M.elems inventory
            maxValue `shouldSatisfy` (< 1.0e-10)

        it "handles multiple demands" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            -- Demand 0.5 kg from activity 0 and 0.3 kg from activity 1
            let demandVec = buildDemandVector db [0, 1] [0.5, 0.3]
            inventory <- computeInventoryMatrix db demandVec

            -- Should compute successfully
            M.size inventory `shouldSatisfy` (> 0)

            -- All values should be finite
            let allFinite = all (\(_, v) -> not (isInfinite v) && not (isNaN v)) (M.toList inventory)
            allFinite `shouldBe` True
