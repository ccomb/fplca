{-# LANGUAGE OverloadedStrings #-}

module RegionFallbackSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Test.Hspec

import Method.Mapping (MatchStrategy (..), MethodTables, buildMethodTables, lookupCFForFlow)
import Method.Types (Compartment (..), FlowDirection (..), MethodCF (..), extractLocationSuffix)
import Types (BiosphereFlow (..))
import qualified Types as VT

mkUUID :: Integer -> UUID
mkUUID n = UUID.fromWords64 (fromIntegral n) 0

-- A method CF (emission to air) named for the base substance.
baseCF :: Text -> Double -> MethodCF
baseCF name val =
    MethodCF
        { mcfFlowRef = mkUUID 1
        , mcfFlowName = name
        , mcfDirection = Output
        , mcfValue = val
        , mcfCompartment = Just (Compartment "air" "" "")
        , mcfCAS = Nothing
        , mcfUnit = "kg"
        , mcfConsumerLocation = Nothing
        }

mkFlow :: Integer -> Text -> BiosphereFlow
mkFlow i name =
    BiosphereFlow
        { bfId = mkUUID i
        , bfName = name
        , bfUnitId = mkUUID 0
        , bfSynonyms = M.empty
        , bfCAS = Nothing
        , bfSubstanceId = Nothing
        , bfCompartment = Just (VT.Compartment "air" Nothing)
        }

{- | Tables holding a name-matched CF for a base substance (no region tag), the
situation a non-regionalized method (e.g. EF v3.1 JRC ILCD) produces.
-}
tablesFor :: Text -> Double -> MethodTables
tablesFor base val =
    buildMethodTables "" M.empty M.empty [(baseCF base val, Just (mkFlow 1 base, ByName))]

-- | Score a flow of the given name against those tables.
scoreOf :: Text -> Double -> Text -> Maybe Double
scoreOf base val flowName =
    fmap fst (lookupCFForFlow (tablesFor base val) (mkUUID 99) (Just (mkFlow 99 flowName)))

spec :: Spec
spec = do
    describe "extractLocationSuffix" $ do
        it "splits a real region code off the base name" $
            extractLocationSuffix "Ammonia, FR" `shouldBe` ("Ammonia", Just "FR")
        it "splits a regional aggregate" $
            extractLocationSuffix "Ammonia, RER" `shouldBe` ("Ammonia", Just "RER")
        it "leaves a lowercase chemical qualifier intact" $
            extractLocationSuffix "Methane, fossil" `shouldBe` ("Methane, fossil", Nothing)
        it "leaves 'ion' intact" $
            extractLocationSuffix "Arsenic, ion" `shouldBe` ("Arsenic, ion", Nothing)

    describe "region base-name fallback in the score lookup" $ do
        it "characterizes a region-suffixed flow via the base substance CF" $
            scoreOf "Ammonia" 11.5 "Ammonia, FR" `shouldBe` Just 11.5
        it "characterizes a regional-aggregate suffix too" $
            scoreOf "Ammonia" 11.5 "Ammonia, RER" `shouldBe` Just 11.5
        it "does NOT borrow the base CF for a chemical qualifier (', fossil')" $
            scoreOf "Methane" 28.0 "Methane, fossil" `shouldBe` Nothing
        it "still characterizes the base flow itself" $
            scoreOf "Ammonia" 11.5 "Ammonia" `shouldBe` Just 11.5
