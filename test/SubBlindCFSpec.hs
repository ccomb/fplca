{-# LANGUAGE OverloadedStrings #-}

module SubBlindCFSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Test.Hspec

import Method.Mapping (MatchStrategy (..), buildMethodTables, lookupCFForFlow)
import Method.Types (Compartment (..), FlowDirection (..), MethodCF (..))
import Types (BiosphereFlow (..))
import qualified Types as VT

mkUUID :: Integer -> UUID
mkUUID n = UUID.fromWords64 (fromIntegral n) 0

-- A resource CF at a specific subcompartment.
mkCF :: Integer -> Text -> Text -> Double -> MethodCF
mkCF i name sub val =
    MethodCF
        { mcfFlowRef = mkUUID i
        , mcfFlowName = name
        , mcfDirection = Input
        , mcfValue = val
        , mcfCompartment = Just (Compartment "resource" sub "")
        , mcfCAS = Nothing
        , mcfUnit = "kg"
        , mcfConsumerLocation = Nothing
        }

mkFlow :: Integer -> Text -> Maybe Text -> BiosphereFlow
mkFlow i name sub =
    BiosphereFlow
        { bfId = mkUUID i
        , bfName = name
        , bfUnitId = mkUUID 0
        , bfSynonyms = M.empty
        , bfCAS = Nothing
        , bfSubstanceId = Nothing
        , bfCompartment = Just (VT.Compartment "resource" sub)
        }

score :: [(MethodCF, Maybe (BiosphereFlow, MatchStrategy))] -> BiosphereFlow -> Maybe Double
score mappings flow =
    fmap fst (lookupCFForFlow (buildMethodTables M.empty M.empty mappings) (bfId flow) (Just flow))

spec :: Spec
spec = describe "sub-blind CF fallback" $ do
    it "borrows a sub-specific CF for an unspecified-sub flow when the factor is sub-independent" $ do
        -- "Cadmium, in ground" = 0.157; the method has no unspecified entry.
        let mappings = [(mkCF 1 "Cadmium" "in ground" 0.157, Just (mkFlow 1 "Cadmium" (Just "in ground"), ByName))]
        score mappings (mkFlow 99 "Cadmium" Nothing) `shouldBe` Just 0.157

    it "still resolves the sub-specific flow itself" $ do
        let mappings = [(mkCF 1 "Cadmium" "in ground" 0.157, Just (mkFlow 1 "Cadmium" (Just "in ground"), ByName))]
        score mappings (mkFlow 1 "Cadmium" (Just "in ground")) `shouldBe` Just 0.157

    it "does NOT guess when the factor varies by subcompartment (ambiguous)" $ do
        -- Mercury differs by sub: in ground 1.0, in water 2.0 — no safe default.
        let mappings =
                [ (mkCF 1 "Mercury" "in ground" 1.0, Just (mkFlow 1 "Mercury" (Just "in ground"), ByName))
                , (mkCF 2 "Mercury" "in water" 2.0, Just (mkFlow 2 "Mercury" (Just "in water"), ByName))
                ]
        score mappings (mkFlow 99 "Mercury" Nothing) `shouldBe` Nothing
