{-# LANGUAGE OverloadedStrings #-}

module EnergyResourceFillSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Test.Hspec

import Method.Mapping (MatchStrategy (..), MethodTables, buildMethodTables, lookupCFForFlow)
import Method.Types (Compartment (..), EnergyDensity (..), EnergyDensityMap, FlowDirection (..), MethodCF (..), parseEnergyDensitySuffix)
import SynonymDB (normalizeName)
import Types (BiosphereFlow (..))
import qualified Types as VT

mkUUID :: Integer -> UUID
mkUUID n = UUID.fromWords64 (fromIntegral n) 0

-- A resource CF (the generic per-MJ fossil-resource-use factor).
resourceCF :: Text -> Double -> MethodCF
resourceCF name val =
    MethodCF
        { mcfFlowRef = mkUUID 1
        , mcfFlowName = name
        , mcfDirection = Input
        , mcfValue = val
        , mcfCompartment = Just (Compartment "resource" "" "")
        , mcfCAS = Nothing
        , mcfUnit = "MJ"
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
        , bfCompartment = Just (VT.Compartment "resource" Nothing)
        }

-- Tables where "Coal, hard" carries the generic resource CF (1 per MJ), and the
-- engine knows coal is an energy resource (an energy_density entry).
coalTables :: EnergyDensityMap -> MethodTables
coalTables eds =
    buildMethodTables "" M.empty eds [(resourceCF "Coal, hard" 1.0, Just (mkFlow 1 "Coal, hard", ByName))]

coalDensity :: EnergyDensityMap
coalDensity = M.singleton (normalizeName "Coal, hard") (EnergyDensity 18.01 "MJ" "kg")

-- Two coal-family resources with DISAGREEING generic CFs, both known to the
-- engine: the family factor is ambiguous, so the fallback must not guess.
disagreeingCoalTables :: MethodTables
disagreeingCoalTables =
    buildMethodTables
        ""
        M.empty
        ( M.fromList
            [ (normalizeName "Coal, hard", EnergyDensity 18 "MJ" "kg")
            , (normalizeName "Coal, brown", EnergyDensity 8 "MJ" "kg")
            ]
        )
        [ (resourceCF "Coal, hard" 1.0, Just (mkFlow 1 "Coal, hard", ByName))
        , (resourceCF "Coal, brown" 2.0, Just (mkFlow 2 "Coal, brown", ByName))
        ]

-- Borrowed raw CF (the density is applied later by convertAndMultiply).
borrowFor :: EnergyDensityMap -> Text -> Maybe Double
borrowFor eds flowName =
    fmap fst (lookupCFForFlow (coalTables eds) (mkUUID 99) (Just (mkFlow 99 flowName)))

spec :: Spec
spec = do
    describe "parseEnergyDensitySuffix" $ do
        it "parses a coal energy density (MJ per kg)" $
            parseEnergyDensitySuffix "Coal, 18 MJ per kg" `shouldBe` Just ("Coal", EnergyDensity 18 "MJ" "kg")
        it "keeps an internal qualifier in the base" $
            parseEnergyDensitySuffix "Gas, natural, 35 MJ per m3" `shouldBe` Just ("Gas, natural", EnergyDensity 35 "MJ" "m3")
        it "parses a GJ density verbatim (unit conversion is downstream)" $
            parseEnergyDensitySuffix "Uranium, 2291 GJ per kg" `shouldBe` Just ("Uranium", EnergyDensity 2291 "GJ" "kg")
        it "ignores a non-joule 'per' phrase" $
            parseEnergyDensitySuffix "Water, per capita" `shouldBe` Nothing
        it "ignores a name with no 'per'" $
            parseEnergyDensitySuffix "Methane, fossil" `shouldBe` Nothing

    describe "energy-resource CF fallback in the score lookup" $ do
        it "borrows the resource-family CF for an energy-density variant" $
            borrowFor coalDensity "Coal, 18 MJ per kg" `shouldBe` Just 1.0
        it "borrows it for the higher-energy variant too (density differs, CF is the family's)" $
            borrowFor coalDensity "Coal, 29.3 MJ per kg" `shouldBe` Just 1.0
        it "does NOT borrow when the resource family is unknown to the engine" $
            borrowFor M.empty "Coal, 18 MJ per kg" `shouldBe` Nothing
        it "does NOT borrow when same-family CFs disagree (ambiguous, never guesses)" $
            fmap fst (lookupCFForFlow disagreeingCoalTables (mkUUID 99) (Just (mkFlow 99 "Coal, 18 MJ per kg")))
                `shouldBe` Nothing
        it "does NOT fill a non-energy name" $
            borrowFor coalDensity "Water, per capita" `shouldBe` Nothing
