{-# LANGUAGE OverloadedStrings #-}

module ProjectRegionalSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Test.Hspec

import Method.Mapping (MatchStrategy (..), projectRegionalResourceFlows)
import Method.Types (Compartment (..), FlowDirection (..), MethodCF (..))
import SynonymDB (buildFromPairs)
import Types (BiosphereFlow (..))
import qualified Types as VT

mkUUID :: Integer -> UUID
mkUUID n = UUID.fromWords64 (fromIntegral n) 0

-- | A withdrawal CF for a named resource flow at a consumer location.
mkLocatedCF :: Text -> Double -> Maybe Text -> MethodCF
mkLocatedCF name val loc =
    MethodCF
        { mcfFlowRef = mkUUID 1
        , mcfFlowName = name
        , mcfDirection = Input
        , mcfValue = val
        , mcfCompartment = Just (Compartment "natural resource" "" "")
        , mcfCAS = Nothing
        , mcfUnit = "m3"
        , mcfConsumerLocation = loc
        }

mkResourceFlow :: Integer -> Text -> BiosphereFlow
mkResourceFlow i name =
    BiosphereFlow
        { bfId = mkUUID i
        , bfName = name
        , bfUnitId = mkUUID 0
        , bfSynonyms = M.empty
        , bfCAS = Nothing
        , bfSubstanceId = Nothing
        , bfCompartment = Just (VT.Compartment "natural resource" Nothing)
        }

-- A projection nulls the CF's consumer location (it becomes a GLOBAL entry) and
-- re-targets it onto the region-tagged flow's own name. Since the result is
-- @mappings ++ projected@, a projected entry shows up as a @(flow name, Nothing,
-- value)@ triple carried by the region-tagged flow.
projected :: [(MethodCF, Maybe (BiosphereFlow, MatchStrategy))] -> [(Text, Maybe Text, Double)]
projected xs = [(bfName f, mcfConsumerLocation cf, mcfValue cf) | (cf, Just (f, BySynonym)) <- xs]

spec :: Spec
spec = describe "projectRegionalResourceFlows" $ do
    let synDB = buildFromPairs [("river water", "Water, river")]
        baseFlow = mkResourceFlow 10 "Water, river"
        cfFR = mkLocatedCF "river water" 6.98 (Just "FR")
        baseMappings = [(cfFR, Just (baseFlow, BySynonym))]
        runWith flows =
            projected (projectRegionalResourceFlows synDB (M.fromList [(bfId f, f) | f <- flows]) baseMappings)
        frProjection = ("Water, river, FR", Nothing, 6.98)

    it "projects a region-tagged resource flow onto its region's located CF, globally" $
        runWith [baseFlow, mkResourceFlow 11 "Water, river, FR"]
            `shouldContain` [frProjection]

    it "does not project a region that has no located CF" $
        runWith [baseFlow, mkResourceFlow 12 "Water, river, ZZ"]
            `shouldNotContain` [("Water, river, ZZ", Nothing, 6.98)]

    it "leaves an unlocated method untouched (the SimaPro name-regionalized convention)" $ do
        let cfGlobal = mkLocatedCF "river water" 6.98 Nothing
            frFlow = mkResourceFlow 11 "Water, river, FR"
        projected
            (projectRegionalResourceFlows synDB (M.fromList [(bfId frFlow, frFlow)]) [(cfGlobal, Just (baseFlow, BySynonym))])
            `shouldNotContain` [frProjection]

    it "projects a release CF onto a region-tagged water emission flow by its own name" $ do
        let releaseCF =
                (mkLocatedCF "Water" (-42.0) (Just "FR"))
                    { mcfCompartment = Just (Compartment "water" "" "")
                    }
            waterFR =
                (mkResourceFlow 20 "Water, FR")
                    { bfCompartment = Just (VT.Compartment "water" Nothing)
                    }
            bareWater =
                (mkResourceFlow 21 "Water")
                    { bfCompartment = Just (VT.Compartment "water" Nothing)
                    }
        projected
            ( projectRegionalResourceFlows
                synDB
                (M.fromList [(bfId waterFR, waterFR), (bfId bareWater, bareWater)])
                [(releaseCF, Just (bareWater, ByName))]
            )
            `shouldContain` [("Water, FR", Nothing, -42.0)]

    it "does not project an air emission flow, so air-regionalized methods stay global" $ do
        let airCF =
                (mkLocatedCF "Sulfur dioxide" 1.5 (Just "FR"))
                    { mcfCompartment = Just (Compartment "air" "" "")
                    }
            so2FR =
                (mkResourceFlow 22 "Sulfur dioxide, FR")
                    { bfCompartment = Just (VT.Compartment "air" Nothing)
                    }
            bareSo2 =
                (mkResourceFlow 23 "Sulfur dioxide")
                    { bfCompartment = Just (VT.Compartment "air" Nothing)
                    }
        projected
            ( projectRegionalResourceFlows
                synDB
                (M.fromList [(bfId so2FR, so2FR), (bfId bareSo2, bareSo2)])
                [(airCF, Just (bareSo2, ByName))]
            )
            `shouldNotContain` [("Sulfur dioxide, FR", Nothing, 1.5)]
