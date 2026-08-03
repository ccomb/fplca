{-# LANGUAGE OverloadedStrings #-}

{- | The explanation a flow's factor comes with.

Two things are pinned here. First the sentences: 'renderResolution' matches
exhaustively, so a new rung will not compile without wording, but only a golden
test catches wording that quietly stops matching what the engine does. Second
the trail: which rungs were tried, which were vetoed, and which refused to
guess, for the cases the cascade exists to get right.
-}
module ExplainCFSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Test.Hspec

import qualified API.Resources as R
import Method.Explain
import Method.Mapping (
    BuildProvenance (..),
    CF (..),
    CFUnit (..),
    DensityDirection (..),
    MatchStrategy (..),
    MethodTables,
    RefusalReason (..),
    RungId (..),
    UnitBridge (..),
    VetoReason (..),
    buildMethodTables,
    fillBroadcastVector,
 )
import Method.Types (CFFamily (..), Compartment (..), EnergyDensity (..), EnergyDensityMap, FlowDirection (..), MethodCF (..))
import SynonymDB (normalizeName)
import Types (BiosphereFlow (..), Unit (..), UnitDB)
import qualified Types as VT
import UnitConversion (UnitConfig (..), UnitDef (..), defaultUnitConfig)

mkUUID :: Integer -> UUID
mkUUID n = UUID.fromWords64 (fromIntegral n) 0

kgUnitId :: UUID
kgUnitId = mkUUID 900

unitDB :: UnitDB
unitDB = M.singleton kgUnitId (Unit kgUnitId "kg" "kg" "")

{- | kg and MJ both known, so a cross-dimension pair is a real mismatch rather
than an unknown unit.
-}
massEnergyConfig :: UnitConfig
massEnergyConfig =
    UnitConfig
        { ucDimensionOrder = ["mass", "energy"]
        , ucUnits =
            M.fromList
                [ ("kg", UnitDef [1, 0] 1.0)
                , ("mj", UnitDef [0, 1] 1.0)
                ]
        , ucOriginalKeys = M.fromList [("kg", "kg"), ("mj", "MJ")]
        }

cfLine :: Text -> Text -> Text -> Double -> MethodCF
cfLine name sub unit val =
    MethodCF
        { mcfFlowRef = mkUUID 1
        , mcfFlowName = name
        , mcfDirection = Output
        , mcfValue = val
        , mcfCompartment = Just (Compartment "air" sub "")
        , mcfCAS = Nothing
        , mcfUnit = unit
        , mcfConsumerLocation = Nothing
        }

resourceLine :: Text -> Text -> Double -> MethodCF
resourceLine name unit val =
    (cfLine name "" unit val){mcfCompartment = Just (Compartment "resource" "" "")}

waterLine :: Text -> Text -> Double -> MethodCF
waterLine name sub val =
    (cfLine name sub "kg" val){mcfCompartment = Just (Compartment "water" sub "")}

flowIn :: Integer -> Text -> Text -> Maybe Text -> BiosphereFlow
flowIn i name medium sub =
    BiosphereFlow
        { bfId = mkUUID i
        , bfName = name
        , bfUnitId = kgUnitId
        , bfSynonyms = M.empty
        , bfCAS = Nothing
        , bfSubstanceId = Nothing
        , bfCompartment = Just (VT.Compartment medium sub)
        }

explainOf :: UnitConfig -> MethodTables -> BiosphereFlow -> CFExplanation
explainOf cfg tables flow = explainFlowCF cfg unitDB tables (bfId flow) flow

-- | Tables plus the broadcast fill, the shape the read path actually serves.
tablesFor :: EnergyDensityMap -> [(MethodCF, Maybe (BiosphereFlow, MatchStrategy))] -> [BiosphereFlow] -> MethodTables
tablesFor densities mappings flows =
    fillBroadcastVector defaultUnitConfig unitDB (flowDBOf flows) $
        buildMethodTables OtherCFFamily M.empty densities mappings

flowDBOf :: [BiosphereFlow] -> M.Map UUID BiosphereFlow
flowDBOf flows = M.fromList [(bfId f, f) | f <- flows]

-- | Any match will do where only the outcome's name is under test.
sampleMatch :: CFMatch
sampleMatch = CFMatch RungExactName (CF 1 (CFUnit "kg")) (BuildProvenance ByName (cfLine "Methane, fossil" "" "kg" 1))

resultsFor :: CFExplanation -> [(RungId, StepResult)]
resultsFor e = [(stRung s, stResult s) | s <- ceTrail e]

spec :: Spec
spec = do
    describe "renderResolution (every outcome says what it means)" $ do
        let provenance strat = BuildProvenance strat (cfLine "Methane, fossil" "" "kg" 29.8)
            match rung strat = CFMatch rung (CF 29.8 (CFUnit "kg")) (provenance strat)
            matchIn unit rung strat = CFMatch rung (CF 29.8 (CFUnit unit)) (provenance strat)

        it "a direct name match reads as one" $
            renderResolution (Characterized (match RungExactName ByName) UnitsIdentical)
                `shouldBe` [ "The factor line \"Methane, fossil\" matches this flow's name and compartment."
                           , "The factor applied is 29.8 per kg."
                           ]

        it "names the synonym bridge that attached the line" $
            renderResolution (Characterized (match RungExactName BySynonym) UnitsIdentical)
                `shouldBe` [ "The factor line \"Methane, fossil\" matches this flow's name and compartment."
                           , "That line was tied to this flow's name through a known synonym when the method was loaded."
                           , "The factor applied is 29.8 per kg."
                           ]

        it "spells out the energy content that bridges the units" $
            renderResolution
                ( Characterized
                    (matchIn "MJ" RungEnergyResource ByName)
                    (EnergyBridged (EnergyDensity 18.0 "MJ" "kg") DensityForward)
                )
                `shouldBe` [ "No factor carries this flow's name. The flow is an energy resource, so its family's factor per unit of energy, from \"Methane, fossil\", applies."
                           , "This flow holds 18.0 MJ per kg, which carries the amount from kg to MJ."
                           , "The factor applied is 29.8 per MJ."
                           ]

        -- A method loaded from ILCD writes its factor in the result expression
        -- ("kg CO2 eq"): the factor yields that much per base unit, so the
        -- sentence must not read "per kg CO2 eq" — that is the factor backwards.
        it "states a result-expression factor on the flow's base unit" $
            renderResolution (Characterized (matchIn "kg CO2 eq" RungExactName ByName) (NormalizedToBase "kg"))
                `shouldBe` [ "The factor line \"Methane, fossil\" matches this flow's name and compartment."
                           , "The factor is written per kg, so the amount was brought to kg first."
                           , "The factor applied is 29.8 kg CO2 eq per kg."
                           ]

        it "says why a refused conversion scores nothing" $
            renderResolution (ConversionRefused (match RungCasBridge ByCAS) (DimensionalMismatch "kg" "m3"))
                `shouldBe` [ "No factor carries this flow's name. \"Methane, fossil\" describes the same substance in the same compartment, so its factor applies."
                           , "That line was tied to this flow by CAS number when the method was loaded."
                           , "The factor is written per m3, which does not measure the same thing as this flow's kg. The engine refuses to convert between them, so the flow adds nothing to the score."
                           ]

        it "says plainly when nothing reaches the flow" $
            renderResolution Uncharacterized
                `shouldBe` ["No factor in this method reaches this flow, so it adds nothing to the score."]

        it "gives every rung a sentence of its own" $ do
            let sentences =
                    [ renderResolution (Characterized (match rung ByName) UnitsIdentical)
                    | rung <- [minBound .. maxBound]
                    ]
            length sentences `shouldBe` length [minBound .. maxBound :: RungId]
            any (any T.null) sentences `shouldBe` False

    describe "rung names (what a client reads)" $ do
        it "gives every rung a distinct name" $ do
            let names = map rungName [minBound .. maxBound]
            length names `shouldBe` length (foldr (\n acc -> if n `elem` acc then acc else n : acc) [] names)

        -- A client, human or agent, reads these names with only the tool
        -- description to go on. An exhaustive match forces a new rung to have a
        -- sentence; nothing forces it to be documented, so this does.
        it "documents every rung name in the tool description" $ do
            let told = R.description R.ExplainCF
                undocumented = [n | n <- map rungName [minBound .. maxBound], not (n `T.isInfixOf` told)]
            undocumented `shouldBe` []

        it "documents every outcome in the tool description" $ do
            let told = R.description R.ExplainCF
                outcomes =
                    map
                        outcomeName
                        [ Characterized sampleMatch UnitsIdentical
                        , ConversionRefused sampleMatch (NoCanonicalBase "kg")
                        , Uncharacterized
                        ]
                undocumented = [o | o <- outcomes, not (o `T.isInfixOf` told)]
            undocumented `shouldBe` []

    describe "explainFlowCF (replaying the cascade)" $ do
        it "reports the rung that answered and stops there" $ do
            let flow = flowIn 1 "Methane, fossil" "air" Nothing
                tables = tablesFor M.empty [(cfLine "Methane, fossil" "" "kg" 29.8, Just (flow, ByUUID))] [flow]
                explained = explainOf defaultUnitConfig tables flow
            case ceResolution explained of
                Characterized m _ -> cmRung m `shouldBe` RungUuid
                other -> expectationFailure ("expected a characterized flow, got " <> show other)
            -- The UUID rung answers first, so nothing below it was tried.
            resultsFor explained `shouldBe` [(RungUuid, StepHit)]

        it "walks past the rungs that missed before the one that answered" $ do
            -- A name-matched factor with no subcompartment lands in the
            -- compartment-level table, so the trail must show the rungs above
            -- it being tried and missing.
            let flow = flowIn 11 "Methane, fossil" "air" Nothing
                tables = tablesFor M.empty [(cfLine "Methane, fossil" "" "kg" 29.8, Just (flow, ByName))] [flow]
                explained = explainOf defaultUnitConfig tables flow
                results = resultsFor explained
            case ceResolution explained of
                Characterized m _ -> cmRung m `shouldBe` RungMediumDefault
                other -> expectationFailure ("expected a characterized flow, got " <> show other)
            lookup RungUuid results `shouldBe` Just StepMiss
            lookup RungExactName results `shouldBe` Just StepMiss
            lookup RungMediumDefault results `shouldBe` Just StepHit
            -- Nothing below the rung that answered was tried.
            lookup RungCasBridge results `shouldBe` Nothing

        it "records the sea-water veto on every wildcard rung it blocks" $ do
            -- A method that names the sea somewhere meant to leave this
            -- emission out, so its freshwater factor must not reach the ocean,
            -- and the trail must say the veto is what stopped it.
            let ocean = flowIn 2 "Water" "water" (Just "ocean")
                fresh = flowIn 3 "Water" "water" Nothing
                tables =
                    tablesFor
                        M.empty
                        [ (waterLine "Water" "" 1.0, Just (fresh, ByName))
                        , (waterLine "Sea water" "ocean" 0.0, Nothing)
                        ]
                        [ocean, fresh]
                explained = explainOf defaultUnitConfig tables ocean
                vetoed = [rung | (rung, StepVetoed ForeignMediumVeto _) <- resultsFor explained]
            ceResolution explained `shouldBe` Uncharacterized
            vetoed `shouldContain` [RungMediumDefault]
            vetoed `shouldContain` [RungSubBlind]

        it "lets a method that never names the sea characterize an ocean flow" $ do
            -- The mirror case: silence about sea water is not an exclusion, so
            -- the freshwater default applies and no veto appears in the trail.
            let ocean = flowIn 12 "Water" "water" (Just "ocean")
                fresh = flowIn 13 "Water" "water" Nothing
                tables = tablesFor M.empty [(waterLine "Water" "" 1.0, Just (fresh, ByName))] [ocean, fresh]
                explained = explainOf defaultUnitConfig tables ocean
            [rung | (rung, StepVetoed ForeignMediumVeto _) <- resultsFor explained] `shouldBe` []
            case ceResolution explained of
                Characterized m _ -> cmRung m `shouldBe` RungMediumDefault
                other -> expectationFailure ("expected the freshwater factor to apply, got " <> show other)

        it "marks a rung the flow cannot use as not applicable" $ do
            -- No CAS on the flow, so the CAS bridge is not a miss: it never ran.
            let flow = flowIn 4 "Nitrous oxide" "air" Nothing
                tables = tablesFor M.empty [(cfLine "Something else" "" "kg" 1.0, Nothing)] [flow]
                explained = explainOf defaultUnitConfig tables flow
            lookup RungCasBridge (resultsFor explained) `shouldBe` Just StepNotApplicable

        it "refuses to guess between disagreeing energy-family factors" $ do
            -- Two coal factors that disagree: the family factor is ambiguous,
            -- and the trail says so rather than reporting an ordinary miss.
            let coal = flowIn 5 "Coal, 18 MJ per kg" "resource" Nothing
                hard = flowIn 6 "Coal, hard" "resource" Nothing
                brown = flowIn 7 "Coal, brown" "resource" Nothing
                densities =
                    M.fromList
                        [ (normalizeName "Coal, hard", EnergyDensity 18.0 "MJ" "kg")
                        , (normalizeName "Coal, brown", EnergyDensity 9.0 "MJ" "kg")
                        ]
                tables =
                    tablesFor
                        densities
                        [ (resourceLine "Coal, hard" "MJ" 1.0, Just (hard, ByName))
                        , (resourceLine "Coal, brown" "MJ" 2.0, Just (brown, ByName))
                        ]
                        [coal, hard, brown]
                explained = explainOf defaultUnitConfig tables coal
            lookup RungEnergyResource (resultsFor explained) `shouldBe` Just StepAmbiguous
            ceResolution explained `shouldBe` Uncharacterized

        it "explains a factor borrowed through the flow's energy content" $ do
            let coal = flowIn 8 "Coal, 18 MJ per kg" "resource" Nothing
                hard = flowIn 9 "Coal, hard" "resource" Nothing
                densities = M.singleton (normalizeName "Coal, 18 MJ per kg") (EnergyDensity 18.0 "MJ" "kg")
                tables =
                    tablesFor
                        (M.insert (normalizeName "Coal, hard") (EnergyDensity 18.0 "MJ" "kg") densities)
                        [(resourceLine "Coal, hard" "MJ" 0.5, Just (hard, ByName))]
                        [coal, hard]
                explained = explainOf massEnergyConfig tables coal
            case ceResolution explained of
                Characterized m (EnergyBridged density DensityForward) -> do
                    cmRung m `shouldBe` RungEnergyResource
                    edValue density `shouldBe` 18.0
                other -> expectationFailure ("expected an energy-bridged factor, got " <> show other)

        it "reports a factor whose unit the flow cannot reach as refused, not missing" $ do
            -- A per-m3 factor against a kg flow with no density to bridge them:
            -- the flow looks characterized and scores nothing, and the
            -- explanation is what says so.
            let flow = flowIn 10 "Water" "resource" Nothing
                tables = tablesFor M.empty [(resourceLine "Water" "m3" 42.95, Just (flow, ByName))] [flow]
                explained = explainOf volumeMassConfig tables flow
            case ceResolution explained of
                ConversionRefused _ (DimensionalMismatch "kg" "m3") -> pure ()
                other -> expectationFailure ("expected a refused conversion, got " <> show other)
  where
    -- kg and m3 known but dimensionally apart, so the pair is a mismatch.
    volumeMassConfig =
        UnitConfig
            { ucDimensionOrder = ["mass", "volume"]
            , ucUnits = M.fromList [("kg", UnitDef [1, 0] 1.0), ("m3", UnitDef [0, 1] 1.0)]
            , ucOriginalKeys = M.fromList [("kg", "kg"), ("m3", "m3")]
            }
