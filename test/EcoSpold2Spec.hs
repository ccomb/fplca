{-# LANGUAGE OverloadedStrings #-}

module EcoSpold2Spec (spec) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import EcoSpold.Parser2 (streamParseActivityAndFlowsFromFile)
import Types

{- | The bundled fixture has a `<comment xml:lang="en">...</comment>` on each
of its four exchanges (1 input, 1 reference output, 2 emissions).

streamParseActivityAndFlowsFromFile derives a synthetic ProcessId from the
filename and rejects names that don't match `actUUID_prodUUID`, so we copy
the fixture into a temp path with that shape.
-}
withFixture :: ((Activity, [TechnosphereFlow], [BiosphereFlow], [WasteFlow], [Unit]) -> IO ()) -> IO ()
withFixture k = withSystemTempDirectory "es2-spec" $ \dir -> do
    bytes <- BS.readFile "test-data/electricity-production.spold"
    let path = dir </> "12345678-1234-5678-9abc-123456789001_12345678-1234-5678-9abc-123456789002.spold"
    BS.writeFile path bytes
    result <- streamParseActivityAndFlowsFromFile path
    case result of
        Left err -> expectationFailure $ "Parse failed: " ++ err
        Right res -> k res

spec :: Spec
spec = describe "per-exchange comments" $ do
    it "captures English <comment> on intermediateExchange and elementaryExchange" $
        withFixture $ \(act, _, _, _, _) ->
            map exchangeComment (exchanges act)
                `shouldMatchList` [ Just "Coal input for electricity generation"
                                  , Just "Electricity output (reference product)"
                                  , Just "CO2 emission from coal combustion"
                                  , Just "SO2 emission from coal combustion"
                                  ]

    it "preserves all four exchanges" $
        withFixture $
            \(act, _, _, _, _) -> length (exchanges act) `shouldBe` 4

    it "comments contain no &-entity artefacts" $
        withFixture $ \(act, _, _, _, _) ->
            let comments = [c | ex <- exchanges act, Just c <- [exchangeComment ex]]
             in all (not . T.isInfixOf "&") comments `shouldBe` True

    -- Critical correctness test: <property> children of an exchange may carry
    -- their own <comment> describing the property (e.g. "dry mass on a kg
    -- basis"). Those must NOT be attributed to the exchange itself.
    it "ignores <comment> nested inside <property> children of an exchange" $ do
        result <- streamParseActivityAndFlowsFromFile "test-data/sawnwood-properties_12345678-1234-5678-9abc-12345678aaaa.spold"
        case result of
            Left err -> expectationFailure $ "Parse failed: " ++ err
            Right (act, _, _, _, _) ->
                map exchangeComment (exchanges act)
                    `shouldMatchList` [ Nothing -- ex1 has no top-level comment, only property comments
                                      , Just "Adhesive applied during pressing" -- ex2's exchange-level comment, NOT the noisy property comment
                                      ]

    -- Pattern A: elementaryExchange with compartment=inventory indicator
    -- subcompartment=waste must surface as a WasteExchange / WasteFlow,
    -- not a BiosphereExchange. Pattern B: intermediateExchange with
    -- classification (By-product classification=Waste) likewise.
    describe "waste flow detection" $ do
        it "routes Pattern A (elementary 'inventory indicator/waste') to WasteExchange" $
            withWastePatternsFixture $ \(act, _, bios, wastes, _) -> do
                let waste = [e | e@WasteExchange{} <- exchanges act]
                length waste `shouldBe` 2 -- one Pattern A + one Pattern B
                length wastes `shouldBe` 2 -- WasteFlow registry populated
                length bios `shouldBe` 1 -- the genuine CO2 emission stays biosphere
        it "routes Pattern B (intermediate classification 'By-product:Waste') to WasteExchange" $
            withWastePatternsFixture $ \(act, _, _, _, _) -> do
                let wasteInputs = [e | e@WasteExchange{waIsInput = True} <- exchanges act]
                    wasteOutputs = [e | e@WasteExchange{waIsInput = False} <- exchanges act]
                -- Pattern B sample is an input; Pattern A sample is an output.
                length wasteInputs `shouldBe` 1
                length wasteOutputs `shouldBe` 1

    -- -----------------------------------------------------------------------
    -- Robustness: malformed input should never crash; we expect a clean Left.
    -- The byte-level fuzzing inputs are the ones the parser actually sees in
    -- the wild when an upload is truncated or a file is mis-extended.
    -- -----------------------------------------------------------------------
    describe "malformed input — returns Left without crashing" $ do
        let runOnBytes bytes = withSystemTempDirectory "es2-bad" $ \dir -> do
                let path = dir </> "12345678-1234-5678-9abc-123456789001_12345678-1234-5678-9abc-123456789002.spold"
                BS.writeFile path bytes
                streamParseActivityAndFlowsFromFile path
        let shouldBeLeft res = case res of
                Left _ -> pure ()
                Right _ -> expectationFailure "expected a Left for malformed input"

        it "returns Left on an empty file" $
            runOnBytes "" >>= shouldBeLeft

        it "returns Left on a stray non-XML byte sequence" $
            runOnBytes "this is not xml at all" >>= shouldBeLeft

        it "returns Left on truncated XML (unclosed tag)" $
            runOnBytes "<ecoSpold xmlns=\"http://www.EcoInvent.org/EcoSpold02\"><activityDataset>" >>= shouldBeLeft

        it "returns Left on well-formed XML that is not an EcoSpold dataset" $
            runOnBytes "<?xml version=\"1.0\"?><root><child>hello</child></root>" >>= shouldBeLeft

withWastePatternsFixture :: ((Activity, [TechnosphereFlow], [BiosphereFlow], [WasteFlow], [Unit]) -> IO ()) -> IO ()
withWastePatternsFixture k = withSystemTempDirectory "es2-waste-spec" $ \dir -> do
    let path = dir </> "12345678-1234-5678-9abc-123456789001_12345678-1234-5678-9abc-123456789002.spold"
    BS.writeFile path wastePatternsXml
    result <- streamParseActivityAndFlowsFromFile path
    case result of
        Left err -> expectationFailure $ "Parse failed: " ++ err
        Right res -> k res

{- | Synthetic fixture exercising both EcoSpold2 waste patterns:
  - Pattern A: elementary exchange with compartment "inventory indicator"
    / "waste" (waste output surfaced through the elementary axis)
  - Pattern B: intermediate exchange tagged via classification
    (System="By-product classification", Value="Waste")
Plus one genuine biosphere emission and the mandatory reference output.
-}
wastePatternsXml :: BS.ByteString
wastePatternsXml =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
    \<ecoSpold xmlns=\"http://www.EcoInvent.org/EcoSpold02\">\n\
    \  <activityDataset>\n\
    \    <activityDescription>\n\
    \      <activity id=\"aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa\" activityNameId=\"waste-test\">\n\
    \        <activityName xml:lang=\"en\">Waste patterns test activity</activityName>\n\
    \      </activity>\n\
    \      <geography geographyId=\"TEST\"><shortname xml:lang=\"en\">TEST</shortname></geography>\n\
    \    </activityDescription>\n\
    \    <flowData>\n\
    \      <!-- Reference output (kept as a real product so applyCutoffStrategy is happy) -->\n\
    \      <intermediateExchange id=\"ref\" unitId=\"unit-kg\" amount=\"1.0\"\n\
    \                           intermediateExchangeId=\"bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb\">\n\
    \        <name xml:lang=\"en\">Treated thing</name>\n\
    \        <unitName xml:lang=\"en\">kg</unitName>\n\
    \        <outputGroup>0</outputGroup>\n\
    \      </intermediateExchange>\n\
    \      <!-- Pattern B: intermediate exchange with By-product:Waste classification (treated as INPUT)-->\n\
    \      <intermediateExchange id=\"pat-b\" unitId=\"unit-kg\" amount=\"0.4\"\n\
    \                           intermediateExchangeId=\"cccccccc-cccc-cccc-cccc-cccccccccccc\">\n\
    \        <name xml:lang=\"en\">Spent solvent for treatment</name>\n\
    \        <unitName xml:lang=\"en\">kg</unitName>\n\
    \        <inputGroup>5</inputGroup>\n\
    \        <classification classificationId=\"bp-waste\">\n\
    \          <classificationSystem xml:lang=\"en\">By-product classification</classificationSystem>\n\
    \          <classificationValue xml:lang=\"en\">Waste</classificationValue>\n\
    \        </classification>\n\
    \      </intermediateExchange>\n\
    \      <!-- Pattern A: elementary exchange with compartment=inventory indicator / waste -->\n\
    \      <elementaryExchange id=\"pat-a\" unitId=\"unit-kg\" amount=\"0.2\"\n\
    \                         elementaryExchangeId=\"dddddddd-dddd-dddd-dddd-dddddddddddd\">\n\
    \        <name xml:lang=\"en\">Hazardous waste, to inventory</name>\n\
    \        <unitName xml:lang=\"en\">kg</unitName>\n\
    \        <compartment>\n\
    \          <compartment xml:lang=\"en\">inventory indicator</compartment>\n\
    \          <subcompartment xml:lang=\"en\">waste</subcompartment>\n\
    \        </compartment>\n\
    \        <outputGroup>4</outputGroup>\n\
    \      </elementaryExchange>\n\
    \      <!-- Genuine biosphere emission for contrast -->\n\
    \      <elementaryExchange id=\"bio\" unitId=\"unit-kg\" amount=\"0.1\"\n\
    \                         elementaryExchangeId=\"eeeeeeee-eeee-eeee-eeee-eeeeeeeeeeee\">\n\
    \        <name xml:lang=\"en\">Carbon dioxide</name>\n\
    \        <unitName xml:lang=\"en\">kg</unitName>\n\
    \        <compartment>\n\
    \          <compartment xml:lang=\"en\">air</compartment>\n\
    \          <subcompartment xml:lang=\"en\">unspecified</subcompartment>\n\
    \        </compartment>\n\
    \        <outputGroup>4</outputGroup>\n\
    \      </elementaryExchange>\n\
    \    </flowData>\n\
    \  </activityDataset>\n\
    \</ecoSpold>\n"
