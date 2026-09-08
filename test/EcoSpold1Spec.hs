{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module EcoSpold1Spec (spec) where

import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Test.Hspec

import EcoSpold.Parser1
import Types

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Minimal valid EcoSpold1 XML with one reference product and one air emission
minimalXml :: BC.ByteString
minimalXml =
    BC.unlines
        [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        , "<ecoSpold xmlns=\"http://www.EcoInvent.org/EcoSpold01\">"
        , "  <dataset number=\"42\">"
        , "    <metaInformation>"
        , "      <processInformation>"
        , "        <referenceFunction name=\"electricity production\" category=\"Energy\""
        , "                           subCategory=\"Electricity\" unit=\"kWh\""
        , "                           generalComment=\"A comment\"/>"
        , "        <geography location=\"DE\" />"
        , "      </processInformation>"
        , "    </metaInformation>"
        , "    <flowData>"
        , "      <exchange number=\"1\" name=\"electricity, high voltage\" category=\"Energy\""
        , "                subCategory=\"Electricity\" unit=\"kWh\" meanValue=\"1.0\">"
        , "        <outputGroup>0</outputGroup>"
        , "      </exchange>"
        , "      <exchange number=\"2\" name=\"Carbon dioxide, fossil\" category=\"air\""
        , "                subCategory=\"low population density\" unit=\"kg\" meanValue=\"0.05\""
        , "                CASNumber=\"124-38-9\">"
        , "        <outputGroup>4</outputGroup>"
        , "      </exchange>"
        , "      <exchange number=\"3\" name=\"natural gas\" category=\"resource\""
        , "                subCategory=\"in ground\" unit=\"MJ\" meanValue=\"10.0\">"
        , "        <inputGroup>4</inputGroup>"
        , "      </exchange>"
        , "      <exchange number=\"4\" name=\"fuel oil\" category=\"Liquid fuels\""
        , "                unit=\"kg\" meanValue=\"2.0\">"
        , "        <inputGroup>5</inputGroup>"
        , "      </exchange>"
        , "    </flowData>"
        , "  </dataset>"
        , "</ecoSpold>"
        ]

-- | 'minimalXml' with the @number@ attribute taken off its dataset element.
unnumberedXml :: BC.ByteString
unnumberedXml = BC.unlines (map withoutNumber (BC.lines minimalXml))
  where
    withoutNumber :: BC.ByteString -> BC.ByteString
    withoutNumber line
        | "  <dataset number=" `BC.isPrefixOf` line = "  <dataset>"
        | otherwise = line

{- | The same two elementary flows as 'minimalXml', in another dataset written
by another author. The @<person number>@ sits where EcoSpold1 metadata really
carries one: under @<dataset>@, after the dataset's own number. The reference
product repeats the dataset's number, which is what every export observed does
and what lets a consumer name this dataset as its supplier.
-}
otherAuthorXml :: BC.ByteString
otherAuthorXml =
    BC.unlines
        [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        , "<ecoSpold xmlns=\"http://www.EcoInvent.org/EcoSpold01\">"
        , "  <dataset number=\"43\">"
        , "    <metaInformation>"
        , "      <processInformation>"
        , "        <referenceFunction name=\"heat production\" category=\"Energy\""
        , "                           subCategory=\"Heat\" unit=\"MJ\"/>"
        , "        <geography location=\"FR\" />"
        , "      </processInformation>"
        , "      <administrativeInformation>"
        , "        <dataGeneratorAndPublication person=\"777\" />"
        , "        <person number=\"777\" name=\"Doe\" />"
        , "      </administrativeInformation>"
        , "    </metaInformation>"
        , "    <flowData>"
        , "      <exchange number=\"43\" name=\"heat, district network\" category=\"Energy\""
        , "                subCategory=\"Heat\" unit=\"MJ\" meanValue=\"1.0\">"
        , "        <outputGroup>0</outputGroup>"
        , "      </exchange>"
        , "      <exchange number=\"2\" name=\"Carbon dioxide, fossil\" category=\"air\""
        , "                subCategory=\"low population density\" unit=\"kg\" meanValue=\"0.08\""
        , "                CASNumber=\"124-38-9\">"
        , "        <outputGroup>4</outputGroup>"
        , "      </exchange>"
        , "      <exchange number=\"3\" name=\"natural gas\" category=\"resource\""
        , "                subCategory=\"in ground\" unit=\"MJ\" meanValue=\"14.0\">"
        , "        <inputGroup>4</inputGroup>"
        , "      </exchange>"
        , "    </flowData>"
        , "  </dataset>"
        , "</ecoSpold>"
        ]

{- | Fixture exercising per-exchange `generalComment` attribute.
Exchange #1 (reference) carries a comment, #2 has none. The
referenceFunction also carries an activity-level generalComment to
regression-check that the two paths don't cross-contaminate.
-}
commentXml :: BC.ByteString
commentXml =
    BC.unlines
        [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        , "<ecoSpold xmlns=\"http://www.EcoInvent.org/EcoSpold01\">"
        , "  <dataset number=\"99\">"
        , "    <metaInformation>"
        , "      <processInformation>"
        , "        <referenceFunction name=\"steel mill\" category=\"metals\""
        , "                           subCategory=\"steel\" unit=\"kg\""
        , "                           generalComment=\"Process-level note\"/>"
        , "        <geography location=\"GLO\" />"
        , "      </processInformation>"
        , "    </metaInformation>"
        , "    <flowData>"
        , "      <exchange number=\"1\" name=\"steel\" category=\"metals\" unit=\"kg\""
        , "                meanValue=\"1.0\" generalComment=\"Global&#10;\">"
        , "        <outputGroup>0</outputGroup>"
        , "      </exchange>"
        , "      <exchange number=\"2\" name=\"iron ore\" category=\"resource\""
        , "                subCategory=\"in ground\" unit=\"kg\" meanValue=\"1.5\">"
        , "        <inputGroup>4</inputGroup>"
        , "      </exchange>"
        , "    </flowData>"
        , "  </dataset>"
        , "</ecoSpold>"
        ]

-- | Multi-dataset EcoSpold1 XML
multiDatasetXml :: BC.ByteString
multiDatasetXml =
    BC.unlines
        [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        , "<ecoSpold xmlns=\"http://www.EcoInvent.org/EcoSpold01\">"
        , "  <dataset number=\"1\">"
        , "    <metaInformation>"
        , "      <processInformation>"
        , "        <referenceFunction name=\"process A\" unit=\"kg\"/>"
        , "        <geography location=\"CH\" />"
        , "      </processInformation>"
        , "      <administrativeInformation>"
        , "        <person number=\"777\" name=\"Doe\" />"
        , "      </administrativeInformation>"
        , "    </metaInformation>"
        , "    <flowData>"
        , "      <exchange number=\"1\" name=\"product A\" category=\"goods\" unit=\"kg\" meanValue=\"1.0\">"
        , "        <outputGroup>0</outputGroup>"
        , "      </exchange>"
        , "    </flowData>"
        , "  </dataset>"
        , "  <dataset number=\"2\">"
        , "    <metaInformation>"
        , "      <processInformation>"
        , "        <referenceFunction name=\"process B\" unit=\"kg\"/>"
        , "        <geography location=\"FR\" />"
        , "      </processInformation>"
        , "    </metaInformation>"
        , "    <flowData>"
        , "      <exchange number=\"1\" name=\"product B\" category=\"goods\" unit=\"kg\" meanValue=\"1.0\">"
        , "        <outputGroup>0</outputGroup>"
        , "      </exchange>"
        , "    </flowData>"
        , "  </dataset>"
        , "</ecoSpold>"
        ]

{- | Fixture exercising the 'Final waste flows' export shape. Waste with no
modelled treatment lands on inputGroup=5, the export's way of fitting a fifth
flow class into a four-type model, with the category attribute preserved.
Exchange #1 is the reference product, #2 is such a row: nothing treats it, so
the parser must read it as an elementary flow rather than as a demand on a
supplier that cannot exist.
-}
wasteFlowXml :: BC.ByteString
wasteFlowXml =
    BC.unlines
        [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        , "<ecoSpold xmlns=\"http://www.EcoInvent.org/EcoSpold01\">"
        , "  <dataset number=\"7\">"
        , "    <metaInformation>"
        , "      <processInformation>"
        , "        <referenceFunction name=\"aluminium ingot\" category=\"metals\""
        , "                           subCategory=\"primary\" unit=\"kg\"/>"
        , "        <geography location=\"RoW\" />"
        , "      </processInformation>"
        , "    </metaInformation>"
        , "    <flowData>"
        , "      <exchange number=\"1\" name=\"aluminium ingot\" category=\"metals\""
        , "                subCategory=\"primary\" unit=\"kg\" meanValue=\"1.0\">"
        , "        <outputGroup>0</outputGroup>"
        , "      </exchange>"
        , "      <exchange number=\"2\" name=\"Organic carbon, placed in landfill\""
        , "                category=\"Final waste flows\" subCategory=\"landfill\""
        , "                unit=\"kg\" meanValue=\"0.02\">"
        , "        <inputGroup>5</inputGroup>"
        , "      </exchange>"
        , "    </flowData>"
        , "  </dataset>"
        , "</ecoSpold>"
        ]

{- | Fixture carrying the provenance blocks a real export writes: two numbered
sources of which only the second is the one the dataset was published in, a
numbered person who proof-read it, and the free texts of the process
information. Shaped after the ESU/BAFU and ecoinvent 2.x exports.
-}
documentedXml :: BC.ByteString
documentedXml =
    BC.unlines
        [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        , "<ecoSpold xmlns=\"http://www.EcoInvent.org/EcoSpold01\">"
        , "  <dataset number=\"9\">"
        , "    <metaInformation>"
        , "      <processInformation>"
        , "        <referenceFunction name=\"natural gas, liquefied\" category=\"natural gas\""
        , "                           subCategory=\"production\" unit=\"Nm3\""
        , "                           includedProcesses=\"Transport on a freight ship.\"/>"
        , "        <geography location=\"KW\" text=\"not known\"/>"
        , "        <technology text=\"Distances based on port distances.\"/>"
        , "        <timePeriod text=\"Transport modes investigated for 2023.\">"
        , "          <startYear>2023</startYear>"
        , "          <endYear>2024</endYear>"
        , "        </timePeriod>"
        , "      </processInformation>"
        , "      <modellingAndValidation>"
        , "        <representativeness samplingProcedure=\"Literature.\" extrapolations=\"none\""
        , "                            productionVolume=\"not known\"/>"
        , "        <source number=\"1\" firstAuthor=\"Frischknecht R.\" year=\"2007\""
        , "                title=\"Overview and Methodology\""
        , "                titleOfAnthology=\"ecoinvent report No. 1\""
        , "                publisher=\"Swiss Centre for LCI\" placeOfPublications=\"Duebendorf, CH\"/>"
        , "        <source number=\"2\" firstAuthor=\"Bussa M.\" additionalAuthors=\"Jungbluth N.\""
        , "                year=\"2025\" title=\"LCI long-distance transport of natural gas\""
        , "                publisher=\"ESU-services Ltd.\" placeOfPublications=\"Schaffhausen, CH\"/>"
        , "        <validation proofReadingDetails=\"Passed.\" proofReadingValidator=\"41\"/>"
        , "      </modellingAndValidation>"
        , "      <administrativeInformation>"
        , "        <dataGeneratorAndPublication person=\"41\" referenceToPublishedSource=\"2\"/>"
        , "        <person number=\"41\" name=\"Niels Jungbluth\" companyCode=\"ESU\"/>"
        , "      </administrativeInformation>"
        , "    </metaInformation>"
        , "    <flowData>"
        , "      <exchange number=\"9\" name=\"natural gas, liquefied\" category=\"natural gas\""
        , "                subCategory=\"production\" unit=\"Nm3\" meanValue=\"1.0\">"
        , "        <outputGroup>0</outputGroup>"
        , "      </exchange>"
        , "    </flowData>"
        , "  </dataset>"
        , "</ecoSpold>"
        ]

{- | A dataset whose only documentation is the period, stated with the bounds
given. EcoSpold1 has two ways to write them and a real export uses both.
-}
datedPeriodXml :: BC.ByteString -> BC.ByteString
datedPeriodXml bounds =
    BC.unlines
        [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        , "<ecoSpold xmlns=\"http://www.EcoInvent.org/EcoSpold01\">"
        , "  <dataset number=\"10\">"
        , "    <metaInformation>"
        , "      <processInformation>"
        , "        <referenceFunction name=\"dated process\" category=\"c\" subCategory=\"s\" unit=\"kg\"/>"
        , "        <timePeriod>" <> bounds <> "</timePeriod>"
        , "      </processInformation>"
        , "    </metaInformation>"
        , "    <flowData>"
        , "      <exchange number=\"10\" name=\"dated product\" category=\"c\" subCategory=\"s\""
        , "                unit=\"kg\" meanValue=\"1.0\">"
        , "        <outputGroup>0</outputGroup>"
        , "      </exchange>"
        , "    </flowData>"
        , "  </dataset>"
        , "</ecoSpold>"
        ]

{- | A dataset written by an exporter that fills what it has nothing for with
the literal @\<null\>@, as openLCA does across a third of the BAFU export.
-}
nullMarkerXml :: BC.ByteString
nullMarkerXml =
    BC.unlines
        [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        , "<ecoSpold xmlns=\"http://www.EcoInvent.org/EcoSpold01\">"
        , "  <dataset number=\"11\">"
        , "    <metaInformation>"
        , "      <processInformation>"
        , "        <referenceFunction name=\"null process\" category=\"c\" subCategory=\"s\" unit=\"kg\"/>"
        , "        <geography location=\"CH\" text=\"&lt;null&gt;\"/>"
        , "        <technology text=\"Port distances.\"/>"
        , "      </processInformation>"
        , "    </metaInformation>"
        , "    <flowData>"
        , "      <exchange number=\"11\" name=\"null product\" category=\"c\" subCategory=\"s\""
        , "                unit=\"kg\" meanValue=\"1.0\">"
        , "        <outputGroup>0</outputGroup>"
        , "      </exchange>"
        , "    </flowData>"
        , "  </dataset>"
        , "</ecoSpold>"
        ]

-- | The documentation section under that label, if the parser recorded one.
sectionNamed :: Text -> Activity -> Maybe Text
sectionNamed label act = lookup label [(docLabel s, docText s) | s <- activityDocumentation act]

{- | Parse 'documentedXml' and hand the activity to an expectation, failing the
example rather than the whole run when the fixture stops parsing.
-}
withDocumented :: (Activity -> Expectation) -> Expectation
withDocumented k = case parseWithXeno documentedXml of
    Left err -> expectationFailure $ "Parse failed: " ++ err
    Right (act, _, _, _, _, _, _) -> k act

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
    describe "dataset documentation" $ do
        it "names the source the dataset was published in, not the first one read" $
            withDocumented $ \act ->
                sectionNamed "Published in" act
                    `shouldBe` Just "Bussa M., Jungbluth N. (2025). LCI long-distance transport of natural gas. ESU-services Ltd., Schaffhausen, CH."

        it "keeps the methodological report the other source names" $
            withDocumented $ \act ->
                sectionNamed "Sources" act
                    `shouldBe` Just "Frischknecht R. (2007). Overview and Methodology. ecoinvent report No. 1. Swiss Centre for LCI, Duebendorf, CH."

        it "reads the free texts of the process information" $
            withDocumented $ \act -> do
                sectionNamed "Included processes" act `shouldBe` Just "Transport on a freight ship."
                sectionNamed "Technology" act `shouldBe` Just "Distances based on port distances."
                sectionNamed "Geography" act `shouldBe` Just "not known"
                sectionNamed "Sampling procedure" act `shouldBe` Just "Literature."

        it "reads the period as its years followed by what the dataset says about them" $
            withDocumented $ \act ->
                sectionNamed "Time period" act `shouldBe` Just "2023 - 2024 Transport modes investigated for 2023."

        it "prefers the dates over the years when a dataset states both" $
            case parseWithXeno (datedPeriodXml "<startYear>2000</startYear><endYear>2020</endYear><startDate>2000-01</startDate><endDate>2020-01</endDate>") of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right (act, _, _, _, _, _, _) -> sectionNamed "Time period" act `shouldBe` Just "2000-01 - 2020-01"

        it "reads a period stated only as dates, the form most of a real export uses" $
            case parseWithXeno (datedPeriodXml "<startDate>2000-01</startDate><endDate>2020-01</endDate>") of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right (act, _, _, _, _, _, _) -> sectionNamed "Time period" act `shouldBe` Just "2000-01 - 2020-01"

        it "names the proof reader by the person number the validation points at" $
            withDocumented $ \act ->
                sectionNamed "Review" act `shouldBe` Just "Passed. (Niels Jungbluth)"

        it "reads an exporter's null placeholder as an unfilled rubric" $
            case parseWithXeno nullMarkerXml of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right (act, _, _, _, _, _, _) -> do
                    sectionNamed "Geography" act `shouldBe` Nothing
                    sectionNamed "Technology" act `shouldBe` Just "Port distances."

        it "records no section for a dataset that states none" $
            case parseWithXeno minimalXml of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right (act, _, _, _, _, _, _) -> activityDocumentation act `shouldBe` []

    describe "generateFlowUUID" $ do
        it "produces a stable UUID for known inputs" $
            generateFlowUUID 1 "CO2" "air" "" "kg"
                `shouldBe` read "10615fc0-b605-52fc-86d4-273d5523c752"

        it "differs when exchange number changes" $
            generateFlowUUID 1 "CO2" "air" "" "kg"
                `shouldNotBe` generateFlowUUID 2 "CO2" "air" "" "kg"

        it "differs when flow name changes" $
            generateFlowUUID 1 "CO2" "air" "" "kg"
                `shouldNotBe` generateFlowUUID 1 "methane" "air" "" "kg"

        it "differs when subcategory changes (river vs groundwater must not collapse)" $
            generateFlowUUID 1 "Hydrogen sulfide" "water" "river" "kg"
                `shouldNotBe` generateFlowUUID 1 "Hydrogen sulfide" "water" "groundwater, long-term" "kg"

        it "differs when the unit changes (MJ must not be summed into kWh)" $
            generateFlowUUID 1 "Heat, waste" "air" "unspecified" "MJ"
                `shouldNotBe` generateFlowUUID 1 "Heat, waste" "air" "unspecified" "kWh"

    describe "flow identity across datasets" $ do
        it "gives one substance one flow id in every dataset that draws it" $
            case (parseWithXeno minimalXml, parseWithXeno otherAuthorXml) of
                (Right (_, _, bios1, _, _, _, _), Right (_, _, bios2, _, _, _, _)) ->
                    map bfId bios1 `shouldBe` map bfId bios2
                (Left err, _) -> expectationFailure ("minimalXml: " <> err)
                (_, Left err) -> expectationFailure ("otherAuthorXml: " <> err)

        it "reads the dataset number off <dataset>, not off a numbered <person>" $
            case parseWithXeno otherAuthorXml of
                Right (_, _, _, _, _, dsNum, _) -> dsNum `shouldBe` 43
                Left err -> expectationFailure err

        it "reads every dataset's own number when a numbered person precedes them" $
            case parseAllWithXeno multiDatasetXml of
                Right results -> map (fmap (\(_, _, _, _, _, n, _) -> n)) results `shouldBe` [Right 1, Right 2]
                Left err -> expectationFailure err

    describe "generateUnitUUID" $ do
        it "produces a stable UUID for known inputs" $
            generateUnitUUID "kg" `shouldBe` read "d74bc05e-6502-555a-a40c-e6e7580dbf93"

        it "differs for different unit names" $
            generateUnitUUID "kg" `shouldNotBe` generateUnitUUID "m3"

    -- -----------------------------------------------------------------------
    -- parseWithXeno — parsing inline XML
    -- -----------------------------------------------------------------------
    describe "parseWithXeno" $ do
        it "returns Left for invalid XML" $
            case parseWithXeno "<not-xml" of
                Left _ -> return ()
                Right _ -> expectationFailure "Expected Left for invalid XML"

        it "returns Left on an empty input" $
            case parseWithXeno "" of
                Left _ -> return ()
                Right _ -> expectationFailure "Expected Left for empty input"

        it "returns Left on well-formed XML that is not an EcoSpold1 dataset" $
            -- A reasonable XML doc that doesn't carry the EcoSpold structure
            -- must surface as a parse error, not silently produce an empty Activity.
            case parseWithXeno "<?xml version=\"1.0\"?><root><child>hello</child></root>" of
                Left _ -> return ()
                Right _ -> expectationFailure "Expected Left for non-EcoSpold XML"

        it "parses activity name from referenceFunction" $
            case parseWithXeno minimalXml of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right (act, _, _, _, _, _, _) -> activityName act `shouldBe` "electricity production"

        it "parses activity location from geography" $
            case parseWithXeno minimalXml of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right (act, _, _, _, _, _, _) -> activityLocation act `shouldBe` "DE"

        it "parses activity unit from referenceFunction" $
            case parseWithXeno minimalXml of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right (act, _, _, _, _, _, _) -> activityUnit act `shouldBe` "kWh"

        it "parses dataset number" $
            case parseWithXeno minimalXml of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right (_, _, _, _, _, num, _) -> num `shouldBe` 42

        it "keeps the dataset number as the identifier the source gave it" $
            case parseWithXeno minimalXml of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right (act, _, _, _, _, _, _) ->
                    activityNativeId act `shouldBe` Just (NativeProcessId "42")

        -- A missing or unparseable number is read as 0, which is the loader
        -- saying the dataset published none. Displaying that as the identifier
        -- would put a number on a dataset that never had one.
        it "gives no identifier to a dataset that publishes no number" $
            case parseWithXeno unnumberedXml of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right (act, _, _, _, _, _, _) -> activityNativeId act `shouldBe` Nothing

        it "produces 4 exchanges" $
            case parseWithXeno minimalXml of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right (act, _, _, _, _, _, _) -> length (exchanges act) `shouldBe` 4

        it "produces 4 flows (1 tech reference + 3 bio)" $
            case parseWithXeno minimalXml of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right (_, techs, bios, _, _, _, _) ->
                    (length techs + length bios) `shouldBe` 4

        it "marks the reference output (outputGroup 0) as isReference" $
            case parseWithXeno minimalXml of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right (act, _, _, _, _, _, _) ->
                    length (filter exchangeIsReference (exchanges act)) `shouldBe` 1

        it "marks biosphere exchange (outputGroup 4) as BiosphereExchange" $
            case parseWithXeno minimalXml of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right (act, _, _, _, _, _, _) ->
                    let bios = filter (\case BiosphereExchange{} -> True; _ -> False) (exchanges act)
                     in length bios `shouldBe` 2 -- CO2 output + natural gas input (inputGroup 4)
        it "parses flow with CAS number" $
            case parseWithXeno minimalXml of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right (_, _, bios, _, _, _, _) ->
                    let co2Flows = filter (\f -> bfName f == "Carbon dioxide, fossil") bios
                     in case co2Flows of
                            [f] -> bfCAS f `shouldBe` Just "124-38-9"
                            _ -> expectationFailure "Expected exactly one CO2 flow"

        it "sets activity classification from category/subCategory" $
            case parseWithXeno minimalXml of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right (act, _, _, _, _, _, _) ->
                    M.lookup "Category" (activityClassification act) `shouldBe` Just "Energy"

        it "captures per-exchange generalComment as exchangeComment" $
            case parseWithXeno commentXml of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right (act, _, _, _, _, _, _) ->
                    map exchangeComment (exchanges act) `shouldBe` [Just "Global", Nothing]

        it "still routes referenceFunction generalComment to activity description" $
            case parseWithXeno commentXml of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right (act, _, _, _, _, _, _) ->
                    activityDescription act `shouldBe` ["Process-level note"]

    -- -----------------------------------------------------------------------
    -- parseAllWithXeno — multi-dataset
    -- -----------------------------------------------------------------------
    describe "parseAllWithXeno" $ do
        it "returns Left for invalid XML" $
            case parseAllWithXeno "<not-xml" of
                Left _ -> return ()
                Right _ -> expectationFailure "Expected Left for invalid XML"

        it "parses two datasets from multi-dataset XML" $
            case parseAllWithXeno multiDatasetXml of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right results -> length results `shouldBe` 2

        it "parses activity names from both datasets" $
            case parseAllWithXeno multiDatasetXml of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right results ->
                    let names = [activityName act | Right (act, _, _, _, _, _, _) <- results]
                     in names `shouldBe` ["process A", "process B"]

        it "preserves dataset numbers in order" $
            case parseAllWithXeno multiDatasetXml of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right results ->
                    let nums = [n | Right (_, _, _, _, _, n, _) <- results]
                     in nums `shouldBe` [1, 2]

        it "parses location from each dataset" $
            case parseAllWithXeno multiDatasetXml of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right results ->
                    let locs = [activityLocation act | Right (act, _, _, _, _, _, _) <- results]
                     in locs `shouldBe` ["CH", "FR"]

    -- -----------------------------------------------------------------------
    -- Final waste flows: waste with no modelled treatment is an elementary
    -- flow of medium "waste". Nothing treats it, so nothing produces it, and
    -- reading it as an input would demand a supplier no database can provide.
    -- -----------------------------------------------------------------------
    describe "Final waste flows routing" $ do
        it "reads category=\"Final waste flows\" as a biosphere flow of medium waste" $
            case parseWithXeno wasteFlowXml of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right (act, _, bios, wastes, _, _, _) -> do
                    let bioExchanges = [e | e@BiosphereExchange{} <- exchanges act]
                    length bioExchanges `shouldBe` 1
                    map bfName bios `shouldBe` ["Organic carbon, placed in landfill"]
                    map bfCompartment bios
                        `shouldBe` [Just (Compartment Waste (Just "landfill"))]
                    length wastes `shouldBe` 0

        it "does not route the waste flow to the technosphere bucket" $
            case parseWithXeno wasteFlowXml of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right (_, techs, _, _, _, _, _) ->
                    -- Only the reference product remains on the tech side;
                    -- the Final-waste-flows row must not show up there.
                    map tfName techs `shouldBe` ["aluminium ingot"]

        it "reads it as an emission despite the input group" $
            case parseWithXeno wasteFlowXml of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right (act, _, _, _, _, _, _) ->
                    -- inputGroup=5 is an artefact of the format, not a
                    -- direction: the row is waste leaving the system.
                    [bioDirection e | e@BiosphereExchange{} <- exchanges act]
                        `shouldBe` [Emission]

        it "falls back to the activity location, as for any elementary flow" $
            case parseWithXeno wasteFlowXml of
                Left err -> expectationFailure $ "Parse failed: " ++ err
                Right (act, _, _, _, _, _, _) ->
                    [bioLocation e | e@BiosphereExchange{} <- exchanges act] `shouldBe` ["RoW"]
