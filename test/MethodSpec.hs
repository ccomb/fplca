{-# LANGUAGE OverloadedStrings #-}

module MethodSpec (spec) where

import Test.Hspec
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID

import Method.Mapping (computeLCIAScore, MatchStrategy(..))
import Method.Parser
import Method.Types
import SynonymDB
import Types (Flow(..), FlowType(..))

spec :: Spec
spec = do
    describe "SynonymDB" $ do
        describe "normalizeName" $ do
            it "lowercases names" $ do
                normalizeName "Carbon Dioxide" `shouldBe` "carbon dioxide"

            it "strips leading/trailing whitespace" $ do
                normalizeName "  carbon dioxide  " `shouldBe` "carbon dioxide"

            it "collapses multiple spaces" $ do
                normalizeName "carbon   dioxide" `shouldBe` "carbon dioxide"

            it "strips ', in ground' suffix" $ do
                normalizeName "iron, in ground" `shouldBe` "iron"

            it "strips ' in ground' suffix" $ do
                normalizeName "iron in ground" `shouldBe` "iron"

            it "strips '/kg' suffix" $ do
                normalizeName "carbon dioxide/kg" `shouldBe` "carbon dioxide"

            it "removes punctuation (commas, parens, quotes)" $ do
                normalizeName "carbon (IV) oxide, fossil" `shouldBe` "carbon iv oxide fossil"

            it "handles complex names" $ do
                normalizeName "  Carbon Dioxide (biogenic), in air  " `shouldBe` "carbon dioxide biogenic in air"

        describe "loadEmbeddedSynonymDB" $ do
            it "loads the embedded synonym database" $ do
                db <- loadEmbeddedSynonymDB
                -- Should have loaded some data
                synNameToId db `shouldSatisfy` (not . null)
                synIdToNames db `shouldSatisfy` (not . null)

            it "can look up 'carbon dioxide'" $ do
                db <- loadEmbeddedSynonymDB
                let result = lookupSynonymGroup db "carbon dioxide"
                result `shouldSatisfy` maybe False (const True)

            it "maps 'co2' to same group as 'carbon dioxide'" $ do
                db <- loadEmbeddedSynonymDB
                let co2Group = lookupSynonymGroup db "co2"
                    carbonGroup = lookupSynonymGroup db "carbon dioxide"
                co2Group `shouldBe` carbonGroup

            it "can retrieve synonyms for a group" $ do
                db <- loadEmbeddedSynonymDB
                case lookupSynonymGroup db "carbon dioxide" of
                    Nothing -> expectationFailure "carbon dioxide not found in DB"
                    Just gid -> do
                        let synonyms = getSynonyms db gid
                        synonyms `shouldSatisfy` maybe False (not . null)

    describe "Method Parser" $ do
        describe "parseMethodBytes" $ do
            it "parses a minimal valid method XML" $ do
                let xml = TE.encodeUtf8 $ T.unlines
                        [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                        , "<LCIAMethodDataSet>"
                        , "  <LCIAMethodInformation>"
                        , "    <dataSetInformation>"
                        , "      <UUID>12345678-1234-1234-1234-123456789012</UUID>"
                        , "      <name>Test Method</name>"
                        , "      <impactCategory>Climate change</impactCategory>"
                        , "    </dataSetInformation>"
                        , "    <quantitativeReference>"
                        , "      <referenceQuantity>"
                        , "        <shortDescription>kg CO2 eq</shortDescription>"
                        , "      </referenceQuantity>"
                        , "    </quantitativeReference>"
                        , "  </LCIAMethodInformation>"
                        , "  <characterisationFactors>"
                        , "    <factor>"
                        , "      <referenceToFlowDataSet refObjectId=\"aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee\">"
                        , "        <shortDescription>Carbon dioxide (Mass, kg)</shortDescription>"
                        , "      </referenceToFlowDataSet>"
                        , "      <exchangeDirection>Output</exchangeDirection>"
                        , "      <meanValue>1.0</meanValue>"
                        , "    </factor>"
                        , "  </characterisationFactors>"
                        , "</LCIAMethodDataSet>"
                        ]
                case parseMethodBytes xml of
                    Left err -> expectationFailure $ "Parse failed: " ++ err
                    Right method -> do
                        methodName method `shouldBe` "Test Method"
                        methodUnit method `shouldBe` "kg CO2 eq"
                        methodCategory method `shouldBe` "Climate change"
                        length (methodFactors method) `shouldBe` 1

            it "extracts characterization factor values correctly" $ do
                let xml = TE.encodeUtf8 $ T.unlines
                        [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                        , "<LCIAMethodDataSet>"
                        , "  <LCIAMethodInformation>"
                        , "    <dataSetInformation>"
                        , "      <UUID>12345678-1234-1234-1234-123456789012</UUID>"
                        , "      <name>Test</name>"
                        , "    </dataSetInformation>"
                        , "    <quantitativeReference>"
                        , "      <referenceQuantity>"
                        , "        <shortDescription>kg</shortDescription>"
                        , "      </referenceQuantity>"
                        , "    </quantitativeReference>"
                        , "  </LCIAMethodInformation>"
                        , "  <characterisationFactors>"
                        , "    <factor>"
                        , "      <referenceToFlowDataSet refObjectId=\"aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee\">"
                        , "        <shortDescription>Methane (Mass)</shortDescription>"
                        , "      </referenceToFlowDataSet>"
                        , "      <exchangeDirection>Output</exchangeDirection>"
                        , "      <meanValue>28.5</meanValue>"
                        , "    </factor>"
                        , "  </characterisationFactors>"
                        , "</LCIAMethodDataSet>"
                        ]
                case parseMethodBytes xml of
                    Left err -> expectationFailure $ "Parse failed: " ++ err
                    Right method -> do
                        let cf = head (methodFactors method)
                        mcfFlowName cf `shouldBe` "Methane"
                        mcfValue cf `shouldBe` 28.5
                        mcfDirection cf `shouldBe` Output

            it "parses Input direction correctly" $ do
                let xml = TE.encodeUtf8 $ T.unlines
                        [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                        , "<LCIAMethodDataSet>"
                        , "  <LCIAMethodInformation>"
                        , "    <dataSetInformation>"
                        , "      <UUID>12345678-1234-1234-1234-123456789012</UUID>"
                        , "      <name>Test</name>"
                        , "    </dataSetInformation>"
                        , "    <quantitativeReference>"
                        , "      <referenceQuantity>"
                        , "        <shortDescription>MJ</shortDescription>"
                        , "      </referenceQuantity>"
                        , "    </quantitativeReference>"
                        , "  </LCIAMethodInformation>"
                        , "  <characterisationFactors>"
                        , "    <factor>"
                        , "      <referenceToFlowDataSet refObjectId=\"aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee\">"
                        , "        <shortDescription>Crude oil (Mass)</shortDescription>"
                        , "      </referenceToFlowDataSet>"
                        , "      <exchangeDirection>Input</exchangeDirection>"
                        , "      <meanValue>42.0</meanValue>"
                        , "    </factor>"
                        , "  </characterisationFactors>"
                        , "</LCIAMethodDataSet>"
                        ]
                case parseMethodBytes xml of
                    Left err -> expectationFailure $ "Parse failed: " ++ err
                    Right method -> do
                        let cf = head (methodFactors method)
                        mcfDirection cf `shouldBe` Input

            it "fails on missing UUID" $ do
                let xml = TE.encodeUtf8 $ T.unlines
                        [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                        , "<LCIAMethodDataSet>"
                        , "  <LCIAMethodInformation>"
                        , "    <dataSetInformation>"
                        , "      <name>Test</name>"
                        , "    </dataSetInformation>"
                        , "  </LCIAMethodInformation>"
                        , "</LCIAMethodDataSet>"
                        ]
                parseMethodBytes xml `shouldSatisfy` isLeft

            it "fails on missing name" $ do
                let xml = TE.encodeUtf8 $ T.unlines
                        [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                        , "<LCIAMethodDataSet>"
                        , "  <LCIAMethodInformation>"
                        , "    <dataSetInformation>"
                        , "      <UUID>12345678-1234-1234-1234-123456789012</UUID>"
                        , "    </dataSetInformation>"
                        , "  </LCIAMethodInformation>"
                        , "</LCIAMethodDataSet>"
                        ]
                parseMethodBytes xml `shouldSatisfy` isLeft

    describe "LCIA Score Computation" $ do
        describe "computeLCIAScore" $ do
            it "computes score as sum of inventory * CF for mapped flows" $ do
                let co2Uuid = UUID.fromWords 1 2 3 4
                    ch4Uuid = UUID.fromWords 5 6 7 8
                    -- Inventory: CO2 = 10 kg, CH4 = 2 kg
                    inventory = M.fromList [(co2Uuid, 10.0), (ch4Uuid, 2.0)]
                    -- Method: CO2 CF = 1.0, CH4 CF = 28.0
                    co2CF = MethodCF co2Uuid "Carbon dioxide" Output 1.0
                    ch4CF = MethodCF ch4Uuid "Methane" Output 28.0
                    co2Flow = mkTestFlow co2Uuid "Carbon dioxide"
                    ch4Flow = mkTestFlow ch4Uuid "Methane"
                    mappings = [ (co2CF, Just (co2Flow, ByUUID))
                               , (ch4CF, Just (ch4Flow, ByUUID))
                               ]
                -- Score = 10*1 + 2*28 = 10 + 56 = 66
                computeLCIAScore inventory mappings `shouldBe` 66.0

            it "ignores unmapped flows" $ do
                let co2Uuid = UUID.fromWords 1 2 3 4
                    ch4Uuid = UUID.fromWords 5 6 7 8
                    inventory = M.fromList [(co2Uuid, 10.0), (ch4Uuid, 2.0)]
                    co2CF = MethodCF co2Uuid "Carbon dioxide" Output 1.0
                    ch4CF = MethodCF ch4Uuid "Methane" Output 28.0
                    co2Flow = mkTestFlow co2Uuid "Carbon dioxide"
                    mappings = [ (co2CF, Just (co2Flow, ByUUID))
                               , (ch4CF, Nothing)  -- CH4 not mapped
                               ]
                -- Score = 10*1 = 10 (CH4 ignored because not mapped)
                computeLCIAScore inventory mappings `shouldBe` 10.0

            it "returns 0 for flows not in inventory" $ do
                let co2Uuid = UUID.fromWords 1 2 3 4
                    n2oUuid = UUID.fromWords 9 10 11 12
                    -- Inventory has only CO2
                    inventory = M.fromList [(co2Uuid, 10.0)]
                    -- Method has N2O that's not in inventory
                    n2oCF = MethodCF n2oUuid "Dinitrogen monoxide" Output 265.0
                    n2oFlow = mkTestFlow n2oUuid "Dinitrogen monoxide"
                    mappings = [ (n2oCF, Just (n2oFlow, ByName)) ]
                -- Score = 0 (N2O not in inventory)
                computeLCIAScore inventory mappings `shouldBe` 0.0

            it "handles negative inventory values (resource extraction)" $ do
                let oilUuid = UUID.fromWords 11 12 13 14
                    -- Resource extraction has negative sign in inventory
                    inventory = M.fromList [(oilUuid, -5.0)]
                    oilCF = MethodCF oilUuid "Crude oil" Input 42.0
                    oilFlow = mkTestFlow oilUuid "Crude oil"
                    mappings = [ (oilCF, Just (oilFlow, ByUUID)) ]
                -- Score = -5 * 42 = -210 (negative = resource depletion)
                computeLCIAScore inventory mappings `shouldBe` (-210.0)

            it "returns 0 for empty mappings" $ do
                let inventory = M.fromList [(UUID.fromWords 1 2 3 4, 100.0)]
                computeLCIAScore inventory [] `shouldBe` 0.0

-- Helper for testing Either values
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

-- Helper to create a test Flow
mkTestFlow :: UUID.UUID -> T.Text -> Flow
mkTestFlow uuid name = Flow
    { flowId = uuid
    , flowName = name
    , flowUnitId = UUID.nil
    , flowType = Biosphere
    , flowCategory = "air"
    , flowSubcompartment = Nothing
    , flowCAS = Nothing
    , flowSubstanceId = Nothing
    , flowSynonyms = M.empty
    }
