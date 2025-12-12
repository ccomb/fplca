{-# LANGUAGE OverloadedStrings #-}

module MethodSpec (spec) where

import Test.Hspec
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID

import LCA.Method.Parser
import LCA.Method.Types
import LCA.SynonymDB

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

-- Helper for testing Either values
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
