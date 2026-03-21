{-# LANGUAGE OverloadedStrings #-}

module ILCDParserSpec (spec) where

import Test.Hspec
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import Data.Text (Text)
import ILCD.Parser (parseProcessXML, ILCDProcessRaw(..))

classOf :: BS.ByteString -> M.Map Text Text
classOf = maybe M.empty iprClassifications . parseProcessXML

spec :: Spec
spec = do
    describe "ILCD Process Parser" $ do
        it "parses classification from classificationInformation" $
            M.lookup "ILCDCategories" (classOf ilcdProcessWithClassification)
                `shouldBe` Just "End-of-life treatment/Material recycling"

        it "parses multiple classification systems" $ do
            let cls = classOf ilcdProcessWithTwoClassifications
            M.lookup "ILCDCategories" cls `shouldBe` Just "Energy/Electricity"
            M.lookup "EcoSpold" cls `shouldBe` Just "Supply"

        it "produces empty classifications when none present" $
            classOf ilcdProcessNoClassification `shouldBe` M.empty

-- Minimal ILCD process XML with classification
ilcdProcessWithClassification :: BS.ByteString
ilcdProcessWithClassification =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
    \<processDataSet xmlns=\"http://lca.jrc.it/ILCD/Process\" \
    \xmlns:common=\"http://lca.jrc.it/ILCD/Common\">\
    \<processInformation>\
    \<dataSetInformation>\
    \<common:UUID>12345678-1234-1234-1234-123456789abc</common:UUID>\
    \<name><baseName>Test Process</baseName></name>\
    \<classificationInformation>\
    \<common:classification name=\"ILCDCategories\">\
    \<common:class level=\"0\">End-of-life treatment</common:class>\
    \<common:class level=\"1\">Material recycling</common:class>\
    \</common:classification>\
    \</classificationInformation>\
    \</dataSetInformation>\
    \<geography location=\"DE\"/>\
    \<quantitativeReference>\
    \<referenceToReferenceFlow>0</referenceToReferenceFlow>\
    \</quantitativeReference>\
    \</processInformation>\
    \<exchanges>\
    \<exchange dataSetInternalID=\"0\">\
    \<referenceToFlowDataSet refObjectId=\"aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee\"/>\
    \<exchangeDirection>Output</exchangeDirection>\
    \<resultingAmount>1.0</resultingAmount>\
    \</exchange>\
    \</exchanges>\
    \</processDataSet>"

ilcdProcessWithTwoClassifications :: BS.ByteString
ilcdProcessWithTwoClassifications =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
    \<processDataSet xmlns=\"http://lca.jrc.it/ILCD/Process\" \
    \xmlns:common=\"http://lca.jrc.it/ILCD/Common\">\
    \<processInformation>\
    \<dataSetInformation>\
    \<common:UUID>22345678-1234-1234-1234-123456789abc</common:UUID>\
    \<name><baseName>Test Process 2</baseName></name>\
    \<classificationInformation>\
    \<common:classification name=\"ILCDCategories\">\
    \<common:class level=\"0\">Energy</common:class>\
    \<common:class level=\"1\">Electricity</common:class>\
    \</common:classification>\
    \<common:classification name=\"EcoSpold\">\
    \<common:class level=\"0\">Supply</common:class>\
    \</common:classification>\
    \</classificationInformation>\
    \</dataSetInformation>\
    \<geography location=\"FR\"/>\
    \<quantitativeReference>\
    \<referenceToReferenceFlow>0</referenceToReferenceFlow>\
    \</quantitativeReference>\
    \</processInformation>\
    \<exchanges>\
    \<exchange dataSetInternalID=\"0\">\
    \<referenceToFlowDataSet refObjectId=\"aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee\"/>\
    \<exchangeDirection>Output</exchangeDirection>\
    \<resultingAmount>1.0</resultingAmount>\
    \</exchange>\
    \</exchanges>\
    \</processDataSet>"

ilcdProcessNoClassification :: BS.ByteString
ilcdProcessNoClassification =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
    \<processDataSet xmlns=\"http://lca.jrc.it/ILCD/Process\" \
    \xmlns:common=\"http://lca.jrc.it/ILCD/Common\">\
    \<processInformation>\
    \<dataSetInformation>\
    \<common:UUID>32345678-1234-1234-1234-123456789abc</common:UUID>\
    \<name><baseName>Test Process 3</baseName></name>\
    \</dataSetInformation>\
    \<geography location=\"US\"/>\
    \<quantitativeReference>\
    \<referenceToReferenceFlow>0</referenceToReferenceFlow>\
    \</quantitativeReference>\
    \</processInformation>\
    \<exchanges>\
    \<exchange dataSetInternalID=\"0\">\
    \<referenceToFlowDataSet refObjectId=\"aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee\"/>\
    \<exchangeDirection>Output</exchangeDirection>\
    \<resultingAmount>1.0</resultingAmount>\
    \</exchange>\
    \</exchanges>\
    \</processDataSet>"
