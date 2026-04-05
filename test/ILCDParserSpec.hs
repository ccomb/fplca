{-# LANGUAGE OverloadedStrings #-}

module ILCDParserSpec (spec) where

import Test.Hspec
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.UUID as UUID
import ILCD.Parser (parseProcessXML, ILCDProcessRaw(..), buildSupplierIndex, fixActivityExchanges)
import Types

classOf :: BS.ByteString -> M.Map Text Text
classOf = maybe M.empty iprClassifications . parseProcessXML

-- UUIDs used in supplier linking tests
flowUUID1, flowUUID2, actUUID1, actUUID2, prodUUID1, prodUUID2 :: UUID.UUID
flowUUID1 = read "11111111-0000-0000-0000-000000000001"
flowUUID2 = read "22222222-0000-0000-0000-000000000002"
actUUID1  = read "aaaaaaaa-0000-0000-0000-000000000001"
actUUID2  = read "aaaaaaaa-0000-0000-0000-000000000002"
prodUUID1 = read "bbbbbbbb-0000-0000-0000-000000000001"
prodUUID2 = read "bbbbbbbb-0000-0000-0000-000000000002"

-- An activity with a single reference output exchange for the given flow UUID
activityWithRefExchange :: UUID.UUID -> Activity
activityWithRefExchange fid = Activity
    { activityName = "test"
    , activityDescription = []
    , activitySynonyms = M.empty
    , activityClassification = M.empty
    , activityLocation = "GLO"
    , activityUnit = "kg"
    , exchanges =
        [ TechnosphereExchange
            { techFlowId = fid
            , techAmount = 1.0
            , techUnitId = UUID.nil
            , techIsInput = False
            , techIsReference = True
            , techActivityLinkId = UUID.nil
            , techProcessLinkId = Nothing
            , techLocation = ""
            } ]
    , activityParams = M.empty
    , activityParamExprs = M.empty
    }

-- An activity with a single unresolved input exchange for the given flow UUID
activityWithInputExchange :: UUID.UUID -> Activity
activityWithInputExchange fid = Activity
    { activityName = "consumer"
    , activityDescription = []
    , activitySynonyms = M.empty
    , activityClassification = M.empty
    , activityLocation = "GLO"
    , activityUnit = "kg"
    , exchanges =
        [ TechnosphereExchange
            { techFlowId = fid
            , techAmount = 0.5
            , techUnitId = UUID.nil
            , techIsInput = True
            , techIsReference = False
            , techActivityLinkId = UUID.nil
            , techProcessLinkId = Nothing
            , techLocation = ""
            } ]
    , activityParams = M.empty
    , activityParamExprs = M.empty
    }

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

    -- -------------------------------------------------------------------
    -- buildSupplierIndex: UUID-keyed, no name indirection
    -- -------------------------------------------------------------------
    describe "buildSupplierIndex" $ do

        it "indexes reference exchanges by flow UUID" $ do
            let activities = M.fromList
                    [ ((actUUID1, prodUUID1), activityWithRefExchange flowUUID1)
                    , ((actUUID2, prodUUID2), activityWithRefExchange flowUUID2)
                    ]
                idx = buildSupplierIndex activities
            M.lookup flowUUID1 idx `shouldBe` Just (actUUID1, prodUUID1)
            M.lookup flowUUID2 idx `shouldBe` Just (actUUID2, prodUUID2)

        it "does not index non-reference exchanges" $ do
            let activities = M.fromList
                    [ ((actUUID1, prodUUID1), activityWithInputExchange flowUUID1) ]
                idx = buildSupplierIndex activities
            M.lookup flowUUID1 idx `shouldBe` Nothing

        it "two activities with same flow name but different UUIDs are both indexed" $ do
            -- This is the bug the old name-based code had: M.fromList on names
            -- would silently discard one. UUID keys have no such collision.
            let activities = M.fromList
                    [ ((actUUID1, prodUUID1), activityWithRefExchange flowUUID1)
                    , ((actUUID2, prodUUID2), activityWithRefExchange flowUUID2)
                    ]
                idx = buildSupplierIndex activities
            M.size idx `shouldBe` 2

    -- -------------------------------------------------------------------
    -- fixActivityExchanges: resolves input exchanges via supplier index
    -- -------------------------------------------------------------------
    describe "fixActivityExchanges" $ do

        it "resolves input exchange flow UUID to supplier (actUUID, prodUUID)" $ do
            let idx = M.fromList [(flowUUID1, (actUUID1, prodUUID1))]
                act = activityWithInputExchange flowUUID1
                fixed = fixActivityExchanges idx act
            case exchanges fixed of
                [TechnosphereExchange { techFlowId = fid, techActivityLinkId = alink }] -> do
                    fid   `shouldBe` prodUUID1
                    alink `shouldBe` actUUID1
                _ -> expectationFailure "expected one TechnosphereExchange"

        it "leaves input exchange unchanged when flow UUID not in index" $ do
            let idx = M.empty
                act = activityWithInputExchange flowUUID1
                fixed = fixActivityExchanges idx act
            case exchanges fixed of
                [TechnosphereExchange { techFlowId = fid }] ->
                    fid `shouldBe` flowUUID1
                _ -> expectationFailure "expected one TechnosphereExchange"

        it "does not touch output (reference) exchanges" $ do
            let idx = M.fromList [(flowUUID1, (actUUID1, prodUUID1))]
                act = activityWithRefExchange flowUUID1
                fixed = fixActivityExchanges idx act
            case exchanges fixed of
                [TechnosphereExchange { techFlowId = fid, techIsReference = isRef }] -> do
                    fid   `shouldBe` flowUUID1   -- unchanged
                    isRef `shouldBe` True
                _ -> expectationFailure "expected one TechnosphereExchange"

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
