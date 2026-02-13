{-# LANGUAGE OverloadedStrings #-}

module SimaProParserSpec (spec) where

import Test.Hspec
import SimaPro.Parser (parseSimaProCSV)
import LCA.Types (Activity(..), Exchange(..), Unit(..), Flow, UUID)
import LCA.UnitConversion (defaultUnitConfig, isKnownUnit)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import System.IO.Temp (withSystemTempFile)
import System.IO (hClose)

-- | Test CSV content with a quoted product name containing the delimiter (;)
testCSV :: BS.ByteString
testCSV = BS.intercalate "\r\n"
    [ "{SimaPro 9.6.0.1}"
    , "{CSV separator: semicolon}"
    , "{Decimal separator: .}"
    , ""
    , "Process"
    , ""
    , "Category type"
    , "material"
    , ""
    , "Process name"
    , "Steel Production"
    , ""
    , "Type"
    , "Unit process"
    , ""
    , "Geography"
    , "GLO"
    , ""
    , "Products"
    , "Steel;kg;1.0;100;not defined;material;"
    , ""
    , "End"
    , ""
    , "Process"
    , ""
    , "Category type"
    , "material"
    , ""
    , "Process name"
    , "Irradiated Food"
    , ""
    , "Type"
    , "Unit process"
    , ""
    , "Geography"
    , "GLO"
    , ""
    , "Products"
    , "\"Food product (irradiated ; with treatment)\";foo_unit;2.0;100;not defined;material;"
    , ""
    , "End"
    ]

-- | Parse the test CSV via a temp file
parseTestCSV :: IO ([Activity], M.Map UUID Flow, M.Map UUID Unit)
parseTestCSV = withSystemTempFile "test.csv" $ \path handle -> do
    BS.hPut handle testCSV
    hClose handle
    parseSimaProCSV path

spec :: Spec
spec = do
    describe "SimaPro CSV parsing" $ do
        it "correctly extracts units from CSV with quoted fields" $ do
            (_, _, unitDB) <- parseTestCSV
            let unitNames = map unitName $ M.elems unitDB
            unitNames `shouldContain` ["kg"]
            unitNames `shouldContain` ["foo_unit"]
            -- No garbage from quoted product names should appear as unit names
            -- Garbage strings would be long or contain parentheses
            let isGarbage u = T.length u > 20 || T.isInfixOf ")" u || T.isInfixOf "treatment" u
            filter isGarbage unitNames `shouldBe` []

        it "reports unknown units correctly" $ do
            (_, _, unitDB) <- parseTestCSV
            let cfg = defaultUnitConfig
                unknowns = [unitName u | u <- M.elems unitDB
                           , not (isKnownUnit cfg (unitName u))]
            unknowns `shouldContain` ["foo_unit"]
            unknowns `shouldNotContain` ["kg"]

        it "parses product names with embedded delimiters correctly" $ do
            (activities, _, _) <- parseTestCSV
            let names = map activityName activities
            -- The quoted product name should be extracted intact
            names `shouldContain` ["Food product (irradiated ; with treatment)"]

    describe "SimaPro waste treatment parsing" $ do
        it "parses waste treatment processes (Waste treatment section)" $ do
            (activities, _, _) <- parseWasteCSV
            length activities `shouldSatisfy` (>= 2)

        it "uses Waste treatment row as activity name" $ do
            (activities, _, _) <- parseWasteCSV
            let names = map activityName activities
            names `shouldContain` ["Municipal waste incineration"]

        it "marks Waste to treatment exchanges as inputs" $ do
            (activities, _, _) <- parseWasteCSV
            let producer = head [a | a <- activities, activityName a == "Widget"]
                wasteExchanges = [e | e@TechnosphereExchange{} <- exchanges producer
                                    , not (techIsReference e)
                                    , techIsInput e]
            length wasteExchanges `shouldSatisfy` (>= 1)

-- | Test CSV with waste treatment process and waste-to-treatment demand
wasteTestCSV :: BS.ByteString
wasteTestCSV = BS.intercalate "\r\n"
    [ "{SimaPro 9.6.0.1}"
    , "{CSV separator: semicolon}"
    , "{Decimal separator: .}"
    , ""
    -- Producer with a Waste to treatment section
    , "Process"
    , ""
    , "Category type"
    , "material"
    , ""
    , "Process name"
    , "Widget production"
    , ""
    , "Type"
    , "Unit process"
    , ""
    , "Products"
    , "Widget;kg;1.0;100;not defined;material;"
    , ""
    , "Waste to treatment"
    , "Municipal waste;kg;0.5;Undefined;;;;;;"
    , ""
    , "End"
    , ""
    -- Waste treatment process (no Products section, only Waste treatment)
    , "Process"
    , ""
    , "Category type"
    , "waste treatment"
    , ""
    , "Process name"
    , "Incineration process"
    , ""
    , "Type"
    , "Unit process"
    , ""
    , "Waste treatment"
    , "Municipal waste incineration;kg;1.0;100;All waste types;waste treatment;"
    , ""
    , "End"
    ]

-- | Parse the waste test CSV via a temp file
parseWasteCSV :: IO ([Activity], M.Map UUID Flow, M.Map UUID Unit)
parseWasteCSV = withSystemTempFile "waste-test.csv" $ \path handle -> do
    BS.hPut handle wasteTestCSV
    hClose handle
    parseSimaProCSV path
