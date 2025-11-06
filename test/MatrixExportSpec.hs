{-# LANGUAGE OverloadedStrings #-}

module MatrixExportSpec (spec) where

import Test.Hspec
import TestHelpers
import GoldenData
import ACV.Types
import ACV.Service (exportUniversalMatrixFormat)
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

spec :: Spec
spec = do
    describe "Matrix Export Format" $ do
        it "exports A_public.csv in (I-A) format with negative off-diagonal" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            withSystemTempDirectory "acv-test" $ \tmpDir -> do
                -- Export matrices
                exportUniversalMatrixFormat tmpDir db

                -- Read A_public.csv
                let aMatrixPath = tmpDir </> "A_public.csv"
                aMatrixContent <- TIO.readFile aMatrixPath

                -- Check header
                let lines = T.lines aMatrixContent
                length lines `shouldSatisfy` (> 1)

                -- Parse first line as header
                let header = head lines
                T.isInfixOf "row;column;coefficient" header `shouldBe` True

                -- Check diagonal entries (should be 1.0)
                let diagonalLines = filter (T.isInfixOf "0;0;1.0") lines
                length diagonalLines `shouldSatisfy` (>= 1)

                -- Check off-diagonal entries (should be NEGATIVE for (I-A) format)
                let offDiagonalLines = tail lines  -- Skip header
                let offDiagonalEntries = filter (\l -> not (T.isInfixOf ";1.0;" l)) offDiagonalLines

                -- For SAMPLE.min3: Expected -0.6 and -0.4
                let hasNegative = any (T.isInfixOf "-0.") aMatrixContent
                hasNegative `shouldBe` True

        it "exports B_public.csv with correct signs" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            withSystemTempDirectory "acv-test" $ \tmpDir -> do
                exportUniversalMatrixFormat tmpDir db

                -- Read B_public.csv
                let bMatrixPath = tmpDir </> "B_public.csv"
                bMatrixContent <- TIO.readFile bMatrixPath

                -- Check header
                let lines = T.lines bMatrixContent
                length lines `shouldSatisfy` (> 1)

                -- Biosphere values should be positive for emissions
                -- SAMPLE.min3 has 4.0 kg CO2 and 0.003 kg Zinc
                let hasPositive = any (\l -> T.isInfixOf ";4.0;" l || T.isInfixOf ";0.003;" l) lines
                hasPositive `shouldBe` True

        it "exports ie_index.csv with activity information" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            withSystemTempDirectory "acv-test" $ \tmpDir -> do
                exportUniversalMatrixFormat tmpDir db

                -- Read ie_index.csv
                let indexPath = tmpDir </> "ie_index.csv"
                indexContent <- TIO.readFile indexPath

                -- Check header and 3 activities
                let lines = T.lines indexContent
                length lines `shouldBe` 4  -- header + 3 activities

        it "exports ee_index.csv with biosphere flow information" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            withSystemTempDirectory "acv-test" $ \tmpDir -> do
                exportUniversalMatrixFormat tmpDir db

                -- Read ee_index.csv
                let indexPath = tmpDir </> "ee_index.csv"
                indexContent <- TIO.readFile indexPath

                -- Check header and 2 flows (CO2, Zinc)
                let lines = T.lines indexContent
                length lines `shouldBe` 3  -- header + 2 flows

    describe "Export CSV Format Validation" $ do
        it "uses semicolon as delimiter" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            withSystemTempDirectory "acv-test" $ \tmpDir -> do
                exportUniversalMatrixFormat tmpDir db

                let aMatrixPath = tmpDir </> "A_public.csv"
                aMatrixContent <- TIO.readFile aMatrixPath

                -- All lines should contain semicolons
                let lines = T.lines aMatrixContent
                let allHaveSemicolon = all (T.isInfixOf ";") lines
                allHaveSemicolon `shouldBe` True

        it "exports correct number of columns" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            withSystemTempDirectory "acv-test" $ \tmpDir -> do
                exportUniversalMatrixFormat tmpDir db

                let aMatrixPath = tmpDir </> "A_public.csv"
                aMatrixContent <- TIO.readFile aMatrixPath

                -- Format: row;column;coefficient;uncertainty type;varianceWithPedigreeUncertainty;minValue;mostLikelyValue;maxValue
                -- Should have 8 fields
                let dataLines = tail $ T.lines aMatrixContent  -- Skip header
                let firstDataLine = head dataLines
                let fields = T.splitOn ";" firstDataLine
                length fields `shouldBe` 8
