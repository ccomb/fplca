{-# LANGUAGE OverloadedStrings #-}

{- | Covers the end-to-end upload pipeline for openLCA JSON-LD method files:
the upload byte-stream is sniffed, persisted under the right extension,
and the loader picks it up via OlcaSchema. Regression gate for the
pre-existing bug where a JSON blob was mis-classified as CSV.
-}
module MethodUploadSpec (spec) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Maybe (fromMaybe)
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import API.DatabaseHandlers (formatToText)
import Config (MethodConfig (..))
import Database.Manager (loadMethodCollectionFromConfig)
import Database.Upload
import Method.Types (Method (..), MethodCF (..), mcMethods)

{- | A minimal, hand-written openLCA ImpactCategory JSON-LD document.
One impact factor is enough to assert the full pipeline parses correctly.
-}
miniImpactCategoryJson :: BL.ByteString
miniImpactCategoryJson =
    BLC.pack $
        unlines
            [ "{"
            , "  \"@context\": \"http://greendelta.github.io/olca-schema/context.jsonld\","
            , "  \"@type\": \"ImpactCategory\","
            , "  \"@id\": \"00000000-0000-0000-0000-000000000001\","
            , "  \"name\": \"Test category\","
            , "  \"referenceUnitName\": \"m2*year\","
            , "  \"impactFactors\": ["
            , "    {"
            , "      \"@type\": \"ImpactFactor\","
            , "      \"value\": 1.5,"
            , "      \"flow\": {"
            , "        \"@type\": \"Flow\","
            , "        \"@id\": \"00000000-0000-0000-0000-000000000002\","
            , "        \"name\": \"Occupation, test\","
            , "        \"flowType\": \"ELEMENTARY_FLOW\""
            , "      },"
            , "      \"unit\": { \"@type\": \"Unit\", \"name\": \"m2*year\" }"
            , "    }"
            , "  ]"
            , "}"
            ]

-- | An openLCA Process document — must NOT be picked up as a method.
miniProcessJson :: BL.ByteString
miniProcessJson =
    BLC.pack "{ \"@type\": \"Process\", \"name\": \"not a method\" }"

spec :: Spec
spec = do
    describe "detectArchiveFormat on JSON blobs" $ do
        it "routes an openLCA ImpactCategory JSON to ArchivePlainJSON (regression: was ArchivePlainCSV)" $
            detectArchiveFormat miniImpactCategoryJson `shouldBe` ArchivePlainJSON

        it "leaves an unrelated JSON blob on the plain-text branch" $
            -- Process documents start with '{' too but lack the ImpactCategory
            -- marker, so the sniff must not over-fire.
            detectArchiveFormat miniProcessJson `shouldBe` ArchivePlainCSV

    describe "handleUpload writes JSON-LD as .json" $
        it "persists data.json (not data.csv) so the loader can dispatch through OlcaSchema" $
            withSystemTempDirectory "volca-method-upload" $ \tmp -> do
                let payload =
                        UploadData
                            { udName = "Test JSON-LD method"
                            , udDescription = Nothing
                            , udZipData = miniImpactCategoryJson
                            }
                result <- handleUpload tmp payload (\_ -> pure ())
                case result of
                    Left err -> expectationFailure ("upload failed: " ++ show err)
                    Right res -> do
                        urFormat res `shouldBe` OpenLcaJsonLd
                        -- Lock the side fix: the API response advertises the
                        -- detected format slug, not a hardcoded "ILCD".
                        formatToText (urFormat res) `shouldBe` "openlca-jsonld"
                        let slugDir = tmp </> "test-json-ld-method"
                        files <- listDirectory slugDir
                        files `shouldContain` ["data.json"]
                        doesFileExist (slugDir </> "data.csv") `shouldReturn` False

    describe "detectDatabaseFormat on a directory with a JSON-LD ImpactCategory" $
        it "returns OpenLcaJsonLd (covers the directory branch missed by the single-file test)" $
            withSystemTempDirectory "volca-method-detect" $ \tmp -> do
                let dir = tmp </> "method-dir"
                createDirectoryIfMissing True dir
                BL.writeFile (dir </> "impact-category.json") miniImpactCategoryJson
                detectDatabaseFormat dir `shouldReturn` OpenLcaJsonLd

    describe "loadMethodCollectionFromConfig on the uploaded JSON" $
        it "produces one Method with one CF carrying the fixture's value" $
            withSystemTempDirectory "volca-method-load" $ \tmp -> do
                let payload =
                        UploadData
                            { udName = "Test JSON-LD method"
                            , udDescription = Nothing
                            , udZipData = miniImpactCategoryJson
                            }
                Right res <- handleUpload tmp payload (\_ -> pure ())
                let mc =
                        MethodConfig
                            { mcName = "Test JSON-LD method"
                            , mcPath = urPath res
                            , mcActive = False
                            , mcIsUploaded = True
                            , mcDescription = Nothing
                            , mcFormat = Just "openlca-jsonld"
                            , mcScoringSets = []
                            , mcGlobalMethods = []
                            }
                loaded <- loadMethodCollectionFromConfig mc
                case loaded of
                    Left err -> expectationFailure ("load failed: " ++ show err)
                    Right (collection, _) -> do
                        let methods = mcMethods collection
                        length methods `shouldBe` 1
                        let factors = methodFactors (head methods)
                        length factors `shouldBe` 1
                        mcfValue (head factors) `shouldBe` 1.5
                        methodName (head methods) `shouldBe` "Test category"
                        fromMaybe "" (methodMethodology (head methods))
                            `shouldBe` "openLCA JSON-LD"
