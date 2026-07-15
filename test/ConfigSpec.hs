{-# LANGUAGE OverloadedStrings #-}

module ConfigSpec (spec) where

import Config (
    CFPatchOp (..),
    Config (..),
    MethodConfig (..),
    MethodPatch (..),
    MethodPatchMatch (..),
    RefDataConfig (..),
    ScoringSetConfig (..),
    applyDataDir,
    defaultConfig,
    loadConfigOrDefault,
    redirectIntoDataDir,
 )
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified TOML
import Test.Hspec

mkRef :: FilePath -> RefDataConfig
mkRef p =
    RefDataConfig
        { rdName = "test"
        , rdPath = p
        , rdActive = True
        , rdIsUploaded = False
        , rdIsAuto = False
        , rdDescription = Nothing
        }

spec :: Spec
spec = do
    describe "MethodConfig global-methods" $ do
        let decodeMethod t = TOML.decode t :: Either TOML.TOMLError MethodConfig
        it "parses the global-methods list" $
            case decodeMethod "name = \"EF\"\npath = \"x.zip\"\nglobal-methods = [\"Land use\"]\n" of
                Right mc -> mcGlobalMethods mc `shouldBe` ["Land use"]
                Left e -> expectationFailure (show e)
        it "defaults global-methods to empty when the key is absent" $
            case decodeMethod "name = \"EF\"\npath = \"x.zip\"\n" of
                Right mc -> mcGlobalMethods mc `shouldBe` []
                Left e -> expectationFailure (show e)

    describe "MethodConfig patches" $ do
        let decodeMethod t = TOML.decode t :: Either TOML.TOMLError MethodConfig

        it "defaults patches to empty when the key is absent" $
            case decodeMethod "name = \"EF\"\npath = \"x.zip\"\n" of
                Right mc -> mcPatches mc `shouldBe` []
                Left e -> expectationFailure (show e)

        it "parses a scale patch with a category + flow-name-prefix selector" $
            case decodeMethod
                "name = \"EF\"\npath = \"x.zip\"\n\n\
                \[[patches]]\n\
                \description = \"uraniumFRU\"\n\
                \match = { category = \"Resource use, fossils\", flow-name-prefix = \"Uranium\" }\n\
                \scale = 0.6\n" of
                Right mc -> case mcPatches mc of
                    [patch] -> do
                        mpDescription patch `shouldBe` Just "uraniumFRU"
                        mpmCategory (mpMatch patch) `shouldBe` Just "Resource use, fossils"
                        mpmFlowNamePrefix (mpMatch patch) `shouldBe` Just "Uranium"
                        mpOp patch `shouldBe` ScaleBy 0.6
                    ps -> expectationFailure ("expected exactly one patch, got " <> show (length ps))
                Left e -> expectationFailure (show e)

        it "parses a set-value patch with a subcompartment-contains selector" $
            case decodeMethod
                "name = \"EF\"\npath = \"x.zip\"\n\n\
                \[[patches]]\n\
                \match = { subcompartment-contains = \"long-term\" }\n\
                \set-value = 0.0\n" of
                Right mc -> case mcPatches mc of
                    [patch] -> do
                        mpmSubcompartmentContains (mpMatch patch) `shouldBe` Just "long-term"
                        mpOp patch `shouldBe` SetValueTo 0.0
                    ps -> expectationFailure ("expected exactly one patch, got " <> show (length ps))
                Left e -> expectationFailure (show e)

        it "rejects a patch with both scale and set-value" $
            case decodeMethod
                "name = \"EF\"\npath = \"x.zip\"\n\n\
                \[[patches]]\n\
                \match = { flow-name = \"Uranium\" }\n\
                \scale = 0.6\n\
                \set-value = 0.0\n" of
                Left _ -> pure ()
                Right _ -> expectationFailure "expected a decode error for scale + set-value together"

        it "rejects a patch with neither scale nor set-value" $
            case decodeMethod
                "name = \"EF\"\npath = \"x.zip\"\n\n\
                \[[patches]]\n\
                \match = { flow-name = \"Uranium\" }\n" of
                Left _ -> pure ()
                Right _ -> expectationFailure "expected a decode error when neither scale nor set-value is set"

        it "rejects a patch whose selector matches every CF" $
            case decodeMethod
                "name = \"EF\"\npath = \"x.zip\"\n\n\
                \[[patches]]\n\
                \match = {}\n\
                \scale = 0.6\n" of
                Left _ -> pure ()
                Right _ -> expectationFailure "expected a decode error for an empty selector"

    describe "redirectIntoDataDir" $ do
        it "leaves paths unchanged when VOLCA_DATA_DIR is unset" $
            redirectIntoDataDir Nothing "data/flows.csv" `shouldBe` "data/flows.csv"

        it "redirects unix-style data/ prefix to the env-var dir" $
            redirectIntoDataDir (Just "/opt/volca-data/v1") "data/flows.csv"
                `shouldBe` "/opt/volca-data/v1/flows.csv"

        it "redirects windows-style data\\ prefix the same way" $
            redirectIntoDataDir (Just "/opt/volca-data/v1") "data\\flows.csv"
                `shouldBe` "/opt/volca-data/v1/flows.csv"

        it "leaves non-data paths alone (user databases must not be redirected)" $
            redirectIntoDataDir (Just "/opt/volca-data/v1") "DBs/agribalyse.7z"
                `shouldBe` "DBs/agribalyse.7z"

        it "leaves absolute paths alone even if they happen to start with 'data'" $
            redirectIntoDataDir (Just "/opt/volca-data/v1") "/etc/data/flows.csv"
                `shouldBe` "/etc/data/flows.csv"

    describe "applyDataDir" $ do
        let cfg =
                defaultConfig
                    { cfgGeographies = Just "data/geographies.csv"
                    , cfgFlowSynonyms = [mkRef "data/flows.csv"]
                    , cfgCompartmentMappings = [mkRef "data/compartments.csv"]
                    , cfgUnits = [mkRef "data/units.csv"]
                    }

        it "rewrites every reference-data path when the env var is set" $ do
            let resolved = applyDataDir (Just "/d") cfg
            cfgGeographies resolved `shouldBe` Just "/d/geographies.csv"
            map rdPath (cfgFlowSynonyms resolved) `shouldBe` ["/d/flows.csv"]
            map rdPath (cfgCompartmentMappings resolved) `shouldBe` ["/d/compartments.csv"]
            map rdPath (cfgUnits resolved) `shouldBe` ["/d/units.csv"]

        it "is a no-op when the env var is unset" $
            applyDataDir Nothing cfg `shouldBe` cfg

    describe "loadConfigOrDefault" $ do
        it "yields the validated defaults when no path is given" $ do
            result <- loadConfigOrDefault Nothing
            case result of
                Right cfg -> cfgDatabases cfg `shouldBe` []
                Left err -> expectationFailure (show err)

        it "still fails loudly on an explicit path that does not exist" $ do
            result <- loadConfigOrDefault (Just "/nonexistent/volca.toml")
            case result of
                Left err -> err `shouldSatisfy` ("Config file not found" `T.isPrefixOf`)
                Right _ -> expectationFailure "expected a missing explicit config to fail"

    describe "ScoringSetConfig labels" $ do
        let decodeSet :: Text -> Either TOML.TOMLError ScoringSetConfig
            decodeSet = TOML.decode

        it "accepts a label on a computed variable" $ do
            let toml =
                    "name = \"ECS\"\n\
                    \[computed]\n\
                    \etf = \"2 * etfo + etfi\"\n\
                    \[labels]\n\
                    \etf = \"Ecotoxicity, freshwater\"\n"
            fmap sscLabels (decodeSet toml)
                `shouldBe` Right (M.singleton "etf" "Ecotoxicity, freshwater")

        it "accepts a label on a primitive variable" $ do
            let toml =
                    "name = \"ECS\"\n\
                    \[variables]\n\
                    \cch = \"Climate change\"\n\
                    \[labels]\n\
                    \cch = \"Changement climatique\"\n"
            fmap sscLabels (decodeSet toml)
                `shouldBe` Right (M.singleton "cch" "Changement climatique")

        it "rejects a label whose key matches no scoring variable" $ do
            let toml =
                    "name = \"ECS\"\n\
                    \[computed]\n\
                    \etf = \"2 * etfo + etfi\"\n\
                    \[labels]\n\
                    \eft = \"Ecotoxicity, freshwater\"\n"
            case decodeSet toml of
                Right _ -> expectationFailure "orphan label key must be rejected"
                Left err -> show err `shouldContain` "eft"
