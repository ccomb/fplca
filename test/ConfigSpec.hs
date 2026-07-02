{-# LANGUAGE OverloadedStrings #-}

module ConfigSpec (spec) where

import Config (
    Config (..),
    MethodConfig (..),
    RefDataConfig (..),
    ScoringSetConfig (..),
    applyDataDir,
    defaultConfig,
    redirectIntoDataDir,
 )
import qualified Data.Map.Strict as M
import Data.Text (Text)
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
