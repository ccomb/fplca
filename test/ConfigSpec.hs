{-# LANGUAGE OverloadedStrings #-}

module ConfigSpec (spec) where

import Config (
    CFPatchOp (..),
    ClassificationEntry (..),
    ClassificationPreset (..),
    Config (..),
    DatabaseConfig (..),
    HostingConfig (..),
    Listen (..),
    MethodConfig (..),
    MethodPatch (..),
    MethodPatchMatch (..),
    RefDataConfig (..),
    ScoringSetConfig (..),
    ServerConfig (..),
    applyDataDir,
    clientHost,
    configKeys,
    defaultConfig,
    documentKeyPaths,
    expandClassificationPreset,
    keyPaths,
    listenOn,
    loadConfigOrDefault,
    redirectIntoDataDir,
    resolveConfigPaths,
    unknownKeys,
    validateConfig,
 )
import Data.Either (isRight)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.FilePath (normalise)
import qualified TOML
import Test.Hspec

serverOn :: Text -> ServerConfig
serverOn host =
    ServerConfig
        { scPort = 8080
        , scHost = host
        , scPassword = Nothing
        , scName = Nothing
        }

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
    describe "listenOn" $ do
        it "listens on the interface the configuration names" $
            listenOn Nothing (serverOn "0.0.0.0") `shouldBe` ListenOn "0.0.0.0" 8080

        -- Read through the real decoder rather than a hand-built record: what
        -- has to hold is the decoder's own fallback, since it is what a file
        -- with no host at all gets, and what a password left unset assumes.
        it "keeps a configuration that names no host on loopback" $
            case TOML.decode "port = 8080\n" :: Either TOML.TOMLError ServerConfig of
                Left err -> expectationFailure (show err)
                Right sc -> listenOn Nothing sc `shouldBe` ListenOn "127.0.0.1" 8080

        it "lets --port override the configured port without moving the interface" $
            listenOn (Just 9000) (serverOn "0.0.0.0") `shouldBe` ListenOn "0.0.0.0" 9000

        -- --port 0 goes through the free-port path, which binds loopback and
        -- takes no host, so the configured one cannot be honoured there.
        it "asks for a free loopback port whatever host the configuration names" $
            listenOn (Just 0) (serverOn "0.0.0.0") `shouldBe` ListenOnFreeLoopbackPort

    describe "clientHost" $ do
        -- A listening address names interfaces to accept on. Handing one to a
        -- client as a destination gives http://0.0.0.0:8080, which fails
        -- outright on Windows, or http://*:8080, which is not a URL at all.
        it "sends a client to this machine when the server accepts on every interface" $
            map clientHost ["0.0.0.0", "::", "*", "*4", "!4", "*6", "!6"]
                `shouldBe` replicate 7 "localhost"

        it "leaves an address that names one interface alone" $
            map clientHost ["127.0.0.1", "::1", "192.168.1.10", "engine.internal"]
                `shouldBe` ["127.0.0.1", "::1", "192.168.1.10", "engine.internal"]

    describe "unknownKeys" $ do
        let unread t = case TOML.decode t :: Either TOML.TOMLError TOML.Table of
                Left err -> ["did not parse: " <> T.pack (show err)]
                Right doc -> unknownKeys configKeys doc

        -- The one that matters: a key wrongly reported unread is a warning on
        -- a file that is perfectly good, which teaches the reader to ignore
        -- warnings. The shipped configuration exercises about a quarter of the
        -- schema, so the fixture below names the rest.
        it "reads every key of the configuration this repository ships" $ do
            shipped <- TIO.readFile "volca.toml"
            unread shipped `shouldBe` []

        it "reads every key a document can name" $ do
            everyKey <- TIO.readFile "test/data/every-config-key.toml"
            unread everyKey `shouldBe` []

        -- The other direction, so the fixture cannot quietly stop covering
        -- what it claims to: whatever the schema names, the fixture spells
        -- out. Without this, a key dropped from configKeys would go on being
        -- reported unread on every valid file and no test would notice.
        it "leaves no key of the schema unexercised" $ do
            everyKey <- TIO.readFile "test/data/every-config-key.toml"
            case TOML.decode everyKey :: Either TOML.TOMLError TOML.Table of
                Left err -> expectationFailure (show err)
                Right doc ->
                    -- Compared without the [] an array carries in a path: the
                    -- schema names keys, not how many of each a file holds.
                    let named = map (T.replace "[]" "") (documentKeyPaths doc)
                     in filter (`notElem` named) (keyPaths configKeys) `shouldBe` []

        -- How geographies went missing from the Docker image's own config: a
        -- top-level key written below a header belongs to that header.
        it "names a top-level key written under a section" $
            unread "[server]\nport = 8080\ngeographies = \"data/geographies.csv\"\n"
                `shouldBe` ["server.geographies"]

        it "names a key an array of tables does not carry" $
            unread "[[databases]]\nname = \"a\"\npath = \"a.zip\"\nactive = true\n"
                `shouldBe` ["databases[].active"]

        it "says once what twenty entries get wrong" $
            unread "[[databases]]\nname=\"a\"\npath=\"a\"\nactive=true\n[[databases]]\nname=\"b\"\npath=\"b\"\nactive=true\n"
                `shouldBe` ["databases[].active"]

        -- Scoring variables, computed formulas and location aliases are named
        -- by whoever writes the file, so no name there can be unknown.
        it "leaves the keys the file's author invents alone" $
            unread
                "[[methods]]\nname=\"EF\"\npath=\"x.zip\"\n\
                \[[methods.scoring]]\nname=\"ECS\"\n\
                \[methods.scoring.variables]\ncch = \"Climate change\"\n\
                \[methods.scoring.weighting]\ncch = 0.21\n"
                `shouldBe` []

        it "reaches a section nested two deep" $
            unread
                "[[methods]]\nname=\"EF\"\npath=\"x.zip\"\n\
                \[[methods.patches]]\nscale = 0.6\nmatch = { flow-name = \"Uranium\", flavour = \"x\" }\n"
                `shouldBe` ["methods[].patches[].match.flavour"]

    describe "expandClassificationPreset" $ do
        let raw =
                ClassificationPreset
                    { cpName = "raw"
                    , cpLabel = "Raw"
                    , cpDescription = Nothing
                    , cpFilters =
                        [ ClassificationEntry{ceSystem = "AGB", ceValue = "Agriculture", ceMode = "exact"}
                        , ClassificationEntry{ceSystem = "AGB", ceValue = "Food", ceMode = "contains"}
                        ]
                    }
        it "expands a configured preset into its filters" $
            expandClassificationPreset [raw] (Just "raw")
                `shouldBe` Right [("AGB", "Agriculture", True), ("AGB", "Food", False)]

        it "filters nothing when no preset was asked for" $
            expandClassificationPreset [raw] Nothing `shouldBe` Right []

        -- An unknown name used to expand to no filters at all, which turned a
        -- request for one slice of the database into a request for all of it.
        it "refuses an unknown name instead of widening the query" $
            case expandClassificationPreset [raw] (Just "transformed") of
                Right filters -> expectationFailure ("expected a refusal, got " <> show filters)
                Left err -> do
                    err `shouldSatisfy` T.isInfixOf "transformed"
                    err `shouldSatisfy` T.isInfixOf "raw"

        it "says so when the instance carries no preset at all" $
            case expandClassificationPreset [] (Just "raw") of
                Right filters -> expectationFailure ("expected a refusal, got " <> show filters)
                Left err -> err `shouldSatisfy` T.isInfixOf "no classification presets"

    describe "HostingConfig" $ do
        -- The [hosting] fragment is the one interface the operator actually
        -- touches; a typo in a key name here would silently drop their words.
        let decodeHosting t = TOML.decode t :: Either TOML.TOMLError HostingConfig
        it "parses read_only_message" $
            case decodeHosting "read_only = true\nread_only_message = \"Ask the operator.\"\n" of
                Right hc -> hcReadOnlyMessage hc `shouldBe` "Ask the operator."
                Left e -> expectationFailure (show e)
        it "defaults read_only_message to unset when the key is absent" $
            case decodeHosting "read_only = true\n" of
                Right hc -> hcReadOnlyMessage hc `shouldBe` ""
                Left e -> expectationFailure (show e)

    describe "validateConfig" $ do
        let preset name =
                ClassificationPreset
                    { cpName = name
                    , cpLabel = name
                    , cpDescription = Nothing
                    , cpFilters = []
                    }
            decodeMethod t = TOML.decode t :: Either TOML.TOMLError MethodConfig

        -- Presets and methods are looked up by name, so a duplicate would
        -- silently shadow one of its bearers; startup refuses it instead.
        it "refuses two classification presets sharing a name" $
            case validateConfig defaultConfig{cfgClassificationPresets = [preset "raw", preset "raw"]} of
                Right _ -> expectationFailure "expected a refusal"
                Left err -> do
                    err `shouldSatisfy` T.isInfixOf "Duplicate classification preset"
                    err `shouldSatisfy` T.isInfixOf "raw"

        it "refuses two method collections sharing a name" $
            case decodeMethod "name = \"EF\"\npath = \"x.zip\"\n" of
                Left e -> expectationFailure (show e)
                Right mc -> case validateConfig defaultConfig{cfgMethods = [mc, mc]} of
                    Right _ -> expectationFailure "expected a refusal"
                    Left err -> do
                        err `shouldSatisfy` T.isInfixOf "Duplicate method collection"
                        err `shouldSatisfy` T.isInfixOf "EF"

        it "accepts distinct names" $
            case decodeMethod "name = \"EF\"\npath = \"x.zip\"\n" of
                Left e -> expectationFailure (show e)
                Right mc ->
                    validateConfig
                        defaultConfig
                            { cfgClassificationPresets = [preset "raw", preset "transformed"]
                            , cfgMethods = [mc]
                            }
                        `shouldSatisfy` isRight

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

    describe "resolveConfigPaths" $ do
        let decodeDb t = TOML.decode t :: Either TOML.TOMLError DatabaseConfig
            decodeMethod t = TOML.decode t :: Either TOML.TOMLError MethodConfig
            withParsed body =
                case (decodeDb "name = \"agb\"\npath = \"agb.CSV\"\n", decodeMethod "name = \"EF\"\npath = \"ef.zip\"\n") of
                    (Right db, Right method) -> body db method
                    (Left e, _) -> expectationFailure (show e)
                    (_, Left e) -> expectationFailure (show e)

        it "prefixes every relative path with the config file's directory" $
            withParsed $ \db method -> do
                let cfg =
                        defaultConfig
                            { cfgDatabases = [db]
                            , cfgMethods = [method]
                            , cfgFlowSynonyms = [mkRef "flows.csv"]
                            , cfgCompartmentMappings = [mkRef "compartments.csv"]
                            , cfgUnits = [mkRef "units.csv"]
                            , cfgEnergyDensities = [mkRef "energy.csv"]
                            , cfgGeographies = Just "geographies.csv"
                            , cfgChemSynonyms = Just "chem.csv"
                            , cfgSubstanceEdges = Just "edges.csv"
                            }
                    resolved = resolveConfigPaths (Just "/etc/volca/volca.toml") cfg
                -- A method path used to follow the process while the database
                -- path beside it followed the file. They move together now.
                -- Expected values go through 'normalise' too: on Windows the
                -- resolver emits backslashes.
                map dcPath (cfgDatabases resolved) `shouldBe` [normalise "/etc/volca/agb.CSV"]
                map mcPath (cfgMethods resolved) `shouldBe` [normalise "/etc/volca/ef.zip"]
                map rdPath (cfgFlowSynonyms resolved) `shouldBe` [normalise "/etc/volca/flows.csv"]
                map rdPath (cfgCompartmentMappings resolved) `shouldBe` [normalise "/etc/volca/compartments.csv"]
                map rdPath (cfgUnits resolved) `shouldBe` [normalise "/etc/volca/units.csv"]
                map rdPath (cfgEnergyDensities resolved) `shouldBe` [normalise "/etc/volca/energy.csv"]
                cfgGeographies resolved `shouldBe` Just (normalise "/etc/volca/geographies.csv")
                cfgChemSynonyms resolved `shouldBe` Just (normalise "/etc/volca/chem.csv")
                cfgSubstanceEdges resolved `shouldBe` Just (normalise "/etc/volca/edges.csv")

        it "leaves an absolute path alone" $
            withParsed $ \_ method -> do
                let cfg = defaultConfig{cfgMethods = [method{mcPath = "/srv/methods/ef.zip"}]}
                map mcPath (cfgMethods (resolveConfigPaths (Just "/etc/volca/volca.toml") cfg))
                    `shouldBe` [normalise "/srv/methods/ef.zip"]

        it "falls back to the process directory when there is no config file" $
            withParsed $ \_ method -> do
                let cfg = defaultConfig{cfgMethods = [method]}
                map mcPath (cfgMethods (resolveConfigPaths Nothing cfg)) `shouldBe` ["ef.zip"]

        it "keeps the shipped data bundle applyDataDir already pointed at" $ do
            -- applyDataDir runs first and turns "data/x" into an absolute path;
            -- resolving after it must not prefix that a second time.
            let cfg = defaultConfig{cfgFlowSynonyms = [mkRef "data/flows.csv"]}
                bundled = applyDataDir (Just "/opt/volca/data") cfg
            map rdPath (cfgFlowSynonyms (resolveConfigPaths (Just "/etc/volca/volca.toml") bundled))
                `shouldBe` [normalise "/opt/volca/data/flows.csv"]

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
