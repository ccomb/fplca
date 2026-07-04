{-# LANGUAGE OverloadedStrings #-}

module CLISpec (spec) where

import Options.Applicative (
    ParserResult (..),
    defaultPrefs,
    execParserPure,
    renderFailure,
 )
import Test.Hspec

import CLI.Parser (cliParserInfo)
import CLI.Types

-- Parse argv and return either the parsed config (Right) or the failure summary (Left).
runParse :: [String] -> Either String CLIConfig
runParse argv =
    case execParserPure defaultPrefs cliParserInfo argv of
        Success cfg -> Right cfg
        Failure f -> Left (fst (renderFailure f "volca"))
        CompletionInvoked _ -> Left "completion-invoked"

-- Convenience: extract the Command from a successful parse (or fail loudly).
parseCmd :: [String] -> IO Command
parseCmd argv = case runParse argv of
    Right cfg -> case command cfg of
        Just c -> pure c
        Nothing -> expectationFailure "Expected a command, got Nothing" >> error "unreachable"
    Left err -> expectationFailure ("Parse failed: " <> err) >> error "unreachable"

spec :: Spec
spec = do
    describe "CLI.Types.parseOutputFormat" $ do
        let cases =
                [ ("json", Just JSON)
                , ("csv", Just CSV)
                , ("table", Just Table)
                , ("pretty", Just Pretty)
                , ("JSON", Just JSON) -- case-insensitive
                , ("Csv", Just CSV)
                , ("PRETTY", Just Pretty)
                , ("xml", Nothing) -- unknown format
                , ("", Nothing) -- empty string
                ]
        mapM_
            ( \(input, expected) ->
                it ("parses " <> show input <> " → " <> show expected) $
                    parseOutputFormat input `shouldBe` expected
            )
            cases

    describe "global options" $ do
        it "parses --config FILE" $ do
            case runParse ["--config", "volca.toml", "methods"] of
                Right cfg -> configFile (globalOptions cfg) `shouldBe` Just "volca.toml"
                Left err -> expectationFailure err

        it "parses --db NAME and --methods PATH" $ do
            case runParse ["--db", "ecoinvent", "--methods", "/m", "methods"] of
                Right cfg -> do
                    dbName (globalOptions cfg) `shouldBe` Just "ecoinvent"
                    methodsDir (globalOptions cfg) `shouldBe` Just "/m"
                Left err -> expectationFailure err

        it "parses --format pretty" $ do
            case runParse ["--format", "pretty", "methods"] of
                Right cfg -> format (globalOptions cfg) `shouldBe` Just Pretty
                Left err -> expectationFailure err

        it "rejects an unknown --format value with a non-zero exit" $ do
            case runParse ["--format", "yaml", "methods"] of
                Left _ -> pure ()
                Right _ -> expectationFailure "Expected parse failure on --format yaml"

        it "defaults noCache to False when --no-cache is absent" $ do
            case runParse ["methods"] of
                Right cfg -> noCache (globalOptions cfg) `shouldBe` False
                Left err -> expectationFailure err

        it "sets noCache to True when --no-cache is given" $ do
            case runParse ["--no-cache", "methods"] of
                Right cfg -> noCache (globalOptions cfg) `shouldBe` True
                Left err -> expectationFailure err

    describe "no command → load-only mode" $ do
        it "accepts an invocation with only global options" $ do
            case runParse ["--config", "volca.toml"] of
                Right cfg -> command cfg `shouldBe` Nothing
                Left err -> expectationFailure err

    describe "listing commands" $ do
        let listingCases =
                [ (["methods"], Methods)
                , (["synonyms"], Synonyms)
                , (["compartment-mappings"], CompartmentMappings)
                , (["units"], Units)
                , (["stop"], Stop)
                , (["repl"], Repl)
                ]
        mapM_
            ( \(argv, expected) ->
                it ("parses `" <> unwords argv <> "` → " <> show expected) $ do
                    cmd <- parseCmd argv
                    cmd `shouldBe` expected
            )
            listingCases

    describe "resource subcommands" $ do
        it "parses `database` with no subcommand → DbList" $ do
            cmd <- parseCmd ["database"]
            cmd `shouldBe` Database DbList

        it "parses `database list`" $ do
            cmd <- parseCmd ["database", "list"]
            cmd `shouldBe` Database DbList

        it "parses `database delete NAME`" $ do
            cmd <- parseCmd ["database", "delete", "ecoinvent"]
            cmd `shouldBe` Database (DbDelete "ecoinvent")

        it "parses `database upload FILE --name NAME`" $ do
            cmd <- parseCmd ["database", "upload", "db.7z", "--name", "My DB"]
            case cmd of
                Database (DbUpload args) -> do
                    uaFile args `shouldBe` "db.7z"
                    uaName args `shouldBe` "My DB"
                    uaDescription args `shouldBe` Nothing
                _ -> expectationFailure ("Unexpected command: " <> show cmd)

        it "parses `database upload FILE --name N --description D`" $ do
            cmd <- parseCmd ["database", "upload", "db.7z", "--name", "N", "--description", "D"]
            case cmd of
                Database (DbUpload args) -> uaDescription args `shouldBe` Just "D"
                _ -> expectationFailure ("Unexpected command: " <> show cmd)

        it "parses `method` with no subcommand → McList" $ do
            cmd <- parseCmd ["method"]
            cmd `shouldBe` Method McList

    describe "server command" $ do
        it "parses `server` with defaults" $ do
            cmd <- parseCmd ["server"]
            case cmd of
                Server opts -> do
                    serverPort opts `shouldBe` Nothing
                    serverIdleTimeout opts `shouldBe` 0
                    serverTreeDepth opts `shouldBe` 2
                    serverDesktopMode opts `shouldBe` False
                _ -> expectationFailure "Expected Server command"

        it "parses `server --port 9000 --idle-timeout 600 --tree-depth 5 --desktop`" $ do
            cmd <- parseCmd ["server", "--port", "9000", "--idle-timeout", "600", "--tree-depth", "5", "--desktop"]
            case cmd of
                Server opts -> do
                    serverPort opts `shouldBe` Just 9000
                    serverIdleTimeout opts `shouldBe` 600
                    serverTreeDepth opts `shouldBe` 5
                    serverDesktopMode opts `shouldBe` True
                _ -> expectationFailure "Expected Server command"

        it "parses `server --load db1,db2,db3` as a list" $ do
            cmd <- parseCmd ["server", "--load", "db1,db2,db3"]
            case cmd of
                Server opts -> serverLoadDbs opts `shouldBe` Just ["db1", "db2", "db3"]
                _ -> expectationFailure "Expected Server command"

    describe "resource queries" $ do
        it "parses `activity UUID`" $ do
            cmd <- parseCmd ["activity", "abc-123"]
            cmd `shouldBe` Activity "abc-123"

        it "parses `inventory UUID`" $ do
            cmd <- parseCmd ["inventory", "abc-123"]
            cmd `shouldBe` Inventory "abc-123"

        it "parses `flow FLOW_ID activities`" $ do
            cmd <- parseCmd ["flow", "flow-1", "activities"]
            cmd `shouldBe` Flow "flow-1" (Just FlowActivities)

        it "parses `impacts UUID --method MID`" $ do
            cmd <- parseCmd ["impacts", "abc-123", "--method", "method-uuid"]
            case cmd of
                Impacts uuid opts -> do
                    uuid `shouldBe` "abc-123"
                    lciaMethodId opts `shouldBe` "method-uuid"
                _ -> expectationFailure ("Unexpected command: " <> show cmd)

    describe "rejection" $ do
        it "rejects an unknown subcommand" $ do
            case runParse ["floop"] of
                Left _ -> pure ()
                Right _ -> expectationFailure "Expected parse failure on `floop`"

        it "rejects `database upload` without a FILE positional" $ do
            case runParse ["database", "upload", "--name", "X"] of
                Left _ -> pure ()
                Right _ -> expectationFailure "Expected parse failure (missing FILE)"

        it "rejects `database upload FILE` without --name" $ do
            case runParse ["database", "upload", "db.7z"] of
                Left _ -> pure ()
                Right _ -> expectationFailure "Expected parse failure (missing --name)"

        it "rejects `impacts UUID` without --method" $ do
            case runParse ["impacts", "abc"] of
                Left _ -> pure ()
                Right _ -> expectationFailure "Expected parse failure (missing --method)"
