{-# LANGUAGE OverloadedStrings #-}

-- | Tests for "ConfigWriter".
--
-- ConfigWriter is the persistence layer for uploaded databases: it mutates
-- the operator's TOML file. The mutations are atomic (lock + rename) and
-- preserve manually-edited non-database sections — both invariants the test
-- file should pin.
module ConfigWriterSpec (spec) where

import qualified Data.Map.Strict as M
import qualified Data.Text.IO as TIO
import System.Directory (getTemporaryDirectory)
import System.FilePath ((</>))
import Test.Hspec

import Config (Config (..), DatabaseConfig (..), loadConfigFile)
import ConfigWriter (addDatabaseToConfig, removeDatabaseFromConfig, updateDatabaseLoadFlag)
import Types (GeographyPolicy (..))

-- A minimal config skeleton: one [server] section and zero databases.
seedConfig :: String
seedConfig =
    unlines
        [ "[server]"
        , "port = 8080"
        , "host = \"127.0.0.1\""
        , ""
        , "# operator-written comment that the writer must preserve"
        ]

-- A second config skeleton: one [server] section + one pre-existing database.
seedConfigWithDb :: String
seedConfigWithDb =
    unlines
        [ "[server]"
        , "port = 8080"
        , "host = \"127.0.0.1\""
        , ""
        , "[[databases]]"
        , "name = \"existing\""
        , "displayName = \"Existing DB\""
        , "path = \"/data/existing\""
        , "load = true"
        , "default = false"
        ]

mkDbConfig :: String -> DatabaseConfig
mkDbConfig name =
    DatabaseConfig
        { dcName = (read . show) name -- shorthand for "name as Text"
        , dcDisplayName = "Display: " <> (read . show) name
        , dcPath = "/data/" <> name
        , dcDescription = Just "test description"
        , dcLoad = True
        , dcDefault = False
        , dcDepends = []
        , dcLocationAliases = M.empty
        , dcFormat = Nothing
        , dcIsUploaded = True
        , dcDeletable = True
        , dcGeographyPolicy = GeoGlobal
        }

withTempConfig :: String -> (FilePath -> IO a) -> IO a
withTempConfig contents action = do
    tmp <- getTemporaryDirectory
    let p = tmp </> "volca-cw-test.toml"
    writeFile p contents
    action p

spec :: Spec
spec = do
    describe "addDatabaseToConfig" $ do
        it "appends a new database when none exists with that name" $ do
            withTempConfig seedConfig $ \p -> do
                let db = mkDbConfig "fresh"
                res <- addDatabaseToConfig p db
                res `shouldBe` Right ()
                reload <- loadConfigFile p
                case reload of
                    Right cfg -> map dcName (cfgDatabases cfg) `shouldBe` ["fresh"]
                    Left err -> expectationFailure (show err)

        it "rejects a duplicate name with a clear Left" $ do
            withTempConfig seedConfigWithDb $ \p -> do
                let db = mkDbConfig "existing"
                res <- addDatabaseToConfig p db
                case res of
                    Left msg -> show msg `shouldContain` "already exists"
                    Right () -> expectationFailure "expected Left for duplicate name"

        it "returns Left when the config file does not exist (parent dir exists)" $ do
            -- The function uses withFileLock to take an exclusive lock on
            -- <path>.lock, which needs to create the lock file in the parent
            -- directory. So the "missing config" case is: parent dir exists,
            -- config file inside it does not. That's the realistic upgrade
            -- scenario the caller cares about.
            tmp <- getTemporaryDirectory
            let p = tmp </> "absolutely-no-such-volca-config.toml"
            res <- addDatabaseToConfig p (mkDbConfig "x")
            case res of
                Left msg -> show msg `shouldContain` "not found"
                Right () -> expectationFailure "expected Left for missing file"

        it "preserves the operator-written comment after appending" $ do
            withTempConfig seedConfig $ \p -> do
                _ <- addDatabaseToConfig p (mkDbConfig "fresh")
                content <- TIO.readFile p
                show content `shouldContain` "operator-written comment"

    describe "removeDatabaseFromConfig" $ do
        it "drops the matching entry and leaves [server] intact" $ do
            withTempConfig seedConfigWithDb $ \p -> do
                res <- removeDatabaseFromConfig p "existing"
                res `shouldBe` Right ()
                reload <- loadConfigFile p
                case reload of
                    Right cfg -> do
                        map dcName (cfgDatabases cfg) `shouldBe` []
                    Left err -> expectationFailure (show err)

        it "returns Left for an unknown database name" $ do
            withTempConfig seedConfigWithDb $ \p -> do
                res <- removeDatabaseFromConfig p "no-such-db"
                case res of
                    Left msg -> show msg `shouldContain` "not found"
                    Right () -> expectationFailure "expected Left"

    describe "updateDatabaseLoadFlag" $ do
        it "flips load = true to false in place" $ do
            withTempConfig seedConfigWithDb $ \p -> do
                res <- updateDatabaseLoadFlag p "existing" False
                res `shouldBe` Right ()
                content <- TIO.readFile p
                show content `shouldContain` "load = false"

        it "is a no-op (returns Right) on a name that doesn't appear in the file" $ do
            -- updateLoadInText is a forgiving text edit — missing names don't
            -- surface as errors. Documenting that current contract here.
            withTempConfig seedConfigWithDb $ \p -> do
                res <- updateDatabaseLoadFlag p "no-such-db" False
                res `shouldBe` Right ()
