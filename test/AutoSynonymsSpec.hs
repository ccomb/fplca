{-# LANGUAGE OverloadedStrings #-}

{- | Locks the runtime contract of auto-extracted flow synonyms: they are a
persisted, opt-in CANDIDATE set for offline curation — the engine registers
them but never feeds them into flow matching. Flow matching trusts only the
curated registry plus explicitly activated sources.
-}
module AutoSynonymsSpec (spec) where

import Control.Concurrent.STM (readTVarIO)
import Control.Exception (finally)
import qualified Data.Map.Strict as M
import System.Directory (doesFileExist, getCurrentDirectory, setCurrentDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Config (defaultConfig)
import Database.Manager (
    CachePolicy (..),
    DatabaseLoadStatus (..),
    DatabaseManager (..),
    RefDataStatus (..),
    SynonymOrigin (..),
    autoCreateFlowSynonyms,
    initDatabaseManager,
    listFlowSynonyms,
 )

{- | Run an action with a fresh manager in a temp working directory, so the
uploads/ tree the extraction persists to never touches the repo.
-}
withTempManager :: (DatabaseManager -> IO a) -> IO a
withTempManager action =
    withSystemTempDirectory "volca-auto-syns" $ \tmp -> do
        oldCwd <- getCurrentDirectory
        setCurrentDirectory tmp
        (initDatabaseManager defaultConfig NoCache >>= action)
            `finally` setCurrentDirectory oldCwd

spec :: Spec
spec = describe "autoCreateFlowSynonyms" $ do
    it "persists and registers the candidate set but never loads it into matching" $
        withTempManager $ \manager -> do
            autoCreateFlowSynonyms manager "test-method" (SynonymOrigin "desc") [("alpha", "beta")]
            loaded <- readTVarIO (dmLoadedFlowSyns manager)
            M.member "auto-test-method" loaded `shouldBe` False
            statuses <- listFlowSynonyms manager
            let entry = [s | s <- statuses, rdsName s == "auto-test-method"]
            map rdsStatus entry `shouldBe` [Unloaded]
            map rdsIsAuto entry `shouldBe` [True]
            doesFileExist ("uploads/flow-synonyms" </> "auto-test-method" </> "data.csv")
                `shouldReturn` True

    it "skips re-extraction when the candidate is already registered" $
        withTempManager $ \manager -> do
            autoCreateFlowSynonyms manager "test-method" (SynonymOrigin "desc") [("alpha", "beta")]
            autoCreateFlowSynonyms manager "test-method" (SynonymOrigin "desc") [("gamma", "delta")]
            -- The persisted CSV must still hold the first extraction: registry
            -- membership alone cannot tell a skip from a silent overwrite.
            readFile ("uploads/flow-synonyms" </> "auto-test-method" </> "data.csv")
                `shouldReturn` "name1,name2\nalpha,beta\n"
