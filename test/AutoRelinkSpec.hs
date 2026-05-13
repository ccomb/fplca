{-# LANGUAGE OverloadedStrings #-}

{- | Regression tests for the auto-relink-on-Load behavior.

The PR these tests guard introduced two related changes:

  1. 'loadDatabaseRawWithCrossDB' returns @(Database, Bool)@ where the
     'Bool' is True iff the result came straight from the matrix cache
     (i.e. cross-DB linking was NOT freshly run against 'otherIndexes').
  2. 'loadDatabaseSingleFromConfig' uses that flag to skip the no-op
     self-relink on fresh parses.

The 'fromCache' flag is the contract these tests pin down. The end-to-end
dep-set-swap scenario (load consumer with deps A → swap to B → reload
consumer → links repointed) is still covered by the PR's manual test plan;
fully automating it requires multi-DB cross-link fixtures and is left to a
follow-up.
-}
module AutoRelinkSpec (spec) where

import Control.Monad (forM_)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import System.Directory (
    copyFile,
    createDirectoryIfMissing,
    doesFileExist,
    listDirectory,
 )
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Database.Manager (loadDatabaseRawWithCrossDB)
import SynonymDB (emptySynonymDB)
import UnitConversion (defaultUnitConfig)

-- | Copy regular files from one directory into another (non-recursive,
-- which is all the EcoSpold v2 fixtures here need).
copyDirContents :: FilePath -> FilePath -> IO ()
copyDirContents src dst = do
    createDirectoryIfMissing True dst
    files <- listDirectory src
    forM_ files $ \f -> copyFile (src </> f) (dst </> f)

-- | Drive 'loadDatabaseRawWithCrossDB' with the inert defaults that this
-- test cares about. Cross-DB linking is not exercised; we only need the
-- cache-hit detection.
runRaw :: FilePath -> IO (Either T.Text Bool)
runRaw dstDir = do
    result <-
        loadDatabaseRawWithCrossDB
            "test"
            M.empty
            dstDir
            False -- noCache disabled: cache must be written/read
            emptySynonymDB
            defaultUnitConfig
            []
            M.empty
    return (fmap snd result)

spec :: Spec
spec = do
    describe "loadDatabaseRawWithCrossDB cache-hit flag" $ do
        it "returns fromCache=False on a fresh parse and writes the cache" $
            withSystemTempDirectory "volca-relink" $ \tmp -> do
                let dstDir = tmp </> "sample"
                copyDirContents "test-data/SAMPLE.min1" dstDir

                -- Cache file lives next to sourcePath (in 'tmp', because
                -- takeDirectory dstDir == tmp).
                let cacheFile = tmp </> "volca.cache.test.bin.zst"
                doesFileExist cacheFile `shouldReturn` False

                r1 <- runRaw dstDir
                r1 `shouldBe` Right False

                doesFileExist cacheFile `shouldReturn` True

        it "returns fromCache=True on a second load against the same path" $
            withSystemTempDirectory "volca-relink" $ \tmp -> do
                let dstDir = tmp </> "sample"
                copyDirContents "test-data/SAMPLE.min1" dstDir

                -- Prime the cache.
                _ <- runRaw dstDir

                -- Second call: must come back from the cache.
                r2 <- runRaw dstDir
                r2 `shouldBe` Right True
