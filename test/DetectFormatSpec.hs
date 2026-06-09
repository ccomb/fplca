{-# LANGUAGE OverloadedStrings #-}

module DetectFormatSpec (spec) where

import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Database.Manager (DirectoryFormat (..), detectDirectoryFormat)

spec :: Spec
spec = describe "detectDirectoryFormat" $ do
    it "detects EcoSpold2 when .spold datasets sit in a subdirectory beside a root CSV" $
        -- Native ecoinvent layout: datasets/*.spold plus a top-level
        -- FilenameToActivityLookup.csv that must not mask the .spold files
        -- (otherwise the package misdetects as SimaPro CSV and loads nothing).
        withSystemTempDirectory "ecospold-detect" $ \dir -> do
            createDirectoryIfMissing True (dir </> "datasets")
            writeFile (dir </> "datasets" </> "a_b.spold") "<x/>"
            writeFile (dir </> "FilenameToActivityLookup.csv") "Filename;ActivityName\n"
            detectDirectoryFormat dir `shouldReturn` FormatSpold

    it "still detects SimaPro CSV for a directory of only CSV files" $
        withSystemTempDirectory "csv-detect" $ \dir -> do
            writeFile (dir </> "export.csv") "{ SimaPro\n"
            detectDirectoryFormat dir `shouldReturn` FormatCSV
