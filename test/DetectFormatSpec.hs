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

    -- The detector knew four formats while the loader read five, so an
    -- uploaded workbook extracted into a directory came back "No supported
    -- database files found" naming the four it did know.
    it "detects a Brightway workbook sitting in a directory" $
        withSystemTempDirectory "xlsx-detect" $ \dir -> do
            writeFile (dir </> "inventory.xlsx") "PK\003\004"
            detectDirectoryFormat dir `shouldReturn` FormatExcel

    it "detects a workbook handed over as the path itself" $
        withSystemTempDirectory "xlsx-file-detect" $ \dir -> do
            let file = dir </> "inventory.xlsx"
            writeFile file "PK\003\004"
            detectDirectoryFormat file `shouldReturn` FormatExcel

    -- .spold beats a workbook the same way it beats a CSV: an EcoSpold package
    -- may ship a spreadsheet beside its datasets.
    it "keeps EcoSpold2 ahead of a workbook at the package root" $
        withSystemTempDirectory "xlsx-vs-spold" $ \dir -> do
            createDirectoryIfMissing True (dir </> "datasets")
            writeFile (dir </> "datasets" </> "a_b.spold") "<x/>"
            writeFile (dir </> "summary.xlsx") "PK\003\004"
            detectDirectoryFormat dir `shouldReturn` FormatSpold

    it "names every format it knows when it recognises none of them" $
        withSystemTempDirectory "unknown-detect" $ \dir -> do
            writeFile (dir </> "notes.txt") "nothing to load here\n"
            detectDirectoryFormat dir `shouldReturn` FormatUnknown
