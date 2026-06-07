module SynonymDBSpec (spec) where

import Test.Hspec

import SynonymDB (loadFromCSVFileWithCache)

spec :: Spec
spec =
    describe "loadFromCSVFileWithCache" $
        it "returns Left for a missing CSV instead of throwing" $ do
            -- Regression: the load used a bare readFile, so a missing
            -- reference file threw an uncaught IOException and took down
            -- server startup. The type promises Either; a missing file
            -- must surface as Left, like the other reference-data loaders.
            result <- loadFromCSVFileWithCache "test-data/does-not-exist-synonyms.csv"
            case result of
                Left _ -> pure ()
                Right _ -> expectationFailure "expected Left for a missing CSV file"
