{-# LANGUAGE OverloadedStrings #-}

module SynonymDBSpec (spec) where

import qualified Data.Set as S
import Data.Text (pack)
import Test.Hspec

import SynonymDB (buildFromPairs, getSynonyms, loadFromCSVFileWithCache, lookupSynonymGroup, oversizedClasses)

spec :: Spec
spec = do
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

    describe "buildFromPairs transitive closure" $ do
        let db = buildFromPairs [("alpha", "beta"), ("beta", "gamma")]
            classFor name = S.fromList <$> (lookupSynonymGroup db name >>= getSynonyms db)

        it "groups chained synonyms (a=b, b=c) into one class, reachable from either end" $
            classFor "alpha" `shouldBe` Just (S.fromList ["alpha", "beta", "gamma"])

        it "gives both ends of the chain the same group id" $
            lookupSynonymGroup db "alpha" `shouldBe` lookupSynonymGroup db "gamma"

    describe "oversizedClasses" $ do
        -- A junk hub fuses everything it touches into one transitive class.
        let hub = buildFromPairs [("hub", "s" <> pack (show i)) | i <- [1 :: Int .. 12]]

        it "flags a class larger than the bound (a closure that fused a junk hub)" $
            map length (oversizedClasses 10 hub) `shouldBe` [13]

        it "stays silent when every class is within the bound" $
            oversizedClasses 10 (buildFromPairs [("alpha", "beta"), ("beta", "gamma")])
                `shouldBe` []
