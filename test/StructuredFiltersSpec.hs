{-# LANGUAGE OverloadedStrings #-}

{- | The structured (geo, product, classification) filters applied on top of
name candidates. Pins the exact-match contract: @exact=true@ turns the
product filter into case-insensitive equality, matching what it already
meant for names and geographies.
-}
module StructuredFiltersSpec (spec) where

import Database (findActivitiesByFields)
import Test.Hspec
import TestHelpers (loadSampleDatabase)
import Types

spec :: Spec
spec = describe "exact product filter" $ do
    it "matches the full product name only" $ do
        db <- loadSampleDatabase "SAMPLE.min"
        let hits = findActivitiesByFields db Nothing Nothing (Just "product C") [] True
        map (activityName . snd) hits `shouldBe` ["production of product C"]

    it "is case-insensitive" $ do
        db <- loadSampleDatabase "SAMPLE.min"
        let hits = findActivitiesByFields db Nothing Nothing (Just "PRODUCT c") [] True
        map (activityName . snd) hits `shouldBe` ["production of product C"]

    it "rejects a partial product name" $ do
        db <- loadSampleDatabase "SAMPLE.min"
        let hits = findActivitiesByFields db Nothing Nothing (Just "product") [] True
        map (activityName . snd) hits `shouldBe` []

    it "combines with an exact name filter" $ do
        db <- loadSampleDatabase "SAMPLE.min"
        let hits = findActivitiesByFields db (Just "production of product C") Nothing (Just "product C") [] True
        map (activityName . snd) hits `shouldBe` ["production of product C"]
