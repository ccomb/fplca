{-# LANGUAGE OverloadedStrings #-}

{- | What the three tabs of a search box count.

The three are disjoint and together cover the database, so a query's three
counts partition what it matched. A counter that disagreed with the tab it
labels would be worse than no counter, which is why the process count is
taken from the very function the process list is built from.
-}
module SearchCountsSpec (spec) where

import Service (SearchCounts (..), searchCounts)
import Test.Hspec
import TestHelpers (loadSampleDatabase)

spec :: Spec
spec = describe "the counts behind a search box's tabs" $ do
    it "counts the processes a term matches" $ do
        db <- loadSampleDatabase "SAMPLE.ilcd"
        scProcesses (searchCounts db "Coal") `shouldBe` 1

    it "counts a product under products, not under flows" $ do
        -- SAMPLE.ilcd's outputs are technosphere flows, so a term matching
        -- them belongs to the middle tab and leaves the right-hand one empty.
        db <- loadSampleDatabase "SAMPLE.ilcd"
        let counts = searchCounts db "Coal"
        (scProducts counts > 0, scFlows counts) `shouldBe` (True, 0)

    it "answers zero everywhere for a term the database does not have" $ do
        db <- loadSampleDatabase "SAMPLE.ilcd"
        searchCounts db "zirconium" `shouldBe` SearchCounts 0 0 0
