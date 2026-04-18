{-# LANGUAGE OverloadedStrings #-}

module NormalizeSpec (spec) where

import Search.Normalize (tokenize)
import Test.Hspec

spec :: Spec
spec = describe "Search.Normalize.tokenize" $ do
    it "strips diacritics and lowercases" $
        tokenize "Haché" `shouldBe` ["hache"]

    it "splits on punctuation and whitespace" $
        tokenize "Steak haché, à la poêle" `shouldBe` ["steak", "hache", "a", "la", "poele"]

    it "handles empty input" $
        tokenize "" `shouldBe` []

    it "handles combining marks and mixed punctuation" $
        tokenize "Bœuf CRU  —  à l'étuvée" `shouldBe` ["bœuf", "cru", "a", "l", "etuvee"]
