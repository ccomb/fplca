{-# LANGUAGE OverloadedStrings #-}

module ProgressSpec (spec) where

import Test.Hspec
import Progress (formatDuration, formatBytes)

spec :: Spec
spec = do

    describe "formatDuration" $ do
        it "formats sub-millisecond durations with 2 decimal places" $
            formatDuration 0.0005 `shouldBe` "0.50ms"

        it "formats millisecond durations as integer ms" $
            formatDuration 0.5 `shouldBe` "500ms"

        it "formats second durations with 2 decimal places" $
            formatDuration 1.5 `shouldBe` "1.50s"

        it "formats exact 1.0s boundary as seconds" $
            formatDuration 1.0 `shouldBe` "1.00s"

        it "formats durations >= 60s as minutes" $
            formatDuration 90.0 `shouldBe` "1.5min"

        it "formats exactly 60s as 1.0min" $
            formatDuration 60.0 `shouldBe` "1.0min"

    describe "formatBytes" $ do
        it "formats bytes below 1024 as B" $
            formatBytes 512.0 `shouldBe` "512 B"

        it "formats exactly 1024 bytes as KB" $
            formatBytes 1024.0 `shouldBe` "1.0 KB"

        it "formats KB with one decimal place" $
            formatBytes 1536.0 `shouldBe` "1.5 KB"

        it "formats MB with one decimal place" $
            formatBytes (1.5 * 1024 * 1024) `shouldBe` "1.5 MB"

        it "formats GB with two decimal places" $
            formatBytes (1.5 * 1024 * 1024 * 1024) `shouldBe` "1.50 GB"

        it "formats zero bytes" $
            formatBytes 0.0 `shouldBe` "0 B"
