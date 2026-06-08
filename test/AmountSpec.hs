{-# LANGUAGE OverloadedStrings #-}

module AmountSpec (spec) where

import Amount (readAmount)
import qualified Data.Text as T
import EcoSpold.Common (showFFloatTrim)
import Test.Hspec

spec :: Spec
spec = describe "Amount.readAmount" $ do
    describe "correct rounding (the inverse of showFFloatTrim)" $ do
        it "recovers a value Data.Text.Read.double rounds off by a ULP" $
            -- 0.0000010897906999999999 is 1.0897906999999999e-6 exactly; the old
            -- reader parsed it to 1.0897907e-6, a different Double.
            readAmount "0.0000010897906999999999" `shouldBe` Just 1.0897906999999999e-6

        it "round-trips every finite magnitude through showFFloatTrim, subnormals included" $
            mapM_
                roundTrips
                [ 1.0897906999999999e-6
                , 3.3e-20
                , 5.0e-324 -- smallest positive subnormal
                , 4.9e-324
                , 1.0e308
                , 123456.789
                , -2.5
                , 0
                ]

    describe "grammar (a superset of Data.Text.Read.double)" $ do
        it "accepts a bare leading or trailing decimal point and a leading plus" $ do
            readAmount ".5" `shouldBe` Just 0.5
            readAmount "1." `shouldBe` Just 1.0
            readAmount "+1.5" `shouldBe` Just 1.5
            readAmount "-.5" `shouldBe` Just (-0.5)
        it "accepts surrounding whitespace and scientific notation" $ do
            readAmount " 7.0 " `shouldBe` Just 7.0
            readAmount "1.5E-6" `shouldBe` Just 1.5e-6

    describe "rejection (surfaced rather than silently mis-parsed)" $ do
        it "rejects the non-finite literals" $ do
            readAmount "Infinity" `shouldBe` Nothing
            readAmount "NaN" `shouldBe` Nothing
        it "rejects an overflowing literal rather than admitting Infinity" $
            readAmount "1e400" `shouldBe` Nothing
        it "rejects trailing garbage and non-numbers" $ do
            readAmount "1.5abc" `shouldBe` Nothing
            readAmount "2*3" `shouldBe` Nothing
            readAmount "" `shouldBe` Nothing
  where
    roundTrips x = readAmount (T.pack (showFFloatTrim x)) `shouldBe` Just x
