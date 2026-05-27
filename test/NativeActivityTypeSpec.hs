{-# LANGUAGE OverloadedStrings #-}

-- | Roundtrip tests for the flat JSON encoding of NativeActivityType.
--   ToJSON flattens the three variants to a single-shape record; FromJSON
--   reconstructs the right variant from the `source` discriminator.
module NativeActivityTypeSpec (spec) where

import API.Types ()
import Data.Aeson (Result (..), Value (..), decode, encode, fromJSON, toJSON, (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import Test.Hspec
import Types (NativeActivityType (..))

spec :: Spec
spec = describe "NativeActivityType flat JSON encoding" $ do
    it "encodes EcoSpoldActivityType with all five fields populated" $ do
        let nat =
                EcoSpoldActivityType
                    { eatCode = 2
                    , eatLabel = "Market activity"
                    , eatSpecialCode = Just 1
                    , eatSpecialLabel = Just "Hard link"
                    }
        toJSON nat
            `shouldBe` A.object
                [ "source" .= ("ecospold2" :: String)
                , "label" .= ("Market activity" :: String)
                , "code" .= (2 :: Int)
                , "special_code" .= (1 :: Int)
                , "special_label" .= ("Hard link" :: String)
                ]

    it "encodes SimaProProcessType with code/special_* set to null" $ do
        let nat = SimaProProcessType{sptLabel = "Unit process"}
            obj = case toJSON nat of
                Object o -> o
                _ -> error "expected Object"
        KM.lookup "source" obj `shouldBe` Just "simapro"
        KM.lookup "label" obj `shouldBe` Just "Unit process"
        KM.lookup "code" obj `shouldBe` Just Null
        KM.lookup "special_code" obj `shouldBe` Just Null
        KM.lookup "special_label" obj `shouldBe` Just Null

    it "encodes ILCDProcessType with code/special_* set to null" $ do
        let nat = ILCDProcessType{iptLabel = "Unit process, single operation"}
            obj = case toJSON nat of
                Object o -> o
                _ -> error "expected Object"
        KM.lookup "source" obj `shouldBe` Just "ilcd"
        KM.lookup "label" obj `shouldBe` Just "Unit process, single operation"
        KM.lookup "code" obj `shouldBe` Just Null

    it "round-trips an EcoSpoldActivityType" $ do
        let original =
                EcoSpoldActivityType
                    { eatCode = 8
                    , eatLabel = "Market group"
                    , eatSpecialCode = Nothing
                    , eatSpecialLabel = Nothing
                    }
        decode (encode original) `shouldBe` Just original

    it "round-trips a SimaProProcessType" $ do
        let original = SimaProProcessType{sptLabel = "System"}
        decode (encode original) `shouldBe` Just original

    it "round-trips an ILCDProcessType" $ do
        let original = ILCDProcessType{iptLabel = "LCI result"}
        decode (encode original) `shouldBe` Just original

    it "rejects unknown source discriminator" $ do
        let bad =
                A.object
                    [ "source" .= ("openlca" :: String)
                    , "label" .= ("foo" :: String)
                    ]
        case fromJSON bad :: Result NativeActivityType of
            Success _ -> expectationFailure "expected parse failure for unknown source"
            Error _ -> pure ()
