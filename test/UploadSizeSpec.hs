{-# LANGUAGE OverloadedStrings #-}

module UploadSizeSpec (spec) where

import API.DatabaseHandlers (checkUploadSize, uploadBodyCeiling)
import Config (HostingConfig (..))
import Data.Either (isLeft)
import Data.Word (Word64)
import Test.Hspec

-- | A hosting config with the given upload-size limit (in MB); other fields are irrelevant here.
hostingWithLimit :: Int -> HostingConfig
hostingWithLimit limitMb =
    HostingConfig
        { hcMaxUploads = -1
        , hcMaxUploadMb = limitMb
        , hcApiAccess = True
        , hcUpgradeUpload = ""
        , hcUpgradeApi = ""
        , hcUpgradeVmSize = ""
        }

mb :: Int -> Int
mb n = n * 1024 * 1024

spec :: Spec
spec = do
    describe "checkUploadSize" $ do
        it "allows any size with no hosting config (local / CLI / desktop)" $
            checkUploadSize Nothing (mb 5000) `shouldBe` Right ()

        it "is unlimited when the configured limit is negative" $
            checkUploadSize (Just (hostingWithLimit (-1))) (mb 5000) `shouldBe` Right ()

        it "disables uploads entirely when the limit is zero" $
            checkUploadSize (Just (hostingWithLimit 0)) 1
                `shouldBe` Left "Uploads are disabled on this plan."

        it "accepts a file exactly at the limit" $
            checkUploadSize (Just (hostingWithLimit 100)) (mb 100) `shouldBe` Right ()

        it "rejects a file one byte over the limit" $
            checkUploadSize (Just (hostingWithLimit 100)) (mb 100 + 1) `shouldSatisfy` isLeft

    describe "uploadBodyCeiling" $ do
        let dbUpload = ["api", "v1", "db", "upload"]
            methodUpload = ["api", "v1", "method-collections", "upload"]

        it "does not bound non-upload paths" $
            uploadBodyCeiling (Just (hostingWithLimit 100)) ["api", "v1", "search"] `shouldBe` Nothing

        it "does not bound the reference-data CSV upload routes (out of policy)" $
            uploadBodyCeiling (Just (hostingWithLimit 100)) ["api", "v1", "units", "upload"] `shouldBe` Nothing

        it "does not bound uploads with no hosting config (local / CLI)" $
            uploadBodyCeiling Nothing dbUpload `shouldBe` Nothing

        it "does not bound uploads on the unlimited tier" $
            uploadBodyCeiling (Just (hostingWithLimit (-1))) dbUpload `shouldBe` Nothing

        it "does not bound at the HTTP layer when uploads are disabled (handler rejects)" $
            uploadBodyCeiling (Just (hostingWithLimit 0)) dbUpload `shouldBe` Nothing

        it "caps the db upload route at 2x the policy limit (base64 + JSON slack)" $
            uploadBodyCeiling (Just (hostingWithLimit 100)) dbUpload
                `shouldBe` Just (200 * 1024 * 1024 :: Word64)

        it "caps the method upload route the same way" $
            uploadBodyCeiling (Just (hostingWithLimit 50)) methodUpload
                `shouldBe` Just (100 * 1024 * 1024 :: Word64)
