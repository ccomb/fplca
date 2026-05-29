{-# LANGUAGE OverloadedStrings #-}

module UploadSizeSpec (spec) where

import API.DatabaseHandlers (checkUploadSize)
import Config (HostingConfig (..))
import Data.Either (isLeft)
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
spec = describe "checkUploadSize" $ do
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
