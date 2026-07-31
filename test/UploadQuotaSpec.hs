{-# LANGUAGE OverloadedStrings #-}

{- | The hosting plan's database quotas, and what they count.

Two separate budgets: how many databases of their own a user may keep, and how
many of those may sit in memory at once. Both count only the user's own
databases — the ones a tier preloads are what an uploaded inventory links
against, so counting them would make the quota forbid the very thing uploading
is for.
-}
module UploadQuotaSpec (spec) where

import API.DatabaseHandlers (hostingQuotaRefusal)
import Config (HostingConfig (..))
import Data.Text (Text)
import Test.Hspec

-- | A plan allowing @stored@ databases and @loaded@ of them in memory.
plan :: Int -> Int -> HostingConfig
plan stored loaded =
    HostingConfig
        { hcMaxUploads = stored
        , hcMaxUploadMb = 100
        , hcMaxLoadedUploads = loaded
        , hcApiAccess = True
        , hcReadOnly = False
        , hcUpgradeUpload = ""
        , hcUpgradeApi = ""
        , hcUpgradeVmSize = ""
        }

-- | The stored-database budget, as the upload handler applies it.
storedRefusal :: Int -> Maybe HostingConfig -> Maybe Text
storedRefusal = hostingQuotaRefusal hcMaxUploads hcUpgradeUpload "no room"

-- | The in-memory budget, as the load handler applies it.
memoryRefusal :: Int -> Maybe HostingConfig -> Maybe Text
memoryRefusal = hostingQuotaRefusal hcMaxLoadedUploads hcUpgradeVmSize "unload first"

spec :: Spec
spec = describe "hosting database quotas" $ do
    it "does not apply at all without a hosting config (local / CLI / desktop)" $ do
        storedRefusal 99 Nothing `shouldBe` Nothing
        memoryRefusal 99 Nothing `shouldBe` Nothing

    it "treats a negative limit as unlimited" $
        storedRefusal 99 (Just (plan (-1) (-1))) `shouldBe` Nothing

    it "allows up to the limit and refuses at it" $ do
        storedRefusal 0 (Just (plan 2 1)) `shouldBe` Nothing
        storedRefusal 1 (Just (plan 2 1)) `shouldBe` Nothing
        storedRefusal 2 (Just (plan 2 1)) `shouldBe` Just "no room"
        storedRefusal 3 (Just (plan 2 1)) `shouldBe` Just "no room"

    it "refuses everything when the limit is zero" $
        storedRefusal 0 (Just (plan 0 0)) `shouldBe` Just "no room"

    it "keeps the two budgets independent" $ do
        -- room to store another, but no room to hold another in memory
        storedRefusal 1 (Just (plan 2 1)) `shouldBe` Nothing
        memoryRefusal 1 (Just (plan 2 1)) `shouldBe` Just "unload first"

    it "prefers the plan's own wording when it has some" $ do
        let worded = (plan 1 1){hcUpgradeUpload = "Upgrade to store more."}
        storedRefusal 1 (Just worded) `shouldBe` Just "Upgrade to store more."
