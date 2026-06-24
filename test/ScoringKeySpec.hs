{-# LANGUAGE OverloadedStrings #-}

module ScoringKeySpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.UUID as UUID
import Test.Hspec

import API.Routes (rawScoreMapByName)
import API.Types (LCIAResult (..))

-- | Minimal LCIAResult for keying tests (only name/category/score matter here).
mkR :: Text -> Text -> Double -> LCIAResult
mkR name category score =
    LCIAResult
        { lrMethodId = UUID.nil
        , lrMethodName = name
        , lrCategory = category
        , lrDamageCategory = category
        , lrScore = score
        , lrUnit = ""
        , lrNormalizedScore = Nothing
        , lrWeightedScore = Nothing
        , lrMappedFlows = 0
        , lrFunctionalUnit = ""
        , lrTopContributors = []
        }

spec :: Spec
spec = describe "rawScoreMapByName" $ do
    it "keys by method name, retaining methods that share a coarse category" $ do
        -- Mirrors JRC ILCD EF 3.1: fossils and minerals both sit under the
        -- "Abiotic resource depletion" damage category. Keying by category
        -- would drop one; keying by name keeps both so single-score variables
        -- (mapped to method names) resolve.
        let results =
                [ mkR "Climate change" "Climate change" 10
                , mkR "Resource use, fossils" "Abiotic resource depletion" 5
                , mkR "Resource use, minerals and metals" "Abiotic resource depletion" 3
                ]
            m = rawScoreMapByName results
        M.lookup "Resource use, fossils" m `shouldBe` Just 5
        M.lookup "Resource use, minerals and metals" m `shouldBe` Just 3
        M.lookup "Climate change" m `shouldBe` Just 10
        M.size m `shouldBe` 3

    it "is identical to category-keying when name == category (SimaPro case)" $ do
        let results =
                [ mkR "Acidification" "Acidification" 2
                , mkR "Water use" "Water use" 7
                ]
        rawScoreMapByName results
            `shouldBe` M.fromList [("Acidification", 2), ("Water use", 7)]
