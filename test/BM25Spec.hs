{-# LANGUAGE OverloadedStrings #-}

module BM25Spec (spec) where

import Test.Hspec
import Data.UUID (UUID, nil)
import Data.Text (Text)
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.List (sortOn)
import Data.Ord (Down(Down))

import Types
import Search.BM25 (buildIndex, score)
import Search.Normalize (tokenize)

-- Minimal factory helpers ---------------------------------------------------

mkActivity :: Text -> Text -> [Exchange] -> Activity
mkActivity name loc xs = Activity
    { activityName = name
    , activityDescription = []
    , activitySynonyms = M.empty
    , activityClassification = M.empty
    , activityLocation = loc
    , activityUnit = "kg"
    , exchanges = xs
    , activityParams = M.empty
    , activityParamExprs = M.empty
    }

mkRefOutput :: UUID -> Exchange
mkRefOutput fid = TechnosphereExchange fid 1.0 nil False True nil Nothing ""

mkFlow :: UUID -> Text -> Flow
mkFlow fid name = Flow
    { flowId = fid
    , flowName = name
    , flowCategory = ""
    , flowSubcompartment = Nothing
    , flowUnitId = nil
    , flowType = Technosphere
    , flowSynonyms = M.empty
    , flowCAS = Nothing
    , flowSubstanceId = Nothing
    }

f1, f2, f3 :: UUID
f1 = read "11111111-1111-1111-1111-111111111111"
f2 = read "22222222-2222-2222-2222-222222222222"
f3 = read "33333333-3333-3333-3333-333333333333"

-- | Return docIds sorted by descending score, filtering out zeros.
ranking :: VU.Vector Double -> [Int]
ranking v =
    map fst
    . sortOn (Down . snd)
    . filter ((> 0) . snd)
    $ zip [0..] (VU.toList v)

spec :: Spec
spec = describe "Search.BM25" $ do

    it "ranks the matching document first" $ do
        let acts = V.fromList
              [ mkActivity "Electricity, grid, FR"           "FR" []
              , mkActivity "Viande bovine, steak haché, cru" "FR" []
              , mkActivity "Lait demi-écrémé"                "FR" []
              ]
            idx = buildIndex acts M.empty
            scores = score idx (tokenize "steak hache")
        head (ranking scores) `shouldBe` 1

    it "returns empty results for no query terms" $ do
        let acts = V.fromList [mkActivity "foo" "GLO" []]
            idx = buildIndex acts M.empty
        ranking (score idx []) `shouldBe` []

    it "returns zero scores when no term matches" $ do
        let acts = V.fromList [mkActivity "Electricity" "FR" []]
            idx = buildIndex acts M.empty
        ranking (score idx (tokenize "steak")) `shouldBe` []

    it "matches reference product names too" $ do
        let flows = M.fromList [(f1, mkFlow f1 "Lait, demi-écrémé")]
            acts = V.fromList
              [ mkActivity "Production activity alpha" "FR" [mkRefOutput f1]
              , mkActivity "Electricity, grid"         "FR" []
              ]
            idx = buildIndex acts flows
            scores = score idx (tokenize "lait")
        head (ranking scores) `shouldBe` 0

    it "is accent- and case-insensitive" $ do
        let acts = V.fromList
              [ mkActivity "Steak haché 15% MG" "FR" []
              , mkActivity "Electricity"       "FR" []
              ]
            idx = buildIndex acts M.empty
        head (ranking (score idx (tokenize "HACHE")))  `shouldBe` 0
        head (ranking (score idx (tokenize "Haché")))  `shouldBe` 0
        head (ranking (score idx (tokenize "hache")))  `shouldBe` 0

    it "prefers shorter documents with the same term (BM25 length normalization)" $ do
        let short = "Bovine steak"
            longTxt = "Bovine steak cooked in a long-winded description with many extra words only tangentially related"
            acts = V.fromList
              [ mkActivity longTxt "FR" []
              , mkActivity short   "FR" []
              ]
            idx = buildIndex acts M.empty
            scores = score idx (tokenize "bovine steak")
        head (ranking scores) `shouldBe` 1
