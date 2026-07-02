{-# LANGUAGE OverloadedStrings #-}

{- | Display names for scoring-set breakdown indicators: primitive variables
take their impact-category name, computed variables take their entry in
@labels@, and only as a last resort does the raw variable key leak out.
-}
module ScoringLabelSpec (spec) where

import API.Routes (computeAllScoringSets)
import API.Types (ScoringIndicator (..))
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Method.Types (ScoringSet (..))
import Test.Hspec

scoringSet :: ScoringSet
scoringSet =
    ScoringSet
        { ssName = "ECS"
        , ssUnit = "Pts"
        , ssVariables =
            M.fromList
                [ ("cch", "Climate change")
                , ("etfo", "Ecotoxicity, freshwater_organics")
                , ("etfi", "Ecotoxicity, freshwater_inorganics")
                ]
        , ssComputed = M.fromList [("etf", "2 * etfo + etfi")]
        , ssLabels = M.fromList [("etf", "Ecotoxicity, freshwater")]
        , ssNormalization = M.empty
        , ssWeighting = M.fromList [("cch", 1.0), ("etf", 1.0)]
        , ssScores = M.fromList [("total", "cch + etf")]
        , ssDisplayMultiplier = Nothing
        }

rawScores :: M.Map Text Double
rawScores =
    M.fromList
        [ ("Climate change", 10.0)
        , ("Ecotoxicity, freshwater_organics", 2.0)
        , ("Ecotoxicity, freshwater_inorganics", 3.0)
        ]

spec :: Spec
spec = describe "scoring indicator display names" $ do
    it "labels a computed variable via [methods.scoring.labels]" $ do
        (_, indicators) <- computeAllScoringSets [scoringSet] rawScores
        (siCategory <$> (M.lookup "ECS" indicators >>= M.lookup "etf"))
            `shouldBe` Just "Ecotoxicity, freshwater"

    it "keeps impact-category names for primitive variables" $ do
        (_, indicators) <- computeAllScoringSets [scoringSet] rawScores
        (siCategory <$> (M.lookup "ECS" indicators >>= M.lookup "cch"))
            `shouldBe` Just "Climate change"
