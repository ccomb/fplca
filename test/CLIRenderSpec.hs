{-# LANGUAGE OverloadedStrings #-}

module CLIRenderSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import Data.Either (isLeft)
import qualified Data.Text as T
import qualified Data.Vector as V
import Test.Hspec

import CLI.Render (csvRows, renderResult, selectPath)
import CLI.Types (OutputFormat (..))

-- A search response: two arrays, so nothing can guess which one to flatten.
searchResponse :: Value
searchResponse =
    object
        [ "srResults"
            .= [ object ["name" .= T.pack "electricity", "location" .= T.pack "FR"]
               , object ["name" .= T.pack "steel", "location" .= T.pack "RER"]
               ]
        , "srWarnings" .= ([] :: [Value])
        , "srTotal" .= (2 :: Int)
        ]

nested :: Value
nested = object ["piActivity" .= object ["pfaExchanges" .= [object ["amount" .= (1.5 :: Double)]]]]

spec :: Spec
spec = do
    describe "selectPath" $ do
        it "resolves a single field" $
            selectPath "srTotal" searchResponse `shouldBe` Right (Number 2)

        it "walks a dotted path through nested objects" $
            selectPath "piActivity.pfaExchanges" nested
                `shouldBe` Right (Array (V.fromList [object ["amount" .= (1.5 :: Double)]]))

        it "names the fields that are there when the path names one that is not" $
            case selectPath "srResult" searchResponse of
                Left err -> do
                    err `shouldSatisfy` T.isInfixOf "no field \"srResult\""
                    err `shouldSatisfy` T.isInfixOf "srResults"
                Right v -> expectationFailure ("expected a refusal, got " <> show v)

        it "says where it stopped when an intermediate step is not an object" $
            case selectPath "srTotal.deeper" searchResponse of
                Left err -> err `shouldSatisfy` T.isInfixOf "which is a number"
                Right v -> expectationFailure ("expected a refusal, got " <> show v)

    describe "csvRows" $ do
        it "takes the array the path names, not whichever one comes first" $
            fmap length (csvRows (Just "srResults") searchResponse) `shouldBe` Right 2

        it "refuses a path that names something other than an array" $
            csvRows (Just "srTotal") searchResponse `shouldSatisfy` isLeft

        it "refuses to guess when a response holds several arrays" $
            csvRows Nothing searchResponse `shouldSatisfy` isLeft

        it "still accepts a bare array with no path" $
            fmap length (csvRows Nothing (Array (V.fromList [Number 1, Number 2]))) `shouldBe` Right 2

    describe "renderResult" $ do
        -- Columns follow the key order aeson's KeyMap yields, not the order the
        -- record declares them.
        it "flattens the named array into a header row plus one row per element" $
            renderResult CSV (Just "srResults") searchResponse
                `shouldBe` Right "\"location\",\"name\"\n\"FR\",\"electricity\"\n\"RER\",\"steel\"\n"

        it "reports the bad path instead of printing JSON where a table was asked for" $
            renderResult CSV (Just "nope") searchResponse `shouldSatisfy` isLeft

        it "ignores the path for a format that does not select an array" $
            renderResult JSON (Just "srResults") (object ["a" .= (1 :: Int)])
                `shouldBe` Right "{\"a\":1}\n"
