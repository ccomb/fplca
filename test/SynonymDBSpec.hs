{-# LANGUAGE OverloadedStrings #-}

module SynonymDBSpec (spec) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text, pack)
import Test.Hspec

import SynonymDB (buildFromPairs, getSynonyms, loadFromCSVFileWithCache, lookupSynonymGroup, normalizeName, oversizedClasses, uncoveredUnitSuffixes)

spec :: Spec
spec = do
    describe "loadFromCSVFileWithCache" $
        it "returns Left for a missing CSV instead of throwing" $ do
            -- Regression: the load used a bare readFile, so a missing
            -- reference file threw an uncaught IOException and took down
            -- server startup. The type promises Either; a missing file
            -- must surface as Left, like the other reference-data loaders.
            result <- loadFromCSVFileWithCache "test-data/does-not-exist-synonyms.csv"
            case result of
                Left _ -> pure ()
                Right _ -> expectationFailure "expected Left for a missing CSV file"

    describe "buildFromPairs transitive closure" $ do
        let db = buildFromPairs [("alpha", "beta"), ("beta", "gamma")]
            classFor name = S.fromList <$> (lookupSynonymGroup db name >>= getSynonyms db)

        it "groups chained synonyms (a=b, b=c) into one class, reachable from either end" $
            classFor "alpha" `shouldBe` Just (S.fromList ["alpha", "beta", "gamma"])

        it "gives both ends of the chain the same group id" $
            lookupSynonymGroup db "alpha" `shouldBe` lookupSynonymGroup db "gamma"

    describe "oversizedClasses" $ do
        -- A junk hub fuses everything it touches into one transitive class.
        let hub = buildFromPairs [("hub", "s" <> pack (show i)) | i <- [1 :: Int .. 12]]

        it "flags a class larger than the bound (a closure that fused a junk hub)" $
            map length (oversizedClasses 10 hub) `shouldBe` [13]

        it "stays silent when every class is within the bound" $
            oversizedClasses 10 (buildFromPairs [("alpha", "beta"), ("beta", "gamma")])
                `shouldBe` []

    describe "normalizeName" $ do
        it "strips a trailing SimaPro unit suffix (/kg, /m3, /Sm3) so unit variants share a node" $ do
            normalizeName "Gas, natural/m3" `shouldBe` "gas natural"
            normalizeName "Gas, natural/Sm3" `shouldBe` "gas natural"
            normalizeName "Coal, hard/kg" `shouldBe` "coal hard"

        it "leaves a name without a unit suffix unchanged (modulo casing/punctuation)" $
            normalizeName "Gas, natural" `shouldBe` "gas natural"

    describe "uncoveredUnitSuffixes" $ do
        -- A stand-in unit vocabulary; the real predicate is UnitConversion.isKnownUnit.
        let knownUnit = (`elem` (["MJ", "kg", "m3", "Sm3", "ha"] :: [Text]))
            grouped = M.map S.fromList . uncoveredUnitSuffixes knownUnit

        it "flags a flow whose /unit suffix is a real unit not covered by unitSuffixes" $
            grouped ["Electricity/MJ", "Heat/MJ", "Occupation, pasture/ha"]
                `shouldBe` M.fromList
                    [ ("MJ", S.fromList ["Electricity/MJ", "Heat/MJ"])
                    , ("ha", S.fromList ["Occupation, pasture/ha"])
                    ]

        it "ignores suffixes already stripped by unitSuffixes (/kg, /m3, /Sm3, any case)" $
            uncoveredUnitSuffixes knownUnit ["Gas, natural/m3", "Gas/Sm3", "Coal/kg"]
                `shouldBe` M.empty

        it "ignores a trailing slash that is not a known unit" $
            uncoveredUnitSuffixes knownUnit ["cis/trans-isomer", "Carbon dioxide"]
                `shouldBe` M.empty
