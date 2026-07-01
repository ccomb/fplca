{-# LANGUAGE OverloadedStrings #-}

module SynonymDBSpec (spec) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text, pack)
import Test.Hspec

import SynonymDB (buildFromPairs, excludeJunkSynonyms, excludeOverFrequentSynonyms, getSynonyms, isJunkSynonymName, loadFromCSVFileWithCache, lookupSynonymGroup, normalizeName, oversizedClasses, uncoveredUnitSuffixes)

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
        let hub = [("hub", "s" <> pack (show i)) | i <- [1 :: Int .. 12]]

        it "flags a class larger than the bound (a closure that fused a junk hub)" $
            map length (oversizedClasses 10 hub) `shouldBe` [13]

        it "stays silent when every class is within the bound" $
            oversizedClasses 10 [("alpha", "beta"), ("beta", "gamma")]
                `shouldBe` []

    describe "excludeOverFrequentSynonyms" $ do
        -- "organic" acts as a synonym for 3 distinct flows -> a class label, dropped.
        -- "acetaminophen" merely HAS 3 synonyms (out-degree) -> a real flow, kept.
        let pairs =
                [ ("benzene", "organic")
                , ("toluene", "organic")
                , ("phenol", "organic")
                , ("acetaminophen", "paracetamol")
                , ("acetaminophen", "tylenol")
                , ("acetaminophen", "apap")
                ]
            (kept, excluded) = excludeOverFrequentSynonyms 2 pairs

        it "drops the over-frequent synonym and surfaces it with its flow count" $ do
            excluded `shouldBe` [("organic", 3)]
            ("organic" `elem` map snd kept) `shouldBe` False

        it "keeps a real flow that merely has many synonyms (out-degree, not in-degree)" $
            kept `shouldMatchList` [("acetaminophen", s) | s <- ["paracetamol", "tylenol", "apap"]]

        it "counts case/punctuation variants of a synonym together (normalized)" $
            snd (excludeOverFrequentSynonyms 2 [("a", "Organic"), ("b", "organic"), ("c", "ORGANIC")])
                `shouldBe` [("organic", 3)]

    describe "excludeJunkSynonyms" $ do
        -- Dossier placeholders / id stubs are dropped; real substances survive,
        -- including names that contain "(mixture)" or are digit-heavy.
        let pairs =
                [ ("arsenic", "not available")
                , ("benzene", "unknown")
                , ("sodium hydroxide", "98%activematter")
                , ("n-butane", "echa-8600dbe1-6174-49ec-b025-9cd03d318e49")
                , ("toluene diisocyanate", "2,4/2,6-toluenediisocyanate (mixture)")
                , ("hexachlorocyclohexane", "pcb-1254")
                ]
            (kept, dropped) = excludeJunkSynonyms pairs

        it "drops pairs touching a placeholder/id-stub token, keeps real ones" $
            kept
                `shouldMatchList` [ ("toluene diisocyanate", "2,4/2,6-toluenediisocyanate (mixture)")
                                  , ("hexachlorocyclohexane", "pcb-1254")
                                  ]

        it "surfaces the distinct dropped tokens" $
            length dropped `shouldBe` 4

        it "flags dossier prose and ECHA id stubs" $ do
            isJunkSynonymName "not available" `shouldBe` True
            isJunkSynonymName "unknown atom or ion" `shouldBe` True
            isJunkSynonymName "100%activematter" `shouldBe` True
            isJunkSynonymName "echa-8600dbe1-6174-49ec-b025-9cd03d318e49" `shouldBe` True

        it "spares real names with 'mixture', digits, or an inner 'echa'" $ do
            isJunkSynonymName "2,4/2,6-toluenediisocyanate (mixture)" `shouldBe` False
            isJunkSynonymName "pcb-1254" `shouldBe` False
            isJunkSynonymName "carbon 14" `shouldBe` False
            isJunkSynonymName "huile de chauffage" `shouldBe` False

        it "drops bare numeric ids and registry numbers (ENT, CIPAC)" $ do
            isJunkSynonymName "ENT 27164" `shouldBe` True
            isJunkSynonymName "ENT 27,164" `shouldBe` True
            isJunkSynonymName "27164" `shouldBe` True
            isJunkSynonymName "CIPAC 12" `shouldBe` True

        -- The ozd leak: carbon tetrachloride's ODP factor reached carbofuran
        -- because both carry the USDA number "ENT 27164" / "ENT 27,164", which
        -- normalize to the same token. Dropping the id breaks the bridge.
        it "drops a registry-id bridge so it cannot fuse unrelated substances" $ do
            let raw = [("carbon tetrachloride", "ENT 27164"), ("carbofuran", "ENT 27,164")]
                (kept, dropped) = excludeJunkSynonyms raw
            kept `shouldBe` []
            dropped `shouldBe` ["ent 27164"]

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
