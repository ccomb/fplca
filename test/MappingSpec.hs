{-# LANGUAGE OverloadedStrings #-}

module MappingSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID, nil)
import Data.UUID.V4 (nextRandom)
import Test.Hspec

import Method.ChemSynonyms (emptyChemSynonyms, parseChemSynonymsCSV)
import Method.Mapping
import Method.Types (Compartment (..), FlowDirection (..), Method (..), MethodCF (..))
import SynonymDB (buildFromPairs, emptySynonymDB)
import Types (Flow (..), FlowType (..), Unit (..))
import UnitConversion (defaultUnitConfig)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

mkFlow :: UUID -> Text -> Text -> Maybe Text -> Flow
mkFlow fid name cat msub =
    Flow
        { flowId = fid
        , flowName = name
        , flowCategory = cat
        , flowSubcompartment = msub
        , flowUnitId = nil
        , flowType = Biosphere
        , flowSynonyms = M.empty
        , flowCAS = Nothing
        , flowSubstanceId = Nothing
        }

mkCF :: Text -> Maybe Text -> Double -> MethodCF
mkCF name mCas val =
    MethodCF
        { mcfFlowRef = nil
        , mcfFlowName = name
        , mcfDirection = Output
        , mcfValue = val
        , mcfCompartment = Nothing
        , mcfCAS = mCas
        , mcfUnit = "kg"
        , mcfLocation = Nothing
        }

mkCFComp :: Text -> Text -> Text -> Double -> MethodCF
mkCFComp name medium subcomp val =
    (mkCF name Nothing val)
        { mcfCompartment = Just (Compartment medium subcomp "")
        }

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
    describe "strategyFromText" $ do
        it "parses uuid" $ strategyFromText "uuid" `shouldBe` ByUUID
        it "parses cas" $ strategyFromText "CAS" `shouldBe` ByCAS
        it "parses name" $ strategyFromText "Name" `shouldBe` ByName
        it "parses synonym" $ strategyFromText "synonym" `shouldBe` BySynonym
        it "parses fuzzy" $ strategyFromText "fuzzy" `shouldBe` ByFuzzy
        it "unknown falls back to fuzzy" $ strategyFromText "xyz" `shouldBe` ByFuzzy

    describe "findFlowByUUID" $ do
        it "finds a flow by its UUID" $ do
            fid <- nextRandom
            let flow = mkFlow fid "CO2" "air" Nothing
                db = M.singleton fid flow
            fmap flowId (findFlowByUUID db fid) `shouldBe` Just fid

        it "returns Nothing for unknown UUID" $ do
            fid <- nextRandom
            fmap flowId (findFlowByUUID M.empty fid) `shouldBe` Nothing

    describe "pickByCompartment (via findFlowByNameComp)" $ do
        it "returns Nothing for empty candidate list" $
            fmap flowId (findFlowByNameComp M.empty "co2" Nothing) `shouldBe` Nothing

        it "returns first flow when no compartment preference" $ do
            fid1 <- nextRandom
            fid2 <- nextRandom
            let f1 = mkFlow fid1 "co2" "air" Nothing
                f2 = mkFlow fid2 "co2" "water" Nothing
                byName = M.singleton "co2" [f1, f2]
            fmap flowId (findFlowByNameComp byName "co2" Nothing) `shouldBe` Just fid1

        it "prefers exact medium+subcomp match" $ do
            fid1 <- nextRandom
            fid2 <- nextRandom
            let fAir = mkFlow fid1 "co2" "air" (Just "urban air")
                fWater = mkFlow fid2 "co2" "water" (Just "surface water")
                byName = M.singleton "co2" [fWater, fAir]
                comp = Compartment "air" "urban air" ""
            fmap flowId (findFlowByNameComp byName "co2" (Just comp)) `shouldBe` Just fid1

        it "falls back to medium match when no exact subcomp" $ do
            fid1 <- nextRandom
            fid2 <- nextRandom
            let fAir = mkFlow fid1 "co2" "air" (Just "non-urban air")
                fWater = mkFlow fid2 "co2" "water" Nothing
                byName = M.singleton "co2" [fWater, fAir]
                comp = Compartment "air" "unspecified" ""
            fmap flowId (findFlowByNameComp byName "co2" (Just comp)) `shouldBe` Just fid1

        it "falls back to first candidate when no medium matches" $ do
            fid1 <- nextRandom
            let fWater = mkFlow fid1 "co2" "water" Nothing
                byName = M.singleton "co2" [fWater]
                comp = Compartment "air" "" ""
            fmap flowId (findFlowByNameComp byName "co2" (Just comp)) `shouldBe` Just fid1

    describe "findFlowByCAS" $ do
        it "finds flow by CAS number" $ do
            fid <- nextRandom
            let flow = mkFlow fid "Carbon dioxide" "air" Nothing
                byCAS = M.singleton "124-38-9" [flow]
            fmap flowId (findFlowByCAS byCAS "124-38-9" Nothing) `shouldBe` Just fid

        it "returns Nothing for unknown CAS" $
            fmap flowId (findFlowByCAS M.empty "000-00-0" Nothing) `shouldBe` Nothing

    describe "findFlowByName" $ do
        it "finds a flow by name (case-insensitive via normalization)" $ do
            fid <- nextRandom
            let flow = mkFlow fid "Carbon dioxide" "air" Nothing
                byName = M.singleton "carbon dioxide" [flow]
            fmap flowId (findFlowByName byName "Carbon dioxide") `shouldBe` Just fid

        it "returns Nothing for unknown name" $
            fmap flowId (findFlowByName M.empty "co2") `shouldBe` Nothing

    describe "findFlowBySynonym" $ do
        it "returns Nothing when synonym not in DB" $ do
            fid <- nextRandom
            let flow = mkFlow fid "Carbon dioxide" "air" Nothing
                byName = M.singleton "carbon dioxide" [flow]
            fmap flowId (findFlowBySynonym emptySynonymDB byName "CO2") `shouldBe` Nothing

    describe "findFlowBySynonymComp" $ do
        it "finds flow via synonym with compartment preference" $ do
            fid1 <- nextRandom
            fid2 <- nextRandom
            let synDB = buildFromPairs [("CO2", "Carbon dioxide")]
                fAir = mkFlow fid1 "Carbon dioxide" "air" Nothing
                fWater = mkFlow fid2 "Carbon dioxide" "water" Nothing
                byName = M.singleton "carbon dioxide" [fWater, fAir]
                comp = Compartment "air" "" ""
            fmap flowId (findFlowBySynonymComp synDB byName "CO2" (Just comp))
                `shouldBe` Just fid1

        it "returns Nothing when synonym not in DB" $ do
            fid <- nextRandom
            let synDB = buildFromPairs [("CO2", "Carbon dioxide")]
                flow = mkFlow fid "Carbon dioxide" "air" Nothing
                byName = M.singleton "carbon dioxide" [flow]
            fmap flowId (findFlowBySynonymComp synDB byName "methane" Nothing)
                `shouldBe` Nothing

        it "returns Nothing when no flows match any synonym" $ do
            let synDB = buildFromPairs [("CO2", "Carbon dioxide")]
            fmap flowId (findFlowBySynonymComp synDB M.empty "CO2" Nothing)
                `shouldBe` Nothing

    describe "computeMappingStats" $ do
        it "counts totals and strategies correctly" $ do
            fid1 <- nextRandom
            fid2 <- nextRandom
            fid3 <- nextRandom
            let f1 = mkFlow fid1 "co2" "air" Nothing
                f2 = mkFlow fid2 "methane" "air" Nothing
                f3 = mkFlow fid3 "n2o" "air" Nothing
                cf1 = mkCF "co2" Nothing 1.0
                cf2 = mkCF "methane" Nothing 25.0
                cf3 = mkCF "n2o" Nothing 298.0
                cf4 = mkCF "hfc" Nothing 1300.0
                mappings =
                    [ (cf1, Just (f1, ByUUID))
                    , (cf2, Just (f2, ByName))
                    , (cf3, Just (f3, ByCAS))
                    , (cf4, Nothing)
                    ]
                stats = computeMappingStats mappings
            msTotal stats `shouldBe` 4
            msByUUID stats `shouldBe` 1
            msByName stats `shouldBe` 1
            msByCAS stats `shouldBe` 1
            msBySynonym stats `shouldBe` 0
            msUnmatched stats `shouldBe` 1

        it "handles all-unmatched" $ do
            let cf = mkCF "xyz" Nothing 1.0
                stats = computeMappingStats [(cf, Nothing)]
            msUnmatched stats `shouldBe` 1
            msByUUID stats `shouldBe` 0

    describe "computeLCIAScore" $ do
        it "sums UUID-matched flows" $ do
            fid <- nextRandom
            let flow = mkFlow fid "co2" "air" Nothing
                unit = Unit{unitId = nil, unitName = "kg", unitSymbol = "kg", unitComment = ""}
                cf = mkCF "co2" Nothing 1.0
                mapping = [(cf, Just (flow, ByUUID))]
                inventory = M.singleton fid 100.0
                flowDB = M.singleton fid flow
                unitDB = M.singleton nil unit
                score = loScore (computeLCIAScore defaultUnitConfig unitDB flowDB inventory mapping)
            score `shouldBe` 100.0

        it "returns 0 when inventory is empty" $ do
            let cf = mkCF "co2" Nothing 1.0
                score = loScore (computeLCIAScore defaultUnitConfig M.empty M.empty M.empty [(cf, Nothing)])
            score `shouldBe` 0.0

        it "skips zero-quantity flows" $ do
            fid <- nextRandom
            let flow = mkFlow fid "co2" "air" Nothing
                cf = mkCF "co2" Nothing 1.0
                mapping = [(cf, Just (flow, ByUUID))]
                inventory = M.singleton fid 0.0
                score = loScore (computeLCIAScore defaultUnitConfig M.empty (M.singleton fid flow) inventory mapping)
            score `shouldBe` 0.0

        it "scores via fallback CF (name+medium, empty subcomp)" $ do
            fid <- nextRandom
            let flow = mkFlow fid "Carbon dioxide" "air" Nothing
                cf = mkCFComp "Carbon dioxide" "air" "" 2.5
                mapping = [(cf, Nothing)] -- unmatched → name-based lookup
                inventory = M.singleton fid 10.0
                flowDB = M.singleton fid flow
                score = loScore (computeLCIAScore defaultUnitConfig M.empty flowDB inventory mapping)
            score `shouldBe` 25.0

        it "scores via exact CF (name+medium+subcomp)" $ do
            fid <- nextRandom
            let flow = mkFlow fid "Carbon dioxide" "air" (Just "urban air close to ground")
                cf = mkCFComp "Carbon dioxide" "air" "urban air close to ground" 3.0
                mapping = [(cf, Nothing)]
                inventory = M.singleton fid 5.0
                flowDB = M.singleton fid flow
                score = loScore (computeLCIAScore defaultUnitConfig M.empty flowDB inventory mapping)
            score `shouldBe` 15.0

        it "normalizes 'natural resource' category to 'resource'" $ do
            fid <- nextRandom
            let flow = mkFlow fid "crude oil" "natural resource" Nothing
                cf = mkCFComp "crude oil" "natural resource" "" 1.5
                mapping = [(cf, Nothing)]
                inventory = M.singleton fid 4.0
                flowDB = M.singleton fid flow
                score = loScore (computeLCIAScore defaultUnitConfig M.empty flowDB inventory mapping)
            score `shouldBe` 6.0

        it "returns 0 for flow not in flowDB" $ do
            fid <- nextRandom
            let cf = mkCF "co2" Nothing 1.0
                mapping = [(cf, Nothing)]
                inventory = M.singleton fid 10.0
                score = loScore (computeLCIAScore defaultUnitConfig M.empty M.empty inventory mapping)
            score `shouldBe` 0.0

    describe "location-aware CF lookup" $ do
        it "prefers the activity's location over the global default" $ do
            fid <- nextRandom
            let flow = mkFlow fid "Ammonia" "air" Nothing
                cfGlobal = (mkCFComp "Ammonia" "air" "" 3.02){mcfLocation = Nothing}
                cfCH = (mkCFComp "Ammonia" "air" "" 0.747){mcfLocation = Just "CH"}
                cfNO = (mkCFComp "Ammonia" "air" "" 11.491){mcfLocation = Just "NO"}
                tables = buildMethodTables M.empty [(cfGlobal, Nothing), (cfCH, Nothing), (cfNO, Nothing)]
                inventory = M.singleton fid 10.0
                flowDB = M.singleton fid flow
                scoreCH = loScore (computeLCIAScoreFromTables defaultUnitConfig M.empty flowDB inventory tables (Just "CH"))
                scoreNO = loScore (computeLCIAScoreFromTables defaultUnitConfig M.empty flowDB inventory tables (Just "NO"))
                scoreGlobal = loScore (computeLCIAScoreFromTables defaultUnitConfig M.empty flowDB inventory tables Nothing)
            scoreCH `shouldBe` 7.47
            scoreNO `shouldBe` 114.91
            scoreGlobal `shouldBe` 30.2

        it "falls back to the global CF when the activity's location has no specific CF" $ do
            fid <- nextRandom
            let flow = mkFlow fid "Methane" "air" Nothing
                cfGlobal = (mkCFComp "Methane" "air" "" 28.0){mcfLocation = Nothing}
                tables = buildMethodTables M.empty [(cfGlobal, Nothing)]
                inventory = M.singleton fid 1.0
                flowDB = M.singleton fid flow
                -- "ZZ" not represented; should fall back to global.
                score = loScore (computeLCIAScoreFromTables defaultUnitConfig M.empty flowDB inventory tables (Just "ZZ"))
            score `shouldBe` 28.0

    describe "computeMappingStats (ByFuzzy and BySynonym)" $ do
        it "counts ByFuzzy matches" $ do
            fid <- nextRandom
            let flow = mkFlow fid "co2" "air" Nothing
                cf = mkCF "co2" Nothing 1.0
                stats = computeMappingStats [(cf, Just (flow, ByFuzzy))]
            msByFuzzy stats `shouldBe` 1
            msBySynonym stats `shouldBe` 0

        it "counts BySynonym matches" $ do
            fid <- nextRandom
            let flow = mkFlow fid "co2" "air" Nothing
                cf = mkCF "co2" Nothing 1.0
                stats = computeMappingStats [(cf, Just (flow, BySynonym))]
            msBySynonym stats `shouldBe` 1

    describe "findFlowBySynonym (finds via synonym)" $ do
        it "finds flow via synonym group (no compartment)" $ do
            fid <- nextRandom
            let synDB = buildFromPairs [("co2", "carbon dioxide")]
                flow = mkFlow fid "carbon dioxide" "air" Nothing
                byName = M.singleton "carbon dioxide" [flow]
            fmap flowId (findFlowBySynonym synDB byName "co2")
                `shouldBe` Just fid

    describe "pickByCompartment (matchMedium edge cases)" $ do
        it "null medium matches any flow" $ do
            fid <- nextRandom
            let flow = mkFlow fid "co2" "water" Nothing
                byName = M.singleton "co2" [flow]
                comp = Compartment "" "" ""
            fmap flowId (findFlowByNameComp byName "co2" (Just comp)) `shouldBe` Just fid

        it "medium isInfixOf category matches (air in urban air)" $ do
            fid1 <- nextRandom
            fid2 <- nextRandom
            let fUrbanAir = mkFlow fid1 "nox" "urban air" Nothing
                fWater = mkFlow fid2 "nox" "water" Nothing
                byName = M.singleton "nox" [fWater, fUrbanAir]
                comp = Compartment "air" "urban" ""
            fmap flowId (findFlowByNameComp byName "nox" (Just comp)) `shouldBe` Just fid1

    describe "findSimilarCFs (post-scoring suggester)" $ do
        let mkMethod cfs =
                Method
                    { methodId = nil
                    , methodName = "Test"
                    , methodDescription = Nothing
                    , methodUnit = "kg eq"
                    , methodCategory = "Climate change"
                    , methodMethodology = Nothing
                    , methodFactors = cfs
                    }
            airComp = Just (Compartment "air" "" "")

        it "returns no candidates from an empty method" $ do
            fid <- nextRandom
            let flow = (mkFlow fid "Carbon dioxide" "air" Nothing){flowCAS = Nothing}
                idx = buildMethodIndex (mkMethod [])
            findSimilarCFs emptyChemSynonyms idx flow 3 `shouldBe` []

        it "matches CO2 to Carbon dioxide via PubChem synonym expansion" $ do
            fid <- nextRandom
            let csv =
                    "cas;canonical_name;synonyms...\n\
                    \124-38-9;Carbon dioxide;CO2;Carbonic anhydride\n"
                Right syns = parseChemSynonymsCSV csv
                co2 = (mkCFComp "CO2" "air" "" 1.0){mcfCompartment = airComp}
                ch4 = (mkCFComp "Methane" "air" "" 27.0){mcfCompartment = airComp}
                idx = buildMethodIndex (mkMethod [co2, ch4])
                flow = (mkFlow fid "Carbon dioxide" "air" Nothing){flowCAS = Nothing}
                cands = findSimilarCFs syns idx flow 3
            -- The CO2 candidate must be present, with the synonym-expansion reason.
            let names = map scfMethodFlowName cands
            names `shouldSatisfy` ("CO2" `elem`)
            let co2Cand = head [c | c <- cands, scfMethodFlowName c == "CO2"]
            scfReason co2Cand `shouldBe` SimBySynonymExpansion
            scfScore co2Cand `shouldSatisfy` (> 0)

        it "matches via CAS bridge when names diverge entirely" $ do
            fid <- nextRandom
            let oddName =
                    (mkCFComp "Some weird IUPAC name" "air" "" 1.0)
                        { mcfCAS = Just "124-38-9"
                        , mcfCompartment = airComp
                        }
                idx = buildMethodIndex (mkMethod [oddName])
                flow =
                    (mkFlow fid "Random unrelated text" "air" Nothing)
                        { flowCAS = Just "124-38-9"
                        }
                cands = findSimilarCFs emptyChemSynonyms idx flow 3
            map scfReason cands `shouldBe` [SimByCASBridge]
            map scfScore cands `shouldBe` [0.95]

        it "ranks the higher-similarity candidate first" $ do
            fid <- nextRandom
            let close = mkCFComp "Methane biogenic" "air" "" 27.0
                far = mkCFComp "Crude oil" "air" "" 0.0
                idx = buildMethodIndex (mkMethod [far, close])
                flow = mkFlow fid "Methane, biogenic" "air" Nothing
                cands = findSimilarCFs emptyChemSynonyms idx flow 2
            map scfMethodFlowName cands `shouldSatisfy` (\ns -> not (null ns) && head ns == "Methane biogenic")

        it "respects maxN cap" $ do
            fid <- nextRandom
            let cfs = [mkCFComp ("foo " <> tShow i) "air" "" 1.0 | i <- [1 .. 10 :: Int]]
                idx = buildMethodIndex (mkMethod cfs)
                flow = mkFlow fid "foo bar" "air" Nothing
                cands = findSimilarCFs emptyChemSynonyms idx flow 3
            length cands `shouldSatisfy` (<= 3)

    describe "findUncharacterized" $ do
        let mkMethod cfs =
                Method
                    { methodId = nil
                    , methodName = "Test"
                    , methodDescription = Nothing
                    , methodUnit = "kg eq"
                    , methodCategory = "Climate change"
                    , methodMethodology = Nothing
                    , methodFactors = cfs
                    }

        it "returns [] when uoMaxFlows is 0" $ do
            fid <- nextRandom
            let flow = mkFlow fid "co2" "air" Nothing
                inv = M.singleton fid 100.0
                tables = buildMethodTables M.empty []
                idx = buildMethodIndex (mkMethod [])
                opts = defaultUncharacterizedOpts{uoMaxFlows = 0}
            findUncharacterized
                defaultUnitConfig
                M.empty
                (M.singleton fid flow)
                inv
                tables
                emptyChemSynonyms
                idx
                opts
                Nothing
                `shouldBe` []

        it "drops flows below the absolute-weight threshold" $ do
            big <- nextRandom
            small <- nextRandom
            let bigFlow = mkFlow big "tiny stuff" "air" Nothing
                smallFlow = mkFlow small "huge stuff" "air" Nothing
                inv = M.fromList [(big, 999.0), (small, 1.0)]
                flowDB = M.fromList [(big, bigFlow), (small, smallFlow)]
                tables = buildMethodTables M.empty []
                idx = buildMethodIndex (mkMethod [])
                opts = defaultUncharacterizedOpts{uoMinAbsWeight = 0.5}
                result =
                    findUncharacterized
                        defaultUnitConfig
                        M.empty
                        flowDB
                        inv
                        tables
                        emptyChemSynonyms
                        idx
                        opts
                        Nothing
            -- Only the big flow (99.9% of mass) clears the 50% threshold.
            map ucfFlowName result `shouldBe` ["tiny stuff"]

        it "skips flows that DO have a CF (they're characterized)" $ do
            fid <- nextRandom
            let flow = mkFlow fid "co2" "air" Nothing
                cf = (mkCF "co2" Nothing 1.0){mcfFlowRef = fid}
                tables = buildMethodTables M.empty [(cf, Just (flow, ByUUID))]
                idx = buildMethodIndex (mkMethod [cf])
                inv = M.singleton fid 100.0
                flowDB = M.singleton fid flow
            findUncharacterized
                defaultUnitConfig
                M.empty
                flowDB
                inv
                tables
                emptyChemSynonyms
                idx
                defaultUncharacterizedOpts
                Nothing
                `shouldBe` []

tShow :: (Show a) => a -> Text
tShow = T.pack . show
