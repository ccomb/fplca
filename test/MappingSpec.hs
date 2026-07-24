{-# LANGUAGE OverloadedStrings #-}

module MappingSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID, fromWords, nil)
import Data.UUID.V4 (nextRandom)
import Test.Hspec

import Method.ChemSynonyms (emptyChemSynonyms, parseChemSynonymsCSV)
import Method.Mapping
import Method.ParserCSV (parseMethodCSVBytes)
import Method.Types (Compartment (..), FlowDirection (..), Method (..), MethodCF (..), buildCompartmentMapFromCSV)
import SynonymDB (BridgeDirection (..), SynEdge (..), buildFromEdges, buildFromPairs, emptySynonymDB, normalizeName)
import Types (BiosphereFlow (..), Unit (..))
import qualified Types as VT
import UnitConversion (UnitConfig (..), UnitDef (..), defaultUnitConfig)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

mkFlow :: UUID -> Text -> Text -> Maybe Text -> BiosphereFlow
mkFlow fid name cat msub =
    BiosphereFlow
        { bfId = fid
        , bfName = name
        , bfUnitId = nil
        , bfSynonyms = M.empty
        , bfCAS = Nothing
        , bfSubstanceId = Nothing
        , bfCompartment = Just (VT.Compartment cat msub)
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
        , mcfConsumerLocation = Nothing
        }

mkCFComp :: Text -> Text -> Text -> Double -> MethodCF
mkCFComp name medium subcomp val =
    (mkCF name Nothing val)
        { mcfCompartment = Just (Compartment medium subcomp "")
        }

unitNamed :: Text -> Unit
unitNamed n = Unit{unitId = nil, unitName = n, unitSymbol = n, unitComment = ""}

-- | UnitConfig with both kg and g (mass) so g→kg conversion succeeds.
gKgUnitConfig :: UnitConfig
gKgUnitConfig =
    UnitConfig
        { ucDimensionOrder = ["mass", "length", "time", "energy", "area", "volume", "count", "currency"]
        , ucUnits =
            M.fromList
                [ ("kg", UnitDef [1, 0, 0, 0, 0, 0, 0, 0] 1.0)
                , ("g", UnitDef [1, 0, 0, 0, 0, 0, 0, 0] 0.001)
                ]
        , ucOriginalKeys = M.fromList [("kg", "kg"), ("g", "g")]
        }

{- | UnitConfig whose mass dimension has NO canonical base (g only, no kg at
factor 1.0), so 'normalizeToCanonical' fails — exercises the result-expression
branch's hard-fail to 0.
-}
gOnlyUnitConfig :: UnitConfig
gOnlyUnitConfig =
    UnitConfig
        { ucDimensionOrder = ["mass", "length", "time", "energy", "area", "volume", "count", "currency"]
        , ucUnits = M.fromList [("g", UnitDef [1, 0, 0, 0, 0, 0, 0, 0] 0.001)]
        , ucOriginalKeys = M.fromList [("g", "g")]
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
            fmap bfId (findFlowByUUID db fid) `shouldBe` Just fid

        it "returns Nothing for unknown UUID" $ do
            fid <- nextRandom
            fmap bfId (findFlowByUUID M.empty fid) `shouldBe` Nothing

    describe "pickByCompartment (via findFlowByNameComp)" $ do
        it "returns Nothing for empty candidate list" $
            fmap bfId (findFlowByNameComp M.empty "co2" Nothing) `shouldBe` Nothing

        it "returns first flow when no compartment preference" $ do
            fid1 <- nextRandom
            fid2 <- nextRandom
            let f1 = mkFlow fid1 "co2" "air" Nothing
                f2 = mkFlow fid2 "co2" "water" Nothing
                byName = M.singleton "co2" [f1, f2]
            fmap bfId (findFlowByNameComp byName "co2" Nothing) `shouldBe` Just fid1

        it "prefers exact medium+subcomp match" $ do
            fid1 <- nextRandom
            fid2 <- nextRandom
            let fAir = mkFlow fid1 "co2" "air" (Just "urban air")
                fWater = mkFlow fid2 "co2" "water" (Just "surface water")
                byName = M.singleton "co2" [fWater, fAir]
                comp = Compartment "air" "urban air" ""
            fmap bfId (findFlowByNameComp byName "co2" (Just comp)) `shouldBe` Just fid1

        it "falls back to medium match when no exact subcomp" $ do
            fid1 <- nextRandom
            fid2 <- nextRandom
            let fAir = mkFlow fid1 "co2" "air" (Just "non-urban air")
                fWater = mkFlow fid2 "co2" "water" Nothing
                byName = M.singleton "co2" [fWater, fAir]
                comp = Compartment "air" "unspecified" ""
            fmap bfId (findFlowByNameComp byName "co2" (Just comp)) `shouldBe` Just fid1

        it "falls back to first candidate when no medium matches" $ do
            fid1 <- nextRandom
            let fWater = mkFlow fid1 "co2" "water" Nothing
                byName = M.singleton "co2" [fWater]
                comp = Compartment "air" "" ""
            fmap bfId (findFlowByNameComp byName "co2" (Just comp)) `shouldBe` Just fid1

    describe "findFlowByCAS" $ do
        it "finds flow by CAS number" $ do
            fid <- nextRandom
            let flow = mkFlow fid "Carbon dioxide" "air" Nothing
                byCAS = M.singleton "124-38-9" [flow]
            fmap bfId (findFlowByCAS byCAS "124-38-9" Nothing) `shouldBe` Just fid

        it "returns Nothing for unknown CAS" $
            fmap bfId (findFlowByCAS M.empty "000-00-0" Nothing) `shouldBe` Nothing

    describe "findFlowByName" $ do
        it "finds a flow by name (case-insensitive via normalization)" $ do
            fid <- nextRandom
            let flow = mkFlow fid "Carbon dioxide" "air" Nothing
                byName = M.singleton "carbon dioxide" [flow]
            fmap bfId (findFlowByName byName "Carbon dioxide") `shouldBe` Just fid

        it "returns Nothing for unknown name" $
            fmap bfId (findFlowByName M.empty "co2") `shouldBe` Nothing

    describe "findFlowBySynonym" $ do
        it "returns Nothing when synonym not in DB" $ do
            fid <- nextRandom
            let flow = mkFlow fid "Carbon dioxide" "air" Nothing
                byName = M.singleton "carbon dioxide" [flow]
            fmap bfId (findFlowBySynonym emptySynonymDB byName "CO2") `shouldBe` Nothing

    describe "findFlowBySynonymComp" $ do
        it "finds flow via synonym with compartment preference" $ do
            fid1 <- nextRandom
            fid2 <- nextRandom
            let synDB = buildFromPairs [("CO2", "Carbon dioxide")]
                fAir = mkFlow fid1 "Carbon dioxide" "air" Nothing
                fWater = mkFlow fid2 "Carbon dioxide" "water" Nothing
                byName = M.singleton "carbon dioxide" [fWater, fAir]
                comp = Compartment "air" "" ""
            fmap bfId (findFlowBySynonymComp synDB byName "CO2" (Just comp))
                `shouldBe` Just fid1

        it "returns Nothing when synonym not in DB" $ do
            fid <- nextRandom
            let synDB = buildFromPairs [("CO2", "Carbon dioxide")]
                flow = mkFlow fid "Carbon dioxide" "air" Nothing
                byName = M.singleton "carbon dioxide" [flow]
            fmap bfId (findFlowBySynonymComp synDB byName "methane" Nothing)
                `shouldBe` Nothing

        it "returns Nothing when no flows match any synonym" $ do
            let synDB = buildFromPairs [("CO2", "Carbon dioxide")]
            fmap bfId (findFlowBySynonymComp synDB M.empty "CO2" Nothing)
                `shouldBe` Nothing

    describe "expandSynonymMappings direction" $ do
        -- The water withdrawal bridge "freshwater" → resource flow applies to an
        -- INPUT (withdrawal) CF only. An OUTPUT (release) CF named "freshwater"
        -- must not fan out onto the resource flow through it — else a release
        -- inherits a withdrawal scarcity factor (wrong sign/magnitude).
        let synDB = buildFromEdges [SynEdge "freshwater" "Water, unspecified natural origin" BridgeInput]
            resourceFlow fid = mkFlow fid "Water, unspecified natural origin" "natural resource" Nothing
            flowsByName fid = M.singleton "water unspecified natural origin" [resourceFlow fid]
            inputCF = (mkCF "freshwater" Nothing 1.0){mcfDirection = Input}
            outputCF = (mkCF "freshwater" Nothing 1.0){mcfDirection = Output}
            fannedIds cf fid =
                [bfId flow | (_, Just (flow, _)) <- drop 1 (expandSynonymMappings synDB (flowsByName fid) [(cf, Nothing)])]

        it "fans an INPUT CF out onto the withdrawal resource flow" $ do
            fid <- nextRandom
            fannedIds inputCF fid `shouldBe` [fid]

        it "does NOT fan an OUTPUT CF through the input-only bridge" $ do
            fid <- nextRandom
            fannedIds outputCF fid `shouldBe` []

    describe "expandSynonymMappings transitivity" $ do
        -- A curated chain routinely pivots through an alias that names no loaded
        -- flow: "Energy, from coal" = "hard coal" = "Coal, hard", where only the
        -- endpoints are flow or CF names. The fan-out must follow the closure
        -- through that pivot — requiring every intermediate to be a flow or CF
        -- name silently cut the whole coal family out of energy accounting.
        let synDB =
                buildFromEdges
                    [ SynEdge "Energy, from coal" "hard coal" BridgeBoth
                    , SynEdge "hard coal" "Coal, hard" BridgeBoth
                    ]
            energyCF = (mkCF "Energy, from coal" Nothing 1.0){mcfDirection = Input}

        it "fans out through a pivot alias that is neither a flow nor a CF name" $ do
            fid <- nextRandom
            let coalFlow = mkFlow fid "Coal, hard" "resource" Nothing
                flowsByName = M.singleton "coal hard" [coalFlow]
            [ bfId flow
              | (_, Just (flow, BySynonym)) <-
                    drop 1 (expandSynonymMappings synDB flowsByName [(energyCF, Nothing)])
              ]
                `shouldBe` [fid]

    describe "directionExcludedCFs" $ do
        -- An unmapped CF whose name matches through the UNION synonym tables but
        -- not through its own direction's view was excluded by the direction
        -- restriction alone — e.g. a parser defaulted the direction when the
        -- method carried none. The loader surfaces these so the loss is
        -- distinguishable from a genuinely uncharacterized flow.
        let synDB = buildFromEdges [SynEdge "freshwater" "Water, unspecified natural origin" BridgeInput]
            flowsByName fid = M.singleton "water unspecified natural origin" [mkFlow fid "Water, unspecified natural origin" "natural resource" Nothing]
            inputCF = (mkCF "freshwater" Nothing 1.0){mcfDirection = Input}
            outputCF = (mkCF "freshwater" Nothing 1.0){mcfDirection = Output}

        it "flags an unmapped CF whose synonym match exists only outside its direction view" $ do
            fid <- nextRandom
            map mcfFlowName (directionExcludedCFs synDB (flowsByName fid) [(outputCF, Nothing)])
                `shouldBe` ["freshwater"]

        it "does not flag a CF its own direction view still matches, nor a genuinely unmatched one" $ do
            fid <- nextRandom
            directionExcludedCFs synDB (flowsByName fid) [(inputCF, Nothing)] `shouldSatisfy` null
            directionExcludedCFs synDB (flowsByName fid) [(mkCF "unrelated" Nothing 1.0, Nothing)] `shouldSatisfy` null

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

    describe "buildMethodTables compartment normalization" $ do
        -- Regression: BAFU categorizes air emissions as "emissions to air/low. pop.",
        -- ILCD CFs are keyed on bare "air/...". Without a compartment map applied to
        -- both sides, the lookup misses and the flow silently scores zero.
        it "scores zero without a compartment map (regression)" $ do
            fid <- nextRandom
            let flow = mkFlow fid "ammonia" "emissions to air/low. pop." Nothing
                cf = mkCFComp "ammonia" "air" "low. pop." 0.747
                tables = buildMethodTables OtherCFFamily M.empty M.empty [(cf, Nothing)]
                inventory = M.singleton fid 10.0
                flowDB = M.singleton fid flow
                score = loScore (computeLCIAScoreFromTables defaultUnitConfig M.empty flowDB inventory tables)
            score `shouldBe` 0.0

        it "bridges 'emissions to air' → 'air' via a medium-only rule" $ do
            fid <- nextRandom
            let flow = mkFlow fid "ammonia" "emissions to air/low. pop." Nothing
                cf = mkCFComp "ammonia" "air" "low. pop." 0.747
                cmap = M.singleton ("emissions to air", "", "") (Compartment "air" "" "")
                tables = buildMethodTables OtherCFFamily cmap M.empty [(cf, Nothing)]
                inventory = M.singleton fid 10.0
                flowDB = M.singleton fid flow
                score = loScore (computeLCIAScoreFromTables defaultUnitConfig M.empty flowDB inventory tables)
            score `shouldBe` 7.47

        it "bridges full (medium, sub, qual) triples for subcompartment rewrites" $ do
            fid <- nextRandom
            let flow = mkFlow fid "ammonia" "emissions to air/low. pop." Nothing
                -- ILCD-style CF on a different subcompartment than the BAFU flow.
                cf = mkCFComp "ammonia" "air" "non-urban air or from high stacks" 0.747
                cmap =
                    M.singleton
                        ("emissions to air", "low. pop.", "")
                        (Compartment "air" "non-urban air or from high stacks" "")
                tables = buildMethodTables OtherCFFamily cmap M.empty [(cf, Nothing)]
                inventory = M.singleton fid 10.0
                flowDB = M.singleton fid flow
                score = loScore (computeLCIAScoreFromTables defaultUnitConfig M.empty flowDB inventory tables)
            score `shouldBe` 7.47

        -- Regression: a failed unit conversion used to fall back to the
        -- unconverted quantity, contaminating the score with wrong-unit data.
        -- Now an unconvertible flow contributes 0, matching the "no-CF" branch.
        it "returns 0 when unit conversion fails (dimension mismatch)" $ do
            fid <- nextRandom
            let flow = mkFlow fid "co2" "air" Nothing
                cf = mkCF "co2" Nothing 1.0 -- mcfUnit = "kg"
                mapping = [(cf, Just (flow, ByUUID))]
                inventory = M.singleton fid 100.0
                flowDB = M.singleton fid flow
                unitDB = M.singleton nil (unitNamed "m") -- length, not mass
                score = loScore (computeLCIAScore defaultUnitConfig unitDB flowDB inventory mapping)
            score `shouldBe` 0.0

        it "applies conversion factor when units differ but are compatible (g→kg)" $ do
            fid <- nextRandom
            let flow = mkFlow fid "co2" "air" Nothing
                cf = mkCF "co2" Nothing 2.0 -- mcfUnit = "kg"
                mapping = [(cf, Just (flow, ByUUID))]
                inventory = M.singleton fid 1000.0 -- 1000 g
                flowDB = M.singleton fid flow
                unitDB = M.singleton nil (unitNamed "g")
                score = loScore (computeLCIAScore gKgUnitConfig unitDB flowDB inventory mapping)
            -- 1000 g → 1.0 kg, * cf 2.0 = 2.0
            score `shouldBe` 2.0

    describe "buildMethodTables unit-suffixed homonym collision" $ do
        -- SimaPro-style methods carry one CF row per denominator unit for the
        -- same substance ("Gas, natural/kg" = 43.1 MJ/kg and "Gas, natural/m3"
        -- = 34.5 MJ/m3). normalizeName strips the suffix, so both rows collide
        -- on one name key. The row whose raw name equals the flow's raw name
        -- must win: the other variant is dimensionally incompatible with the
        -- flow and would silently convert to 0.
        let kgDef = UnitDef [1, 0, 0, 0, 0, 0, 0, 0] 1.0
            m3Def = UnitDef [0, 3, 0, 0, 0, 0, 0, 0] 1.0
            cfg =
                UnitConfig
                    { ucDimensionOrder = []
                    , ucUnits = M.fromList [("kg", kgDef), ("m3", m3Def)]
                    , ucOriginalKeys = M.fromList [("kg", "kg"), ("m3", "m3")]
                    }
            cfPerKg = (mkCFComp "Gas, natural/kg" "natural resource" "" 43.1){mcfUnit = "kg"}
            cfPerM3 = (mkCFComp "Gas, natural/m3" "natural resource" "" 34.5){mcfUnit = "m3"}

        it "the verbatim-named CF wins over the higher-valued homonym" $ do
            fid <- nextRandom
            uidM3 <- nextRandom
            let flow = (mkFlow fid "Gas, natural/m3" "natural resource" (Just "in ground")){bfUnitId = uidM3}
                mappings = [(cfPerKg, Just (flow, ByName)), (cfPerM3, Just (flow, ByName))]
                unitDB = M.singleton uidM3 Unit{unitId = uidM3, unitName = "m3", unitSymbol = "m3", unitComment = ""}
                flowDB = M.singleton fid flow
                score ms = loScore (computeLCIAScoreFromTables cfg unitDB flowDB (M.singleton fid 2.0) (buildMethodTables OtherCFFamily M.empty M.empty ms))
            score mappings `shouldBe` 2.0 * 34.5
            -- insertion order must not matter
            score (reverse mappings) `shouldBe` 2.0 * 34.5

    describe "groundwater gate on wildcard fallbacks (read path)" $ do
        -- EF SimaPro exports leave immediate groundwater implicit (only
        -- "groundwater, long-term" carries an explicit zero), so SimaPro
        -- subcompartment semantics apply: an implicit sub inherits the
        -- unspecified CF. The USEtox gate must therefore block only the
        -- LONG-TERM groundwater fate — otherwise the method's explicit zero
        -- would be bypassed via the CAS bridge — and never the immediate one.
        cmap <- runIO $ do
            csv <- BL.readFile "data/compartments.csv"
            either (fail . ("compartments.csv: " <>)) pure (buildCompartmentMapFromCSV csv)
        let cfUns = (mkCFComp "Iron, ion" "water" "(unspecified)" 2108.5){mcfCAS = Just "7439-89-6"}
            cfLt = mkCFComp "Iron, ion" "water" "groundwater, long-term" 0.0
            scoreVia cm fam name mCas sub = do
                fid <- nextRandom
                mid <- nextRandom
                let flow = (mkFlow fid name "water" (Just sub)){bfCAS = mCas}
                    -- cfUns matched ByCAS on a sibling flow, so it also
                    -- populates the CAS bridge (mtCasCF).
                    matched = (mkFlow mid "Iron, ion" "water" Nothing){bfCAS = Just "7439-89-6"}
                    tables = buildMethodTables fam cm M.empty [(cfUns, Just (matched, ByCAS)), (cfLt, Nothing)]
                    flowDB = M.singleton fid flow
                pure (loScore (computeLCIAScoreFromTables defaultUnitConfig M.empty flowDB (M.singleton fid 1.0) tables))
            scoreFor = scoreVia M.empty

        it "lets a USEtox wildcard reach river, lake and IMMEDIATE groundwater" $ do
            scoreFor USEtoxFamily "Iron, ion" Nothing "river" `shouldReturn` 2108.5
            scoreFor USEtoxFamily "Iron, ion" Nothing "lake" `shouldReturn` 2108.5
            scoreFor USEtoxFamily "Iron, ion" Nothing "groundwater" `shouldReturn` 2108.5

        it "keeps the method's explicit long-term zero for a USEtox method" $
            scoreFor USEtoxFamily "Iron, ion" Nothing "groundwater, long-term" `shouldReturn` 0.0

        it "blocks the CAS bridge for a long-term groundwater flow under USEtox" $
            -- Name-mismatched flow sharing the CAS: without the gate it would
            -- borrow the immediate 2108.5 and bypass the explicit zero.
            scoreFor USEtoxFamily "Iron(2+)" (Just "7439-89-6") "groundwater, long-term" `shouldReturn` 0.0

        it "keeps every groundwater fallback for a non-USEtox method" $ do
            scoreFor OtherCFFamily "Iron, ion" Nothing "groundwater" `shouldReturn` 2108.5
            scoreFor OtherCFFamily "Iron(2+)" (Just "7439-89-6") "groundwater, long-term" `shouldReturn` 2108.5

        it "lands the ecoinvent spellings on the same gate (compartments.csv)" $ do
            -- "ground-" / "ground-, long-term" are the ecoinvent spellings of
            -- the same emissions. compartments.csv must map the immediate one
            -- to surface water (inherits the wildcard CF, like the SimaPro
            -- spelling) and the long-term one to "groundwater, long-term"
            -- (explicit zero wins, CAS bridge blocked) — otherwise the same
            -- emission scores differently depending on the source database.
            -- Uses the shipped CSV so the mapping itself is pinned.
            scoreVia cmap USEtoxFamily "Iron, ion" Nothing "ground-" `shouldReturn` 2108.5
            scoreVia cmap USEtoxFamily "Iron, ion" Nothing "ground-, long-term" `shouldReturn` 0.0
            scoreVia cmap USEtoxFamily "Iron(2+)" (Just "7439-89-6") "ground-, long-term" `shouldReturn` 0.0
            scoreVia cmap OtherCFFamily "Iron(2+)" (Just "7439-89-6") "ground-, long-term" `shouldReturn` 2108.5

    describe "inventoryContributions" $ do
        -- Regression: same fallback bug as computeLCIAScoreFromTables.
        it "yields zero contribution when unit conversion fails" $ do
            fid <- nextRandom
            let flow = mkFlow fid "co2" "air" Nothing
                cf = mkCF "co2" Nothing 1.0
                tables = buildMethodTables OtherCFFamily M.empty M.empty [(cf, Just (flow, ByUUID))]
                inventory = M.singleton fid 100.0
                flowDB = M.singleton fid flow
                unitDB = M.singleton nil (unitNamed "m")
                (contribs, unknowns) =
                    inventoryContributions defaultUnitConfig unitDB flowDB inventory tables
            unknowns `shouldBe` []
            map (\(_, _, c) -> c) contribs `shouldBe` [0.0]

    describe "convertForCharacterization" $ do
        -- Each row encodes a (flowUnit, cfUnit, qty) → expected mapping under a
        -- specific UnitConfig. Semantic groups: pass-through (units match, or no
        -- flow unit), refuse cross-dimension injection (→ 0), apply the factor
        -- when both units are known, and — when the CF unit is a result
        -- expression unknown to the UnitConfig — normalize the flow to its
        -- canonical base unit (a kg flow is unchanged; a g flow scales to kg), or
        -- hard-fail to 0 when that dimension defines no canonical base.
        let cases =
                [ ("units match by name", defaultUnitConfig, "kg", "kg", 5.0, 5.0)
                , ("cfUnit empty (method without unit)", defaultUnitConfig, "kg", "", 7.0, 7.0)
                , ("flowUnit empty (no metadata)", defaultUnitConfig, "", "kg", 9.0, 9.0)
                , ("LCIA-expression CF unit, flow already canonical → unchanged", defaultUnitConfig, "kg", "kg CO2 eq", 3.0, 3.0)
                , ("LCIA-expression CF unit, g flow → normalized to canonical kg", gKgUnitConfig, "g", "kg CO2 eq", 1000.0, 1.0)
                , ("LCIA-expression CF unit, flow dimension has no canonical base → 0", gOnlyUnitConfig, "g", "kg CO2 eq", 1000.0, 0.0)
                , ("dimensionally incompatible → 0", defaultUnitConfig, "m", "kg", 100.0, 0.0)
                , ("compatible units differ → apply factor (1000 g → 1.0 kg)", gKgUnitConfig, "g", "kg", 1000.0, 1.0)
                ]
        mapM_
            ( \(label, cfg, flowU, cfU, qty, expected) ->
                it label $
                    convertForCharacterization cfg flowU (CFUnit cfU) qty `shouldBe` expected
            )
            cases

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
            fmap bfId (findFlowBySynonym synDB byName "co2")
                `shouldBe` Just fid

    describe "pickByCompartment (matchMedium edge cases)" $ do
        it "null medium matches any flow" $ do
            fid <- nextRandom
            let flow = mkFlow fid "co2" "water" Nothing
                byName = M.singleton "co2" [flow]
                comp = Compartment "" "" ""
            fmap bfId (findFlowByNameComp byName "co2" (Just comp)) `shouldBe` Just fid

        it "medium isInfixOf category matches (air in urban air)" $ do
            fid1 <- nextRandom
            fid2 <- nextRandom
            let fUrbanAir = mkFlow fid1 "nox" "urban air" Nothing
                fWater = mkFlow fid2 "nox" "water" Nothing
                byName = M.singleton "nox" [fWater, fUrbanAir]
                comp = Compartment "air" "urban" ""
            fmap bfId (findFlowByNameComp byName "nox" (Just comp)) `shouldBe` Just fid1

    describe "fillBroadcastVector + computeLCIAScoreFromTables (Phase 1)" $ do
        let mkUnit uid name = Unit{unitId = uid, unitName = name, unitSymbol = name, unitComment = ""}

        it "scoring with empty broadcast equals scoring with filled broadcast (UUID match)" $ do
            fid <- nextRandom
            uidKg <- nextRandom
            let flow = (mkFlow fid "co2" "air" Nothing){bfUnitId = uidKg}
                cf = (mkCF "co2" Nothing 2.5){mcfUnit = "kg"}
                rawTables = buildMethodTables OtherCFFamily M.empty M.empty [(cf, Just (flow, ByUUID))]
                flowDB = M.singleton fid flow
                unitDB = M.singleton uidKg (mkUnit uidKg "kg")
                inv = M.fromList [(fid, 4.0 :: Double)]
                -- empty broadcast → legacy path
                legacyScore = loScore (computeLCIAScoreFromTables defaultUnitConfig unitDB flowDB inv rawTables)
                -- filled broadcast → fast path
                filled = fillBroadcastVector defaultUnitConfig unitDB flowDB rawTables
                fastScore = loScore (computeLCIAScoreFromTables defaultUnitConfig unitDB flowDB inv filled)
            legacyScore `shouldBe` (4.0 * 2.5 :: Double)
            fastScore `shouldBe` legacyScore

        it "pre-multiplied broadcast equals legacy when CF unit absorbs into flow unit" $ do
            -- Build a custom config with both kg and g (default config only has kg).
            -- 1 g = 0.001 kg → factor 0.001 against the SI base.
            let kgDef = UnitConversion.UnitDef [1, 0, 0, 0, 0, 0, 0, 0] 1.0
                gDef = UnitConversion.UnitDef [1, 0, 0, 0, 0, 0, 0, 0] 1.0e-3
                cfg =
                    UnitConversion.UnitConfig
                        { UnitConversion.ucDimensionOrder = []
                        , UnitConversion.ucUnits = M.fromList [("kg", kgDef), ("g", gDef)]
                        , UnitConversion.ucOriginalKeys = M.fromList [("kg", "kg"), ("g", "g")]
                        }
            fid <- nextRandom
            uidKg <- nextRandom
            let flow = (mkFlow fid "co2" "air" Nothing){bfUnitId = uidKg}
                cf = (mkCF "co2" Nothing 1.0e-3){mcfUnit = "g"}
                tables0 = buildMethodTables OtherCFFamily M.empty M.empty [(cf, Just (flow, ByUUID))]
                flowDB = M.singleton fid flow
                unitDB = M.singleton uidKg (mkUnit uidKg "kg")
                inv = M.fromList [(fid, 1.0 :: Double)]
                filled = fillBroadcastVector cfg unitDB flowDB tables0
                fast = loScore (computeLCIAScoreFromTables cfg unitDB flowDB inv filled)
                legacy = loScore (computeLCIAScoreFromTables cfg unitDB flowDB inv tables0)
            -- Parity: pre-multiplication must match the on-the-fly path.
            fast `shouldBe` legacy
            -- 1 kg × convert(kg→g, 1) × 1e-3 (CF) = 1 × 1000 × 1e-3 = 1.0.
            fast `shouldSatisfy` (\v -> abs (v - 1.0) < 1.0e-12)
            -- Broadcast must be filled.
            M.null (mtBroadcast filled) `shouldBe` False

        it "broadcast covers exact (name, medium, subcomp) cascade" $ do
            fid <- nextRandom
            uidKg <- nextRandom
            let flow = (mkFlow fid "co2" "air" (Just "high pop")){bfUnitId = uidKg}
                cf = (mkCFComp "co2" "air" "high pop" 3.0){mcfUnit = "kg"}
                tables0 = buildMethodTables OtherCFFamily M.empty M.empty [(cf, Just (flow, ByName))]
                flowDB = M.singleton fid flow
                unitDB = M.singleton uidKg (mkUnit uidKg "kg")
                inv = M.fromList [(fid, 2.0 :: Double)]
                filled = fillBroadcastVector defaultUnitConfig unitDB flowDB tables0
                fast = loScore (computeLCIAScoreFromTables defaultUnitConfig unitDB flowDB inv filled)
                legacy = loScore (computeLCIAScoreFromTables defaultUnitConfig unitDB flowDB inv tables0)
            fast `shouldBe` legacy
            fast `shouldBe` (2.0 * 3.0 :: Double)

        it "broadcast covers fallback (name, medium) cascade" $ do
            fid <- nextRandom
            uidKg <- nextRandom
            -- Flow has subcomp "high pop", but CF only has medium-level entry (subcomp "")
            let flow = (mkFlow fid "co2" "air" (Just "high pop")){bfUnitId = uidKg}
                cf = (mkCFComp "co2" "air" "" 5.0){mcfUnit = "kg"}
                tables0 = buildMethodTables OtherCFFamily M.empty M.empty [(cf, Just (flow, ByName))]
                flowDB = M.singleton fid flow
                unitDB = M.singleton uidKg (mkUnit uidKg "kg")
                inv = M.fromList [(fid, 1.0 :: Double)]
                filled = fillBroadcastVector defaultUnitConfig unitDB flowDB tables0
                fast = loScore (computeLCIAScoreFromTables defaultUnitConfig unitDB flowDB inv filled)
                legacy = loScore (computeLCIAScoreFromTables defaultUnitConfig unitDB flowDB inv tables0)
            fast `shouldBe` legacy
            fast `shouldBe` (5.0 :: Double)

        it "inventory UUID outside broadcast falls back to legacy lookup (cross-DB)" $ do
            fidLocal <- nextRandom
            fidExtra <- nextRandom -- in inventory but NOT in flowDB at fill time
            uidKg <- nextRandom
            let flowLocal = (mkFlow fidLocal "co2" "air" Nothing){bfUnitId = uidKg}
                cf = (mkCF "co2" Nothing 1.5){mcfUnit = "kg"}
                tables0 = buildMethodTables OtherCFFamily M.empty M.empty [(cf, Just (flowLocal, ByUUID))]
                flowDBAtBuild = M.singleton fidLocal flowLocal
                unitDB = M.singleton uidKg (mkUnit uidKg "kg")
                filled = fillBroadcastVector defaultUnitConfig unitDB flowDBAtBuild tables0
                -- Scoring time: inventory has fidExtra (cross-DB flow added later)
                inv = M.fromList [(fidLocal, 2.0 :: Double), (fidExtra, 7.0)]
                fast = loScore (computeLCIAScoreFromTables defaultUnitConfig unitDB flowDBAtBuild inv filled)
            -- fidLocal contributes 2.0 * 1.5 = 3.0; fidExtra has no CF → 0.
            -- The fallback path must NOT crash on the unknown UUID.
            fast `shouldBe` (3.0 :: Double)

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
            let flow = (mkFlow fid "Carbon dioxide" "air" Nothing){bfCAS = Nothing}
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
                flow = (mkFlow fid "Carbon dioxide" "air" Nothing){bfCAS = Nothing}
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
                        { bfCAS = Just "124-38-9"
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
                tables = buildMethodTables OtherCFFamily M.empty M.empty []
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
                `shouldBe` []

        it "drops flows below the absolute-weight threshold" $ do
            big <- nextRandom
            small <- nextRandom
            let bigFlow = mkFlow big "tiny stuff" "air" Nothing
                smallFlow = mkFlow small "huge stuff" "air" Nothing
                inv = M.fromList [(big, 999.0), (small, 1.0)]
                flowDB = M.fromList [(big, bigFlow), (small, smallFlow)]
                tables = buildMethodTables OtherCFFamily M.empty M.empty []
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
            -- Only the big flow (99.9% of mass) clears the 50% threshold.
            map ucfFlowName result `shouldBe` ["tiny stuff"]

        it "skips flows that DO have a CF (they're characterized)" $ do
            fid <- nextRandom
            let flow = mkFlow fid "co2" "air" Nothing
                cf = (mkCF "co2" Nothing 1.0){mcfFlowRef = fid}
                tables = buildMethodTables OtherCFFamily M.empty M.empty [(cf, Just (flow, ByUUID))]
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
                `shouldBe` []

    describe "cfFamily" $ do
        it "classifies USEtox toxicity units, case/whitespace-insensitively" $ do
            cfFamily "CTUe" `shouldBe` USEtoxFamily
            cfFamily "CTUh" `shouldBe` USEtoxFamily
            cfFamily " ctue " `shouldBe` USEtoxFamily
        it "classifies every other unit (incl. unknown/empty) as OtherCFFamily" $ do
            cfFamily "kg P eq" `shouldBe` OtherCFFamily
            cfFamily "unknown" `shouldBe` OtherCFFamily
            cfFamily "" `shouldBe` OtherCFFamily

    describe "wildcard (pattern) CFs" $ do
        let flowDB = M.fromList [(bfId f, f) | f <- allFlows]
            allFlows =
                [ occAnnual
                , occOrchard
                , transformation
                , waterRiver
                , waterWell
                , methaneAir
                , methaneWater
                ]
            occAnnual = mkFlow (u 1) "Occupation, annual crop" "resource" Nothing
            occOrchard = mkFlow (u 2) "Occupation, permanent crop, fruit" "resource" Nothing
            transformation = mkFlow (u 3) "Transformation, to annual crop" "resource" Nothing
            waterRiver = mkFlow (u 4) "Water, river" "resource" Nothing
            waterWell = mkFlow (u 7) "Water, well" "resource" (Just "in ground")
            methaneAir = (mkFlow (u 5) "Methane, fossil" "air" Nothing){bfCAS = Just "74-82-8"}
            methaneWater = (mkFlow (u 6) "Methane, fossil" "water" Nothing){bfCAS = Just "74-82-8"}
            u = uuidFromInt
            -- Compare by UUID: BiosphereFlow has no Eq/Show instance.
            expandedIds = map (fmap (bfId . fst) . snd) . fst

        it "detects a trailing-star name as a pattern, a literal name as not" $ do
            isPatternCF (mkCF "Occupation*" Nothing 1.0) `shouldBe` True
            isPatternCF (mkCF "*" Nothing 1.0) `shouldBe` True
            isPatternCF (mkCF "Occupation, annual crop" Nothing 1.0) `shouldBe` False

        it "expands a prefix pattern to every flow of the compartment, none else" $ do
            let cf = mkCFComp "Occupation*" "natural resource" "" 1.0
            expandedIds (expandPatternCF flowDB cf)
                `shouldMatchList` map (Just . bfId) [occAnnual, occOrchard]

        it "materializes each match with the flow's own identity, keeping value and unit" $ do
            let cf = (mkCFComp "Occupation*" "natural resource" "" 1.0){mcfUnit = "m2a"}
                (rows, warnings) = expandPatternCF flowDB cf
            warnings `shouldBe` []
            [(mcfFlowRef m, mcfFlowName m, mcfValue m, mcfUnit m) | (m, _) <- rows]
                `shouldMatchList` [(bfId f, bfName f, 1.0, "m2a") | f <- [occAnnual, occOrchard]]

        it "honors the sub-compartment a pattern row states, widens without one" $ do
            let inGround = mkCFComp "Water*" "natural resource" "in ground" 1.0
                anySub = mkCFComp "Water*" "natural resource" "" 1.0
            expandedIds (expandPatternCF flowDB inGround) `shouldBe` [Just (bfId waterWell)]
            expandedIds (expandPatternCF flowDB anySub)
                `shouldMatchList` map (Just . bfId) [waterRiver, waterWell]

        it "a bare * with a CAS expands by CAS, filtered by the row's compartment" $ do
            let cf = (mkCFComp "*" "air" "" 1.0){mcfCAS = Just "74-82-8"}
            expandedIds (expandPatternCF flowDB cf) `shouldBe` [Just (bfId methaneAir)]

        it "a pattern matching no flow surfaces one unmatched row and a warning" $ do
            let cf = mkCFComp "Uranium*" "natural resource" "" 1.0
                (rows, warnings) = expandPatternCF flowDB cf
            map (fmap (bfId . fst) . snd) rows `shouldBe` [Nothing]
            length warnings `shouldBe` 1

        it "a bare * constrained by nothing is refused, not matched to everything" $ do
            let cf = mkCF "*" Nothing 1.0
                (rows, warnings) = expandPatternCF flowDB cf
            map (fmap (bfId . fst) . snd) rows `shouldBe` [Nothing]
            length warnings `shouldBe` 1

        it "mapMethodFlows resolves literal rows via the cascade and expands patterns" $ do
            let method =
                    Method
                        { methodId = nil
                        , methodName = "Land occupied"
                        , methodDescription = Nothing
                        , methodUnit = "m2a"
                        , methodCategory = "Land occupied"
                        , methodMethodology = Nothing
                        , methodFactors =
                            [ mkCFComp "Water, river" "natural resource" "" 1.0
                            , mkCFComp "Occupation*" "natural resource" "" 1.0
                            ]
                        }
                ctx = MapContext flowDB (byName allFlows) M.empty emptySynonymDB M.empty M.empty
            mappings <- mapMethodFlows ctx method
            map (fmap (bfId . fst) . snd) mappings
                `shouldMatchList` map (Just . bfId) [waterRiver, occAnnual, occOrchard]

    -- Lint of the shipped method file, like RegistryLintSpec for data/flows.csv:
    -- a header typo or a misplaced comment would otherwise ship silently.
    describe "shipped plain-indicators method (data/methods/plain-indicators.csv)" $ do
        parsed <- runIO (parseMethodCSVBytes <$> BS.readFile "data/methods/plain-indicators.csv")

        it "parses into its seven categories" $
            fmap (map methodName) parsed
                `shouldBe` Right ["Land occupied", "Water used", "Fossil CO2", "Methane", "Primary energy", "Waste heat", "Cadmium"]

        it "carries its wildcard rows as patterns" $
            [mcfFlowName cf | Right ms <- [parsed], m <- ms, cf <- methodFactors m, isPatternCF cf]
                `shouldBe` ["Occupation*", "Water*", "Energy, *", "Heat, waste*"]

tShow :: (Show a) => a -> Text
tShow = T.pack . show

-- | Deterministic UUID for wildcard-CF fixtures.
uuidFromInt :: Int -> UUID
uuidFromInt n = fromWords (fromIntegral n) 0 0 0

-- | Name index the cascade reads, keyed like the loader keys it.
byName :: [BiosphereFlow] -> M.Map Text [BiosphereFlow]
byName fs = M.fromListWith (++) [(normalizeName (bfName f), [f]) | f <- fs]
