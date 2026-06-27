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

-- | UnitConfig whose mass dimension has NO canonical base (g only, no kg at
-- factor 1.0), so 'normalizeToCanonical' fails — exercises the result-expression
-- branch's hard-fail to 0.
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
                tables = buildMethodTables M.empty M.empty [(cf, Nothing)]
                inventory = M.singleton fid 10.0
                flowDB = M.singleton fid flow
                score = loScore (computeLCIAScoreFromTables defaultUnitConfig M.empty flowDB inventory tables)
            score `shouldBe` 0.0

        it "bridges 'emissions to air' → 'air' via a medium-only rule" $ do
            fid <- nextRandom
            let flow = mkFlow fid "ammonia" "emissions to air/low. pop." Nothing
                cf = mkCFComp "ammonia" "air" "low. pop." 0.747
                cmap = M.singleton ("emissions to air", "", "") (Compartment "air" "" "")
                tables = buildMethodTables cmap M.empty [(cf, Nothing)]
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
                tables = buildMethodTables cmap M.empty [(cf, Nothing)]
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

    describe "inventoryContributions" $ do
        -- Regression: same fallback bug as computeLCIAScoreFromTables.
        it "yields zero contribution when unit conversion fails" $ do
            fid <- nextRandom
            let flow = mkFlow fid "co2" "air" Nothing
                cf = mkCF "co2" Nothing 1.0
                tables = buildMethodTables M.empty M.empty [(cf, Just (flow, ByUUID))]
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
                    convertForCharacterization cfg flowU cfU qty `shouldBe` expected
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
                rawTables = buildMethodTables M.empty M.empty [(cf, Just (flow, ByUUID))]
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
                tables0 = buildMethodTables M.empty M.empty [(cf, Just (flow, ByUUID))]
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
                tables0 = buildMethodTables M.empty M.empty [(cf, Just (flow, ByName))]
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
                tables0 = buildMethodTables M.empty M.empty [(cf, Just (flow, ByName))]
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
                tables0 = buildMethodTables M.empty M.empty [(cf, Just (flowLocal, ByUUID))]
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
                tables = buildMethodTables M.empty M.empty []
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
                tables = buildMethodTables M.empty M.empty []
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
                tables = buildMethodTables M.empty M.empty [(cf, Just (flow, ByUUID))]
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

tShow :: (Show a) => a -> Text
tShow = T.pack . show
