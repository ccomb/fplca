{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module WasteCrossDBSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.UUID as UUID
import Database.CrossLinking
import SynonymDB (emptySynonymDB)
import Test.Hspec
import qualified UnitConversion as UC

{- | Unit tests for the exact-match cross-DB waste-treatment linker.

The linker honors only author-provided alignment: same flow UUID or
byte-exact normalized name on the supplier's reference product. There is
no synonym graph, no compound-name extraction, no widening, no scoring.
Multiple databases offering a candidate resolves to 'WasteAmbiguous',
never to a first-wins auto-pick.
-}
spec :: Spec
spec = describe "findWasteTreatmentAcrossDatabases" $ do
    let supplierFor :: Text -> SupplierEntry
        supplierFor nm =
            SupplierEntry
                { seActivityUUID = UUID.fromWords 1 1 1 1
                , seProductUUID = UUID.fromWords 2 2 2 2
                , seLocation = "RoW"
                , seUnit = "kg"
                , seProductName = nm
                , seRefSign = 1.0
                }

        idbWith :: Text -> [(UUID.UUID, SupplierEntry)] -> [(Text, SupplierEntry)] -> IndexedDatabase
        idbWith name uuidEntries nameEntries =
            IndexedDatabase
                { idbName = name
                , idbByProductName = M.empty
                , idbBySynonymGroup = M.empty
                , idbWasteTreatmentByFlowUUID = M.fromListWith (++) [(u, [e]) | (u, e) <- uuidEntries]
                , idbWasteTreatmentByCanonicalName = M.fromListWith (++) [(n, [e]) | (n, e) <- nameEntries]
                , idbByActivityProduct = M.empty
                }

        ctxWith :: [IndexedDatabase] -> LinkingContext
        ctxWith dbs =
            LinkingContext
                { lcIndexedDatabases = dbs
                , lcSynonymDB = emptySynonymDB
                , lcUnitConfig = UC.defaultUnitConfig
                , lcThreshold = defaultLinkingThreshold
                , lcLocationHierarchy = M.empty
                , lcGeographyPolicy = GeoExact
                , lcSupplierAliases = emptyAliasMap
                }

        wasteUUID = UUID.fromWords 0xa 0xb 0xc 0xd
        entry = supplierFor "Organic carbon, placed in landfill"

        expectMatched dbExpected = \case
            WasteMatched _ dbN -> dbN `shouldBe` dbExpected
            WasteAmbiguous dbs -> expectationFailure $ "expected match in " <> show dbExpected <> ", got ambiguous across " <> show dbs
            WasteNoMatch -> expectationFailure $ "expected match in " <> show dbExpected <> ", got no match"

        expectNoMatch = \case
            WasteNoMatch -> pure ()
            WasteMatched _ dbN -> expectationFailure $ "expected no match, got match in " <> show dbN
            WasteAmbiguous dbs -> expectationFailure $ "expected no match, got ambiguous across " <> show dbs

    it "matches exactly when a single DB offers a UUID-aligned treatment" $ do
        let ctx = ctxWith [idbWith "ecoinvent" [(wasteUUID, entry)] []]
        expectMatched "ecoinvent" (findWasteTreatmentAcrossDatabases ctx wasteUUID "Organic carbon, placed in landfill")

    it "matches via byte-exact canonical name when UUIDs differ" $ do
        let otherUUID = UUID.fromWords 99 99 99 99
            ctx = ctxWith [idbWith "ecoinvent" [(otherUUID, entry)] [("organic carbon, placed in landfill", entry)]]
        expectMatched "ecoinvent" (findWasteTreatmentAcrossDatabases ctx wasteUUID "Organic carbon, placed in landfill")

    it "name fallback is case- and whitespace-tolerant via normalizeText" $ do
        let otherUUID = UUID.fromWords 77 77 77 77
            ctx = ctxWith [idbWith "ecoinvent" [(otherUUID, entry)] [("organic carbon, placed in landfill", entry)]]
        expectMatched "ecoinvent" (findWasteTreatmentAcrossDatabases ctx wasteUUID "  Organic Carbon, Placed in Landfill  ")

    it "stays orphan when no DB knows the waste flow" $ do
        let ctx = ctxWith [idbWith "ecoinvent" [] []]
        expectNoMatch (findWasteTreatmentAcrossDatabases ctx wasteUUID "Plastic, waste, landfill")

    it "stays orphan when only a similar-but-not-equal name exists (no fuzzy)" $ do
        let ctx = ctxWith [idbWith "ecoinvent" [] [("organic carbon, landfill", entry)]]
        expectNoMatch (findWasteTreatmentAcrossDatabases ctx wasteUUID "Organic carbon, placed in landfill")

    it "stays orphan when one DB offers multiple within-DB candidates (author must pick)" $ do
        let entryFR = (supplierFor "Organic carbon, landfill, FR"){seLocation = "FR"}
            entryDE = (supplierFor "Organic carbon, landfill, DE"){seLocation = "DE"}
            ecoinventMulti =
                IndexedDatabase
                    { idbName = "ecoinvent"
                    , idbByProductName = M.empty
                    , idbBySynonymGroup = M.empty
                    , idbWasteTreatmentByFlowUUID = M.singleton wasteUUID [entryFR, entryDE]
                    , idbWasteTreatmentByCanonicalName = M.empty
                    , idbByActivityProduct = M.empty
                    }
            ctx = ctxWith [ecoinventMulti]
        expectNoMatch (findWasteTreatmentAcrossDatabases ctx wasteUUID "Organic carbon")

    it "reports WasteAmbiguous when two databases each offer a single candidate" $ do
        let ctx =
                ctxWith
                    [ idbWith "ecoinvent" [(wasteUUID, entry)] []
                    , idbWith "wfldb" [(wasteUUID, entry)] []
                    ]
        case findWasteTreatmentAcrossDatabases ctx wasteUUID "Organic carbon, placed in landfill" of
            WasteAmbiguous dbs -> do
                length dbs `shouldBe` 2
                ("ecoinvent" `elem` dbs) `shouldBe` True
                ("wfldb" `elem` dbs) `shouldBe` True
            WasteMatched _ dbN -> expectationFailure $ "expected ambiguous, got match in " <> show dbN
            WasteNoMatch -> expectationFailure "expected ambiguous, got no match"

    it "prefers UUID over name when a UUID match wins in one DB and only a name match exists in another" $ do
        let ctx =
                ctxWith
                    [ idbWith "ecoinvent" [(wasteUUID, entry)] []
                    , idbWith "wfldb" [] [("organic carbon, placed in landfill", entry)]
                    ]
        expectMatched "ecoinvent" (findWasteTreatmentAcrossDatabases ctx wasteUUID "Organic carbon, placed in landfill")
