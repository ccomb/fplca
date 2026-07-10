{-# LANGUAGE OverloadedStrings #-}

{- | Cross-database linking by EcoSpold2 @activityLinkId@ identity.

A partial EcoSpold2 import carries non-nil @activityLinkId@s pointing at
background activities it does not ship. These tests pin the resolution cascade
'Database.Loader.findExchangeCrossDBLink' runs for such inputs:

1. exact @(activityLinkId, flowId)@ identity against a loaded dependency;
2. attribute matching (name/location/unit) when the exact identity is absent —
   the cross-version case, flagged in 'cdlAttributeFallbacks';
3. and that an input whose target is present *in the same database* resolves
   internally and gets no cross-DB link (no double counting).
-}
module CrossDBActivityLinkSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.UUID as UUID
import Database.CrossLinking (
    IndexedDatabase,
    LinkingContext (..),
    buildIndexedDatabase,
    defaultLinkingThreshold,
    findSupplierByActivityProduct,
    locationHierarchy,
 )
import Database.Loader (collectStagedDanglingProductNames, findAllCrossDBLinks)
import SynonymDB (emptySynonymDB)
import Test.Hspec
import Types
import UnitConversion (defaultUnitConfig)

-- ---------------------------------------------------------------------------
-- UUIDs
-- ---------------------------------------------------------------------------

cAct, cProd, supAct, supProd, oldAct, newAct, supProd2, kgUnit :: UUID.UUID
cAct = read "c0000000-0000-0000-0000-000000000001"
cProd = read "c0000000-0000-0000-0000-000000000002"
supAct = read "50000000-0000-0000-0000-000000000001"
supProd = read "50000000-0000-0000-0000-000000000002"
oldAct = read "01d00000-0000-0000-0000-000000000001"
newAct = read "0ee00000-0000-0000-0000-000000000001"
supProd2 = read "50000000-0000-0000-0000-000000000003"
kgUnit = read "00000000-0000-0000-0000-0000000000a1"

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

mkFlow :: UUID.UUID -> Text -> TechnosphereFlow
mkFlow fid name =
    TechnosphereFlow
        { tfId = fid
        , tfName = name
        , tfUnitId = kgUnit
        , tfSynonyms = M.empty
        , tfCAS = Nothing
        , tfSubstanceId = Nothing
        }

mkActivity :: Text -> Text -> [Exchange] -> Activity
mkActivity name loc exs =
    Activity
        { activityName = name
        , activityDescription = []
        , activitySynonyms = M.empty
        , activityClassification = M.empty
        , activityLocation = loc
        , activityUnit = "kg"
        , exchanges = exs
        , activityParams = M.empty
        , activityParamExprs = M.empty
        , activityAllocationPercent = Nothing
        , activityAllocationFormula = Nothing
        , activityNativeType = Nothing
        , activityNativeId = Nothing
        }

refEx :: UUID.UUID -> Exchange
refEx fid =
    TechnosphereExchange
        { techFlowId = fid
        , techAmount = 1.0
        , techUnitId = kgUnit
        , techRole = ReferenceProduct
        , techActivityLinkId = UUID.nil
        , techProcessLinkId = Nothing
        , techLocation = "GLO"
        , techComment = Nothing
        , techPedigree = Nothing
        }

-- | An input for @flowId@, linked to producer activity @linkId@ (nil = unlinked).
inputEx :: UUID.UUID -> UUID.UUID -> Exchange
inputEx flowId linkId =
    TechnosphereExchange
        { techFlowId = flowId
        , techAmount = 2.0
        , techUnitId = kgUnit
        , techRole = Input
        , techActivityLinkId = linkId
        , techProcessLinkId = Nothing
        , techLocation = "GLO"
        , techComment = Nothing
        , techPedigree = Nothing
        }

mkDB :: [((UUID.UUID, UUID.UUID), Activity)] -> [TechnosphereFlow] -> SimpleDatabase
mkDB acts flows =
    SimpleDatabase
        { sdbActivities = M.fromList acts
        , sdbTechFlows = M.fromList [(tfId f, f) | f <- flows]
        , sdbBioFlows = M.empty
        , sdbWasteFlows = M.empty
        , sdbUnits = M.singleton kgUnit (Unit kgUnit "kg" "" "")
        }

ctxFor :: [IndexedDatabase] -> LinkingContext
ctxFor idbs =
    LinkingContext
        { lcIndexedDatabases = idbs
        , lcSynonymDB = emptySynonymDB
        , lcUnitConfig = defaultUnitConfig
        , lcThreshold = defaultLinkingThreshold
        , lcLocationHierarchy = locationHierarchy
        , lcGeographyPolicy = GeoGlobal
        , lcSupplierAliases = Nothing
        }

runLinks :: SimpleDatabase -> [IndexedDatabase] -> CrossDBLinkingStats
runLinks fg idbs =
    findAllCrossDBLinks (ctxFor idbs) (sdbTechFlows fg) (sdbWasteFlows fg) (sdbUnits fg) (sdbActivities fg)

indexBg :: SimpleDatabase -> IndexedDatabase
indexBg = buildIndexedDatabase "bg" emptySynonymDB

-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
    describe "findSupplierByActivityProduct" $ do
        let bg = indexBg (mkDB [((supAct, supProd), mkActivity "widget" "GLO" [refEx supProd])] [mkFlow supProd "widget"])
        it "returns the supplier for an exact (activity, product) identity" $
            map snd (findSupplierByActivityProduct [bg] supAct supProd) `shouldBe` ["bg"]
        it "returns empty when no database ships that identity" $
            null (findSupplierByActivityProduct [bg] oldAct supProd) `shouldBe` True

    describe "findExchangeCrossDBLink (activityLinkId cascade)" $ do
        it "links a dangling non-nil input to a background supplier by exact identity" $ do
            let consumer = mkActivity "consumer" "GLO" [refEx cProd, inputEx supProd supAct]
                fg = mkDB [((cAct, cProd), consumer)] [mkFlow cProd "consumer-product", mkFlow supProd "widget"]
                bg = indexBg (mkDB [((supAct, supProd), mkActivity "widget" "GLO" [refEx supProd])] [mkFlow supProd "widget"])
                stats = runLinks fg [bg]
            map (\l -> (cdlSupplierActUUID l, cdlSupplierProdUUID l, cdlSourceDatabase l)) (cdlLinks stats)
                `shouldBe` [(supAct, supProd, "bg")]
            -- Exact identity is not a cross-version stitch.
            cdlAttributeFallbacks stats `shouldBe` []

        it "does not emit a cross-DB link when the target resolves internally" $ do
            let consumer = mkActivity "consumer" "GLO" [refEx cProd, inputEx supProd supAct]
                supplier = mkActivity "widget" "GLO" [refEx supProd]
                -- supplier present in the SAME database as the consumer
                fg = mkDB [((cAct, cProd), consumer), ((supAct, supProd), supplier)] [mkFlow cProd "consumer-product", mkFlow supProd "widget"]
                -- and also offered by a dependency, to prove internal wins
                bg = indexBg (mkDB [((supAct, supProd), supplier)] [mkFlow supProd "widget"])
            cdlLinks (runLinks fg [bg]) `shouldBe` []

        it "falls back to attribute matching and flags a cross-version stitch" $ do
            -- Consumer references a release whose activity UUID (oldAct) the
            -- background does not ship; the background offers the same product
            -- under a different activity UUID (newAct).
            let consumer = mkActivity "consumer" "GLO" [refEx cProd, inputEx supProd oldAct]
                fg = mkDB [((cAct, cProd), consumer)] [mkFlow cProd "consumer-product", mkFlow supProd "widget"]
                bg = indexBg (mkDB [((newAct, supProd2), mkActivity "widget" "GLO" [refEx supProd2])] [mkFlow supProd2 "widget"])
                stats = runLinks fg [bg]
            map (\l -> (cdlSupplierActUUID l, cdlSupplierProdUUID l, cdlSourceDatabase l)) (cdlLinks stats)
                `shouldBe` [(newAct, supProd2, "bg")]
            map (\a -> (afProduct a, afSourceDatabase a)) (cdlAttributeFallbacks stats)
                `shouldBe` [("widget", "bg")]

        it "does not flag a nil-link input matched by attributes" $ do
            let consumer = mkActivity "consumer" "GLO" [refEx cProd, inputEx supProd UUID.nil]
                fg = mkDB [((cAct, cProd), consumer)] [mkFlow cProd "consumer-product", mkFlow supProd "widget"]
                bg = indexBg (mkDB [((supAct, supProd), mkActivity "widget" "GLO" [refEx supProd])] [mkFlow supProd "widget"])
                stats = runLinks fg [bg]
            length (cdlLinks stats) `shouldBe` 1
            cdlAttributeFallbacks stats `shouldBe` []

    describe "collectStagedDanglingProductNames (duplicate product flow)" $ do
        it "still names a residual gap when only one of two same-product inputs is covered" $ do
            -- One activity consumes "widget" twice, from two suppliers (same
            -- flowId, different activityLinkId); neither supplier is in the
            -- database. A single cross-DB link covers one of them — the other
            -- must still be reported, not masked by the shared (act, prod, flow)
            -- triple. The engine resolves demands by (activityLinkId, flowId),
            -- so coverage is counted per occurrence, not tested for membership.
            let consumer = mkActivity "consumer" "GLO" [refEx cProd, inputEx supProd supAct, inputEx supProd oldAct]
                fg = mkDB [((cAct, cProd), consumer)] [mkFlow cProd "consumer-product", mkFlow supProd "widget"]
                coveringLink =
                    CrossDBLink
                        { cdlConsumerActUUID = cAct
                        , cdlConsumerProdUUID = cProd
                        , cdlConsumerFlowId = supProd
                        , cdlSupplierActUUID = supAct
                        , cdlSupplierProdUUID = supProd
                        , cdlCoefficient = 2.0
                        , cdlExchangeUnit = "kg"
                        , cdlFlowName = "widget"
                        , cdlLocation = "GLO"
                        , cdlSourceDatabase = "bg"
                        , cdlTiedAlternatives = []
                        }
            collectStagedDanglingProductNames fg [coveringLink] `shouldBe` M.singleton "widget" 1
