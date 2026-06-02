{-# LANGUAGE OverloadedStrings #-}

{- | Tests for the DELETE-BY-SELECTION primitive
('Database.Edit.deleteActivities', 'Database.Edit.resolveDeleteSelection' and
the effectful 'Database.Edit.deleteActivitiesInDB').

Delete is reconstruction over an immutable 'Database':

* deleting a set of ProcessIds drops exactly those activities and renumbers the
  survivors, rebuilding the interning tables, indexes, and sparse matrices;
* an exchange in a surviving activity that pointed at a deleted activity is
  UNLINKED (activity link reset to nil), never silently dropped — the database
  is left ready for relinking;
* the selection resolver computes @(filtered ∪ extra) \\ keep@, so the UI's
  "delete the whole filtered set" honours per-row checkbox overrides;
* deleting by filter removes the WHOLE matching set, independent of pagination.
-}
module DeleteSelectionSpec (spec) where

import Control.Concurrent.STM (atomically, modifyTVar', readTVarIO)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Test.Hspec

import Config (DatabaseConfig (..), defaultConfig)
import Database (buildDatabaseWithMatrices)
import Database.Edit (
    DeleteSelection (..),
    deleteActivities,
    deleteActivitiesInDB,
    resolveDeleteSelection,
 )
import Database.Manager (
    DatabaseManager (..),
    LoadedDatabase (..),
    initDatabaseManager,
 )
import SharedSolver (SharedSolver, createSharedSolver)
import Types (
    Activity (..),
    Database (..),
    Exchange (..),
    GeographyPolicy (..),
    ProcessId,
    SparseTriple (..),
    TechRole (..),
    TechnosphereFlow (..),
    UUID,
    Unit (..),
    findProcessId,
    processIdToText,
 )
import UnitConversion (defaultUnitConfig)

spec :: Spec
spec = describe "Database.Edit delete-by-selection primitive" $ do
    describe "resolveDeleteSelection" $ do
        it "deletes the whole filtered set when there are no overrides" $
            S.fromList (resolveDeleteSelection (DeleteSelection [0, 1, 2, 3] [] []))
                `shouldBe` S.fromList [0, 1, 2, 3]

        it "spares the explicit keep set (unticked checkboxes)" $
            S.fromList (resolveDeleteSelection (DeleteSelection [0, 1, 2, 3] [1, 2] []))
                `shouldBe` S.fromList [0, 3]

        it "adds the explicit extra set (ticked rows outside the filter)" $
            S.fromList (resolveDeleteSelection (DeleteSelection [0, 1] [] [5, 6]))
                `shouldBe` S.fromList [0, 1, 5, 6]

        it "lets keep win over both filtered and extra" $
            S.fromList (resolveDeleteSelection (DeleteSelection [0, 1] [1, 5] [5, 6]))
                `shouldBe` S.fromList [0, 6]

    describe "deleteActivities (pure rebuild)" $ do
        it "removes exactly the requested activities and renumbers survivors" $ do
            db <- buildOrFail (chainDB 100)
            dbActivityCount db `shouldBe` 3
            -- Delete the leaf supplier (index 0 = "supplier" sorts first).
            let supplierPid = pidFor db 100 0
            case deleteActivities [supplierPid] db of
                Left err -> expectationFailure ("deleteActivities failed: " <> show err)
                Right db' -> do
                    dbActivityCount db' `shouldBe` 2
                    V.length (dbActivities db') `shouldBe` 2
                    -- ProcessId table and lookup agree after renumbering.
                    V.length (dbProcessIdTable db') `shouldBe` 2
                    M.size (dbProcessIdLookup db') `shouldBe` 2
                    -- The biosphere/tech matrices were rebuilt: the deleted
                    -- supplier's row no longer contributes any tech triple.
                    U.length (dbTechnosphereTriples db')
                        `shouldSatisfy` (< U.length (dbTechnosphereTriples db))

        it "unlinks surviving exchanges that pointed at a deleted activity" $ do
            db <- buildOrFail (chainDB 200)
            let supplierPid = pidFor db 200 0
            case deleteActivities [supplierPid] db of
                Left err -> expectationFailure ("deleteActivities failed: " <> show err)
                Right db' -> do
                    -- The mid activity kept its input exchange, but the link to
                    -- the deleted supplier is now nil — ready for relinking.
                    let midInputLinks =
                            [ (techActivityLinkId ex, techProcessLinkId ex)
                            | act <- V.toList (dbActivities db')
                            , activityName act == "mid"
                            , ex@TechnosphereExchange{techRole = Input} <- exchanges act
                            ]
                    midInputLinks `shouldBe` [(UUID.nil, Nothing)]

        it "keeps a multi-product link target when only one product is deleted" $ do
            db <- buildOrFail (multiProductDB 300)
            -- supplier exposes two products; delete only product B.
            let prodBPid = pidFor2 db (mkUUID 301) (mkUUID 303)
            case deleteActivities [prodBPid] db of
                Left err -> expectationFailure ("deleteActivities failed: " <> show err)
                Right db' -> do
                    dbActivityCount db' `shouldBe` 2
                    -- The consumer linking to product A (a surviving key) keeps its link.
                    let consumerLinks =
                            [ techActivityLinkId ex
                            | act <- V.toList (dbActivities db')
                            , activityName act == "consumer-A"
                            , ex@TechnosphereExchange{techRole = Input} <- exchanges act
                            ]
                    consumerLinks `shouldBe` [mkUUID 301]

        it "unlinks a surviving waste generator when its treatment is deleted" $ do
            db <- buildOrFail (wasteDB 350)
            -- The treatment activity sorts first; delete it.
            let treatmentPid = pidFor db 350 0
            case deleteActivities [treatmentPid] db of
                Left err -> expectationFailure ("deleteActivities failed: " <> show err)
                Right db' -> do
                    dbActivityCount db' `shouldBe` 1
                    -- The surviving generator's waste output no longer resolves to
                    -- the deleted treatment: the link is nil, ready for relinking.
                    let generatorLinks =
                            [ (waActivityLinkId ex, waProcessLinkId ex)
                            | act <- V.toList (dbActivities db')
                            , activityName act == "generator"
                            , ex@WasteExchange{} <- exchanges act
                            ]
                    generatorLinks `shouldBe` [(UUID.nil, Nothing)]

        it "fails loudly on an out-of-range ProcessId" $ do
            db <- buildOrFail (chainDB 400)
            case deleteActivities [999] db of
                Left _ -> pure ()
                Right _ -> expectationFailure "expected out-of-range ProcessId to fail"

        it "refuses to delete every activity" $ do
            db <- buildOrFail (chainDB 500)
            let allPids = [0 .. dbActivityCount db - 1]
            case deleteActivities allPids db of
                Left _ -> pure ()
                Right _ -> expectationFailure "expected delete-all to be refused"

    describe "deleteActivitiesInDB (filter-driven, in-place)" $ do
        it "deletes the whole filtered set and respects keep" $ do
            manager <- initDatabaseManager defaultConfig True Nothing
            db <- buildOrFail (classifiedDB 600)
            installLoaded manager "edit-me" db
            -- Filter matches the two "food" activities; keep one by its
            -- canonical process-id string (the value the UI carries).
            let foodA = processIdToText db (pidFor2 db (mkUUID 601) (mkUUID 601))
            r <-
                deleteActivitiesInDB
                    manager
                    "edit-me"
                    Nothing
                    Nothing
                    Nothing
                    [("category", "food", False)]
                    False
                    [foodA] -- keep
                    [] -- extra
            case r of
                Left err -> expectationFailure ("deleteActivitiesInDB failed: " <> show err)
                Right deleted -> do
                    deleted `shouldBe` 1 -- two matched, one kept
                    loaded <- readTVarIO (dmLoadedDbs manager)
                    let db' = ldDatabase (loaded M.! "edit-me")
                    dbActivityCount db' `shouldBe` 2
                    -- The kept food activity and the non-food activity survive.
                    map activityName (V.toList (dbActivities db'))
                        `shouldSatisfy` elem "food-A"

        it "fails loudly on an unknown keep process id" $ do
            manager <- initDatabaseManager defaultConfig True Nothing
            db <- buildOrFail (classifiedDB 700)
            installLoaded manager "edit-me-2" db
            r <-
                deleteActivitiesInDB
                    manager
                    "edit-me-2"
                    Nothing
                    Nothing
                    Nothing
                    [("category", "food", False)]
                    False
                    ["not-a-real-process-id"]
                    []
            case r of
                Left _ -> pure ()
                Right _ -> expectationFailure "expected unknown keep id to fail"

        it "fails when the database is not loaded" $ do
            manager <- initDatabaseManager defaultConfig True Nothing
            r <- deleteActivitiesInDB manager "ghost" Nothing Nothing Nothing [] False [] []
            r `shouldBe` Left "Database not loaded: ghost"

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

pidFor :: Database -> Int -> Int -> ProcessId
pidFor db offset i = pidFor2 db (mkUUID (offset + 10 * i + 1)) (mkUUID (offset + 10 * i + 1))

pidFor2 :: Database -> UUID -> UUID -> ProcessId
pidFor2 db actUUID prodUUID =
    maybe (error "pidFor2: key not found") id (findProcessId db actUUID prodUUID)

installLoaded :: DatabaseManager -> Text -> Database -> IO ()
installLoaded manager name db = do
    solver <- mkSolver name db
    let loaded =
            LoadedDatabase
                { ldDatabase = db
                , ldSharedSolver = solver
                , ldConfig = mkConfig name
                }
    atomically $ do
        modifyTVar' (dmLoadedDbs manager) (M.insert name loaded)
        modifyTVar' (dmAvailableDbs manager) (M.insert name (mkConfig name))

mkSolver :: Text -> Database -> IO SharedSolver
mkSolver name db =
    let triples = [(fromIntegral i, fromIntegral j, v) | SparseTriple i j v <- U.toList (dbTechnosphereTriples db)]
     in createSharedSolver name triples (fromIntegral (dbActivityCount db))

mkConfig :: Text -> DatabaseConfig
mkConfig name =
    DatabaseConfig
        { dcName = name
        , dcDisplayName = name
        , dcPath = ""
        , dcDescription = Nothing
        , dcLoad = True
        , dcDefault = False
        , dcDepends = []
        , dcLocationAliases = M.empty
        , dcFormat = Nothing
        , dcIsUploaded = True
        , dcDeletable = True
        , dcGeographyPolicy = GeoGlobal
        }

buildOrFail :: SimpleParts -> IO Database
buildOrFail (SimpleParts acts flows units) = do
    r <- buildDatabaseWithMatrices defaultUnitConfig acts flows M.empty M.empty units
    case r of
        Right db -> pure db
        Left err -> fail ("buildDatabaseWithMatrices: " <> show err)

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

data SimpleParts
    = SimpleParts
        (M.Map (UUID, UUID) Activity)
        (M.Map UUID TechnosphereFlow)
        (M.Map UUID Unit)

mkUUID :: Int -> UUID
mkUUID n = UUID.fromWords64 (fromIntegral n) 0

kgUnitId :: UUID
kgUnitId = mkUUID 0

kgUnit :: Unit
kgUnit = Unit{unitId = kgUnitId, unitName = "kg", unitSymbol = "kg", unitComment = ""}

mkTechFlow :: UUID -> Text -> TechnosphereFlow
mkTechFlow fid name =
    TechnosphereFlow
        { tfId = fid
        , tfName = name
        , tfUnitId = kgUnitId
        , tfSynonyms = M.empty
        , tfCAS = Nothing
        , tfSubstanceId = Nothing
        }

{- | A reference-product output exchange whose product UUID equals its flow UUID
(so @(actUUID, prodUUID)@ is also the @(actUUID, flowId)@ link key).
-}
refOut :: UUID -> UUID -> Exchange
refOut actUUID prodUUID =
    TechnosphereExchange
        { techFlowId = prodUUID
        , techAmount = 1.0
        , techUnitId = kgUnitId
        , techRole = ReferenceProduct
        , techActivityLinkId = actUUID
        , techProcessLinkId = Nothing
        , techLocation = ""
        , techComment = Nothing
        , techPedigree = Nothing
        }

-- | An input exchange linking to a supplier's @(supplierActUUID, supplierProdUUID)@.
inputFrom :: UUID -> UUID -> Exchange
inputFrom supplierActUUID supplierProdUUID =
    TechnosphereExchange
        { techFlowId = supplierProdUUID
        , techAmount = 0.5
        , techUnitId = kgUnitId
        , techRole = Input
        , techActivityLinkId = supplierActUUID
        , techProcessLinkId = Nothing
        , techLocation = ""
        , techComment = Nothing
        , techPedigree = Nothing
        }

{- | A waste output linking to a treatment activity's
@(treatmentActUUID, treatmentProdUUID)@ — the same @(activityLink, flowId)@
key resolution a technosphere input uses.
-}
wasteOut :: UUID -> UUID -> Exchange
wasteOut treatmentActUUID treatmentProdUUID =
    WasteExchange
        { waFlowId = treatmentProdUUID
        , waAmount = 0.5
        , waUnitId = kgUnitId
        , waIsInput = False
        , waActivityLinkId = treatmentActUUID
        , waProcessLinkId = Nothing
        , waLocation = ""
        , waComment = Nothing
        , waPedigree = Nothing
        }

mkActivity :: Text -> Text -> M.Map Text Text -> [Exchange] -> Activity
mkActivity name loc classif exs =
    Activity
        { activityName = name
        , activityDescription = []
        , activitySynonyms = M.empty
        , activityClassification = classif
        , activityLocation = loc
        , activityUnit = "kg"
        , exchanges = exs
        , activityParams = M.empty
        , activityParamExprs = M.empty
        , activityAllocationPercent = Nothing
        , activityAllocationFormula = Nothing
        , activityNativeType = Nothing
        }

units :: M.Map UUID Unit
units = M.singleton kgUnitId kgUnit

{- | A three-activity chain: top → mid → supplier. UUIDs are
@offset + 10*i + 1@ for activity i (i: 0=supplier, 1=mid, 2=top), with product
UUID equal to the activity UUID for simple linking.
-}
chainDB :: Int -> SimpleParts
chainDB offset =
    let supA = mkUUID (offset + 1)
        midA = mkUUID (offset + 11)
        topA = mkUUID (offset + 21)
        supplier = mkActivity "supplier" "GLO" M.empty [refOut supA supA]
        mid = mkActivity "mid" "GLO" M.empty [refOut midA midA, inputFrom supA supA]
        top = mkActivity "top" "GLO" M.empty [refOut topA topA, inputFrom midA midA]
     in SimpleParts
            ( M.fromList
                [ ((supA, supA), supplier)
                , ((midA, midA), mid)
                , ((topA, topA), top)
                ]
            )
            ( M.fromList
                [ (supA, mkTechFlow supA "supplier-product")
                , (midA, mkTechFlow midA "mid-product")
                , (topA, mkTechFlow topA "top-product")
                ]
            )
            units

{- | A supplier exposing two products (A, B) plus a consumer linking to product
A only. Deleting product B must keep the consumer's link to A intact.
-}
multiProductDB :: Int -> SimpleParts
multiProductDB offset =
    let supA = mkUUID (offset + 1)
        prodA = mkUUID (offset + 2)
        prodB = mkUUID (offset + 3)
        consA = mkUUID (offset + 11)
        supplierA = mkActivity "supplier" "GLO" M.empty [refOut supA prodA]
        supplierB = mkActivity "supplier" "GLO" M.empty [refOut supA prodB]
        consumer = mkActivity "consumer-A" "GLO" M.empty [refOut consA consA, inputFrom supA prodA]
     in SimpleParts
            ( M.fromList
                [ ((supA, prodA), supplierA)
                , ((supA, prodB), supplierB)
                , ((consA, consA), consumer)
                ]
            )
            ( M.fromList
                [ (prodA, mkTechFlow prodA "product-A")
                , (prodB, mkTechFlow prodB "product-B")
                , (consA, mkTechFlow consA "consumer-product")
                ]
            )
            units

{- | A waste generator wired to a treatment activity via a 'WasteExchange'.
UUIDs are @offset + 10*i + 1@ (i: 0=treatment, 1=generator) so the treatment
sorts first. Deleting the treatment must unlink the generator's waste output.
-}
wasteDB :: Int -> SimpleParts
wasteDB offset =
    let treatA = mkUUID (offset + 1)
        genA = mkUUID (offset + 11)
        treatment = mkActivity "treatment" "GLO" M.empty [refOut treatA treatA]
        generator = mkActivity "generator" "GLO" M.empty [refOut genA genA, wasteOut treatA treatA]
     in SimpleParts
            ( M.fromList
                [ ((treatA, treatA), treatment)
                , ((genA, genA), generator)
                ]
            )
            ( M.fromList
                [ (treatA, mkTechFlow treatA "treatment-product")
                , (genA, mkTechFlow genA "generator-product")
                ]
            )
            units

{- | Two "food" activities and one "energy" activity, classified under the
@category@ system so a classification filter selects the food pair.
-}
classifiedDB :: Int -> SimpleParts
classifiedDB offset =
    let foodA = mkUUID (offset + 1)
        foodB = mkUUID (offset + 11)
        energ = mkUUID (offset + 21)
        food = M.singleton "category" "food"
        energy = M.singleton "category" "energy"
     in SimpleParts
            ( M.fromList
                [ ((foodA, foodA), mkActivity "food-A" "GLO" food [refOut foodA foodA])
                , ((foodB, foodB), mkActivity "food-B" "GLO" food [refOut foodB foodB])
                , ((energ, energ), mkActivity "energy" "GLO" energy [refOut energ energ])
                ]
            )
            ( M.fromList
                [ (foodA, mkTechFlow foodA "food-a")
                , (foodB, mkTechFlow foodB "food-b")
                , (energ, mkTechFlow energ "energy")
                ]
            )
            units
