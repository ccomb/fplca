{-# LANGUAGE OverloadedStrings #-}

{- | Sign correctness for waste-treatment scoring across the two reference
conventions VoLCA's parsers emit:

  * EcoSpold2 (ecoinvent): a treatment's reference is a NEGATIVE technosphere
    output (e.g. -1 kg of the waste it treats) → 'activityNormFactor' = -1.
  * ILCD: a treatment's reference is a POSITIVE 'ReferenceInput' (+1 kg of the
    waste it consumes) → 'activityNormFactor' = +1 (refInputs is abs-summed).

Each convention is self-consistent on its own. Within a single database the
convention is uniform, so the only realistic way to mix them is
cross-database: an ecoinvent-style orphan waste OUTPUT resolved to a
background treatment in another DB via 'findWasteTreatmentAcrossDatabases'.
That path scores through the dep-demand solve ('accumulateDepDemandsWith'),
NOT the static technosphere triples — and the dep-demand path applies no
input/output sign, only 'cdlCoefficient'.

A correctly treated 3 kg of waste at 2 kg CO2 / kg MUST contribute +6 kg CO2 in
every case — treating waste adds burden, it never subtracts it.
-}
module WasteTreatmentSignSpec (spec) where

import Data.List (elemIndex)
import qualified Data.Map as M
import qualified Data.Map.Strict as MS
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import Database (buildDatabaseWithMatrices)
import Database.CrossLinking (LinkingContext (..), buildIndexedDatabaseFromDB, defaultLinkingThreshold)
import Database.Loader (findAllCrossDBLinks)
import Matrix (computeInventoryMatrix)
import SharedSolver (CrossDBSolution (..), computeInventoryMatrixWithDepsCached)
import SynonymDB (emptySynonymDB)
import Test.Hspec
import TestHelpers (mkDepLookupFromMap, mkSolverFromDb, withinTolerance)
import Types
import UnitConversion (defaultUnitConfig)

-- | A deterministic UUID from its canonical string form.
mkUUID :: String -> UUID
mkUUID s = fromMaybe UUID.nil (UUID.fromString s)

-- Shared identifiers.
kgU, co2, wW, yY, tA, pA :: UUID
kgU = mkUUID "66666666-6666-6666-6666-666666666666"
co2 = mkUUID "55555555-5555-5555-5555-555555555555"
wW = mkUUID "22222222-2222-2222-2222-222222222222"
yY = mkUUID "44444444-4444-4444-4444-444444444444"
tA = mkUUID "11111111-1111-1111-1111-111111111111"
pA = mkUUID "33333333-3333-3333-3333-333333333333"

emptyActivity :: Activity
emptyActivity =
    Activity
        { activityName = ""
        , activityDescription = []
        , activitySynonyms = M.empty
        , activityClassification = M.empty
        , activityLocation = "GLO"
        , activityUnit = "kg"
        , exchanges = []
        , activityParams = M.empty
        , activityParamExprs = M.empty
        , activityAllocationPercent = Nothing
        , activityAllocationFormula = Nothing
        , activityNativeType = Nothing
        }

techEx :: UUID -> Double -> TechRole -> UUID -> Exchange
techEx flow amt role link =
    TechnosphereExchange
        { techFlowId = flow
        , techAmount = amt
        , techUnitId = kgU
        , techRole = role
        , techActivityLinkId = link
        , techProcessLinkId = Nothing
        , techLocation = ""
        , techComment = Nothing
        , techPedigree = Nothing
        }

co2Emission :: Double -> Exchange
co2Emission amt =
    BiosphereExchange
        { bioFlowId = co2
        , bioAmount = amt
        , bioUnitId = kgU
        , bioDirection = Emission
        , bioLocation = ""
        , bioComment = Nothing
        , bioPedigree = Nothing
        }

wasteEx :: Bool -> UUID -> Double -> Exchange
wasteEx isInput link amt =
    WasteExchange
        { waFlowId = wW
        , waAmount = amt
        , waUnitId = kgU
        , waIsInput = isInput
        , waActivityLinkId = link
        , waProcessLinkId = Nothing
        , waLocation = ""
        , waComment = Nothing
        , waPedigree = Nothing
        }

{- | A treatment of waste W: reference of W (role + signed amount vary by
convention), emitting 2 kg CO2 per kg treated.
-}
treatment :: TechRole -> Double -> Activity
treatment role amt =
    emptyActivity
        { activityName = "treatment of waste W"
        , exchanges = [techEx wW amt role UUID.nil, co2Emission 2.0]
        }

{- | A producer of Y that also emits 3 kg of waste W. @waste@ supplies the
waste exchange (intra-DB link, or orphan for the cross-DB cases).
-}
producer :: Exchange -> Activity
producer waste =
    emptyActivity
        { activityName = "producer of Y"
        , exchanges = [techEx yY 1.0 ReferenceProduct UUID.nil, waste]
        }

co2Flow :: BiosphereFlow
co2Flow = BiosphereFlow co2 "carbon dioxide" kgU M.empty Nothing Nothing (Just (Compartment "air" Nothing))

wasteFlowDB :: M.Map UUID WasteFlow
wasteFlowDB = M.singleton wW (WasteFlow wW "waste W" kgU M.empty Nothing Nothing)

techFlowDB :: M.Map UUID TechnosphereFlow
techFlowDB =
    M.fromList
        [ (wW, TechnosphereFlow wW "waste W" kgU M.empty Nothing Nothing)
        , (yY, TechnosphereFlow yY "product Y" kgU M.empty Nothing Nothing)
        ]

buildDB :: T.Text -> M.Map (UUID, UUID) Activity -> IO Database
buildDB name acts =
    buildDatabaseWithMatrices
        defaultUnitConfig
        acts
        techFlowDB
        (M.singleton co2 co2Flow)
        wasteFlowDB
        (M.singleton kgU (Unit kgU "kg" "kg" ""))
        >>= either (\e -> fail (T.unpack name <> ": " <> T.unpack e)) pure

co2Of :: M.Map UUID Double -> Double
co2Of = M.findWithDefault 0.0 co2

processIdOf :: Database -> (UUID, UUID) -> Maybe ProcessId
processIdOf db key = fromIntegral <$> elemIndex key (V.toList (dbProcessIdTable db))

-- | Intra-DB scoring of the producer's CO2 (single database, static triples).
scoreIntra :: T.Text -> M.Map (UUID, UUID) Activity -> IO Double
scoreIntra name acts = do
    db <- buildDB name acts
    case processIdOf db (pA, yY) of
        Nothing -> fail "producer not interned"
        Just pid -> co2Of <$> computeInventoryMatrix db (fromIntegral pid)

{- | Cross-DB scoring: a root DB whose orphan waste OUTPUT is resolved — by the
real linker ('findAllCrossDBLinks' → 'findWasteTreatmentAcrossDatabases') — to a
treatment in a separate dependency DB, then scored through the dep-demand solve.
Going through the linker (rather than a hand-built 'CrossDBLink') is the point:
that is where the treatment's reference-sign correction is applied.
-}
scoreCross :: T.Text -> TechRole -> Double -> IO Double
scoreCross depName depRole depRefAmount = do
    let rootActs = M.singleton (pA, yY) (producer (wasteEx False UUID.nil 3.0))
    rootBase <- buildDB "root" rootActs
    depDB <- buildDB depName (M.singleton (tA, wW) (treatment depRole depRefAmount))
    let ctx =
            LinkingContext
                { lcIndexedDatabases = [buildIndexedDatabaseFromDB depName emptySynonymDB depDB]
                , lcSynonymDB = emptySynonymDB
                , lcUnitConfig = defaultUnitConfig
                , lcThreshold = defaultLinkingThreshold
                , lcLocationHierarchy = M.empty
                , lcGeographyPolicy = GeoExact
                , lcSupplierAliases = Nothing
                }
        links = cdlLinks (findAllCrossDBLinks ctx techFlowDB wasteFlowDB (M.singleton kgU (Unit kgU "kg" "kg" "")) rootActs)
        rootDB = rootBase{dbCrossDBLinks = links}
    if null links
        then fail "linker created no cross-DB waste link (matcher did not fire)"
        else do
            rootSolver <- mkSolverFromDb rootDB "root"
            depSolver <- mkSolverFromDb depDB depName
            let depLookup = mkDepLookupFromMap (MS.singleton depName (depDB, depSolver))
            case processIdOf rootDB (pA, yY) of
                Nothing -> fail "producer not interned"
                Just pid -> do
                    res <- computeInventoryMatrixWithDepsCached defaultUnitConfig depLookup rootDB "root" rootSolver pid
                    case res of
                        Left err -> fail (T.unpack err)
                        Right sol -> pure (co2Of (csInventory sol))

spec :: Spec
spec = describe "Waste-treatment scoring sign across reference conventions" $ do
    it "intra-DB ecoinvent (negative ReferenceProduct output) scores +6" $ do
        score <-
            scoreIntra
                "intra-eco"
                ( M.fromList
                    [ ((pA, yY), producer (wasteEx False tA 3.0))
                    , ((tA, wW), treatment ReferenceProduct (-1.0))
                    ]
                )
        withinTolerance 1.0e-9 6.0 score `shouldBe` True

    it "intra-DB ILCD (positive ReferenceInput) scores +6" $ do
        score <-
            scoreIntra
                "intra-ilcd"
                ( M.fromList
                    [ ((pA, yY), producer (wasteEx True tA 3.0))
                    , ((tA, wW), treatment ReferenceInput 1.0)
                    ]
                )
        withinTolerance 1.0e-9 6.0 score `shouldBe` True

    it "cross-DB to an ILCD (positive ReferenceInput) treatment scores +6" $ do
        score <- scoreCross "ilcd-dep" ReferenceInput 1.0
        withinTolerance 1.0e-9 6.0 score `shouldBe` True

    it "cross-DB to an ecoinvent (negative ReferenceProduct) treatment scores +6" $ do
        score <- scoreCross "eco-dep" ReferenceProduct (-1.0)
        withinTolerance 1.0e-9 6.0 score `shouldBe` True
