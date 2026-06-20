{-# LANGUAGE OverloadedStrings #-}

{- | Reproduces the water-use sign-inversion defect with hermetic fixtures
(no production database).

Every water biosphere flow shares one CAS number (7732-18-5). The CF→flow
matcher ('mapMethodFlows') resolves each method CF to a __single__ flow, and
the scoring read path ('lookupCascadeCF') keys a flow back to a CF by its
__name__ + compartment. AWARE's resource-side CF names ("freshwater",
"river water", …) do not lexically match ecoinvent's resource-water flow
names ("Water, river", "Water, cooling, …"), so the positive consumption
side is left uncharacterized, while the emission "Water" flow (name
coincides) keeps its negative CF. A net water consumer therefore scores
__negative__ — the opposite sign of the published value.

The fixtures drive the real mapper + table build + scoring, so they track
the engine's true behaviour rather than a hand-modelled approximation.

The second group reproduces the broadcast-table pollution defect of
regionalized methods: location-specific CF rows used to land in the
name-keyed broadcast tables ('mtExactCF' / 'mtFallbackCF') alongside the
global row, where one arbitrary location's value won the key — a
water-abundant region's 0 erased the global credit, and a high-scarcity
region's factor inflated the global charge. Broadcast tables must hold
only non-regionalized CFs.
-}
module SharedCASCoverageSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Test.Hspec

import Method.Mapping
import Method.Types (Compartment (..), FlowDirection (..), Method (..), MethodCF (..))
import Plugin.Builtin (defaultMappers)
import Plugin.Types (MapContext (..))
import SynonymDB (buildFromPairs, emptySynonymDB, normalizeName)
import Types (BiosphereFlow (..))
import qualified Types as VT
import UnitConversion (defaultUnitConfig)

-- ---------------------------------------------------------------------------
-- Fixture: 3 ecoinvent water flows + an AWARE-style method, all CAS 7732-18-5
-- ---------------------------------------------------------------------------

mkUUID :: Integer -> UUID
mkUUID n = UUID.fromWords64 (fromIntegral n) 0

waterCAS :: Text
waterCAS = "7732-18-5"

-- bfUnitId = mkUUID 0 with an empty UnitDB ⇒ identity unit conversion.
mkWaterFlow :: Integer -> Text -> Text -> BiosphereFlow
mkWaterFlow i name medium =
    BiosphereFlow
        { bfId = mkUUID i
        , bfName = name
        , bfUnitId = mkUUID 0
        , bfSynonyms = M.empty
        , bfCAS = Just waterCAS
        , bfSubstanceId = Nothing
        , bfCompartment = Just (VT.Compartment medium Nothing)
        }

-- An AWARE-style CF: resolvable only via CAS + compartment medium.
-- mcfFlowRef is a non-flow UUID so the UUID mapper misses and the CAS mapper fires.
mkWaterCF :: Text -> Text -> FlowDirection -> Double -> MethodCF
mkWaterCF name medium dir val =
    MethodCF
        { mcfFlowRef = mkUUID 999
        , mcfFlowName = name
        , mcfDirection = dir
        , mcfValue = val
        , mcfCompartment = Just (Compartment medium "" "")
        , mcfCAS = Just waterCAS
        , mcfUnit = "m3"
        , mcfConsumerLocation = Nothing
        }

-- Real ecoinvent flow names (CAS 7732-18-5).
river, cooling, emWater :: BiosphereFlow
river = mkWaterFlow 1 "Water, river" "natural resource"
cooling = mkWaterFlow 2 "Water, cooling, unspecified natural origin" "natural resource"
emWater = mkWaterFlow 3 "Water" "water"

allFlows :: [BiosphereFlow]
allFlows = [river, cooling, emWater]

flowDB :: M.Map UUID BiosphereFlow
flowDB = M.fromList [(bfId f, f) | f <- allFlows]

-- AWARE-style method: positive consumption CFs on water resources, negative on
-- the water emission. Real AWARE input flow names that do NOT lexically match
-- the ecoinvent resource-flow names above.
awareMethod :: Method
awareMethod =
    Method
        { methodId = mkUUID 100
        , methodName = "Water use"
        , methodDescription = Nothing
        , methodUnit = "m3"
        , methodCategory = "Water use"
        , methodMethodology = Nothing
        , methodFactors =
            [ mkWaterCF "freshwater" "natural resource" Input 5
            , mkWaterCF "river water" "natural resource" Input 5
            , mkWaterCF "water" "natural resource" Input 5
            , mkWaterCF "Water" "water" Output (-5)
            ]
        }

-- | Same CF, but applying only to consumers in @loc@ (a regionalized row).
atLocation :: Text -> MethodCF -> MethodCF
atLocation loc cf = cf{mcfConsumerLocation = Just loc}

{- | AWARE-style regionalized method whose CF names coincide with database
flow names, so the name-keyed broadcast tables are actually read. Each
global CF is shadowed by a location row with a wildly different value:
a high-scarcity region (+100) on the charge side, a water-abundant
region (0) on the credit side.
-}
regionalizedMethod :: Method
regionalizedMethod =
    Method
        { methodId = mkUUID 101
        , methodName = "Water use (regionalized)"
        , methodDescription = Nothing
        , methodUnit = "m3"
        , methodCategory = "Water use"
        , methodMethodology = Nothing
        , methodFactors =
            [ mkWaterCF "Water, river" "natural resource" Input 5
            , atLocation "IN" (mkWaterCF "Water, river" "natural resource" Input 100)
            , mkWaterCF "Water" "water" Output (-5)
            , atLocation "GL" (mkWaterCF "Water" "water" Output 0)
            ]
        }

-- mcBioFlowsByName left empty so the CAS bridge is exercised (as for EF/ILCD,
-- whose flow UUIDs and names differ from ecoinvent's).
mapCtx :: MapContext
mapCtx =
    MapContext
        { mcBioFlowsByUUID = flowDB
        , mcBioFlowsByName = M.empty
        , mcBioFlowsByCAS = M.fromList [(waterCAS, allFlows)]
        , mcSynonymDB = emptySynonymDB
        , mcActivities = M.empty
        }

-- Build scoring tables through the real mapper + table build + broadcast fill.
buildTablesFor :: Method -> IO MethodTables
buildTablesFor method = do
    mappings <- mapMethodFlows defaultMappers mapCtx method
    let raw = buildMethodTables M.empty M.empty mappings
    pure (fillBroadcastVector defaultUnitConfig M.empty flowDB raw)

buildTables :: IO MethodTables
buildTables = buildTablesFor awareMethod

-- ---------------------------------------------------------------------------
-- Fixture: fossil vs biogenic methane — CAS must NOT cross the name split
-- ---------------------------------------------------------------------------

-- Both methane flows share CAS 74-82-8 and medium air; only their name carries
-- the carbon-origin distinction the biogenic method splits on.
methaneCAS :: Text
methaneCAS = "74-82-8"

mkMethane :: Integer -> Text -> BiosphereFlow
mkMethane i name =
    BiosphereFlow
        { bfId = mkUUID i
        , bfName = name
        , bfUnitId = mkUUID 0
        , bfSynonyms = M.empty
        , bfCAS = Just methaneCAS
        , bfSubstanceId = Nothing
        , bfCompartment = Just (VT.Compartment "air" Nothing)
        }

methaneNonFossil, methaneFossil :: BiosphereFlow
methaneNonFossil = mkMethane 10 "Methane, non-fossil"
methaneFossil = mkMethane 11 "Methane, fossil"

carbonFlows :: M.Map UUID BiosphereFlow
carbonFlows = M.fromList [(bfId f, f) | f <- [methaneNonFossil, methaneFossil]]

-- Biogenic-climate-style method: the CF is named "methane (biogenic)" and
-- carries CAS 74-82-8. ecoinvent's "Methane, non-fossil" reaches it only via
-- the curated synonym below — fossil methane must stay out.
biogenicMethaneMethod :: Method
biogenicMethaneMethod =
    Method
        { methodId = mkUUID 110
        , methodName = "Climate change-Biogenic"
        , methodDescription = Nothing
        , methodUnit = "kg CO2 eq"
        , methodCategory = "Climate change"
        , methodMethodology = Nothing
        , methodFactors = [mkMethaneCF "methane (biogenic)" 27]
        }
  where
    mkMethaneCF name val =
        MethodCF
            { mcfFlowRef = mkUUID 998
            , mcfFlowName = name
            , mcfDirection = Output
            , mcfValue = val
            , mcfCompartment = Just (Compartment "air" "" "")
            , mcfCAS = Just methaneCAS
            , mcfUnit = "kg CO2 eq"
            , mcfConsumerLocation = Nothing
            }

-- The same biogenic↔non-fossil synonym the production reference data ships.
carbonSynonyms :: MapContext
carbonSynonyms =
    MapContext
        { mcBioFlowsByUUID = carbonFlows
        , mcBioFlowsByName =
            M.fromListWith
                (++)
                [(normalizeName (bfName f), [f]) | f <- [methaneNonFossil, methaneFossil]]
        , mcBioFlowsByCAS = M.fromList [(methaneCAS, [methaneNonFossil, methaneFossil])]
        , mcSynonymDB = buildFromPairs [("Methane, biogenic", "Methane, non-fossil")]
        , mcActivities = M.empty
        }

buildCarbonTables :: IO MethodTables
buildCarbonTables = do
    mappings <- mapMethodFlows defaultMappers carbonSynonyms biogenicMethaneMethod
    let raw = buildMethodTables M.empty M.empty mappings
    pure (fillBroadcastVector defaultUnitConfig M.empty carbonFlows raw)

-- ---------------------------------------------------------------------------
-- Fixture: regionalized rows must stay out of the global tables
-- ---------------------------------------------------------------------------

-- A regionalized CAS-only-matchable CF: belongs in 'mtRegionalCasCF'.
regionalCasMethod :: Method
regionalCasMethod =
    Method
        { methodId = mkUUID 130
        , methodName = "Water use (regional CAS)"
        , methodDescription = Nothing
        , methodUnit = "m3"
        , methodCategory = "Water use"
        , methodMethodology = Nothing
        , methodFactors = [atLocation "FR" (mkWaterCF "river water" "natural resource" Input 9)]
        }

-- A global + a regionalized row, both UUID-matched to the same flow. The
-- regionalized row comes last so, were it admitted to 'mtUuidCF',
-- 'M.fromList' (last wins) would clobber the global value.
uuidRegionalMethod :: Method
uuidRegionalMethod =
    Method
        { methodId = mkUUID 131
        , methodName = "Water use (regional UUID)"
        , methodDescription = Nothing
        , methodUnit = "m3"
        , methodCategory = "Water use"
        , methodMethodology = Nothing
        , methodFactors =
            [ (mkWaterCF "global row" "natural resource" Input 5){mcfFlowRef = bfId river}
            , atLocation "IN" ((mkWaterCF "regional row" "natural resource" Input 100){mcfFlowRef = bfId river})
            ]
        }

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "Water-use sign: CAS-shared resource flows must be characterized" $ do
    it "characterizes the emission water flow (positive control)" $ do
        tables <- buildTables
        M.member (bfId emWater) (mtBroadcast tables) `shouldBe` True

    it "characterizes a consumed resource water flow sharing the same CAS" $ do
        tables <- buildTables
        -- "Water, river" shares CAS 7732-18-5 and is a water resource. Its
        -- ecoinvent name ("water river") matches no AWARE CF name, and the CAS
        -- bridge resolved a single flow at build time — so before the CAS
        -- read-path fallback it was uncharacterized. Now it is reached by its
        -- own CAS + medium.
        M.member (bfId river) (mtBroadcast tables) `shouldBe` True

    it "scores a net water consumer positive (no sign inversion)" $ do
        tables <- buildTables
        -- Consume 10 m3 of river water, release 4 m3 ⇒ net consumption 6.
        -- Correct: 10·(+5) + 4·(−5) = +30. The old bug left river
        -- uncharacterized ⇒ 0 + 4·(−5) = −20 (wrong sign).
        let inventory = M.fromList [(bfId river, 10), (bfId emWater, 4)]
            outcome = computeLCIAScoreFromTables defaultUnitConfig M.empty flowDB inventory tables
        loScore outcome `shouldSatisfy` (> 0)

    describe "broadcast tables ignore regionalized CF rows" $ do
        it "a water-abundant region's 0 does not erase the global credit" $ do
            tables <- buildTablesFor regionalizedMethod
            M.lookup (bfId emWater) (mtBroadcast tables) `shouldBe` Just (-5)

        it "a high-scarcity region's factor does not inflate the global charge" $ do
            tables <- buildTablesFor regionalizedMethod
            M.lookup (bfId river) (mtBroadcast tables) `shouldBe` Just 5

    describe "CAS bridge defers to the name/synonym split (fossil vs biogenic)" $ do
        it "characterizes biogenic methane via its synonym to the CF name" $ do
            tables <- buildCarbonTables
            M.lookup (bfId methaneNonFossil) (mtBroadcast tables) `shouldBe` Just 27

        it "does NOT leak the biogenic CF onto fossil methane (same CAS)" $ do
            tables <- buildCarbonTables
            -- The CF pinned "Methane, non-fossil" by synonym, so it is name-
            -- discriminated and never enters the CAS broadcast table. Fossil
            -- methane shares CAS 74-82-8 but is a distinct variant the method
            -- excludes — it must stay uncharacterized, not inherit +27.
            M.member (bfId methaneFossil) (mtBroadcast tables) `shouldBe` False

    describe "regionalized rows stay out of the global tables" $ do
        it "routes a location-bearing CAS-matched CF to mtRegionalCasCF only" $ do
            mappings <- mapMethodFlows defaultMappers mapCtx regionalCasMethod
            let tables = buildMethodTables M.empty M.empty mappings
            M.lookup (waterCAS, "resource") (mtRegionalCasCF tables)
                `shouldBe` Just (M.fromList [("FR", (9, "m3"))])
            M.member (waterCAS, "resource") (mtCasCF tables) `shouldBe` False

        it "keeps regionalized UUID-matched rows out of mtUuidCF" $ do
            mappings <- mapMethodFlows defaultMappers mapCtx uuidRegionalMethod
            let tables = buildMethodTables M.empty M.empty mappings
            -- The global row stands; the location row lives in the regional
            -- table instead of clobbering the flow's universal value.
            M.lookup (bfId river) (mtUuidCF tables) `shouldBe` Just (5, "m3")
            M.lookup (bfId river, "IN") (mtRegionalizedCF tables) `shouldBe` Just (100, "m3")
