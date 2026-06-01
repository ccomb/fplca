{-# LANGUAGE OverloadedStrings #-}

{- | Round-trip contract for the EcoSpold2 writer ('EcoSpold.Writer2'), the
inverse of 'EcoSpold.Parser2'.

We exercise three properties against a self-contained fixture 'SimpleDatabase'
built in Haskell:

  (a) __idempotence modulo volatile metadata__ — @write(D)@ then
      @write(parse(write(D)))@ produce byte-identical output once volatile
      metadata is excluded (we exclude it by writing with 'noVolatileMeta').
  (b) __semantic round-trip__ — @parse(write(D))@ is structurally equal to @D@
      (order-insensitive on exchanges/flows).
  (c) __score-equivalence__ — @parse(write(D))@ yields the same LCIA inventory
      (the engine's matrix-level score input) as @D@ within tolerance.

The fixture is constructed directly rather than loaded from a bundled
@.spold@ directory because the bundled @SAMPLE.units@ fixture deliberately
reuses a single @unitId@ across flows with *different* unit-name strings (a
degenerate-input stress test for the parser). The loader collapses those to
one unit per UUID in name-resolution order, which depends on filename
ordering — a property of the loader on malformed input, not of the writer.
A clean fixture (one name per unit UUID, distinct flow UUIDs) isolates the
writer's own determinism, which is what this spec asserts.
-}
module EcoSpold2WriterSpec (spec) where

import Control.Monad (forM_)
import Data.List (sort, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import Database (buildDatabaseWithMatrices)
import Database.Loader (loadDatabase)
import EcoSpold.Writer2 (noVolatileMeta, sortExchanges, writeEcoSpold2)
import Matrix (computeInventoryMatrix)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Types
import UnitConversion (defaultUnitConfig)

-- | A UUID from its canonical text, or nil if (impossibly) malformed.
uuid :: Text -> UUID.UUID
uuid t = fromMaybe UUID.nil (UUID.fromText t)

-- Distinct, well-formed UUIDs for the fixture.
actA, prodA, actB, prodB :: UUID.UUID
actA = uuid "aaaaaaaa-0000-4000-8000-000000000001"
prodA = uuid "aaaaaaaa-0000-4000-8000-0000000000a1"
actB = uuid "bbbbbbbb-0000-4000-8000-000000000002"
prodB = uuid "bbbbbbbb-0000-4000-8000-0000000000b2"

co2, land :: UUID.UUID
co2 = uuid "cccccccc-0000-4000-8000-0000000000c0"
land = uuid "dddddddd-0000-4000-8000-0000000000d0"

unitKg, unitMJ, unitM2a :: UUID.UUID
unitKg = uuid "11111111-0000-4000-8000-000000000001"
unitMJ = uuid "22222222-0000-4000-8000-000000000002"
unitM2a = uuid "33333333-0000-4000-8000-000000000003"

{- | A self-contained two-activity EcoSpold2 database.

  * Activity A produces product A (1 kg), consumes 2 MJ of product B, and
    emits 0.5 kg CO2 (biosphere).
  * Activity B produces product B (1 MJ) and occupies 0.1 m2*year of land.

Every unit UUID maps to exactly one name; every flow UUID is distinct.
-}
fixtureSimple :: SimpleDatabase
fixtureSimple =
    SimpleDatabase
        { sdbActivities =
            M.fromList
                [ ((actA, prodA), activityA)
                , ((actB, prodB), activityB)
                ]
        , sdbTechFlows =
            M.fromList
                [ (prodA, TechnosphereFlow prodA "product A" unitKg M.empty Nothing Nothing)
                , (prodB, TechnosphereFlow prodB "product B" unitMJ M.empty Nothing Nothing)
                ]
        , sdbBioFlows =
            M.fromList
                [ (co2, BiosphereFlow co2 "Carbon dioxide, fossil" unitKg M.empty Nothing Nothing (Just (Compartment "air" (Just "unspecified"))))
                , (land, BiosphereFlow land "Occupation, arable land" unitM2a M.empty Nothing Nothing (Just (Compartment "natural resource" (Just "land"))))
                ]
        , sdbWasteFlows = M.empty
        , sdbUnits =
            M.fromList
                [ (unitKg, Unit unitKg "kg" "kg" "")
                , (unitMJ, Unit unitMJ "MJ" "MJ" "")
                , (unitM2a, Unit unitM2a "m2*year" "m2*year" "")
                ]
        }
  where
    activityA =
        Activity
            "production of A"
            ["First fixture activity"]
            M.empty
            M.empty
            "GLO"
            "kg"
            [ TechnosphereExchange prodA 1.0 unitKg ReferenceProduct UUID.nil Nothing "" Nothing Nothing
            , TechnosphereExchange prodB 2.0 unitMJ Input actB Nothing "" (Just "energy input") Nothing
            , BiosphereExchange co2 0.5 unitKg Emission "" Nothing Nothing
            ]
            M.empty
            M.empty
            Nothing
            Nothing
            (Just (EcoSpoldActivityType 1 "Ordinary transforming activity" Nothing Nothing))
    activityB =
        Activity
            "production of B"
            []
            M.empty
            M.empty
            "GLO"
            "MJ"
            [ TechnosphereExchange prodB 1.0 unitMJ ReferenceProduct UUID.nil Nothing "" Nothing Nothing
            , BiosphereExchange land 0.1 unitM2a Resource "" Nothing Nothing
            ]
            M.empty
            M.empty
            Nothing
            Nothing
            (Just (EcoSpoldActivityType 2 "Market activity" (Just 1) (Just "Hard link")))

-- | The fixture as a 'SimpleDatabase' (matches the previous IO-shaped helper).
loadFixtureSimple :: IO SimpleDatabase
loadFixtureSimple = pure fixtureSimple

-- | Build a full 'Database' (with matrices) from a 'SimpleDatabase'.
buildDb :: SimpleDatabase -> IO Database
buildDb sdb = do
    res <-
        buildDatabaseWithMatrices
            defaultUnitConfig
            (sdbActivities sdb)
            (sdbTechFlows sdb)
            (sdbBioFlows sdb)
            (sdbWasteFlows sdb)
            (sdbUnits sdb)
    case res of
        Left err -> error $ "matrix build failed: " ++ T.unpack err
        Right db -> pure db

{- | Write a 'SimpleDatabase' to a fresh temp directory and re-load it through
the production loader, returning the round-tripped 'SimpleDatabase'.
-}
roundTrip :: SimpleDatabase -> IO SimpleDatabase
roundTrip sdb = withSystemTempDirectory "es2-writer" $ \dir -> do
    forM_ (writeEcoSpold2 noVolatileMeta sdb) $ \(fname, doc) ->
        TIO.writeFile (dir </> fname) doc
    res <- loadDatabase defaultUnitConfig dir
    case res of
        Left err -> error $ "reparse failed: " ++ T.unpack err
        Right sdb' -> pure sdb'

spec :: Spec
spec = describe "EcoSpold2 writer round-trip" $ do
    -- (a) Idempotence modulo volatile metadata.
    it "is idempotent modulo volatile metadata: write . parse . write == write" $ do
        sdb <- loadFixtureSimple
        let f0 = writeEcoSpold2 noVolatileMeta sdb
        sdb' <- roundTrip sdb
        let f1 = writeEcoSpold2 noVolatileMeta sdb'
        -- Compare the sorted (filename, document) sequences byte-for-byte.
        sortOn fst f1 `shouldBe` sortOn fst f0

    -- Regression: two exchanges sharing a sort key (same kind + flow) must both
    -- survive ordering. The previous Map-based sort silently dropped one,
    -- undercounting the inventory.
    it "keeps duplicate exchanges of the same flow (no silent dedup)" $ do
        let e1 = BiosphereExchange co2 0.5 unitKg Emission "" Nothing Nothing
            e2 = BiosphereExchange co2 0.3 unitKg Emission "" Nothing Nothing
        length (sortExchanges [e1, e2]) `shouldBe` 2

    -- (b) Semantic round-trip: parse(write(D)) ≅ D, order-insensitive.
    describe "semantic round-trip (structural equality)" $ do
        it "preserves the activity set keyed by (actUUID, prodUUID)" $ do
            sdb <- loadFixtureSimple
            sdb' <- roundTrip sdb
            M.keys (sdbActivities sdb') `shouldMatchList` M.keys (sdbActivities sdb)

        it "preserves each activity's name, location and exchange multiset" $ do
            sdb <- loadFixtureSimple
            sdb' <- roundTrip sdb
            forM_ (M.toList (sdbActivities sdb)) $ \(key, act) ->
                case M.lookup key (sdbActivities sdb') of
                    Nothing -> expectationFailure $ "missing activity after round-trip: " ++ show key
                    Just act' -> do
                        activityName act' `shouldBe` activityName act
                        activityLocation act' `shouldBe` activityLocation act
                        activityNativeType act' `shouldBe` activityNativeType act
                        sort (map exchangeFingerprint (exchanges act'))
                            `shouldBe` sort (map exchangeFingerprint (exchanges act))

        it "preserves the biosphere flow registry (id, name, compartment)" $ do
            sdb <- loadFixtureSimple
            sdb' <- roundTrip sdb
            map bioFingerprint (M.elems (sdbBioFlows sdb'))
                `shouldMatchList` map bioFingerprint (M.elems (sdbBioFlows sdb))

        it "preserves the technosphere flow registry (id, name)" $ do
            sdb <- loadFixtureSimple
            sdb' <- roundTrip sdb
            map (\f -> (tfId f, tfName f)) (M.elems (sdbTechFlows sdb'))
                `shouldMatchList` map (\f -> (tfId f, tfName f)) (M.elems (sdbTechFlows sdb))

    -- (c) Score-equivalence: same inventory for every activity within tolerance.
    it "yields the same LCIA inventory as the original (within tolerance)" $ do
        sdb <- loadFixtureSimple
        sdb' <- roundTrip sdb
        db <- buildDb sdb
        db' <- buildDb sdb'
        -- Compare inventories for every process id present in the original.
        let pids = [0 .. fromIntegral (V.length (dbProcessIdTable db)) - 1] :: [ProcessId]
        forM_ pids $ \pid -> do
            inv <- computeInventoryMatrix db pid
            -- The round-tripped DB may order activities differently; locate the
            -- matching process by its (actUUID, prodUUID) key.
            let key = dbProcessIdTable db V.! fromIntegral pid
            case V.elemIndex key (dbProcessIdTable db') of
                Nothing -> expectationFailure $ "process key missing after round-trip: " ++ show key
                Just pid' -> do
                    inv' <- computeInventoryMatrix db' (fromIntegral pid')
                    inventoriesClose inv inv' `shouldBe` True

-- ----------------------------------------------------------------------------
-- Comparison helpers (order-insensitive, tolerance-aware)
-- ----------------------------------------------------------------------------

-- | Structural fingerprint of an exchange, stable under reordering.
exchangeFingerprint :: Exchange -> (Int, Text, Bool, Bool, Integer)
exchangeFingerprint ex =
    ( kindRank ex
    , UUID.toText (exchangeFlowId ex)
    , exchangeIsInput ex
    , exchangeIsReference ex
    , roundAmount (exchangeAmount ex)
    )
  where
    kindRank TechnosphereExchange{} = 0
    kindRank WasteExchange{} = 1
    kindRank BiosphereExchange{} = 2

-- | Biosphere flow fingerprint: identity plus compartment medium/sub.
bioFingerprint :: BiosphereFlow -> (Text, Text, Text, Maybe Text)
bioFingerprint f =
    ( UUID.toText (bfId f)
    , bfName f
    , maybe "" compartmentName (bfCompartment f)
    , bfCompartment f >>= compartmentSub
    )

-- | Quantise an amount so float jitter doesn't break equality (9 sig decimals).
roundAmount :: Double -> Integer
roundAmount d = round (d * 1e9)

{- | Two inventories agree within relative+absolute tolerance on every flow
present in either map. Missing flows are treated as zero.
-}
inventoriesClose :: M.Map UUID.UUID Double -> M.Map UUID.UUID Double -> Bool
inventoriesClose a b =
    let keys = S.toList (S.fromList (M.keys a) `S.union` S.fromList (M.keys b))
        close k =
            let x = M.findWithDefault 0 k a
                y = M.findWithDefault 0 k b
             in abs (x - y) <= 1e-9 + 1e-6 * max (abs x) (abs y)
     in all close keys
