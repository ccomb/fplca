{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Round-trip tests for the Brightway Excel (.xlsx) /writer/, the inverse of
"BrightwayExcel.Parser".

The fixture 'SimpleDatabase' is built in Haskell using the very UUID generators
the parser feeds its flows/units/activities through ('generateFlowUUID',
'generateUnitUUID', 'generateActivityUUID'), so @parse (write D)@ reconstructs
the same identities — no committed binary fixture, and no dependence on the
cross-database link pass (technosphere links are left @nil@, exactly the state
the parser produces for a freshly imported workbook).

Three contracts are proven:

  (a) /logical-cell idempotence/ — re-exporting the parsed content yields the
      same worksheet rows. Byte identity is not attempted (zip metadata is
      volatile); we compare the parsed-then-rewritten logical cells.
  (b) /semantic round-trip/ — @parse (write D)@ is structurally equal to @D@,
      order-insensitively (activities, exchanges, flows, units).
  (c) /score equivalence/ — the biosphere inventory of a sample activity is
      preserved across the round-trip, within tolerance, using the engine's
      matrix solver.
-}
module BrightwayExcelWriterSpec (spec) where

import BrightwayExcel.Parser (parseBrightwayExcel)
import BrightwayExcel.Writer (
    Cell (..),
    WriterConfig (..),
    activityRows,
    formatAmount,
    renderCategories,
    renderWorkbook,
 )
import qualified Data.ByteString.Lazy as BL
import Data.List (find, sortOn)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Database (buildDatabaseWithMatrices)
import Database.Loader (getReferenceProductUUID)
import Matrix (computeInventoryMatrix)
import SimaPro.Parser (generateActivityUUID, generateFlowUUID, generateUnitUUID)
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import Test.Hspec
import Types
import UnitConversion (defaultUnitConfig)

spec :: Spec
spec = describe "BrightwayExcel.Writer" $ do
    describe "formatAmount" $ do
        it "prints integers without a decimal point" $
            formatAmount 1 `shouldBe` "1"
        it "prints fractions with full precision" $
            formatAmount 8.5 `shouldBe` "8.5"
        it "round-trips a small scientific value" $
            (read (T.unpack (formatAmount 1.0e-3)) :: Double) `shouldBe` 1.0e-3

    describe "renderCategories" $ do
        it "joins compartment and subcompartment with ::" $
            renderCategories (Just (Compartment "natural resource" (Just "in water")))
                `shouldBe` "natural resource::in water"
        it "emits the bare medium when there is no sub" $
            renderCategories (Just (Compartment "air" Nothing)) `shouldBe` "air"
        it "emits empty for no recorded compartment" $
            renderCategories Nothing `shouldBe` ""

    describe "round-trip" $ do
        it "(b) parse (write D) is structurally equal to D" $
            withWritten fixtureDb $ \path -> do
                parseBrightwayExcel defaultUnitConfig path >>= \case
                    Left err -> expectationFailure (T.unpack err)
                    Right parsed -> normalizeParsed parsed `shouldBe` expectedNormalized

        it "(a) re-exporting the parsed content yields the same logical cells" $
            withWritten fixtureDb $ \path -> do
                parseBrightwayExcel defaultUnitConfig path >>= \case
                    Left err -> expectationFailure (T.unpack err)
                    Right parsed -> do
                        let db' = rebuild parsed
                        logicalCells db' `shouldBe` logicalCells fixtureDb

        it "(c) preserves a biosphere inventory amount across the round-trip" $
            withWritten fixtureDb $ \path -> do
                parseBrightwayExcel defaultUnitConfig path >>= \case
                    Left err -> expectationFailure (T.unpack err)
                    Right parsed -> do
                        before <- co2Inventory fixtureDb
                        after <- co2Inventory (rebuild parsed)
                        case (before, after) of
                            (Right (Just b), Right (Just a)) ->
                                abs (a - b) < 1.0e-9 `shouldBe` True
                            other -> expectationFailure ("CO2 inventory missing: " <> show other)

-- ---------------------------------------------------------------------------
-- Fixture database, built with the parser's UUID conventions
-- ---------------------------------------------------------------------------

cfg :: WriterConfig
cfg = WriterConfig{wcDatabaseName = "Test_Inventory_DB"}

-- One activity producing electricity, consuming gas, emitting CO2.
fixtureDb :: SimpleDatabase
fixtureDb =
    SimpleDatabase
        { sdbActivities = M.singleton (generateActivityUUID elec, getReferenceProductUUID elec) elec
        , sdbTechFlows = M.fromList [(tfId f, f) | f <- [elecFlow, gasFlow]]
        , sdbBioFlows = M.fromList [(bfId co2, co2)]
        , sdbWasteFlows = M.empty
        , sdbUnits = M.fromList [(unitId u, u) | u <- [kwh, m3, kg]]
        }

elec :: Activity
elec =
    Activity
        { activityName = "Electricity production, natural gas"
        , activityDescription = []
        , activitySynonyms = M.empty
        , activityClassification = M.empty
        , activityLocation = "GLO"
        , activityUnit = "kilowatt hour"
        , exchanges = [prodExch, gasExch, co2Exch]
        , activityParams = M.empty
        , activityParamExprs = M.empty
        , activityAllocationPercent = Nothing
        , activityAllocationFormula = Nothing
        , activityNativeType = Nothing
        }

prodExch :: Exchange
prodExch =
    TechnosphereExchange
        { techFlowId = generateFlowUUID "electricity, high voltage" "" "kilowatt hour"
        , techAmount = 1
        , techUnitId = generateUnitUUID "kilowatt hour"
        , techRole = ReferenceProduct
        , techActivityLinkId = UUID.nil
        , techProcessLinkId = Nothing
        , techLocation = "GLO"
        , techComment = Nothing
        , techPedigree = Nothing
        }

gasExch :: Exchange
gasExch =
    TechnosphereExchange
        { techFlowId = generateFlowUUID "natural gas, high pressure" "" "cubic meter"
        , techAmount = 8.5
        , techUnitId = generateUnitUUID "cubic meter"
        , techRole = Input
        , techActivityLinkId = UUID.nil
        , techProcessLinkId = Nothing
        , techLocation = "RoW"
        , techComment = Nothing
        , techPedigree = Nothing
        }

co2Exch :: Exchange
co2Exch =
    BiosphereExchange
        { bioFlowId = bfId co2
        , bioAmount = 0.5
        , bioUnitId = generateUnitUUID "kilogram"
        , bioDirection = Emission
        , bioLocation = ""
        , bioComment = Nothing
        , bioPedigree = Nothing
        }

elecFlow :: TechnosphereFlow
elecFlow =
    TechnosphereFlow
        (generateFlowUUID "electricity, high voltage" "" "kilowatt hour")
        "electricity, high voltage"
        (generateUnitUUID "kilowatt hour")
        M.empty
        Nothing
        Nothing

gasFlow :: TechnosphereFlow
gasFlow =
    TechnosphereFlow
        (generateFlowUUID "natural gas, high pressure" "" "cubic meter")
        "natural gas, high pressure"
        (generateUnitUUID "cubic meter")
        M.empty
        Nothing
        Nothing

co2 :: BiosphereFlow
co2 =
    BiosphereFlow
        { bfId = generateFlowUUID "Carbon dioxide, fossil" "air" "kilogram"
        , bfName = "Carbon dioxide, fossil"
        , bfUnitId = generateUnitUUID "kilogram"
        , bfSynonyms = M.empty
        , bfCAS = Nothing
        , bfSubstanceId = Nothing
        , bfCompartment = Just (Compartment "air" Nothing)
        }

kwh, m3, kg :: Unit
kwh = mkUnit "kilowatt hour"
m3 = mkUnit "cubic meter"
kg = mkUnit "kilogram"

mkUnit :: Text -> Unit
mkUnit n = Unit (generateUnitUUID n) n n ""

-- ---------------------------------------------------------------------------
-- Structural comparison (order-insensitive)
-- ---------------------------------------------------------------------------

{- | A normalized, comparable projection of a parsed database: activity names
each with their reference product, sorted technosphere inputs, and sorted
biosphere flows (name, amount, compartment, direction). This captures the
semantic content the format can carry without depending on Map/Set ordering or
on fields the format does not represent (links, pedigree).
-}
data NormActivity = NormActivity
    { nName :: Text
    , nLocation :: Text
    , nRefProduct :: Maybe Text
    , nInputs :: [(Text, Double, Text)]
    , nBio :: [(Text, Double, Text, BioDirection)]
    }
    deriving (Eq, Show)

type Parsed = ([Activity], TechFlowDB, BioFlowDB, WasteFlowDB, UnitDB)

normalizeParsed :: Parsed -> [NormActivity]
normalizeParsed (acts, techDB, bioDB, _waste, unitDB) =
    sortOn nName (map norm acts)
  where
    norm a =
        NormActivity
            { nName = activityName a
            , nLocation = activityLocation a
            , nRefProduct = listTo [tfName f | ex <- exchanges a, exchangeIsReference ex, Just f <- [M.lookup (exchangeFlowId ex) techDB]]
            , nInputs =
                sortOn (\(n, _, _) -> n) $
                    [ (tfName f, techAmount ex, unitNameOf (techUnitId ex))
                    | ex@TechnosphereExchange{techRole = Input} <- exchanges a
                    , Just f <- [M.lookup (techFlowId ex) techDB]
                    ]
            , nBio =
                sortOn (\(n, _, _, _) -> n) $
                    [ (bfName f, bioAmount ex, bfCompartmentName f, bioDirection ex)
                    | ex@BiosphereExchange{} <- exchanges a
                    , Just f <- [M.lookup (bioFlowId ex) bioDB]
                    ]
            }
    unitNameOf uid = maybe "" unitName (M.lookup uid unitDB)
    listTo = \case
        (x : _) -> Just x
        [] -> Nothing

-- | The same projection over the original fixture database.
expectedNormalized :: [NormActivity]
expectedNormalized =
    normalizeParsed
        ( M.elems (sdbActivities fixtureDb)
        , sdbTechFlows fixtureDb
        , sdbBioFlows fixtureDb
        , sdbWasteFlows fixtureDb
        , sdbUnits fixtureDb
        )

-- ---------------------------------------------------------------------------
-- Logical-cell extraction (for idempotence)
-- ---------------------------------------------------------------------------

{- | Re-export the parsed 5-tuple to the writer's input shape. Activities are
keyed exactly as 'Database.Loader.loadBrightwayExcel' keys them.
-}
rebuild :: Parsed -> SimpleDatabase
rebuild (acts, techDB, bioDB, wasteDB, unitDB) =
    SimpleDatabase
        { sdbActivities = M.fromList [((generateActivityUUID a, getReferenceProductUUID a), a) | a <- acts]
        , sdbTechFlows = techDB
        , sdbBioFlows = bioDB
        , sdbWasteFlows = wasteDB
        , sdbUnits = unitDB
        }

{- | The deterministic logical cells the writer would emit for a database: the
per-activity 'activityRows' (which already carry every value the format records,
addressed by column). Comparing these is the logical-cell idempotence contract —
byte identity of the zip is deliberately not required.
-}
logicalCells :: SimpleDatabase -> [[[Cell]]]
logicalCells db =
    [ activityRows cfg db a
    | a <- sortOn (\x -> (activityName x, activityLocation x)) (M.elems (sdbActivities db))
    ]

-- ---------------------------------------------------------------------------
-- Scoring (c)
-- ---------------------------------------------------------------------------

-- | CO2 biosphere inventory for the electricity activity (ProcessId 0).
co2Inventory :: SimpleDatabase -> IO (Either Text (Maybe Double))
co2Inventory sdb = do
    built <-
        buildDatabaseWithMatrices
            defaultUnitConfig
            (sdbActivities sdb)
            (sdbTechFlows sdb)
            (sdbBioFlows sdb)
            (sdbWasteFlows sdb)
            (sdbUnits sdb)
    case built of
        Left err -> pure (Left err)
        Right db -> do
            inv <- computeInventoryMatrix db 0
            pure (Right (lookupCo2 db inv))
  where
    lookupCo2 db inv =
        case find ((== "Carbon dioxide, fossil") . bfName) (M.elems (dbBioFlows db)) of
            Just f -> M.lookup (bfId f) inv
            Nothing -> Nothing

-- ---------------------------------------------------------------------------
-- File helper
-- ---------------------------------------------------------------------------

withWritten :: SimpleDatabase -> (FilePath -> IO a) -> IO a
withWritten db action =
    withSystemTempFile "brightway-writer.xlsx" $ \path h -> do
        BL.hPut h (renderWorkbook cfg db)
        hClose h
        action path
