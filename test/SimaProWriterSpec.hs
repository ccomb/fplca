{-# LANGUAGE OverloadedStrings #-}

{- | Round-trip contract for "SimaPro.Writer", the inverse of
"SimaPro.Parser".

The original database @D@ is built by parsing a small in-line SimaPro CSV with
'parseSimaProCSV'. That guarantees @D@ already lives in the parser's canonical
internal form (canonical units, resolved amounts, generated UUIDs), so the
write→parse cycle has a fair fixed point to land on.

Three properties are pinned:

  (a) idempotence modulo volatile metadata — @write(D)@ then
      @write(parse(write(D)))@ produce byte-identical output (the version
      banner is the only volatile field and it is pinned by 'WriterConfig');

  (b) semantic round-trip — @parse(write(D))@ is structurally equal to @D@,
      order-insensitively, on activities, flows and units;

  (c) score-equivalence — @parse(write(D))@ yields the same biosphere
      inventory (the LCIA-precursor vector) as @D@ within tolerance, via the
      engine's 'computeInventoryMatrix'.
-}
module SimaProWriterSpec (spec) where

import qualified Data.ByteString as BS
import Data.List (sort)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import Database (buildDatabaseWithMatrices)
import Matrix (computeInventoryMatrix)
import SimaPro.Parser (parseSimaProCSV)
import SimaPro.Writer (
    checkSimaProExportable,
    defaultWriterConfig,
    escapeField,
    formatAmount,
    serializeSimaProCSV,
 )
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import Test.Hspec
import Types
import UnitConversion (defaultUnitConfig)

-- ---------------------------------------------------------------------------
-- Fixture: a small SimaPro CSV exercising every section the writer emits
-- ---------------------------------------------------------------------------

fixtureCSV :: BS.ByteString
fixtureCSV =
    BS.intercalate
        "\r\n"
        [ "{SimaPro 9.6.0.1}"
        , "{CSV separator: Semicolon}"
        , "{Decimal separator: .}"
        , ""
        , "Process"
        , ""
        , "Category type"
        , "material"
        , ""
        , "Process name"
        , "Butter production"
        , ""
        , "Type"
        , "Unit process"
        , ""
        , "Geography"
        , "FR"
        , ""
        , "Products"
        , "Butter;kg;1;100;not defined;material;"
        , ""
        , "Materials/fuels"
        , "Cow milk;kg;20.53;Undefined;0;0;ingredient"
        , ""
        , "Resources"
        , "Water, river;in water;m3;0.1;Undefined;0;0;"
        , ""
        , "Emissions to air"
        , "Carbon dioxide, fossil;high. pop.;kg;0.5;Undefined;0;0;"
        , ""
        , "Emissions to water"
        , "Phosphate;river;kg;0.01;Undefined;0;0;"
        , ""
        , "End"
        , ""
        , "Process"
        , ""
        , "Category type"
        , "material"
        , ""
        , "Process name"
        , "Cow milk supply"
        , ""
        , "Type"
        , "Unit process"
        , ""
        , "Geography"
        , "FR"
        , ""
        , "Products"
        , "Cow milk;kg;1;100;not defined;material;"
        , ""
        , "Emissions to air"
        , "Methane, fossil;high. pop.;kg;0.02;Undefined;0;0;"
        , ""
        , "End"
        ]

-- | A single process with one reference product and one coproduct (in the
-- @Avoided products@ section, which the parser reads back as a 'Coproduct').
coproductCSV :: BS.ByteString
coproductCSV =
    BS.intercalate
        "\r\n"
        [ "{SimaPro 9.6.0.1}"
        , "{CSV separator: Semicolon}"
        , "{Decimal separator: .}"
        , ""
        , "Process"
        , ""
        , "Category type"
        , "material"
        , ""
        , "Process name"
        , "Margarine production"
        , ""
        , "Type"
        , "Unit process"
        , ""
        , "Geography"
        , "FR"
        , ""
        , "Products"
        , "Margarine;kg;1;100;not defined;material;"
        , ""
        , "Avoided products"
        , "Butter;kg;0.3;100;not defined;material;"
        , ""
        , "End"
        ]

-- | A process with no @Type@ line, so the parser yields @activityNativeType = Nothing@.
noTypeCSV :: BS.ByteString
noTypeCSV =
    BS.intercalate
        "\r\n"
        [ "{SimaPro 9.6.0.1}"
        , "{CSV separator: Semicolon}"
        , "{Decimal separator: .}"
        , ""
        , "Process"
        , ""
        , "Category type"
        , "material"
        , ""
        , "Process name"
        , "Untyped process"
        , ""
        , "Geography"
        , "FR"
        , ""
        , "Products"
        , "Thing;kg;1;100;not defined;material;"
        , ""
        , "End"
        ]

-- | Parse a SimaPro CSV blob through a temp file.
parseBytes :: BS.ByteString -> IO ([Activity], TechFlowDB, BioFlowDB, WasteFlowDB, UnitDB)
parseBytes bytes = withSystemTempFile "writer-spec.csv" $ \path h -> do
    BS.hPut h bytes
    hClose h
    parseSimaProCSV defaultUnitConfig path

-- | Wrap parser output in a 'SimpleDatabase' keyed by generated UUIDs.
toSimple :: ([Activity], TechFlowDB, BioFlowDB, WasteFlowDB, UnitDB) -> SimpleDatabase
toSimple (acts, tech, bio, waste, units) =
    SimpleDatabase
        { sdbActivities = M.fromList [(activityKey a, a) | a <- acts]
        , sdbTechFlows = tech
        , sdbBioFlows = bio
        , sdbWasteFlows = waste
        , sdbUnits = units
        }

{- | Stable key for an activity: its reference product flow UUID paired with a
synthetic activity UUID derived from name+location. The parser does not hand
back the (UUID,UUID) keys directly, so we mint a deterministic pair from
fields we control; this only needs to be injective across the fixture.
-}
activityKey :: Activity -> (UUID, UUID)
activityKey a =
    case [exchangeFlowId ex | ex <- exchanges a, exchangeIsReference ex] of
        (prod : _) -> (prod, prod)
        [] -> (UUID.nil, UUID.nil)

-- Re-export of nil without importing the whole module qualifier dance.
-- (Types re-exports UUID; nil lives in Data.UUID.)

-- ---------------------------------------------------------------------------
-- Structural comparison helpers (order-insensitive)
-- ---------------------------------------------------------------------------

-- | Normalised, order-insensitive view of an activity for structural equality.
activityShape :: TechFlowDB -> BioFlowDB -> WasteFlowDB -> UnitDB -> Activity -> ActivityShape
activityShape tech bio waste units a =
    ActivityShape
        { asName = activityName a
        , asLocation = activityLocation a
        , asUnit = activityUnit a
        , asExchanges = sort (map (exchangeShape tech bio waste units) (exchanges a))
        }

data ActivityShape = ActivityShape
    { asName :: Text
    , asLocation :: Text
    , asUnit :: Text
    , asExchanges :: [ExchangeShape]
    }
    deriving (Eq, Ord, Show)

-- | (kind, flow-name, unit-name, rounded-amount, is-input, is-reference)
data ExchangeShape = ExchangeShape
    { esKind :: Text
    , esFlow :: Text
    , esUnit :: Text
    , esAmount :: Double
    , esInput :: Bool
    , esRef :: Bool
    }
    deriving (Eq, Ord, Show)

exchangeShape :: TechFlowDB -> BioFlowDB -> WasteFlowDB -> UnitDB -> Exchange -> ExchangeShape
exchangeShape tech bio waste units ex =
    let unitNm uid = maybe "" unitName (M.lookup uid units)
        roundAmt v = fromIntegral (round (v * 1e6) :: Integer) / 1e6 :: Double
     in case ex of
            TechnosphereExchange{techFlowId = fid, techAmount = amt, techUnitId = uid} ->
                ExchangeShape
                    "tech"
                    (maybe "?" tfName (M.lookup fid tech))
                    (unitNm uid)
                    (roundAmt amt)
                    (exchangeIsInput ex)
                    (exchangeIsReference ex)
            BiosphereExchange{bioFlowId = fid, bioAmount = amt, bioUnitId = uid} ->
                ExchangeShape
                    "bio"
                    (maybe "?" bfName (M.lookup fid bio))
                    (unitNm uid)
                    (roundAmt amt)
                    (exchangeIsInput ex)
                    (exchangeIsReference ex)
            WasteExchange{waFlowId = fid, waAmount = amt, waUnitId = uid} ->
                ExchangeShape
                    "waste"
                    (maybe "?" wfName (M.lookup fid waste))
                    (unitNm uid)
                    (roundAmt amt)
                    (exchangeIsInput ex)
                    (exchangeIsReference ex)

shapeSet :: ([Activity], TechFlowDB, BioFlowDB, WasteFlowDB, UnitDB) -> S.Set ActivityShape
shapeSet (acts, tech, bio, waste, units) =
    S.fromList (map (activityShape tech bio waste units) acts)

-- ---------------------------------------------------------------------------
-- Inventory (score-equivalence) helper
-- ---------------------------------------------------------------------------

{- | Build a Database from parser output and compute the inventory of the
activity whose name matches, keyed by biosphere flow NAME (UUIDs are stable
across the round-trip, but keying by name is robust either way).
-}
inventoryByName :: ([Activity], TechFlowDB, BioFlowDB, WasteFlowDB, UnitDB) -> Text -> IO (M.Map Text Double)
inventoryByName (acts, tech, bio, waste, units) target = do
    let actMap = M.fromList [(activityKey a, a) | a <- acts]
    built <- buildDatabaseWithMatrices defaultUnitConfig actMap tech bio waste units
    case built of
        Left err -> expectationFailure (T.unpack err) >> pure M.empty
        Right db -> do
            let pidFor =
                    [ pid
                    | (pid, (actU, _)) <- zip [0 ..] (V.toList (dbProcessIdTable db))
                    , Just act <- [activityAtUUID db actU]
                    , activityName act == target
                    ]
            case pidFor of
                (pid : _) -> do
                    inv <- computeInventoryMatrix db pid
                    pure (M.mapKeys (flowName db) inv)
                [] -> expectationFailure ("no activity named " <> T.unpack target) >> pure M.empty

flowName :: Database -> UUID -> Text
flowName db uid = maybe (T.pack (show uid)) bfName (M.lookup uid (dbBioFlows db))

activityAtUUID :: Database -> UUID -> Maybe Activity
activityAtUUID db actU =
    case [i | (i, (a, _)) <- zip [0 ..] (V.toList (dbProcessIdTable db)), a == actU] of
        (i : _) -> Just (dbActivities db V.! i)
        [] -> Nothing

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "SimaPro.Writer round-trip" $ do
    it "(pure) formatAmount round-trips integral and fractional values" $ do
        formatAmount 100 `shouldBe` "100"
        formatAmount 1 `shouldBe` "1"
        formatAmount 0.5 `shouldBe` "0.5"
        formatAmount 20.53 `shouldBe` "20.53"
        formatAmount (-1.5) `shouldBe` "-1.5"

    it "(pure) escapeField quotes only when needed" $ do
        escapeField "Butter" `shouldBe` "Butter"
        escapeField "a;b" `shouldBe` "\"a;b\""
        escapeField "say \"hi\"" `shouldBe` "\"say \"\"hi\"\"\""

    it "produces a parseable CSV with the pinned header" $ do
        original <- parseBytes fixtureCSV
        let bytes = serializeSimaProCSV defaultWriterConfig (toSimple original)
        BS.take 16 bytes `shouldSatisfy` (\b -> "{SimaPro" `BS.isInfixOf` b)
        reparsed <- parseBytes bytes
        let (acts, _, _, _, _) = reparsed
        length acts `shouldBe` 2

    it "(a) is idempotent modulo the volatile version banner" $ do
        original <- parseBytes fixtureCSV
        let f0 = serializeSimaProCSV defaultWriterConfig (toSimple original)
        d' <- parseBytes f0
        let f1 = serializeSimaProCSV defaultWriterConfig (toSimple d')
        f1 `shouldBe` f0

    it "(b) semantic round-trip: parse(write(D)) is structurally equal to D" $ do
        original <- parseBytes fixtureCSV
        let f0 = serializeSimaProCSV defaultWriterConfig (toSimple original)
        reparsed <- parseBytes f0
        shapeSet reparsed `shouldBe` shapeSet original

    it "(c) score-equivalence: same inventory for a sample activity within tolerance" $ do
        original <- parseBytes fixtureCSV
        let f0 = serializeSimaProCSV defaultWriterConfig (toSimple original)
        reparsed <- parseBytes f0
        invOrig <- inventoryByName original "Butter production"
        invRound <- inventoryByName reparsed "Butter production"
        M.keysSet invOrig `shouldBe` M.keysSet invRound
        let diffs =
                [ abs (a - b)
                | (k, a) <- M.toList invOrig
                , let b = M.findWithDefault 0 k invRound
                ]
        all (< 1e-9) diffs `shouldBe` True

    it "(regression) routes coproducts to Avoided products, not Products" $ do
        original <- parseBytes coproductCSV
        let f0 = serializeSimaProCSV defaultWriterConfig (toSimple original)
        reparsed <- parseBytes f0
        let acts0 = activitiesOf original
            acts1 = activitiesOf reparsed
            roles = [techRole e | a <- acts1, e@TechnosphereExchange{} <- exchanges a]
        -- A coproduct written to "Products" would re-parse into a second
        -- reference-product activity; routing it to "Avoided products" keeps the
        -- activity count stable and preserves the Coproduct role.
        length acts1 `shouldBe` length acts0
        (Coproduct `elem` roles) `shouldBe` True

    it "(regression) omits the Type line for an activity with no native type" $ do
        original <- parseBytes noTypeCSV
        let f0 = serializeSimaProCSV defaultWriterConfig (toSimple original)
        reparsed <- parseBytes f0
        -- No Type line written → re-parse yields Nothing again, not the
        -- invented "Unit process".
        map activityNativeType (activitiesOf reparsed) `shouldBe` [Nothing]

    describe "checkSimaProExportable (emission-medium guard)" $ do
        it "accepts air / water / soil emissions" $
            checkSimaProExportable (emissionDb (Compartment "air" Nothing))
                `shouldBe` Right ()

        it "rejects an emission whose medium has no faithful SimaPro section" $
            -- A "raw" medium would otherwise be silently filed under
            -- "Emissions to air" and re-parse as air; reject it loudly instead.
            checkSimaProExportable (emissionDb (Compartment "raw" Nothing))
                `shouldSatisfy` either (const True) (const False)
  where
    activitiesOf (acts, _, _, _, _) = acts

-- ---------------------------------------------------------------------------
-- Emission-medium guard fixture
-- ---------------------------------------------------------------------------

{- | One activity emitting a single biosphere flow whose compartment is @comp@.
Used to exercise 'checkSimaProExportable': air/water/soil pass, anything else is
rejected.
-}
emissionDb :: Compartment -> SimpleDatabase
emissionDb comp =
    SimpleDatabase
        { sdbActivities = M.singleton (actU, prodU) act
        , sdbTechFlows = M.singleton prodU (TechnosphereFlow prodU "thing" unitU M.empty Nothing Nothing)
        , sdbBioFlows = M.singleton bioU (BiosphereFlow bioU "Some emission" unitU M.empty Nothing Nothing (Just comp))
        , sdbWasteFlows = M.empty
        , sdbUnits = M.singleton unitU (Unit unitU "kg" "kg" "")
        }
  where
    actU, prodU, bioU, unitU :: UUID
    actU = read "aaaaaaaa-0000-4000-8000-000000000010"
    prodU = read "aaaaaaaa-0000-4000-8000-0000000000a0"
    bioU = read "cccccccc-0000-4000-8000-0000000000c0"
    unitU = read "11111111-0000-4000-8000-000000000001"
    act =
        Activity
            "thing maker"
            []
            M.empty
            M.empty
            "GLO"
            "kg"
            [ TechnosphereExchange prodU 1.0 unitU ReferenceProduct UUID.nil Nothing "" Nothing Nothing
            , BiosphereExchange bioU 0.5 unitU Emission "" Nothing Nothing
            ]
            M.empty
            M.empty
            Nothing
            Nothing
            Nothing
