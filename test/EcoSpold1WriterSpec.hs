{-# LANGUAGE OverloadedStrings #-}

{- | Round-trip contract for the EcoSpold1 writer (the inverse of
"EcoSpold.Parser1"). Three properties:

  (a) idempotence modulo volatile metadata — write, parse, write again,
      and the two serialisations are byte-identical when the volatile
      @generator@/@timestamp@ attributes are omitted ('canonicalWriterOptions');
  (b) semantic round-trip — parse(write(D)) reproduces the observable
      structure of D (names, amounts, units, roles/directions, compartments,
      CAS, comments), compared order-insensitively;
  (c) score-equivalence — a sample activity yields the same direct LCIA
      inventory (biosphere flow amounts, keyed by flow name) after a
      write→parse→build round-trip, within tolerance.
-}
module EcoSpold1WriterSpec (spec) where

import qualified Data.ByteString.Char8 as BC
import Data.Either (isLeft)
import Data.List (sort)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID
import Test.Hspec

import qualified Database as DB
import Database.Loader (generateActivityUUIDFromActivity)
import EcoSpold.Parser1 (parseAllWithXeno)
import EcoSpold.Writer1
import qualified Matrix
import Types
import UnitConversion (defaultUnitConfig)

-- ---------------------------------------------------------------------------
-- Fixture: a small in-memory database, distilled from EcoSpold1Spec's minimalXml
-- (one production activity: 1 kWh electricity, with a fossil-CO2 emission and
--  a natural-gas resource input). Built straight from the parser output so the
-- flow/unit UUIDs already line up with the EcoSpold1 UUID scheme.
-- ---------------------------------------------------------------------------

minimalXml :: BC.ByteString
minimalXml =
    BC.unlines
        [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        , "<ecoSpold xmlns=\"http://www.EcoInvent.org/EcoSpold01\">"
        , "  <dataset number=\"42\">"
        , "    <metaInformation>"
        , "      <processInformation>"
        , "        <referenceFunction name=\"electricity production\" category=\"Energy\""
        , "                           subCategory=\"Electricity\" unit=\"kWh\""
        , "                           generalComment=\"A comment\"/>"
        , "        <geography location=\"DE\" />"
        , "      </processInformation>"
        , "    </metaInformation>"
        , "    <flowData>"
        , "      <exchange number=\"1\" name=\"electricity, high voltage\" category=\"Energy\""
        , "                subCategory=\"Electricity\" unit=\"kWh\" meanValue=\"1.0\">"
        , "        <outputGroup>0</outputGroup>"
        , "      </exchange>"
        , "      <exchange number=\"2\" name=\"Carbon dioxide, fossil\" category=\"air\""
        , "                subCategory=\"low population density\" unit=\"kg\" meanValue=\"0.05\""
        , "                CASNumber=\"124-38-9\">"
        , "        <outputGroup>4</outputGroup>"
        , "      </exchange>"
        , "      <exchange number=\"3\" name=\"natural gas\" category=\"resource\""
        , "                subCategory=\"in ground\" unit=\"MJ\" meanValue=\"10.0\">"
        , "        <inputGroup>4</inputGroup>"
        , "      </exchange>"
        , "    </flowData>"
        , "  </dataset>"
        , "</ecoSpold>"
        ]

{- | Parsed fixture as a 'SimpleDatabase'. Fails the spec loudly if the
fixture can't be parsed (it is a known-good EcoSpold1 document).
-}
fixtureDb :: IO SimpleDatabase
fixtureDb = case parseAllWithXeno minimalXml of
    Left err -> fail ("fixture parse failed: " ++ err)
    Right results -> case sequence results of
        Left err -> fail ("fixture dataset failed: " ++ err)
        Right datasets -> pure (assembleSimpleDb datasets)

-- | Fold parser per-dataset tuples into a 'SimpleDatabase'.
assembleSimpleDb ::
    [(Activity, [TechnosphereFlow], [BiosphereFlow], [WasteFlow], [Unit], Int, M.Map UUID Int)] ->
    SimpleDatabase
assembleSimpleDb datasets =
    SimpleDatabase
        { sdbActivities = M.fromList [(processKey a, a) | (a, _, _, _, _, _, _) <- datasets]
        , sdbTechFlows = M.fromList [(tfId f, f) | (_, tfs, _, _, _, _, _) <- datasets, f <- tfs]
        , sdbBioFlows = M.fromList [(bfId f, f) | (_, _, bfs, _, _, _, _) <- datasets, f <- bfs]
        , sdbWasteFlows = M.fromList [(wfId f, f) | (_, _, _, wfs, _, _, _) <- datasets, f <- wfs]
        , sdbUnits = M.fromList [(unitId u, u) | (_, _, _, _, us, _, _) <- datasets, u <- us]
        }

{- | The (activityUUID, productUUID) key the matrix builder expects. We use the
reference exchange's flow id as the product, and derive a stable activity id
from name+location via the same namespace the Loader uses is unnecessary
here: the writer round-trip only depends on flow ids, so any injective key
works. We reuse the reference product flow id for both slots.
-}
processKey :: Activity -> (UUID, UUID)
processKey act =
    case mapMaybe refId (exchanges act) of
        (fid : _) -> (fid, fid)
        [] -> case exchanges act of
            (e : _) -> (exchangeFlowId e, exchangeFlowId e)
            [] -> (UUID.nil, UUID.nil)
  where
    refId e = if exchangeIsReference e then Just (exchangeFlowId e) else Nothing

-- ---------------------------------------------------------------------------
-- Hand-built fixtures for boundary cases (empty DB, escaping/unicode, linked
-- supplier, Final-waste-flows, non-finite amount). Built straight from the
-- domain types so the writer's export-boundary checks can be exercised on data
-- the parser would never itself produce (a NaN amount, a dangling link).
-- ---------------------------------------------------------------------------

kgUnit :: UUID
kgUnit = read "11111111-0000-4000-8000-000000000001"

units1 :: UnitDB
units1 = M.singleton kgUnit (Unit kgUnit "kg" "kg" "")

{- | A single-activity database with one reference product and the given
extra exchanges, named @name@ so its flows resolve from @techs@/@bios@/@wastes@.
-}
soloDb :: Text -> UUID -> [Exchange] -> TechFlowDB -> BioFlowDB -> WasteFlowDB -> SimpleDatabase
soloDb name prodU extra techs bios wastes =
    SimpleDatabase
        { sdbActivities = M.singleton (prodU, prodU) act
        , sdbTechFlows = M.insert prodU (TechnosphereFlow prodU name kgUnit M.empty Nothing Nothing) techs
        , sdbBioFlows = bios
        , sdbWasteFlows = wastes
        , sdbUnits = units1
        }
  where
    ref = TechnosphereExchange prodU 1.0 kgUnit ReferenceProduct UUID.nil Nothing "" Nothing Nothing
    act = Activity name [] M.empty M.empty "GLO" "kg" (ref : extra) M.empty M.empty Nothing Nothing Nothing

-- | Empty database: no activities, no flows.
emptyDb :: SimpleDatabase
emptyDb = SimpleDatabase M.empty M.empty M.empty M.empty M.empty

{- | Two activities where the consumer's technosphere input is a resolved link
to the supplier. The link UUID is the loader's content hash of the supplier's
@(name, location)@, which is exactly the key 'checkEcoSpold1Exportable' /
'supplierNumberIndex' resolve to a dataset number. Passing a UUID absent from
the database instead models a dangling supplier link.
-}
linkedDb :: UUID -> SimpleDatabase
linkedDb link =
    SimpleDatabase
        { sdbActivities = M.fromList [((supU, supU), supplier), ((conU, conU), consumer)]
        , sdbTechFlows =
            M.fromList
                [ (supU, TechnosphereFlow supU "aaa supplier" kgUnit M.empty Nothing Nothing)
                , (conU, TechnosphereFlow conU "bbb consumer" kgUnit M.empty Nothing Nothing)
                ]
        , sdbBioFlows = M.empty
        , sdbWasteFlows = M.empty
        , sdbUnits = units1
        }
  where
    supU = read "22222222-0000-4000-8000-000000000001"
    conU = read "33333333-0000-4000-8000-000000000001"
    mkAct nm prodU exs = Activity nm [] M.empty M.empty "GLO" "kg" (refOf prodU : exs) M.empty M.empty Nothing Nothing Nothing
    refOf prodU = TechnosphereExchange prodU 1.0 kgUnit ReferenceProduct UUID.nil Nothing "" Nothing Nothing
    supplier = mkAct "aaa supplier" supU []
    -- The consumer's input consumes the supplier's product and links to it.
    consumer = mkAct "bbb consumer" conU [TechnosphereExchange supU 2.0 kgUnit Input link Nothing "" Nothing Nothing]

-- | The supplier's resolved activity UUID, the value a consumer's input links to.
supplierLink :: UUID
supplierLink =
    generateActivityUUIDFromActivity
        (Activity "aaa supplier" [] M.empty M.empty "GLO" "kg" [] M.empty M.empty Nothing Nothing Nothing)

{- | A non-nil UUID that no exported activity hashes to, so a consumer input
carrying it has no resolvable dataset number — a dangling link. Distinct from
'UUID.nil', which the guard treats as "no link".
-}
danglingLink :: UUID
danglingLink = read "99999999-0000-4000-8000-000000000099"

{- | All supplier-dataset-number values recorded across a parser round-trip of
@sdb@. 'EcoSpold.Parser1.closeExchange' fills 'psSupplierLinks' (the 7th tuple
slot) for every technosphere input whose @number@ attribute is non-zero — i.e.
the supplier dataset number the writer must re-emit. A surviving link therefore
shows up here as the supplier's dataset number.
-}
roundTripSupplierLinks :: SimpleDatabase -> Either String [Int]
roundTripSupplierLinks sdb =
    case writeSimpleDatabase canonicalWriterOptions sdb of
        Left e -> Left (T.unpack e)
        Right txt ->
            case parseAllWithXeno (TE.encodeUtf8 txt) of
                Left err -> Left err
                Right results ->
                    concatMap (\(_, _, _, _, _, _, links) -> M.elems links) <$> sequence results

-- ---------------------------------------------------------------------------
-- Order-insensitive observable projection of an activity, for semantic equality
-- ---------------------------------------------------------------------------

{- | Observable, UUID-free fingerprint of one exchange. Names come from the
flow tables; everything else is intrinsic to the exchange.
-}
data ExchangeView = ExchangeView
    { evKind :: !Text
    , evName :: !Text
    , evAmount :: !Double
    , evUnit :: !Text
    , evRole :: !Text
    , evLocation :: !Text
    , evCas :: !(Maybe Text)
    , evComment :: !(Maybe Text)
    }
    deriving (Eq, Ord, Show)

exchangeViews :: SimpleDatabase -> Activity -> [ExchangeView]
exchangeViews sdb = map (exchangeView sdb) . exchanges

exchangeView :: SimpleDatabase -> Exchange -> ExchangeView
exchangeView sdb ex = case ex of
    TechnosphereExchange{techFlowId = fid, techRole = role} ->
        base "tech" (techName fid) (roleText role)
    BiosphereExchange{bioFlowId = fid, bioDirection = dir} ->
        base "bio" (bioName fid) (dirText dir)
    WasteExchange{waFlowId = fid, waIsInput = isInput} ->
        base "waste" (wasteName fid) (if isInput then "input" else "output")
  where
    base kind nm role =
        ExchangeView
            { evKind = kind
            , evName = nm
            , evAmount = exchangeAmount ex
            , evUnit = maybe "" unitName (M.lookup (exchangeUnitId ex) (sdbUnits sdb))
            , evRole = role
            , evLocation = exchangeLocation ex
            , evCas = casOf ex
            , evComment = exchangeComment ex
            }
    techName fid = maybe "" tfName (M.lookup fid (sdbTechFlows sdb))
    bioName fid = maybe "" bfName (M.lookup fid (sdbBioFlows sdb))
    wasteName fid = maybe "" wfName (M.lookup fid (sdbWasteFlows sdb))
    casOf e = case e of
        TechnosphereExchange{techFlowId = fid} -> M.lookup fid (sdbTechFlows sdb) >>= tfCAS
        BiosphereExchange{bioFlowId = fid} -> M.lookup fid (sdbBioFlows sdb) >>= bfCAS
        WasteExchange{waFlowId = fid} -> M.lookup fid (sdbWasteFlows sdb) >>= wfCAS
    roleText r = case r of
        ReferenceProduct -> "ref"
        ReferenceInput -> "ref"
        Coproduct -> "coproduct"
        Input -> "input"
    dirText d = case d of
        Resource -> "resource"
        Emission -> "emission"

{- | An order-insensitive fingerprint of the whole database: per-activity
(name, location, unit, classification, sorted exchange views).
-}
data ActivityView = ActivityView
    { avName :: !Text
    , avLocation :: !Text
    , avUnit :: !Text
    , avClassification :: ![(Text, Text)]
    , avExchanges :: ![ExchangeView]
    }
    deriving (Eq, Ord, Show)

dbViews :: SimpleDatabase -> [ActivityView]
dbViews sdb =
    sort
        [ ActivityView
            { avName = activityName a
            , avLocation = activityLocation a
            , avUnit = activityUnit a
            , avClassification = M.toList (activityClassification a)
            , avExchanges = sort (exchangeViews sdb a)
            }
        | a <- M.elems (sdbActivities sdb)
        ]

-- ---------------------------------------------------------------------------
-- Parse helper: write → parse back into a SimpleDatabase
-- ---------------------------------------------------------------------------

roundTrip :: SimpleDatabase -> Either String SimpleDatabase
roundTrip sdb =
    case writeSimpleDatabase canonicalWriterOptions sdb of
        Left e -> Left (T.unpack e)
        Right txt ->
            case parseAllWithXeno (TE.encodeUtf8 txt) of
                Left err -> Left err
                Right results -> assembleSimpleDb <$> sequence results

{- | Unwrap the guard-returning writer for a fixture known to be exportable,
failing the test on an unexpected 'Left'.
-}
writeOk :: WriterOptions -> SimpleDatabase -> IO Text
writeOk opts sdb =
    either (\e -> expectationFailure (T.unpack e) >> pure "") pure $
        writeSimpleDatabase opts sdb

-- | Build a full 'Database' from a 'SimpleDatabase' so we can score it.
buildDb :: SimpleDatabase -> IO Database
buildDb sdb = do
    result <-
        DB.buildDatabaseWithMatrices
            defaultUnitConfig
            (sdbActivities sdb)
            (sdbTechFlows sdb)
            (sdbBioFlows sdb)
            (sdbWasteFlows sdb)
            (sdbUnits sdb)
    case result of
        Left err -> fail ("buildDatabaseWithMatrices failed: " ++ T.unpack err)
        Right db -> pure db

{- | Direct inventory of the reference activity, keyed by biosphere flow NAME
(UUIDs are regenerated across a round-trip, names are invariant).
-}
inventoryByName :: Database -> IO (M.Map Text Double)
inventoryByName db = do
    let pid = 0 -- single-activity fixture
    inv <- Matrix.computeInventoryMatrix db pid
    pure $
        M.fromListWith
            (+)
            [ (bfName bf, amt)
            | (fid, amt) <- M.toList inv
            , Just bf <- [M.lookup fid (dbBioFlows db)]
            ]

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
    describe "escapeXmlAttr" $ do
        it "escapes the five metacharacters and newlines" $
            escapeXmlAttr "a<b>&\"'\n"
                `shouldBe` "a&lt;b&gt;&amp;&quot;&apos;&#10;"

        it "is a left-inverse of the parser's entity decoder (no double-escape)" $
            -- The lone & must become &amp; exactly once.
            escapeXmlAttr "Tom & Jerry" `shouldBe` "Tom &amp; Jerry"

    describe "formatAmount" $ do
        it "normalises negative zero" $
            formatAmount (-0.0) `shouldBe` "0.0"

        it "keeps a whole number parser-readable" $
            formatAmount 1.0 `shouldBe` "1.0"

    describe "writeSimpleDatabase" $ do
        it "emits a well-formed EcoSpold1 document the parser accepts" $ do
            sdb <- fixtureDb
            xml <- writeOk canonicalWriterOptions sdb
            T.isInfixOf "<ecoSpold" xml `shouldBe` True
            case parseAllWithXeno (TE.encodeUtf8 xml) of
                Left err -> expectationFailure ("re-parse failed: " ++ err)
                Right results -> length results `shouldBe` 1

        it "omits volatile attributes under canonicalWriterOptions" $ do
            sdb <- fixtureDb
            xml <- writeOk canonicalWriterOptions sdb
            T.isInfixOf "generator=" xml `shouldBe` False
            T.isInfixOf "timestamp=" xml `shouldBe` False

        it "includes a pinned generator under defaultWriterOptions" $ do
            sdb <- fixtureDb
            xml <- writeOk defaultWriterOptions sdb
            T.isInfixOf "generator=\"VoLCA\"" xml `shouldBe` True

    describe "round-trip (a) idempotence modulo volatile metadata" $
        it "write . parse . write == write (canonical)" $ do
            sdb <- fixtureDb
            f0 <- writeOk canonicalWriterOptions sdb
            case parseAllWithXeno (TE.encodeUtf8 f0) of
                Left err -> expectationFailure ("re-parse failed: " ++ err)
                Right results -> case sequence results of
                    Left err -> expectationFailure ("dataset failed: " ++ err)
                    Right datasets -> do
                        let sdb' = assembleSimpleDb datasets
                        f1 <- writeOk canonicalWriterOptions sdb'
                        f1 `shouldBe` f0

    describe "round-trip (b) semantic equality (order-insensitive)" $
        it "parse(write(D)) reproduces the observable structure of D" $ do
            sdb <- fixtureDb
            case roundTrip sdb of
                Left err -> expectationFailure ("round-trip failed: " ++ err)
                Right sdb' -> dbViews sdb' `shouldBe` dbViews sdb

    describe "round-trip (c) score-equivalence" $
        it "yields the same direct biosphere inventory within tolerance" $ do
            sdb <- fixtureDb
            case roundTrip sdb of
                Left err -> expectationFailure ("round-trip failed: " ++ err)
                Right sdb' -> do
                    db0 <- buildDb sdb
                    db1 <- buildDb sdb'
                    inv0 <- inventoryByName db0
                    inv1 <- inventoryByName db1
                    M.keys inv1 `shouldBe` M.keys inv0
                    let near a b = abs (a - b) < 1e-9
                    and (M.elems (M.intersectionWith near inv0 inv1)) `shouldBe` True
                    M.null inv0 `shouldBe` False

    describe "supplier links (multi-dataset)" $ do
        it "re-emits a resolved supplier link as the supplier's dataset number" $
            -- "aaa supplier" sorts first → dataset 1; the consumer's input links
            -- to it, so the parser reads its number attribute back as 1.
            roundTripSupplierLinks (linkedDb supplierLink) `shouldBe` Right [1]

        it "passes the export-boundary check when the link targets an exported dataset" $
            checkEcoSpold1Exportable (linkedDb supplierLink) `shouldBe` Right ()

        it "rejects a dangling supplier link rather than emitting a wrong dataset number" $
            -- The link targets a UUID not among the exported datasets, so its
            -- dataset number is unknown; fail loudly instead of mislabelling it.
            checkEcoSpold1Exportable (linkedDb danglingLink) `shouldSatisfy` isLeft

    describe "empty database" $ do
        it "writes a well-formed, dataset-free document the parser accepts" $ do
            xml <- writeOk canonicalWriterOptions emptyDb
            case parseAllWithXeno (TE.encodeUtf8 xml) of
                Left err -> expectationFailure ("re-parse failed: " ++ err)
                Right results -> length results `shouldBe` 0

        it "is exportable" $
            checkEcoSpold1Exportable emptyDb `shouldBe` Right ()

    describe "escaping / unicode" $
        it "round-trips XML metacharacters and non-ASCII in names" $ do
            let nm = "Cu <ore> & «café» 95% — 1\"" :: Text
                prodU = read "44444444-0000-4000-8000-000000000001" :: UUID
                sdb = soloDb nm prodU [] M.empty M.empty M.empty
            case roundTrip sdb of
                Left err -> expectationFailure ("round-trip failed: " ++ err)
                Right sdb' -> map activityName (M.elems (sdbActivities sdb')) `shouldBe` [nm]

    describe "Final waste flows" $
        it "round-trips a waste output as a waste flow (not a coproduct)" $ do
            let prodU = read "55555555-0000-4000-8000-000000000001" :: UUID
                wasteU = read "55555555-0000-4000-8000-0000000000a0" :: UUID
                wasteEx = WasteExchange wasteU 0.3 kgUnit False UUID.nil Nothing "" Nothing Nothing
                wastes = M.singleton wasteU (WasteFlow wasteU "spent solvent" kgUnit M.empty Nothing Nothing)
                sdb = soloDb "solvent user" prodU [wasteEx] M.empty M.empty wastes
            case roundTrip sdb of
                Left err -> expectationFailure ("round-trip failed: " ++ err)
                Right sdb' -> do
                    map wfName (M.elems (sdbWasteFlows sdb')) `shouldBe` ["spent solvent"]
                    let isWasteOutput WasteExchange{waIsInput = i} = not i
                        isWasteOutput TechnosphereExchange{} = False
                        isWasteOutput BiosphereExchange{} = False
                    any isWasteOutput (concatMap exchanges (M.elems (sdbActivities sdb'))) `shouldBe` True

    describe "non-finite amounts" $
        it "rejects an Infinity exchange amount rather than exporting it as 0.0" $ do
            let prodU = read "66666666-0000-4000-8000-000000000001" :: UUID
                bioU = read "66666666-0000-4000-8000-0000000000c0" :: UUID
                bioEx = BiosphereExchange bioU (1 / 0) kgUnit Emission "" Nothing Nothing
                bios = M.singleton bioU (BiosphereFlow bioU "Carbon dioxide" kgUnit M.empty Nothing Nothing Nothing)
                sdb = soloDb "leaky process" prodU [bioEx] M.empty bios M.empty
            checkEcoSpold1Exportable sdb `shouldSatisfy` isLeft

    describe "reference input (treatment process)" $
        it "rejects a ReferenceInput rather than flipping it to a reference product" $ do
            -- EcoSpold1 has no marker for a reference input; emitting outputGroup 0
            -- would re-parse it as a reference product (input → output flip).
            let prodU = read "77777777-0000-4000-8000-000000000001" :: UUID
                refInU = read "77777777-0000-4000-8000-0000000000a0" :: UUID
                refInEx = TechnosphereExchange refInU 1.0 kgUnit ReferenceInput UUID.nil Nothing "" Nothing Nothing
                techs = M.singleton refInU (TechnosphereFlow refInU "waste to treat" kgUnit M.empty Nothing Nothing)
                sdb = soloDb "treatment process" prodU [refInEx] techs M.empty M.empty
            checkEcoSpold1Exportable sdb `shouldSatisfy` isLeft

    describe "numeric round-trip" $
        it "round-trips a small-exponent amount without scientific-notation loss" $ do
            -- show 3.3e-20 emits scientific notation that re-reads lossily;
            -- showFFloatTrim keeps it value-identical across the round-trip.
            let prodU = read "88888888-0000-4000-8000-000000000001" :: UUID
                bioU = read "88888888-0000-4000-8000-0000000000c0" :: UUID
                bioEx = BiosphereExchange bioU 3.3e-20 kgUnit Emission "" Nothing Nothing
                bios = M.singleton bioU (BiosphereFlow bioU "dioxin" kgUnit M.empty Nothing Nothing Nothing)
                sdb = soloDb "trace emitter" prodU [bioEx] M.empty bios M.empty
            case roundTrip sdb of
                Left err -> expectationFailure ("round-trip failed: " ++ err)
                Right sdb' ->
                    concatMap (exchangeViews sdb') (M.elems (sdbActivities sdb'))
                        `shouldSatisfy` any ((== 3.3e-20) . evAmount)
