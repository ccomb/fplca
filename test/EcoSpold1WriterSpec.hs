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
import Data.List (sort)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID
import Test.Hspec

import qualified Database as DB
import EcoSpold.Cutoff (applyCutoffStrategy)
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
    let xml = TE.encodeUtf8 (writeSimpleDatabase canonicalWriterOptions sdb)
     in case parseAllWithXeno xml of
            Left err -> Left err
            Right results -> assembleSimpleDb <$> sequence results

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
            let xml = writeSimpleDatabase canonicalWriterOptions sdb
            T.isInfixOf "<ecoSpold" xml `shouldBe` True
            case parseAllWithXeno (TE.encodeUtf8 xml) of
                Left err -> expectationFailure ("re-parse failed: " ++ err)
                Right results -> length results `shouldBe` 1

        it "omits volatile attributes under canonicalWriterOptions" $ do
            sdb <- fixtureDb
            let xml = writeSimpleDatabase canonicalWriterOptions sdb
            T.isInfixOf "generator=" xml `shouldBe` False
            T.isInfixOf "timestamp=" xml `shouldBe` False

        it "includes a pinned generator under defaultWriterOptions" $ do
            sdb <- fixtureDb
            let xml = writeSimpleDatabase defaultWriterOptions sdb
            T.isInfixOf "generator=\"VoLCA\"" xml `shouldBe` True

    describe "round-trip (a) idempotence modulo volatile metadata" $
        it "write . parse . write == write (canonical)" $ do
            sdb <- fixtureDb
            let f0 = writeSimpleDatabase canonicalWriterOptions sdb
            case parseAllWithXeno (TE.encodeUtf8 f0) of
                Left err -> expectationFailure ("re-parse failed: " ++ err)
                Right results -> case sequence results of
                    Left err -> expectationFailure ("dataset failed: " ++ err)
                    Right datasets ->
                        let sdb' = assembleSimpleDb datasets
                            f1 = writeSimpleDatabase canonicalWriterOptions sdb'
                         in f1 `shouldBe` f0

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
