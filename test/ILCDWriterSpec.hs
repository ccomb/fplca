{-# LANGUAGE OverloadedStrings #-}

{- | Round-trip contract for "ILCD.Writer", the inverse of "ILCD.Parser".

The original database @D@ is obtained by parsing the @SAMPLE.ilcd@ fixture with
'parseILCDDirectory'. That guarantees @D@ already lives in the parser's
canonical internal form (resolved units, classified flows, linked exchanges),
so the write→parse cycle has a fair fixed point to land on.

Three properties are pinned, exactly as for the SimaPro/Brightway writers:

  (a) idempotence modulo volatile metadata — @write(D)@ then
      @write(parse(write(D)))@ produce byte-identical output. The only volatile
      fields (export timestamp, generator string) are /omitted/ by
      'defaultWriteOptions', so this holds without any normalization;

  (b) semantic round-trip — @parse(write(D))@ is structurally equal to @D@,
      order-insensitively, over activities, exchanges and the flow catalog;

  (c) score-equivalence — @parse(write(D))@ yields the same biosphere inventory
      (the LCIA-precursor vector; an LCIA score is linear in it) as @D@ within
      tolerance, via the engine's 'computeInventoryMatrix'.
-}
module ILCDWriterSpec (spec) where

import Data.List (isPrefixOf, sort)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Database (buildDatabaseWithMatrices)
import ILCD.Parser (parseILCDDirectory)
import ILCD.Writer (
    defaultWriteOptions,
    escapeXml,
    formatDouble,
    ilcdFiles,
    writeILCDDatabase,
 )
import Matrix (computeInventoryMatrix)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Types
import UnitConversion (defaultUnitConfig)

fixtureDir :: FilePath
fixtureDir = "test-data/SAMPLE.ilcd"

-- | Parse the fixture into a 'SimpleDatabase' (fails the test on Left).
loadFixture :: IO SimpleDatabase
loadFixture = do
    r <- parseILCDDirectory fixtureDir
    case r of
        Left err -> error ("fixture parse failed: " <> T.unpack err)
        Right db -> pure db

{- | Write a 'SimpleDatabase' to a fresh temp ILCD tree and parse it back.
A fresh directory per call avoids the parser's on-disk flow cache leaking
between round-trips.
-}
roundTrip :: SimpleDatabase -> IO SimpleDatabase
roundTrip db = withSystemTempDirectory "ilcd-writer-spec" $ \dir -> do
    writeILCDDatabase defaultWriteOptions dir db
    r <- parseILCDDirectory dir
    case r of
        Left err -> error ("round-trip parse failed: " <> T.unpack err)
        Right db' -> pure db'

-- ---------------------------------------------------------------------------
-- Structural comparison (order-insensitive)
-- ---------------------------------------------------------------------------

data ActivityShape = ActivityShape
    { asName :: Text
    , asLocation :: Text
    , asClass :: [(Text, Text)]
    , asNative :: Maybe Text
    , asExchanges :: [ExchangeShape]
    }
    deriving (Eq, Ord, Show)

data ExchangeShape = ExchangeShape
    { esKind :: Text
    , esFlow :: Text
    , esUnit :: Text
    , esAmount :: Double
    , esInput :: Bool
    , esRef :: Bool
    , esComment :: Maybe Text
    }
    deriving (Eq, Ord, Show)

exchangeShape :: SimpleDatabase -> Exchange -> ExchangeShape
exchangeShape db ex =
    let unitNm uid = maybe "" unitName (M.lookup uid (sdbUnits db))
        roundAmt v = fromIntegral (round (v * 1e9) :: Integer) / 1e9 :: Double
     in case ex of
            TechnosphereExchange{techFlowId = fid, techAmount = amt, techUnitId = uid, techComment = c} ->
                ExchangeShape
                    "tech"
                    (maybe "?" tfName (M.lookup fid (sdbTechFlows db)))
                    (unitNm uid)
                    (roundAmt amt)
                    (exchangeIsInput ex)
                    (exchangeIsReference ex)
                    c
            BiosphereExchange{bioFlowId = fid, bioAmount = amt, bioUnitId = uid, bioComment = c} ->
                ExchangeShape
                    "bio"
                    (maybe "?" bfName (M.lookup fid (sdbBioFlows db)))
                    (unitNm uid)
                    (roundAmt amt)
                    (exchangeIsInput ex)
                    (exchangeIsReference ex)
                    c
            WasteExchange{waFlowId = fid, waAmount = amt, waUnitId = uid, waComment = c} ->
                ExchangeShape
                    "waste"
                    (maybe "?" wfName (M.lookup fid (sdbWasteFlows db)))
                    (unitNm uid)
                    (roundAmt amt)
                    (exchangeIsInput ex)
                    (exchangeIsReference ex)
                    c

activityShape :: SimpleDatabase -> Activity -> ActivityShape
activityShape db a =
    ActivityShape
        { asName = activityName a
        , asLocation = activityLocation a
        , asClass = M.toAscList (activityClassification a)
        , asNative = nativeLabel (activityNativeType a)
        , asExchanges = sort (map (exchangeShape db) (exchanges a))
        }

nativeLabel :: Maybe NativeActivityType -> Maybe Text
nativeLabel nt = case nt of
    Just (ILCDProcessType l) -> Just l
    Just (SimaProProcessType l) -> Just l
    Just (EcoSpoldActivityType{}) -> Nothing
    Nothing -> Nothing

activityShapes :: SimpleDatabase -> S.Set ActivityShape
activityShapes db = S.fromList (map (activityShape db) (M.elems (sdbActivities db)))

-- | Order-insensitive view of the flow catalog: (kind, name, unit, cas).
data FlowShape = FlowShape Text Text Text (Maybe Text)
    deriving (Eq, Ord, Show)

flowShapes :: SimpleDatabase -> S.Set FlowShape
flowShapes db =
    S.fromList $
        [FlowShape "tech" (tfName f) (unitNm (tfUnitId f)) (tfCAS f) | f <- M.elems (sdbTechFlows db)]
            ++ [FlowShape "bio" (bfName f) (unitNm (bfUnitId f)) (bfCAS f) | f <- M.elems (sdbBioFlows db)]
            ++ [FlowShape "waste" (wfName f) (unitNm (wfUnitId f)) (wfCAS f) | f <- M.elems (sdbWasteFlows db)]
  where
    unitNm uid = maybe "" unitName (M.lookup uid (sdbUnits db))

-- ---------------------------------------------------------------------------
-- Inventory (score-equivalence) helper
-- ---------------------------------------------------------------------------

{- | Build a Database from a 'SimpleDatabase' and compute the inventory of the
named activity, keyed by biosphere flow name.
-}
inventoryByName :: SimpleDatabase -> Text -> IO (M.Map Text Double)
inventoryByName db target = do
    built <-
        buildDatabaseWithMatrices
            defaultUnitConfig
            (sdbActivities db)
            (sdbTechFlows db)
            (sdbBioFlows db)
            (sdbWasteFlows db)
            (sdbUnits db)
    case built of
        Left err -> expectationFailure (T.unpack err) >> pure M.empty
        Right d -> do
            let pids =
                    [ pid
                    | (pid, (actU, _)) <- zip [0 ..] (V.toList (dbProcessIdTable d))
                    , Just act <- [activityAtUUID d actU]
                    , activityName act == target
                    ]
            case pids of
                (pid : _) -> do
                    inv <- computeInventoryMatrix d pid
                    pure (M.mapKeys (flowName d) inv)
                [] -> expectationFailure ("no activity named " <> T.unpack target) >> pure M.empty
  where
    flowName d uid = maybe (T.pack (show uid)) bfName (M.lookup uid (dbBioFlows d))
    activityAtUUID d actU =
        case [i | (i, (a, _)) <- zip [0 ..] (V.toList (dbProcessIdTable d)), a == actU] of
            (i : _) -> Just (dbActivities d V.! i)
            [] -> Nothing

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "ILCD.Writer round-trip" $ do
    it "(pure) formatDouble round-trips integral and fractional values" $ do
        formatDouble 1.0 `shouldBe` "1"
        formatDouble 2.5 `shouldBe` "2.5"
        formatDouble 0.001 `shouldBe` "1.0e-3"
        formatDouble (-3.0) `shouldBe` "-3"

    it "(pure) escapeXml escapes the predefined entities" $ do
        escapeXml "a & b < c > d" `shouldBe` "a &amp; b &lt; c &gt; d"
        escapeXml "plain" `shouldBe` "plain"

    it "emits one process file per activity" $ do
        db <- loadFixture
        let procFiles = [p | (p, _) <- ilcdFiles defaultWriteOptions db, "processes/" `isPrefixOfFp` p]
        length procFiles `shouldBe` M.size (sdbActivities db)

    it "produces a parseable ILCD tree with the same activity count" $ do
        db <- loadFixture
        db' <- roundTrip db
        M.size (sdbActivities db') `shouldBe` M.size (sdbActivities db)

    it "(a) is idempotent modulo volatile metadata" $ do
        db <- loadFixture
        let f0 = ilcdFiles defaultWriteOptions db
        db' <- roundTrip db
        let f1 = ilcdFiles defaultWriteOptions db'
        f1 `shouldBe` f0

    it "(b) semantic round-trip: activities are structurally equal" $ do
        db <- loadFixture
        db' <- roundTrip db
        activityShapes db' `shouldBe` activityShapes db

    it "(b) semantic round-trip: the flow catalog is structurally equal" $ do
        db <- loadFixture
        db' <- roundTrip db
        flowShapes db' `shouldBe` flowShapes db

    it "(c) score-equivalence: same inventory for the sample activity within tolerance" $ do
        db <- loadFixture
        db' <- roundTrip db
        invOrig <- inventoryByName db "Coal extraction"
        invRound <- inventoryByName db' "Coal extraction"
        M.keysSet invRound `shouldBe` M.keysSet invOrig
        let diffs =
                [ abs (a - b)
                | (k, a) <- M.toList invOrig
                , let b = M.findWithDefault 0 k invRound
                ]
        all (< 1e-9) diffs `shouldBe` True

isPrefixOfFp :: String -> FilePath -> Bool
isPrefixOfFp = isPrefixOf
