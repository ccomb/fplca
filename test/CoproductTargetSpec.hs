{-# LANGUAGE OverloadedStrings #-}

{- | An allocated dataset is one activity written as one row per coproduct, all
sharing an activity UUID. Anything keyed on that UUID alone therefore answers
with an arbitrary one of them. These tests pin the places a consumer reads a
coproduct back.

The fixture is built so the wrong answer is visible: the input names the
coproduct with the /lower/ product UUID, which is exactly the row a
UUID-keyed map drops.
-}
module CoproductTargetSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.UUID as UUID
import Test.Hspec

import API.Types (ActivityForAPI (..), ActivitySummary (..), ExchangeDetail (..), ExchangeWithUnit (..))
import Database (buildDatabaseWithMatrices)
import qualified Service
import Types
import UnitConversion (defaultUnitConfig)

spec :: Spec
spec = do
    describe "the target of an exchange" $ do
        it "names the coproduct the input asks for, not another product of the same activity" $ do
            db <- twoCoproductFixture
            targetPidOfInput db `shouldBe` Just (pidText milkId)

        it "names it on the exchange-details path too" $ do
            db <- twoCoproductFixture
            targetSummaryOfInput db `shouldBe` Just (pidText milkId)

        it "reports a waste output whose treatment is not loaded as such" $ do
            db <- untreatedWasteFixture
            wasteLineOf db `shouldBe` (Nothing, Just TreatmentNotLoaded)

-- ---------------------------------------------------------------------------
-- Reading one exchange back
-- ---------------------------------------------------------------------------

-- | The target process id of the consumer's single technosphere input.
targetPidOfInput :: Database -> Maybe Text
targetPidOfInput db = do
    (pid, act) <- consumerRow db
    case [ewu | ewu <- pfaExchanges (Service.convertActivityForAPI defaultUnitConfig db pid act), exchangeIsInput (ewuExchange ewu)] of
        [ewu] -> ewuTargetProcessId ewu
        _ -> Nothing

-- | The same, read through 'Service.getActivityInputDetails'.
targetSummaryOfInput :: Database -> Maybe Text
targetSummaryOfInput db = do
    (_, act) <- consumerRow db
    case Service.getActivityInputDetails db act of
        [ed] -> prsProcessId <$> edTargetActivity ed
        _ -> Nothing

-- | Target and waste role of the consumer's single waste output.
wasteLineOf :: Database -> (Maybe Text, Maybe WasteRole)
wasteLineOf db = case consumerRow db of
    Nothing -> (Just "no consumer row", Nothing)
    Just (pid, act) ->
        case [ewu | ewu <- pfaExchanges (Service.convertActivityForAPI defaultUnitConfig db pid act), WasteExchange{waIsInput = False} <- [ewuExchange ewu]] of
            [ewu] -> (ewuTargetProcessId ewu, ewuWasteRole ewu)
            _ -> (Just "no waste output", Nothing)

consumerRow :: Database -> Maybe (ProcessId, Activity)
consumerRow db = do
    pid <- findProcessId db consumerActId consumerProdId
    act <- getActivity db pid
    pure (pid, act)

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

{- | A supplier written as two coproduct rows, and a consumer whose input names
the one with the lower product UUID.
-}
twoCoproductFixture :: IO Database
twoCoproductFixture = buildFixture (consumerActivity [milkInput] []) supplierRows supplierFlows

{- | A consumer whose waste output names a treatment the database does not hold
at that pair. The treatment activity is present, but under another flow, so an
answer built from the activity UUID alone would name it anyway.
-}
untreatedWasteFixture :: IO Database
untreatedWasteFixture =
    buildFixture
        (consumerActivity [] [wasteOutput])
        supplierRows
        supplierFlows

buildFixture :: Activity -> M.Map (UUID, UUID) Activity -> M.Map UUID TechnosphereFlow -> IO Database
buildFixture consumer rows flows = do
    r <-
        buildDatabaseWithMatrices
            defaultUnitConfig
            (M.insert (consumerActId, consumerProdId) consumer rows)
            (M.insert consumerProdId (techFlow consumerProdId "cheese") flows)
            M.empty
            (M.singleton scrapId (wasteFlow scrapId "scrap"))
            unitTable
    either (fail . show) pure r

supplierRows :: M.Map (UUID, UUID) Activity
supplierRows =
    M.fromList
        [ ((supplierActId, milkId), supplierActivity)
        , ((supplierActId, creamId), supplierActivity)
        ]

supplierFlows :: M.Map UUID TechnosphereFlow
supplierFlows =
    M.fromList
        [ (milkId, techFlow milkId "milk")
        , (creamId, techFlow creamId "cream")
        ]

-- | The supplier: one activity, two produced coproducts.
supplierActivity :: Activity
supplierActivity =
    bareActivity
        "milk production"
        [ techExchange milkId 1.0 ReferenceProduct supplierActId
        , techExchange creamId 0.3 Coproduct supplierActId
        ]

consumerActivity :: [Exchange] -> [Exchange] -> Activity
consumerActivity inputs outputs =
    bareActivity
        "cheese production"
        ([techExchange consumerProdId 1.0 ReferenceProduct consumerActId] ++ inputs ++ outputs)

-- | An input naming the supplier's lower-UUID coproduct.
milkInput :: Exchange
milkInput = techExchange milkId 2.0 Input supplierActId

{- | A waste output naming the supplier: the activity exists, no row of it
produces the waste flow, so nothing routes it.
-}
wasteOutput :: Exchange
wasteOutput =
    WasteExchange
        { waFlowId = scrapId
        , waAmount = 0.5
        , waUnitId = kgUnitId
        , waIsInput = False
        , waActivityLinkId = supplierActId
        , waProcessLinkId = Nothing
        , waLocation = ""
        , waComment = Nothing
        , waPedigree = Nothing
        }

techExchange :: UUID -> Double -> TechRole -> UUID -> Exchange
techExchange flowId amount role link =
    TechnosphereExchange
        { techFlowId = flowId
        , techAmount = amount
        , techUnitId = kgUnitId
        , techRole = role
        , techActivityLinkId = link
        , techProcessLinkId = Nothing
        , techLocation = ""
        , techComment = Nothing
        , techPedigree = Nothing
        }

bareActivity :: Text -> [Exchange] -> Activity
bareActivity name exs =
    Activity
        { activityName = name
        , activityDescription = []
        , activityDocumentation = []
        , activitySynonyms = M.empty
        , activityClassification = M.empty
        , activityLocation = "FR"
        , activityLocationSource = LocationDeclared
        , activityUnit = "kg"
        , exchanges = exs
        , activityParams = M.empty
        , activityParamExprs = M.empty
        , activityAllocationPercent = Nothing
        , activityAllocationFormula = Nothing
        , activityNativeType = Nothing
        , activityNativeId = Nothing
        , activityFormulaCheck = Nothing
        }

techFlow :: UUID -> Text -> TechnosphereFlow
techFlow fid name =
    TechnosphereFlow
        { tfId = fid
        , tfName = name
        , tfUnitId = kgUnitId
        , tfSynonyms = M.empty
        , tfCAS = Nothing
        , tfSubstanceId = Nothing
        }

wasteFlow :: UUID -> Text -> WasteFlow
wasteFlow fid name =
    WasteFlow
        { wfId = fid
        , wfName = name
        , wfUnitId = kgUnitId
        , wfSynonyms = M.empty
        , wfCAS = Nothing
        , wfSubstanceId = Nothing
        }

unitTable :: M.Map UUID Unit
unitTable = M.singleton kgUnitId Unit{unitId = kgUnitId, unitName = "kg", unitSymbol = "kg", unitComment = ""}

mkUUID :: Int -> UUID
mkUUID n = UUID.fromWords64 (fromIntegral n) 0

{- | The supplier's two products are ordered on purpose: 'milkId' is the one a
map keyed on the activity UUID alone drops.
-}
supplierActId, milkId, creamId, consumerActId, consumerProdId, scrapId, kgUnitId :: UUID
supplierActId = mkUUID 1
milkId = mkUUID 2
creamId = mkUUID 9
consumerActId = mkUUID 3
consumerProdId = mkUUID 4
scrapId = mkUUID 5
kgUnitId = mkUUID 10

-- | Process id text of a supplier coproduct row.
pidText :: UUID -> Text
pidText prodId = UUID.toText supplierActId <> "_" <> UUID.toText prodId
