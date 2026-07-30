{-# LANGUAGE OverloadedStrings #-}

{- | Tests for authoring activities into an editable database
('Database.Author.validateAuthored', 'Database.Edit.insertActivities' and
'Database.Edit.replaceActivities').

Authoring is the strict counterpart of importing. Where the loader warns and
drops a row it cannot resolve, authoring refuses — so most of what follows is
one red case per refusal, plus the two properties that make repeated authoring
safe: identity is a function of what was written (author twice, get the same
key), and writing then deleting leaves the activity set it started from.
-}
module AuthorSpec (spec) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text, isInfixOf)
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import Test.Hspec

import Database (buildDatabaseWithMatrices)
import Database.Author (
    AuthorContext (..),
    AuthoredActivity (..),
    AuthoredExchange (..),
    FlowRef (..),
    ResolvedInsert (..),
    authoredActivityUUID,
    authoredProductUUID,
    validateAuthored,
 )
import Database.Edit (deleteActivities, insertActivities, replaceActivities)
import Types (
    Activity (..),
    BioDirection (..),
    BiosphereFlow (..),
    Compartment (..),
    Database (..),
    Exchange (..),
    LocationSource (..),
    TechRole (..),
    TechnosphereFlow (..),
    UUID,
    Unit (..),
    exchangeAmount,
    findProcessId,
    isTechnosphereExchange,
 )
import UnitConversion (defaultUnitConfig)

spec :: Spec
spec = do
    fixtureDb <- runIO buildFixture
    let failsWith = refusalOf fixtureDb
    describe "Database.Author" $ do
        describe "validateAuthored (refusals)" $ do
            it "refuses an empty activity name" $
                failsWith "the activity name is empty" (baseActivity{aaName = "  "})

            it "refuses an empty product name" $
                failsWith "the product name is empty" (baseActivity{aaProductName = ""})

            it "refuses a zero product amount" $
                -- Zero output would divide the whole column by a zero normalization
                -- factor; it is a line that should not have been written.
                failsWith "finite non-zero" (baseActivity{aaProductAmount = 0})

            it "refuses a non-finite product amount" $
                failsWith "finite non-zero" (baseActivity{aaProductAmount = 0 / 0})

            it "refuses a product unit the database does not know" $
                failsWith "unknown unit \"furlong\"" (baseActivity{aaProductUnit = "furlong"})

            it "refuses a supplier that resolves to nothing" $
                -- The loader warns and drops this case; authoring will not, because
                -- a dropped input silently undercounts the activity's inventory.
                failsWith
                    "unknown provider"
                    (baseActivity{aaExchanges = [techInput "no-such-process" 1 Nothing]})

            it "refuses an input whose unit cannot reach the supplier's" $
                failsWith
                    "cannot convert \"m\" into the supplier's \"kg\""
                    (baseActivity{aaExchanges = [techInput supplierPid 1 (Just "m")]})

            it "refuses a non-finite exchange amount" $
                failsWith
                    "finite non-zero"
                    (baseActivity{aaExchanges = [techInput supplierPid (1 / 0) Nothing]})

            it "refuses a biosphere flow identifier nothing declares" $
                failsWith
                    "no biosphere flow"
                    (baseActivity{aaExchanges = [bioOf (ExistingFlow (mkUUID 999)) 1 Nothing]})

            it "refuses a biosphere amount restated in another unit" $
                -- The biosphere matrix carries amounts through unconverted, so a
                -- unit the flow does not use would land as a wrong number.
                failsWith
                    "biosphere amounts are not converted"
                    (baseActivity{aaExchanges = [bioOf (ExistingFlow co2Id) 1 (Just "m")]})

            it "reports every defect of a batch at once, each naming its activity" $ do
                let bad =
                        baseActivity
                            { aaName = "two-problems"
                            , aaExchanges = [techInput "nope" 1 Nothing, bioOf (ExistingFlow (mkUUID 998)) 1 Nothing]
                            }
                case validateAuthored (contextOf fixtureDb) [bad, baseActivity{aaProductUnit = "furlong"}] of
                    Right _ -> expectationFailure "expected the batch to be refused"
                    Left errs -> do
                        length errs `shouldBe` 3
                        errs `shouldSatisfy` any (isInfixOf "two-problems {FR}: exchange 1: unknown provider")
                        errs `shouldSatisfy` any (isInfixOf "exchange 2: no biosphere flow")
                        errs `shouldSatisfy` any (isInfixOf "unknown unit \"furlong\"")

            it "refuses a batch that mints one identity twice" $
                case validateAuthored (contextOf fixtureDb) [baseActivity, baseActivity] of
                    Right _ -> expectationFailure "expected the duplicate identity to be refused"
                    Left errs -> errs `shouldSatisfy` any (isInfixOf "mint the same identity")

        describe "validateAuthored (resolution)" $ do
            it "mints the same identity for the same description, twice" $ do
                a <- resolveOrFail fixtureDb baseActivity
                b <- resolveOrFail fixtureDb baseActivity
                riKey a `shouldBe` riKey b
                riKey a
                    `shouldBe` ( authoredActivityUUID "cheese, at dairy" "FR"
                               , authoredProductUUID "cheese" "kg"
                               )

            it "gives the same product two units two identities" $ do
                a <- resolveOrFail fixtureDb baseActivity
                b <- resolveOrFail fixtureDb baseActivity{aaProductUnit = "item"}
                snd (riKey a) `shouldNotBe` snd (riKey b)

            it "resolves a local supplier to a process link" $ do
                r <- resolveOrFail fixtureDb baseActivity{aaExchanges = [techInput supplierPid 2 Nothing]}
                case [ex | ex <- exchanges (riActivity r), isTechnosphereExchange ex, exchangeAmount ex == 2] of
                    [TechnosphereExchange{techActivityLinkId = link, techProcessLinkId = pid}] -> do
                        link `shouldBe` supplierActId
                        pid `shouldBe` findProcessId fixtureDb supplierActId supplierProdId
                    other -> expectationFailure ("expected one resolved input, got " <> show (length other))

            it "leaves a dependency's supplier for cross-database relinking" $ do
                -- A supplier in another database has no process id here; the link
                -- carries the activity UUID and waits for the relink.
                depDb <- buildFixture
                let ctx = (contextOf fixtureDb){acDeps = [("other", depDb)]}
                    authored = baseActivity{aaName = "from-dep", aaExchanges = [techInput supplierPid 3 Nothing]}
                case validateAuthored ctx{acDb = emptyOf fixtureDb} [authored] of
                    Left errs -> expectationFailure ("expected resolution, got " <> show errs)
                    Right ([r], _) ->
                        case [ex | ex <- exchanges (riActivity r), exchangeAmount ex == 3] of
                            [TechnosphereExchange{techActivityLinkId = link, techProcessLinkId = pid}] -> do
                                link `shouldBe` supplierActId
                                pid `shouldBe` Nothing
                            other -> expectationFailure ("expected one dependency input, got " <> show (length other))
                    Right (rs, _) -> expectationFailure ("expected one insert, got " <> show (length rs))

            it "warns about a biosphere flow new to the database without refusing it" $ do
                let authored = baseActivity{aaExchanges = [bioOf (NewBioFlow "Nitrous oxide" air "kg") 0.5 Nothing]}
                case validateAuthored (contextOf fixtureDb) [authored] of
                    Left errs -> expectationFailure ("expected acceptance, got " <> show errs)
                    Right ([r], warnings) -> do
                        map bfName (riNewBioFlows r) `shouldBe` ["Nitrous oxide"]
                        warnings `shouldSatisfy` any (isInfixOf "no characterization factor matches it")
                    Right (rs, _) -> expectationFailure ("expected one insert, got " <> show (length rs))

            it "reuses a biosphere flow the database already declares" $ do
                -- Same three coordinates the flow was minted on: re-declaring it is
                -- not a second flow.
                r <- resolveOrFail fixtureDb baseActivity{aaExchanges = [bioOf (ExistingFlow co2Id) 1 Nothing]}
                map bfName (riNewBioFlows r) `shouldBe` []

        describe "insertActivities" $ do
            it "adds the activity, its product flow and its new biosphere flow together" $ do
                let authored =
                        baseActivity
                            { aaExchanges =
                                [ techInput supplierPid 2 Nothing
                                , bioOf (NewBioFlow "Nitrous oxide" air "kg") 0.5 Nothing
                                ]
                            }
                r <- resolveOrFail fixtureDb authored
                case insertActivities defaultUnitConfig [r] fixtureDb of
                    Left err -> expectationFailure ("insertActivities: " <> show err)
                    Right db' -> do
                        dbActivityCount db' `shouldBe` dbActivityCount fixtureDb + 1
                        M.member (snd (riKey r)) (dbTechFlows db') `shouldBe` True
                        map bfName (M.elems (dbBioFlows db')) `shouldSatisfy` elem "Nitrous oxide"
                        uncurry (findProcessId db') (riKey r) `shouldSatisfy` (/= Nothing)

            it "refuses a key the database already holds" $ do
                r <- resolveOrFail fixtureDb baseActivity
                case insertActivities defaultUnitConfig [r] fixtureDb >>= insertActivities defaultUnitConfig [r] of
                    Left err -> err `shouldSatisfy` isInfixOf "already exists"
                    Right _ -> expectationFailure "expected the second insert to be refused"

            it "leaves the database untouched on an empty batch" $
                -- A rebuild would clear cross-database links; "nothing to write"
                -- must not silently unlink the database.
                case insertActivities defaultUnitConfig [] fixtureDb{dbDependsOn = ["other"]} of
                    Left err -> expectationFailure ("expected a no-op, got " <> show err)
                    Right db' -> dbDependsOn db' `shouldBe` ["other"]

            it "restores the original activity set when the insert is deleted again" $ do
                r <- resolveOrFail fixtureDb baseActivity
                case insertActivities defaultUnitConfig [r] fixtureDb of
                    Left err -> expectationFailure ("insertActivities: " <> show err)
                    Right db' -> case uncurry (findProcessId db') (riKey r) of
                        Nothing -> expectationFailure "the inserted activity has no process id"
                        Just pid -> case deleteActivities [pid] db' of
                            Left err -> expectationFailure ("deleteActivities: " <> show err)
                            Right db'' -> do
                                processKeys db'' `shouldBe` processKeys fixtureDb
                                dbActivityCount db'' `shouldBe` dbActivityCount fixtureDb

        describe "replaceActivities" $ do
            it "refuses a key the database does not hold" $ do
                r <- resolveOrFail fixtureDb baseActivity
                case replaceActivities defaultUnitConfig [r] fixtureDb of
                    Left err -> err `shouldSatisfy` isInfixOf "does not exist"
                    Right _ -> expectationFailure "expected the replace to be refused"

            it "rewrites the activity in place, keeping its identity" $ do
                first <- resolveOrFail fixtureDb baseActivity{aaExchanges = [techInput supplierPid 2 Nothing]}
                case insertActivities defaultUnitConfig [first] fixtureDb of
                    Left err -> expectationFailure ("insertActivities: " <> show err)
                    Right db' -> do
                        second <- resolveOrFail db' baseActivity{aaExchanges = [techInput supplierPid 7 Nothing]}
                        riKey second `shouldBe` riKey first
                        case replaceActivities defaultUnitConfig [second] db' of
                            Left err -> expectationFailure ("replaceActivities: " <> show err)
                            Right db'' -> do
                                dbActivityCount db'' `shouldBe` dbActivityCount db'
                                inputAmounts db'' (fst (riKey second)) `shouldBe` [7]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | "Authoring this against that database is refused, and says why."
refusalOf :: Database -> Text -> AuthoredActivity -> Expectation
refusalOf db needle authored =
    case validateAuthored (contextOf db) [authored] of
        Right _ -> expectationFailure ("expected a refusal mentioning " <> show needle)
        Left errs -> errs `shouldSatisfy` any (isInfixOf needle)

resolveOrFail :: Database -> AuthoredActivity -> IO ResolvedInsert
resolveOrFail db authored = case validateAuthored (contextOf db) [authored] of
    Left errs -> fail ("validateAuthored: " <> show errs)
    Right ([r], _) -> pure r
    Right (rs, _) -> fail ("expected one insert, got " <> show (length rs))

contextOf :: Database -> AuthorContext
contextOf db = AuthorContext{acDb = db, acDeps = [], acUnitConfig = defaultUnitConfig}

-- | The same database with no activities — used to prove a dependency link resolves.
emptyOf :: Database -> Database
emptyOf db = db{dbActivities = V.empty, dbProcessIdTable = V.empty, dbProcessIdLookup = M.empty}

processKeys :: Database -> S.Set (UUID, UUID)
processKeys = S.fromList . V.toList . dbProcessIdTable

-- | Amounts of the non-reference technosphere inputs of one activity.
inputAmounts :: Database -> UUID -> [Double]
inputAmounts db actUUID =
    [ exchangeAmount ex
    | (key, act) <- zip (V.toList (dbProcessIdTable db)) (V.toList (dbActivities db))
    , fst key == actUUID
    , ex@TechnosphereExchange{techRole = Input} <- exchanges act
    ]

baseActivity :: AuthoredActivity
baseActivity =
    AuthoredActivity
        { aaName = "cheese, at dairy"
        , aaLocation = "FR"
        , aaDescription = ["An authored activity."]
        , aaProductName = "cheese"
        , aaProductAmount = 1.0
        , aaProductUnit = "kg"
        , aaExchanges = []
        }

techInput :: Text -> Double -> Maybe Text -> AuthoredExchange
techInput provider amount unit =
    AuthoredTechInput{atiProvider = provider, atiAmount = amount, atiUnit = unit, atiComment = Nothing}

bioOf :: FlowRef -> Double -> Maybe Text -> AuthoredExchange
bioOf flow amount unit =
    AuthoredBio
        { abFlow = flow
        , abDirection = Emission
        , abAmount = amount
        , abUnit = unit
        , abComment = Nothing
        }

air :: Compartment
air = Compartment{compartmentName = "air", compartmentSub = Nothing}

-- ---------------------------------------------------------------------------
-- Fixture: one supplier producing "milk" in kg, emitting CO2
-- ---------------------------------------------------------------------------

buildFixture :: IO Database
buildFixture = do
    r <-
        buildDatabaseWithMatrices
            defaultUnitConfig
            (M.singleton (supplierActId, supplierProdId) supplierActivity)
            (M.singleton supplierProdId milkFlow)
            (M.singleton co2Id co2Flow)
            M.empty
            unitTable
    either (fail . show) pure r

mkUUID :: Int -> UUID
mkUUID n = UUID.fromWords64 (fromIntegral n) 0

supplierActId, supplierProdId, co2Id, kgUnitId, itemUnitId, metreUnitId :: UUID
supplierActId = mkUUID 1
supplierProdId = mkUUID 2
co2Id = mkUUID 3
kgUnitId = mkUUID 10
itemUnitId = mkUUID 11
metreUnitId = mkUUID 12

supplierPid :: Text
supplierPid = UUID.toText supplierActId <> "_" <> UUID.toText supplierProdId

unitTable :: M.Map UUID Unit
unitTable =
    M.fromList
        [ (kgUnitId, Unit{unitId = kgUnitId, unitName = "kg", unitSymbol = "kg", unitComment = ""})
        , (itemUnitId, Unit{unitId = itemUnitId, unitName = "item", unitSymbol = "item", unitComment = ""})
        , (metreUnitId, Unit{unitId = metreUnitId, unitName = "m", unitSymbol = "m", unitComment = ""})
        ]

milkFlow :: TechnosphereFlow
milkFlow =
    TechnosphereFlow
        { tfId = supplierProdId
        , tfName = "milk"
        , tfUnitId = kgUnitId
        , tfSynonyms = M.empty
        , tfCAS = Nothing
        , tfSubstanceId = Nothing
        }

co2Flow :: BiosphereFlow
co2Flow =
    BiosphereFlow
        { bfId = co2Id
        , bfName = "Carbon dioxide"
        , bfUnitId = kgUnitId
        , bfSynonyms = M.empty
        , bfCAS = Just "124-38-9"
        , bfSubstanceId = Nothing
        , bfCompartment = Just air
        }

supplierActivity :: Activity
supplierActivity =
    Activity
        { activityName = "milk production"
        , activityDescription = []
        , activitySynonyms = M.empty
        , activityClassification = M.empty
        , activityLocation = "FR"
        , activityLocationSource = LocationDeclared
        , activityUnit = "kg"
        , exchanges =
            [ TechnosphereExchange
                { techFlowId = supplierProdId
                , techAmount = 1.0
                , techUnitId = kgUnitId
                , techRole = ReferenceProduct
                , techActivityLinkId = supplierActId
                , techProcessLinkId = Nothing
                , techLocation = ""
                , techComment = Nothing
                , techPedigree = Nothing
                }
            , BiosphereExchange
                { bioFlowId = co2Id
                , bioAmount = 1.2
                , bioUnitId = kgUnitId
                , bioDirection = Emission
                , bioLocation = ""
                , bioComment = Nothing
                , bioPedigree = Nothing
                }
            ]
        , activityParams = M.empty
        , activityParamExprs = M.empty
        , activityAllocationPercent = Nothing
        , activityAllocationFormula = Nothing
        , activityNativeType = Nothing
        , activityNativeId = Nothing
        , activityFormulaCheck = Nothing
        }
