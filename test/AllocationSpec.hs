{-# LANGUAGE OverloadedStrings #-}

{- | The allocation gate: how an activity as its source wrote it becomes the
single-output processes the matrix holds, and what happens to one that
cannot.
-}
module AllocationSpec (spec) where

import Control.Monad ((<=<))
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Database (buildDatabaseWithMatrices)
import Database.Allocation
import Database.Loader (loadDatabaseWithLocationAliases)
import Database.MatrixBuild (InterningTables (..), buildInterningTables, buildSupplierRefUnits, buildTechTriples)
import Database.Quality (QualityCheck (..), QualityOffender (..), QualityReport (..), qualityReport)
import qualified Service
import Types
import UnitConversion (UnitConfig, UnitDef (..), defaultUnitConfig, mkUnitConfig, ucDimensionOrder, ucOriginalKeys, ucUnits)

-- | Amounts of the five products of the Abondance cheese block, in kilograms.
abondance :: NE.NonEmpty (Text, Double)
abondance = NE.fromList [("kg", q) | q <- [1.0, 5.58318, 1.12527, 0.775791, 0.0686462]]

-- | 'defaultUnitConfig' plus a gram and a megajoule, to have a second mass and a non-mass.
massUnits :: UnitConfig
massUnits =
    mkUnitConfig
        (ucDimensionOrder defaultUnitConfig)
        (M.union (M.fromList [("g", UnitDef mass 0.001), ("mj", UnitDef energy 1.0)]) (ucUnits defaultUnitConfig))
        (M.union (M.fromList [("g", "g"), ("mj", "MJ")]) (ucOriginalKeys defaultUnitConfig))
  where
    mass, energy :: [Int]
    mass = [1, 0, 0, 0, 0, 0, 0, 0]
    energy = [0, 0, 0, 1, 0, 0, 0, 0]

round1 :: Double -> Double
round1 x = fromIntegral (round (x * 10) :: Int) / 10

spec :: Spec
spec = do
    describe "allocate Declared" $ do
        it "splits a block into one process per product, scaled by each declared share" $ do
            let processes = NE.toList (allocate Declared units block)
            length processes `shouldBe` 3
            -- Each process keeps its own product as the reference, and only it.
            [exchangeFlowId ex | p <- processes, ex <- exchanges p, exchangeIsReference ex] `shouldBe` [cheeseId, wheyId, creamId]
            [length (filter exchangeIsProductOutput (exchanges p)) | p <- processes] `shouldBe` [1, 1, 1]
            -- The shared exchanges follow, scaled by share / 100, in source order.
            [inputAmounts p | p <- processes] `shouldBe` [[5.0], [3.0], [2.0]]
            [bioAmounts p | p <- processes] `shouldBe` [[2.0], [1.2], [0.8]]
            [avoidedAmounts p | p <- processes] `shouldBe` [[0.5], [0.3], [0.2]]

        it "keeps the declared share on each process's reference, for the writer and the wire" $ do
            let processes = NE.toList (allocate Declared units block)
            map (fmap dsPercent . activityReferenceShare) processes `shouldBe` [Just 50, Just 30, Just 20]
            map (dsFormula <=< activityReferenceShare) processes `shouldBe` [Nothing, Just "Qw*DMw/total*100", Nothing]

        it "names each process's unit after its product, and its category after the product row" $ do
            let processes = NE.toList (allocate Declared units block)
            map activityUnit processes `shouldBe` ["kg", "MJ", "kg"]
            map (M.lookup "Category" . activityClassification) processes
                `shouldBe` [Just "Food\\Transformation", Just "Animal feed\\Others", Just "Food\\Transformation"]
            -- What the block said beyond the category survives on every process.
            map (M.lookup "Category type" . activityClassification) processes `shouldBe` replicate 3 (Just "material")

        it "applies a single product's share as declared, 0 % included" $ do
            let zero = activity [productRow cheeseId 1.0 ReferenceProduct (Just 0) M.empty, input 10.0]
                half = activity [productRow cheeseId 1.0 ReferenceProduct (Just 51) M.empty, input 10.0]
            concatMap inputAmounts (allocate Declared units zero) `shouldBe` [0.0]
            concatMap inputAmounts (allocate Declared units half) `shouldBe` [5.1]

        it "leaves a single product with no share as it is" $ do
            let plain = activity [productRow cheeseId 1.0 ReferenceProduct Nothing M.empty, input 10.0]
            map shape (NE.toList (allocate Declared units plain)) `shouldBe` [shape plain]

        it "leaves an activity whole when a product output carries no share" $ do
            let noShares = activity [productRow cheeseId 1.0 ReferenceProduct Nothing M.empty, productRow wheyId 2.0 Coproduct Nothing M.empty, input 10.0]
                oneShare = activity [productRow cheeseId 1.0 ReferenceProduct (Just 60) M.empty, productRow wheyId 2.0 Coproduct Nothing M.empty, input 10.0]
            map shape (NE.toList (allocate Declared units noShares)) `shouldBe` [shape noShares]
            map shape (NE.toList (allocate Declared units oneShare)) `shouldBe` [shape oneShare]

        it "lets an avoided product through unsplit, scaled with the other shared lines" $ do
            let substituting = activity [productRow cheeseId 1.0 ReferenceProduct (Just 50) M.empty, avoided 2.0]
            concatMap avoidedAmounts (allocate Declared units substituting) `shouldBe` [1.0]

    describe "allocate normalises first, as the EcoSpold parsers used to" $ do
        it "drops a zero-amount coproduct the source states no share for" $ do
            let act = activity [productRow cheeseId 1.0 ReferenceProduct Nothing M.empty, productRow wheyId 0.0 Coproduct Nothing M.empty]
            length (concatMap exchanges (allocate Declared units act)) `shouldBe` 1

        it "keeps a zero-amount product row that declares a share: it is a process of its own" $ do
            let act = activity [productRow cheeseId 1.0 ReferenceProduct (Just 100) M.empty, productRow wheyId 0.0 Coproduct (Just 0) M.empty]
            length (allocate Declared units act) `shouldBe` 2

        it "promotes the only non-zero output of an activity with no reference" $ do
            let act = activity [productRow wheyId 2.0 Coproduct Nothing M.empty, productRow creamId 0.0 Coproduct Nothing M.empty]
                result = NE.toList (allocate Declared units act)
            [exchangeFlowId ex | p <- result, ex <- exchanges p, exchangeIsReference ex] `shouldBe` [wheyId]

        it "does not choose between two non-zero outputs" $ do
            let act = activity [productRow wheyId 2.0 Coproduct Nothing M.empty, productRow creamId 1.0 Coproduct Nothing M.empty]
            any exchangeIsReference (concatMap exchanges (allocate Declared units act)) `shouldBe` False

    describe "asAllocated" $ do
        it "accepts a split process and a single-output activity alike" $ do
            let processes = NE.toList (allocate Declared units block)
            map (either (const False) (const True) . asAllocated) processes `shouldBe` [True, True, True]
            either (const False) (const True) (asAllocated (activity [productRow cheeseId 1.0 ReferenceProduct Nothing M.empty])) `shouldBe` True

        it "accepts an activity that displaces a product" $
            either (const False) (const True) (asAllocated (activity [productRow cheeseId 1.0 ReferenceProduct Nothing M.empty, avoided 2.0]))
                `shouldBe` True

        it "refuses an activity still carrying a coproduct, counting the outputs without a share" $ do
            refusalOf (activity [productRow cheeseId 1.0 ReferenceProduct Nothing M.empty, productRow wheyId 2.0 Coproduct Nothing M.empty])
                `shouldBe` Just (UnallocatedOutputs 2 2)
            refusalOf (activity [productRow cheeseId 1.0 ReferenceProduct (Just 60) M.empty, productRow wheyId 2.0 Coproduct Nothing M.empty])
                `shouldBe` Just (UnallocatedOutputs 2 1)

        it "refuses an activity with no reference, or with two" $ do
            refusalOf (activity [input 1.0]) `shouldBe` Just (NoSingleReference 0)
            refusalOf (activity [productRow cheeseId 1.0 ReferenceProduct Nothing M.empty, productRow wheyId 1.0 ReferenceProduct Nothing M.empty])
                `shouldBe` Just (NoSingleReference 2)

        it "says what is missing and what would repair it" $ do
            describeRefusal (UnallocatedOutputs 3 3) `shouldSatisfy` T.isInfixOf "3 product outputs, 3 without a declared share"
            describeRefusal (UnallocatedOutputs 3 3) `shouldSatisfy` T.isInfixOf "state a share on every product row"
            describeRefusal (NoSingleReference 0) `shouldSatisfy` T.isInfixOf "no reference exchange"
            describeRefusal (NoSingleReference 2) `shouldSatisfy` T.isInfixOf "2 reference exchanges"

    describe "massShares" $ do
        it "reads the Abondance block the way the mass would, against its declared key" $
            -- Quantities of AGRIBALU000000003100165, whose declared key is dry matter:
            -- cheese 51.4 %, permeate 24.3 %, concentrated whey 17.6 %, whey 4.4 %, cream 2.3 %.
            fmap (map round1 . NE.toList) (massShares massUnits abondance)
                `shouldBe` Right [11.7, 65.3, 13.2, 9.1, 0.8]

        it "converts before summing, so a half-kilo is a third and not almost everything" $
            fmap (map round1 . NE.toList) (massShares massUnits (NE.fromList [("kg", 1.0), ("g", 500.0)]))
                `shouldBe` Right [66.7, 33.3]

        it "refuses a block whose product is not stated in a mass" $
            massShares massUnits (NE.fromList [("kg", 1.0), ("MJ", 4.0)])
                `shouldBe` Left (NotAMass "MJ")

        it "refuses an amount no share can be read from, rather than dropping it to zero" $ do
            massShares massUnits (NE.fromList [("kg", 1.0), ("kg", 0.0)]) `shouldBe` Left (NonPositiveMass 0.0)
            massShares massUnits (NE.fromList [("kg", -1.0)]) `shouldBe` Left (NonPositiveMass (-1.0))

    describe "the matrix" $ do
        it "gives a refused activity no column, and says why" $ do
            let refused = activity [productRow cheeseId 1.0 ReferenceProduct Nothing M.empty, productRow wheyId 2.0 Coproduct Nothing M.empty, input 10.0]
                consumer = (activity [productRow creamId 1.0 ReferenceProduct Nothing M.empty, (tech cheeseId 3.0 Input){techActivityLinkId = refusedActId}]){activityName = "consumer"}
                tables = buildInterningTables (M.fromList [((refusedActId, cheeseId), refused), ((consumerActId, creamId), consumer)])
                refUnits = buildSupplierRefUnits units (itActivities tables)
            case buildTechTriples defaultUnitConfig units tables refUnits of
                Left err -> expectationFailure (T.unpack err)
                Right (triples, warnings) -> do
                    -- Column 0 is the refused activity: nothing in it. Column 1 is
                    -- the consumer, whose input reaches the refused row (0) as usual.
                    [(i, j) | SparseTriple i j _ <- VU.toList triples] `shouldBe` [(0, 1)]
                    length warnings `shouldBe` 1
                    concat warnings `shouldSatisfy` isInfixOfS "is not allocated: 2 product outputs, 2 without a declared share"
            V.length (itActivities tables) `shouldBe` 2

    endToEnd

-- ---------------------------------------------------------------------------
-- Fixture: a cheese block with three products and one of each shared line.
-- ---------------------------------------------------------------------------

block :: Activity
block =
    ( activity
        [ productRow cheeseId 1.0 ReferenceProduct (Just 50) M.empty
        , productRow wheyId 2.0 Coproduct (Just 30) (M.singleton "Category" "Animal feed\\Others")
        , productRow creamId 3.0 Coproduct (Just 20) M.empty
        , input 10.0
        , bio 4.0
        , avoided 1.0
        ]
    )
        { activityClassification = M.fromList [("Category type", "material"), ("Category", "Food\\Transformation")]
        }

productRow :: UUID -> Double -> TechRole -> Maybe Double -> M.Map Text Text -> Exchange
productRow fid amount role share cls =
    (tech fid amount role)
        { techUnitId = if fid == wheyId then mjId else kgId
        , techShare = (\p -> DeclaredShare p (if fid == wheyId then Just "Qw*DMw/total*100" else Nothing)) <$> share
        , techClassification = cls
        }

input :: Double -> Exchange
input amount = tech feedId amount Input

avoided :: Double -> Exchange
avoided amount = tech avoidedId amount AvoidedProduct

tech :: UUID -> Double -> TechRole -> Exchange
tech fid amount role =
    TechnosphereExchange
        { techFlowId = fid
        , techAmount = amount
        , techUnitId = kgId
        , techRole = role
        , techActivityLinkId = UUID.nil
        , techProcessLinkId = Nothing
        , techLocation = ""
        , techComment = Nothing
        , techPedigree = Nothing
        , techShare = Nothing
        , techClassification = M.empty
        }

bio :: Double -> Exchange
bio amount =
    BiosphereExchange
        { bioFlowId = co2Id
        , bioAmount = amount
        , bioUnitId = kgId
        , bioDirection = Emission
        , bioLocation = ""
        , bioComment = Nothing
        , bioPedigree = Nothing
        }

activity :: [Exchange] -> Activity
activity exs =
    Activity
        { activityName = "cheese production"
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
        , activityNativeType = Nothing
        , activityNativeId = Just (NativeProcessId "block-1")
        , activityFormulaCheck = Nothing
        }

units :: UnitDB
units = M.fromList [(kgId, Unit kgId "kg" "kg" ""), (mjId, Unit mjId "MJ" "MJ" "")]

inputAmounts, bioAmounts, avoidedAmounts :: Activity -> [Double]
inputAmounts act = [techAmount ex | ex@TechnosphereExchange{techRole = Input} <- exchanges act]
bioAmounts act = [bioAmount ex | ex@BiosphereExchange{} <- exchanges act]
avoidedAmounts act = [techAmount ex | ex@TechnosphereExchange{techRole = AvoidedProduct} <- exchanges act]

-- | What a test compares two activities on: their exchanges' flows, roles and amounts.
shape :: Activity -> [(UUID, TechRole, Double)]
shape act = [(techFlowId ex, techRole ex, techAmount ex) | ex@TechnosphereExchange{} <- exchanges act]

refusalOf :: Activity -> Maybe AllocationRefusal
refusalOf = either Just (const Nothing) . asAllocated

isInfixOfS :: String -> String -> Bool
isInfixOfS needle haystack = T.pack needle `T.isInfixOf` T.pack haystack

u :: String -> UUID
u suffix = read ("00000000-0000-0000-0000-0000000000" <> suffix)

cheeseId, wheyId, creamId, feedId, avoidedId, co2Id, kgId, mjId, refusedActId, consumerActId :: UUID
cheeseId = u "01"
wheyId = u "02"
creamId = u "03"
feedId = u "04"
avoidedId = u "05"
co2Id = u "06"
kgId = u "07"
mjId = u "08"
refusedActId = u "a1"
consumerActId = u "a2"

-- ---------------------------------------------------------------------------
-- End to end: an unlinked EcoSpold 2 dataset with a reference product and
-- a coproduct the source declares no share for.
-- ---------------------------------------------------------------------------

endToEnd :: Spec
endToEnd =
    describe "an EcoSpold 2 dataset with an unallocated coproduct, loaded" $ do
        it "loads, reads, is refused a score and is named by the quality report" $
            withTwoOutputDataset $ \(simpleDb, db) -> do
                V.length (dbActivities db) `shouldBe` 1
                -- No column for it: the only tech triples would be its own inputs.
                VU.length (dbTechnosphereTriples db) `shouldBe` 0
                let pidText = processIdToText db 0
                either (const Nothing) (Just . activityName . snd) (Service.resolveActivityAndProcessId db pidText)
                    `shouldBe` Just "Two outputs"
                case Service.resolveScorable db pidText of
                    Left (Service.NotScorable msg) -> T.unpack msg `shouldContain` "2 product outputs, 2 without a declared share"
                    Left other -> expectationFailure ("expected NotScorable, got " ++ show other)
                    Right _ -> expectationFailure "expected a refusal, the activity resolved for scoring"
                map qoDetail (qcOffenders (qrUnallocated (qualityReport "two" simpleDb)))
                    `shouldBe` ["2 product outputs, 2 without a declared share: state a share on every product row, or load the dataset already allocated"]

withTwoOutputDataset :: ((SimpleDatabase, Database) -> IO ()) -> IO ()
withTwoOutputDataset k = withSystemTempDirectory "es2-two-outputs" $ \dir -> do
    BS.writeFile (dir </> "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa_bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb.spold") twoOutputsXml
    loaded <- loadDatabaseWithLocationAliases defaultUnitConfig M.empty dir
    simpleDb <- either (fail . T.unpack) pure loaded
    built <- buildDatabaseWithMatrices (BuildInputs defaultUnitConfig mempty) (sdbActivities simpleDb) (sdbTechFlows simpleDb) (sdbBioFlows simpleDb) (sdbWasteFlows simpleDb) (sdbUnits simpleDb)
    db <- either (fail . T.unpack) pure built
    k (simpleDb, db)

twoOutputsXml :: BS.ByteString
twoOutputsXml =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
    \<ecoSpold xmlns=\"http://www.EcoInvent.org/EcoSpold02\">\n\
    \  <activityDataset>\n\
    \    <activityDescription>\n\
    \      <activity id=\"aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa\" activityNameId=\"two-outputs\">\n\
    \        <activityName xml:lang=\"en\">Two outputs</activityName>\n\
    \      </activity>\n\
    \      <geography geographyId=\"TEST\"><shortname xml:lang=\"en\">TEST</shortname></geography>\n\
    \    </activityDescription>\n\
    \    <flowData>\n\
    \      <intermediateExchange id=\"ref\" unitId=\"unit-kg\" amount=\"1.0\"\n\
    \                           intermediateExchangeId=\"bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb\">\n\
    \        <name xml:lang=\"en\">Cheese</name>\n\
    \        <unitName xml:lang=\"en\">kg</unitName>\n\
    \        <outputGroup>0</outputGroup>\n\
    \      </intermediateExchange>\n\
    \      <intermediateExchange id=\"co\" unitId=\"unit-kg\" amount=\"2.0\"\n\
    \                           intermediateExchangeId=\"cccccccc-cccc-cccc-cccc-cccccccccccc\">\n\
    \        <name xml:lang=\"en\">Whey</name>\n\
    \        <unitName xml:lang=\"en\">kg</unitName>\n\
    \        <outputGroup>2</outputGroup>\n\
    \      </intermediateExchange>\n\
    \      <intermediateExchange id=\"in\" unitId=\"unit-kg\" amount=\"10.0\"\n\
    \                           intermediateExchangeId=\"dddddddd-dddd-dddd-dddd-dddddddddddd\">\n\
    \        <name xml:lang=\"en\">Milk</name>\n\
    \        <unitName xml:lang=\"en\">kg</unitName>\n\
    \        <inputGroup>5</inputGroup>\n\
    \      </intermediateExchange>\n\
    \    </flowData>\n\
    \  </activityDataset>\n\
    \</ecoSpold>\n"
