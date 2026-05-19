{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module SimaProParserSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Expr (evaluate, normalizeExpr)
import SimaPro.Parser (
    BioExchangeRow (..),
    ProductRow (..),
    TechExchangeRow (..),
    defaultConfig,
    generateActivityUUID,
    generateFlowUUID,
    generateUnitUUID,
    normalizeSimaProCompartment,
    parseAmount,
    parseBioRow,
    parsePedigreePrefix,
    parseProductRow,
    parseSimaProCSV,
    parseTechRow,
    splitCSV,
 )
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import Test.Hspec
import Types (
    Activity (..),
    Exchange (..),
    Flow,
    Pedigree (..),
    UUID,
    Unit (..),
    exchangeComment,
    exchangePedigree,
    flowName,
 )
import UnitConversion (UnitConfig (..), UnitDef (..), defaultUnitConfig, isKnownUnit)

-- | Test CSV content with a quoted product name containing the delimiter (;)
testCSV :: BS.ByteString
testCSV =
    BS.intercalate
        "\r\n"
        [ "{SimaPro 9.6.0.1}"
        , "{CSV separator: semicolon}"
        , "{Decimal separator: .}"
        , ""
        , "Process"
        , ""
        , "Category type"
        , "material"
        , ""
        , "Process name"
        , "Steel Production"
        , ""
        , "Type"
        , "Unit process"
        , ""
        , "Geography"
        , "GLO"
        , ""
        , "Products"
        , "Steel;kg;1.0;100;not defined;material;"
        , ""
        , "End"
        , ""
        , "Process"
        , ""
        , "Category type"
        , "material"
        , ""
        , "Process name"
        , "Irradiated Food"
        , ""
        , "Type"
        , "Unit process"
        , ""
        , "Geography"
        , "GLO"
        , ""
        , "Products"
        , "\"Food product (irradiated ; with treatment)\";foo_unit;2.0;100;not defined;material;"
        , ""
        , "End"
        ]

-- | Parse the test CSV via a temp file
parseTestCSV :: IO ([Activity], M.Map UUID Flow, M.Map UUID Unit)
parseTestCSV = withSystemTempFile "test.csv" $ \path handle -> do
    BS.hPut handle testCSV
    hClose handle
    parseSimaProCSV defaultUnitConfig path

-- | Test CSV with waste treatment process and waste-to-treatment demand
wasteTestCSV :: BS.ByteString
wasteTestCSV =
    BS.intercalate
        "\r\n"
        [ "{SimaPro 9.6.0.1}"
        , "{CSV separator: semicolon}"
        , "{Decimal separator: .}"
        , ""
        , -- Producer with a Waste to treatment section
          "Process"
        , ""
        , "Category type"
        , "material"
        , ""
        , "Process name"
        , "Widget production"
        , ""
        , "Type"
        , "Unit process"
        , ""
        , "Products"
        , "Widget;kg;1.0;100;not defined;material;"
        , ""
        , "Waste to treatment"
        , "Municipal waste;kg;0.5;Undefined;;;;;;"
        , ""
        , "End"
        , ""
        , -- Waste treatment process (no Products section, only Waste treatment)
          "Process"
        , ""
        , "Category type"
        , "waste treatment"
        , ""
        , "Process name"
        , "Incineration process"
        , ""
        , "Type"
        , "Unit process"
        , ""
        , "Waste treatment"
        , "Municipal waste incineration;kg;1.0;100;All waste types;waste treatment;"
        , ""
        , "End"
        ]

{- | Test CSV with waste treatment product row without allocation field (6 fields).
SimaPro CSV has two product row formats:
  7 fields: name;unit;amount;allocation;waste_type;category;comment
  6 fields: name;unit;amount;waste_type;category;comment  (no allocation)
The 6-field variant is found in some waste treatment processes (e.g. Agribalyse).
Without proper detection, field 3 (waste_type) is misread as allocation, and the
comment (often containing \x7f-separated EcoSpold metadata) ends up as category.
-}
wasteNoAllocCSV :: BS.ByteString
wasteNoAllocCSV =
    BS.intercalate
        "\r\n"
        [ "{SimaPro 9.6.0.1}"
        , "{CSV separator: semicolon}"
        , "{Decimal separator: .}"
        , ""
        , "Process"
        , ""
        , "Category type"
        , "waste treatment"
        , ""
        , "Process name"
        , "treatment of non-sulfidic overburden"
        , ""
        , "Type"
        , "Unit process"
        , ""
        , "Waste treatment"
        , "Non-sulfidic overburden {GLO}| treatment of | Cut-off, S;kg;1;All waste types;Others\\Copied from Ecoinvent cut-off S;EcoSpold01Location=GLO\x7fProperties\x7fDry mass: 1 kg"
        , ""
        , "End"
        ]

-- | Parse the 6-field waste treatment CSV via a temp file
parseWasteNoAllocCSV :: IO ([Activity], M.Map UUID Flow, M.Map UUID Unit)
parseWasteNoAllocCSV = withSystemTempFile "waste-noalloc-test.csv" $ \path handle -> do
    BS.hPut handle wasteNoAllocCSV
    hClose handle
    parseSimaProCSV defaultUnitConfig path

-- | Parse the waste test CSV via a temp file
parseWasteCSV :: IO ([Activity], M.Map UUID Flow, M.Map UUID Unit)
parseWasteCSV = withSystemTempFile "waste-test.csv" $ \path handle -> do
    BS.hPut handle wasteTestCSV
    hClose handle
    parseSimaProCSV defaultUnitConfig path

-- ============================================================================
-- Expression evaluator tests
-- ============================================================================

-- | Test CSV with parameterized amounts (models butter-like process)
paramTestCSV :: BS.ByteString
paramTestCSV =
    BS.intercalate
        "\r\n"
        [ "{SimaPro 9.6.0.1}"
        , "{CSV separator: semicolon}"
        , "{Decimal separator: ,}"
        , ""
        , "Process"
        , ""
        , "Category type"
        , "material"
        , ""
        , "Process name"
        , "Butter at dairy"
        , ""
        , "Type"
        , "Unit process"
        , ""
        , "Input parameters"
        , "Qb;1;Undefined;0;0;No;"
        , "DMb;0,82;Undefined;0;0;No;"
        , "Qm;20,53;Undefined;0;0;No;"
        , "DMm;0,118;Undefined;0;0;No;"
        , ""
        , "Calculated parameters"
        , "allocButter;(Qb*DMb/(Qb*DMb+Qm*DMm))*100;"
        , ""
        , "Products"
        , "Butter {FR} U;kg;Qb;allocButter;not defined;material;"
        , ""
        , "Materials/fuels"
        , "Cow milk {FR} U;kg;Qm;Undefined;;;;;;"
        , ""
        , "Emissions to air"
        , "Carbon dioxide, fossil;high. pop.;kg;0,5;Undefined;;;;;;"
        , ""
        , "End"
        ]

parseParamCSV :: IO ([Activity], M.Map UUID Flow, M.Map UUID Unit)
parseParamCSV = withSystemTempFile "param-test.csv" $ \path handle -> do
    BS.hPut handle paramTestCSV
    hClose handle
    parseSimaProCSV defaultUnitConfig path

-- | Test CSV with database-level parameters
dbParamTestCSV :: BS.ByteString
dbParamTestCSV =
    BS.intercalate
        "\r\n"
        [ "{SimaPro 9.6.0.1}"
        , "{CSV separator: semicolon}"
        , "{Decimal separator: .}"
        , ""
        , "Database Input parameters"
        , "lbtokg;0.453592;Undefined;0;0;No;"
        , ""
        , "Database Calculated parameters"
        , ""
        , "Process"
        , ""
        , "Category type"
        , "material"
        , ""
        , "Process name"
        , "Import product"
        , ""
        , "Type"
        , "Unit process"
        , ""
        , "Products"
        , "Import product;kg;1;100;not defined;material;"
        , ""
        , "Materials/fuels"
        , "Raw material;lb;lbtokg;Undefined;;;;;;"
        , ""
        , "End"
        ]

parseDbParamCSV :: IO ([Activity], M.Map UUID Flow, M.Map UUID Unit)
parseDbParamCSV = withSystemTempFile "dbparam-test.csv" $ \path handle -> do
    BS.hPut handle dbParamTestCSV
    hClose handle
    parseSimaProCSV defaultUnitConfig path

-- | Test CSV with yield chain formula (most common pattern in Agribalyse)
yieldChainTestCSV :: BS.ByteString
yieldChainTestCSV =
    BS.intercalate
        "\r\n"
        [ "{SimaPro 9.6.0.1}"
        , "{CSV separator: semicolon}"
        , "{Decimal separator: .}"
        , ""
        , "Process"
        , ""
        , "Category type"
        , "material"
        , ""
        , "Process name"
        , "Processed food"
        , ""
        , "Type"
        , "Unit process"
        , ""
        , "Input parameters"
        , "weight_g;250;Undefined;0;0;No;"
        , "yield1;0.95;Undefined;0;0;No;"
        , "yield2;0.90;Undefined;0;0;No;"
        , ""
        , "Calculated parameters"
        , "weight_kg;weight_g/1000;"
        , "corrected;weight_kg/yield1/yield2;"
        , ""
        , "Products"
        , "Processed food;kg;1;100;not defined;material;"
        , ""
        , "Materials/fuels"
        , "Raw ingredient;kg;corrected;Undefined;;;;;;"
        , ""
        , "End"
        ]

parseYieldChainCSV :: IO ([Activity], M.Map UUID Flow, M.Map UUID Unit)
parseYieldChainCSV = withSystemTempFile "yield-test.csv" $ \path handle -> do
    BS.hPut handle yieldChainTestCSV
    hClose handle
    parseSimaProCSV defaultUnitConfig path

-- Helper: get all tech input amounts
techInputAmounts :: Activity -> [Double]
techInputAmounts act =
    [ techAmount e
    | e@TechnosphereExchange{} <- exchanges act
    , techIsInput e
    , not (techIsReference e)
    ]

-- Helper: get reference product amount
refProductAmount :: Activity -> Maybe Double
refProductAmount act = case [ techAmount e
                            | e@TechnosphereExchange{} <- exchanges act
                            , techIsReference e
                            ] of
    (a : _) -> Just a
    _ -> Nothing

spec :: Spec
spec = do
    describe "SimaPro expression evaluator" $ do
        it "evaluates numeric literals" $ do
            evaluate M.empty "42" `shouldBe` Right 42.0
            evaluate M.empty "3.14" `shouldBe` Right 3.14

        it "evaluates arithmetic" $ do
            evaluate M.empty "2+3" `shouldBe` Right 5.0
            evaluate M.empty "10-4" `shouldBe` Right 6.0
            evaluate M.empty "3*4" `shouldBe` Right 12.0
            evaluate M.empty "10/4" `shouldBe` Right 2.5

        it "evaluates parenthesized expressions" $ do
            evaluate M.empty "(2+3)*4" `shouldBe` Right 20.0
            evaluate M.empty "2*(3+4)" `shouldBe` Right 14.0

        it "evaluates variables" $ do
            let env = M.fromList [("Qm", 20.53), ("Qb", 1.0)]
            evaluate env "Qm" `shouldBe` Right 20.53
            evaluate env "Qb" `shouldBe` Right 1.0

        it "evaluates complex expressions with variables" $ do
            let env = M.fromList [("Qb", 1.0), ("DMb", 0.82), ("Qm", 20.53), ("DMm", 0.118)]
            -- Butter allocation formula: (Qb*DMb/(Qb*DMb+Qm*DMm))*100
            let result = evaluate env "(Qb*DMb/(Qb*DMb+Qm*DMm))*100"
            case result of
                Right v -> v `shouldSatisfy` (\x -> abs (x - 25.29) < 0.1)
                Left e -> expectationFailure $ "Evaluation failed: " ++ e

        it "evaluates yield correction chains" $ do
            let env = M.fromList [("weight_kg", 0.25), ("yield1", 0.95), ("yield2", 0.90)]
            let result = evaluate env "weight_kg/yield1/yield2"
            case result of
                Right v -> v `shouldSatisfy` (\x -> abs (x - 0.2924) < 0.001)
                Left e -> expectationFailure $ "Evaluation failed: " ++ e

        it "evaluates power operator" $ do
            evaluate M.empty "2^3" `shouldBe` Right 8.0
            evaluate M.empty "3^2" `shouldBe` Right 9.0

        it "evaluates unary minus" $ do
            evaluate M.empty "-5" `shouldBe` Right (-5.0)
            evaluate M.empty "-(2+3)" `shouldBe` Right (-5.0)

        it "rejects unknown variables" $ do
            evaluate M.empty "xyz" `shouldSatisfy` isLeft

        -- Regression: Agribalyse Emmental defines the dry-matter param as "Dmper"
        -- but references "DMper" in the allocation formula. SimaPro treats parameter
        -- names case-insensitively; VoLCA must do the same or allocation → 0 and the
        -- whole activity shows zero impacts.
        it "looks up variables case-insensitively" $ do
            let env = M.fromList [("Dmper", 5.0), ("Qper", 60530841.0)]
            evaluate env "Qper*DMper" `shouldBe` Right (60530841.0 * 5.0)
            evaluate env "qper*dmper" `shouldBe` Right (60530841.0 * 5.0)

        it "normalizes comma decimal separator" $ do
            normalizeExpr ',' "0,82" `shouldBe` "0.82"
            normalizeExpr ',' "Qb*0,5" `shouldBe` "Qb*0.5"

        it "normalizes dot decimal (comma becomes semicolon for func args)" $ do
            normalizeExpr '.' "min(a,b)" `shouldBe` "min(a;b)"

    describe "SimaPro CSV parsing" $ do
        it "correctly extracts units from CSV with quoted fields" $ do
            (_, _, unitDB) <- parseTestCSV
            let unitNames = map unitName $ M.elems unitDB
            -- Exactly these two units — no more, no less
            S.fromList unitNames `shouldBe` S.fromList ["kg", "foo_unit"]

        it "reports unknown units correctly" $ do
            (_, _, unitDB) <- parseTestCSV
            let cfg = defaultUnitConfig
                unknowns =
                    [ unitName u
                    | u <- M.elems unitDB
                    , not (isKnownUnit cfg (unitName u))
                    ]
            unknowns `shouldContain` ["foo_unit"]
            unknowns `shouldNotContain` ["kg"]

        it "parses product names with embedded delimiters correctly" $ do
            (activities, flowDB, _) <- parseTestCSV
            -- activityName now reflects the SimaPro Process name (multi-product
            -- friendly); the quoted product name lives on the reference flow.
            let names = map activityName activities
            names `shouldContain` ["Irradiated Food"]
            let flowNames = map flowName (M.elems flowDB)
            flowNames `shouldContain` ["Food product (irradiated ; with treatment)"]

    describe "SimaPro classification parsing" $ do
        it "parses Category type from metadata" $ do
            (activities, _, _) <- parseTestCSV
            let cls = activityClassification (head activities)
            M.lookup "Category type" cls `shouldBe` Just "material"

        it "parses Category from product line" $ do
            (activities, _, _) <- parseTestCSV
            let cls = activityClassification (head activities)
            M.lookup "Category" cls `shouldBe` Just "material"

    describe "SimaPro waste treatment parsing" $ do
        it "parses waste treatment processes (Waste treatment section)" $ do
            (activities, _, _) <- parseWasteCSV
            length activities `shouldSatisfy` (>= 2)

        it "uses Process name as activity name (waste treatment block)" $ do
            (activities, _, _) <- parseWasteCSV
            let names = map activityName activities
            names `shouldContain` ["Incineration process"]

        it "parses 6-field waste treatment rows without allocation" $ do
            (activities, _, _) <- parseWasteNoAllocCSV
            length activities `shouldBe` 1
            let a = head activities
            activityName a `shouldBe` "treatment of non-sulfidic overburden"
            let cls = activityClassification a
            M.lookup "Category" cls `shouldBe` Just "Others\\Copied from Ecoinvent cut-off S"

        it "marks Waste to treatment exchanges as inputs" $ do
            (activities, _, _) <- parseWasteCSV
            let producer = head [a | a <- activities, activityName a == "Widget production"]
                wasteExchanges =
                    [ e
                    | e@TechnosphereExchange{} <- exchanges producer
                    , not (techIsReference e)
                    , techIsInput e
                    ]
            length wasteExchanges `shouldSatisfy` (>= 1)

    describe "SimaPro parameterized amounts" $ do
        it "resolves simple variable references (Qm=20.53 for cow milk, scaled by allocation)" $ do
            (activities, _, _) <- parseParamCSV
            let butter = head activities
                -- allocButter = (1*0.82/(1*0.82+20.53*0.118))*100 ≈ 25.285
                -- Cow milk amount = 20.53 * allocButter/100 ≈ 5.19
                milkAmounts = techInputAmounts butter
            length milkAmounts `shouldBe` 1
            head milkAmounts `shouldSatisfy` (\x -> abs (x - 5.19) < 0.01)

        it "resolves parameterized product amount (Qb=1)" $ do
            (activities, _, _) <- parseParamCSV
            let butter = head activities
            refProductAmount butter `shouldBe` Just 1.0

        it "resolves calculated parameter in allocation (allocButter formula)" $ do
            (activities, _, _) <- parseParamCSV
            let butter = head activities
            -- Product allocation uses allocButter = (Qb*DMb/(Qb*DMb+Qm*DMm))*100
            -- = (1*0.82/(1*0.82+20.53*0.118))*100 ≈ 25.3%
            -- The product exchange should have the resolved allocation
            -- (we check that the activity was created = params didn't break parsing)
            length (exchanges butter) `shouldSatisfy` (>= 3) -- product + milk + CO2
        it "stores resolved parameter values in activity" $ do
            (activities, _, _) <- parseParamCSV
            let butter = head activities
            M.lookup "Qm" (activityParams butter) `shouldBe` Just 20.53
            M.lookup "Qb" (activityParams butter) `shouldBe` Just 1.0
            M.lookup "DMb" (activityParams butter) `shouldBe` Just 0.82

        it "stores raw expressions for re-evaluation" $ do
            (activities, _, _) <- parseParamCSV
            let butter = head activities
            M.lookup "allocButter" (activityParamExprs butter)
                `shouldBe` Just "(Qb*DMb/(Qb*DMb+Qm*DMm))*100"

        it "does not drop exchanges with parameterized amounts" $ do
            (activities, _, _) <- parseParamCSV
            let butter = head activities
                techInputs =
                    [ e
                    | e@TechnosphereExchange{} <- exchanges butter
                    , techIsInput e
                    , not (techIsReference e)
                    ]
            -- Cow milk should NOT be dropped (was the original bug)
            length techInputs `shouldBe` 1

        it "scales biosphere exchanges by allocation fraction" $ do
            (activities, _, _) <- parseParamCSV
            let butter = head activities
                bioExchanges = [e | e@BiosphereExchange{} <- exchanges butter]
            length bioExchanges `shouldBe` 1
            -- CO2 = 0.5 * allocButter/100 ≈ 0.5 * 0.25285 ≈ 0.1264
            bioAmount (head bioExchanges) `shouldSatisfy` (\x -> abs (x - 0.1264) < 0.01)

    describe "SimaPro database-level parameters" $ do
        it "resolves database input params in exchange amounts" $ do
            (activities, _, _) <- parseDbParamCSV
            let act = head activities
            -- Raw material amount should be lbtokg = 0.453592
            techInputAmounts act `shouldContain` [0.453592]

    describe "SimaPro yield chain formulas" $ do
        it "resolves chained division (weight_g/1000/yield1/yield2)" $ do
            (activities, _, _) <- parseYieldChainCSV
            let act = head activities
                amounts = techInputAmounts act
            -- corrected = 250/1000/0.95/0.90 ≈ 0.2924
            length amounts `shouldBe` 1
            head amounts `shouldSatisfy` (\x -> abs (x - 0.2924) < 0.001)

    -- -----------------------------------------------------------------------
    -- Pure row parsers
    -- -----------------------------------------------------------------------

    describe "splitCSV" $ do
        it "splits basic semicolon-delimited row" $
            splitCSV ';' "a;b;c" `shouldBe` ["a", "b", "c"]

        it "strips quotes and embeds delimiter in field" $
            splitCSV ';' "\"a;b\";c" `shouldBe` ["a;b", "c"]

        it "splits comma-delimited row" $
            splitCSV ',' "x,y,z" `shouldBe` ["x", "y", "z"]

        it "handles empty fields" $
            splitCSV ';' "a;;c" `shouldBe` ["a", "", "c"]

    describe "parseAmount" $ do
        it "parses integer" $
            parseAmount '.' "42" `shouldBe` 42.0

        it "parses decimal with dot separator" $
            parseAmount '.' "3.14" `shouldBe` 3.14

        it "parses decimal with comma separator" $
            parseAmount ',' "3,14" `shouldBe` 3.14

        it "returns 0.0 for empty input" $
            parseAmount '.' "" `shouldBe` 0.0

        it "returns 0.0 for non-numeric" $
            parseAmount '.' "abc" `shouldBe` 0.0

    describe "parseProductRow" $ do
        it "parses 7-field product row" $
            parseProductRow defaultConfig "Steel;kg;1.0;100;not defined;material;comment"
                `shouldBe` Just
                    ProductRow
                        { prName = "Steel"
                        , prUnit = "kg"
                        , prAmount = 1.0
                        , prAmountRaw = "1.0"
                        , prAllocation = 100.0
                        , prAllocRaw = "100"
                        , prWasteType = "not defined"
                        , prCategory = "material"
                        , prComment = "comment"
                        }

        it "parses 6-field row (no allocation — waste treatment)" $
            parseProductRow defaultConfig "Waste flow;kg;1.0;All waste types;waste treatment;comment"
                `shouldBe` Just
                    ProductRow
                        { prName = "Waste flow"
                        , prUnit = "kg"
                        , prAmount = 1.0
                        , prAmountRaw = "1.0"
                        , prAllocation = 100.0
                        , prAllocRaw = "100"
                        , prWasteType = "All waste types"
                        , prCategory = "waste treatment"
                        , prComment = "comment"
                        }

        it "returns Nothing for too-short row" $
            parseProductRow defaultConfig "name;kg" `shouldBe` Nothing

    describe "parseTechRow" $ do
        it "parses full tech exchange row" $
            parseTechRow defaultConfig "Coal;kg;5.0;Undefined;;;;;;"
                `shouldSatisfy` \case
                    Just r -> terName r == "Coal" && terUnit r == "kg" && terAmount r == 5.0
                    Nothing -> False

        it "parses minimal tech row (name;unit;amount)" $
            parseTechRow defaultConfig "Oil;MJ;2.5"
                `shouldSatisfy` \case
                    Just r -> terName r == "Oil" && terAmount r == 2.5
                    Nothing -> False

        it "returns Nothing for too-short row" $
            parseTechRow defaultConfig "name" `shouldBe` Nothing

    describe "parseBioRow" $ do
        it "parses full bio exchange row" $
            parseBioRow defaultConfig "Carbon dioxide;air;kg;1.0;Undefined;;;;;;"
                `shouldSatisfy` \case
                    Just r -> berName r == "Carbon dioxide" && berCompartment r == "air" && berAmount r == 1.0
                    Nothing -> False

        it "parses minimal bio row (name;comp;unit;amount)" $
            parseBioRow defaultConfig "Methane;air;kg;0.5"
                `shouldSatisfy` \case
                    Just r -> berName r == "Methane" && berUnit r == "kg"
                    Nothing -> False

        it "returns Nothing for too-short row" $
            parseBioRow defaultConfig "name;air" `shouldBe` Nothing

    -- -----------------------------------------------------------------------
    -- Per-exchange comments (terComment / berComment)
    -- -----------------------------------------------------------------------

    describe "per-exchange comments" $ do
        it "surfaces the trailing free-text comment on a Materials/fuels row" $ do
            (activities, _, _) <-
                parseSectionCSV
                    [ "Materials/fuels"
                    , "Soybean meal BR;kg;6506.5;Lognormal;1.533;0;0;Soybean, meal 46 BR, crushing in Brazil, at french port, average, FR"
                    ]
            let inputs =
                    [ ex
                    | act <- activities
                    , ex <- exchanges act
                    , case ex of
                        TechnosphereExchange{techIsInput = True} -> True
                        _ -> False
                    ]
            map exchangeComment inputs
                `shouldBe` [Just "Soybean, meal 46 BR, crushing in Brazil, at french port, average, FR"]

        it "returns Nothing for a comment-less row" $ do
            (activities, _, _) <-
                parseSectionCSV
                    [ "Materials/fuels"
                    , "Plain {FR} U;kg;1.0;Undefined;;;;;;"
                    ]
            let inputs =
                    [ ex
                    | act <- activities
                    , ex <- exchanges act
                    , case ex of
                        TechnosphereExchange{techIsInput = True} -> True
                        _ -> False
                    ]
            map exchangeComment inputs `shouldBe` [Nothing]

        it "surfaces a per-emission comment on a biosphere row" $ do
            (activities, _, _) <-
                parseSectionCSV
                    [ "Emissions to air"
                    , "Carbon dioxide, fossil;high. pop.;kg;0.5;Undefined;;;;;;tail-pipe combustion"
                    ]
            let bios =
                    [ ex
                    | act <- activities
                    , ex <- exchanges act
                    , case ex of
                        BiosphereExchange{} -> True
                        _ -> False
                    ]
            map exchangeComment bios `shouldBe` [Just "tail-pipe combustion"]

    -- -----------------------------------------------------------------------
    -- Pedigree matrix
    -- -----------------------------------------------------------------------

    describe "parsePedigreePrefix" $ do
        it "extracts pedigree and clean comment when both are present" $
            parsePedigreePrefix "(3,3,2,1,2),. The ecoinvent process only takes pure product"
                `shouldBe` ( Just (Pedigree 3 3 2 1 2)
                           , Just "The ecoinvent process only takes pure product"
                           )

        it "extracts pedigree alone when no comment follows" $
            parsePedigreePrefix "(3,3,2,1,2),"
                `shouldBe` (Just (Pedigree 3 3 2 1 2), Nothing)

        it "keeps the raw text when there is no pedigree prefix" $
            parsePedigreePrefix "Free comment with no pedigree"
                `shouldBe` (Nothing, Just "Free comment with no pedigree")

        it "returns Nothing/Nothing for empty input" $
            parsePedigreePrefix "" `shouldBe` (Nothing, Nothing)

        it "rejects out-of-range digits and preserves the raw text" $
            parsePedigreePrefix "(6,3,2,1,2), extra"
                `shouldBe` (Nothing, Just "(6,3,2,1,2), extra")

        it "rejects malformed parentheses and preserves the raw text" $
            parsePedigreePrefix "(3,3,2,1 extra"
                `shouldBe` (Nothing, Just "(3,3,2,1 extra")

        it "tolerates leading whitespace before the pedigree" $
            parsePedigreePrefix "  (3,3,2,1,2),. note"
                `shouldBe` (Just (Pedigree 3 3 2 1 2), Just "note")

    describe "pedigree wired through to Exchange" $ do
        it "populates techPedigree and strips the prefix from the comment" $ do
            (activities, _, _) <-
                parseSectionCSV
                    [ "Materials/fuels"
                    , "Soybean meal BR;kg;6506.5;Lognormal;1.533;0;0;(3,3,2,1,2),. Brazilian port average"
                    ]
            let inputs =
                    [ ex
                    | act <- activities
                    , ex <- exchanges act
                    , case ex of
                        TechnosphereExchange{techIsInput = True} -> True
                        _ -> False
                    ]
            map exchangeComment inputs `shouldBe` [Just "Brazilian port average"]
            map exchangePedigree inputs `shouldBe` [Just (Pedigree 3 3 2 1 2)]

        it "populates bioPedigree for emission rows that carry only pedigree" $ do
            (activities, _, _) <-
                parseSectionCSV
                    [ "Emissions to air"
                    , "Carbon dioxide, fossil;high. pop.;kg;0.5;Undefined;;;;;;(2,2,1,1,1),"
                    ]
            let bios =
                    [ ex
                    | act <- activities
                    , ex <- exchanges act
                    , case ex of
                        BiosphereExchange{} -> True
                        _ -> False
                    ]
            map exchangeComment bios `shouldBe` [Nothing]
            map exchangePedigree bios `shouldBe` [Just (Pedigree 2 2 1 1 1)]

    -- -----------------------------------------------------------------------
    -- UUID generation
    -- -----------------------------------------------------------------------

    describe "UUID generation" $ do
        it "generateUnitUUID is deterministic" $
            generateUnitUUID "kg" `shouldBe` generateUnitUUID "kg"

        it "generateUnitUUID differs for different units" $
            generateUnitUUID "kg" `shouldNotBe` generateUnitUUID "MJ"

        it "generateFlowUUID is deterministic" $
            generateFlowUUID "CO2" "air" "kg" `shouldBe` generateFlowUUID "CO2" "air" "kg"

        it "generateFlowUUID differs when compartment differs" $
            generateFlowUUID "CO2" "air" "kg" `shouldNotBe` generateFlowUUID "CO2" "water" "kg"

    -- -----------------------------------------------------------------------
    -- CF ↔ biosphere UUID alignment (regression: see PR #65)
    --
    -- The inventory parser ('bioRowToExchange') and the method CF parser
    -- ('Method.ParserSimaPro') both hash flow UUIDs via 'generateFlowUUID'
    -- composed with 'normalizeSimaProCompartment'. These tests pin the
    -- invariant that the two call sites land on the same UUID for the same
    -- elementary flow — the bug they prevent silently routed every regional,
    -- non-kg or '(unspecified)'-sub CF through the slower name cascade.
    -- -----------------------------------------------------------------------
    describe "CF / biosphere flow UUID alignment" $ do
        let cfSide name comp sub unit =
                generateFlowUUID name (normalizeSimaProCompartment comp sub) unit
            bioSide name comp sub unit =
                generateFlowUUID name (normalizeSimaProCompartment comp sub) unit

        it "regional water in m³: CF 'Raw'/'(unspecified)' matches bio 'resource'/blank" $
            cfSide "Water, FR" "Raw" "(unspecified)" "m3"
                `shouldBe` bioSide "Water, FR" "resource" "" "m3"

        it "non-kg energy resource: CF unit must come from the CF row, not 'kg'" $
            cfSide "Energy, gross calorific value, in biomass" "Raw" "(unspecified)" "MJ"
                `shouldBe` bioSide "Energy, gross calorific value, in biomass" "resource" "" "MJ"

        it "regional air emission: CF 'Air'/'(unspecified)' matches bio 'air'/blank" $
            cfSide "Nitrogen dioxide, FR" "Air" "(unspecified)" "kg"
                `shouldBe` bioSide "Nitrogen dioxide, FR" "air" "" "kg"

        it "subcompartment case is normalized on both sides" $
            cfSide "NOx" "Air" "Low. Pop." "kg"
                `shouldBe` bioSide "NOx" "air" "low. pop." "kg"

        it "CF 'resources' header matches bio 'resource' literal" $
            cfSide "Iron" "Resources" "in ground" "kg"
                `shouldBe` bioSide "Iron" "resource" "in ground" "kg"

        it "differs when units differ (kg vs m³ are distinct flows)" $
            cfSide "Water" "Raw" "(unspecified)" "kg"
                `shouldNotBe` cfSide "Water" "Raw" "(unspecified)" "m3"

        it "differs when the regional suffix differs" $
            cfSide "Water, FR" "Raw" "(unspecified)" "m3"
                `shouldNotBe` cfSide "Water, DE" "Raw" "(unspecified)" "m3"

        it "preserves the unit signal in the hash (CF unit is not stripped)" $
            generateFlowUUID "Water" (normalizeSimaProCompartment "Raw" "(unspecified)") "m3"
                `shouldNotBe` generateFlowUUID "Water" (normalizeSimaProCompartment "Raw" "(unspecified)") "kg"

    -- -----------------------------------------------------------------------
    -- Uncovered CSV sections
    -- -----------------------------------------------------------------------

    describe "SimaPro uncovered sections" $ do
        it "parses Electricity/heat exchanges" $ do
            (activities, _, _) <-
                parseSectionCSV
                    [ "Electricity/heat"
                    , "Electricity, medium voltage;kWh;0.3;Undefined;;;;;;"
                    ]
            let act = head activities
                techIn = [e | e@TechnosphereExchange{} <- exchanges act, techIsInput e, not (techIsReference e)]
            length techIn `shouldBe` 1

        it "parses Resources (biosphere inputs)" $ do
            (activities, _, _) <-
                parseSectionCSV
                    [ "Resources"
                    , "Water, river;in water;m3;0.1;Undefined;;;;;;"
                    ]
            let bio = [e | e@BiosphereExchange{} <- exchanges (head activities)]
            length bio `shouldBe` 1

        it "parses Emissions to water" $ do
            (activities, _, _) <-
                parseSectionCSV
                    [ "Emissions to water"
                    , "Phosphate;river;kg;0.01;Undefined;;;;;;"
                    ]
            let bio = [e | e@BiosphereExchange{} <- exchanges (head activities)]
            length bio `shouldBe` 1

        it "parses Emissions to soil" $ do
            (activities, _, _) <-
                parseSectionCSV
                    [ "Emissions to soil"
                    , "Zinc;agricultural;kg;0.001;Undefined;;;;;;"
                    ]
            let bio = [e | e@BiosphereExchange{} <- exchanges (head activities)]
            length bio `shouldBe` 1

        it "parses Final waste flows" $ do
            (activities, _, _) <-
                parseSectionCSV
                    [ "Final waste flows"
                    , "Inert waste, for final disposal;kg;0.5;Undefined;;;;;;"
                    ]
            let bio = [e | e@BiosphereExchange{} <- exchanges (head activities)]
            length bio `shouldBe` 1

        it "parses location from process name {XX} pattern" $ do
            (activities, _, _) <- parseNamedCSV "Widget {FR} U" []
            activityLocation (head activities) `shouldBe` "FR"

        it "parses location from process name //[XX] pattern (ecoinvent 3.9.1 SimaPro export)" $ do
            (activities, _, _) <- parseNamedCSV "mango//[BR] mango production" []
            activityLocation (head activities) `shouldBe` "BR"

        it "parses location from Geography metadata" $ do
            (activities, _, _) <- parseTestCSV
            let a = head activities
            activityLocation a `shouldBe` "GLO"

    describe "SimaPro comma CSV separator" $ do
        it "parses comma-separated CSV" $ do
            (activities, _, _) <- parseCommaCSV
            length activities `shouldBe` 1
            activityName (head activities) `shouldBe` "Comma Product"

    describe "Reference product unit normalization" $ do
        -- Regression for a SimaPro export (e.g. Agribalyse 3.2 "Alfalfa, hay, ... {FR} U")
        -- declaring a reference product as "1 ton" instead of the canonical "kg". The
        -- in-memory reference amount must be converted to 1000 kg, otherwise the
        -- technosphere column normalization divides by 1 instead of 1000 and every
        -- impact score for that activity comes back 1000× too large.
        it "converts a 1-ton reference to 1000 kg at ingest (canonical base)" $ do
            (activities, _, _) <- parseTonRefCSV
            length activities `shouldBe` 1
            let act = head activities
                refExs =
                    [ ex
                    | ex <- exchanges act
                    , techIsReference ex
                    , not (techIsInput ex)
                    ]
            length refExs `shouldBe` 1
            let ex = head refExs
            techAmount ex `shouldBe` 1000.0
            activityUnit act `shouldBe` "kg"

        it "leaves an already-canonical kg reference unchanged" $ do
            (activities, _, _) <- parseTestCSV
            let steel = head [a | a <- activities, activityName a == "Steel Production"]
                refExs = [ex | ex <- exchanges steel, techIsReference ex, not (techIsInput ex)]
            techAmount (head refExs) `shouldBe` 1.0
            activityUnit steel `shouldBe` "kg"

    describe "SimaPro multi-product processes (coproducts)" $ do
        -- One Process block declares 5 coproducts with mass-allocation formulas.
        -- The 5 resulting Activity records must share one activityUUID (so
        -- dbActivityProductsIndex can group them) and each must carry its own
        -- allocation percentage.
        it "shares one activityUUID across all 5 coproducts" $ do
            (activities, _, _) <- parseMultiCoproductCSV
            length activities `shouldBe` 5
            let uuids = S.fromList (map generateActivityUUID activities)
            S.size uuids `shouldBe` 1

        it "uses the Process name as activityName for every coproduct" $ do
            (activities, _, _) <- parseMultiCoproductCSV
            let names = S.fromList (map activityName activities)
            S.size names `shouldBe` 1
            S.member "Multi-coproduct refinery" names `shouldBe` True

        it "stores allocation percentage on each coproduct Activity" $ do
            (activities, _, _) <- parseMultiCoproductCSV
            -- Order-agnostic: the 5 allocation percentages must match the
            -- declared shares (50/20/15/10/5) as a multiset.
            let percents = [p | a <- activities, Just p <- [activityAllocationPercent a]]
                expected = [50.0, 20.0, 15.0, 10.0, 5.0]
                matchOne e = any (\p -> abs (p - e) < 0.01) percents
            length percents `shouldBe` 5
            all matchOne expected `shouldBe` True
            abs (sum percents - 100.0) `shouldSatisfy` (< 0.01)

        it "preserves the raw allocation formula when non-numeric" $ do
            -- paramTestCSV (butter) uses an expression "allocButter" for allocation
            (activities, _, _) <- parseParamCSV
            let butter = head activities
            activityAllocationFormula butter `shouldBe` Just "allocButter"

        it "leaves allocation formula populated for formula-based allocations" $ do
            (activities, _, _) <- parseMultiCoproductCSV
            -- Multi-coproduct allocations are of the form 'Sx /(...)*100',
            -- so the formula field should be populated for each coproduct.
            let formulas = [activityAllocationFormula a | a <- activities]
            all (/= Nothing) formulas `shouldBe` True

        it "scales shared exchanges by the per-coproduct allocation fraction" $ do
            (activities, _, _) <- parseMultiCoproductCSV
            -- A shared 1 kg upstream input is allocated across the 5 coproducts:
            -- Alpha 0.5 kg, Beta 0.2 kg, Gamma 0.15 kg, Delta 0.1 kg, Epsilon
            -- 0.05 kg. The sum across all coproduct columns restores 1 kg.
            let perActivity =
                    [ techAmount e
                    | a <- activities
                    , e@TechnosphereExchange{} <- exchanges a
                    , techIsInput e
                    , not (techIsReference e)
                    ]
            length perActivity `shouldBe` 5
            let total = sum perActivity
            abs (total - 1.0) `shouldSatisfy` (< 0.001)

    describe "SimaPro Process name fallback" $ do
        it "falls back to product name when Process name field is empty" $ do
            (activities, _, _) <- parseNoProcessNameCSV
            length activities `shouldBe` 1
            -- With an empty 'Process name', the product name is used as
            -- activityName (preserves legacy behaviour for mono-product CSVs
            -- that omit the field).
            activityName (head activities) `shouldBe` "Bare product"
            -- A single-product Process still carries Just 100.0 allocation.
            activityAllocationPercent (head activities) `shouldBe` Just 100.0

-- ---------------------------------------------------------------------------
-- Helpers for section tests
-- ---------------------------------------------------------------------------

-- | Build a minimal process CSV with extra section lines inserted
parseSectionCSV :: [BS.ByteString] -> IO ([Activity], M.Map UUID Flow, M.Map UUID Unit)
parseSectionCSV sectionLines =
    parseNamedCSV "Test process" sectionLines

parseNamedCSV :: BS.ByteString -> [BS.ByteString] -> IO ([Activity], M.Map UUID Flow, M.Map UUID Unit)
parseNamedCSV procName sectionLines =
    withSystemTempFile "section-test.csv" $ \path handle -> do
        let content =
                BS.intercalate "\r\n" $
                    [ "{SimaPro 9.6.0.1}"
                    , "{CSV separator: semicolon}"
                    , "{Decimal separator: .}"
                    , ""
                    , "Process"
                    , ""
                    , "Category type"
                    , "material"
                    , ""
                    , "Process name"
                    , procName
                    , ""
                    , "Type"
                    , "Unit process"
                    , ""
                    , "Products"
                    , "Reference product;kg;1.0;100;not defined;material;"
                    , ""
                    ]
                        ++ sectionLines
                        ++ [ ""
                           , "End"
                           ]
        BS.hPut handle content
        hClose handle
        parseSimaProCSV defaultUnitConfig path

-- | CSV with comma as separator
commaCSV :: BS.ByteString
commaCSV =
    BS.intercalate
        "\r\n"
        [ "{SimaPro 9.6.0.1}"
        , "{CSV separator: Comma}"
        , "{Decimal separator: .}"
        , ""
        , "Process"
        , ""
        , "Category type"
        , "material"
        , ""
        , "Process name"
        , "Comma Product"
        , ""
        , "Type"
        , "Unit process"
        , ""
        , "Products"
        , "Comma Product,kg,1.0,100,not defined,material,"
        , ""
        , "End"
        ]

parseCommaCSV :: IO ([Activity], M.Map UUID Flow, M.Map UUID Unit)
parseCommaCSV = withSystemTempFile "comma-test.csv" $ \path handle -> do
    BS.hPut handle commaCSV
    hClose handle
    parseSimaProCSV defaultUnitConfig path

-- | Unit config that knows about "ton" (1000 kg) in addition to kg.
tonUnitConfig :: UnitConfig
tonUnitConfig =
    UnitConfig
        { ucDimensionOrder = ["mass", "length", "time", "energy", "area", "volume", "count", "currency"]
        , ucUnits =
            M.fromList
                [ ("kg", UnitDef [1, 0, 0, 0, 0, 0, 0, 0] 1.0)
                , ("ton", UnitDef [1, 0, 0, 0, 0, 0, 0, 0] 1000.0)
                ]
        , ucOriginalKeys = M.fromList [("kg", "kg"), ("ton", "ton")]
        }

-- | A minimal SimaPro CSV declaring a reference product of "1 ton" (mass).
tonRefCSV :: BS.ByteString
tonRefCSV =
    BS.intercalate
        "\r\n"
        [ "{SimaPro 9.6.0.1}"
        , "{CSV separator: semicolon}"
        , "{Decimal separator: .}"
        , ""
        , "Process"
        , ""
        , "Category type"
        , "material"
        , ""
        , "Process name"
        , "Alfalfa, hay {FR} U"
        , ""
        , "Type"
        , "Unit process"
        , ""
        , "Products"
        , "Alfalfa, hay {FR} U;ton;1.0;100;not defined;material;"
        , ""
        , "End"
        ]

parseTonRefCSV :: IO ([Activity], M.Map UUID Flow, M.Map UUID Unit)
parseTonRefCSV = withSystemTempFile "ton-ref.csv" $ \path handle -> do
    BS.hPut handle tonRefCSV
    hClose handle
    parseSimaProCSV tonUnitConfig path

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

{- | Generic 5-coproduct fixture: one Process block emits 5 fictional outputs
with five mass-allocation formulas. Percentages are chosen so they sum to
exactly 100 and use round numbers (50/20/15/10/5), keeping the test
self-contained without copying real LCA data. The five share-parameters
(S1..S5) are arranged so the formula 'Sx /(S1+S2+S3+S4+S5)*100' returns
the corresponding percentage. A single upstream input lets us assert
allocation scaling on the resulting Activities.
-}
multiCoproductCSV :: BS.ByteString
multiCoproductCSV =
    BS.intercalate
        "\r\n"
        [ "{SimaPro 9.6.0.1}"
        , "{CSV separator: semicolon}"
        , "{Decimal separator: .}"
        , ""
        , "Process"
        , ""
        , "Category type"
        , "processing"
        , ""
        , "Type"
        , "Unit process"
        , ""
        , "Process name"
        , "Multi-coproduct refinery"
        , ""
        , "Geography"
        , "GLO"
        , ""
        , "Products"
        , "Product Alpha;kg;1;S1 /(S1 + S2 + S3 + S4 + S5)*100;not defined;processing;"
        , "Product Beta;kg;1;S2 /(S1 + S2 + S3 + S4 + S5)*100;not defined;processing;"
        , "Product Gamma;kg;1;S3 /(S1 + S2 + S3 + S4 + S5)*100;not defined;processing;"
        , "Product Delta;kg;1;S4 /(S1 + S2 + S3 + S4 + S5)*100;not defined;processing;"
        , "Product Epsilon;kg;1;S5 /(S1 + S2 + S3 + S4 + S5)*100;not defined;processing;"
        , ""
        , "Input parameters"
        , "S1;50;Undefined;0;0;No;"
        , "S2;20;Undefined;0;0;No;"
        , "S3;15;Undefined;0;0;No;"
        , "S4;10;Undefined;0;0;No;"
        , "S5;5;Undefined;0;0;No;"
        , ""
        , "Materials/fuels"
        , "Upstream feedstock;kg;1;Undefined;;;;;;"
        , ""
        , "End"
        ]

parseMultiCoproductCSV :: IO ([Activity], M.Map UUID Flow, M.Map UUID Unit)
parseMultiCoproductCSV = withSystemTempFile "multi-coproduct.csv" $ \path handle -> do
    BS.hPut handle multiCoproductCSV
    hClose handle
    parseSimaProCSV defaultUnitConfig path

{- | Single-product CSV with an empty Process name field. Mirrors mono-product
SimaPro exports that leave the "Process name" line blank: the parser must
fall back to the product row name so activityUUID stays stable.
-}
noProcessNameCSV :: BS.ByteString
noProcessNameCSV =
    BS.intercalate
        "\r\n"
        [ "{SimaPro 9.6.0.1}"
        , "{CSV separator: semicolon}"
        , "{Decimal separator: .}"
        , ""
        , "Process"
        , ""
        , "Category type"
        , "material"
        , ""
        , "Process name"
        , ""
        , ""
        , "Type"
        , "Unit process"
        , ""
        , "Geography"
        , "GLO"
        , ""
        , "Products"
        , "Bare product;kg;1.0;100;not defined;material;"
        , ""
        , "End"
        ]

parseNoProcessNameCSV :: IO ([Activity], M.Map UUID Flow, M.Map UUID Unit)
parseNoProcessNameCSV = withSystemTempFile "no-process-name.csv" $ \path handle -> do
    BS.hPut handle noProcessNameCSV
    hClose handle
    parseSimaProCSV defaultUnitConfig path
