{-# LANGUAGE OverloadedStrings #-}

{- | Dataset-soundness report tests.

Each check gets a database built to trip exactly it, plus the negative controls
that keep it from crying wolf: a treatment activity whose reference is an input,
allocation that rounds (33.3 × 3), an ordinary input at zero, coproduct blocks
that share a name but not an identifier.
-}
module QualityReportSpec (spec) where

import Data.Aeson (decode, encode)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Test.Hspec

import API.DatabaseHandlers (qualityReportToAPI)
import API.Types (QualityCheckAPI (..), QualityOffenderAPI (..), QualityReportAPI (..))
import Database.Quality (
    QualityCheck (..),
    QualityOffender (..),
    QualityReport (..),
    qualityReport,
 )
import Types (
    Activity (..),
    Exchange (..),
    FormulaCheck (..),
    NativeProcessId (..),
    Severity (..),
    SimpleDatabase (..),
    TechRole (..),
    TechnosphereFlow (..),
    Unit (..),
    WasteFlow (..),
 )

-- ---------------------------------------------------------------------------
-- UUID helpers: readable fixed identifiers.
-- ---------------------------------------------------------------------------

u :: String -> UUID
u suffix = read ("00000000-0000-0000-0000-0000000000" <> suffix)

kgUnit, unknownUnit, breadFlow, flourFlow, wasteFlow :: UUID
kgUnit = u "01"
unknownUnit = u "02"
breadFlow = u "03"
flourFlow = u "04"
wasteFlow = u "05"

actA, actB, prodA, prodB :: UUID
actA = u "0a"
actB = u "0b"
prodA = u "1a"
prodB = u "1b"

-- ---------------------------------------------------------------------------
-- Fixture building blocks
-- ---------------------------------------------------------------------------

mkActivity :: Text -> [Exchange] -> Activity
mkActivity name exs =
    Activity
        { activityName = name
        , activityDescription = ["A described activity"]
        , activitySynonyms = M.empty
        , activityClassification = M.singleton "ISIC" "1071"
        , activityLocation = "FR"
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

techExchange :: UUID -> Double -> TechRole -> Exchange
techExchange fid amount role =
    TechnosphereExchange
        { techFlowId = fid
        , techAmount = amount
        , techUnitId = kgUnit
        , techRole = role
        , techActivityLinkId = UUID.nil
        , techProcessLinkId = Nothing
        , techLocation = "FR"
        , techComment = Nothing
        , techPedigree = Nothing
        }

reference :: UUID -> Exchange
reference fid = techExchange fid 1.0 ReferenceProduct

input :: UUID -> Double -> Exchange
input fid amount = techExchange fid amount Input

-- | A treatment activity is defined by the waste it consumes, not by an output.
referenceInput :: UUID -> Exchange
referenceInput fid = techExchange fid 1.0 ReferenceInput

{- | Database from activities keyed by (activity, product), with the standard
flow and unit registries every fixture shares.
-}
dbOf :: [((UUID, UUID), Activity)] -> SimpleDatabase
dbOf acts =
    SimpleDatabase
        { sdbActivities = M.fromList acts
        , sdbTechFlows =
            M.fromList
                [ (breadFlow, TechnosphereFlow breadFlow "bread" kgUnit M.empty Nothing Nothing)
                , (flourFlow, TechnosphereFlow flourFlow "flour" kgUnit M.empty Nothing Nothing)
                ]
        , sdbBioFlows = M.empty
        , sdbWasteFlows = M.fromList [(wasteFlow, WasteFlow wasteFlow "municipal waste" kgUnit M.empty Nothing Nothing)]
        , sdbUnits = M.singleton kgUnit (Unit kgUnit "kg" "kg" "")
        }

-- | The report of a one-activity database, for the many single-defect cases.
reportOf :: Activity -> QualityReport
reportOf act = qualityReport "testdb" (dbOf [((actA, prodA), act)])

details :: QualityCheck -> [Text]
details = map qoDetail . qcOffenders

severities :: QualityCheck -> [Severity]
severities = map qoSeverity . qcOffenders

-- | An activity carrying an allocation percentage and a source block identity.
allocated :: Text -> Maybe Double -> Maybe Text -> Activity
allocated name percent nativeId =
    (mkActivity name [reference breadFlow])
        { activityAllocationPercent = percent
        , activityNativeId = NativeProcessId <$> nativeId
        }

spec :: Spec
spec = do
    describe "reference product check" $ do
        it "flags an activity with no reference exchange" $ do
            let check = qrReferenceProduct (reportOf (mkActivity "no reference" [input flourFlow 1.0]))
            details check `shouldBe` ["0 reference exchanges instead of exactly one"]
            severities check `shouldBe` [DangerSev]

        it "flags an activity with two reference exchanges" $ do
            let check = qrReferenceProduct (reportOf (mkActivity "two references" [reference breadFlow, reference flourFlow]))
            details check `shouldBe` ["2 reference exchanges instead of exactly one"]

        it "passes an activity with exactly one reference exchange" $
            qcOffenders (qrReferenceProduct (reportOf (mkActivity "bread" [reference breadFlow, input flourFlow 1.0])))
                `shouldBe` []

        it "passes a treatment activity defined by its reference input" $
            qcOffenders (qrReferenceProduct (reportOf (mkActivity "waste treatment" [referenceInput wasteFlow])))
                `shouldBe` []

    describe "allocation sums check" $ do
        it "passes coproducts of one block summing to 100%" $ do
            let db = dbOf [((actA, prodA), allocated "block" (Just 60) (Just "b1")), ((actA, prodB), allocated "block" (Just 40) (Just "b1"))]
            qcOffenders (qrAllocationSums (qualityReport "testdb" db)) `shouldBe` []

        it "flags coproducts summing to 90%, naming the sum" $ do
            let db = dbOf [((actA, prodA), allocated "block" (Just 60) (Just "b1")), ((actA, prodB), allocated "block" (Just 30) (Just "b1"))]
                check = qrAllocationSums (qualityReport "testdb" db)
            details check `shouldBe` ["allocation sums to 90.00% across 2 coproduct(s)"]
            severities check `shouldBe` [DangerSev]

        it "rounds the reported sum to two decimals, free of floating-point dust" $ do
            -- 33.3 + 33.3 + 3.3 is 69.89999999999999 as a double; the message
            -- must not say so, and the judgement still flags it.
            let db =
                    dbOf
                        [ ((actA, prodA), allocated "block" (Just 33.3) (Just "b1"))
                        , ((actA, prodB), allocated "block" (Just 33.3) (Just "b1"))
                        , ((actA, u "1c"), allocated "block" (Just 3.3) (Just "b1"))
                        ]
            details (qrAllocationSums (qualityReport "testdb" db))
                `shouldBe` ["allocation sums to 69.90% across 3 coproduct(s)"]

        it "tolerates source rounding (33.3 + 33.3 + 33.4)" $ do
            let db =
                    dbOf
                        [ ((actA, prodA), allocated "block" (Just 33.3) (Just "b1"))
                        , ((actA, prodB), allocated "block" (Just 33.3) (Just "b1"))
                        , ((actA, u "1c"), allocated "block" (Just 33.4) (Just "b1"))
                        ]
            qcOffenders (qrAllocationSums (qualityReport "testdb" db)) `shouldBe` []

        it "flags a NaN percentage, which every comparison would otherwise pass" $ do
            let db = dbOf [((actA, prodA), allocated "block" (Just (0 / 0)) (Just "b1"))]
            length (qcOffenders (qrAllocationSums (qualityReport "testdb" db))) `shouldBe` 1

        it "keeps same-named blocks with distinct identifiers apart" $ do
            -- Both blocks are internally complete at 100%. Merging them by name
            -- would sum to 200% and invent two findings.
            let db = dbOf [((actA, prodA), allocated "truncated name" (Just 100) (Just "b1")), ((actB, prodB), allocated "truncated name" (Just 100) (Just "b2"))]
            qcOffenders (qrAllocationSums (qualityReport "testdb" db)) `shouldBe` []

        it "flags a block where only some coproducts carry a percentage, without judging its sum" $ do
            -- The 60% alone is neither a good sum nor a bad one — the missing
            -- percentage is the defect, so it gets its own warning instead of
            -- a misdiagnosed danger.
            let db = dbOf [((actA, prodA), allocated "block" (Just 60) (Just "b1")), ((actA, prodB), allocated "block" Nothing (Just "b1"))]
                check = qrAllocationSums (qualityReport "testdb" db)
            details check `shouldBe` ["1 of 2 coproduct(s) carry no allocation percentage"]
            severities check `shouldBe` [WarningSev]

        it "leaves a block with no percentages at all alone" $ do
            let db = dbOf [((actA, prodA), allocated "block" Nothing (Just "b1")), ((actA, prodB), allocated "block" Nothing (Just "b1"))]
            qcOffenders (qrAllocationSums (qualityReport "testdb" db)) `shouldBe` []

        it "reports not-applicable on a database without allocation data" $ do
            let check = qrAllocationSums (reportOf (mkActivity "bread" [reference breadFlow]))
            qcApplicable check `shouldBe` False
            qcOffenders check `shouldBe` []

        it "reports applicable as soon as one entry carries a percentage" $
            qcApplicable (qrAllocationSums (reportOf (allocated "block" (Just 100) (Just "b1")))) `shouldBe` True

    describe "duplicate activities check" $ do
        it "flags the same name, location and reference product under distinct keys" $ do
            let db = dbOf [((actA, prodA), mkActivity "bread" [reference breadFlow]), ((actB, prodB), mkActivity "bread" [reference breadFlow])]
                check = qrDuplicateActivities (qualityReport "testdb" db)
            details check `shouldBe` ["2 identical entries (same name, location and reference product)"]
            map qoProductName (qcOffenders check) `shouldBe` [Just "bread"]
            severities check `shouldBe` [WarningSev]

        it "passes the same name with a different reference product" $ do
            let db = dbOf [((actA, prodA), mkActivity "bakery" [reference breadFlow]), ((actB, prodB), mkActivity "bakery" [reference flourFlow])]
            qcOffenders (qrDuplicateActivities (qualityReport "testdb" db)) `shouldBe` []

        it "skips entries whose reference is broken, leaving them to the reference check" $ do
            let db = dbOf [((actA, prodA), mkActivity "bread" []), ((actB, prodB), mkActivity "bread" [])]
            qcOffenders (qrDuplicateActivities (qualityReport "testdb" db)) `shouldBe` []

    describe "suspicious amounts check" $ do
        it "flags a non-finite amount" $ do
            let check = qrSuspiciousAmounts (reportOf (mkActivity "bread" [reference breadFlow, input flourFlow (0 / 0)]))
            details check `shouldBe` ["exchange \"flour\" has a non-finite amount"]
            severities check `shouldBe` [DangerSev]

        it "flags an infinite amount" $
            length (qcOffenders (qrSuspiciousAmounts (reportOf (mkActivity "bread" [reference breadFlow, input flourFlow (1 / 0)]))))
                `shouldBe` 1

        it "flags a zero reference amount, which normalization divides by" $ do
            let check = qrSuspiciousAmounts (reportOf (mkActivity "bread" [techExchange breadFlow 0 ReferenceProduct]))
            details check `shouldBe` ["reference exchange \"bread\" has amount 0, which normalization would divide by"]

        it "passes an ordinary input at zero" $
            qcOffenders (qrSuspiciousAmounts (reportOf (mkActivity "bread" [reference breadFlow, input flourFlow 0])))
                `shouldBe` []

    describe "missing metadata check" $ do
        it "flags an empty description as info" $ do
            let check = qrMissingMetadata (reportOf (mkActivity "bread" [reference breadFlow]){activityDescription = []})
            details check `shouldBe` ["no description"]
            severities check `shouldBe` [InfoSev]

        it "flags a description of blank paragraphs" $
            details (qrMissingMetadata (reportOf (mkActivity "bread" [reference breadFlow]){activityDescription = ["", "   "]}))
                `shouldBe` ["no description"]

        it "flags a missing classification as info" $
            details (qrMissingMetadata (reportOf (mkActivity "bread" [reference breadFlow]){activityClassification = M.empty}))
                `shouldBe` ["no classification"]

        it "flags a missing location as warning" $ do
            let check = qrMissingMetadata (reportOf (mkActivity "bread" [reference breadFlow]){activityLocation = ""})
            details check `shouldBe` ["no location"]
            severities check `shouldBe` [WarningSev]

        it "flags exchanges whose unit is absent from the registry, with a count" $ do
            let act = mkActivity "bread" [reference breadFlow, (input flourFlow 1.0){techUnitId = unknownUnit}]
                check = qrMissingMetadata (reportOf act)
            details check `shouldBe` ["1 exchange(s) whose unit is absent from the unit registry"]
            severities check `shouldBe` [WarningSev]

        it "passes a fully documented activity" $
            qcOffenders (qrMissingMetadata (reportOf (mkActivity "bread" [reference breadFlow]))) `shouldBe` []

    describe "formula consistency check" $ do
        it "flags an activity whose formulas diverge, with counts and the example" $ do
            let fc = FormulaCheck{fcChecked = 30, fcDivergent = 12, fcUnevaluable = 3, fcExample = Just "\"a*2\" evaluates to 5.0 but the dataset stores 4.0"}
                check = qrFormulaConsistency (reportOf ((mkActivity "bread" [reference breadFlow]){activityFormulaCheck = Just fc}))
            details check
                `shouldBe` ["12 of 30 evaluable formula(s) disagree with the stored amount (e.g. \"a*2\" evaluates to 5.0 but the dataset stores 4.0); 3 more could not be evaluated"]
            severities check `shouldBe` [InfoSev]

        it "passes an activity whose formulas only failed to evaluate" $ do
            let fc = FormulaCheck{fcChecked = 0, fcDivergent = 0, fcUnevaluable = 7, fcExample = Nothing}
                check = qrFormulaConsistency (reportOf ((mkActivity "bread" [reference breadFlow]){activityFormulaCheck = Just fc}))
            qcOffenders check `shouldBe` []
            qcApplicable check `shouldBe` True

        it "is not applicable to a database without any formula" $
            qcApplicable (qrFormulaConsistency (reportOf (mkActivity "bread" [reference breadFlow])))
                `shouldBe` False

    describe "report header" $
        it "counts one process per (activity, product) entry" $ do
            let db = dbOf [((actA, prodA), mkActivity "bread" [reference breadFlow]), ((actB, prodB), mkActivity "cake" [reference flourFlow])]
            qrProcessCount (qualityReport "testdb" db) `shouldBe` 2

    describe "wire projection" $ do
        it "caps the offender list but keeps the full count" $ do
            let act = (mkActivity "bread" [reference breadFlow]){activityDescription = [], activityClassification = M.empty}
                check = qraMissingMetadata (qualityReportToAPI (Just 1) (reportOf act))
            qcaOffenderCount check `shouldBe` 2
            length (qcaOffenders check) `shouldBe` 1

        it "returns every offender when no limit is given" $ do
            let act = (mkActivity "bread" [reference breadFlow]){activityDescription = [], activityClassification = M.empty}
            length (qcaOffenders (qraMissingMetadata (qualityReportToAPI Nothing (reportOf act)))) `shouldBe` 2

        it "orders findings worst-first, so a cap keeps the worst" $ do
            -- Missing location (warning) must outrank missing description (info).
            let act = (mkActivity "bread" [reference breadFlow]){activityDescription = [], activityLocation = ""}
            map qoaSeverity (qcaOffenders (qraMissingMetadata (qualityReportToAPI Nothing (reportOf act))))
                `shouldBe` [WarningSev, InfoSev]

        it "carries applicable through to the wire" $
            qcaApplicable (qraAllocationSums (qualityReportToAPI Nothing (reportOf (mkActivity "bread" [reference breadFlow]))))
                `shouldBe` False

    describe "severity wire codes" $ do
        it "round-trips every severity through JSON" $
            map (decode . encode) [DangerSev, WarningSev, InfoSev]
                `shouldBe` [Just DangerSev, Just WarningSev, Just InfoSev]

        it "encodes as stable lowercase codes, not constructor names" $
            encode [DangerSev, WarningSev, InfoSev] `shouldBe` "[\"danger\",\"warning\",\"info\"]"

        it "rejects an unknown code rather than defaulting" $
            (decode "\"bogus\"" :: Maybe Severity) `shouldBe` Nothing
