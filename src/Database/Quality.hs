{-# LANGUAGE OverloadedStrings #-}

{- | Dataset-soundness checks, for the people who build or repair databases.

A score tells you whether a database computes; it says nothing about whether
the dataset is well formed. These checks look for the structural defects a
score can't reveal: processes without exactly one reference exchange,
coproduct allocation that doesn't sum to 100%, entries duplicated outright,
amounts that aren't finite, missing metadata, and stored amounts that
disagree with the formulas documenting them.

Every check is a pure scan over a 'SimpleDatabase', so the report is
identical on a staged database (parsed, matrices not built) and on a loaded
one — a maker can read it before committing to a build.
-}
module Database.Quality (
    QualityOffender (..),
    QualityCheck (..),
    QualityReport (..),
    qualityReport,
    qualityChecks,
    allocationTolerance,
) where

import Control.Applicative ((<|>))
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Semigroup (Min (..), Sum (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Numeric (showFFloat)

import Types (
    Activity (..),
    BiosphereFlow (..),
    FormulaCheck (..),
    Severity (..),
    SimpleDatabase (..),
    TechnosphereFlow (..),
    WasteFlow (..),
    activityGroupKey,
    exchangeAmount,
    exchangeFlowId,
    exchangeIsReference,
    exchangeUnitId,
 )

-- | One finding: the activity it was found on, and what is wrong with it.
data QualityOffender = QualityOffender
    { qoSeverity :: !Severity
    , qoProcessId :: !Text
    {- ^ Canonical @activityUUID_productUUID@ address of the entry the finding
    anchors to, so a consumer can navigate to it. A finding that covers
    several entries (duplicates) names one of them.
    -}
    , qoActivityName :: !Text
    , qoLocation :: !Text
    , qoProductName :: !(Maybe Text)
    -- ^ Reference product, when the check knows which one it means
    , qoDetail :: !Text
    -- ^ Human-readable specifics, e.g. @"3 reference exchanges instead of exactly one"@
    }
    deriving (Show, Eq)

{- | The outcome of one check. Offenders are sorted worst-first and complete —
capping for display happens at the wire boundary, not here.
-}
data QualityCheck = QualityCheck
    { qcApplicable :: !Bool
    {- ^ 'False' when the database carries nothing this check could judge, which
    is not the same as passing. See the allocation check in 'qualityReport'.
    -}
    , qcOffenders :: ![QualityOffender]
    }
    deriving (Show, Eq)

{- | Soundness report of a database: one named field per check. Named fields
rather than a list keyed by codes — the field name /is/ the check's identity,
so neither this module nor its consumers can misspell one.
-}
data QualityReport = QualityReport
    { qrDbName :: !Text
    , qrProcessCount :: !Int
    -- ^ Entries scanned: one per (activity, product) pair
    , qrReferenceProduct :: !QualityCheck
    , qrAllocationSums :: !QualityCheck
    , qrDuplicateActivities :: !QualityCheck
    , qrSuspiciousAmounts :: !QualityCheck
    , qrMissingMetadata :: !QualityCheck
    , qrFormulaConsistency :: !QualityCheck
    }
    deriving (Show, Eq)

{- | Every check of a report, in report order. For consumers that treat the
checks uniformly (counting findings, rendering a list) without losing the
per-field identity above.
-}
qualityChecks :: QualityReport -> [QualityCheck]
qualityChecks r =
    [ qrReferenceProduct r
    , qrAllocationSums r
    , qrDuplicateActivities r
    , qrSuspiciousAmounts r
    , qrMissingMetadata r
    , qrFormulaConsistency r
    ]

{- | Allowed drift when summing coproduct allocation percentages. Sources round
their percentages (33.3 + 33.3 + 33.4), so an exact comparison would flag
correct data; half a point is far below what a dropped or mistyped coproduct
costs.
-}
allocationTolerance :: Double
allocationTolerance = 0.5

{- | Two-decimal rendering for detail texts. The judgement uses the exact
double; only the message is rounded, so a drifting sum reads as @69.90@ rather
than as floating-point dust like @69.89999999999999@.
-}
formatPercent :: Double -> Text
formatPercent x = T.pack (showFFloat (Just 2) x "")

-- | Run every check over a database.
qualityReport :: Text -> SimpleDatabase -> QualityReport
qualityReport dbName db =
    QualityReport
        { qrDbName = dbName
        , qrProcessCount = M.size (sdbActivities db)
        , qrReferenceProduct = QualityCheck True (worstFirst referenceOffenders)
        , qrAllocationSums = QualityCheck allocationApplicable (worstFirst allocationOffenders)
        , qrDuplicateActivities = QualityCheck True (worstFirst duplicateOffenders)
        , qrSuspiciousAmounts = QualityCheck True (worstFirst amountOffenders)
        , qrMissingMetadata = QualityCheck True (worstFirst metadataOffenders)
        , qrFormulaConsistency = QualityCheck formulaApplicable (worstFirst formulaOffenders)
        }
  where
    entries = M.toList (sdbActivities db)
    acts = map snd entries
    pidText (actUUID, prodUUID) = UUID.toText actUUID <> "_" <> UUID.toText prodUUID
    worstFirst = sortOn (\o -> (qoSeverity o, qoActivityName o))
    offender sev key act = QualityOffender sev (pidText key) (activityName act) (activityLocation act)

    -- Names of the flow an exchange points at, whichever registry holds it.
    -- An unresolved id degrades to its UUID rather than to a blank: a finding
    -- naming nothing would be unactionable.
    techOrWasteFlowName fid =
        (tfName <$> M.lookup fid (sdbTechFlows db))
            <|> (wfName <$> M.lookup fid (sdbWasteFlows db))
    anyFlowName fid =
        fromMaybe (UUID.toText fid) $
            techOrWasteFlowName fid
                <|> (bfName <$> M.lookup fid (sdbBioFlows db))

    -- Exactly one reference exchange defines the process: none leaves nothing
    -- to normalize against, several make the entry ambiguous. 'exchangeIsReference'
    -- counts a treatment activity's reference /input/ too, so waste treatment
    -- passes on its own terms.
    referenceOffenders =
        [ offender DangerSev key act Nothing $
            T.pack (show n) <> " reference exchanges instead of exactly one"
        | (key, act) <- entries
        , let n = length (filter exchangeIsReference (exchanges act))
        , n /= 1
        ]

    -- The coproducts of one source block share an 'activityGroupKey'. A block
    -- whose source format has no identifier falls back to grouping by activity
    -- UUID alone: two SimaPro blocks whose names collide after the format's
    -- 80-character truncation would then merge, and their percentages sum to
    -- ~200%. That is the same over-grouping 'Database.MatrixBuild' accepts.
    allocationApplicable = any (isJust . activityAllocationPercent) acts
    allocationGroups =
        M.fromListWith
            (<>)
            [ (activityGroupKey actUUID act, [(key, act)])
            | (key@(actUUID, _productUUID), act) <- entries
            ]
    allocationOffenders = concatMap allocationGroupOffenders (M.elems allocationGroups)

    -- A block whose every coproduct carries a percentage is judged on its sum.
    -- A block where only some do is its own defect — the sum means nothing
    -- until the missing percentages are restored, so reporting it as a bad sum
    -- would misdiagnose. A block where none do is simply unallocated: nothing
    -- to judge.
    allocationGroupOffenders group' = case group' of
        [] -> []
        (repKey, representative) : _
            | null carried -> []
            | missing > 0 ->
                [ offender WarningSev repKey representative Nothing $
                    T.pack (show missing)
                        <> " of "
                        <> T.pack (show (length group'))
                        <> " coproduct(s) carry no allocation percentage"
                ]
            -- NaN needs its own test: any comparison against it is False, so
            -- the tolerance check alone would let it through.
            | isNaN total || isInfinite total || abs (total - 100) > allocationTolerance ->
                [ offender DangerSev repKey representative Nothing $
                    "allocation sums to "
                        <> formatPercent total
                        <> "% across "
                        <> T.pack (show (length group'))
                        <> " coproduct(s)"
                ]
            | otherwise -> []
      where
        carried = mapMaybe (activityAllocationPercent . snd) group'
        missing = length group' - length carried
        total = sum carried

    -- Same name, same place, same product, twice: one of them is stale. Entries
    -- without exactly one reference are skipped — check 1 already reports them,
    -- and grouping them by a missing product would invent duplicates.
    referenceProductName act = case filter exchangeIsReference (exchanges act) of
        [ex] -> Just (fromMaybe (UUID.toText (exchangeFlowId ex)) (techOrWasteFlowName (exchangeFlowId ex)))
        _ -> Nothing
    duplicateGroups =
        M.fromListWith
            (<>)
            [ ((activityName act, activityLocation act, productName), (Min (pidText key), Sum (1 :: Int)))
            | (key, act) <- entries
            , Just productName <- [referenceProductName act]
            ]
    duplicateOffenders =
        [ QualityOffender WarningSev pid name location (Just productName) $
            T.pack (show n) <> " identical entries (same name, location and reference product)"
        | ((name, location, productName), (Min pid, Sum n)) <- M.toList duplicateGroups
        , n > 1
        ]

    -- A non-finite amount poisons every downstream sum; a zero reference amount
    -- is what normalization divides by. Zero on an ordinary input is legal and
    -- says the activity simply doesn't consume it.
    amountOffenders =
        [ offender DangerSev key act Nothing detail
        | (key, act) <- entries
        , ex <- exchanges act
        , let amount = exchangeAmount ex
        , let flowName = anyFlowName (exchangeFlowId ex)
        , Just detail <-
            [ if isNaN amount || isInfinite amount
                then Just ("exchange \"" <> flowName <> "\" has a non-finite amount")
                else
                    if exchangeIsReference ex && amount == 0
                        then Just ("reference exchange \"" <> flowName <> "\" has amount 0, which normalization would divide by")
                        else Nothing
            ]
        ]

    -- The parse-time mathematicalRelation check (EcoSpold2): formulas that
    -- re-evaluate away from the amount they document. Expected in system-model
    -- exports — allocation rescales amounts without updating the copied
    -- formulas — hence Info: the stored amounts stay authoritative, this only
    -- tells a maker where their own formulas and amounts drifted apart.
    -- Datasets whose formulas merely could not be evaluated are not findings;
    -- 'False' applicability means no dataset carried a formula at all.
    formulaApplicable = any (isJust . activityFormulaCheck) acts
    formulaOffenders =
        [ offender InfoSev key act Nothing (formulaDetail fc)
        | (key, act) <- entries
        , Just fc <- [activityFormulaCheck act]
        , fcDivergent fc > 0
        ]
    formulaDetail fc =
        T.pack (show (fcDivergent fc))
            <> " of "
            <> T.pack (show (fcEvaluated fc))
            <> " evaluable formula(s) disagree with the stored amount"
            <> maybe "" (\e -> " (e.g. " <> e <> ")") (fcExample fc)
            <> ( if fcUnevaluable fc > 0
                    then "; " <> T.pack (show (fcUnevaluable fc)) <> " more could not be evaluated"
                    else ""
               )

    -- Incomplete rather than wrong, hence Info — except a missing location or an
    -- unknown unit, which change how the entry links and converts.
    metadataOffenders =
        concat
            [ [offender InfoSev key act Nothing "the dataset carries no description" | all (T.null . T.strip) (activityDescription act)]
                <> [offender InfoSev key act Nothing "the dataset carries no classification" | M.null (activityClassification act)]
                <> [offender WarningSev key act Nothing "the dataset carries no location" | T.null (T.strip (activityLocation act))]
                <> [ offender WarningSev key act Nothing $
                        T.pack (show unknownUnits) <> " exchange(s) whose unit is absent from the unit registry"
                   | let unknownUnits = length [() | ex <- exchanges act, M.notMember (exchangeUnitId ex) (sdbUnits db)]
                   , unknownUnits > 0
                   ]
            | (key, act) <- entries
            ]
