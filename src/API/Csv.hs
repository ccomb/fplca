{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The @text/csv@ representation of the quality reports.

One report, two representations: the JSON route answers a program, this one
answers a spreadsheet. Every surface that offers the report as a file - the
web UI's download button, the CLI's @quality-report@ command, a plain @curl@ -
reads this module, so the columns and the quoting have one definition instead
of one per client.

The two reports share a row shape, so they share an encoder: one row per
finding, the name of the check that raised it in the first column.
-}
module API.Csv (
    CSV,
    qualityReportCsv,
    computedQualityReportCsv,
) where

import API.Types (ComputedQualityReportAPI (..), QualityCheckAPI (..), QualityOffenderAPI (..), QualityReportAPI (..))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Media ((//), (/:))
import Servant.API (Accept (..), MimeRender (..))
import Types (severityCode)

-- | The @text/csv@ content type.
data CSV

instance Accept CSV where
    contentType _ = "text" // "csv" /: ("charset", "utf-8")

instance MimeRender CSV QualityReportAPI where
    mimeRender _ = qualityReportCsv

instance MimeRender CSV ComputedQualityReportAPI where
    mimeRender _ = computedQualityReportCsv

{- | The structural report as CSV, one row per finding, checks in report order.
A check is named by its own field name, so a row says which check raised it in
the same word the JSON uses.
-}
qualityReportCsv :: QualityReportAPI -> BL.ByteString
qualityReportCsv r =
    checksCsv
        [ ("reference_product", qraReferenceProduct r)
        , ("allocation_sums", qraAllocationSums r)
        , ("duplicate_activities", qraDuplicateActivities r)
        , ("suspicious_amounts", qraSuspiciousAmounts r)
        , ("missing_metadata", qraMissingMetadata r)
        , ("undeclared_geography", qraUndeclaredGeography r)
        , ("formula_consistency", qraFormulaConsistency r)
        , ("truncated_name_collisions", qraTruncatedNameCollisions r)
        , ("missing_pedigree", qraMissingPedigree r)
        , ("unconsumed_products", qraUnconsumedProducts r)
        , ("land_transformation_balance", qraLandTransformationBalance r)
        , ("oxygen_demand_order", qraOxygenDemandOrder r)
        , ("invalid_cas", qraInvalidCas r)
        , ("allocation_out_of_range", qraAllocationOutOfRange r)
        , ("unmeasurable_amounts", qraUnmeasurableAmounts r)
        ]

-- | The computed report as CSV: same columns, same guards, its own checks.
computedQualityReportCsv :: ComputedQualityReportAPI -> BL.ByteString
computedQualityReportCsv r =
    checksCsv
        [ ("score_outliers", cqaScoreOutliers r)
        , ("zero_scores", cqaZeroScores r)
        , ("negative_scores", cqaNegativeScores r)
        ]

{- | The findings of a report as rows, checks in report order. A check with
nothing to report contributes no row; the header is always written, so a
database with nothing wrong yields a header and no rows rather than an empty
file the caller has to guess about.
-}
checksCsv :: [(Text, QualityCheckAPI)] -> BL.ByteString
checksCsv namedChecks =
    Csv.encode (header : concatMap checkRows namedChecks)
  where
    header = ["check", "severity", "activity_name", "location", "product_name", "detail", "process_id"]
    checkRows (name, check) = map (offenderRow name) (qcaOffenders check)
    offenderRow name o =
        [ name
        , severityCode (qoaSeverity o)
        , spreadsheetSafe (qoaActivityName o)
        , qoaLocation o
        , spreadsheetSafe (fromMaybe "" (qoaProductName o))
        , spreadsheetSafe (qoaDetail o)
        , qoaProcessId o
        ]

{- | A cell opening with @=@, @+@, @-@ or @\@@ is read as a formula by
spreadsheets, and an allocation detail legitimately opens with a minus sign.
A leading space keeps it text. Applied to the cells carrying database content,
never to the whole row: quoting must not alter data.
-}
spreadsheetSafe :: Text -> Text
spreadsheetSafe t
    | any (`T.isPrefixOf` t) ["=", "+", "-", "@"] = T.cons ' ' t
    | otherwise = t
