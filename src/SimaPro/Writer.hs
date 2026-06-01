{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- | SimaPro CSV Writer for volca — the inverse of "SimaPro.Parser".

Serializes an in-memory 'Database' / 'SimpleDatabase' back to a SimaPro CSV
export. The output is /canonical/ and /deterministic/: given the same database
(modulo a pinned header version) it always produces byte-identical bytes.

Determinism is achieved by:

  * sorting every activity by (name, location) and every section's rows by
    (flow name, unit, signed amount), so 'Map'/'Set' iteration order never
    leaks into the output;
  * a single fixed numeric formatter ('formatAmount') that round-trips through
    the parser's 'parseAmount' / 'Expr.normalizeExpr';
  * pinning the only volatile header field — the SimaPro version banner — via
    'WriterConfig'. No export timestamp or generator line is emitted, so a
    write→parse→write cycle is stable.

Encoding/layout mirrors the parser's expectations:

  * semicolon-separated fields, CRLF line endings;
  * dot decimal separator (matching @{Decimal separator: .}@);
  * the @{SimaPro …}@ / @{CSV separator: Semicolon}@ / @{Decimal separator: .}@
    header block;
  * one @Process … End@ block per activity, with the metadata keys and section
    headers the parser recognises.

Field shapes are kept aligned with the parser's row parsers
('parseProductRow', 'parseTechRow', 'parseBioRow') so a faithful round-trip is
possible. CSV escaping reuses the same rule as 'Matrix.Export.escapeCsvField'.
-}
module SimaPro.Writer (
    WriterConfig (..),
    defaultWriterConfig,
    writeSimaProCSV,
    serializeSimaProCSV,
    checkSimaProExportable,

    -- * Pure helpers (exposed for testing)
    escapeField,
    formatAmount,
) where

import qualified Data.ByteString as BS
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Types

-- ============================================================================
-- Configuration
-- ============================================================================

{- | Writer knobs. The only volatile field a SimaPro export normally carries is
the version banner; pinning it here keeps a round-trip byte-stable. We
deliberately omit the export-date / generator lines entirely (the parser
ignores them anyway) so there is no timestamp to normalise away.
-}
newtype WriterConfig = WriterConfig
    { wcVersion :: Text
    -- ^ Value of the @{SimaPro …}@ banner line.
    }
    deriving (Eq, Show)

-- | Default config: a fixed, neutral version banner (no timestamp).
defaultWriterConfig :: WriterConfig
defaultWriterConfig = WriterConfig{wcVersion = "SimaPro 9.6.0.1"}

-- ============================================================================
-- Export guard
-- ============================================================================

{- | SimaPro CSV files emissions into air / water / soil sections only. An
emission whose compartment medium is some other non-empty value has no faithful
section, so report it rather than silently filing it under @Emissions to air@.
An unspecified (empty) medium is allowed — it carries no medium to lose and
follows SimaPro's air default. Resources are bucketed by direction, not medium,
so they never offend. This is the export-boundary check; 'serializeSimaProCSV'
itself stays pure and total.
-}
checkSimaProExportable :: SimpleDatabase -> Either Text ()
checkSimaProExportable db =
    case offenders of
        [] -> Right ()
        ((name, medium) : _) ->
            Left $
                "SimaPro export cannot represent emission \""
                    <> name
                    <> "\": compartment medium \""
                    <> medium
                    <> "\" is not one of air, water, soil."
  where
    offenders =
        [ (bfName flow, bfCompartmentName flow)
        | act <- M.elems (sdbActivities db)
        , ex@BiosphereExchange{bioDirection = Emission} <- exchanges act
        , Just flow <- [M.lookup (exchangeFlowId ex) (sdbBioFlows db)]
        , let medium = T.toLower (bfCompartmentName flow)
        , not (T.null medium)
        , medium `notElem` ["air", "water", "soil"]
        ]

-- ============================================================================
-- Constants
-- ============================================================================

-- | Field delimiter — semicolon, matching @{CSV separator: Semicolon}@.
delim :: Text
delim = ";"

-- | Line terminator — SimaPro exports use Windows CRLF.
crlf :: Text
crlf = "\r\n"

-- ============================================================================
-- Numeric formatting
-- ============================================================================

{- | Canonical numeric formatter. Produces a dot-decimal literal that the
parser's 'parseAmount' / 'Expr.normalizeExpr' read back to the same 'Double'.

Integral values are written without a trailing @.0@ (e.g. @100@, not
@100.0@) so allocation percentages match the parser's @"100"@ raw form;
all other values use Haskell's 'show', which is dot-decimal and exact
enough to round-trip a 'Double'.
-}
formatAmount :: Double -> Text
formatAmount x
    | isNaN x || isInfinite x = "0"
    | x == fromIntegral (round x :: Integer) =
        T.pack (show (round x :: Integer))
    | otherwise = T.pack (show x)

-- ============================================================================
-- CSV escaping
-- ============================================================================

{- | Escape a field for semicolon-delimited CSV. Same rule as
'Matrix.Export.escapeCsvField': quote only when the field contains the
delimiter, a quote, or a newline, doubling embedded quotes.
-}
escapeField :: Text -> Text
escapeField text
    | T.any (\c -> c == ';' || c == '"' || c == '\n' || c == '\r') text =
        "\"" <> T.replace "\"" "\"\"" text <> "\""
    | otherwise = text

-- | Join fields with the delimiter (each escaped).
row :: [Text] -> Text
row = T.intercalate delim . map escapeField

-- ============================================================================
-- Flow / unit name resolution
-- ============================================================================

{- | Resolve a unit UUID to its name via the unit DB, falling back to the empty
string when absent (a missing unit is surfaced downstream by the matrix
builder, not silently defaulted to a wrong unit here).
-}
unitNameOf :: UnitDB -> UUID -> Text
unitNameOf units uid = maybe "" unitName (M.lookup uid units)

-- ============================================================================
-- Process block serialization
-- ============================================================================

{- | A single technosphere/biosphere/waste exchange projected to the
(name, unit, compartment, amount, comment) tuple the writer needs, plus a
sort key. Keeping this intermediate makes the per-section sorting and
formatting uniform.
-}
data Line = Line
    { lName :: !Text
    , lCompartment :: !Text -- sub-compartment for bio rows; "" otherwise
    , lUnit :: !Text
    , lAmount :: !Double
    , lComment :: !Text
    }

-- | Deterministic ordering key for a section's lines.
lineKey :: Line -> (Text, Text, Text, Double)
lineKey l = (lName l, lCompartment l, lUnit l, lAmount l)

-- | Render the comment column, re-attaching a pedigree prefix when present.
renderComment :: Maybe Pedigree -> Maybe Text -> Text
renderComment ped cmt =
    let pedTxt = maybe "" renderPedigree ped
        cmtTxt = fromMaybe "" cmt
     in case (pedTxt, cmtTxt) of
            ("", c) -> c
            (p, "") -> p <> ","
            (p, c) -> p <> "," <> c

-- | @(r,c,t,g,f)@ pedigree quintuple, matching 'parsePedigreePrefix'.
renderPedigree :: Pedigree -> Text
renderPedigree Pedigree{..} =
    "("
        <> T.intercalate
            ","
            (map (T.pack . show) [pedReliability, pedCompleteness, pedTemporal, pedGeographical, pedTechnological])
        <> ")"

{- | Build a 'Line' for a technosphere input exchange. Returns 'Nothing' for
the reference/coproduct outputs (those go to the Products section) and for
exchanges whose flow is unknown to the tech-flow DB.
-}
techInputLine :: TechFlowDB -> UnitDB -> Exchange -> Maybe Line
techInputLine techDB units ex@TechnosphereExchange{..} =
    if exchangeIsReference ex
        then Nothing
        else case M.lookup techFlowId techDB of
            Nothing -> Nothing
            Just flow ->
                Just
                    Line
                        { lName = tfName flow
                        , lCompartment = ""
                        , lUnit = unitNameOf units techUnitId
                        , lAmount = techAmount
                        , lComment = renderComment techPedigree techComment
                        }
techInputLine _ _ BiosphereExchange{} = Nothing
techInputLine _ _ WasteExchange{} = Nothing

-- | Build a 'Line' for a biosphere exchange (resource or emission).
bioLine :: BioFlowDB -> UnitDB -> Exchange -> Maybe Line
bioLine bioDB units BiosphereExchange{..} =
    case M.lookup bioFlowId bioDB of
        Nothing -> Nothing
        Just flow ->
            Just
                Line
                    { lName = bfName flow
                    , lCompartment = fromMaybe "" (bfCompartmentSub flow)
                    , lUnit = unitNameOf units bioUnitId
                    , lAmount = bioAmount
                    , lComment = renderComment bioPedigree bioComment
                    }
bioLine _ _ TechnosphereExchange{} = Nothing
bioLine _ _ WasteExchange{} = Nothing

-- | Build a 'Line' for a waste exchange (Final waste flows section).
wasteLine :: WasteFlowDB -> UnitDB -> Exchange -> Maybe Line
wasteLine wasteDB units WasteExchange{..} =
    case M.lookup waFlowId wasteDB of
        Nothing -> Nothing
        Just flow ->
            Just
                Line
                    { lName = wfName flow
                    , lCompartment = ""
                    , lUnit = unitNameOf units waUnitId
                    , lAmount = waAmount
                    , lComment = renderComment waPedigree waComment
                    }
wasteLine _ _ TechnosphereExchange{} = Nothing
wasteLine _ _ BiosphereExchange{} = Nothing

{- | The medium a biosphere exchange belongs to, derived from the flow's
compartment name. SimaPro splits biosphere rows into four sections keyed on
the medium: Resources, Emissions to air/water/soil. Resources are the
'Resource' direction; emissions are bucketed by compartment name.
-}
bioSection :: BioFlowDB -> Exchange -> Maybe BioSec
bioSection bioDB ex@BiosphereExchange{bioDirection = dir} =
    case dir of
        Resource -> Just SecRes
        Emission -> case M.lookup (exchangeFlowId ex) bioDB of
            Nothing -> Just SecAir -- unknown medium → air (parser default-ish); flow itself dropped anyway
            Just flow -> case T.toLower (bfCompartmentName flow) of
                "water" -> Just SecWater
                "soil" -> Just SecSoil
                "air" -> Just SecAir
                "" -> Just SecAir
                -- Any other medium has no faithful SimaPro section.
                -- 'checkSimaProExportable' rejects it at the export boundary, so
                -- this air fallback only ever fires for a direct (un-guarded) caller.
                _ -> Just SecAir
bioSection _ TechnosphereExchange{} = Nothing
bioSection _ WasteExchange{} = Nothing

data BioSec = SecRes | SecAir | SecWater | SecSoil
    deriving (Eq)

{- | Output rows (@name;unit;amount;allocation;waste_type;category;comment@) for
the technosphere outputs matching @keep@. Used for both the @Products@ section
(references) and the @Avoided products@ section (coproducts) — the two are
rendered identically but the parser routes them to different exchange roles, so
they must be emitted under their own headers, never merged.
-}
productLines :: (Exchange -> Bool) -> TechFlowDB -> UnitDB -> Maybe Double -> Text -> [Exchange] -> [Text]
productLines keep techDB units allocPct category exchs =
    let alloc = formatAmount (fromMaybe 100 allocPct)
        -- Extract the row fields in the comprehension, where the exchange is
        -- known to be a TechnosphereExchange — so mkRow is total (no unreachable
        -- blank-row arm). Sort by (name, unit, amount) for determinism.
        entries =
            [ (tfName flow, unitNameOf units (exchangeUnitId ex), exchangeAmount ex)
            | ex@TechnosphereExchange{} <- exchs
            , keep ex
            , Just flow <- [M.lookup (exchangeFlowId ex) techDB]
            ]
        mkRow (nm, unit, amt) = row [nm, unit, formatAmount amt, alloc, "not defined", category, ""]
     in map mkRow (sortOn id entries)

-- | A coproduct technosphere output (SimaPro @Avoided products@ section, which
-- the parser reads back as a 'Coproduct' role).
isCoproduct :: Exchange -> Bool
isCoproduct ex = case ex of
    TechnosphereExchange{techRole = Coproduct} -> True
    TechnosphereExchange{} -> False
    BiosphereExchange{} -> False
    WasteExchange{} -> False

{- | Prepend the @Avoided products@ header to coproduct rows, or emit nothing
when there are none (an empty section would be noise the parser ignores).
-}
avoidedHeader :: [Text] -> [Text]
avoidedHeader [] = []
avoidedHeader rows = "Avoided products" : rows

{- | Render one section: a header line followed by its sorted rows. Emits
nothing when there are no rows, matching the parser (it tolerates absent
sections).
-}
section :: Text -> (Line -> Text) -> [Line] -> [Text]
section _ _ [] = []
section header render ls = header : map render (sortOn lineKey ls)

-- | Render a technosphere input row: name;unit;amount;Undefined;0;0;comment
techRowText :: Line -> Text
techRowText Line{..} =
    row [lName, lUnit, formatAmount lAmount, "Undefined", "0", "0", lComment]

-- | Render a biosphere row: name;compartment;unit;amount;Undefined;0;0;comment
bioRowText :: Line -> Text
bioRowText Line{..} =
    row [lName, lCompartment, lUnit, formatAmount lAmount, "Undefined", "0", "0", lComment]

{- | Serialize a single activity to a @Process … End@ block (list of lines,
without trailing terminator). Flow/unit names are resolved through the
respective DBs; exchanges whose flow is missing are dropped (the matrix
builder is the authority on unknown flows, not the serializer).
-}
serializeActivity ::
    TechFlowDB ->
    BioFlowDB ->
    WasteFlowDB ->
    UnitDB ->
    Activity ->
    [Text]
serializeActivity techDB bioDB wasteDB units Activity{..} =
    let catType = M.findWithDefault "" "Category type" activityClassification
        category = M.findWithDefault "" "Category" activityClassification
        -- No native type → omit the Type line entirely (meta drops empty values),
        -- so a re-parse yields Nothing again rather than drifting to "Unit process".
        typeLabel = case activityNativeType of
            Just (SimaProProcessType lbl) -> lbl
            Just (EcoSpoldActivityType{eatLabel = lbl}) -> lbl
            Just (ILCDProcessType lbl) -> lbl
            Nothing -> ""
        comment = T.intercalate " " activityDescription

        techLines = mapMaybe (techInputLine techDB units) exchanges
        bioByName name = [l | (sec, l) <- bioPaired, sec == name]
        bioPaired =
            [ (sec, l)
            | ex@BiosphereExchange{} <- exchanges
            , Just sec <- [bioSection bioDB ex]
            , Just l <- [bioLine bioDB units ex]
            ]
        wasteLines = mapMaybe (wasteLine wasteDB units) exchanges

        meta key val = if T.null val then [] else [key, val, ""]
     in concat
            [ ["Process", ""]
            , meta "Category type" catType
            , meta "Process name" activityName
            , meta "Type" typeLabel
            , meta "Geography" activityLocation
            , meta "Comment" comment
            , -- Products section is always present (an activity has a reference).
              -- Coproducts go to "Avoided products" so the parser reads them back
              -- as coproducts, not as extra reference-product activities.
              "Products" : productLines exchangeIsReference techDB units activityAllocationPercent category exchanges
            , [""]
            , withBlank (avoidedHeader (productLines isCoproduct techDB units activityAllocationPercent category exchanges))
            , -- Inputs.
              withBlank (section "Materials/fuels" techRowText techLines)
            , withBlank (section "Resources" bioRowText (bioByName SecRes))
            , withBlank (section "Emissions to air" bioRowText (bioByName SecAir))
            , withBlank (section "Emissions to water" bioRowText (bioByName SecWater))
            , withBlank (section "Emissions to soil" bioRowText (bioByName SecSoil))
            , withBlank (section "Final waste flows" bioRowText wasteLines)
            , ["End", ""]
            ]
  where
    -- Append a blank separator line after a non-empty section.
    withBlank [] = []
    withBlank ls = ls ++ [""]

-- ============================================================================
-- Header
-- ============================================================================

-- | The fixed SimaPro header block. No timestamp / generator line is emitted.
headerLines :: WriterConfig -> [Text]
headerLines cfg =
    [ "{" <> wcVersion cfg <> "}"
    , "{CSV separator: Semicolon}"
    , "{Decimal separator: .}"
    , "{Date separator: /}"
    , "{Short date format: dd/MM/yyyy}"
    , ""
    ]

-- ============================================================================
-- Top-level serialization
-- ============================================================================

{- | Serialize a 'SimpleDatabase' to canonical SimaPro CSV bytes (UTF-8, CRLF).
Activities are sorted by (name, location) so the byte stream is independent
of the underlying 'Map' iteration order.
-}
serializeSimaProCSV :: WriterConfig -> SimpleDatabase -> BS.ByteString
serializeSimaProCSV cfg SimpleDatabase{..} =
    let acts = sortOn (\a -> (activityName a, activityLocation a)) (M.elems sdbActivities)
        blocks = concatMap (serializeActivity sdbTechFlows sdbBioFlows sdbWasteFlows sdbUnits) acts
        allLines = headerLines cfg ++ blocks
     in TE.encodeUtf8 (T.intercalate crlf allLines <> crlf)

-- | Write canonical SimaPro CSV bytes to a file.
writeSimaProCSV :: WriterConfig -> FilePath -> SimpleDatabase -> IO ()
writeSimaProCSV cfg path = BS.writeFile path . serializeSimaProCSV cfg
