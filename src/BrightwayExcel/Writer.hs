{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Writer for the Brightway Excel (@.xlsx@) inventory interchange format — the
inverse of "BrightwayExcel.Parser".

It serializes a 'SimpleDatabase' (the natural writer input, obtained from a
'Database' via 'toSimpleDatabase') back into the linear block-stream layout that
@bw2io@'s @ExcelImporter@ — and our own parser — consumes:

@
Database              <database name>

Activity              <activity name>
production amount     1
reference product     <product>
location              GLO
unit                  kilogram
Exchanges
name   amount   reference product   location   unit   categories   type           database
...    1        <product>           GLO        kg                  production     <db>
...    0.5      <supplier product>  RoW        kg                  technosphere   <db>
Water  1.6e-4                       GLO        m3     air           biosphere      <db>

@

== Determinism

The output is canonical and deterministic: activities are emitted sorted by
@(name, location)@, exchanges in a fixed role order (reference product, then
coproducts, then technosphere inputs, then biosphere flows — each group sorted
by flow name), and a single fixed @Exchanges@ column order is used regardless of
how the source file was laid out. Numbers are rendered with 'formatAmount' so
@1.0@ and @8.5@ are stable. The only volatile field — the workbook-level
database name — is supplied explicitly via 'WriterConfig', so a round-trip never
depends on ambient state (timestamps, tool version, machine).

== Encoding

An @.xlsx@ is a zip of XML parts. We mirror the parser's toolchain exactly
(@zip-archive@ + hand-built SpreadsheetML, the same shape openpyxl/bw2io emit),
rather than pulling in a new dependency: one worksheet @xl/worksheets/sheet1.xml@
of inline-string / numeric cells, wired through @xl/workbook.xml@ and its rels.
Inline strings (@t="inlineStr"@) are used throughout, so no shared-string table
is needed — and the parser already resolves either form.

Byte-identical round-trips are not a goal (zip stores per-entry metadata): the
contract proven by the spec is /logical-cell/ idempotence — re-exporting the
parsed content yields the same workbook, and 'parseBrightwayExcel' of the output
is structurally equal to the input.
-}
module BrightwayExcel.Writer (
    WriterConfig (..),
    defaultWriterConfig,
    writeBrightwayExcel,
    renderWorkbook,

    -- * Exposed for testing
    Cell (..),
    activityRows,
    formatAmount,
    renderCategories,
) where

import Codec.Archive.Zip (addEntryToArchive, emptyArchive, fromArchive, toEntry)
import qualified Data.ByteString.Lazy as BL
import Data.Char (chr, ord)
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Types

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

{- | The only export-time choice that is not derived from the database content:
the workbook-level @Database@ name written at the top of the sheet and into each
exchange's @database@ column. Pinning it here (rather than reading a clock or a
build version) is what makes round-trips reproducible.
-}
newtype WriterConfig = WriterConfig
    { wcDatabaseName :: Text
    }
    deriving (Eq, Show)

-- | A neutral default name for ad-hoc exports.
defaultWriterConfig :: WriterConfig
defaultWriterConfig = WriterConfig{wcDatabaseName = "exported-database"}

-- ---------------------------------------------------------------------------
-- Cell model (mirrors the parser's CellValue / the spec emitter)
-- ---------------------------------------------------------------------------

-- | A cell to emit: text, a number, or an empty (omitted) cell.
data Cell
    = CText !Text
    | CNum !Double
    | CEmpty
    deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Top-level
-- ---------------------------------------------------------------------------

{- | Serialize a 'SimpleDatabase' to a Brightway @.xlsx@ workbook on disk. Pure
construction ('renderWorkbook') wrapped in a single write effect.
-}
writeBrightwayExcel :: WriterConfig -> SimpleDatabase -> FilePath -> IO ()
writeBrightwayExcel cfg db = flip BL.writeFile (renderWorkbook cfg db)

-- | Pure: assemble the full @.xlsx@ byte stream from the database.
renderWorkbook :: WriterConfig -> SimpleDatabase -> BL.ByteString
renderWorkbook cfg db =
    fromArchive $
        foldr
            addEntryToArchive
            emptyArchive
            [ toEntry "xl/workbook.xml" 0 (enc workbookXml)
            , toEntry "xl/_rels/workbook.xml.rels" 0 (enc relsXml)
            , toEntry "xl/worksheets/sheet1.xml" 0 (enc (sheetXml (sheetRows cfg db)))
            ]
  where
    enc = BL.fromStrict . TE.encodeUtf8

-- ---------------------------------------------------------------------------
-- Sheet content (pure)
-- ---------------------------------------------------------------------------

{- | The whole worksheet as a list of rows. A leading @Database@ section, then
one activity block per activity, separated by blank rows. Activities are sorted
by @(name, location)@ for determinism.
-}
sheetRows :: WriterConfig -> SimpleDatabase -> [[Cell]]
sheetRows cfg db =
    [ [CText "Database", CText (wcDatabaseName cfg)]
    , []
    ]
        ++ concatMap (\a -> activityRows cfg db a ++ [[]]) sortedActivities
  where
    sortedActivities =
        sortOn (\a -> (activityName a, activityLocation a)) (M.elems (sdbActivities db))

-- | The fixed canonical @Exchanges@ table header.
exchangeHeader :: [Cell]
exchangeHeader =
    map
        CText
        ["name", "amount", "reference product", "location", "unit", "categories", "type", "database"]

{- | One activity block: the @Activity@ row, metadata key/value rows, the
@Exchanges@ header, then one row per exchange in canonical order. The metadata
@unit@ / @reference product@ are taken from the activity's reference exchange so
a parse → write round-trip reproduces them.
-}
activityRows :: WriterConfig -> SimpleDatabase -> Activity -> [[Cell]]
activityRows cfg db act =
    [ [CText "Activity", CText (activityName act)]
    , [CText "production amount", CNum 1]
    ]
        ++ [[CText "reference product", CText p] | p <- maybeToList refProduct]
        ++ [[CText "location", CText (activityLocation act)]]
        ++ [[CText "unit", CText u] | u <- maybeToList refUnit, not (T.null u)]
        ++ [[CText "comment", CText c] | c <- take 1 (activityDescription act)]
        ++ [[CText "Exchanges"], exchangeHeader]
        ++ exchangeDataRows
  where
    ordered = orderedExchanges (exchanges act)
    exchangeDataRows = mapMaybe (exchangeRow cfg db) ordered
    refExchange = lookup' exchangeIsReference ordered
    refProduct = refExchange >>= flowNameOf db
    refUnit = (`unitNameOf` db) . exchangeUnitId =<< refExchange

{- | Canonical exchange order: reference product first, then coproducts, then
ordinary technosphere inputs, then biosphere flows — each group sorted by flow
name. This is what makes two databases with the same content serialize byte-for
-byte identically.
-}
orderedExchanges :: [Exchange] -> [Exchange]
orderedExchanges exs = concatMap (sortOn sortKey) groups
  where
    groups = [refs, coproducts, techInputs, bios, wastes]
    refs = filter exchangeIsReference exs
    coproducts = filter isCoproduct exs
    techInputs = filter isTechInput exs
    bios = filter isBio exs
    wastes = filter isWaste exs
    sortKey = exchangeFlowId

isCoproduct :: Exchange -> Bool
isCoproduct = \case
    TechnosphereExchange{techRole = Coproduct} -> True
    TechnosphereExchange{} -> False
    BiosphereExchange{} -> False
    WasteExchange{} -> False

isTechInput :: Exchange -> Bool
isTechInput = \case
    TechnosphereExchange{techRole = Input} -> True
    TechnosphereExchange{techRole = ReferenceInput} -> True
    TechnosphereExchange{} -> False
    BiosphereExchange{} -> False
    WasteExchange{} -> False

isBio :: Exchange -> Bool
isBio = \case
    BiosphereExchange{} -> True
    TechnosphereExchange{} -> False
    WasteExchange{} -> False

isWaste :: Exchange -> Bool
isWaste = \case
    WasteExchange{} -> True
    TechnosphereExchange{} -> False
    BiosphereExchange{} -> False

{- | Render one exchange to a data row aligned to 'exchangeHeader'. Returns
'Nothing' for an exchange whose flow or unit is missing from the database (it
cannot be written faithfully, so it is dropped rather than emitted with blanks
that would re-parse into a different flow). The @name@ and @reference product@
columns both carry the flow name so the parser reconstructs the same flow UUID.
-}
exchangeRow :: WriterConfig -> SimpleDatabase -> Exchange -> Maybe [Cell]
exchangeRow cfg db = \case
    ex@TechnosphereExchange{techAmount = amt, techRole = role, techLocation = loc} -> do
        name <- flowNameOf db ex
        unit <- unitNameOf (exchangeUnitId ex) db
        Just
            [ CText name
            , CNum amt
            , CText name
            , locCell loc
            , CText unit
            , CEmpty
            , CText (techTypeLabel role)
            , CText (wcDatabaseName cfg)
            ]
    ex@BiosphereExchange{bioAmount = amt, bioLocation = loc} -> do
        flow <- M.lookup (exchangeFlowId ex) (sdbBioFlows db)
        unit <- unitNameOf (exchangeUnitId ex) db
        Just
            [ CText (bfName flow)
            , CNum amt
            , CEmpty
            , locCell loc
            , CText unit
            , CText (renderCategories (bfCompartment flow))
            , CText "biosphere"
            , CText (wcDatabaseName cfg)
            ]
    ex@WasteExchange{waAmount = amt, waLocation = loc} -> do
        name <- flowNameOf db ex
        unit <- unitNameOf (exchangeUnitId ex) db
        -- Brightway has no native waste type; emit as a technosphere input so the
        -- amount and supplier link survive the round-trip.
        Just
            [ CText name
            , CNum amt
            , CText name
            , locCell loc
            , CText unit
            , CEmpty
            , CText "technosphere"
            , CText (wcDatabaseName cfg)
            ]
  where
    locCell l = if T.null l then CEmpty else CText l

-- | Brightway @type@ label for a technosphere role.
techTypeLabel :: TechRole -> Text
techTypeLabel = \case
    ReferenceProduct -> "production"
    Coproduct -> "production"
    ReferenceInput -> "technosphere"
    Input -> "technosphere"

{- | Render a 'Compartment' to a Brightway @categories@ cell (@"air"@ or
@"natural resource::in water"@). 'Nothing' (no compartment recorded) and an
empty medium both become an empty cell — the inverse of 'splitCategories'.
-}
renderCategories :: Maybe Compartment -> Text
renderCategories = \case
    Nothing -> ""
    Just (Compartment comp sub) -> case sub of
        Just s | not (T.null (T.strip s)) -> comp <> "::" <> s
        _ -> comp

-- ---------------------------------------------------------------------------
-- Lookups (pure)
-- ---------------------------------------------------------------------------

-- | Resolve the display name of a technosphere/waste exchange's flow.
flowNameOf :: SimpleDatabase -> Exchange -> Maybe Text
flowNameOf db = \case
    ex@TechnosphereExchange{} -> tfName <$> M.lookup (exchangeFlowId ex) (sdbTechFlows db)
    ex@WasteExchange{} -> wfName <$> M.lookup (exchangeFlowId ex) (sdbWasteFlows db)
    ex@BiosphereExchange{} -> bfName <$> M.lookup (exchangeFlowId ex) (sdbBioFlows db)

unitNameOf :: UUID -> SimpleDatabase -> Maybe Text
unitNameOf uid db = unitName <$> M.lookup uid (sdbUnits db)

-- | First element matching a predicate (total; no partial 'head').
lookup' :: (a -> Bool) -> [a] -> Maybe a
lookup' p = foldr (\x acc -> if p x then Just x else acc) Nothing

-- ---------------------------------------------------------------------------
-- Numeric formatting (pure, deterministic)
-- ---------------------------------------------------------------------------

{- | Canonical numeric rendering for amount cells. Integers print without a
decimal point (@1@, not @1.0@); everything else uses the shortest round-tripping
'show' for a 'Double'. Both forms re-parse via @Data.Text.Read.double@, so the
written cell reconstructs the same 'Double'.
-}
formatAmount :: Double -> Text
formatAmount d
    | isNaN d || isInfinite d = T.pack (show d)
    | d == fromIntegral i && abs d < 1.0e15 = T.pack (show i)
    | otherwise = T.pack (show d)
  where
    i = round d :: Integer

-- ---------------------------------------------------------------------------
-- SpreadsheetML emitter (hand-built, mirrors the parser's reader)
-- ---------------------------------------------------------------------------

workbookXml :: Text
workbookXml =
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
        <> "<workbook xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\""
        <> " xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\">"
        <> "<sheets><sheet name=\"data\" sheetId=\"1\" r:id=\"rId1\"/></sheets>"
        <> "</workbook>"

relsXml :: Text
relsXml =
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
        <> "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">"
        <> "<Relationship Id=\"rId1\""
        <> " Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet\""
        <> " Target=\"worksheets/sheet1.xml\"/>"
        <> "</Relationships>"

sheetXml :: [[Cell]] -> Text
sheetXml rows =
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
        <> "<worksheet xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\"><sheetData>"
        <> T.concat [rowXml n r | (n, r) <- zip [1 ..] rows]
        <> "</sheetData></worksheet>"

rowXml :: Int -> [Cell] -> Text
rowXml n cells =
    "<row r=\""
        <> tshow n
        <> "\">"
        <> T.concat [cellXml col n cell | (col, cell) <- zip [0 ..] cells]
        <> "</row>"

-- | Empty cells are omitted entirely (sparse rows, like the parser expects).
cellXml :: Int -> Int -> Cell -> Text
cellXml _ _ CEmpty = ""
cellXml col n (CNum d) =
    "<c r=\"" <> cellRef col n <> "\" t=\"n\"><v>" <> formatAmount d <> "</v></c>"
cellXml col n (CText t) =
    "<c r=\""
        <> cellRef col n
        <> "\" t=\"inlineStr\"><is><t xml:space=\"preserve\">"
        <> escapeXml t
        <> "</t></is></c>"

-- | A1-style cell reference. Columns beyond Z use multi-letter references.
cellRef :: Int -> Int -> Text
cellRef col n = columnLetters col <> tshow n

columnLetters :: Int -> Text
columnLetters = T.pack . reverse . go
  where
    go c =
        let (q, r) = (c `div` 26, c `mod` 26)
            letter = chr (ord 'A' + r)
         in letter : if q == 0 then [] else go (q - 1)

escapeXml :: Text -> Text
escapeXml =
    T.replace "\"" "&quot;"
        . T.replace ">" "&gt;"
        . T.replace "<" "&lt;"
        . T.replace "&" "&amp;"

tshow :: (Show a) => a -> Text
tshow = T.pack . show
