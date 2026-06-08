{-# LANGUAGE OverloadedStrings #-}

{- | Format dispatch for database export.

Inverts the upload/parse path: given an in-memory 'Database', serialize it to
bytes in one of the supported formats by delegating to the per-format writer.

Single-file formats (SimaPro CSV, EcoSpold 1, Brightway Excel) serialize to one
byte stream. Multi-file formats (EcoSpold 2, ILCD) are inherently directory
trees, so they are packaged into a deterministic zip archive. 'OpenLcaJsonLd'
has no writer and 'UnknownFormat' is not a real target, so both fail loudly
('Left') rather than emit a silent empty file.
-}
module Database.Export (
    serializeDatabase,
    exportWarnings,
    exportDatabase,
    parseExportFormat,
) where

import qualified Codec.Archive.Zip as Zip
import Control.Exception (SomeException, try)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import qualified BrightwayExcel.Writer as BE
import Database.Upload (DatabaseFormat (..))
import qualified EcoSpold.Writer1 as ES1
import qualified EcoSpold.Writer2 as ES2
import qualified ILCD.Writer as ILCD
import qualified SimaPro.Writer as SP
import Types (Database, toSimpleDatabase)

{- | Serialize a database to a single byte stream in the requested format. Pure:
the multi-file formats are zipped in-memory. Fails loudly for formats without a
writer.
-}
serializeDatabase :: DatabaseFormat -> Database -> Either Text BL.ByteString
serializeDatabase fmt db = case fmt of
    -- Each writer runs its own check*Exportable and returns 'Left' on a database
    -- the format cannot represent faithfully, so the guard is unskippable.
    SimaProCSV -> BL.fromStrict <$> SP.serializeSimaProCSV SP.defaultWriterConfig sdb
    EcoSpold1 -> BL.fromStrict . TE.encodeUtf8 <$> ES1.writeDatabase ES1.canonicalWriterOptions db
    EcoSpold2 -> zipText <$> ES2.writeEcoSpold2 ES2.noVolatileMeta sdb
    ILCDProcess -> ILCD.writeILCDArchive ILCD.defaultWriteOptions sdb
    BrightwayExcel -> BE.renderWorkbook BE.defaultWriterConfig sdb
    OpenLcaJsonLd ->
        Left "openLCA JSON-LD export is not supported"
    UnknownFormat ->
        Left "cannot export to an unknown format"
  where
    sdb = toSimpleDatabase db

{- | Best-effort approximations made when serializing @db@ to @fmt@. Empty for a
faithful export; non-empty when a writer had to approximate. Currently only the
Brightway writer approximates: it has no waste type, so it rewrites waste
exchanges as technosphere flows (inventory-preserving, but the waste tag is lost
on re-import) and reports the affected activities here.
-}
exportWarnings :: DatabaseFormat -> Database -> [Text]
exportWarnings BrightwayExcel db = BE.wasteManifest (toSimpleDatabase db)
exportWarnings _ _ = []

{- | Parse a user-facing export-format name (case- and whitespace-insensitive)
to a 'DatabaseFormat'. Shared by the CLI and the HTTP handler so the accepted
spellings and the error message stay in one place.
-}
parseExportFormat :: Text -> Either Text DatabaseFormat
parseExportFormat raw = case T.toLower (T.strip raw) of
    "simapro" -> Right SimaProCSV
    "ecospold1" -> Right EcoSpold1
    "ecospold2" -> Right EcoSpold2
    "ilcd" -> Right ILCDProcess
    "brightway" -> Right BrightwayExcel
    other -> Left ("unknown export format: " <> other <> " (expected simapro|ecospold1|ecospold2|ilcd|brightway)")

-- | Serialize a database and write it to @path@.
exportDatabase :: DatabaseFormat -> Database -> FilePath -> IO (Either Text ())
exportDatabase fmt db path = case serializeDatabase fmt db of
    Left err -> pure (Left err)
    Right bytes -> either (Left . renderErr) Right <$> try (BL.writeFile path bytes)
  where
    renderErr :: SomeException -> Text
    renderErr e = "export failed: " <> T.pack (show e)

-- | Pack @(path, text)@ entries into a deterministic zip (epoch-0 mtimes).
zipText :: [(FilePath, Text)] -> BL.ByteString
zipText = Zip.fromArchive . foldl addOne Zip.emptyArchive
  where
    addOne arc (p, t) =
        Zip.addEntryToArchive
            (Zip.toEntry p 0 (BL.fromStrict (TE.encodeUtf8 t)))
            arc
