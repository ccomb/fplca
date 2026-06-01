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
    exportDatabase,
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
    SimaProCSV ->
        Right (BL.fromStrict (SP.serializeSimaProCSV SP.defaultWriterConfig sdb))
    EcoSpold1 ->
        Right (BL.fromStrict (TE.encodeUtf8 (ES1.writeDatabase ES1.canonicalWriterOptions db)))
    EcoSpold2 ->
        Right (zipText (ES2.writeEcoSpold2 ES2.noVolatileMeta sdb))
    ILCDProcess ->
        Right (ILCD.writeILCDArchive ILCD.defaultWriteOptions sdb)
    BrightwayExcel ->
        Right (BE.renderWorkbook BE.defaultWriterConfig sdb)
    OpenLcaJsonLd ->
        Left "openLCA JSON-LD export is not supported"
    UnknownFormat ->
        Left "cannot export to an unknown format"
  where
    sdb = toSimpleDatabase db

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
