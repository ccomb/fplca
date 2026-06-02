{-# LANGUAGE OverloadedStrings #-}

{- | Relink-with-mapping: re-link an (unlinked) database to a chosen background
dependency using a name→name alias mapping loaded from a CSV.

Agribalyse carries Ecoinvent-named background inputs; the BAFU database names
the same activities differently. A curated CSV maps the source (consumer)
input-flow name to the target (BAFU) activity name, so the existing cross-DB
matcher resolves the link even though the raw names differ.

The CSV is header-based (cassava 'decodeByName'). Recognized columns:

  * @source@ (or @source_name@ / @from@) — required: the consumer's input flow name
  * @target@ (or @target_name@ / @to@)  — required: the supplier activity name
  * @source_location@ / @target_location@ — optional, currently informational

Only the (source → target) name pair drives linking; location is matched by
the existing geography policy, not by this map. Unit-incompatible or
consumed-nowhere outcomes are not silently dropped — they surface through the
relink's 'CrossDBLinkingStats' (unresolved products carry a 'LinkBlocker') and
through the returned 'RelinkResult'.
-}
module Database.RelinkMapping (
    -- * CSV mapping
    AliasRow (..),
    parseAliasCSV,
    buildAliasMap,
    rejectEmpty,
    loadAliasMap,

    -- * Relink entry
    relinkWithMappingFile,
) where

import Control.Applicative ((<|>))
import Control.Monad (mfilter)
import qualified Data.ByteString.Lazy as BL
import Data.Csv (FromNamedRecord (..), (.:))
import qualified Data.Csv as Csv
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import Control.Exception (SomeException, try)
import Database.Manager (DatabaseManager, RelinkResult, relinkDatabaseWithMapping)

{- | One row of the alias mapping. Locations are parsed when present but are
not (yet) used to disambiguate the link — the cross-DB matcher applies the
database's geography policy independently.
-}
data AliasRow = AliasRow
    { arSource :: !Text
    , arTarget :: !Text
    , arSourceLocation :: !(Maybe Text)
    , arTargetLocation :: !(Maybe Text)
    }
    deriving (Show, Eq)

{- | Header-tolerant decoding: accept the primary column name or any documented
synonym. The two name columns are required; location columns are optional.
-}
instance FromNamedRecord AliasRow where
    parseNamedRecord r =
        AliasRow
            <$> firstField r ["source", "source_name", "from"]
            <*> firstField r ["target", "target_name", "to"]
            <*> optField r ["source_location", "source_geo"]
            <*> optField r ["target_location", "target_geo"]

{- | Parse the first present column among @names@; fail if none is found.
Each candidate is tried via cassava's '(.:)' (which fails on a missing key),
falling through with '(<|>)'.
-}
firstField :: Csv.NamedRecord -> [Text] -> Csv.Parser Text
firstField r names =
    foldr (\nm acc -> (r .: enc nm) <|> acc) noColumn names
  where
    noColumn =
        fail $
            "missing required column (one of: "
                <> T.unpack (T.intercalate ", " names)
                <> ")"

{- | Parse the first present column among @names@ as optional. A present but
blank (whitespace-only) cell normalizes to 'Nothing' so "absent" has a single
representation before any consumer relies on it.
-}
optField :: Csv.NamedRecord -> [Text] -> Csv.Parser (Maybe Text)
optField r names =
    foldr (\nm acc -> (blankToNothing <$> r .: enc nm) <|> acc) (pure Nothing) names
  where
    blankToNothing = mfilter (not . T.null) . Just . T.strip

enc :: Text -> Csv.Name
enc = Csv.toField

{- | Parse alias rows from CSV bytes. A fully-blank source/target row is a
no-op and skipped; a half-specified row (exactly one side blank) is a curation
mistake and rejected loudly rather than silently dropped.
-}
parseAliasCSV :: BL.ByteString -> Either Text [AliasRow]
parseAliasCSV bytes =
    case Csv.decodeByName bytes of
        Left err -> Left $ "alias CSV parse error: " <> T.pack err
        Right (_, rows) ->
            fmap concat . traverse classify . zip [1 :: Int ..] $
                V.toList (rows :: V.Vector AliasRow)
  where
    classify (n, row) =
        let src = T.strip (arSource row)
            tgt = T.strip (arTarget row)
            halfBlank side =
                Left $
                    "alias CSV row "
                        <> T.pack (show n)
                        <> ": "
                        <> side
                        <> " is blank (rows must specify both source and target, or neither)"
         in case (T.null src, T.null tgt) of
                (True, True) -> Right []
                (True, False) -> halfBlank "source"
                (False, True) -> halfBlank "target"
                (False, False) -> Right [row{arSource = src, arTarget = tgt}]

{- | Build the source-name → target-name alias map. On a duplicate source name
with conflicting targets, fail rather than silently pick one. Identical
duplicates collapse harmlessly.
-}
buildAliasMap :: [AliasRow] -> Either Text (Map Text Text)
buildAliasMap rows =
    foldr step (Right M.empty) rows
  where
    step row acc = do
        m <- acc
        let src = arSource row
            tgt = arTarget row
        case M.lookup src m of
            Just existing
                | existing /= tgt ->
                    Left $
                        "conflicting alias for "
                            <> src
                            <> ": "
                            <> existing
                            <> " vs "
                            <> tgt
            _ -> Right (M.insert src tgt m)

{- | Load and validate an alias map from a CSV file path. An alias map with no
usable rows (header-only / all-blank) would relink as a silent no-op, so it is
rejected loudly.
-}
loadAliasMap :: FilePath -> IO (Either Text (Map Text Text))
loadAliasMap path = do
    result <- try (BL.readFile path) :: IO (Either SomeException BL.ByteString)
    case result of
        Left e -> pure $ Left $ "cannot read mapping file " <> T.pack path <> ": " <> T.pack (show e)
        Right bytes -> pure (parseAliasCSV bytes >>= buildAliasMap >>= rejectEmpty)

{- | Reject an alias map with no usable rows (header-only / all-blank). Such a
map would relink as a silent no-op, so both the CLI ('loadAliasMap') and the
HTTP relink handler reject it loudly rather than return 200 with no effect.
-}
rejectEmpty :: Map Text Text -> Either Text (Map Text Text)
rejectEmpty m
    | M.null m = Left "mapping file contains no usable source→target rows"
    | otherwise = Right m

{- | Relink @dbName@ against @depDb@ using the alias mapping in @csvPath@.
Loads + validates the CSV, then delegates to 'relinkDatabaseWithMapping'.
Guardrails (unit-incompatible / consumed-nowhere) are not swallowed: they are
reflected in the resulting 'RelinkResult' (unresolved counts) and the
database's linking stats.
-}
relinkWithMappingFile ::
    DatabaseManager ->
    -- | database to relink
    Text ->
    -- | dependency database to link against
    Text ->
    -- | mapping CSV path
    FilePath ->
    IO (Either Text RelinkResult)
relinkWithMappingFile manager dbName depDb csvPath = do
    aliasResult <- loadAliasMap csvPath
    case aliasResult of
        Left err -> pure (Left err)
        Right aliases -> relinkDatabaseWithMapping manager dbName depDb aliases
