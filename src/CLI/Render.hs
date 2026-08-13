{-# LANGUAGE OverloadedStrings #-}

{- | Turn a result value into the text the user asked for.

One renderer for both ways a command reaches a result: over HTTP
("CLI.Client") and against a database loaded in-process ("CLI.Command").
Pure, so the rendering rules are testable without a server or a database.

@--format csv@ flattens an array of objects into a header row plus one row
per element. Which array is a choice the caller has to make — a response
carries several — so @--jsonpath@ names it, as a dotted field path
(@srResults@, @piActivity.pfaExchanges@). A path that names nothing, or
names something that is not an array, is refused with what was found
instead of quietly printing JSON where a table was expected.
-}
module CLI.Render (
    renderResult,

    -- * Pure parts (exported for testing)
    selectPath,
    csvRows,
) where

import Data.Aeson (Value (..), encode)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.List (intercalate, transpose)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Vector as V

import CLI.Types (OutputFormat (..))

{- | Render a result, or say why it cannot be rendered that way. The path is
the @--jsonpath@ the caller was given; it only bears on 'CSV', the one format
that has to pick a single array out of the response.
-}
renderResult :: OutputFormat -> Maybe Text -> Value -> Either Text Text
renderResult fmt mPath val = case fmt of
    JSON -> Right (lazyUtf8 (encode val) <> "\n")
    Pretty -> Right (lazyUtf8 (encodePretty val) <> "\n")
    Table -> Right (T.pack (renderTable val))
    CSV -> T.pack . renderCSV <$> csvRows mPath val

lazyUtf8 :: BL.ByteString -> Text
lazyUtf8 = TL.toStrict . TLE.decodeUtf8

{- | The rows @--format csv@ should flatten. With a path, the array it names;
without one, the response's single array field, which is all the guess can
resolve unambiguously.
-}
csvRows :: Maybe Text -> Value -> Either Text [Value]
csvRows Nothing val = case findArray val of
    Just rows -> Right rows
    Nothing ->
        Left "--format csv needs --jsonpath to name the array to flatten: this result holds no single array field"
csvRows (Just path) val = do
    selected <- selectPath path val
    case selected of
        Array arr -> Right (V.toList arr)
        other ->
            Left $
                "--jsonpath \""
                    <> path
                    <> "\" names a "
                    <> jsonKind other
                    <> ", and --format csv flattens an array"

{- | Resolve a dotted field path against a JSON value: @piActivity.pfaExchanges@
walks two object fields. A step that finds no such field lists the fields that
are there, because the wire names are the stripped record prefixes and are easy
to misremember.
-}
selectPath :: Text -> Value -> Either Text Value
selectPath path = go [] (T.splitOn "." path)
  where
    go _ [] value = Right value
    go walked (field : rest) value = case value of
        Object o -> case KM.lookup (Key.fromText field) o of
            Just next -> go (walked ++ [field]) rest next
            Nothing ->
                Left $
                    "--jsonpath \""
                        <> path
                        <> "\": no field \""
                        <> field
                        <> "\""
                        <> atPath walked
                        <> ". Available: "
                        <> T.intercalate ", " (map Key.toText (KM.keys o))
        other ->
            Left $
                "--jsonpath \""
                    <> path
                    <> "\": cannot look up \""
                    <> field
                    <> "\""
                    <> atPath walked
                    <> ", which is a "
                    <> jsonKind other

    atPath [] = ""
    atPath walked = " in \"" <> T.intercalate "." walked <> "\""

jsonKind :: Value -> Text
jsonKind v = case v of
    Object _ -> "object"
    Array _ -> "array"
    String _ -> "string"
    Number _ -> "number"
    Bool _ -> "boolean"
    Null -> "null"

-- | Render a JSON value as an aligned text table
renderTable :: Value -> String
renderTable val =
    case findArray val of
        Just rows -> formatTable (extractTable rows)
        Nothing -> BSL.unpack (encodePretty val) ++ "\n" -- fallback for non-array

-- | Render rows as CSV
renderCSV :: [Value] -> String
renderCSV rows =
    let (headers, dataRows) = extractTable rows
     in unlines $ intercalate "," (map quote headers) : map (intercalate "," . map quote) dataRows
  where
    quote s = "\"" ++ concatMap (\c -> if c == '"' then "\"\"" else [c]) s ++ "\""

-- | Find the first array in a JSON value (top-level or one level deep)
findArray :: Value -> Maybe [Value]
findArray (Array arr) = Just (V.toList arr)
findArray (Object obj) =
    -- Look for a single array field (e.g., databases, results, methods, items)
    case mapMaybe extractArr (KM.toList obj) of
        [(_, arr)] -> Just arr
        _ -> Nothing
  where
    extractArr (_, Array arr) = Just ((), V.toList arr)
    extractArr _ = Nothing
findArray _ = Nothing

-- | Extract headers and rows from a list of JSON objects
extractTable :: [Value] -> ([String], [[String]])
extractTable [] = ([], [])
extractTable rows@(Object first : _) =
    let keys = map fst (KM.toList first)
        headers = map Key.toString keys
        dataRows = map (rowValues keys) rows
     in (headers, dataRows)
extractTable rows = (["value"], map (\v -> [cellValue v]) rows)

rowValues :: [KM.Key] -> Value -> [String]
rowValues keys (Object obj) = map (\k -> cellValue (fromMaybe Null (KM.lookup k obj))) keys
rowValues _ v = [cellValue v]

-- | Convert a JSON value to a display string for table cells
cellValue :: Value -> String
cellValue (String t) = T.unpack t
cellValue (Number n) = let s = show n in if ".0" `isSuffixOf` s then take (length s - 2) s else s
cellValue (Bool True) = "yes"
cellValue (Bool False) = ""
cellValue Null = ""
cellValue v = BSL.unpack (encode v)

isSuffixOf :: String -> String -> Bool
isSuffixOf suffix str = drop (length str - length suffix) str == suffix

-- | Format headers + rows as an aligned table with separators
formatTable :: ([String], [[String]]) -> String
formatTable ([], _) = ""
formatTable (headers, rows) =
    let allRows = headers : rows
        widths = map (maximum . map length) (transpose (map (map (take maxColWidth)) allRows))
        padRow = zipWith (\w c -> take maxColWidth c ++ replicate (w - length (take maxColWidth c)) ' ') widths
        sep = intercalate "+" (map (\w -> replicate (w + 2) '-') widths)
        fmtRow r = "  " ++ intercalate " | " (padRow r)
     in unlines $ fmtRow headers : ("--" ++ sep) : map fmtRow rows
  where
    maxColWidth = 60
