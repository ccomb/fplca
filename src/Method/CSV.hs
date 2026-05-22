{- | Shared CSV plumbing for the LCIA method parsers.

Delimiter detection, row splitting and number parsing were previously
duplicated across the CSV-based method parsers — and had drifted apart
(delimiter detection disagreed on tab support). They live here as the
single source of truth.
-}
module Method.CSV (
    detectDelimiter,
    splitRow,
    parseDouble,
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.Read as TR

-- | Auto-detect a row delimiter: semicolon, then tab, then comma.
detectDelimiter :: BS.ByteString -> Char
detectDelimiter line
    | BC.elem ';' line = ';'
    | BC.elem '\t' line = '\t'
    | otherwise = ','

-- | Split a row on the delimiter, decoding each cell to 'Text' leniently.
splitRow :: Char -> BS.ByteString -> [Text]
splitRow delim = map (TE.decodeUtf8With TEE.lenientDecode) . BC.split delim

-- | Parse a 'Double', 'Nothing' on failure.
parseDouble :: Text -> Maybe Double
parseDouble t = case TR.double t of
    Right (v, _) -> Just v
    Left _ -> Nothing
