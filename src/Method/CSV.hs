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
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE

-- | Auto-detect a row delimiter: semicolon, then tab, then comma.
detectDelimiter :: BS.ByteString -> Char
detectDelimiter line
    | BC.elem ';' line = ';'
    | BC.elem '\t' line = '\t'
    | otherwise = ','

-- | Split a row on the delimiter, decoding each cell to 'Text' leniently.
splitRow :: Char -> BS.ByteString -> [Text]
splitRow delim = map (TE.decodeUtf8With TEE.lenientDecode) . BC.split delim

{- | Parse a 'Double', 'Nothing' on failure. Goes through 'reads' — the
correctly-rounded parse — not 'TR.double', whose fast path drifts in the
last ulp (@1.2227e-3@ comes back as @1.2227000000000002e-3@), which is a
wrong number. Trailing garbage is tolerated, as it always was.
-}
parseDouble :: Text -> Maybe Double
parseDouble t = case reads (T.unpack t) of
    (v, _) : _ -> Just v
    [] -> Nothing
