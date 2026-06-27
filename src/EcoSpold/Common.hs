{-# LANGUAGE OverloadedStrings #-}

-- | Common utilities shared between EcoSpold1 and EcoSpold2 parsers
module EcoSpold.Common (
    bsToText,
    decodeXmlEntities,
    bsToDouble,
    bsToInt,
    bsToIntMaybe,
    isElement,
    distributeFiles,
    nonEmptyText,
    showFFloatTrim,
) where

import Amount (readAmount)
import qualified Data.ByteString as BS
import Data.Char (chr)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as TR
import Numeric (showFFloat)

-- | ByteString to Text conversion with UTF-8 decoding and XML entity decoding
bsToText :: BS.ByteString -> Text
bsToText = decodeXmlEntities . TE.decodeUtf8

{- | Decode the XML entities Xeno (a fast SAX parser) leaves intact: the five
named entities plus any numeric character reference via 'decodeNumericRefs'.
The numeric pass subsumes the line-feed/carriage-return refs in EcoSpold
attribute values (e.g. @generalComment="text&#10;"@) and the @&#039;@/@&#034;@
apostrophe/quote refs that otherwise truncate chemical-name synonyms when the
ILCD synonym text is split on @;@.

Order is load-bearing. The named entities resolve before @&amp;@, so an escaped
literal @"&lt;"@ (written @"&amp;lt;"@) round-trips to @"&lt;"@ rather than @"<"@
— the inverse of writers escaping @&@ first. Numeric refs resolve LAST, after
@&amp;@: the ILCD flow data double-encodes apostrophes/quotes as @&amp;#039;@, so
the @&amp;@ step must first expose the @&#039;@ for the numeric pass to turn it
into @'@. Run earlier it sees no @&#@, @&amp;@ then leaves a bare @&#039;@, and
the @;@-split truncates the synonym into junk. The trade: a literal @&#039;@-as-
text (were it written @&amp;#039;@) decodes instead of surviving — the flow data
carries no such literals, only double-encoded characters.
-}
decodeXmlEntities :: Text -> Text
decodeXmlEntities =
    decodeNumericRefs
        . T.replace "&amp;" "&"
        . T.replace "&lt;" "<"
        . T.replace "&gt;" ">"
        . T.replace "&quot;" "\""
        . T.replace "&apos;" "'"

{- | Decode XML numeric character references — decimal @&#NNN;@ and hex
@&#xHH;@ — to their characters. A malformed or out-of-range reference is left
verbatim rather than crashing (no partial 'chr' on bad input); 'Integer' parsing
avoids overflow on absurdly long digit runs.
-}
decodeNumericRefs :: Text -> Text
decodeNumericRefs t =
    case T.breakOn "&#" t of
        (before, rest)
            | T.null rest -> before
            | otherwise ->
                let (body, semi) = T.span (/= ';') (T.drop 2 rest)
                 in case (refChar body, T.null semi) of
                        (Just c, False) ->
                            before <> T.singleton c <> decodeNumericRefs (T.drop 1 semi)
                        _ ->
                            before <> "&#" <> decodeNumericRefs (T.drop 2 rest)
  where
    refChar :: Text -> Maybe Char
    refChar body = case T.uncons body of
        Just (x, hexits)
            | x == 'x' || x == 'X' -> readRef TR.hexadecimal hexits
        _ -> readRef TR.decimal body
    readRef :: TR.Reader Integer -> Text -> Maybe Char
    readRef reader digits = case reader digits of
        Right (n, leftover)
            | T.null leftover, n >= 0, n <= 0x10FFFF -> Just (chr (fromInteger n))
        _ -> Nothing

-- | ByteString to Double conversion (strict - errors on parse failure)
bsToDouble :: BS.ByteString -> Double
bsToDouble bs = fromMaybe (error $ "Failed to parse double from: " ++ show bs) (readAmount (bsToText bs))

-- | ByteString to Int conversion (strict - errors on parse failure)
bsToInt :: BS.ByteString -> Int
bsToInt bs = case TR.decimal (bsToText bs) of
    Right (val, _) -> val
    Left _ -> error $ "Failed to parse int from: " ++ show bs

{- | ByteString to Int conversion that returns Nothing on parse failure.
Use for attribute values that are user-controlled or optional and where
crashing the parser on malformed input is not acceptable.
-}
bsToIntMaybe :: BS.ByteString -> Maybe Int
bsToIntMaybe bs = case TR.decimal (bsToText bs) of
    Right (val, _) -> Just val
    Left _ -> Nothing

{- | Check if element name matches (with or without namespace prefix)
Handles both "tagName" and "prefix:tagName" forms
-}
isElement :: BS.ByteString -> BS.ByteString -> Bool
isElement tagName expected =
    tagName == expected || BS.isSuffixOf (":" `BS.append` expected) tagName

-- | Drop empty / whitespace-only text. Single normalisation point.
nonEmptyText :: Text -> Maybe Text
nonEmptyText t =
    let s = T.strip t
     in if T.null s then Nothing else Just s

-- | Distribute a list evenly across N buckets (for parallel workers)
distributeFiles :: Int -> [a] -> [[a]]
distributeFiles n xs =
    let len = length xs
        baseSize = len `div` n
        remainder = len `mod` n
        sizes = replicate remainder (baseSize + 1) ++ replicate (n - remainder) baseSize
     in go sizes xs
  where
    go [] _ = []
    go _ [] = []
    go (s : ss) ys = let (h, t) = splitAt s ys in h : go ss t

{- | Render a 'Double' in fixed-point notation (never scientific) with trailing
zeros trimmed but at least one fractional digit kept. This is the canonical
amount format for the EcoSpold/ILCD/Brightway writers and the exact inverse of
'Amount.readAmount': through that correctly-rounded reader every finite 'Double'
— subnormals included — re-parses to the same value. Fixed-point is also the
form these exchange formats and their external readers expect. The writers'
export guards reject any amount that does not re-parse, which now leaves only the
non-finite @Infinity@/@NaN@.
-}
showFFloatTrim :: Double -> String
showFFloatTrim d =
    case break (== '.') (showFFloat Nothing d "") of
        (intPart, '.' : fracPart) ->
            let trimmed = reverse (dropWhile (== '0') (reverse fracPart))
             in intPart <> "." <> (if null trimmed then "0" else trimmed)
        (intPart, _) -> intPart <> ".0"
