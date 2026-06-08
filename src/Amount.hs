{-# LANGUAGE OverloadedStrings #-}

{- | Correctly-rounded parsing of decimal amounts.

This is the read side of the amount round-trip whose write side is
'EcoSpold.Common.showFFloatTrim'. Keeping the reader in one place lets every
importer and every export round-trip guard share the exact same parse, so the
guards mirror what import will actually do.
-}
module Amount (readAmount) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)

{- | Parse a decimal amount to a 'Double' with correct round-to-nearest
rounding — the exact inverse of 'EcoSpold.Common.showFFloatTrim'.

'Data.Text.Read.double' is /not/ correctly rounded: it can be off by up to one
ULP on perfectly ordinary magnitudes (e.g. @"0.0000010897906999999999"@ parses
to @1.0897907e-6@, a different 'Double'). On import that silently corrupts
amounts; on the writers' round-trip guards it rejects values the format can
represent. 'read' builds the value through @fromRational@ of the exact decimal,
which /is/ correctly rounded, so every finite 'Double' round-trips — including
subnormals.

Accepts the same forms 'Data.Text.Read.double' does — surrounding whitespace, a
leading @\'+\'@, and a bare leading or trailing decimal point (@".5"@, @"1."@) —
but rejects trailing garbage and the non-finite literals @Infinity@/@NaN@, which
are never valid LCA amounts and must surface rather than slip in silently.
-}
readAmount :: Text -> Maybe Double
readAmount t = case readMaybe (T.unpack (normalize t)) :: Maybe Double of
    Just d | not (isNaN d || isInfinite d) -> Just d
    _ -> Nothing
  where
    normalize = padLeadingDot . padTrailingDot . dropLeadingPlus . T.strip
    dropLeadingPlus s = fromMaybe s (T.stripPrefix "+" s)
    padTrailingDot s = if "." `T.isSuffixOf` s then s <> "0" else s
    padLeadingDot s = case T.uncons s of
        Just ('.', _) -> "0" <> s
        Just ('-', r) | "." `T.isPrefixOf` r -> "-0" <> r
        _ -> s
