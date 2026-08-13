{- | Text normalization for search: accent-stripping, lowercase, and
tokenization. Shared by the BM25 index builder and the query path so
the same transformation applies to both sides.
-}
module Search.Normalize (
    normalize,
    tokenize,
    queryWords,
    caseInsensitiveInfixOf,
) where

import Data.Char (GeneralCategory (NonSpacingMark), generalCategory, isAlphaNum)
import Data.List (nub, sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Normalize as TN

normalize :: Text -> Text
normalize =
    T.map replacePunct
        . T.filter (not . isCombining)
        . TN.normalize TN.NFD
        . T.toLower
  where
    isCombining c = generalCategory c == NonSpacingMark
    replacePunct c = if isAlphaNum c then c else ' '

tokenize :: Text -> [Text]
tokenize = filter (not . T.null) . T.words . normalize

{- | Split a search query into the words it looks for, when those words are
matched against text that keeps its accents (unlike 'tokenize', which feeds
an index built on stripped text).

Anything not alphanumeric separates, so punctuation the searched text
carries never has to be retyped. Three details earn their line:

  * the query is recomposed (NFC) first, because a decomposed accent is a
    combining mark, which is not alphanumeric and would otherwise cut a
    word in two;
  * repeats are dropped, since a word already looked for tells us nothing
    the second time, and a caller scanning a corpus per word pays for it;
  * the longest words come first, so a scan that must satisfy every word
    fails on the most selective one first.
-}
queryWords :: Text -> [Text]
queryWords =
    sortOn (Down . T.length)
        . nub
        . filter (not . T.null)
        . T.split (not . isAlphaNum)
        . TN.normalize TN.NFC

{- | Case-insensitive substring test using Unicode case folding.
@needle `caseInsensitiveInfixOf` haystack@ mirrors 'T.isInfixOf' semantics.
-}
caseInsensitiveInfixOf :: Text -> Text -> Bool
caseInsensitiveInfixOf needle haystack =
    T.toCaseFold needle `T.isInfixOf` T.toCaseFold haystack
