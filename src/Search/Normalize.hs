{- | Text normalization for search: accent-stripping, lowercase, and
tokenization. Shared by the BM25 index builder and the query path so
the same transformation applies to both sides.
-}
module Search.Normalize (
    normalize,
    tokenize,
    queryWords,
    matchesEveryWord,
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

{- | Does every word of the query appear, case-blind, in at least one of the
given fields? Order and punctuation are then the searched text's business
rather than the typist's: @carbon dioxide fossil@ reaches a flow named
@Carbon dioxide, fossil@, and so does @fossil carbon dioxide@.

Matching the query as one string instead is what made those two miss, and
every place that filters by name owes the same answer to the same query.

Words stay substrings rather than whole tokens, because names are searched
by fragment: @chlor@ must keep reaching @Trichloroethane@, which no
tokenizer would return. A query holding no word at all (pure punctuation)
matches nothing rather than everything.

Apply it to the query first and to each candidate afterwards: a corpus scan
then splits the query once instead of once per candidate.
-}
matchesEveryWord :: Text -> [Text] -> Bool
matchesEveryWord query = case map T.toCaseFold (queryWords query) of
    [] -> const False
    ws -> \fields ->
        let folded = map T.toCaseFold fields
         in all (\w -> any (w `T.isInfixOf`) folded) ws

{- | Case-insensitive substring test using Unicode case folding.
@needle `caseInsensitiveInfixOf` haystack@ mirrors 'T.isInfixOf' semantics.
-}
caseInsensitiveInfixOf :: Text -> Text -> Bool
caseInsensitiveInfixOf needle haystack =
    T.toCaseFold needle `T.isInfixOf` T.toCaseFold haystack
