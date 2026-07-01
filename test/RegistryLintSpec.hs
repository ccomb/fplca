{-# LANGUAGE OverloadedStrings #-}

{- | Offline lint of the curated flow-name registry (@data/flows.csv@).

The registry asserts substance identities that the LCIA matcher applies
blindly, so these checks constrain what the file may SAY, at commit time —
a bad bridge fails CI here instead of silently corrupting scores:

* every equivalence class stays plausibly small (a large closure means an
  ambiguous bridge fused unrelated substances — the junk-hub failure mode);
* no class fuses distinct carbon-origin qualifiers (fossil, biogenic,
  land-use-change) — those are different flows with different CFs by design;
* CAS metadata, where provided, is well-formed (check digit) and consistent
  within a class (one class = one substance = one CAS).
-}
module RegistryLintSpec (spec) where

import qualified Data.ByteString.Lazy as BL
import Data.Char (digitToInt, isDigit)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import SynonymDB (
    RegistryRow (..),
    SynEdge (..),
    SynonymDB (..),
    buildFromEdges,
    normalizeName,
    parseRegistryCSV,
 )

{- | An accidental transitive fusion shows up as a class an order of magnitude
larger than curation produces (largest legitimate class today: 7 names).
-}
maxClassSize :: Int
maxClassSize = 12

spec :: Spec
spec = describe "curated registry lint (data/flows.csv)" $ do
    parsed <- runIO (parseRegistryCSV <$> BL.readFile "data/flows.csv")
    case parsed of
        Left err -> it "parses" $ expectationFailure err
        Right rows -> do
            let classes = M.elems (synIdToNames (buildFromEdges (map rrEdge rows)))

            it "keeps every equivalence class plausibly small" $
                filter ((> maxClassSize) . length) classes `shouldBe` []

            it "never fuses distinct carbon-origin qualifiers into one class" $
                filter ((> 1) . S.size . originQualifiers) classes `shouldBe` []

            it "has well-formed CAS numbers (format + check digit)" $
                [c | r <- rows, Just c <- [rrCas r], not (casValid c)] `shouldBe` []

            it "gives every equivalence class at most one CAS" $
                filter ((> 1) . S.size . classCas rows) classes `shouldBe` []

{- | Carbon-origin families named inside a class. A name with no qualifier is
compatible with any single family (SimaPro's bare \"Carbon dioxide\" IS the
fossil flow), but two DIFFERENT families in one class means the registry
equates flows that characterization methods deliberately split.
Names arrive normalized (lowercased, punctuation stripped).
-}
originQualifiers :: [Text] -> S.Set Text
originQualifiers = S.fromList . concatMap families
  where
    families n =
        ["fossil" | "fossil" `T.isInfixOf` n, not ("non-fossil" `T.isInfixOf` n)]
            <> ["biogenic" | "biogenic" `T.isInfixOf` n || "non-fossil" `T.isInfixOf` n]
            <> ["luluc" | any (`T.isInfixOf` n) lulucPhrases]
    lulucPhrases =
        ["land transformation", "land use change", "peat oxidation", "soil or biomass stock"]

{- | CAS numbers declared for a class's member names. A row's CAS applies to
both endpoints of its bridge (the bridge asserts one substance), keyed by
normalized name to line up with the class members.
-}
classCas :: [RegistryRow] -> [Text] -> S.Set Text
classCas rows cls = S.unions (mapMaybe (`M.lookup` byName) cls)
  where
    byName =
        M.fromListWith
            S.union
            [ (n, S.singleton cas)
            | r <- rows
            , Just cas <- [rrCas r]
            , n <- [normalizeName (seA (rrEdge r)), normalizeName (seB (rrEdge r))]
            ]

{- | CAS format @NNNNNNN-NN-N@ (2-7 digits, 2 digits, check digit): the check
digit is the mod-10 weighted sum of the other digits, weighted 1.. from the
right. Catches transcription typos at commit time.
-}
casValid :: Text -> Bool
casValid t = case T.splitOn "-" t of
    [a, b, c] ->
        digitsBetween 2 7 a
            && digitsBetween 2 2 b
            && digitsBetween 1 1 c
            && checkDigit (T.unpack (a <> b)) == T.unpack c
    _ -> False
  where
    digitsBetween lo hi s = T.all isDigit s && T.length s >= lo && T.length s <= hi
    checkDigit ds = show (sum (zipWith (*) [1 ..] (map digitToInt (reverse ds))) `mod` 10)
