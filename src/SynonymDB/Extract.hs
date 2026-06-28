{-# LANGUAGE OverloadedStrings #-}

{- | Extract synonym pairs from loaded databases and methods.

Produces CSV-compatible pairs that can be written to disk and loaded
by the existing SynonymDB pipeline.
-}
module SynonymDB.Extract (
    extractFromEcoSpold2,
    extractFromILCDFlows,
    synonymPairsToCSV,
) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.UUID (UUID)

import Method.FlowResolver (ILCDFlowInfo (..))
import Types (BioFlowDB, BiosphereFlow (..))

{- | Extract synonym pairs from EcoSpold2 biosphere flows.
For each flow with English synonyms, generates (flowName, synonym) pairs.
-}
extractFromEcoSpold2 :: BioFlowDB -> [(Text, Text)]
extractFromEcoSpold2 bioFlowDB =
    S.toList $
        S.fromList
            [ (bfName f, syn)
            | f <- M.elems bioFlowDB
            , syns <- maybe [] S.toList (M.lookup "en" (bfSynonyms f))
            , let syn = T.strip syns
            , not (T.null syn)
            , syn /= bfName f
            ]

{- | Extract synonym pairs from ILCD flow definitions: the direct
@<common:synonyms>@ of each flow (baseName ↔ each synonym).

CAS-based equivalence is deliberately NOT emitted as synonym pairs. Flows
sharing a CAS are already matched by the CAS cascade (@mtCasCF@) at lookup time,
so chaining same-CAS names here adds no new match — it only injects transitive
bridges that fuse unrelated substances into oversized closure classes (one
shared or blank CAS can connect hundreds of names through a single chain).
-}
extractFromILCDFlows :: M.Map UUID ILCDFlowInfo -> [(Text, Text)]
extractFromILCDFlows flowInfo =
    S.toList $
        S.fromList
            [ (ilcdBaseName info, syn)
            | info <- M.elems flowInfo
            , syn <- ilcdSynonyms info
            , syn /= ilcdBaseName info
            , not (norm syn `S.member` casAmbiguous)
            ]
  where
    norm = T.toLower . T.strip
    -- A name listed by flows of more than one distinct CAS is an ambiguous
    -- bridge — e.g. "sodium", carried both by the element flow and by hundreds
    -- of sodium-salt flows. Emitting it as a synonym lets the transitive closure
    -- fuse those unrelated substances into one junk hub, which then leaks an
    -- organic compound's characterization factor onto the inorganic element.
    -- Typed by CAS: drop such names so they cannot bridge across substances.
    casOfName :: M.Map Text (S.Set Text)
    casOfName =
        M.fromListWith
            S.union
            [ (norm nm, S.singleton cas)
            | info <- M.elems flowInfo
            , Just cas <- [ilcdCAS info]
            , not (T.null cas)
            , nm <- ilcdBaseName info : ilcdSynonyms info
            ]
    casAmbiguous = M.keysSet (M.filter ((> 1) . S.size) casOfName)

-- | Render synonym pairs as CSV with header.
synonymPairsToCSV :: [(Text, Text)] -> BL.ByteString
synonymPairsToCSV pairs =
    BLC.unlines ("name1,name2" : map renderPair pairs)
  where
    renderPair (a, b) = BLC.fromStrict (T.encodeUtf8 (csvField a <> "," <> csvField b))
    csvField t
        | T.any (\c -> c == ',' || c == '"' || c == '\n') t =
            "\"" <> T.replace "\"" "\"\"" t <> "\""
        | otherwise = t
