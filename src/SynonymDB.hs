{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Synonym Database

Maps flow names to synonym group IDs for flow matching
across different nomenclatures (ILCD, ecoinvent, SimaPro).

Loaded at runtime from CSV files (pairs of synonym names).
-}
module SynonymDB (
    -- * Building
    buildFromCSV,
    buildFromPairs,
    excludeOverFrequentSynonyms,
    excludeJunkSynonyms,
    isJunkSynonymName,
    starEdges,
    loadFromCSVFileWithCache,

    -- * Lookup
    lookupSynonymGroup,
    getSynonyms,
    normalizeName,
    mergeSynonymDBs,
    synonymCount,
    oversizedClasses,

    -- * Unit suffixes
    unitSuffixes,
    uncoveredUnitSuffixes,

    -- * Re-exports
    SynonymDB (..),
    emptySynonymDB,
) where

import qualified Codec.Compression.Zstd as Zstd
import Control.DeepSeq (force)
import Control.Exception (SomeException, catch, evaluate)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (isDigit)
import Data.Csv (HasHeader (..), decode)
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Store (decodeEx, encode)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Directory (doesFileExist, getModificationTime)

import SubstanceRegistry (equivalenceClasses)
import SynonymDB.Types (SynonymDB (..), emptySynonymDB)

{- | Build a SynonymDB from CSV content (two columns: name1, name2).
Each row declares two names as @SameAs@. Names are grouped by transitive
closure (see 'buildFromPairs'): A↔B and B↔C ⟹ one class {A,B,C}.
-}
buildFromCSV :: BL.ByteString -> Either String SynonymDB
buildFromCSV csvData =
    case decode HasHeader csvData of
        Left err -> Left $ "CSV parse error: " <> err
        Right rows -> Right $ buildFromPairs (V.toList (rows :: V.Vector (Text, Text)))

{- | Build a SynonymDB from @SameAs@ name pairs.

Names are normalized, then grouped into equivalence classes by transitive
closure (connected components) — the "set of sets" of the canonical flow
registry. A↔B and B↔C therefore land A, B and C in one class.

Closure is taken honestly, with no silent degree cap. Measured on the current
reference data, no class exceeds a handful of members (the feared
sulfate→…→carbonate chain does not occur), so closure is safe. Bad data — a junk
hub that would fuse unrelated substances — surfaces through 'oversizedClasses'
(the loader warns) rather than being silently dropped, and genuinely-broader
relations (SOx ⊃ SO₂) belong in the typed-edge layer, not as @SameAs@.
-}
buildFromPairs :: [(Text, Text)] -> SynonymDB
buildFromPairs raws =
    let normd = normalizePairs raws
     in (fromClasses (equivalenceClasses normd)){synEdges = normd}
  where
    normalizePairs :: [(Text, Text)] -> [(Text, Text)]
    normalizePairs rs =
        [ (n1, n2)
        | (raw1, raw2) <- rs
        , let n1 = normalizeName raw1
        , let n2 = normalizeName raw2
        , not (T.null n1)
        , not (T.null n2)
        , n1 /= n2
        ]

-- | Number a list of name classes into the bidirectional SynonymDB lookup tables.
fromClasses :: [[Text]] -> SynonymDB
fromClasses classes =
    let numbered = zip [0 ..] classes
        nameToId = M.fromList [(name, gid) | (gid, members) <- numbered, name <- members]
        idToNames = M.fromList numbered
     in SynonymDB nameToId idToNames (starEdges classes)

{- | Star edges for a set of name classes: connect each class's members to its
first member. Their transitive closure is exactly the classes — enough to
re-close the relation. 'buildFromPairs' overrides this with the original pairs,
for a faithful induced-subgraph restriction: a star centred on a node that the
restriction later drops would lose links the original topology preserves.
-}
starEdges :: [[Text]] -> [(Text, Text)]
starEdges classes = [(m0, m) | (m0 : ms) <- classes, m <- ms]

{- | Load a SynonymDB from a CSV file, using a binary cache for speed.
  On first load: parse CSV → build SynonymDB → save .cache.zst
  On subsequent loads: load .cache.zst directly (if newer than CSV)
-}
loadFromCSVFileWithCache :: FilePath -> IO (Either String SynonymDB)
loadFromCSVFileWithCache csvPath = do
    let cachePath = csvPath ++ ".cache.zst"
    cached <- loadCache cachePath csvPath
    case cached of
        Just db -> return (Right db)
        Nothing -> do
            exists <- doesFileExist csvPath
            if not exists
                then return (Left ("File not found: " ++ csvPath))
                else do
                    csvData <- BL.readFile csvPath
                    case buildFromCSV csvData of
                        Left err -> return (Left err)
                        Right db -> do
                            saveCache cachePath db
                            return (Right db)
  where
    loadCache cachePath srcPath = do
        exists <- doesFileExist cachePath
        if not exists
            then return Nothing
            else
                catch
                    ( do
                        cacheTime <- getModificationTime cachePath
                        srcTime <- getModificationTime srcPath
                        if cacheTime < srcTime
                            then return Nothing
                            else do
                                compressed <- BS.readFile cachePath
                                case Zstd.decompress compressed of
                                    Zstd.Decompress raw -> do
                                        let !db = decodeEx raw
                                        result <- evaluate (force db)
                                        return (Just result)
                                    _ -> return Nothing
                    )
                    (\(_ :: SomeException) -> return Nothing)
    saveCache cachePath db =
        catch
            (BS.writeFile cachePath (Zstd.compress 1 (encode db)))
            (\(_ :: SomeException) -> return ())

{- | Merge multiple SynonymDBs into one, re-closing across them: if one source
declares A=B and another B=C, the merged DB groups {A,B,C}. The sources' own
@synEdges@ are concatenated and the whole edge set is closed again, so the merged
DB carries the union of the original pairs (not a lossy star reconstruction).
-}
mergeSynonymDBs :: [SynonymDB] -> SynonymDB
mergeSynonymDBs [] = emptySynonymDB
mergeSynonymDBs [db] = db
mergeSynonymDBs dbs = (fromClasses (equivalenceClasses edges)){synEdges = edges}
  where
    -- Re-close from each source's own (already normalized) pairs rather than
    -- reconstructing stars from its classes, so the merged 'synEdges' keeps the
    -- faithful topology the induced-subgraph restriction needs.
    edges = concatMap synEdges dbs

-- | Number of synonym names in the database.
synonymCount :: SynonymDB -> Int
synonymCount = M.size . synNameToId

{- | Synonym classes with more than @maxSize@ members. Transitive closure has no
degree cap (a hub no longer silently truncates at 50), so an implausibly large
class — a junk hub that fused unrelated substances through one bad pair — must be
surfaced rather than silently polluting the synonym fan-out. The loader warns on
whatever this returns; an empty result means the closure stayed plausible.
-}
oversizedClasses :: Int -> SynonymDB -> [[Text]]
oversizedClasses maxSize = filter ((> maxSize) . length) . M.elems . synIdToNames

{- | Drop synonym pairs whose synonym (the second element) is carried by more
than @maxFlows@ distinct base names (the first element). An over-frequent
"synonym" is a classification label or stop-word — e.g. @"organic"@ (carried by
thousands of flows), @"inorganic"@, @"petroleum product"@ — not a true synonym,
which is ~1:1 with a substance and binds a handful of names at most.

Counting is directional and on normalized names, so a real flow that merely HAS
many synonyms (high out-degree, e.g. @"acetaminophen"@ with its trade names) is
never touched — only a name that ACTS as a synonym for many distinct flows is.
Returns the kept pairs and the excluded tokens with their flow counts
(descending), so the caller can surface the exclusion list, not drop it silently.
-}
excludeOverFrequentSynonyms :: Int -> [(Text, Text)] -> ([(Text, Text)], [(Text, Int)])
excludeOverFrequentSynonyms maxFlows pairs = (kept, excluded)
  where
    normed = [(p, normalizeName base, normalizeName syn) | p@(base, syn) <- pairs]
    flowsPerSynonym = M.fromListWith S.union [(ns, S.singleton nb) | (_, nb, ns) <- normed]
    overFrequent = M.filter (> maxFlows) (M.map S.size flowsPerSynonym)
    kept = [p | (p, _, ns) <- normed, not (ns `M.member` overFrequent)]
    excluded = sortOn (negate . snd) (M.toList overFrequent)

{- | Is this name an obvious non-synonym — a REACH/ILCD dossier placeholder
(@"not available"@, @"unknown"@, @"active matter"@, an ECHA id stub), or a bare
database identifier (a PubChem CID, an @"ENT 27164"@ registry number) — rather
than a substance name? These survive 'excludeOverFrequentSynonyms' (each is
carried by few flows) yet act as cut-vertices that fuse unrelated substances
through long chains, so they are dropped by string shape instead of frequency.
(@"ENT 27164"@ and @"ENT 27,164"@ both normalize to @"ent 27164"@, fusing carbon
tetrachloride and carbofuran through one shared dossier number.)

Deliberately conservative — it matches only forms that no real substance name
takes. A name carrying letters survives, so @"mixture"@ and digit-heavy names are
kept (@"toluenediisocyanate (mixture)"@, @"pcb-1254"@, @"carbon 14"@); only an
all-digit token or a known registry prefix followed by digits is dropped. The
@echa-@ check is anchored so it cannot fire on the @echa@ inside a word (e.g.
French @"huile de chauffage"@).
-}
isJunkSynonymName :: Text -> Bool
isJunkSynonymName name =
    n `elem` exactStops
        || any (`T.isInfixOf` n) infixStops
        || "unknown" `T.isPrefixOf` n
        || "echa-" `T.isPrefixOf` n
        || "echa_" `T.isPrefixOf` n
        || isRegistryId n
  where
    n = normalizeName name
    exactStops = ["none", "no data", "not applicable", "not assigned", "not specified"]
    infixStops =
        [ "available"
        , "confidential"
        , "active matter"
        , "activematter"
        , "active substance"
        , "activesubstance"
        , "active ingredient"
        , "activeingredient"
        ]
    -- A bare numeric identifier (a PubChem CID) or a known registry prefix
    -- followed by digits (USDA @"ent 27164"@, @"cipac 12"@) is a database id, not
    -- a name. A token carrying letters is never matched, so @"pcb-1254"@ and
    -- @"carbon 14"@ survive.
    isRegistryId t =
        (not (T.null t) && T.all isDigit t)
            || case T.words t of
                [pfx, num] -> pfx `elem` registryPrefixes && not (T.null num) && T.all isDigit num
                _ -> False
    registryPrefixes = ["ent", "cipac"]

{- | Drop synonym pairs touching a junk placeholder name ('isJunkSynonymName').
Returns the kept pairs and the distinct (normalized) junk tokens dropped, so the
caller can surface them rather than discard them silently.
-}
excludeJunkSynonyms :: [(Text, Text)] -> ([(Text, Text)], [Text])
excludeJunkSynonyms pairs = (kept, excluded)
  where
    kept = [p | p@(a, b) <- pairs, not (isJunkSynonymName a), not (isJunkSynonymName b)]
    excluded =
        S.toList . S.fromList $
            [normalizeName x | (a, b) <- pairs, x <- [a, b], isJunkSynonymName x]

{- | Normalize a name for lookup in the synonym database

Normalization rules:
- Lowercase
- Strip leading/trailing whitespace
- Collapse multiple spaces to single space
- Strip ", in ground" suffix (ecoinvent resource naming)
- Strip a trailing SimaPro unit suffix ("/kg", "/m3", "/Sm3")
- Remove punctuation: commas, parentheses, quotes
-}
normalizeName :: Text -> Text
normalizeName name =
    let
        -- Lowercase and strip
        t1 = T.strip $ T.toLower name
        -- Collapse whitespace
        t2 = T.unwords $ T.words t1
        -- Strip ", in ground" suffix
        t3 = stripSuffix ", in ground" $ stripSuffix " in ground" t2
        -- Strip a trailing SimaPro unit suffix; see 'unitSuffixes'.
        t4 = foldr stripSuffix t3 unitSuffixes
        -- Remove punctuation. Inlined char predicate: the old version used
        -- @T.filter (`notElem` (",()'\"" :: String))@ which forces 'T.filter'
        -- to traverse a 5-cons-cell @[Char]@ list per input character. With
        -- 'normalizeName' fired ~837K times during a single LCIA warmup, this
        -- alone took 27% of the warmup CPU.
        t5 = T.filter notPunctChar t4
        -- Collapse whitespace again (from removed punctuation)
        t6 = T.unwords $ T.words t5
     in
        t6
  where
    notPunctChar :: Char -> Bool
    notPunctChar c = c /= ',' && c /= '(' && c /= ')' && c /= '\'' && c /= '"'
    stripSuffix :: Text -> Text -> Text
    stripSuffix suffix txt =
        if suffix `T.isSuffixOf` txt
            then T.dropEnd (T.length suffix) txt
            else txt

{- | Unit suffixes that 'normalizeName' strips. SimaPro bakes a flow's unit into
its name (e.g. @"Gas, natural/m3"@); dropping the suffix lets a unit variant
share the registry node — and thus the CF — of its base resource.

MUST be lowercase: 'normalizeName' lowercases before it strips, so an uppercase
entry would never fire. Extending this list is the fix when 'uncoveredUnitSuffixes'
warns that a loaded database carries an un-stripped @/unit@ suffix.
-}
unitSuffixes :: [Text]
unitSuffixes = ["/kg", "/m3", "/sm3"]

{- | Flow names that will silently miss CF matching because they carry a trailing
@"/unit"@ for a real unit 'unitSuffixes' does not strip. Grouped by the offending
unit (each value lists example flow names) so a load-time warning is actionable —
add @"/unit"@ to 'unitSuffixes'. Empty when coverage is complete.

The unit test is supplied by the caller (@UnitConversion.isKnownUnit cfg@), so this
module stays free of a unit-system dependency.
-}
uncoveredUnitSuffixes :: (Text -> Bool) -> [Text] -> M.Map Text [Text]
uncoveredUnitSuffixes isUnit names =
    M.fromListWith
        (<>)
        [ (seg, [name])
        | name <- names
        , let (prefix, seg) = T.breakOnEnd "/" name
        , not (T.null prefix)
        , isUnit seg
        , ("/" <> T.toLower seg) `notElem` unitSuffixes
        ]

-- | Look up the synonym group ID for a flow name
lookupSynonymGroup :: SynonymDB -> Text -> Maybe Int
lookupSynonymGroup db name =
    M.lookup (normalizeName name) (synNameToId db)

-- | Get all synonyms for a group ID
getSynonyms :: SynonymDB -> Int -> Maybe [Text]
getSynonyms db gid = M.lookup gid (synIdToNames db)
