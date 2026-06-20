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
    loadFromCSVFileWithCache,

    -- * Lookup
    lookupSynonymGroup,
    getSynonyms,
    normalizeName,
    mergeSynonymDBs,
    synonymCount,
    oversizedClasses,

    -- * Re-exports
    SynonymDB (..),
    emptySynonymDB,
) where

import qualified Codec.Compression.Zstd as Zstd
import Control.DeepSeq (force)
import Control.Exception (SomeException, catch, evaluate)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Csv (HasHeader (..), decode)
import qualified Data.Map.Strict as M
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
buildFromPairs = fromClasses . equivalenceClasses . normalizePairs
  where
    normalizePairs :: [(Text, Text)] -> [(Text, Text)]
    normalizePairs raws =
        [ (n1, n2)
        | (raw1, raw2) <- raws
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
     in SynonymDB nameToId idToNames

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
declares A=B and another B=C, the merged DB groups {A,B,C}. Each group is
reconnected by a star to its first member (enough to preserve its connectivity),
then the whole edge set is closed again.
-}
mergeSynonymDBs :: [SynonymDB] -> SynonymDB
mergeSynonymDBs [] = emptySynonymDB
mergeSynonymDBs [db] = db
mergeSynonymDBs dbs = fromClasses (equivalenceClasses edges)
  where
    edges = concatMap groupEdges (concatMap (M.elems . synIdToNames) dbs)
    groupEdges [] = []
    groupEdges (m0 : ms) = [(normalizeName m0, normalizeName m) | m <- ms]

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

{- | Normalize a name for lookup in the synonym database

Normalization rules:
- Lowercase
- Strip leading/trailing whitespace
- Collapse multiple spaces to single space
- Strip ", in ground" suffix (ecoinvent resource naming)
- Strip "/kg" suffix (SimaPro unit convention)
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
        -- Strip "/kg" suffix
        t4 = stripSuffix "/kg" t3
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

-- | Look up the synonym group ID for a flow name
lookupSynonymGroup :: SynonymDB -> Text -> Maybe Int
lookupSynonymGroup db name =
    M.lookup (normalizeName name) (synNameToId db)

-- | Get all synonyms for a group ID
getSynonyms :: SynonymDB -> Int -> Maybe [Text]
getSynonyms db gid = M.lookup gid (synIdToNames db)
