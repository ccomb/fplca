{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : EcoSpold.Loader
Description : High-performance EcoSpold XML loading with matrix caching

This module provides optimized loading of EcoSpold XML files together with a
single cache storing the fully indexed database and pre-computed sparse
matrices. When the cache is absent or invalidated, the loader reparses all
EcoSpold datasets, builds the in-memory structures, and writes the matrix cache
for subsequent runs.

Key performance features:
- Parallel parsing with controlled concurrency (prevents resource exhaustion)
- Automatic cache invalidation based on source file changes
- Memory-efficient chunked processing for large databases
- Hash-based cache filenames for multi-dataset support

Cache performance (Ecoinvent 3.8 with 18K activities):
- Cold start (XML parsing + matrix build): ~45s
- Matrix cache hit: ~0.5s

The cache keeps day-to-day execution fast while preserving reproducibility.
-}

module EcoSpold.Loader (loadAllSpoldsWithFlows, loadCachedDatabaseWithMatrices, saveCachedDatabaseWithMatrices, loadDatabaseFromCacheFile) where

import LCA.Progress
import LCA.Types
import Control.Concurrent.Async
import Control.DeepSeq (force)
import Data.Char (toLower)
import Control.Exception (SomeException, catch, evaluate)
import Control.Monad
import Data.Binary (encode, decode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Hashable (hash)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import qualified Data.UUID as UUID
import qualified Data.UUID.V5 as UUID5
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Codec.Compression.Zstd as Zstd
import EcoSpold.Parser (streamParseActivityAndFlowsFromFile)
import EcoSpold.Parser1 (streamParseActivityAndFlowsFromFile1)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Directory (doesFileExist, getFileSize, listDirectory, removeFile)
import System.FilePath (takeBaseName, takeExtension, (</>))
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)

{-|
Cache format version - increment when data structures change.

This version number is embedded in cache filenames to automatically
invalidate caches when the underlying Haskell data types change
(Activity, Flow, Exchange, Unit, etc.).

Increment this number when:
- Adding/removing/changing fields in core data types
- Changing serialization format or compression
- Modifying matrix computation algorithms

Version history:
- Version 8: Matrix pre-computation caching
- Version 9: Improved error handling with cache validation and exception catching
- Version 10: Fixed reference product identification to prevent negative inputs from overwriting activity unit
- Version 11: Memory optimization - changed dbBiosphereFlows from lazy list to strict Vector (saves ~300-500MB)
- Version 12: Major memory optimization - removed unused exchange indexes (saves ~3-4GB by eliminating Exchange duplication)
- Version 13: Force strict evaluation during deserialization - added NFData instances and evaluate . force to prevent lazy thunk buildup during cache loading
- Version 14: MAJOR: UUID type conversion from Text (~80 bytes) to Data.UUID.UUID (16 bytes) - saves ~2-3GB of RAM from ~100,000+ UUID instances
- Version 15: MAJOR: Sparse triple unboxed optimization - converted from boxed Vector (Int32, Int32, Double) to unboxed VU.Vector SparseTriple with UNPACK pragmas - saves ~25MB and eliminates 800K heap objects from GC tracking
-}
cacheFormatVersion :: Int
cacheFormatVersion = 16  -- v16: Added techLocation/bioLocation fields to Exchange types

{-|
Helper function to parse UUID from Text with deterministic UUID generation fallback.
Uses the same namespace as Parser.hs to ensure consistency.
-}
testDataNamespace :: UUID.UUID
testDataNamespace = UUID5.generateNamed UUID5.namespaceURL (BS.unpack $ T.encodeUtf8 "acvengine.test")

parseUUID :: T.Text -> UUID.UUID
parseUUID txt = fromMaybe (UUID5.generateNamed testDataNamespace (BS.unpack $ T.encodeUtf8 txt)) (UUID.fromText txt)

-- | Namespace for EcoSpold1 UUID generation
ecospold1Namespace :: UUID.UUID
ecospold1Namespace = UUID5.generateNamed UUID5.namespaceURL (BS.unpack $ T.encodeUtf8 "ecospold1.ecoinvent.org")

-- | Generate activity UUID from activity name and location (for EcoSpold1)
generateActivityUUIDFromActivity :: Activity -> UUID.UUID
generateActivityUUIDFromActivity act =
    let key = activityName act <> ":" <> activityLocation act
    in UUID5.generateNamed ecospold1Namespace (BS.unpack $ T.encodeUtf8 key)

-- | Get reference product UUID from activity exchanges
getReferenceProductUUID :: Activity -> UUID.UUID
getReferenceProductUUID act =
    case filter exchangeIsReference (exchanges act) of
        (ref:_) -> exchangeFlowId ref
        [] -> UUID.nil  -- No reference product found

-- | Type alias for supplier lookup index
type SupplierIndex = M.Map (T.Text, T.Text) (UUID.UUID, UUID.UUID)

-- | Normalize text for matching: lowercase and strip whitespace
normalizeText :: T.Text -> T.Text
normalizeText = T.toLower . T.strip

-- | Build supplier index: (normalizedProductName, location) → (activityUUID, productUUID)
-- For each activity, we index it by its reference product name + activity location
buildSupplierIndex :: ActivityMap -> FlowDB -> SupplierIndex
buildSupplierIndex activities flowDb = M.fromList
    [ ((normalizeText (flowName flow), activityLocation act), (actUUID, prodUUID))
    | ((actUUID, prodUUID), act) <- M.toList activities
    , ex <- exchanges act
    , exchangeIsReference ex
    , Just flow <- [M.lookup (exchangeFlowId ex) flowDb]
    ]

-- | Fix EcoSpold1 activity links by resolving supplier references
-- Uses (flowName, flowLocation) to look up the correct supplier activity
fixEcoSpold1ActivityLinks :: SimpleDatabase -> IO SimpleDatabase
fixEcoSpold1ActivityLinks db = do
    let supplierIndex = buildSupplierIndex (sdbActivities db) (sdbFlows db)
    reportProgress Info $ printf "Built supplier index with %d entries for activity linking" (M.size supplierIndex)

    -- Count and report statistics
    let (fixedActivities, linkStats) = fixAllActivities supplierIndex (sdbFlows db) (sdbActivities db)
    let (totalLinks, foundLinks, missingLinks) = linkStats

    reportProgress Info $ printf "Activity linking: %d/%d resolved (%.1f%%), %d unresolved"
        foundLinks totalLinks
        (if totalLinks > 0 then 100.0 * fromIntegral foundLinks / fromIntegral totalLinks else 0.0 :: Double)
        missingLinks

    return $ db { sdbActivities = fixedActivities }

-- | Fix all activities and return statistics
fixAllActivities :: SupplierIndex -> FlowDB -> ActivityMap -> (ActivityMap, (Int, Int, Int))
fixAllActivities idx flowDb activities =
    let results = M.map (\act -> fixActivityExchanges idx flowDb act) activities
        statsList = map snd $ M.elems results
        totalLinks = sum $ map (\(t, _, _) -> t) statsList
        foundLinks = sum $ map (\(_, f, _) -> f) statsList
        missingLinks = sum $ map (\(_, _, m) -> m) statsList
        fixedActivities = M.map fst results
    in (fixedActivities, (totalLinks, foundLinks, missingLinks))

-- | Fix activity exchanges and return (fixed activity, (total, found, missing))
fixActivityExchanges :: SupplierIndex -> FlowDB -> Activity -> (Activity, (Int, Int, Int))
fixActivityExchanges idx flowDb act =
    let (fixedExchanges, stats) = unzip $ map (fixExchangeLink idx flowDb (activityName act)) (exchanges act)
        totalLinks = sum $ map (\(t, _, _) -> t) stats
        foundLinks = sum $ map (\(_, f, _) -> f) stats
        missingLinks = sum $ map (\(_, _, m) -> m) stats
    in (act { exchanges = fixedExchanges }, (totalLinks, foundLinks, missingLinks))

-- | Fix a single exchange's activity link
-- Returns (fixed exchange, (total attempts, found, missing))
fixExchangeLink :: SupplierIndex -> FlowDB -> T.Text -> Exchange -> (Exchange, (Int, Int, Int))
fixExchangeLink idx flowDb consumerName ex@(TechnosphereExchange fid amt uid isInp isRef _ procLink loc)
    | isInp && not (T.null loc) =  -- Only fix technosphere inputs with location
        case M.lookup fid flowDb of
            Just flow ->
                let key = (normalizeText (flowName flow), loc)
                in case M.lookup key idx of
                    Just (actUUID, prodUUID) ->
                        -- Found supplier: update both activityLinkId AND flowId to match supplier's reference product
                        -- This is critical because the matrix lookup uses (activityLinkId, flowId) as the key
                        (TechnosphereExchange prodUUID amt uid isInp isRef actUUID procLink loc, (1, 1, 0))
                    Nothing -> do
                        -- Supplier not found - log warning
                        let !_ = unsafePerformIO $ hPutStrLn stderr $
                                "[WARNING] No supplier found for technosphere input:\n" ++
                                "  Flow: \"" ++ T.unpack (flowName flow) ++ "\"\n" ++
                                "  Location: \"" ++ T.unpack loc ++ "\"\n" ++
                                "  Consumer: \"" ++ T.unpack consumerName ++ "\""
                        (ex, (1, 0, 1))
            Nothing ->
                -- Flow not in database - shouldn't happen but be safe
                (ex, (1, 0, 1))
    | otherwise = (ex, (0, 0, 0))  -- Not a linkable exchange
fixExchangeLink _ _ _ ex = (ex, (0, 0, 0))  -- BiosphereExchange - no linking needed

{-|
Load all EcoSpold files with optimized parallel processing and deduplication.

This function implements a high-performance loading strategy:
1. **Chunked Processing**: Split files into optimal chunks (500 files/chunk)
2. **Controlled Parallelism**: Limit concurrent file handles (4 max)
3. **Memory Management**: Process chunks sequentially to control memory usage
4. **Deduplication**: Automatic flow and unit deduplication across files

Performance characteristics:
- Memory usage: ~2-4GB peak for Ecoinvent 3.8
- Processing time: ~45s for 18K activities (cold start)
- Parallelism: 4x concurrent file parsing within chunks
- Chunk size: 500 files (optimal for memory vs parallelism trade-off)

Used when no cache exists or caching is disabled.
-}
loadAllSpoldsWithFlows :: FilePath -> IO SimpleDatabase
loadAllSpoldsWithFlows dir = do
    reportProgress Info "Scanning directory for EcoSpold files"
    files <- listDirectory dir
    -- Support both EcoSpold2 (.spold) and EcoSpold1 (.XML/.xml) files
    let spold2Files = [dir </> f | f <- files, takeExtension f == ".spold"]
    let spold1Files = [dir </> f | f <- files, map toLower (takeExtension f) == ".xml"]

    -- Determine which format to use based on what's found
    let (spoldFiles, formatName, isEcoSpold1) = case (spold2Files, spold1Files) of
            ([], []) -> error $ "No EcoSpold files found in directory: " ++ dir
            ([], xs) -> (xs, "EcoSpold1 (.XML)", True)
            (xs, []) -> (xs, "EcoSpold2 (.spold)", False)
            (xs, _)  -> (xs, "EcoSpold2 (.spold)", False)  -- Prefer EcoSpold2 if both present

    reportProgress Info $ "Found " ++ show (length spoldFiles) ++ " " ++ formatName ++ " files for processing"

    -- Simple N-worker parallelism: divide files among CPU cores
    loadWithWorkerParallelism spoldFiles isEcoSpold1
  where
    numWorkers = 4  -- Number of parallel workers (match CPU cores)

    -- Helper function for unzipping 5-tuples
    unzip5 :: [(a, b, c, d, e)] -> ([a], [b], [c], [d], [e])
    unzip5 = foldr (\(a, b, c, d, e) (as, bs, cs, ds, es) -> (a:as, b:bs, c:cs, d:ds, e:es)) ([], [], [], [], [])

    -- Worker-based parallelism: divide files among N workers, all process in parallel
    loadWithWorkerParallelism :: [FilePath] -> Bool -> IO SimpleDatabase
    loadWithWorkerParallelism allFiles isEcoSpold1 = do
        let workers = distributeFiles numWorkers allFiles
        reportProgress Info $ printf "Processing %d files with %d parallel workers (%d files per worker)"
            (length allFiles) numWorkers (length allFiles `div` numWorkers)

        -- Process all workers in parallel
        startTime <- getCurrentTime
        results <- mapConcurrently (processWorker startTime isEcoSpold1) (zip [1..] workers)
        let (procMaps, flowMaps, unitMaps, rawFlowCounts, rawUnitCounts) = unzip5 results
        let !finalProcMap = M.unions procMaps
        let !finalFlowMap = M.unions flowMaps
        let !finalUnitMap = M.unions unitMaps

        endTime <- getCurrentTime
        let totalDuration = realToFrac $ diffUTCTime endTime startTime
        let totalFiles = length allFiles
        let avgFilesPerSec = fromIntegral totalFiles / totalDuration
        let totalRawFlows = sum rawFlowCounts
        let totalRawUnits = sum rawUnitCounts
        let flowDeduplication = if totalRawFlows > 0 then 100.0 * (1.0 - fromIntegral (M.size finalFlowMap) / fromIntegral totalRawFlows) else 0.0 :: Double
        let unitDeduplication = if totalRawUnits > 0 then 100.0 * (1.0 - fromIntegral (M.size finalUnitMap) / fromIntegral totalRawUnits) else 0.0 :: Double

        reportProgress Info $ printf "Parsing completed (%s, %.1f files/sec):" (formatDuration totalDuration) avgFilesPerSec
        reportProgress Info $ printf "  Activities: %d processes" (M.size finalProcMap)
        reportProgress Info $ printf "  Flows: %d unique (%.1f%% deduplication from %d raw)"
            (M.size finalFlowMap) flowDeduplication totalRawFlows
        reportProgress Info $ printf "  Units: %d unique (%.1f%% deduplication from %d raw)"
            (M.size finalUnitMap) unitDeduplication totalRawUnits
        reportMemoryUsage "Final parsing memory usage"

        -- For EcoSpold1: fix activity links using supplier lookup table
        let simpleDb = SimpleDatabase finalProcMap finalFlowMap finalUnitMap
        if isEcoSpold1
            then fixEcoSpold1ActivityLinks simpleDb
            else return simpleDb

    -- Distribute files evenly among N workers
    distributeFiles :: Int -> [a] -> [[a]]
    distributeFiles n xs =
        let len = length xs
            baseSize = len `div` n
            remainder = len `mod` n
            -- First 'remainder' workers get baseSize+1 files, rest get baseSize
            sizes = replicate remainder (baseSize + 1) ++ replicate (n - remainder) baseSize
        in distribute sizes xs
      where
        distribute [] _ = []
        distribute _ [] = []
        distribute (s:ss) items = take s items : distribute ss (drop s items)

    -- Process one worker's share of files
    processWorker :: UTCTime -> Bool -> (Int, [FilePath]) -> IO (ActivityMap, FlowDB, UnitDB, Int, Int)
    processWorker startTime isEcoSpold1 (workerNum, workerFiles) = do
        workerStartTime <- getCurrentTime
        reportProgress Info $ printf "Worker %d started: processing %d files" workerNum (length workerFiles)

        -- Parse all files for this worker using appropriate parser
        let parseFile = if isEcoSpold1
                        then streamParseActivityAndFlowsFromFile1
                        else streamParseActivityAndFlowsFromFile
        workerResults <- mapM parseFile workerFiles
        let (!procs, flowLists, unitLists) = unzip3 workerResults
        let !allFlows = concat flowLists
        let !allUnits = concat unitLists

        -- Build maps for this worker - extract UUID pairs from filenames
        -- For EcoSpold1: generate UUIDs from numeric dataset number
        -- For EcoSpold2: parse UUIDs from filename (activityUUID_productUUID.spold)
        let !procMap = M.fromList $ zipWith (\filepath activity ->
                if isEcoSpold1
                then
                    -- EcoSpold1: Generate activity UUID from name and location
                    -- Get the reference product UUID from exchanges
                    let actUUID = generateActivityUUIDFromActivity activity
                        prodUUID = getReferenceProductUUID activity
                    in ((actUUID, prodUUID), activity)
                else
                    -- EcoSpold2: Parse UUIDs from filename
                    let filename = T.pack $ takeBaseName filepath
                    in case T.splitOn "_" filename of
                        [actUUIDText, prodUUIDText] ->
                            -- Parse UUIDs, generating deterministic UUIDs for invalid test data
                            -- This prevents deduplication issues where all invalid UUIDs would map to nil
                            let actUUID = parseUUID actUUIDText
                                prodUUID = parseUUID prodUUIDText
                            in ((actUUID, prodUUID), activity)
                        _ -> error $ "Invalid filename format (expected activityUUID_productUUID.spold): " ++ filepath
                ) workerFiles procs
        let !flowMap = M.fromList [(flowId f, f) | f <- allFlows]
        let !unitMap = M.fromList [(unitId u, u) | u <- allUnits]

        workerEndTime <- getCurrentTime
        let workerDuration = realToFrac $ diffUTCTime workerEndTime workerStartTime
        let filesPerSec = fromIntegral (length workerFiles) / workerDuration
        let rawFlowCount = length allFlows
        let rawUnitCount = length allUnits
        reportProgress Info $ printf "Worker %d completed: %d activities, %d flows (%s, %.1f files/sec)"
            workerNum (M.size procMap) (M.size flowMap) (formatDuration workerDuration) filesPerSec

        return (procMap, flowMap, unitMap, rawFlowCount, rawUnitCount)

{-|
Generate filename for matrix cache (second-tier caching).

Matrix caches store pre-computed sparse matrices (technosphere A,
biosphere B) enabling direct LCA solving without matrix construction.

Uses separate filename pattern: "ecoinvent.matrix.v{VERSION}.{HASH}.bin"
-}
generateMatrixCacheFilename :: FilePath -> IO FilePath
generateMatrixCacheFilename dataDir = do
    files <- listDirectory dataDir
    -- Include both EcoSpold1 and EcoSpold2 files in hash
    let spold2Files = [f | f <- files, takeExtension f == ".spold"]
    let spold1Files = [f | f <- files, map toLower (takeExtension f) == ".xml"]
    let allSpoldFiles = spold2Files ++ spold1Files
    let filesHash = abs $ hash (show (dataDir, allSpoldFiles)) -- Hash both directory path and file list
    return $ "ecoinvent.matrix.v" ++ show cacheFormatVersion ++ "." ++ show filesHash ++ ".bin"

{-|
Validate cache file integrity before attempting to decode.

Checks:
- File size is reasonable (> 1KB to avoid empty/corrupted files)
- File exists and is readable

Returns True if cache file appears valid, False otherwise.
-}
validateCacheFile :: FilePath -> IO Bool
validateCacheFile cacheFile = do
    exists <- doesFileExist cacheFile
    if not exists
        then return False
        else do
            fileSize <- getFileSize cacheFile
            -- Cache file should be at least 1KB for a valid database
            -- Typical size is 100MB-600MB
            if fileSize < 1024
                then do
                    reportCacheOperation $ "Cache file is too small (" ++ show fileSize ++ " bytes), likely corrupted"
                    return False
                else return True

{-|
Load Database with pre-computed matrices from cache (second-tier).

This is the fastest loading method (~0.5s) as it bypasses both
XML parsing and matrix construction. The Database includes:
- All activities, flows, units (from SimpleDatabase)
- Pre-built indexes for fast querying
- Pre-computed sparse matrices (technosphere A, biosphere B)
- Activity and flow UUID mappings for matrix operations

Returns Nothing if no matrix cache exists.
-}
loadCachedDatabaseWithMatrices :: FilePath -> IO (Maybe Database)
loadCachedDatabaseWithMatrices dataDir = do
    cacheFile <- generateMatrixCacheFilename dataDir
    let zstdFile = cacheFile ++ ".zst"

    -- Try compressed file first, then fall back to uncompressed for backwards compatibility
    zstdExists <- doesFileExist zstdFile
    cacheExists <- doesFileExist cacheFile

    if zstdExists
        then do
            reportCacheInfo zstdFile
            -- Wrap cache loading in exception handler to prevent crashes
            catch
                (withProgressTiming Cache "Matrix cache load with zstd decompression" $ do
                    compressed <- BS.readFile zstdFile
                    db <- case Zstd.decompress compressed of
                        Zstd.Skip -> error "Zstd decompression failed: Skip"
                        Zstd.Error err -> error $ "Zstd decompression failed: " ++ show err
                        Zstd.Decompress decompressed -> do
                            let !db = decode (BSL.fromStrict decompressed)
                            -- Force full evaluation to prevent lazy thunk buildup
                            evaluate (force db)
                    reportCacheOperation $
                        "Matrix cache loaded: "
                        ++ show (dbActivityCount db)
                        ++ " activities, "
                        ++ show (VU.length $ dbTechnosphereTriples db)
                        ++ " tech entries, "
                        ++ show (VU.length $ dbBiosphereTriples db)
                        ++ " bio entries (decompressed)"
                    return (Just db))
                (\(e :: SomeException) -> do
                    reportError $ "Compressed cache load failed: " ++ show e
                    reportCacheOperation "The compressed cache file is corrupted or incompatible"
                    reportCacheOperation $ "Deleting corrupted cache file: " ++ zstdFile
                    removeFile zstdFile
                    reportCacheOperation "Will rebuild database from source files"
                    return Nothing)
    else if cacheExists
        then do
            reportCacheOperation "Found old uncompressed cache, migrating to compressed format"
            -- Validate cache file before attempting to decode
            isValid <- validateCacheFile cacheFile
            if not isValid
                then do
                    reportCacheOperation "Cache file validation failed, deleting and rebuilding"
                    removeFile cacheFile
                    return Nothing
                else do
                    reportCacheInfo cacheFile
                    -- Load old format and delete it (will be saved in new format)
                    catch
                        (withProgressTiming Cache "Matrix cache load (old format)" $ do
                            !db <- BSL.readFile cacheFile >>= \bs -> evaluate (force (decode bs))
                            reportCacheOperation $
                                "Matrix cache loaded: "
                                ++ show (dbActivityCount db)
                                ++ " activities, "
                                ++ show (VU.length $ dbTechnosphereTriples db)
                                ++ " tech entries, "
                                ++ show (VU.length $ dbBiosphereTriples db)
                                ++ " bio entries"
                            -- Delete old uncompressed cache
                            removeFile cacheFile
                            reportCacheOperation "Deleted old uncompressed cache (will be saved in compressed format)"
                            return (Just db))
                        (\(e :: SomeException) -> do
                            reportError $ "Cache load failed: " ++ show e
                            reportCacheOperation "The cache file is corrupted or incompatible with the current version"
                            reportCacheOperation $ "Deleting corrupted cache file: " ++ cacheFile
                            removeFile cacheFile
                            reportCacheOperation "Will rebuild database from source files"
                            return Nothing)
        else do
            reportCacheOperation "No matrix cache found"
            return Nothing

{-|
Load Database directly from a specified cache file.

Similar to loadCachedDatabaseWithMatrices but takes an explicit cache file path
instead of generating it from a data directory. Supports both compressed (.bin.zst)
and uncompressed (.bin) formats.

This is useful for deploying just the cache file without the original .spold files.

Returns Nothing if the file cannot be loaded.
-}
loadDatabaseFromCacheFile :: FilePath -> IO (Maybe Database)
loadDatabaseFromCacheFile cacheFile = do
    let ext = takeExtension cacheFile
    let isCompressed = ext == ".zst"

    -- Validate file exists
    fileExists <- doesFileExist cacheFile
    if not fileExists
        then do
            reportError $ "Cache file not found: " ++ cacheFile
            return Nothing
        else do
            if isCompressed
                then loadCompressedCacheFile cacheFile
                else loadUncompressedCacheFile cacheFile

-- | Load compressed (.bin.zst) cache file
loadCompressedCacheFile :: FilePath -> IO (Maybe Database)
loadCompressedCacheFile zstdFile = do
    reportCacheInfo zstdFile
    catch
        (withProgressTiming Cache "Matrix cache load with zstd decompression" $ do
            compressed <- BS.readFile zstdFile
            db <- case Zstd.decompress compressed of
                Zstd.Skip -> error "Zstd decompression failed: Skip"
                Zstd.Error err -> error $ "Zstd decompression failed: " ++ show err
                Zstd.Decompress decompressed -> do
                    let !db = decode (BSL.fromStrict decompressed)
                    -- Force full evaluation to prevent lazy thunk buildup
                    evaluate (force db)
            reportCacheOperation $
                "Matrix cache loaded: "
                ++ show (dbActivityCount db)
                ++ " activities, "
                ++ show (VU.length $ dbTechnosphereTriples db)
                ++ " tech entries, "
                ++ show (VU.length $ dbBiosphereTriples db)
                ++ " bio entries (decompressed)"
            return (Just db))
        (\(e :: SomeException) -> do
            reportError $ "Compressed cache load failed: " ++ show e
            reportCacheOperation "The compressed cache file is corrupted or incompatible"
            return Nothing)

-- | Load uncompressed (.bin) cache file
loadUncompressedCacheFile :: FilePath -> IO (Maybe Database)
loadUncompressedCacheFile cacheFile = do
    -- Validate cache file before attempting to decode
    isValid <- validateCacheFile cacheFile
    if not isValid
        then do
            reportCacheOperation "Cache file validation failed"
            return Nothing
        else do
            reportCacheInfo cacheFile
            catch
                (withProgressTiming Cache "Matrix cache load" $ do
                    !db <- BSL.readFile cacheFile >>= \bs -> evaluate (force (decode bs))
                    reportCacheOperation $
                        "Matrix cache loaded: "
                        ++ show (dbActivityCount db)
                        ++ " activities, "
                        ++ show (VU.length $ dbTechnosphereTriples db)
                        ++ " tech entries, "
                        ++ show (VU.length $ dbBiosphereTriples db)
                        ++ " bio entries"
                    return (Just db))
                (\(e :: SomeException) -> do
                    reportError $ "Cache load failed: " ++ show e
                    reportCacheOperation "The cache file is corrupted or incompatible with the current version"
                    return Nothing)

{-|
Save Database with pre-computed matrices to cache (second-tier).

Serializes the complete Database including sparse matrices to enable
ultra-fast startup (~0.5s load time). This is the most efficient
caching tier but requires the largest disk space (~100-200MB).

Should be called after matrix construction is complete.
-}
saveCachedDatabaseWithMatrices :: FilePath -> Database -> IO ()
saveCachedDatabaseWithMatrices dataDir db = do
    cacheFile <- generateMatrixCacheFilename dataDir
    let zstdFile = cacheFile ++ ".zst"
    reportCacheOperation $ "Saving Database with matrices to compressed cache: " ++ zstdFile
    withProgressTiming Cache "Matrix cache save with zstd compression" $ do
        -- Serialize to ByteString
        let serialized = encode db
        -- Compress with zstd (level 3 = good balance of compression and speed)
        -- Convert lazy to strict for compression, then back to lazy for writing
        let compressed = Zstd.compress 3 (BSL.toStrict serialized)
        -- Write compressed data
        BS.writeFile zstdFile compressed
        reportCacheOperation $
            "Matrix cache saved ("
            ++ show (dbActivityCount db)
            ++ " activities, "
            ++ show (VU.length $ dbTechnosphereTriples db)
            ++ " tech entries, "
            ++ show (VU.length $ dbBiosphereTriples db)
            ++ " bio entries, compressed)"
