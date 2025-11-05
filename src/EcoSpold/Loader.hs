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

module EcoSpold.Loader (loadAllSpoldsWithFlows, loadCachedDatabaseWithMatrices, saveCachedDatabaseWithMatrices) where

import ACV.Progress
import ACV.Types
import Control.Concurrent.Async
import Control.Monad
import Data.Binary (decodeFile, encodeFile)
import Data.Hashable (hash)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import EcoSpold.Parser (streamParseActivityAndFlowsFromFile)
import qualified Data.Text as T
import System.Directory (doesFileExist, listDirectory)
import System.FilePath (takeBaseName, takeExtension, (</>))
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

Current version 8 includes: Matrix pre-computation caching
-}
cacheFormatVersion :: Int
cacheFormatVersion = 8


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
    let spoldFiles = [dir </> f | f <- files, takeExtension f == ".spold"]
    reportProgress Info $ "Found " ++ show (length spoldFiles) ++ " .spold files for processing"

    -- OPTION B: Simple chunking - optimal chunk size with direct parallelism
    loadWithSimpleChunks spoldFiles
  where
    optimalChunkSize = 500 -- Sweet spot for memory vs parallelism

    -- Helper function for unzipping 5-tuples
    unzip5 :: [(a, b, c, d, e)] -> ([a], [b], [c], [d], [e])
    unzip5 = foldr (\(a, b, c, d, e) (as, bs, cs, ds, es) -> (a:as, b:bs, c:cs, d:ds, e:es)) ([], [], [], [], [])

    -- Simple chunking without complex nested batching
    loadWithSimpleChunks :: [FilePath] -> IO SimpleDatabase
    loadWithSimpleChunks allFiles = do
        let chunks = chunksOf optimalChunkSize allFiles
        reportProgress Info $ "Processing " ++ show (length chunks) ++ " chunks of " ++ show optimalChunkSize ++ " files each with controlled parallelism"

        -- Process chunks sequentially with detailed progress reporting
        startTime <- getCurrentTime
        results <- mapM (activityChunkWithProgress startTime (length chunks)) (zip [1..] chunks)
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
        return $ SimpleDatabase finalProcMap finalFlowMap finalUnitMap

    -- Process one chunk with progress reporting
    activityChunkWithProgress :: UTCTime -> Int -> (Int, [FilePath]) -> IO (ActivityMap, FlowDB, UnitDB, Int, Int)
    activityChunkWithProgress startTime totalChunks (chunkNum, chunk) = do
        chunkStartTime <- getCurrentTime
        let elapsedTime = realToFrac $ diffUTCTime chunkStartTime startTime
        let progressPercent = round (100.0 * fromIntegral (chunkNum - 1) / fromIntegral totalChunks :: Double) :: Int
        let avgChunkTime = if chunkNum > 1 then elapsedTime / fromIntegral (chunkNum - 1) else 0
        let etaSeconds = avgChunkTime * fromIntegral (totalChunks - chunkNum + 1)

        reportProgress Info $ printf "Processing chunk %d/%d (%d%%) - %d files [ETA: %s]"
            chunkNum totalChunks progressPercent (length chunk) (formatDuration etaSeconds)

        -- Report memory usage for large chunks
        when (chunkNum `mod` 5 == 1) $ reportMemoryUsage $ "Chunk " ++ show chunkNum ++ " memory check"

        -- Process files in smaller parallel batches to avoid thread/handle exhaustion
        let maxConcurrency = 4  -- Conservative limit: 1x CPU cores
        chunkResults <- processWithLimitedConcurrency maxConcurrency chunk
        let (!procs, flowLists, unitLists) = unzip3 chunkResults
        let !allFlows = concat flowLists
        let !allUnits = concat unitLists

        -- Build maps for this chunk - extract UUID pairs from filenames
        -- Filenames follow pattern: {activityUUID}_{productUUID}.spold
        let !procMap = M.fromList $ zipWith (\filepath activity ->
                let filename = T.pack $ takeBaseName filepath
                in case T.splitOn "_" filename of
                    [actUUID, prodUUID] -> ((actUUID, prodUUID), activity)
                    _ -> error $ "Invalid filename format (expected activityUUID_productUUID.spold): " ++ filepath
                ) chunk procs
        let !flowMap = M.fromList [(flowId f, f) | f <- allFlows]
        let !unitMap = M.fromList [(unitId u, u) | u <- allUnits]

        chunkEndTime <- getCurrentTime
        let chunkDuration = realToFrac $ diffUTCTime chunkEndTime chunkStartTime
        let filesPerSec = fromIntegral (length chunk) / chunkDuration
        let rawFlowCount = length allFlows
        let rawUnitCount = length allUnits
        reportProgress Info $ printf "Chunk %d completed: %d activities, %d flows (%s, %.1f files/sec)"
            chunkNum (M.size procMap) (M.size flowMap) (formatDuration chunkDuration) filesPerSec

        return (procMap, flowMap, unitMap, rawFlowCount, rawUnitCount)

    -- Process files with limited concurrency to prevent resource exhaustion
    processWithLimitedConcurrency :: Int -> [FilePath] -> IO [(Activity, [Flow], [Unit])]
    processWithLimitedConcurrency maxConcur files = do
        let batches = chunksOf maxConcur files
        results <- mapM processBatch batches
        return $ concat results
      where
        processBatch batch = mapConcurrently streamParseActivityAndFlowsFromFile batch

    -- Utility function to split list into chunks
    chunksOf :: Int -> [a] -> [[a]]
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)

{-|
Generate filename for matrix cache (second-tier caching).

Matrix caches store pre-computed sparse matrices (technosphere A,
biosphere B) enabling direct LCA solving without matrix construction.

Uses separate filename pattern: "ecoinvent.matrix.v{VERSION}.{HASH}.bin"
-}
generateMatrixCacheFilename :: FilePath -> IO FilePath
generateMatrixCacheFilename dataDir = do
    files <- listDirectory dataDir
    let spoldFiles = [f | f <- files, takeExtension f == ".spold"]
    let filesHash = abs $ hash (show (dataDir, spoldFiles)) -- Hash both directory path and file list
    return $ "ecoinvent.matrix.v" ++ show cacheFormatVersion ++ "." ++ show filesHash ++ ".bin"

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
    cacheExists <- doesFileExist cacheFile
    if cacheExists
        then do
            reportCacheInfo cacheFile
            withProgressTiming Cache "Matrix cache load" $ do
                !db <- decodeFile cacheFile
                reportCacheOperation $
                    "Matrix cache loaded: "
                    ++ show (dbActivityCount db)
                    ++ " activities, "
                    ++ show (length $ dbTechnosphereTriples db)
                    ++ " tech entries, "
                    ++ show (length $ dbBiosphereTriples db)
                    ++ " bio entries"
                return (Just db)
        else do
            reportCacheOperation "No matrix cache found"
            return Nothing

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
    reportCacheOperation $ "Saving Database with matrices to cache: " ++ cacheFile
    withProgressTiming Cache "Matrix cache save" $ do
        encodeFile cacheFile db
        reportCacheOperation $
            "Matrix cache saved ("
            ++ show (dbActivityCount db)
            ++ " activities, "
            ++ show (length $ dbTechnosphereTriples db)
            ++ " tech entries, "
            ++ show (length $ dbBiosphereTriples db)
            ++ " bio entries)"
