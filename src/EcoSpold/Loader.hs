{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EcoSpold.Loader (loadAllSpoldsWithFlows, loadCachedSpoldsWithFlows, saveCachedSpoldsWithFlows) where

import ACV.Query (buildIndexes)
import ACV.Types
import Control.Concurrent.Async
import Control.Exception (SomeException, catch)
import Control.Monad
import Control.Parallel.Strategies
import Data.Binary (decodeFile, encodeFile)
import Data.Hashable (hash)
import Data.List (isInfixOf, isPrefixOf)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (diffUTCTime, getCurrentTime)
import EcoSpold.Parser (streamParseActivityAndFlowsFromFile)
import GHC.Conc (getNumCapabilities)
import System.Directory (doesFileExist, getModificationTime, listDirectory, removeFile)
import System.FilePath (dropExtension, takeExtension, (</>))
import System.IO (hPutStrLn, stderr)

type SpoldIndex = M.Map UUID FilePath
type Visited = S.Set UUID

-- | Cache format version - increment this when types change to invalidate old caches
cacheFormatVersion :: Int
cacheFormatVersion = 7 -- Increment when Activity/Flow/Exchange/Unit types change

-- | Generate cache filename based on data directory, file list, and version
generateCacheFilename :: FilePath -> IO FilePath
generateCacheFilename dataDir = do
    files <- listDirectory dataDir
    let spoldFiles = [f | f <- files, takeExtension f == ".spold"]
    let filesHash = abs $ hash (show (dataDir, spoldFiles)) -- Hash both directory path and file list
    return $ "ecoinvent.cache.v" ++ show cacheFormatVersion ++ "." ++ show filesHash ++ ".bin"

-- | Find and clean up cache files with different format versions for the same dataset only
cleanupOldCaches :: FilePath -> IO ()
cleanupOldCaches dataDir = do
    currentCache <- generateCacheFilename dataDir
    files <- listDirectory "."

    -- Extract hash from current cache filename: "ecoinvent.cache.v7.HASH.bin" -> "HASH"
    let currentHash = case reverse (take 2 (reverse (words (map (\c -> if c == '.' then ' ' else c) currentCache)))) of
            [hash, "bin"] -> hash
            _ -> "" -- Fallback if parsing fails

    -- Only remove caches with same hash but different version
    let cacheFiles =
            [ f | f <- files, "ecoinvent.cache.v" `isPrefixOf` f, f /= currentCache, currentHash `isInfixOf` f, not (("ecoinvent.cache.v" ++ show cacheFormatVersion ++ ".") `isPrefixOf` f) -- Don't remove current cache
            -- Same dataset (same hash)
            -- Different version
            ]
    mapM_ removeOldCache cacheFiles
  where
    removeOldCache cacheFile = do
        hPutStrLn stderr $ "Removing old format version cache: " ++ cacheFile
        catch (removeFile cacheFile) (\(_ :: SomeException) -> return ())

{- | Version optimisée avec déduplication des flux (sans index)
  Charge tous les fichiers .spold et déduplique les flux
-}
loadAllSpoldsWithFlows :: FilePath -> IO SimpleDatabase
loadAllSpoldsWithFlows dir = do
    hPutStrLn stderr "listing directory"
    files <- listDirectory dir
    hPutStrLn stderr "getting spold files"
    let spoldFiles = [dir </> f | f <- files, takeExtension f == ".spold"]
    hPutStrLn stderr $ "Found " ++ show (length spoldFiles) ++ " spold files"

    -- OPTION B: Simple chunking - optimal chunk size with direct parallelism
    loadWithSimpleChunks spoldFiles
  where
    optimalChunkSize = 500 -- Sweet spot for memory vs parallelism

    -- Simple chunking without complex nested batching
    loadWithSimpleChunks :: [FilePath] -> IO SimpleDatabase
    loadWithSimpleChunks allFiles = do
        let chunks = chunksOf optimalChunkSize allFiles
        hPutStrLn stderr $ "Processing " ++ show (length chunks) ++ " chunks of " ++ show optimalChunkSize ++ " files each"

        -- Activity chunks sequentially, but files within each chunk in parallel
        results <- mapM activityChunkSimple chunks
        let (procMaps, flowMaps, unitMaps) = unzip3 results
        let !finalProcMap = M.unions procMaps
        let !finalFlowMap = M.unions flowMaps
        let !finalUnitMap = M.unions unitMaps

        hPutStrLn stderr $ "Final: " ++ show (M.size finalProcMap) ++ " activities, " ++ show (M.size finalFlowMap) ++ " flows, " ++ show (M.size finalUnitMap) ++ " units"
        return $ SimpleDatabase finalProcMap finalFlowMap finalUnitMap

    -- Activity one chunk: all files in chunk processed in parallel
    activityChunkSimple :: [FilePath] -> IO (ActivityDB, FlowDB, UnitDB)
    activityChunkSimple chunk = do
        hPutStrLn stderr $ "Processing chunk of " ++ show (length chunk) ++ " files in parallel"

        -- All files in chunk processed in parallel
        chunkResults <- mapConcurrently streamParseActivityAndFlowsFromFile chunk
        let (!procs, flowLists, unitLists) = unzip3 chunkResults
        let !allFlows = concat flowLists
        let !allUnits = concat unitLists

        -- Build maps for this chunk
        let !procMap = M.fromList [(activityId p, p) | p <- procs]
        let !flowMap = M.fromList [(flowId f, f) | f <- allFlows]
        let !unitMap = M.fromList [(unitId u, u) | u <- allUnits]

        return (procMap, flowMap, unitMap)

    -- Utility function to split list into chunks
    chunksOf :: Int -> [a] -> [[a]]
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Load cached SimpleDatabase with automatic filename management
loadCachedSpoldsWithFlows :: FilePath -> IO (SimpleDatabase, Bool)
loadCachedSpoldsWithFlows dataDir = do
    cacheFile <- generateCacheFilename dataDir
    cleanupOldCaches dataDir

    cacheExists <- doesFileExist cacheFile
    if cacheExists
        then do
            hPutStrLn stderr $ "Loading SimpleDatabase from cache: " ++ cacheFile
            startTime <- getCurrentTime
            !db <- decodeFile cacheFile -- Simple decode, no error handling needed
            endTime <- getCurrentTime
            let elapsed = diffUTCTime endTime startTime
            hPutStrLn stderr $
                "Cache loaded in "
                    ++ show elapsed
                    ++ " ("
                    ++ show (M.size $ sdbActivities db)
                    ++ " activities, "
                    ++ show (M.size $ sdbFlows db)
                    ++ " flows)"
            return (db, True)
        else do
            hPutStrLn stderr "No cache found, parsing XML files..."
            startTime <- getCurrentTime
            !db <- loadAllSpoldsWithFlows dataDir
            endTime <- getCurrentTime
            let elapsed = diffUTCTime endTime startTime
            hPutStrLn stderr $
                "XML parsing completed in "
                    ++ show elapsed
                    ++ " ("
                    ++ show (M.size $ sdbActivities db)
                    ++ " activities, "
                    ++ show (M.size $ sdbFlows db)
                    ++ " flows)"
            return (db, False)

-- | Save SimpleDatabase to binary cache file with automatic filename
saveCachedSpoldsWithFlows :: FilePath -> SimpleDatabase -> IO ()
saveCachedSpoldsWithFlows dataDir db = do
    cacheFile <- generateCacheFilename dataDir
    hPutStrLn stderr $ "Saving SimpleDatabase to cache: " ++ cacheFile
    startTime <- getCurrentTime
    encodeFile cacheFile db -- Simple encode, filename ensures compatibility
    endTime <- getCurrentTime
    let elapsed = diffUTCTime endTime startTime
    hPutStrLn stderr $ "Cache saved in " ++ show elapsed
