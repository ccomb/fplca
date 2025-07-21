{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module EcoSpold.Loader (buildActivityTreeIO, buildSpoldIndex, loadAllSpolds, loadAllSpoldsWithFlows, loadAllSpoldsWithIndexes, loadCachedSpolds, saveCachedSpolds, loadCachedSpoldsWithFlows, saveCachedSpoldsWithFlows) where

import ACV.Query (buildIndexes)
import ACV.Types
import Control.Concurrent.Async
import Control.Exception (catch, SomeException)
import Control.Monad
import Control.Parallel.Strategies
import Data.List (isPrefixOf)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Binary (encodeFile, decodeFile)
import EcoSpold.Parser (parseActivityAndFlowsFromFile, parseActivityFromFile, streamParseActivityAndFlowsFromFile)
import GHC.Conc (getNumCapabilities)
import System.Directory (listDirectory, doesFileExist, getModificationTime, removeFile)
import System.FilePath (takeExtension, (</>))
import System.IO (hPutStrLn, stderr)
import Data.Time (getCurrentTime, diffUTCTime)
import Data.Hashable (hash)
import System.FilePath (dropExtension)

type SpoldIndex = M.Map UUID FilePath
type Visited = S.Set UUID

-- | Cache format version - increment this when types change to invalidate old caches
cacheFormatVersion :: Int
cacheFormatVersion = 6  -- Increment when Activity/Flow/Exchange/Unit types change

-- | Generate cache filename based on data directory and version
generateCacheFilename :: FilePath -> IO FilePath
generateCacheFilename dataDir = do
    files <- listDirectory dataDir
    let spoldFiles = [f | f <- files, takeExtension f == ".spold"]
    let filesHash = abs $ hash (show spoldFiles)  -- Hash of file list
    return $ "ecoinvent.cache.v" ++ show cacheFormatVersion ++ "." ++ show filesHash ++ ".bin"

-- | Find and clean up old cache files with different versions
cleanupOldCaches :: FilePath -> IO ()
cleanupOldCaches dataDir = do
    currentCache <- generateCacheFilename dataDir
    files <- listDirectory "."
    let cacheFiles = [f | f <- files, "ecoinvent.cache.v" `isPrefixOf` f, f /= currentCache]
    mapM_ removeOldCache cacheFiles
  where
    removeOldCache cacheFile = do
        hPutStrLn stderr $ "Removing old cache: " ++ cacheFile
        catch (removeFile cacheFile) (\(_ :: SomeException) -> return ())

buildSpoldIndex :: FilePath -> IO (M.Map UUID FilePath)
buildSpoldIndex dir = do
    files <- listDirectory dir
    let spoldFiles = [f | f <- files, takeExtension f == ".spold"]
    let pairs = [(T.pack (takeWhile (/= '_') f), dir </> f) | f <- spoldFiles]
    return $! M.fromList pairs -- Keep strict return to force Map evaluation

buildActivityTreeIO :: SpoldIndex -> UUID -> IO ActivityTree
buildActivityTreeIO index rootUuid = do
    -- Cette fonction ne peut plus fonctionner efficacement avec la nouvelle structure
    -- car elle nécessiterait de parser chaque fichier deux fois (une fois pour extraire les flux,
    -- une fois pour construire l'arbre). Il vaut mieux utiliser loadAllSpoldsWithFlows + buildActivityTreeWithFlows
    error "buildActivityTreeIO: Use loadAllSpoldsWithFlows + buildActivityTreeWithFlows instead for better performance"

-- | Fonction auxiliaire optimisée avec variants Exchange
isTechnosphereInput :: FlowDB -> Exchange -> Bool
isTechnosphereInput _ ex =
    case ex of
        TechnosphereExchange _ _ _ isInput isRef _ -> isInput && not isRef
        BiosphereExchange _ _ _ _ -> False

placeholder :: Activity
placeholder = Activity "loop" "Loop detected" ["Loop detected"] M.empty M.empty "N/A" "unit" []

missing :: Activity
missing = Activity "missing" "Activity not found" ["Activity not found"] M.empty M.empty "N/A" "unit" []

{- | Version originale - Charge tous les fichiers .spold
  et retourne une base de activités indexée par UUID
-}
loadAllSpolds :: FilePath -> IO ActivityDB
loadAllSpolds dir = do
    print "listing directory"
    files <- listDirectory dir
    print "getting spold files"
    let spoldFiles = [dir </> f | f <- files, takeExtension f == ".spold"]
    print $ "Found " ++ show (length spoldFiles) ++ " spold files"

    -- Load files in chunks to avoid memory explosion
    loadSpoldsInChunks spoldFiles M.empty
  where
    chunkSize = 1000 -- Activity 1000 files at a time
    loadSpoldsInChunks :: [FilePath] -> ActivityDB -> IO ActivityDB
    loadSpoldsInChunks [] acc = return acc
    loadSpoldsInChunks files acc = do
        let (chunk, rest) = splitAt chunkSize files
        print $ "Processing chunk of " ++ show (length chunk) ++ " files"

        -- Force evaluation of chunk to avoid building up thunks
        chunk' <- mapM parseActivityFromFile chunk
        let !chunkMap = M.fromList [(activityId p, p) | p <- chunk'] -- Keep strict for Map

        -- Force evaluation and merge
        let !newAcc = M.union acc chunkMap -- Keep strict for accumulator
        loadSpoldsInChunks rest newAcc

{- | Version optimisée avec déduplication des flux (sans index)
  Charge tous les fichiers .spold et déduplique les flux
-}
loadAllSpoldsWithFlows :: FilePath -> IO SimpleDatabase
loadAllSpoldsWithFlows dir = do
    print "listing directory"
    files <- listDirectory dir
    print "getting spold files"
    let spoldFiles = [dir </> f | f <- files, takeExtension f == ".spold"]
    print $ "Found " ++ show (length spoldFiles) ++ " spold files"

    -- OPTION B: Simple chunking - optimal chunk size with direct parallelism
    loadWithSimpleChunks spoldFiles
  where
    optimalChunkSize = 500 -- Sweet spot for memory vs parallelism

    -- Simple chunking without complex nested batching
    loadWithSimpleChunks :: [FilePath] -> IO SimpleDatabase
    loadWithSimpleChunks allFiles = do
        let chunks = chunksOf optimalChunkSize allFiles
        print $ "Processing " ++ show (length chunks) ++ " chunks of " ++ show optimalChunkSize ++ " files each"

        -- Activity chunks sequentially, but files within each chunk in parallel
        results <- mapM activityChunkSimple chunks
        let (procMaps, flowMaps, unitMaps) = unzip3 results
        let !finalProcMap = M.unions procMaps
        let !finalFlowMap = M.unions flowMaps
        let !finalUnitMap = M.unions unitMaps

        print $ "Final: " ++ show (M.size finalProcMap) ++ " activities, " ++ show (M.size finalFlowMap) ++ " flows, " ++ show (M.size finalUnitMap) ++ " units"
        return $ SimpleDatabase finalProcMap finalFlowMap finalUnitMap

    -- Activity one chunk: all files in chunk activityed in parallel
    activityChunkSimple :: [FilePath] -> IO (ActivityDB, FlowDB, UnitDB)
    activityChunkSimple chunk = do
        print $ "Processing chunk of " ++ show (length chunk) ++ " files in parallel"

        -- All files in chunk activityed in parallel
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

{- | Version complète avec déduplication des flux ET construction d'index
  Charge tous les fichiers .spold, déduplique les flux, et construit les index pour recherches efficaces
-}
loadAllSpoldsWithIndexes :: FilePath -> IO Database
loadAllSpoldsWithIndexes dir = do
    numCaps <- getNumCapabilities
    print $ "loading activities with flow deduplication and indexes using " ++ show numCaps ++ " cores"
    simpleDb <- loadAllSpoldsWithFlows dir

    print "building indexes for efficient queries"
    let indexes = buildIndexes (sdbActivities simpleDb) (sdbFlows simpleDb)

    return $ Database (sdbActivities simpleDb) (sdbFlows simpleDb) (sdbUnits simpleDb) indexes

{- | Load cached ActivityDB with automatic filename management
   Returns (database, wasFromCache)
-}
loadCachedSpolds :: FilePath -> IO (ActivityDB, Bool)
loadCachedSpolds dataDir = do
    cacheFile <- generateCacheFilename dataDir
    cleanupOldCaches dataDir
    
    cacheExists <- doesFileExist cacheFile
    if cacheExists
        then do
            hPutStrLn stderr $ "Loading from cache: " ++ cacheFile
            startTime <- getCurrentTime
            !db <- decodeFile cacheFile  -- Simple decode, no error handling needed
            endTime <- getCurrentTime
            let elapsed = diffUTCTime endTime startTime
            hPutStrLn stderr $ "Cache loaded in " ++ show elapsed ++ " (" ++ show (M.size db) ++ " activities)"
            return (db, True)
        else do
            hPutStrLn stderr "No cache found, parsing XML files..."
            startTime <- getCurrentTime
            !db <- loadAllSpolds dataDir
            endTime <- getCurrentTime
            let elapsed = diffUTCTime endTime startTime
            hPutStrLn stderr $ "XML parsing completed in " ++ show elapsed ++ " (" ++ show (M.size db) ++ " activities)"
            return (db, False)

{- | Save ActivityDB to binary cache file with automatic filename -}
saveCachedSpolds :: FilePath -> ActivityDB -> IO ()
saveCachedSpolds dataDir db = do
    cacheFile <- generateCacheFilename dataDir
    hPutStrLn stderr $ "Saving to cache: " ++ cacheFile
    startTime <- getCurrentTime
    encodeFile cacheFile db  -- Simple encode, filename ensures compatibility
    endTime <- getCurrentTime
    let elapsed = diffUTCTime endTime startTime
    hPutStrLn stderr $ "Cache saved in " ++ show elapsed

{- | Load cached SimpleDatabase with automatic filename management -}
loadCachedSpoldsWithFlows :: FilePath -> IO (SimpleDatabase, Bool)
loadCachedSpoldsWithFlows dataDir = do
    cacheFile <- generateCacheFilename dataDir
    cleanupOldCaches dataDir
    
    cacheExists <- doesFileExist cacheFile
    if cacheExists
        then do
            hPutStrLn stderr $ "Loading SimpleDatabase from cache: " ++ cacheFile
            startTime <- getCurrentTime
            !db <- decodeFile cacheFile  -- Simple decode, no error handling needed
            endTime <- getCurrentTime
            let elapsed = diffUTCTime endTime startTime
            hPutStrLn stderr $ "Cache loaded in " ++ show elapsed 
                ++ " (" ++ show (M.size $ sdbActivities db) ++ " activities, " 
                ++ show (M.size $ sdbFlows db) ++ " flows)"
            return (db, True)
        else do
            hPutStrLn stderr "No cache found, parsing XML files..."
            startTime <- getCurrentTime
            !db <- loadAllSpoldsWithFlows dataDir
            endTime <- getCurrentTime
            let elapsed = diffUTCTime endTime startTime
            hPutStrLn stderr $ "XML parsing completed in " ++ show elapsed
                ++ " (" ++ show (M.size $ sdbActivities db) ++ " activities, " 
                ++ show (M.size $ sdbFlows db) ++ " flows)"
            return (db, False)

{- | Save SimpleDatabase to binary cache file with automatic filename -}
saveCachedSpoldsWithFlows :: FilePath -> SimpleDatabase -> IO ()
saveCachedSpoldsWithFlows dataDir db = do
    cacheFile <- generateCacheFilename dataDir
    hPutStrLn stderr $ "Saving SimpleDatabase to cache: " ++ cacheFile
    startTime <- getCurrentTime
    encodeFile cacheFile db  -- Simple encode, filename ensures compatibility
    endTime <- getCurrentTime
    let elapsed = diffUTCTime endTime startTime
    hPutStrLn stderr $ "Cache saved in " ++ show elapsed
