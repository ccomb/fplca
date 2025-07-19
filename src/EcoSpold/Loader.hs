{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module EcoSpold.Loader (buildProcessTreeIO, buildSpoldIndex, loadAllSpolds, loadAllSpoldsWithFlows, loadAllSpoldsWithIndexes) where

import ACV.Types
import ACV.Query (buildIndexes)
import Control.Monad
import Control.Parallel.Strategies
import Control.Concurrent.Async
import GHC.Conc (getNumCapabilities)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import EcoSpold.Parser (parseProcessFromFile, parseProcessAndFlowsFromFile, streamParseProcessAndFlowsFromFile)
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))

type SpoldIndex = M.Map UUID FilePath
type Visited = S.Set UUID

buildSpoldIndex :: FilePath -> IO (M.Map UUID FilePath)
buildSpoldIndex dir = do
    files <- listDirectory dir
    let spoldFiles = [f | f <- files, takeExtension f == ".spold"]
    let pairs = [(T.pack (takeWhile (/= '_') f), dir </> f) | f <- spoldFiles]
    return $! M.fromList pairs  -- Keep strict return to force Map evaluation

buildProcessTreeIO :: SpoldIndex -> UUID -> IO ProcessTree
buildProcessTreeIO index rootUuid = do
    -- Cette fonction ne peut plus fonctionner efficacement avec la nouvelle structure
    -- car elle nécessiterait de parser chaque fichier deux fois (une fois pour extraire les flux, 
    -- une fois pour construire l'arbre). Il vaut mieux utiliser loadAllSpoldsWithFlows + buildProcessTreeWithFlows
    error "buildProcessTreeIO: Use loadAllSpoldsWithFlows + buildProcessTreeWithFlows instead for better performance"

-- | Fonction auxiliaire qui nécessite maintenant accès à FlowDB
isTechnosphereInput :: FlowDB -> Exchange -> Bool
isTechnosphereInput flowDB ex =
    case M.lookup (exchangeFlowId ex) flowDB of
        Nothing -> False
        Just flow -> flowType flow == Technosphere
                  && exchangeIsInput ex
                  && not (exchangeIsReference ex)

placeholder :: Process
placeholder = Process "loop" "Loop detected" "N/A" []

missing :: Process
missing = Process "missing" "Process not found" "N/A" []

{- | Version originale - Charge tous les fichiers .spold
  et retourne une base de procédés indexée par UUID
-}
loadAllSpolds :: FilePath -> IO ProcessDB
loadAllSpolds dir = do
    print "listing directory"
    files <- listDirectory dir
    print "getting spold files"
    let spoldFiles = [dir </> f | f <- files, takeExtension f == ".spold"]
    print $ "Found " ++ show (length spoldFiles) ++ " spold files"
    
    -- Load files in chunks to avoid memory explosion
    loadSpoldsInChunks spoldFiles M.empty
  where
    chunkSize = 1000  -- Process 1000 files at a time
    
    loadSpoldsInChunks :: [FilePath] -> ProcessDB -> IO ProcessDB
    loadSpoldsInChunks [] acc = return acc
    loadSpoldsInChunks files acc = do
        let (chunk, rest) = splitAt chunkSize files
        print $ "Processing chunk of " ++ show (length chunk) ++ " files"
        
        -- Force evaluation of chunk to avoid building up thunks
        chunk' <- mapM parseProcessFromFile chunk
        let !chunkMap = M.fromList [(processId p, p) | p <- chunk']  -- Keep strict for Map
        
        -- Force evaluation and merge
        let !newAcc = M.union acc chunkMap  -- Keep strict for accumulator
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
    
    -- Load files in chunks with optimized internal parallelism
    loadSpoldsWithOptimizedChunks spoldFiles M.empty M.empty
  where
    chunkSize = 1000  -- Process 1000 files at a time
    
    -- Memory-efficient sequential chunk processing with internal parallelism
    loadSpoldsWithOptimizedChunks :: [FilePath] -> ProcessDB -> FlowDB -> IO SimpleDatabase
    loadSpoldsWithOptimizedChunks [] procAcc flowAcc = do
        print $ "Final: " ++ show (M.size procAcc) ++ " processes, " ++ show (M.size flowAcc) ++ " flows"
        return $ SimpleDatabase procAcc flowAcc
    loadSpoldsWithOptimizedChunks files procAcc flowAcc = do
        let (chunk, rest) = splitAt chunkSize files
        print $ "Processing chunk of " ++ show (length chunk) ++ " files"
        
        -- Parse files in smaller parallel batches to control memory
        let batchSize = 100  -- Process 100 files at a time within chunk
        (chunkProcMap, chunkFlowMap) <- processChunkInBatches chunk
        
        print $ "Built maps: " ++ show (M.size chunkProcMap) ++ " processes, " ++ show (M.size chunkFlowMap) ++ " unique flows"
        
        -- Merge with accumulators and continue
        let !newProcAcc = M.union procAcc chunkProcMap
        let !newFlowAcc = M.union flowAcc chunkFlowMap
        
        loadSpoldsWithOptimizedChunks rest newProcAcc newFlowAcc
    
    -- Process chunk in smaller batches for better memory control
    processChunkInBatches :: [FilePath] -> IO (ProcessDB, FlowDB)
    processChunkInBatches files = do
        let batches = chunksOf 100 files  -- 100 files per batch
        results <- mapM processBatch batches
        let (procMaps, flowMaps) = unzip results
        let !finalProcMap = M.unions procMaps
        let !finalFlowMap = M.unions flowMaps
        return (finalProcMap, finalFlowMap)
    
    -- Process a small batch of files in parallel using streaming parser
    processBatch :: [FilePath] -> IO (ProcessDB, FlowDB)
    processBatch batch = do
        -- Parse files in parallel with streaming parser
        parsedBatch <- mapConcurrently streamParseProcessAndFlowsFromFile batch
        let (!procs, flowLists) = unzip parsedBatch
        let !allFlows = concat flowLists
        
        -- Build maps with controlled parallelism
        let !procMap = M.fromList [(processId p, p) | p <- procs]
        let !flowMap = M.fromList [(flowId f, f) | f <- allFlows]
        
        return (procMap, flowMap)
    
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
    print $ "loading processes with flow deduplication and indexes using " ++ show numCaps ++ " cores"
    simpleDb <- loadAllSpoldsWithFlows dir
    
    print "building indexes for efficient queries"
    let indexes = buildIndexes (sdbProcesses simpleDb) (sdbFlows simpleDb)
    
    return $ Database (sdbProcesses simpleDb) (sdbFlows simpleDb) indexes
