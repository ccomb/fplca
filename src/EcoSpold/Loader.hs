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
import EcoSpold.Parser (parseProcessFromFile, parseProcessAndFlowsFromFile)
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
    
    -- Load files with parallel chunk processing (3 chunks simultaneously)
    loadSpoldsWithParallelChunks spoldFiles
  where
    chunkSize = 1000  -- Process 1000 files at a time
    maxParallelChunks = 3  -- Process up to 3 chunks simultaneously
    
    -- New function for parallel chunk processing
    loadSpoldsWithParallelChunks :: [FilePath] -> IO SimpleDatabase
    loadSpoldsWithParallelChunks files = do
        let chunks = chunksOf chunkSize files
        print $ "Processing " ++ show (length chunks) ++ " chunks with up to " ++ show maxParallelChunks ++ " parallel"
        
        -- Process chunks in batches of maxParallelChunks
        results <- processChunkBatches chunks
        
        -- Merge all results
        let (allProcs, allFlows) = unzip results
        let !finalProcMap = M.unions allProcs
        let !finalFlowMap = M.unions allFlows
        
        print $ "Final: " ++ show (M.size finalProcMap) ++ " processes, " ++ show (M.size finalFlowMap) ++ " flows"
        return $ SimpleDatabase finalProcMap finalFlowMap
    
    -- Process chunks in parallel batches
    processChunkBatches :: [[FilePath]] -> IO [(ProcessDB, FlowDB)]
    processChunkBatches [] = return []
    processChunkBatches chunks = do
        let (currentBatch, remainingChunks) = splitAt maxParallelChunks chunks
        print $ "Processing batch of " ++ show (length currentBatch) ++ " chunks in parallel"
        
        -- Process current batch in parallel
        batchResults <- mapConcurrently processChunk currentBatch
        
        -- Process remaining chunks
        remainingResults <- processChunkBatches remainingChunks
        
        return $ batchResults ++ remainingResults
    
    -- Process a single chunk (same logic as before)
    processChunk :: [FilePath] -> IO (ProcessDB, FlowDB)
    processChunk chunk = do
        print $ "Processing chunk of " ++ show (length chunk) ++ " files"
        
        -- Parse processes and flows in parallel within chunk
        parsedChunk <- mapConcurrently parseProcessAndFlowsFromFile chunk
        let (!procs, flowLists) = unzip parsedChunk
        let !allFlows = concat flowLists
        
        print $ "Parsed " ++ show (length procs) ++ " processes and " ++ show (length allFlows) ++ " flows from chunk"
        
        -- Build process and flow maps in parallel
        let procPairs = [(processId p, p) | p <- procs] `using` parList rdeepseq
        let flowPairs = [(flowId f, f) | f <- allFlows] `using` parList rdeepseq
        let !chunkProcMap = M.fromList procPairs
        let !chunkFlowMap = M.fromList flowPairs
        
        print $ "Built maps: " ++ show (M.size chunkProcMap) ++ " processes, " ++ show (M.size chunkFlowMap) ++ " unique flows"
        
        return (chunkProcMap, chunkFlowMap)
    
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
