{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module EcoSpold.Loader (buildProcessTreeIO, buildSpoldIndex, loadAllSpolds, loadAllSpoldsWithFlows, loadAllSpoldsWithIndexes) where

import ACV.Query (buildIndexes)
import ACV.Types
import Control.Concurrent.Async
import Control.Monad
import Control.Parallel.Strategies
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import EcoSpold.Parser (parseProcessAndFlowsFromFile, parseProcessFromFile, streamParseProcessAndFlowsFromFile)
import GHC.Conc (getNumCapabilities)
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))

type SpoldIndex = M.Map UUID FilePath
type Visited = S.Set UUID

buildSpoldIndex :: FilePath -> IO (M.Map UUID FilePath)
buildSpoldIndex dir = do
    files <- listDirectory dir
    let spoldFiles = [f | f <- files, takeExtension f == ".spold"]
    let pairs = [(T.pack (takeWhile (/= '_') f), dir </> f) | f <- spoldFiles]
    return $! M.fromList pairs -- Keep strict return to force Map evaluation

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
        Just flow ->
            flowType flow == Technosphere
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
    chunkSize = 1000 -- Process 1000 files at a time
    loadSpoldsInChunks :: [FilePath] -> ProcessDB -> IO ProcessDB
    loadSpoldsInChunks [] acc = return acc
    loadSpoldsInChunks files acc = do
        let (chunk, rest) = splitAt chunkSize files
        print $ "Processing chunk of " ++ show (length chunk) ++ " files"

        -- Force evaluation of chunk to avoid building up thunks
        chunk' <- mapM parseProcessFromFile chunk
        let !chunkMap = M.fromList [(processId p, p) | p <- chunk'] -- Keep strict for Map

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

        -- Process chunks sequentially, but files within each chunk in parallel
        results <- mapM processChunkSimple chunks
        let (procMaps, flowMaps) = unzip results
        let !finalProcMap = M.unions procMaps
        let !finalFlowMap = M.unions flowMaps

        print $ "Final: " ++ show (M.size finalProcMap) ++ " processes, " ++ show (M.size finalFlowMap) ++ " flows"
        return $ SimpleDatabase finalProcMap finalFlowMap

    -- Process one chunk: all files in chunk processed in parallel
    processChunkSimple :: [FilePath] -> IO (ProcessDB, FlowDB)
    processChunkSimple chunk = do
        print $ "Processing chunk of " ++ show (length chunk) ++ " files in parallel"

        -- All files in chunk processed in parallel
        chunkResults <- mapConcurrently streamParseProcessAndFlowsFromFile chunk
        let (!procs, flowLists) = unzip chunkResults
        let !allFlows = concat flowLists

        -- Build maps for this chunk
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
