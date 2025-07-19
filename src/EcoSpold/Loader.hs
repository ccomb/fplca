{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module EcoSpold.Loader (buildProcessTreeIO, buildSpoldIndex, loadAllSpolds, loadAllSpoldsWithFlows) where

import ACV.Types
import Control.Monad
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

{- | Version optimisée avec déduplication des flux
  Charge tous les fichiers .spold et déduplique les flux
-}
loadAllSpoldsWithFlows :: FilePath -> IO Database
loadAllSpoldsWithFlows dir = do
    print "listing directory"
    files <- listDirectory dir
    print "getting spold files"
    let spoldFiles = [dir </> f | f <- files, takeExtension f == ".spold"]
    print $ "Found " ++ show (length spoldFiles) ++ " spold files"
    
    -- Load files in chunks to avoid memory explosion
    loadSpoldsWithFlowsInChunks spoldFiles M.empty M.empty
  where
    chunkSize = 1000  -- Process 1000 files at a time
    
    loadSpoldsWithFlowsInChunks :: [FilePath] -> ProcessDB -> FlowDB -> IO Database
    loadSpoldsWithFlowsInChunks [] procAcc flowAcc = return $ Database procAcc flowAcc
    loadSpoldsWithFlowsInChunks files procAcc flowAcc = do
        let (chunk, rest) = splitAt chunkSize files
        print $ "Processing chunk of " ++ show (length chunk) ++ " files"
        
        -- Parse processes and flows
        chunk' <- mapM parseProcessAndFlowsFromFile chunk
        let (procs, flowLists) = unzip chunk'
        let allFlows = concat flowLists
        
        -- Build process map
        let !chunkProcMap = M.fromList [(processId p, p) | p <- procs]
        
        -- Build flow map (deduplicated)
        let !chunkFlowMap = M.fromList [(flowId f, f) | f <- allFlows]
        
        -- Merge with accumulators
        let !newProcAcc = M.union procAcc chunkProcMap
        let !newFlowAcc = M.union flowAcc chunkFlowMap
        
        loadSpoldsWithFlowsInChunks rest newProcAcc newFlowAcc
