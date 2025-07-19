{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module EcoSpold.Loader (buildProcessTreeIO, buildSpoldIndex, loadAllSpolds) where

import ACV.Types
import Control.Monad
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import EcoSpold.Parser (parseProcessFromFile)
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))

type ProcessDB = M.Map UUID Process

type SpoldIndex = M.Map UUID FilePath
type Visited = S.Set UUID

buildSpoldIndex :: FilePath -> IO (M.Map UUID FilePath)
buildSpoldIndex dir = do
    files <- listDirectory dir
    let spoldFiles = [f | f <- files, takeExtension f == ".spold"]
    let pairs = [(T.pack (takeWhile (/= '_') f), dir </> f) | f <- spoldFiles]
    return $! M.fromList pairs  -- Keep strict return to force Map evaluation

buildProcessTreeIO :: SpoldIndex -> UUID -> IO ProcessTree
buildProcessTreeIO index = go S.empty
  where
    go :: Visited -> UUID -> IO ProcessTree
    go visited uuid
        | S.member uuid visited = return $ Leaf placeholder -- boucle détectée
        | otherwise =
            case M.lookup uuid index of
                Nothing -> return $ Leaf missing
                Just path -> do
                    proc <- parseProcessFromFile path
                    let visited' = S.insert uuid visited
                    children <- forM (exchanges proc) $ \ex ->
                        if isTechnosphereInput ex
                            then case M.lookup (flowId $ exchangeFlow ex) index of
                                Nothing -> return Nothing
                                Just childPath -> do
                                    let childId = flowId $ exchangeFlow ex
                                    subtree <- go visited' childId
                                    return $ Just (exchangeAmount ex, subtree)
                            else return Nothing
                    let !validChildren = catMaybes children  -- Keep strict for children list
                    return $!
                        if null validChildren
                            then Leaf proc
                            else Node proc validChildren

isTechnosphereInput :: Exchange -> Bool
isTechnosphereInput ex =
    flowType (exchangeFlow ex) == Technosphere
        && exchangeIsInput ex
        && not (exchangeIsReference ex)

placeholder :: Process
placeholder = Process "loop" "Loop detected" "N/A" []

missing :: Process
missing = Process "missing" "Process not found" "N/A" []

{- | Charge tous les fichiers .spold d'un répertoire donné
  et retourne une base de procédés indexée par UUID - Version optimisée mémoire
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
