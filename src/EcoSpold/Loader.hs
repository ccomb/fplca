module EcoSpold.Loader (buildProcessTreeIO, buildSpoldIndex) where

import ACV.Types
import Control.Monad
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import EcoSpold.Parser (parseProcessFromFile)
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))

type ProcessDB = M.Map UUID Process

type SpoldIndex = M.Map UUID FilePath
type Visited = S.Set UUID

buildSpoldIndex :: FilePath -> IO (M.Map UUID FilePath)
buildSpoldIndex dir = do
    files <- listDirectory dir
    let paths = [dir </> f | f <- files, takeExtension f == ".spold"]
    pairs <- forM paths $ \fp -> do
        proc <- parseProcessFromFile fp
        return (processId proc, fp)
    return $ M.fromList pairs

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
                    let validChildren = catMaybes children
                    return $
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

{- | Charge tous les fichiers .spold d’un répertoire donné
  et retourne une base de procédés indexée par UUID
-}
loadAllSpolds :: FilePath -> IO ProcessDB
loadAllSpolds dir = do
    print "listing directory"
    files <- listDirectory dir
    print "getting spold files"
    let spoldFiles = [dir </> f | f <- files, takeExtension f == ".spold"]
    print "parsing spold files"
    processes <- mapM parseProcessFromFile spoldFiles
    return $ M.fromList [(processId p, p) | p <- processes]
