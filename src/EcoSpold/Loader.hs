module EcoSpold.Loader (loadAllSpolds) where

import ACV.Types (Process (processId), UUID)
import qualified Data.Map as M
import EcoSpold.Parser (parseProcessFromFile)
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))

type ProcessDB = M.Map UUID Process

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
