module Main where

import qualified Data.Map as M
import Options.Applicative

import ACV.Export.CSV (exportInventoryAsCSV)
import ACV.Export.ILCD (exportInventoryAsILCD)
import ACV.Inventory (computeInventory)
import ACV.PEF (applyCharacterization)
import ACV.Tree (buildProcessTree)
import ACV.Types
import EcoSpold.Loader (buildProcessTreeIO, buildSpoldIndex, loadAllSpolds)
import ILCD.Parser (parseMethodFromFile)

-- | Arguments de ligne de commande
data Args = Args
    { dir :: FilePath -- Répertoire contenant les fichiers .spold
    , rootId :: String -- UUID du procédé racine
    , method :: FilePath -- Fichier méthode PEF au format XML
    , output :: Maybe FilePath -- Fichier de sortie XML (optionnel)
    , csvOut :: Maybe FilePath -- Fichier de sortie CSV (optionnel)
    }

-- | Parser des arguments CLI
argsParser :: Parser Args
argsParser =
    Args
        <$> strOption (long "data" <> help "Répertoire des fichiers .spold")
        <*> strOption (long "root" <> help "UUID du procédé racine")
        <*> strOption (long "method" <> help "Fichier méthode PEF (XML)")
        <*> optional (strOption (long "output" <> help "Fichier de sortie XML"))
        <*> optional (strOption (long "csv" <> help "Fichier de sortie CSV"))

-- | Fonction principale
main :: IO ()
main = do
    Args dir root methodFile output csvOut <-
        execParser $
            info
                (argsParser <**> helper)
                (fullDesc <> progDesc "ACV CLI - moteur ACV Haskell en mémoire vive")

    -- Charger tous les procédés (full memory approach)
    print "loading processes"
    db <- loadAllSpolds dir

    -- Construire l'arbre du procédé racine
    print "building process tree"
    let tree = buildProcessTree db root
    -- Calculer l'inventaire global
    print "computing inventory tree"
    let inventory = computeInventory tree

    -- Charger la méthode PEF
    print "loading PEF method"
    method <- parseMethodFromFile methodFile

    -- Calculer les scores par catégorie
    print "Applying characterization"
    let scores = applyCharacterization inventory method

    -- Affichage
    putStrLn "\nInventaire ACV :"
    mapM_ print (M.toList inventory)

    putStrLn "\nScore PEF par catégorie :"
    mapM_ print (M.toList scores)

    -- Export XML (ILCD)
    case output of
        Just outPath -> do
            exportInventoryAsILCD outPath inventory
            putStrLn $ "Inventaire exporté en XML (ILCD) : " ++ outPath
        Nothing -> pure ()

    -- Export CSV
    case csvOut of
        Just csvPath -> do
            exportInventoryAsCSV csvPath inventory
            putStrLn $ "Inventaire exporté en CSV : " ++ csvPath
        Nothing -> pure ()
