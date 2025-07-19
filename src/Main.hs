module Main where

import qualified Data.Map as M
import qualified Data.Text as T
import Options.Applicative

import ACV.API (acvServer, acvAPI)
import ACV.Export.CSV (exportInventoryAsCSV)
import ACV.Export.ILCD (exportInventoryAsILCD)
import ACV.Inventory (computeInventoryWithFlows)
import ACV.PEF (applyCharacterization)
import ACV.Query (getDatabaseStats, findProcessesByName, findFlowsByType, findAllReferenceProducts, findExchangesByFlow)
import ACV.Tree (buildProcessTreeWithDatabase)
import ACV.Types
import EcoSpold.Loader (buildProcessTreeIO, buildSpoldIndex, loadAllSpolds, loadAllSpoldsWithIndexes)
import ILCD.Parser (parseMethodFromFile)
import Network.Wai.Handler.Warp (run)
import Servant

-- | Arguments de ligne de commande
data Args = Args
    { dir :: FilePath -- Répertoire contenant les fichiers .spold
    , rootId :: String -- UUID du procédé racine
    , method :: FilePath -- Fichier méthode PEF au format XML
    , output :: Maybe FilePath -- Fichier de sortie XML (optionnel)
    , csvOut :: Maybe FilePath -- Fichier de sortie CSV (optionnel)
    , serverMode :: Bool -- Mode serveur API
    , serverPort :: Int -- Port du serveur API
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
        <*> switch (long "server" <> help "Lancer le serveur API au lieu du calcul LCA")
        <*> option auto (long "port" <> value 8080 <> help "Port du serveur API (défaut: 8080)")

-- | Fonction principale
main :: IO ()
main = do
    Args dir root methodFile output csvOut server port <-
        execParser $
            info
                (argsParser <**> helper)
                (fullDesc <> progDesc "ACV CLI - moteur ACV Haskell en mémoire vive")

    -- Charger tous les procédés avec déduplication des flux ET index (optimized memory + query approach)
    print "loading processes with flow deduplication and indexes"
    database <- loadAllSpoldsWithIndexes dir

    -- Afficher les statistiques de la base de données
    let stats = getDatabaseStats database
    putStrLn "\n=== DATABASE STATISTICS ==="
    print stats

    if server
        then do
            -- Mode serveur API
            putStrLn $ "\nStarting API server on port " ++ show port
            putStrLn "Available endpoints:"
            putStrLn "  GET /api/v1/process/{uuid} - Get detailed process information"
            run port (serve acvAPI (acvServer database))
        else do
            -- Mode calcul LCA traditionnel
            
            -- Exemples de requêtes exchange-level
            putStrLn "\n=== EXCHANGE-LEVEL QUERIES EXAMPLES ==="
            let refProducts = take 5 $ findAllReferenceProducts database
            putStrLn $ "First 5 reference products: " ++ show (length refProducts)
            mapM_ (\(proc, flow, ex) -> putStrLn $ "  " ++ show (processName proc) ++ " -> " ++ show (flowName flow)) refProducts

            -- Construire l'arbre du procédé racine
            print "building process tree"
            let tree = buildProcessTreeWithDatabase database (T.pack root)
            -- Calculer l'inventaire global
            print "computing inventory tree"
            let inventory = computeInventoryWithFlows (dbFlows database) tree

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
