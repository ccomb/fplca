module Main where

import qualified Data.Map as M
import qualified Data.Text as T
import Options.Applicative

import ACV.API (acvServer, acvAPI)
import ACV.Export.CSV (exportInventoryAsCSV)
import ACV.Export.ILCD (exportInventoryAsILCD)
import ACV.Inventory (computeInventoryWithFlows)
import ACV.PEF (applyCharacterization)
import ACV.Query (getDatabaseStats, findActivitiesByName, findFlowsByType, findAllReferenceProducts, findExchangesByFlow, buildIndexes)
import ACV.Tree (buildActivityTreeWithDatabase)
import ACV.Types
import EcoSpold.Loader (buildActivityTreeIO, buildSpoldIndex, loadAllSpolds, loadAllSpoldsWithIndexes, loadCachedSpoldsWithFlows, saveCachedSpoldsWithFlows)
import ILCD.Parser (parseMethodFromFile)
import Network.Wai.Handler.Warp (run)
import Servant

-- | Arguments de ligne de commande
data Args = Args
    { dir :: FilePath -- Répertoire contenant les fichiers .spold
    , rootId :: String -- UUID du activité racine
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
        <*> strOption (long "root" <> help "UUID du activité racine")
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

    -- Load database with automatic caching
    print "Loading activities with flow deduplication and automatic caching"
    (simpleDb, wasFromCache) <- loadCachedSpoldsWithFlows dir
    if not wasFromCache
        then do
            print "Saving to cache for next time"
            saveCachedSpoldsWithFlows dir simpleDb
        else return ()
    
    -- Convert SimpleDatabase to Database with indexes
    print "Building indexes for efficient queries"
    let indexes = buildIndexes (sdbActivities simpleDb) (sdbFlows simpleDb)
    let database = Database (sdbActivities simpleDb) (sdbFlows simpleDb) (sdbUnits simpleDb) indexes

    -- Afficher les statistiques de la base de données
    let stats = getDatabaseStats database
    putStrLn "\n=== DATABASE STATISTICS ==="
    print stats

    if server
        then do
            -- Mode serveur API
            putStrLn $ "\nStarting API server on port " ++ show port
            putStrLn "Available endpoints:"
            putStrLn "  Activity endpoints:"
            putStrLn "    GET /api/v1/activity/{uuid}                    - Get detailed activity information"
            putStrLn "    GET /api/v1/activity/{uuid}/flows               - Get flows used by activity"
            putStrLn "    GET /api/v1/activity/{uuid}/inputs              - Get input exchanges"
            putStrLn "    GET /api/v1/activity/{uuid}/outputs             - Get output exchanges"
            putStrLn "    GET /api/v1/activity/{uuid}/reference-product   - Get reference product"
            putStrLn ""
            putStrLn "  Flow endpoints:"
            putStrLn "    GET /api/v1/flows/{flowId}                      - Get detailed flow information"
            putStrLn "    GET /api/v1/flows/{flowId}/activities           - Get activities using this flow"
            putStrLn ""
            putStrLn "  Search endpoints (all support pagination with ?limit=N&offset=N):"
            putStrLn "    GET /api/v1/search/flows?q={term}               - Fuzzy search flows (name, category, synonyms)"
            putStrLn "    GET /api/v1/search/flows?q={term}&lang={lang}   - Fuzzy search flows in specific language"  
            putStrLn "    GET /api/v1/search/activities?name={term}        - Search activities by name"
            putStrLn "    GET /api/v1/search/activities?geo={location}     - Search activities by geography (exact match)"
            putStrLn "    GET /api/v1/search/activities?product={product}  - Search activities by reference product"
            putStrLn "    GET /api/v1/search/activities?name={n}&geo={g}   - Combine multiple criteria (AND logic)"
            putStrLn ""
            putStrLn "  Pagination: Default limit=50, offset=0. Max limit=1000"
            putStrLn "  Example: /api/v1/search/activities?name=electricity&limit=20&offset=40"
            putStrLn "  Note: No search parameters returns all activities (paginated)"
            putStrLn ""
            putStrLn "  Synonym endpoints:"
            putStrLn "    GET /api/v1/synonyms/languages                  - Get available languages"
            putStrLn "    GET /api/v1/synonyms/stats                      - Get synonym statistics"
            putStrLn ""
            run port (serve acvAPI (acvServer database))
        else do
            -- Mode calcul LCA traditionnel
            
            -- Exemples de requêtes exchange-level
            putStrLn "\n=== EXCHANGE-LEVEL QUERIES EXAMPLES ==="
            let refProducts = take 5 $ findAllReferenceProducts database
            putStrLn $ "First 5 reference products: " ++ show (length refProducts)
            mapM_ (\(proc, flow, ex) -> putStrLn $ "  " ++ show (activityName proc) ++ " -> " ++ show (flowName flow)) refProducts

            -- Construire l'arbre du activité racine
            print "building activity tree"
            let tree = buildActivityTreeWithDatabase database (T.pack root)
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
