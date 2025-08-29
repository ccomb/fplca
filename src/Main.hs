{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Monad (when)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Map as M
import qualified Data.Text as T
import Options.Applicative
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

import ACV.API (ACVAPI, acvAPI, acvServer)
import qualified ACV.API as API
import ACV.Export.CSV (exportInventoryAsCSV)
import ACV.Export.ILCD (exportInventoryAsILCD)
import ACV.Inventory (computeInventoryWithFlows)
import ACV.Matrix (computeInventoryMatrix)
import ACV.PEF (applyCharacterization)
import ACV.Query (buildIndexes, buildDatabaseWithMatrices, findActivitiesByFields, findAllReferenceProducts, findFlowsBySynonym, findFlowsByType, getDatabaseStats)
import qualified ACV.Service
import ACV.Tree (buildActivityTreeWithDatabase, buildLoopAwareTree)
import ACV.Types
import Data.Aeson (Value, toJSON)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.IORef (newIORef, readIORef, writeIORef)
import EcoSpold.Loader (loadAllSpoldsWithFlows, loadCachedSpoldsWithFlows, saveCachedSpoldsWithFlows, loadCachedDatabaseWithMatrices, saveCachedDatabaseWithMatrices)
import ILCD.Parser (parseMethodFromFile)
import Network.HTTP.Types (Query, methodGet, ok200, statusCode)
import Network.HTTP.Types.Status (Status (..))
import Network.URI (parseURI, uriPath, uriQuery)
import Network.Wai (Application, Request (..), Response, ResponseReceived (..), defaultRequest, responseStatus)
import Network.Wai.Handler.Warp (run)
import Servant

-- | Arguments de ligne de commande
data Args = Args
    { dir :: FilePath -- Répertoire contenant les fichiers .spold
    , rootId :: String -- UUID de l'activité racine
    , method :: FilePath -- Fichier méthode PEF au format XML
    , output :: Maybe FilePath -- Fichier de sortie XML (optionnel)
    , csvOut :: Maybe FilePath -- Fichier de sortie CSV (optionnel)
    , serverMode :: Bool -- Mode serveur API
    , serverPort :: Int -- Port du serveur API
    , queryMode :: Maybe String -- Mode requête CLI (simule API sans serveur)
    , noCache :: Bool -- Disable caching (for testing)
    }

-- | Parser des arguments CLI
argsParser :: Parser Args
argsParser =
    Args
        <$> strOption (long "data" <> help "Répertoire des fichiers .spold")
        <*> strOption (long "root" <> help "UUID de l'activité racine")
        <*> strOption (long "method" <> help "Fichier méthode PEF (XML)")
        <*> optional (strOption (long "output" <> help "Fichier de sortie XML"))
        <*> optional (strOption (long "csv" <> help "Fichier de sortie CSV"))
        <*> switch (long "server" <> help "Lancer le serveur API au lieu du calcul LCA")
        <*> option auto (long "port" <> value 8080 <> help "Port du serveur API (défaut: 8080)")
        <*> optional (strOption (long "query" <> help "Mode requête CLI (ex: activity/uuid/inventory)"))
        <*> switch (long "no-cache" <> help "Disable caching (useful for testing and development)")

-- | Fonction principale
main :: IO ()
main = do
    Args dir root methodFile output csvOut server port queryPath disableCache <-
        execParser $
            info
                (argsParser <**> helper)
                (fullDesc <> progDesc "ACV CLI - moteur ACV Haskell en mémoire vive")

    -- Load database with matrix caching (debug to stderr)
    database <-
        if disableCache
            then do
                hPutStrLn stderr "Loading activities with flow deduplication (caching disabled)"
                simpleDb <- loadAllSpoldsWithFlows dir
                hPutStrLn stderr "Building indexes and pre-computing matrices (no caching)"
                let !db = buildDatabaseWithMatrices (sdbActivities simpleDb) (sdbFlows simpleDb) (sdbUnits simpleDb)
                return db
            else do
                hPutStrLn stderr "Checking for cached Database with matrices"
                cachedDb <- loadCachedDatabaseWithMatrices dir
                case cachedDb of
                    Just db -> do
                        hPutStrLn stderr "Using cached Database with pre-computed matrices"
                        return db
                    Nothing -> do
                        hPutStrLn stderr "No matrix cache found, building from SimpleDatabase cache"
                        (simpleDb, wasFromCache) <- loadCachedSpoldsWithFlows dir
                        when (not wasFromCache) $ do
                            hPutStrLn stderr "Saving SimpleDatabase to cache for next time"
                            saveCachedSpoldsWithFlows dir simpleDb
                        
                        hPutStrLn stderr "Building indexes and pre-computing matrices for efficient queries"
                        let !db = buildDatabaseWithMatrices (sdbActivities simpleDb) (sdbFlows simpleDb) (sdbUnits simpleDb)
                        
                        hPutStrLn stderr "Saving Database with matrices to cache for next time"
                        saveCachedDatabaseWithMatrices dir db
                        return db

    -- Afficher les statistiques de la base de données (debug to stderr)
    let stats = getDatabaseStats database
    hPutStrLn stderr "\n=== DATABASE STATISTICS ==="
    hPutStrLn stderr $ show stats

    case queryPath of
        Just queryStr -> do
            -- Mode requête CLI - simule les appels API sans serveur
            executeQuery database queryStr
        Nothing ->
            if server
                then do
                    -- Mode serveur API (server info to stderr)
                    hPutStrLn stderr $ "\nStarting API server on port " ++ show port
                    hPutStrLn stderr "Available endpoints:"
                    hPutStrLn stderr "  Activity endpoints:"
                    hPutStrLn stderr "    GET /api/v1/activity/{uuid}                    - Get detailed activity information"
                    hPutStrLn stderr "    GET /api/v1/activity/{uuid}/flows               - Get flows used by activity"
                    hPutStrLn stderr "    GET /api/v1/activity/{uuid}/inputs              - Get input exchanges"
                    hPutStrLn stderr "    GET /api/v1/activity/{uuid}/outputs             - Get output exchanges"
                    hPutStrLn stderr "    GET /api/v1/activity/{uuid}/reference-product   - Get reference product"
                    hPutStrLn stderr "    GET /api/v1/activity/{uuid}/tree                 - Get activity supply chain tree (JSON, depth=2)"
                    hPutStrLn stderr "    GET /api/v1/activity/{uuid}/inventory            - Get activity life cycle inventory (LCI)"
                    hPutStrLn stderr ""
                    hPutStrLn stderr "  Flow endpoints:"
                    hPutStrLn stderr "    GET /api/v1/flow/{flowId}                       - Get detailed flow information"
                    hPutStrLn stderr "    GET /api/v1/flow/{flowId}/activities            - Get activities using this flow"
                    hPutStrLn stderr ""
                    hPutStrLn stderr "  Search endpoints (combined results + count, support pagination with ?limit=N&offset=N):"
                    hPutStrLn stderr "    GET /api/v1/search/flows?q={term}               - Search flows (name, category, synonyms)"
                    hPutStrLn stderr "    GET /api/v1/search/flows?q={term}&lang={lang}   - Search flows in specific language"
                    hPutStrLn stderr "    GET /api/v1/search/activities?name={term}        - Search activities by name"
                    hPutStrLn stderr "    GET /api/v1/search/activities?geo={location}     - Search activities by geography (exact match)"
                    hPutStrLn stderr "    GET /api/v1/search/activities?product={product}  - Search activities by reference product"
                    hPutStrLn stderr "    GET /api/v1/search/activities?name={n}&geo={g}   - Combine multiple criteria (AND logic)"
                    hPutStrLn stderr ""
                    hPutStrLn stderr "  Pagination: Default limit=50, offset=0. Max limit=1000"
                    hPutStrLn stderr "  Example: /api/v1/search/activities?name=electricity&limit=20&offset=40"
                    hPutStrLn stderr "  Note: No search parameters returns all activities (paginated)"
                    hPutStrLn stderr ""
                    hPutStrLn stderr "  Synonym endpoints:"
                    hPutStrLn stderr "    GET /api/v1/synonyms/languages                  - Get available languages"
                    hPutStrLn stderr "    GET /api/v1/synonyms/stats                      - Get synonym statistics"
                    hPutStrLn stderr ""
                    run port (serve acvAPI (acvServer database))
                else do
                    -- Mode calcul LCA traditionnel

                    -- Exemples de requêtes exchange-level
                    putStrLn "\n=== EXCHANGE-LEVEL QUERIES EXAMPLES ==="
                    let refProducts = take 5 $ findAllReferenceProducts database
                    putStrLn $ "First 5 reference products: " ++ show (length refProducts)
                    mapM_ (\(proc, flow, ex) -> putStrLn $ "  " ++ show (activityName proc) ++ " -> " ++ show (flowName flow)) refProducts

                    -- Construire l'arbre de l'activité racine
                    print "building activity tree"
                    let tree = buildActivityTreeWithDatabase database (T.pack root)
                    -- Calculer l'inventaire global (méthode arbre)
                    print "computing inventory tree"
                    let inventory = computeInventoryWithFlows (dbFlows database) tree
                    
                    -- Calculer l'inventaire avec la méthode matricielle
                    print "computing inventory matrix"  
                    let inventoryMatrix = computeInventoryMatrix database (T.pack root)

                    -- Charger la méthode PEF
                    print "loading PEF method"
                    method <- parseMethodFromFile methodFile

                    -- Calculer les scores par catégorie
                    print "Applying characterization"
                    let scores = applyCharacterization inventory method

                    -- Affichage comparaison
                    putStrLn "\nInventaire ACV (Tree method):"
                    mapM_ print (M.toList inventory)
                    
                    putStrLn "\nInventaire ACV (Matrix method):"
                    mapM_ print (M.toList inventoryMatrix)
                    
                    -- Comparison for specific flows
                    putStrLn "\nComparison for key flows:"
                    let compareFlow flowId name = do
                          let treeVal = M.findWithDefault 0.0 flowId inventory
                          let matrixVal = M.findWithDefault 0.0 flowId inventoryMatrix  
                          if treeVal /= 0.0 || matrixVal /= 0.0
                          then putStrLn $ name ++ ": Tree=" ++ show treeVal ++ ", Matrix=" ++ show matrixVal ++ ", Ratio=" ++ show (if treeVal /= 0 then matrixVal/treeVal else 0)
                          else return ()
                    
                    -- Check Zinc II and Water flows
                    compareFlow (T.pack "5ce378a0-b48d-471c-977d-79681521efde") "Zinc(II)"
                    compareFlow (T.pack "51254820-3456-4373-b7b4-056cf7b16e01") "Water"

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

-- | Execute CLI query using domain service functions (returns identical JSON to API)
executeQuery :: Database -> String -> IO ()
executeQuery db queryStr = do
    hPutStrLn stderr $ "Executing query: " ++ queryStr
    hPutStrLn stderr $ "MAIN: Starting parseApiPath"
    let endpoint = parseApiPath queryStr
    hPutStrLn stderr $ "MAIN: parseApiPath completed, matching endpoint"
    
    case endpoint of
        ActivityInfo uuid -> do
            hPutStrLn stderr $ "MAIN: ActivityInfo endpoint, calling getActivityInfo"
            case ACV.Service.getActivityInfo db uuid of
                Left err -> hPutStrLn stderr $ "Error: " ++ show err
                Right result -> BSL.putStrLn $ encode result
        ActivityFlows uuid -> do
            hPutStrLn stderr $ "MAIN: ActivityFlows endpoint, calling getActivityFlows"
            case ACV.Service.getActivityFlows db uuid of
                Left err -> hPutStrLn stderr $ "Error: " ++ show err
                Right result -> BSL.putStrLn $ encode result
        ActivityInventory uuid -> do
            hPutStrLn stderr $ "MAIN: ActivityInventory endpoint, calling getActivityInventory"
            hPutStrLn stderr $ "MAIN: UUID received: " ++ T.unpack uuid
            hPutStrLn stderr $ "MAIN: Database activity count: " ++ show (dbActivityCount db)
            hPutStrLn stderr $ "MAIN: About to evaluate ACV.Service.getActivityInventory"
            hPutStrLn stderr $ "MAIN: Evaluating function pointer..."
            let func = ACV.Service.getActivityInventory
            hPutStrLn stderr $ "MAIN: Function pointer evaluated, calling with arguments..."
            hPutStrLn stderr $ "MAIN: About to call service function"
            let serviceResult = func db uuid
            hPutStrLn stderr $ "MAIN: Service function called, result is lazy thunk"
            hPutStrLn stderr $ "MAIN: About to force evaluation with seq"
            case serviceResult of
                Left err -> do
                    hPutStrLn stderr $ "MAIN: Service returned error"
                    hPutStrLn stderr $ "Error: " ++ show err
                Right result -> do
                    hPutStrLn stderr $ "MAIN: Service returned success, evaluating result"
                    result `seq` hPutStrLn stderr $ "MAIN: Result evaluation completed"
                    hPutStrLn stderr $ "MAIN: Starting JSON encoding"
                    let jsonResult = encode result
                    hPutStrLn stderr $ "MAIN: JSON encoding completed, result size: " ++ show (BSL.length jsonResult) ++ " bytes"
                    BSL.putStrLn jsonResult
        ActivityTree uuid ->
            case ACV.Service.getActivityTree db uuid of
                Left err -> hPutStrLn stderr $ "Error: " ++ show err
                Right result -> BSL.putStrLn $ encode result
        SearchActivities nameParam geoParam productParam limitParam offsetParam ->
            case ACV.Service.searchActivities db nameParam geoParam productParam limitParam offsetParam of
                Left err -> hPutStrLn stderr $ "Error: " ++ show err
                Right result -> BSL.putStrLn $ encode result
        SearchFlows queryParam langParam limitParam offsetParam ->
            case ACV.Service.searchFlows db queryParam langParam limitParam offsetParam of
                Left err -> hPutStrLn stderr $ "Error: " ++ show err
                Right result -> BSL.putStrLn $ encode result
        _ -> hPutStrLn stderr $ "Unsupported endpoint: " ++ queryStr

-- | Parse API path into structured endpoint type
parseApiPath :: String -> ApiEndpoint
parseApiPath queryStr = case break (== '?') queryStr of
    -- Activity endpoints
    (path, params)
        | "activity/" `isPrefixOf` path ->
            let uuidAndEndpoint = drop 9 path
             in case break (== '/') uuidAndEndpoint of
                    (uuid, "") -> ActivityInfo (T.pack uuid)
                    (uuid, "/flows") -> ActivityFlows (T.pack uuid)
                    (uuid, "/inputs") -> ActivityInputs (T.pack uuid)
                    (uuid, "/outputs") -> ActivityOutputs (T.pack uuid)
                    (uuid, "/reference-product") -> ActivityRefProduct (T.pack uuid)
                    (uuid, "/tree") -> ActivityTree (T.pack uuid)
                    (uuid, "/inventory") -> ActivityInventory (T.pack uuid)
                    _ -> Unsupported
    -- Flow endpoints
    (path, params)
        | "flow/" `isPrefixOf` path ->
            let flowIdAndEndpoint = drop 5 path
             in case break (== '/') flowIdAndEndpoint of
                    (flowId, "") -> FlowInfo (T.pack flowId)
                    (flowId, "/activities") -> FlowActivities (T.pack flowId)
                    _ -> Unsupported
    -- Search endpoints
    ("search/activities", '?' : params) ->
        let parsedParams = parseParams params
         in SearchActivities
                (lookup "name" parsedParams)
                (lookup "geo" parsedParams)
                (lookup "product" parsedParams)
                (lookup "limit" parsedParams >>= readMaybe . T.unpack)
                (lookup "offset" parsedParams >>= readMaybe . T.unpack)
    ("search/flows", '?' : params) ->
        let parsedParams = parseParams params
         in SearchFlows
                (lookup "q" parsedParams)
                (lookup "lang" parsedParams)
                (lookup "limit" parsedParams >>= readMaybe . T.unpack)
                (lookup "offset" parsedParams >>= readMaybe . T.unpack)
    -- Synonym endpoints
    ("synonyms/languages", _) -> SynonymLanguages
    ("synonyms/stats", _) -> SynonymStats
    _ -> Unsupported

-- | Structured endpoint type matching ACVAPI
data ApiEndpoint
    = ActivityInfo T.Text
    | ActivityFlows T.Text
    | ActivityInputs T.Text
    | ActivityOutputs T.Text
    | ActivityRefProduct T.Text
    | ActivityTree T.Text
    | ActivityInventory T.Text
    | FlowInfo T.Text
    | FlowActivities T.Text
    | SearchActivities (Maybe T.Text) (Maybe T.Text) (Maybe T.Text) (Maybe Int) (Maybe Int)
    | SearchFlows (Maybe T.Text) (Maybe T.Text) (Maybe Int) (Maybe Int)
    | SynonymLanguages
    | SynonymStats
    | Unsupported

-- | Parse URL parameters
parseParams :: String -> [(String, T.Text)]
parseParams params =
    [ (key, T.pack value) | param <- splitBy '&' params, let (key, '=' : value) = break (== '=') param, not (null key), not (null value)
    ]
  where
    splitBy c str = case break (== c) str of
        (prefix, []) -> [prefix]
        (prefix, _ : suffix) -> prefix : splitBy c suffix


-- Helper function
isPrefixOf :: String -> String -> Bool
isPrefixOf prefix str = take (length prefix) str == prefix
