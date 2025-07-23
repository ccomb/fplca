module Main where

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
import ACV.PEF (applyCharacterization)
import ACV.Query (buildIndexes, findActivitiesByFields, findActivitiesByName, findAllReferenceProducts, findExchangesByFlow, findFlowsBySynonym, findFlowsByType, getDatabaseStats)
import qualified ACV.Service
import ACV.Tree (buildActivityTreeWithDatabase, buildLoopAwareTree)
import ACV.Types
import Data.Aeson (Value, toJSON)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.IORef (newIORef, readIORef, writeIORef)
import EcoSpold.Loader (buildActivityTreeIO, buildSpoldIndex, loadAllSpolds, loadAllSpoldsWithFlows, loadAllSpoldsWithIndexes, loadCachedSpoldsWithFlows, saveCachedSpoldsWithFlows)
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

    -- Load database with conditional caching (debug to stderr)
    (simpleDb, wasFromCache) <-
        if disableCache
            then do
                hPutStrLn stderr "Loading activities with flow deduplication (caching disabled)"
                simpleDb <- loadAllSpoldsWithFlows dir
                return (simpleDb, False)
            else do
                hPutStrLn stderr "Loading activities with flow deduplication and automatic caching"
                (simpleDb, wasFromCache) <- loadCachedSpoldsWithFlows dir
                if not wasFromCache
                    then do
                        hPutStrLn stderr "Saving to cache for next time"
                        saveCachedSpoldsWithFlows dir simpleDb
                    else return ()
                return (simpleDb, wasFromCache)

    -- Convert SimpleDatabase to Database with indexes (debug to stderr)
    hPutStrLn stderr "Building indexes for efficient queries"
    let indexes = buildIndexes (sdbActivities simpleDb) (sdbFlows simpleDb)
    let database = Database (sdbActivities simpleDb) (sdbFlows simpleDb) (sdbUnits simpleDb) indexes

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

-- | Execute CLI query using domain service functions (returns identical JSON to API)
executeQuery :: Database -> String -> IO ()
executeQuery db queryStr = do
    hPutStrLn stderr $ "Executing query: " ++ queryStr
    let endpoint = parseApiPath queryStr
    case endpoint of
        ActivityInfo uuid ->
            case ACV.Service.getActivityInfo db uuid of
                Left err -> hPutStrLn stderr $ "Error: " ++ show err
                Right result -> BSL.putStrLn $ encode result
        ActivityInventory uuid ->
            case ACV.Service.getActivityInventory db uuid of
                Left err -> hPutStrLn stderr $ "Error: " ++ show err
                Right result -> BSL.putStrLn $ encode result
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
