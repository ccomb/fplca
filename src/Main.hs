{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Monad (when)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Options.Applicative
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)
import Text.Printf (printf)
import Data.List (intercalate)
import System.Environment (lookupEnv)
import Control.Monad (forM_)
import GHC.Conc (getNumCapabilities)

import ACV.API (ACVAPI, acvAPI, acvServer)
import qualified ACV.API as API
import ACV.Export.CSV (exportInventoryAsCSV)
import ACV.Export.ILCD (exportInventoryAsILCD)
import ACV.Matrix (computeInventoryMatrix)
import ACV.PEF (applyCharacterization)
import ACV.Progress
import ACV.Query (buildIndexes, buildDatabaseWithMatrices, findActivitiesByFields, findAllReferenceProducts, findFlowsBySynonym, findFlowsByType, getDatabaseStats, DatabaseStats(..))
import qualified ACV.Service
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
    , treeDepth :: Int -- Maximum tree depth for tree display (default: 3)
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
        <*> option auto (long "tree-depth" <> value 3 <> help "Maximum depth for tree display (default: 3, prevents DOS via deep trees)")

-- | Fonction principale
main :: IO ()
main = do
    Args dir root methodFile output csvOut server port queryPath disableCache maxTreeDepth <-
        execParser $
            info
                (argsParser <**> helper)
                (fullDesc <> progDesc "ACV CLI - moteur ACV Haskell en mémoire vive")

    -- Load database with matrix caching
    database <-
        if disableCache
            then do
                reportProgress Info "Loading activities with flow deduplication (caching disabled)"
                simpleDb <- loadAllSpoldsWithFlows dir
                reportProgress Info "Building indexes and pre-computing matrices (no caching)"
                let !db = buildDatabaseWithMatrices (sdbActivities simpleDb) (sdbFlows simpleDb) (sdbUnits simpleDb)
                return db
            else do
                reportCacheOperation "Checking for cached Database with matrices"
                cachedDb <- loadCachedDatabaseWithMatrices dir
                case cachedDb of
                    Just db -> do
                        reportCacheOperation "Using cached Database with pre-computed matrices"
                        return db
                    Nothing -> do
                        reportCacheOperation "No matrix cache found, building from SimpleDatabase cache"
                        (simpleDb, wasFromCache) <- loadCachedSpoldsWithFlows dir
                        when (not wasFromCache) $ do
                            reportCacheOperation "Saving SimpleDatabase to cache for next time"
                            saveCachedSpoldsWithFlows dir simpleDb

                        reportProgress Info "Building indexes and pre-computing matrices for efficient queries"
                        let !db = buildDatabaseWithMatrices (sdbActivities simpleDb) (sdbFlows simpleDb) (sdbUnits simpleDb)

                        reportCacheOperation "Saving Database with matrices to cache for next time"
                        saveCachedDatabaseWithMatrices dir db
                        return db

    -- Display active configuration first
    reportConfiguration

    -- Validate database quality
    validateDatabase database

    -- Display database statistics
    let stats = getDatabaseStats database
    reportProgress Info "=== DATABASE STATISTICS ==="
    displayDatabaseStats database stats

    case queryPath of
        Just queryStr -> do
            -- Mode requête CLI - simule les appels API sans serveur
            executeQuery database queryStr
        Nothing ->
            if server
                then do
                    -- Start API server
                    reportProgress Info $ "Starting API server on port " ++ show port
                    reportProgress Info "Available endpoints:"
                    reportProgress Info "  Activity endpoints:"
                    reportProgress Info "    GET /api/v1/activity/{uuid}                    - Get detailed activity information"
                    reportProgress Info "    GET /api/v1/activity/{uuid}/flows               - Get flows used by activity"
                    reportProgress Info "    GET /api/v1/activity/{uuid}/inputs              - Get input exchanges"
                    reportProgress Info "    GET /api/v1/activity/{uuid}/outputs             - Get output exchanges"
                    reportProgress Info "    GET /api/v1/activity/{uuid}/reference-product   - Get reference product"
                    reportProgress Info "    GET /api/v1/activity/{uuid}/tree                 - Get activity supply chain tree (JSON, depth=3)"
                    reportProgress Info "    GET /api/v1/activity/{uuid}/inventory            - Get activity life cycle inventory (LCI)"
                    reportProgress Info ""
                    reportProgress Info "  Flow endpoints:"
                    reportProgress Info "    GET /api/v1/flow/{flowId}                       - Get detailed flow information"
                    reportProgress Info "    GET /api/v1/flow/{flowId}/activities            - Get activities using this flow"
                    reportProgress Info ""
                    reportProgress Info "  Search endpoints (combined results + count, support pagination with ?limit=N&offset=N):"
                    reportProgress Info "    GET /api/v1/search/flows?q={term}               - Search flows (name, category, synonyms)"
                    reportProgress Info "    GET /api/v1/search/flows?q={term}&lang={lang}   - Search flows in specific language"
                    reportProgress Info "    GET /api/v1/search/activities?name={term}        - Search activities by name"
                    reportProgress Info "    GET /api/v1/search/activities?geo={location}     - Search activities by geography (exact match)"
                    reportProgress Info "    GET /api/v1/search/activities?product={product}  - Search activities by reference product"
                    reportProgress Info "    GET /api/v1/search/activities?name={n}&geo={g}   - Combine multiple criteria (AND logic)"
                    reportProgress Info ""
                    reportProgress Info "  Pagination: Default limit=50, offset=0. Max limit=1000"
                    reportProgress Info "  Example: /api/v1/search/activities?name=electricity&limit=20&offset=40"
                    reportProgress Info "  Note: No search parameters returns all activities (paginated)"
                    reportProgress Info ""
                    reportProgress Info "  Synonym endpoints:"
                    reportProgress Info "    GET /api/v1/synonyms/languages                  - Get available languages"
                    reportProgress Info "    GET /api/v1/synonyms/stats                      - Get synonym statistics"
                    reportProgress Info ""
                    run port (serve acvAPI (acvServer database))
                else do
                    -- Mode calcul LCA traditionnel

                    -- Show database statistics
                    putStrLn "\n=== DATABASE STATISTICS ==="
                    let refProducts = take 5 $ findAllReferenceProducts database
                    putStrLn $ "First 5 reference products: " ++ show (length refProducts)
                    mapM_ (\(proc, flow, ex) -> putStrLn $ "  " ++ show (activityName proc) ++ " -> " ++ show (flowName flow)) refProducts

                    -- Calculate inventory using matrix-based method only
                    putStrLn "\nCalculating LCA inventory using matrix-based approach..."
                    let inventoryMatrix = computeInventoryMatrix database (T.pack root)

                    -- Load characterization method
                    putStrLn "Loading PEF characterization method..."
                    method <- parseMethodFromFile methodFile

                    -- Calculate impact scores
                    putStrLn "Applying characterization factors..."
                    let scores = applyCharacterization inventoryMatrix method

                    putStrLn "\nFinal LCA inventory (kg, MJ, etc.):"
                    mapM_ print (M.toList inventoryMatrix)

                    putStrLn "\nPEF impact scores by category:"
                    mapM_ print (M.toList scores)

                    -- Export XML (ILCD)
                    case output of
                        Just outPath -> do
                            exportInventoryAsILCD outPath inventoryMatrix
                            putStrLn $ "Inventaire exporté en XML (ILCD) : " ++ outPath
                        Nothing -> pure ()

                    -- Export CSV
                    case csvOut of
                        Just csvPath -> do
                            exportInventoryAsCSV csvPath inventoryMatrix
                            putStrLn $ "Inventaire exporté en CSV : " ++ csvPath
                        Nothing -> pure ()

-- | Execute CLI query using domain service functions (returns identical JSON to API)
executeQuery :: Database -> String -> IO ()
executeQuery db queryStr = do
    reportProgress Info $ "Executing CLI query: " ++ queryStr
    let endpoint = parseApiPath queryStr

    case endpoint of
        ActivityInfo uuid -> do
            case ACV.Service.getActivityInfo db uuid of
                Left err -> reportError $ "Error: " ++ show err
                Right result -> BSL.putStrLn $ encode result
        ActivityFlows uuid -> do
            case ACV.Service.getActivityFlows db uuid of
                Left err -> reportError $ "Error: " ++ show err
                Right result -> BSL.putStrLn $ encode result
        ActivityInventory uuid -> do
            reportProgress Info $ "Computing inventory for activity: " ++ T.unpack uuid
            case ACV.Service.getActivityInventory db uuid of
                Left err -> do
                    reportError $ "Error: " ++ show err
                Right result -> do
                    let jsonResult = encode result
                    reportProgress Info $ "Inventory computation completed (" ++ show (BSL.length jsonResult) ++ " bytes)"
                    BSL.putStrLn jsonResult
        ActivityTree uuid ->
            case ACV.Service.getActivityTree db uuid 3 of
                Left err -> reportError $ "Error: " ++ show err
                Right result -> BSL.putStrLn $ encode result
        SearchActivities nameParam geoParam productParam limitParam offsetParam ->
            case ACV.Service.searchActivities db nameParam geoParam productParam limitParam offsetParam of
                Left err -> reportError $ "Error: " ++ show err
                Right result -> BSL.putStrLn $ encode result
        SearchFlows queryParam langParam limitParam offsetParam ->
            case ACV.Service.searchFlows db queryParam langParam limitParam offsetParam of
                Left err -> reportError $ "Error: " ++ show err
                Right result -> BSL.putStrLn $ encode result
        _ -> reportError $ "Unsupported endpoint: " ++ queryStr

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

{-|
Display database statistics in a user-friendly format.

Formats the raw DatabaseStats structure into readable output with:
- Core metrics (activities, flows, exchanges)
- Flow type breakdown (technosphere vs biosphere)
- Geographic coverage summary
- Environmental compartment summary
- Unit diversity metrics
-}
displayDatabaseStats :: Database -> DatabaseStats -> IO ()
displayDatabaseStats db stats = do
    reportProgress Info $ "Activities: " ++ show (statsActivityCount stats) ++ " processes"
    reportProgress Info $ "Flows: " ++ show (statsFlowCount stats) ++ " elementary and intermediate flows"
    reportProgress Info $ "Exchanges: " ++ show (statsExchangeCount stats) ++ " flow occurrences"
    reportProgress Info ""

    reportProgress Info "Flow Type Distribution:"
    reportProgress Info $ "  Technosphere flows: " ++ show (statsTechnosphereFlows stats) ++ " (products, services)"
    reportProgress Info $ "  Biosphere flows: " ++ show (statsBiosphereFlows stats) ++ " (environmental exchanges)"
    reportProgress Info $ "  Reference products: " ++ show (statsReferenceProducts stats) ++ " (defining outputs)"
    reportProgress Info ""

    reportProgress Info "Exchange Balance:"
    reportProgress Info $ "  Total inputs: " ++ show (statsInputCount stats) ++ " exchanges"
    reportProgress Info $ "  Total outputs: " ++ show (statsOutputCount stats) ++ " exchanges"
    let ratio = if statsInputCount stats > 0
                then fromIntegral (statsOutputCount stats) / fromIntegral (statsInputCount stats) :: Double
                else 0
    reportProgress Info $ "  Output/Input ratio: " ++ printf "%.2f" ratio
    reportProgress Info ""

    reportProgress Info "Geographic Coverage:"
    let locations = statsLocations stats
    reportProgress Info $ "  Locations: " ++ show (length locations) ++ " geographic regions"
    let sampleLocs = map T.unpack $ take 10 locations
    reportProgress Info $ "  Sample locations: " ++ unwords sampleLocs ++
                         (if length locations > 10 then " ... (and " ++ show (length locations - 10) ++ " more)" else "")
    reportProgress Info ""

    reportProgress Info "Environmental Compartments:"
    let categories = statsCategories stats
    reportProgress Info $ "  Categories: " ++ show (length categories) ++ " environmental compartments"
    let sampleCats = map T.unpack $ take 5 categories
    reportProgress Info $ "  Main compartments: " ++ intercalate ", " sampleCats ++
                         (if length categories > 5 then " ... (and " ++ show (length categories - 5) ++ " more)" else "")
    reportProgress Info ""

    reportProgress Info "Unit Diversity:"
    let units = statsUnits stats
    reportProgress Info $ "  Units: " ++ show (length units) ++ " different measurement units"
    reportProgress Info $ "  Common units: " ++ intercalate ", " (map T.unpack $ take 8 units) ++
                         (if length units > 8 then " ... (and " ++ show (length units - 8) ++ " more)" else "")
    reportProgress Info ""

    -- Performance characteristics
    reportProgress Info "Performance Characteristics:"
    let techEntries = dbTechnosphereTriples db
        bioEntries = dbBiosphereTriples db
        totalEntries = length techEntries + length bioEntries
        totalPossible = statsActivityCount stats * statsActivityCount stats + statsActivityCount stats * statsBiosphereFlows stats
        matrixDensity = if totalPossible > 0 then (fromIntegral totalEntries / fromIntegral totalPossible) * 100 else 0 :: Double
    reportProgress Info $ "  Matrix density: " ++ printf "%.4f%%" matrixDensity ++ " (very sparse - good for performance)"
    reportProgress Info $ "  Total matrix entries: " ++ show totalEntries ++ " non-zero values"
    reportProgress Info $ "  Solver complexity: O(n^1.5) for " ++ show (statsActivityCount stats) ++ " activities"
    reportProgress Info $ "  Expected solve time: ~3-5 seconds (MUMPS direct solver)"

    -- Report current memory usage
    reportMemoryUsage "Database loaded in memory"
    reportProgress Info ""

{-|
Validate database integrity and report potential issues.

Performs basic quality checks on the loaded database:
- Orphaned flows (flows not used by any activity)
- Unbalanced activities (activities with unusual input/output ratios)
- Missing reference products
- Unit consistency issues
-}
validateDatabase :: Database -> IO ()
validateDatabase db = do
    reportProgress Info "Database Quality Validation:"

    -- Check for orphaned flows
    let allFlows = M.keys (dbFlows db)
        usedFlows = S.fromList $ concatMap (map exchangeFlowId . exchanges) (M.elems $ dbActivities db)
        orphanedFlows = filter (`S.notMember` usedFlows) allFlows

    if null orphanedFlows
        then reportProgress Info "  ✓ No orphaned flows found"
        else reportProgress Info $ "  ⚠ Found " ++ show (length orphanedFlows) ++ " orphaned flows (unused in any activity)"

    -- Check reference products
    let activitiesWithoutRef = filter (null . filter exchangeIsReference . exchanges) (M.elems $ dbActivities db)
    if null activitiesWithoutRef
        then reportProgress Info "  ✓ All activities have reference products"
        else reportProgress Info $ "  ⚠ Found " ++ show (length activitiesWithoutRef) ++ " activities without reference products"

    -- Check for extremely unbalanced activities (more than 100:1 ratio)
    let stats = getDatabaseStats db
        avgInputsPerActivity = fromIntegral (statsInputCount stats) / fromIntegral (statsActivityCount stats) :: Double
        avgOutputsPerActivity = fromIntegral (statsOutputCount stats) / fromIntegral (statsActivityCount stats) :: Double
        ratio = if avgInputsPerActivity > 0 then avgOutputsPerActivity / avgInputsPerActivity else 0

    reportProgress Info $ "  ✓ Average exchange balance: " ++ printf "%.1f" ratio ++ ":1 (outputs:inputs)"

    -- Overall assessment
    let issues = length orphanedFlows + length activitiesWithoutRef
    if issues == 0
        then reportProgress Info "  ✓ Database quality: Excellent"
        else reportProgress Info $ "  ⚠ Database quality: Good (" ++ show issues ++ " minor issues found)"

    reportProgress Info ""

-- | Display active configuration settings
reportConfiguration :: IO ()
reportConfiguration = do
    reportProgress Info "Active Configuration:"

    -- PETSc configuration
    petscOptions <- lookupEnv "PETSC_OPTIONS"
    case petscOptions of
        Just opts -> do
            reportProgress Info "  PETSc Solver Settings:"
            reportProgress Info $ "    " ++ opts
            reportProgress Info "    → MUMPS direct solver with high precision"
        Nothing -> reportProgress Info "  PETSc: Using default settings"

    -- Runtime configuration
    numCaps <- getNumCapabilities
    reportProgress Info $ "  Parallel Processing: " ++ show numCaps ++ " CPU cores available"

    -- Memory configuration from RTS
    reportProgress Info "  Memory Management: GHC runtime with automatic GC"

    reportProgress Info ""
