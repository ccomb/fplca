{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad (forM_, unless)
import Control.Exception (catch, SomeException)
import Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import GHC.Conc (getNumCapabilities)
import Options.Applicative
import qualified System.Directory
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import qualified System.FilePath
import System.IO (hPutStrLn, stderr, hFlush, stdout)
import System.Posix.Signals (installHandler, Handler(Ignore), sigPIPE)
import Text.Printf (printf)

-- fpLCA imports
import LCA.Auth (basicAuthMiddleware)
import LCA.CLI.Command
import LCA.CLI.Parser
import LCA.CLI.Types
import LCA.CLI.Types (Command(Server), ServerOptions(..))
import LCA.Matrix (initializePetscForServer, precomputeMatrixFactorization, addFactorizationToDatabase)
import LCA.Matrix.SharedSolver (SharedSolver, createSharedSolver)
import LCA.Progress
import LCA.Query (buildDatabaseWithMatrices)
import LCA.Types
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified EcoSpold.Loader
import EcoSpold.Loader (loadAllSpoldsWithFlows, loadCachedDatabaseWithMatrices, saveCachedDatabaseWithMatrices)

-- For server mode
import LCA.API (LCAAPI, lcaAPI, lcaServer)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.Wai (Application, Request (..), Response, ResponseReceived (..), defaultRequest, rawPathInfo, responseStatus, requestMethod, pathInfo)
import Network.Wai.Application.Static (defaultWebAppSettings, ssIndices, ssRedirectToIndex, staticApp)
import Network.Wai.Handler.Warp (run)
import Servant
import WaiAppStatic.Types (StaticSettings, toPiece, unsafeToPiece)

-- | Main entry point for the refactored CLI
main :: IO ()
main = do
  -- Parse CLI arguments using new parser
  cliConfig <- execParser cliParserInfo

  -- Validate CLI configuration
  validateCLIConfig cliConfig

  -- Resolve data directory with priority: --data > $DATADIR > current directory
  dataDirectory <- resolveDataDirectory (globalOptions cliConfig)

  -- Load database with caching
  database <- loadDatabase dataDirectory (noCache (globalOptions cliConfig))

  -- Display configuration and database stats
  reportConfiguration (globalOptions cliConfig) dataDirectory
  validateAndReportDatabase database

  -- Execute the command
  case LCA.CLI.Types.command cliConfig of
    Server serverOpts -> do
      let port = serverPort serverOpts

      -- Install SIGPIPE handler to prevent broken pipe crashes
      reportProgress Info "Installing SIGPIPE handler to prevent client disconnect crashes"
      _ <- installHandler sigPIPE Ignore Nothing

      -- Initialize PETSc/MPI context once for the server's lifetime
      reportProgress Info "Initializing PETSc/MPI for persistent matrix operations"
      initializePetscForServer

      -- Perform global matrix factorization once
      let techTriples = dbTechnosphereTriples database
          activityCount = dbActivityCount database
          -- Convert Int32 to Int for PETSc functions
          techTriplesInt = [(fromIntegral i, fromIntegral j, v) | SparseTriple i j v <- U.toList techTriples]
          activityCountInt = fromIntegral activityCount
      reportProgress Info "Pre-computing matrix factorization for fast concurrent inventory calculations"
      factorization <- precomputeMatrixFactorization techTriplesInt activityCountInt
      let databaseWithFactorization = addFactorizationToDatabase database factorization

      -- Create shared solver with cached factorization for fast concurrent solves
      reportProgress Info "Creating shared solver with cached factorization"
      sharedSolver <- createSharedSolver (dbCachedFactorization databaseWithFactorization) techTriplesInt activityCountInt

      -- Get password from CLI or env var
      password <- case serverPassword serverOpts of
        Just pwd -> return (Just pwd)
        Nothing -> lookupEnv "FPLCA_PASSWORD"

      reportProgress Info $ "Starting API server on port " ++ show port
      reportProgress Info $ "Tree depth: " ++ show (treeDepth (globalOptions cliConfig))
      case password of
        Just _ -> reportProgress Info "Authentication: ENABLED (HTTP Basic Auth)"
        Nothing -> reportProgress Info "Authentication: DISABLED (use --password or FPLCA_PASSWORD to enable)"
      reportProgress Info "Web interface available at: http://localhost/"

      let baseApp = Main.createCombinedApp databaseWithFactorization (treeDepth (globalOptions cliConfig)) sharedSolver
          finalApp = case password of
            Just pwd -> basicAuthMiddleware (C8.pack pwd) baseApp
            Nothing -> baseApp
      run port finalApp

    _ -> executeCommand cliConfig database

-- | Load database with optional caching
-- Supports three modes:
-- 1. Direct cache file (.bin or .bin.zst)
-- 2. Directory with .spold files + caching
-- 3. Directory with .spold files without caching
loadDatabase :: FilePath -> Bool -> IO Database
loadDatabase dataPath disableCache = do
  -- Check if dataPath is a direct cache file
  isDirectCache <- isDirectCacheFile dataPath

  if isDirectCache
    then loadFromDirectCacheFile dataPath
    else loadFromDirectory dataPath disableCache

-- | Check if path points to a direct cache file
isDirectCacheFile :: FilePath -> IO Bool
isDirectCacheFile path = do
  exists <- System.Directory.doesFileExist path
  if exists
    then do
      let ext = System.FilePath.takeExtension path
      let ext2 = System.FilePath.takeExtension (System.FilePath.dropExtension path)
      -- Check for .bin or .bin.zst
      return $ ext == ".bin" || (ext == ".zst" && ext2 == ".bin")
    else return False

-- | Load database directly from cache file
loadFromDirectCacheFile :: FilePath -> IO Database
loadFromDirectCacheFile cacheFile = do
  reportProgress Info $ "Loading database directly from cache file: " ++ cacheFile
  cachedDb <- EcoSpold.Loader.loadDatabaseFromCacheFile cacheFile
  case cachedDb of
    Just db -> do
      reportProgress Info "Successfully loaded database from cache file"
      return db
    Nothing -> do
      reportError $ "Failed to load cache file: " ++ cacheFile
      exitFailure

-- | Load database from directory (existing logic)
loadFromDirectory :: FilePath -> Bool -> IO Database
loadFromDirectory dataDirectory disableCache =
  if disableCache
    then do
      reportProgress Info "Loading activities with flow deduplication (caching disabled)"
      simpleDb <- loadAllSpoldsWithFlows dataDirectory
      reportProgress Info "Building indexes and pre-computing matrices (no caching)"
      !db <- buildDatabaseWithMatrices (sdbActivities simpleDb) (sdbFlows simpleDb) (sdbUnits simpleDb)
      return db
    else do
      reportCacheOperation "Checking for cached Database with matrices"
      cachedDb <- loadCachedDatabaseWithMatrices dataDirectory
      case cachedDb of
        Just db -> do
          reportCacheOperation "Using cached Database with pre-computed matrices"
          return db
        Nothing -> do
          reportCacheOperation "No matrix cache found, parsing EcoSpold XML files"
          simpleDb <- loadAllSpoldsWithFlows dataDirectory

          reportProgress Info "Building indexes and pre-computing matrices for efficient queries"
          !db <- buildDatabaseWithMatrices (sdbActivities simpleDb) (sdbFlows simpleDb) (sdbUnits simpleDb)

          reportCacheOperation "Saving Database with matrices to cache for next time"
          saveCachedDatabaseWithMatrices dataDirectory db
          return db


-- | Report active configuration
reportConfiguration :: GlobalOptions -> FilePath -> IO ()
reportConfiguration globalOpts dataDirectory = do
  reportProgress Info "=== CONFIGURATION ==="
  reportProgress Info $ "Data directory: " ++ dataDirectory
  reportProgress Info $ "Tree depth: " ++ show (treeDepth globalOpts)
  reportProgress Info $ "Output format: " ++ maybe "default" show (format globalOpts)
  reportProgress Info $ "Caching: " ++ (if noCache globalOpts then "disabled" else "enabled")

  -- PETSc configuration
  petscOptions <- lookupEnv "PETSC_OPTIONS"
  case petscOptions of
    Just opts -> do
      reportProgress Info "PETSc Solver Settings:"
      reportProgress Info $ "  " ++ opts
      reportProgress Info "  → MUMPS direct solver with high precision"
    Nothing -> reportProgress Info "PETSc: Using default settings"

  -- Runtime configuration
  numCaps <- getNumCapabilities
  reportProgress Info $ "Parallel Processing: " ++ show numCaps ++ " CPU cores available"
  reportProgress Info "Memory Management: GHC runtime with automatic GC"
  reportProgress Info ""

-- | Validate database and report statistics
validateAndReportDatabase :: Database -> IO ()
validateAndReportDatabase database = do
  -- Validate database quality
  validateDatabase database

  -- Display database statistics (stats functions not yet implemented after refactoring)
  reportProgress Info "=== DATABASE LOADED ==="
  -- let stats = getDatabaseStats database
  -- displayDatabaseStats database stats
  reportProgress Info ""

-- | Display database statistics in a user-friendly format
-- (Commented out until DatabaseStats is re-implemented after refactoring)
{-
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
  let ratio =
        if statsInputCount stats > 0
          then fromIntegral (statsOutputCount stats) / fromIntegral (statsInputCount stats) :: Double
          else 0
  reportProgress Info $ "  Output/Input ratio: " ++ printf "%.2f" ratio
  reportProgress Info ""

  reportProgress Info "Geographic Coverage:"
  let locations = statsLocations stats
  reportProgress Info $ "  Locations: " ++ show (length locations) ++ " geographic regions"
  let sampleLocs = map T.unpack $ take 10 locations
  reportProgress Info $
    "  Sample locations: "
      ++ unwords sampleLocs
      ++ (if length locations > 10 then " ... (and " ++ show (length locations - 10) ++ " more)" else "")
  reportProgress Info ""

  reportProgress Info "Environmental Compartments:"
  let categories = statsCategories stats
  reportProgress Info $ "  Categories: " ++ show (length categories) ++ " environmental compartments"
  let sampleCats = map T.unpack $ take 5 categories
  reportProgress Info $
    "  Main compartments: "
      ++ intercalate ", " sampleCats
      ++ (if length categories > 5 then " ... (and " ++ show (length categories - 5) ++ " more)" else "")
  reportProgress Info ""

  reportProgress Info "Unit Diversity:"
  let units = statsUnits stats
  reportProgress Info $ "  Units: " ++ show (length units) ++ " different measurement units"
  reportProgress Info $
    "  Common units: "
      ++ intercalate ", " (map T.unpack $ take 8 units)
      ++ (if length units > 8 then " ... (and " ++ show (length units - 8) ++ " more)" else "")
  reportProgress Info ""

  -- Performance characteristics
  reportProgress Info "Performance Characteristics:"
  let techEntries = dbTechnosphereTriples db
      bioEntries = dbBiosphereTriples db
      totalEntries = V.length techEntries + V.length bioEntries
      totalPossible = statsActivityCount stats * statsActivityCount stats + statsActivityCount stats * statsBiosphereFlows stats
      matrixDensity = if totalPossible > 0 then (fromIntegral totalEntries / fromIntegral totalPossible) * 100 else 0 :: Double
  reportProgress Info $ "  Matrix density: " ++ printf "%.4f%%" matrixDensity ++ " (very sparse - good for performance)"
  reportProgress Info $ "  Total matrix entries: " ++ show totalEntries ++ " non-zero values"
  reportProgress Info $ "  Solver complexity: O(n^1.5) for " ++ show (statsActivityCount stats) ++ " activities"
  reportProgress Info $ "  Expected solve time: ~3-5 seconds (MUMPS direct solver)"

  -- Report current memory usage
  reportMemoryUsage "Database loaded in memory"
  reportProgress Info ""
-}

-- | Validate database integrity and report potential issues
validateDatabase :: Database -> IO ()
validateDatabase db = do
  reportProgress Info "Database Quality Validation:"

  -- Check for orphaned flows
  let allFlows = M.keys (dbFlows db)
      usedFlows = S.fromList $ concatMap (map exchangeFlowId . exchanges) (V.toList $ dbActivities db)
      orphanedFlows = filter (`S.notMember` usedFlows) allFlows

  if null orphanedFlows
    then reportProgress Info "  ✓ No orphaned flows found"
    else reportProgress Info $ "  ⚠ Found " ++ show (length orphanedFlows) ++ " orphaned flows (unused in any activity)"

  -- Check reference products
  let activitiesWithoutRef = filter (null . filter exchangeIsReference . exchanges) (V.toList $ dbActivities db)
  if null activitiesWithoutRef
    then reportProgress Info "  ✓ All activities have reference products"
    else reportProgress Info $ "  ⚠ Found " ++ show (length activitiesWithoutRef) ++ " activities without reference products"

  -- Check for extremely unbalanced activities (more than 100:1 ratio)
  -- (Stats disabled until getDatabaseStats is re-implemented)
  {-
  let stats = getDatabaseStats db
      avgInputsPerActivity = fromIntegral (statsInputCount stats) / fromIntegral (statsActivityCount stats) :: Double
      avgOutputsPerActivity = fromIntegral (statsOutputCount stats) / fromIntegral (statsActivityCount stats) :: Double
      ratio = if avgInputsPerActivity > 0 then avgOutputsPerActivity / avgInputsPerActivity else 0

  reportProgress Info $ "  ✓ Average exchange balance: " ++ printf "%.1f" ratio ++ ":1 (outputs:inputs)"
  -}

  -- Overall assessment
  let issues = length orphanedFlows + length activitiesWithoutRef
  if issues == 0
    then reportProgress Info "  ✓ Database quality: Excellent"
    else reportProgress Info $ "  ⚠ Database quality: Good (" ++ show issues ++ " minor issues found)"

  reportProgress Info ""

-- | Create a combined Wai application serving both API and static files with request logging
createCombinedApp :: Database -> Int -> SharedSolver -> Application
createCombinedApp database maxTreeDepth sharedSolver req respond = do
  let path = rawPathInfo req
      queryString = rawQueryString req
      fullUrl = path <> queryString

  -- Simple request logging (like nginx/apache access log) with explicit flush
  putStrLn $ C8.unpack (requestMethod req) ++ " " ++ C8.unpack fullUrl
  hFlush stdout

  -- Route requests based on path prefix
  if C8.pack "/api/" `BS.isPrefixOf` path
    then
      -- API requests go to Servant
      serve lcaAPI (lcaServer database maxTreeDepth sharedSolver Nothing) req respond
    else if C8.pack "/static/" `BS.isPrefixOf` path
      then
        -- Static files: strip /static prefix and serve from web/dist/
        let strippedPath = BS.drop 7 path  -- Remove "/static" (7 chars), keep the "/"
            -- Also update pathInfo by removing "static" segment
            originalPathInfo = pathInfo req
            newPathInfo = case originalPathInfo of
              (segment:rest) | segment == T.pack "static" -> rest
              other -> other
            staticReq = req { rawPathInfo = strippedPath, pathInfo = newPathInfo }
            staticSettings = (defaultWebAppSettings "web/dist")
              { ssIndices = [unsafeToPiece (T.pack "index.html")] }
         in staticApp staticSettings staticReq respond
      else
        -- Everything else: serve index.html for SPA routing
        let staticSettings = (defaultWebAppSettings "web/dist")
              { ssIndices = [unsafeToPiece (T.pack "index.html")] }
            indexReq = req { rawPathInfo = C8.pack "/", pathInfo = [] }
         in staticApp staticSettings indexReq respond

-- | Validate CLI configuration for consistency
validateCLIConfig :: CLIConfig -> IO ()
validateCLIConfig (CLIConfig globalOpts _) = do
  -- CSV format requires jsonPath
  case (format globalOpts, jsonPath globalOpts) of
    (Just CSV, Nothing) -> do
      hPutStrLn stderr "--format csv requires --jsonpath. Use --jsonpath to specify which data to extract"
      hPutStrLn stderr "Examples:"
      hPutStrLn stderr "  --jsonpath 'srResults' (for search results)"
      hPutStrLn stderr "  --jsonpath 'piActivity.pfaExchanges' (for activity exchanges)"
      hPutStrLn stderr "  --jsonpath 'teEdges' (for tree edges)"
      hPutStrLn stderr "  --jsonpath 'ieFlows' (for inventory flows)"
      exitFailure
    (Just fmt, Just _) | fmt /= CSV -> do
      hPutStrLn stderr "--jsonpath can only be used with --format csv"
      exitFailure
    _ -> pure ()
