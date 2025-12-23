{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module LCA.DatabaseManager
    ( -- * Types
      DatabaseManager(..)
    , LoadedDatabase(..)
    , DatabaseStatus(..)
      -- * Initialization
    , initDatabaseManager
    , initSingleDatabaseManager
      -- * Operations
    , activateDatabase
    , getDatabase
    , getCurrentDatabase
    , getCurrentDatabaseName
    , listDatabases
      -- * Internal (for Main.hs to load database)
    , loadDatabaseFromConfig
    ) where

import Control.Concurrent.STM
import Control.Monad (forM, forM_)
import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:), (.:?))
import qualified Data.Aeson as A
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as U
import GHC.Generics (Generic)
import System.Directory (doesFileExist, doesDirectoryExist)

import LCA.Config
import LCA.Matrix (precomputeMatrixFactorization, addFactorizationToDatabase)
import LCA.Matrix.SharedSolver (SharedSolver, createSharedSolver)
import LCA.Progress (reportProgress, reportError, ProgressLevel(..))
import LCA.Query (buildDatabaseWithMatrices)
import LCA.SynonymDB (SynonymDB)
import LCA.Types (Database(..), SparseTriple(..), SimpleDatabase(..), initializeRuntimeFields)
import qualified EcoSpold.Loader as Loader
import System.FilePath (takeExtension, dropExtension)

-- | A fully loaded database with solver ready for queries
data LoadedDatabase = LoadedDatabase
    { ldDatabase     :: !Database
    , ldSharedSolver :: !SharedSolver
    , ldConfig       :: !DatabaseConfig
    }

-- | Status of a database for API responses
data DatabaseStatus = DatabaseStatus
    { dsName        :: !Text           -- Internal identifier
    , dsDisplayName :: !Text           -- Human-readable name for UI
    , dsDescription :: !(Maybe Text)
    , dsActive      :: !Bool           -- Configured as active
    , dsLoaded      :: !Bool           -- Currently in memory
    , dsCached      :: !Bool           -- Cache file exists (kept for API compat)
    , dsPath        :: !Text           -- Data path
    } deriving (Show, Eq, Generic)

instance ToJSON DatabaseStatus where
    toJSON DatabaseStatus{..} = A.object
        [ "dsName" .= dsName
        , "dsDisplayName" .= dsDisplayName
        , "dsDescription" .= dsDescription
        , "dsActive" .= dsActive
        , "dsLoaded" .= dsLoaded
        , "dsCached" .= dsCached
        , "dsPath" .= dsPath
        ]

instance FromJSON DatabaseStatus where
    parseJSON = A.withObject "DatabaseStatus" $ \v -> DatabaseStatus
        <$> v .: "dsName"
        <*> v .: "dsDisplayName"
        <*> v .:? "dsDescription"
        <*> v .: "dsActive"
        <*> v .: "dsLoaded"
        <*> v .: "dsCached"
        <*> v .: "dsPath"

-- | The database manager maintains state for multiple databases
-- All active databases are pre-loaded at startup for instant switching
data DatabaseManager = DatabaseManager
    { dmLoadedDbs     :: !(TVar (Map Text LoadedDatabase))  -- All loaded databases
    , dmCurrentName   :: !(TVar (Maybe Text))               -- Name of current database
    , dmAvailableDbs  :: !(TVar (Map Text DatabaseConfig))  -- All configured databases
    , dmSynonymDB     :: !SynonymDB                         -- Shared synonym database
    , dmNoCache       :: !Bool                              -- Caching disabled flag
    }

-- | Initialize database manager from config
-- Pre-loads ALL active databases at startup for instant switching
initDatabaseManager :: Config -> SynonymDB -> Bool -> IO DatabaseManager
initDatabaseManager config synonymDB noCache = do
    let activeDbs = getActiveDatabases config
        allDbs = cfgDatabases config

    -- Create TVars
    loadedDbsVar <- newTVarIO M.empty
    currentNameVar <- newTVarIO Nothing
    availableDbsVar <- newTVarIO $ M.fromList [(dcName dc, dc) | dc <- allDbs]

    let manager = DatabaseManager
            { dmLoadedDbs = loadedDbsVar
            , dmCurrentName = currentNameVar
            , dmAvailableDbs = availableDbsVar
            , dmSynonymDB = synonymDB
            , dmNoCache = noCache
            }

    -- Pre-load ALL active databases at startup
    reportProgress Info $ "Pre-loading " ++ show (length activeDbs) ++ " active database(s)..."
    forM_ activeDbs $ \dbConfig -> do
        reportProgress Info $ "Loading database: " <> T.unpack (dcDisplayName dbConfig)
        result <- loadDatabaseFromConfig dbConfig synonymDB noCache
        case result of
            Right loaded -> do
                atomically $ modifyTVar' loadedDbsVar (M.insert (dcName dbConfig) loaded)
                reportProgress Info $ "  ✓ Loaded: " <> T.unpack (dcDisplayName dbConfig)
            Left err ->
                reportError $ "  ✗ Failed to load " <> T.unpack (dcName dbConfig) <> ": " <> T.unpack err

    -- Set default as current
    case getDefaultDatabase config of
        Just defaultDb -> do
            atomically $ writeTVar currentNameVar (Just (dcName defaultDb))
            reportProgress Info $ "Default database set to: " <> T.unpack (dcDisplayName defaultDb)
        Nothing -> do
            -- If no default specified, use first active database
            case activeDbs of
                (first:_) -> do
                    atomically $ writeTVar currentNameVar (Just (dcName first))
                    reportProgress Info $ "Default database set to: " <> T.unpack (dcDisplayName first)
                [] -> reportProgress Info "No active databases configured"

    -- Report final status
    loadedCount <- atomically $ M.size <$> readTVar loadedDbsVar
    reportProgress Info $ "Multi-database mode: " ++ show loadedCount ++ " database(s) loaded"

    return manager

-- | Initialize a single-database manager (for --data mode)
-- Creates a config with one active database and initializes it
initSingleDatabaseManager :: FilePath -> SynonymDB -> Bool -> IO DatabaseManager
initSingleDatabaseManager dataPath synonymDB noCache = do
    let dbConfig = DatabaseConfig
            { dcName = "default"
            , dcDisplayName = T.pack dataPath  -- Use path as display name
            , dcPath = dataPath
            , dcDescription = Just "Single database mode (--data)"
            , dcActive = True
            , dcDefault = True
            , dcActivityAliases = M.empty
            }

    let config = Config
            { cfgServer = defaultServerConfig
            , cfgDatabases = [dbConfig]
            , cfgMethods = []
            }

    initDatabaseManager config synonymDB noCache

-- | Activate a database by name (instant switch - already loaded)
activateDatabase :: DatabaseManager -> Text -> IO (Either Text LoadedDatabase)
activateDatabase manager dbName = do
    loadedDbs <- readTVarIO (dmLoadedDbs manager)
    case M.lookup dbName loadedDbs of
        Nothing -> do
            -- Check if it's configured but not loaded (inactive)
            availableDbs <- readTVarIO (dmAvailableDbs manager)
            case M.lookup dbName availableDbs of
                Nothing -> return $ Left $ "Database not found: " <> dbName
                Just cfg | not (dcActive cfg) ->
                    return $ Left $ "Database is inactive (set active=true in config to load at startup): " <> dbName
                Just _ ->
                    return $ Left $ "Database not loaded (this shouldn't happen for active databases): " <> dbName
        Just loaded -> do
            -- Instant switch - just update current name
            atomically $ writeTVar (dmCurrentName manager) (Just dbName)
            return $ Right loaded

-- | Get a database by name
getDatabase :: DatabaseManager -> Text -> IO (Maybe LoadedDatabase)
getDatabase manager dbName = do
    loadedDbs <- readTVarIO (dmLoadedDbs manager)
    return $ M.lookup dbName loadedDbs

-- | Get the currently active database
getCurrentDatabase :: DatabaseManager -> IO (Maybe LoadedDatabase)
getCurrentDatabase manager = do
    currentName <- readTVarIO (dmCurrentName manager)
    case currentName of
        Nothing -> return Nothing
        Just name -> getDatabase manager name

-- | Get the name of the currently active database
getCurrentDatabaseName :: DatabaseManager -> IO (Maybe Text)
getCurrentDatabaseName manager = readTVarIO (dmCurrentName manager)

-- | List all databases with their status
listDatabases :: DatabaseManager -> IO [DatabaseStatus]
listDatabases manager = do
    availableDbs <- readTVarIO (dmAvailableDbs manager)
    loadedDbs <- readTVarIO (dmLoadedDbs manager)
    currentName <- readTVarIO (dmCurrentName manager)

    forM (M.toList availableDbs) $ \(name, config) -> do
        let isLoaded = M.member name loadedDbs
            isCurrent = currentName == Just name
        return DatabaseStatus
            { dsName = name
            , dsDisplayName = dcDisplayName config
            , dsDescription = dcDescription config
            , dsActive = dcActive config
            , dsLoaded = isLoaded
            , dsCached = isLoaded  -- If loaded, we have it cached in memory
            , dsPath = T.pack (dcPath config)
            }

-- | Check if a file path is a cache file
isCacheFile :: FilePath -> Bool
isCacheFile path =
    let ext = takeExtension path
        ext2 = takeExtension (dropExtension path)
    in ext == ".bin" || (ext == ".zst" && ext2 == ".bin")

-- | Load a database from its configuration
loadDatabaseFromConfig :: DatabaseConfig -> SynonymDB -> Bool -> IO (Either Text LoadedDatabase)
loadDatabaseFromConfig dbConfig synonymDB noCache = do
    let path = dcPath dbConfig
        aliases = dcActivityAliases dbConfig

    -- Check if path exists
    isFile <- doesFileExist path
    isDir <- doesDirectoryExist path

    if not isFile && not isDir
        then return $ Left $ "Path does not exist: " <> T.pack path
        else do
            -- Load raw database
            reportProgress Info $ "Loading database from: " <> path
            dbResult <- loadDatabaseRaw (dcName dbConfig) aliases path noCache

            case dbResult of
                Left err -> return $ Left err
                Right dbRaw -> do
                    -- Initialize runtime fields (synonym DB and flow name index)
                    let database = initializeRuntimeFields dbRaw synonymDB

                    -- Pre-compute matrix factorization
                    reportProgress Info "Pre-computing matrix factorization..."
                    let techTriples = dbTechnosphereTriples database
                        activityCount = dbActivityCount database
                        techTriplesInt = [(fromIntegral i, fromIntegral j, v) | SparseTriple i j v <- U.toList techTriples]
                        activityCountInt = fromIntegral activityCount

                    factorization <- precomputeMatrixFactorization (dcName dbConfig) techTriplesInt activityCountInt
                    let databaseWithFact = addFactorizationToDatabase database factorization

                    -- Create shared solver
                    reportProgress Info "Creating shared solver..."
                    sharedSolver <- createSharedSolver (dbCachedFactorization databaseWithFact) techTriplesInt activityCountInt

                    return $ Right LoadedDatabase
                        { ldDatabase = databaseWithFact
                        , ldSharedSolver = sharedSolver
                        , ldConfig = dbConfig
                        }

-- | Load raw database from path (file or directory)
-- Aliases are used for EcoSpold1 supplier linking
loadDatabaseRaw :: T.Text -> M.Map T.Text T.Text -> FilePath -> Bool -> IO (Either Text Database)
loadDatabaseRaw dbName aliases path noCache = do
    isFile <- doesFileExist path
    if isFile && isCacheFile path
        then do
            -- Direct cache file (aliases don't apply - cache was built with them)
            mDb <- Loader.loadDatabaseFromCacheFile path
            case mDb of
                Just db -> return $ Right db
                Nothing -> return $ Left $ "Failed to load cache file: " <> T.pack path
        else if noCache
            then do
                -- No caching: load and build from scratch
                simpleDb <- Loader.loadAllSpoldsWithFlowsAndAliases aliases path
                !db <- buildDatabaseWithMatrices
                    (sdbActivities simpleDb)
                    (sdbFlows simpleDb)
                    (sdbUnits simpleDb)
                return $ Right db
            else do
                -- Try to load from cache, build if missing
                mCachedDb <- Loader.loadCachedDatabaseWithMatrices dbName path
                case mCachedDb of
                    Just db -> return $ Right db
                    Nothing -> do
                        -- Build and cache (with aliases applied)
                        simpleDb <- Loader.loadAllSpoldsWithFlowsAndAliases aliases path
                        !db <- buildDatabaseWithMatrices
                            (sdbActivities simpleDb)
                            (sdbFlows simpleDb)
                            (sdbUnits simpleDb)
                        Loader.saveCachedDatabaseWithMatrices dbName path db
                        return $ Right db
