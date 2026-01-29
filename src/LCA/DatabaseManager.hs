{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
      -- * Load/Unload
    , loadDatabase
    , unloadDatabase
    , addDatabase
    , removeDatabase
      -- * Internal (for Main.hs to load database)
    , loadDatabaseFromConfig
    ) where

import Control.Concurrent.STM
import Control.Exception (SomeException)
import qualified Control.Exception
import Control.Monad (forM, forM_, when)
import System.Mem (performGC)
import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:), (.:?))
import Data.Aeson.Types (Parser)
import qualified Data.Aeson as A
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as U
import GHC.Generics (Generic)
import System.Directory (doesFileExist, doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension)
import Data.Char (toLower)
import LCA.UploadedDatabase (isUploadedPath)

import LCA.Config
import LCA.Matrix (precomputeMatrixFactorization, addFactorizationToDatabase, clearCachedKspSolver)
import LCA.Matrix.SharedSolver (SharedSolver, createSharedSolver)
import LCA.Progress (reportProgress, reportError, ProgressLevel(..))
import LCA.Query (buildDatabaseWithMatrices)
import LCA.SynonymDB (SynonymDB)
import LCA.Types (Database(..), SparseTriple(..), SimpleDatabase(..), initializeRuntimeFields, Activity(..), Exchange(..), UUID, exchangeFlowId, exchangeIsReference)
import qualified LCA.UnitConversion as UnitConversion
import qualified EcoSpold.Loader as Loader
import qualified LCA.Upload as Upload
import qualified LCA.UploadedDatabase as UploadedDB
import qualified SimaPro.Parser as SimaPro
import System.FilePath (takeExtension, dropExtension, (</>))
import System.Directory (removeDirectoryRecursive)

-- | A fully loaded database with solver ready for queries
data LoadedDatabase = LoadedDatabase
    { ldDatabase     :: !Database
    , ldSharedSolver :: !SharedSolver
    , ldConfig       :: !DatabaseConfig
    }

-- | Status of a database for API responses
data DatabaseStatus = DatabaseStatus
    { dsName        :: !Text           -- Internal identifier (slug)
    , dsDisplayName :: !Text           -- Human-readable name for UI
    , dsDescription :: !(Maybe Text)
    , dsLoadAtStartup :: !Bool         -- Configured to load at startup
    , dsLoaded      :: !Bool           -- Currently in memory
    , dsCached      :: !Bool           -- Cache file exists
    , dsIsUploaded  :: !Bool           -- True if path starts with "uploads/"
    , dsPath        :: !Text           -- Data path
    , dsFormat      :: !(Maybe Upload.DatabaseFormat)  -- Detected format
    } deriving (Show, Eq, Generic)

instance ToJSON DatabaseStatus where
    toJSON DatabaseStatus{..} = A.object
        [ "dsName" .= dsName
        , "dsDisplayName" .= dsDisplayName
        , "dsDescription" .= dsDescription
        , "dsLoadAtStartup" .= dsLoadAtStartup
        , "dsLoaded" .= dsLoaded
        , "dsCached" .= dsCached
        , "dsIsUploaded" .= dsIsUploaded
        , "dsPath" .= dsPath
        , "dsFormat" .= fmap formatToDisplayText dsFormat
        ]
      where
        formatToDisplayText Upload.EcoSpold2 = "EcoSpold 2" :: T.Text
        formatToDisplayText Upload.EcoSpold1 = "EcoSpold 1"
        formatToDisplayText Upload.SimaProCSV = "SimaPro CSV"
        formatToDisplayText Upload.UnknownFormat = ""

instance FromJSON DatabaseStatus where
    parseJSON = A.withObject "DatabaseStatus" $ \v -> DatabaseStatus
        <$> v .: "dsName"
        <*> v .: "dsDisplayName"
        <*> v .:? "dsDescription"
        <*> v .: "dsLoadAtStartup"
        <*> v .: "dsLoaded"
        <*> v .: "dsCached"
        <*> v .: "dsIsUploaded"
        <*> v .: "dsPath"
        <*> (parseFormat <$> v .:? "dsFormat")
      where
        parseFormat :: Maybe T.Text -> Maybe Upload.DatabaseFormat
        parseFormat Nothing = Nothing
        parseFormat (Just "EcoSpold 2") = Just Upload.EcoSpold2
        parseFormat (Just "EcoSpold 1") = Just Upload.EcoSpold1
        parseFormat (Just "SimaPro CSV") = Just Upload.SimaProCSV
        parseFormat (Just _) = Just Upload.UnknownFormat

-- | The database manager maintains state for multiple databases
-- Databases with load=true are pre-loaded at startup for instant switching
data DatabaseManager = DatabaseManager
    { dmLoadedDbs     :: !(TVar (Map Text LoadedDatabase))  -- All loaded databases
    , dmCurrentName   :: !(TVar (Maybe Text))               -- Name of current database
    , dmAvailableDbs  :: !(TVar (Map Text DatabaseConfig))  -- All configured databases
    , dmSynonymDB     :: !SynonymDB                         -- Shared synonym database
    , dmNoCache       :: !Bool                              -- Caching disabled flag
    , dmUnitConfig    :: !UnitConversion.UnitConfig         -- Unit configuration
    }

-- | Initialize database manager from config
-- Pre-loads databases with load=true at startup
-- Also discovers uploaded databases from uploads/ directory
initDatabaseManager :: Config -> SynonymDB -> Bool -> Maybe FilePath -> IO DatabaseManager
initDatabaseManager config synonymDB noCache _configPath = do
    -- Get configured databases and detect their format
    configuredDbs <- forM (cfgDatabases config) $ \dbConfig -> do
        format <- Upload.detectDatabaseFormat (dcPath dbConfig)
        return dbConfig { dcFormat = Just format }

    -- Discover uploaded databases from uploads/ directory (self-describing with meta.toml)
    uploadedDbs <- discoverUploadedDatabases

    -- Merge configured + uploaded
    let allDbs = configuredDbs ++ uploadedDbs
        loadableDbs = filter dcLoad allDbs

    -- Build UnitConfig from config (or use defaults)
    unitConfig <- case cfgUnits config of
        Nothing -> do
            reportProgress Info "Using default unit configuration"
            return UnitConversion.defaultUnitConfig
        Just unitsToml -> do
            let aliases = M.map (\uac -> (uacDim uac, uacFactor uac)) (ucAliases unitsToml)
            case UnitConversion.buildUnitConfigFromToml (ucDimensions unitsToml) aliases of
                Right cfg -> do
                    reportProgress Info $ "Loaded " ++ show (M.size aliases) ++ " custom unit aliases from config"
                    return cfg
                Left err -> do
                    reportError $ "Invalid [units] config: " <> T.unpack err <> " - using defaults"
                    return UnitConversion.defaultUnitConfig

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
            , dmUnitConfig = unitConfig
            }

    -- Pre-load databases with load=true at startup
    reportProgress Info $ "Pre-loading " ++ show (length loadableDbs) ++ " database(s) with load=true..."
    forM_ loadableDbs $ \dbConfig -> do
        reportProgress Info $ "Loading database: " <> T.unpack (dcDisplayName dbConfig)
        result <- loadDatabaseFromConfig dbConfig synonymDB noCache
        case result of
            Right loaded -> do
                atomically $ modifyTVar' loadedDbsVar (M.insert (dcName dbConfig) loaded)
                reportProgress Info $ "  ✓ Loaded: " <> T.unpack (dcDisplayName dbConfig)
            Left err ->
                reportError $ "  ✗ Failed to load " <> T.unpack (dcName dbConfig) <> ": " <> T.unpack err

    -- Set current database to first actually loaded database (not just configured as default)
    loadedDbs <- readTVarIO loadedDbsVar
    case M.keys loadedDbs of
        (firstLoaded:_) -> do
            atomically $ writeTVar currentNameVar (Just firstLoaded)
            reportProgress Info $ "Current database set to: " <> T.unpack firstLoaded
        [] -> reportProgress Info "No databases loaded at startup"

    -- Report final status
    loadedCount <- atomically $ M.size <$> readTVar loadedDbsVar
    reportProgress Info $ "Multi-database mode: " ++ show loadedCount ++ " database(s) loaded"

    return manager

-- | Discover uploaded databases from uploads/ directory
-- Reads meta.toml from each subdirectory and converts to DatabaseConfig
discoverUploadedDatabases :: IO [DatabaseConfig]
discoverUploadedDatabases = do
    uploads <- UploadedDB.discoverUploadedDatabases
    forM uploads $ \(slug, dirPath, meta) -> do
        reportProgress Info $ "Discovered uploaded database: " <> T.unpack slug
        return $ uploadMetaToConfig slug dirPath meta

-- | Convert UploadMeta to DatabaseConfig
uploadMetaToConfig :: Text -> FilePath -> UploadedDB.UploadMeta -> DatabaseConfig
uploadMetaToConfig slug dirPath meta = DatabaseConfig
    { dcName = slug
    , dcDisplayName = UploadedDB.umDisplayName meta
    , dcPath = dirPath </> UploadedDB.umDataPath meta  -- Full path to data
    , dcDescription = UploadedDB.umDescription meta
    , dcLoad = False  -- Never auto-load uploads
    , dcDefault = False
    , dcLocationAliases = M.empty
    , dcFormat = Just (UploadedDB.umFormat meta)
    }

-- | Initialize a single-database manager (for --data mode)
-- Creates a config with one database and initializes it
initSingleDatabaseManager :: FilePath -> SynonymDB -> Bool -> IO DatabaseManager
initSingleDatabaseManager dataPath synonymDB noCache = do
    -- Detect format for the data path
    format <- Upload.detectDatabaseFormat dataPath

    let dbConfig = DatabaseConfig
            { dcName = "default"
            , dcDisplayName = T.pack dataPath  -- Use path as display name
            , dcPath = dataPath
            , dcDescription = Just "Single database mode (--data)"
            , dcLoad = True
            , dcDefault = True
            , dcLocationAliases = M.empty
            , dcFormat = Just format
            }

    let config = Config
            { cfgServer = defaultServerConfig
            , cfgDatabases = [dbConfig]
            , cfgMethods = []
            , cfgUnits = Nothing
            }

    initDatabaseManager config synonymDB noCache Nothing

-- | Activate a database by name (instant switch - already loaded)
activateDatabase :: DatabaseManager -> Text -> IO (Either Text LoadedDatabase)
activateDatabase manager dbName = do
    loadedDbs <- readTVarIO (dmLoadedDbs manager)
    case M.lookup dbName loadedDbs of
        Nothing -> do
            -- Check if it's configured but not loaded
            availableDbs <- readTVarIO (dmAvailableDbs manager)
            case M.lookup dbName availableDbs of
                Nothing -> return $ Left $ "Database not found: " <> dbName
                Just cfg | not (dcLoad cfg) ->
                    return $ Left $ "Database is not loaded. Use the Load button to load it first: " <> dbName
                Just _ ->
                    return $ Left $ "Database not loaded (load=true but not in memory): " <> dbName
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
        return DatabaseStatus
            { dsName = name
            , dsDisplayName = dcDisplayName config
            , dsDescription = dcDescription config
            , dsLoadAtStartup = dcLoad config
            , dsLoaded = isLoaded
            , dsCached = isLoaded  -- If loaded, we have it cached in memory
            , dsIsUploaded = isUploadedPath (dcPath config)
            , dsPath = T.pack (dcPath config)
            , dsFormat = dcFormat config
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
        locationAliases = dcLocationAliases dbConfig

    -- Check if path exists
    isFile <- doesFileExist path
    isDir <- doesDirectoryExist path

    if not isFile && not isDir
        then return $ Left $ "Path does not exist: " <> T.pack path
        else do
            -- Load raw database
            reportProgress Info $ "Loading database from: " <> path
            dbResult <- loadDatabaseRaw (dcName dbConfig) locationAliases path noCache

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

-- | Detected format of a database directory
data DirectoryFormat = FormatSpold | FormatXML | FormatCSV | FormatUnknown
    deriving (Show, Eq)

-- | Detect the format of files in a directory
detectDirectoryFormat :: FilePath -> IO DirectoryFormat
detectDirectoryFormat path = do
    isDir <- doesDirectoryExist path
    isFile <- doesFileExist path
    if isFile
        then do
            -- Direct file: check extension
            let ext = map toLower (takeExtension path)
            return $ case ext of
                ".csv" -> FormatCSV
                ".spold" -> FormatSpold
                ".xml" -> FormatXML
                _ -> FormatUnknown
        else if isDir
            then do
                files <- listDirectory path
                let extensions = map (map toLower . takeExtension) files
                -- Check for different formats (in order of preference)
                if any (== ".spold") extensions
                    then return FormatSpold
                    else if any (== ".csv") extensions
                        then return FormatCSV
                        else if any (== ".xml") extensions
                            then return FormatXML
                            else return FormatUnknown
            else return FormatUnknown

-- | Find CSV files in a directory
findCSVFiles :: FilePath -> IO [FilePath]
findCSVFiles path = do
    files <- listDirectory path
    let csvFiles = filter (\f -> map toLower (takeExtension f) == ".csv") files
    return $ map (path </>) csvFiles

-- | Build activity map from list of activities
-- Creates (activityUUID, productUUID) -> Activity mapping
buildActivityMap :: [Activity] -> M.Map (UUID, UUID) Activity
buildActivityMap activities = M.fromList
    [ ((activityUUID, productUUID), activity)
    | activity <- activities
    , let activityUUID = SimaPro.generateActivityUUID (activityName activity <> "@" <> activityLocation activity)
    , let refExchanges = filter exchangeIsReference (exchanges activity)
    , refExchange <- take 1 refExchanges  -- Take first reference product
    , let productUUID = exchangeFlowId refExchange
    ]

-- | Load raw database from path (file or directory)
-- Location aliases are used for EcoSpold1 supplier linking (wrongLocation → correctLocation)
loadDatabaseRaw :: T.Text -> M.Map T.Text T.Text -> FilePath -> Bool -> IO (Either Text Database)
loadDatabaseRaw dbName locationAliases path noCache = do
    isFile <- doesFileExist path
    if isFile && isCacheFile path
        then do
            -- Direct cache file (aliases don't apply - cache was built with them)
            mDb <- Loader.loadDatabaseFromCacheFile path
            case mDb of
                Just db -> return $ Right db
                Nothing -> return $ Left $ "Failed to load cache file: " <> T.pack path
        else do
            -- Check what format the directory contains
            format <- detectDirectoryFormat path
            case format of
                FormatCSV -> do
                    -- Determine the CSV file path (direct file or find in directory)
                    isFileCheck <- doesFileExist path
                    csvFile <- if isFileCheck
                        then return path
                        else do
                            csvFiles <- findCSVFiles path
                            case csvFiles of
                                [] -> error $ "No CSV files found in: " ++ path
                                (f:_) -> return f
                    -- Try to load from cache first (same as EcoSpold)
                    if noCache
                        then do
                            -- No caching: parse CSV directly
                            reportProgress Info $ "Parsing SimaPro CSV: " <> csvFile
                            (activities, flowDB, unitDB) <- SimaPro.parseSimaProCSV csvFile
                            reportProgress Info $ "Building database from " <> show (length activities) <> " activities"
                            let activityMap = buildActivityMap activities
                            !db <- buildDatabaseWithMatrices activityMap flowDB unitDB
                            return $ Right db
                        else do
                            -- Try cache first
                            mCachedDb <- Loader.loadCachedDatabaseWithMatrices dbName path
                            case mCachedDb of
                                Just db -> return $ Right db
                                Nothing -> do
                                    -- Parse SimaPro CSV
                                    reportProgress Info $ "Parsing SimaPro CSV: " <> csvFile
                                    (activities, flowDB, unitDB) <- SimaPro.parseSimaProCSV csvFile
                                    reportProgress Info $ "Building database from " <> show (length activities) <> " activities"
                                    let activityMap = buildActivityMap activities
                                    !db <- buildDatabaseWithMatrices activityMap flowDB unitDB
                                    -- Save to cache for next time
                                    Loader.saveCachedDatabaseWithMatrices dbName path db
                                    return $ Right db
                FormatUnknown ->
                    return $ Left $ "No supported database files found in: " <> T.pack path <>
                                   ". Supported formats: EcoSpold v2 (.spold), EcoSpold v1 (.xml), SimaPro CSV (.csv)"
                -- FormatSpold and FormatXML use the same loader (handles both formats)
                _ ->
                    if noCache
                        then do
                            -- No caching: load and build from scratch
                            loadResult <- Loader.loadAllSpoldsWithLocationAliases locationAliases path
                            case loadResult of
                                Left err -> return $ Left err
                                Right simpleDb -> do
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
                                    -- Build and cache (with location aliases applied)
                                    loadResult <- Loader.loadAllSpoldsWithLocationAliases locationAliases path
                                    case loadResult of
                                        Left err -> return $ Left err
                                        Right simpleDb -> do
                                            !db <- buildDatabaseWithMatrices
                                                (sdbActivities simpleDb)
                                                (sdbFlows simpleDb)
                                                (sdbUnits simpleDb)
                                            Loader.saveCachedDatabaseWithMatrices dbName path db
                                            return $ Right db

-- | Load a database on demand (for databases not loaded at startup)
loadDatabase :: DatabaseManager -> Text -> IO (Either Text LoadedDatabase)
loadDatabase manager dbName = do
    -- Check if already loaded
    loadedDbs <- readTVarIO (dmLoadedDbs manager)
    case M.lookup dbName loadedDbs of
        Just loaded -> return $ Right loaded
        Nothing -> do
            -- Check if it's configured
            availableDbs <- readTVarIO (dmAvailableDbs manager)
            case M.lookup dbName availableDbs of
                Nothing -> return $ Left $ "Database not found: " <> dbName
                Just dbConfig -> do
                    reportProgress Info $ "Loading database: " <> T.unpack (dcDisplayName dbConfig)
                    result <- loadDatabaseFromConfig dbConfig (dmSynonymDB manager) (dmNoCache manager)
                    case result of
                        Left err -> return $ Left err
                        Right loaded -> do
                            atomically $ modifyTVar' (dmLoadedDbs manager) (M.insert dbName loaded)
                            reportProgress Info $ "  ✓ Loaded: " <> T.unpack (dcDisplayName dbConfig)
                            return $ Right loaded

-- | Unload a database from memory (keeps config for reloading)
unloadDatabase :: DatabaseManager -> Text -> IO (Either Text ())
unloadDatabase manager dbName = do
    currentName <- readTVarIO (dmCurrentName manager)
    loadedDbs <- readTVarIO (dmLoadedDbs manager)

    case M.lookup dbName loadedDbs of
        Nothing -> return $ Left $ "Database not loaded: " <> dbName
        Just _ -> do
            -- If unloading the current database, clear the current pointer
            when (currentName == Just dbName) $
                atomically $ writeTVar (dmCurrentName manager) Nothing

            -- Remove from loaded databases
            atomically $ modifyTVar' (dmLoadedDbs manager) (M.delete dbName)

            -- Clear the cached KSP solver to release PETSc memory
            clearCachedKspSolver dbName

            -- Force garbage collection to release memory
            performGC

            reportProgress Info $ "Unloaded database: " <> T.unpack dbName
            return $ Right ()

-- | Add a new database config to the manager (without loading)
addDatabase :: DatabaseManager -> DatabaseConfig -> IO ()
addDatabase manager dbConfig = do
    atomically $ modifyTVar' (dmAvailableDbs manager) (M.insert (dcName dbConfig) dbConfig)
    reportProgress Info $ "Added database config: " <> T.unpack (dcDisplayName dbConfig)

-- | Remove a database from the manager
-- Fails if database is loaded or is the current database
removeDatabase :: DatabaseManager -> Text -> IO (Either Text ())
removeDatabase manager dbName = do
    currentName <- readTVarIO (dmCurrentName manager)
    loadedDbs <- readTVarIO (dmLoadedDbs manager)
    availableDbs <- readTVarIO (dmAvailableDbs manager)

    case M.lookup dbName availableDbs of
        Nothing -> return $ Left $ "Database not found: " <> dbName
        Just dbConfig -> do
            -- Check if it's an uploaded database (only uploaded can be deleted)
            if not (isUploadedPath (dcPath dbConfig))
                then return $ Left $ "Cannot delete configured database. Edit fplca.toml to remove it."
                else if M.member dbName loadedDbs
                    then return $ Left $ "Cannot delete loaded database. Close it first."
                    else if currentName == Just dbName
                        then return $ Left $ "Cannot delete current database. Activate another database first."
                        else do
                            -- Get the upload directory (uploads/<slug>/)
                            uploadsDir <- UploadedDB.getUploadsDir
                            let uploadDir = uploadsDir </> T.unpack dbName
                            pathExists <- doesDirectoryExist uploadDir
                            if pathExists
                                then do
                                    -- Delete the database directory immediately
                                    result <- try $ removeDirectoryRecursive uploadDir
                                    case result of
                                        Left (e :: SomeException) ->
                                            return $ Left $ "Failed to delete: " <> T.pack (show e)
                                        Right () -> do
                                            reportProgress Info $ "Deleted: " <> uploadDir
                                            removeFromMemory manager dbName
                                else do
                                    -- Directory already missing, just remove from memory
                                    reportProgress Info $ "Directory already missing: " <> uploadDir
                                    removeFromMemory manager dbName
  where
    try :: IO a -> IO (Either SomeException a)
    try = Control.Exception.try

-- | Helper to remove database from in-memory maps only
removeFromMemory :: DatabaseManager -> Text -> IO (Either Text ())
removeFromMemory manager dbName = do
    atomically $ modifyTVar' (dmAvailableDbs manager) (M.delete dbName)
    reportProgress Info $ "Removed database: " <> T.unpack dbName
    return $ Right ()
