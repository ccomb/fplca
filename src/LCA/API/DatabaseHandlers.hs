{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : LCA.API.DatabaseHandlers
Description : Database management API handlers

Extracted from LCA.API to reduce module size and improve organization.
Contains handlers for database listing, loading, unloading, uploading, and deletion.
-}
module LCA.API.DatabaseHandlers
    ( -- * Handlers
      getDatabases
    , loadDatabaseHandler
    , unloadDatabaseHandler
    , deleteDatabaseHandler
    , uploadDatabaseHandler
      -- * Setup Page Handlers
    , getDatabaseSetupHandler
    , addDependencyHandler
    , removeDependencyHandler
    , finalizeDatabaseHandler
      -- * Helpers
    , convertDbStatus
    , convertLoadedDbToStatus
    ) where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Servant (Handler, throwError, errBody, err400, err404)
import System.FilePath ((</>))

import qualified LCA.Config
import LCA.Config (DatabaseConfig(..))
import LCA.DatabaseManager
    ( DatabaseManager
    , DatabaseStatus(..)
    , DatabaseSetupInfo(..)
    , DepLoadResult(..)
    , LoadedDatabase(..)
    , addDatabase
    , addDependencyToStaged
    , finalizeDatabase
    , getDatabaseSetupInfo
    , listDatabases
    , loadDatabase
    , removeDatabase
    , removeDependencyFromStaged
    , unloadDatabase
    )
import LCA.Types.API
    ( ActivateResponse(..)
    , LoadDatabaseResponse(..)
    , DatabaseListResponse(..)
    , DatabaseStatusAPI(..)
    , UploadRequest(..)
    , UploadResponse(..)
    )
import LCA.Upload
    ( DatabaseFormat(..)
    , UploadData(..)
    , UploadResult(..)
    , handleUpload
    )
import qualified LCA.UploadedDatabase as UploadedDB

-- | List all databases
getDatabases :: DatabaseManager -> Handler DatabaseListResponse
getDatabases dbManager = do
    dbStatuses <- liftIO $ listDatabases dbManager
    let statusList = map convertDbStatus dbStatuses
    return $ DatabaseListResponse statusList

-- | Load a database on demand
loadDatabaseHandler :: DatabaseManager -> Text -> Handler LoadDatabaseResponse
loadDatabaseHandler dbManager dbName = do
    eitherResult <- liftIO $ try $ loadDatabase dbManager dbName
    case eitherResult of
        Left (ex :: SomeException) ->
            return $ LoadFailed ("Server exception: " <> T.pack (show ex))
        Right (Left err) -> return $ LoadFailed err
        Right (Right (loadedDb, depResults)) -> do
            let config = ldConfig loadedDb
                status = makeStatusFromConfig config
            return $ LoadSucceeded status depResults

-- | Unload a database from memory
unloadDatabaseHandler :: DatabaseManager -> Text -> Handler ActivateResponse
unloadDatabaseHandler dbManager dbName = do
    result <- liftIO $ unloadDatabase dbManager dbName
    case result of
        Left err -> return $ ActivateResponse False err Nothing
        Right () -> return $ ActivateResponse True ("Unloaded database: " <> dbName) Nothing

-- | Delete an uploaded database (move to trash)
deleteDatabaseHandler :: DatabaseManager -> Text -> Handler ActivateResponse
deleteDatabaseHandler dbManager dbName = do
    result <- liftIO $ removeDatabase dbManager dbName
    case result of
        Left err -> return $ ActivateResponse False err Nothing
        Right () -> return $ ActivateResponse True ("Deleted database: " <> dbName) Nothing

-- | Upload a new database
uploadDatabaseHandler :: DatabaseManager -> UploadRequest -> Handler UploadResponse
uploadDatabaseHandler dbManager req = do
    -- Decode base64 ZIP data
    let zipDataResult = B64.decode $ T.encodeUtf8 $ urFileData req
    case zipDataResult of
        Left err -> return $ UploadResponse False ("Invalid base64 data: " <> T.pack err) Nothing Nothing
        Right zipBytes -> do
            let uploadData = UploadData
                    { udName = urName req
                    , udDescription = urDescription req
                    , udZipData = BSL.fromStrict zipBytes
                    }
            -- Handle the upload (extract, detect format)
            result <- liftIO $ handleUpload uploadData (\_ -> return ())

            case result of
                Left err ->
                    return $ UploadResponse False err Nothing Nothing
                Right uploadResult -> do
                    -- Get upload directory path
                    uploadsDir <- liftIO $ UploadedDB.getUploadsDir
                    let uploadDir = uploadsDir </> T.unpack (urSlug uploadResult)

                    -- Create meta.toml for self-describing upload
                    let meta = UploadedDB.UploadMeta
                            { UploadedDB.umVersion = 1
                            , UploadedDB.umDisplayName = urName req
                            , UploadedDB.umDescription = urDescription req
                            , UploadedDB.umFormat = urFormat uploadResult  -- Types are now unified
                            , UploadedDB.umDataPath = makeRelative uploadDir (urPath uploadResult)
                            }
                    liftIO $ UploadedDB.writeUploadMeta uploadDir meta

                    -- Create database config for in-memory manager
                    let dbConfig = DatabaseConfig
                            { dcName = urSlug uploadResult
                            , dcDisplayName = urName req
                            , dcPath = urPath uploadResult
                            , dcDescription = urDescription req
                            , dcLoad = False  -- Don't auto-load
                            , dcDefault = False
                            , dcLocationAliases = M.empty
                            , dcFormat = Just (urFormat uploadResult)
                            , dcIsUploaded = True  -- Freshly uploaded database
                            }

                    -- Add to manager
                    liftIO $ addDatabase dbManager dbConfig

                    return $ UploadResponse True
                        "Database uploaded successfully"
                        (Just $ urSlug uploadResult)
                        (Just $ formatToText $ urFormat uploadResult)

-- | Convert DatabaseManager.DatabaseStatus to API.DatabaseStatusAPI
convertDbStatus :: DatabaseStatus -> DatabaseStatusAPI
convertDbStatus ds = DatabaseStatusAPI
    { dsaName = dsName ds
    , dsaDisplayName = dsDisplayName ds
    , dsaDescription = dsDescription ds
    , dsaLoadAtStartup = dsLoadAtStartup ds
    , dsaLoaded = dsLoaded ds
    , dsaCached = dsCached ds
    , dsaIsUploaded = dsIsUploaded ds
    , dsaPath = dsPath ds
    , dsaFormat = formatToDisplayText <$> dsFormat ds
    }
  where
    formatToDisplayText EcoSpold2 = "EcoSpold 2"
    formatToDisplayText EcoSpold1 = "EcoSpold 1"
    formatToDisplayText SimaProCSV = "SimaPro CSV"
    formatToDisplayText UnknownFormat = ""

-- | Convert LoadedDatabase to DatabaseStatusAPI
convertLoadedDbToStatus :: LoadedDatabase -> DatabaseStatusAPI
convertLoadedDbToStatus loaded =
    let config = ldConfig loaded
    in makeStatusFromConfig config

-- | Create DatabaseStatusAPI from config (loaded database)
makeStatusFromConfig :: DatabaseConfig -> DatabaseStatusAPI
makeStatusFromConfig config = DatabaseStatusAPI
    { dsaName = LCA.Config.dcName config
    , dsaDisplayName = LCA.Config.dcDisplayName config
    , dsaDescription = LCA.Config.dcDescription config
    , dsaLoadAtStartup = LCA.Config.dcLoad config
    , dsaLoaded = True
    , dsaCached = True
    , dsaIsUploaded = LCA.Config.dcIsUploaded config
    , dsaPath = T.pack (LCA.Config.dcPath config)
    , dsaFormat = formatToDisplayText <$> LCA.Config.dcFormat config
    }
  where
    formatToDisplayText EcoSpold2 = "EcoSpold 2"
    formatToDisplayText EcoSpold1 = "EcoSpold 1"
    formatToDisplayText SimaProCSV = "SimaPro CSV"
    formatToDisplayText UnknownFormat = ""

-- uploadFormatToMeta removed - types are now unified (UploadedDB re-exports from Upload)

-- | Make a path relative to a base directory
makeRelative :: FilePath -> FilePath -> FilePath
makeRelative base path
    | base `isPrefixOf` path = drop (length base + 1) path  -- +1 for separator
    | otherwise = path

-- | Convert DatabaseFormat to Text
formatToText :: DatabaseFormat -> Text
formatToText SimaProCSV = "simapro-csv"
formatToText EcoSpold1 = "ecospold1"
formatToText EcoSpold2 = "ecospold2"
formatToText UnknownFormat = "unknown"

--------------------------------------------------------------------------------
-- Setup Page Handlers
--------------------------------------------------------------------------------

-- | Get database setup info
-- Returns completeness, missing suppliers, and dependency suggestions
getDatabaseSetupHandler :: DatabaseManager -> Text -> Handler DatabaseSetupInfo
getDatabaseSetupHandler dbManager dbName = do
    result <- liftIO $ getDatabaseSetupInfo dbManager dbName
    case result of
        Left err -> throwError $ err404 { errBody = BSL.fromStrict $ T.encodeUtf8 err }
        Right setupInfo -> return setupInfo

-- | Add a dependency to a staged database
-- Runs cross-DB linking and returns updated setup info
addDependencyHandler :: DatabaseManager -> Text -> Text -> Handler DatabaseSetupInfo
addDependencyHandler dbManager dbName depName = do
    result <- liftIO $ addDependencyToStaged dbManager dbName depName
    case result of
        Left err -> throwError $ err400 { errBody = BSL.fromStrict $ T.encodeUtf8 err }
        Right setupInfo -> return setupInfo

-- | Remove a dependency from a staged database
-- Re-runs cross-DB linking and returns updated setup info
removeDependencyHandler :: DatabaseManager -> Text -> Text -> Handler DatabaseSetupInfo
removeDependencyHandler dbManager dbName depName = do
    result <- liftIO $ removeDependencyFromStaged dbManager dbName depName
    case result of
        Left err -> throwError $ err400 { errBody = BSL.fromStrict $ T.encodeUtf8 err }
        Right setupInfo -> return setupInfo

-- | Finalize a staged database
-- Builds matrices and makes it ready for queries
finalizeDatabaseHandler :: DatabaseManager -> Text -> Handler ActivateResponse
finalizeDatabaseHandler dbManager dbName = do
    eitherResult <- liftIO $ try $ finalizeDatabase dbManager dbName
    case eitherResult of
        Left (ex :: SomeException) ->
            return $ ActivateResponse False ("Server exception: " <> T.pack (show ex)) Nothing
        Right (Left err) -> return $ ActivateResponse False err Nothing
        Right (Right loaded) -> do
            let config = ldConfig loaded
                status = makeStatusFromConfig config
            return $ ActivateResponse True ("Finalized database: " <> LCA.Config.dcDisplayName config) (Just status)
