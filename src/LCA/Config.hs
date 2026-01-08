{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LCA.Config
    ( -- * Types
      Config(..)
    , ServerConfig(..)
    , DatabaseConfig(..)
    , MethodConfig(..)
    , UnitsConfig(..)
    , UnitAliasConfig(..)
      -- * Loading
    , loadConfig
    , loadConfigFile
      -- * Default values
    , defaultServerConfig
    , defaultConfig
      -- * Utilities
    , getDefaultDatabase
    , getLoadableDatabases
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import GHC.Generics (Generic)
import qualified TOML
import TOML (DecodeTOML(..), Decoder, getField, getFieldOpt, getFieldOptWith, getArrayOf, decodeFile)
import Control.Monad (when)
import System.Directory (doesFileExist)
import LCA.Upload (DatabaseFormat(..))

-- | Main configuration type
data Config = Config
    { cfgServer    :: !ServerConfig
    , cfgDatabases :: ![DatabaseConfig]
    , cfgMethods   :: ![MethodConfig]
    , cfgUnits     :: !(Maybe UnitsConfig)  -- Optional custom unit definitions
    } deriving (Show, Eq, Generic)

-- | Server configuration
data ServerConfig = ServerConfig
    { scPort     :: !Int
    , scHost     :: !Text
    , scPassword :: !(Maybe Text)  -- Optional password for HTTP Basic Auth
    } deriving (Show, Eq, Generic)

-- | Database configuration
data DatabaseConfig = DatabaseConfig
    { dcName        :: !Text           -- Internal identifier (URL-safe slug)
    , dcDisplayName :: !Text           -- Human-readable name for UI
    , dcPath        :: !FilePath
    , dcDescription :: !(Maybe Text)
    , dcLoad        :: !Bool           -- Load at startup (renamed from dcActive)
    , dcDefault     :: !Bool
    , dcActivityAliases :: !(Map Text Text)  -- Alias name → actual activity name
    , dcFormat      :: !(Maybe DatabaseFormat)  -- Detected format (EcoSpold2, EcoSpold1, SimaProCSV)
    } deriving (Show, Eq, Generic)

-- | Method configuration
data MethodConfig = MethodConfig
    { mcName   :: !Text
    , mcPath   :: !FilePath
    , mcActive :: !Bool
    } deriving (Show, Eq, Generic)

-- | Unit configuration for dimensional analysis
-- Allows defining custom unit aliases with dimension expressions
data UnitsConfig = UnitsConfig
    { ucDimensions :: ![Text]                      -- Base dimension names in order
    , ucAliases    :: !(Map Text UnitAliasConfig)  -- Unit name → {dim, factor}
    } deriving (Show, Eq, Generic)

-- | Single unit alias definition
data UnitAliasConfig = UnitAliasConfig
    { uacDim    :: !Text    -- Dimension expression like "mass*length" or "length/time"
    , uacFactor :: !Double  -- Conversion factor to SI base units
    } deriving (Show, Eq, Generic)

-- | Default server configuration
defaultServerConfig :: ServerConfig
defaultServerConfig = ServerConfig
    { scPort = 8081
    , scHost = "127.0.0.1"
    , scPassword = Nothing
    }

-- | Default config (empty databases)
defaultConfig :: Config
defaultConfig = Config
    { cfgServer = defaultServerConfig
    , cfgDatabases = []
    , cfgMethods = []
    , cfgUnits = Nothing
    }

-- TOML Decoders

instance DecodeTOML Config where
    tomlDecoder = do
        cfgServer <- getFieldOptWith tomlDecoder "server" >>= \case
            Just s  -> pure s
            Nothing -> pure defaultServerConfig
        cfgDatabases <- getFieldOptWith (getArrayOf tomlDecoder) "databases" >>= \case
            Just dbs -> pure dbs
            Nothing  -> pure []
        cfgMethods <- getFieldOptWith (getArrayOf tomlDecoder) "methods" >>= \case
            Just ms -> pure ms
            Nothing -> pure []
        cfgUnits <- getFieldOptWith tomlDecoder "units"
        pure Config{..}

instance DecodeTOML ServerConfig where
    tomlDecoder = do
        scPort <- getFieldOpt "port" >>= \case
            Just p  -> pure p
            Nothing -> pure 8081
        scHost <- getFieldOpt "host" >>= \case
            Just h  -> pure h
            Nothing -> pure "127.0.0.1"
        scPassword <- getFieldOpt "password"
        pure ServerConfig{..}

instance DecodeTOML DatabaseConfig where
    tomlDecoder = do
        dcName <- getField "name"
        dcDisplayName <- getFieldOpt "displayName" >>= \case
            Just dn -> pure dn
            Nothing -> pure dcName  -- Fall back to name if no displayName
        dcPath <- getField "path"
        dcDescription <- getFieldOpt "description"
        dcLoad <- getFieldOpt "load" >>= \case
            Just l  -> pure l
            Nothing -> pure False  -- Default to NOT loading at startup
        dcDefault <- getFieldOpt "default" >>= \case
            Just d  -> pure d
            Nothing -> pure False
        dcActivityAliases <- getFieldOpt "activityAliases" >>= \case
            Just m  -> pure m
            Nothing -> pure M.empty
        let dcFormat = Nothing  -- Format is detected at runtime, not stored in config
        pure DatabaseConfig{..}

instance DecodeTOML MethodConfig where
    tomlDecoder = do
        mcName <- getField "name"
        mcPath <- getField "path"
        mcActive <- getFieldOpt "active" >>= \case
            Just a  -> pure a
            Nothing -> pure True  -- Default to active
        pure MethodConfig{..}

instance DecodeTOML UnitsConfig where
    tomlDecoder = do
        ucDimensions <- getFieldOpt "dimensions" >>= \case
            Just dims -> pure dims
            Nothing -> pure ["mass", "length", "time", "energy", "area", "volume", "count", "currency"]
        ucAliases <- getFieldOpt "aliases" >>= \case
            Just m  -> pure m
            Nothing -> pure M.empty
        pure UnitsConfig{..}

instance DecodeTOML UnitAliasConfig where
    tomlDecoder = do
        uacDim <- getField "dim"
        uacFactor <- getField "factor"
        pure UnitAliasConfig{..}

-- | Load configuration from a TOML file
loadConfigFile :: FilePath -> IO (Either Text Config)
loadConfigFile path = do
    exists <- doesFileExist path
    if not exists
        then pure $ Left $ "Config file not found: " <> T.pack path
        else do
            result <- decodeFile path
            case result of
                Right cfg -> pure $ Right cfg
                Left err -> pure $ Left $ "TOML parse error: " <> T.pack (show err)

-- | Load configuration, with validation
loadConfig :: FilePath -> IO (Either Text Config)
loadConfig path = do
    result <- loadConfigFile path
    case result of
        Left err -> pure $ Left err
        Right cfg -> pure $ validateConfig cfg

-- | Validate configuration
validateConfig :: Config -> Either Text Config
validateConfig cfg = do
    -- Check for duplicate database names
    let dbNames = map dcName (cfgDatabases cfg)
        duplicates = findDuplicates dbNames
    when (not $ null duplicates) $
        Left $ "Duplicate database names: " <> T.intercalate ", " duplicates

    -- Check that at most one database is marked as default
    let defaultDbs = filter dcDefault (cfgDatabases cfg)
    when (length defaultDbs > 1) $
        Left $ "Multiple databases marked as default: " <> T.intercalate ", " (map dcName defaultDbs)

    -- Note: we no longer require any databases to have load=true
    -- The user can load databases on demand via the UI

    Right cfg

-- | Find duplicates in a list
findDuplicates :: Eq a => [a] -> [a]
findDuplicates xs = go [] [] xs
  where
    go _ dups [] = dups
    go seen dups (x:rest)
        | x `elem` seen = go seen (if x `elem` dups then dups else x:dups) rest
        | otherwise = go (x:seen) dups rest

-- | Get the default database (or first loadable if none marked default)
getDefaultDatabase :: Config -> Maybe DatabaseConfig
getDefaultDatabase cfg =
    case filter dcDefault (getLoadableDatabases cfg) of
        (db:_) -> Just db
        []     -> case getLoadableDatabases cfg of
            (db:_) -> Just db
            []     -> Nothing

-- | Get all databases configured to load at startup
getLoadableDatabases :: Config -> [DatabaseConfig]
getLoadableDatabases = filter dcLoad . cfgDatabases
