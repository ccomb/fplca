{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.UploadedDatabase
    ( -- * Types
      UploadMeta(..)
    , DatabaseFormat(..)
      -- * Meta file operations
    , readUploadMeta
    , writeUploadMeta
      -- * Discovery
    , discoverUploadedDatabases
    , getUploadsDir
      -- * Data directory
    , getDataDir
    , isUploadedPath
    ) where

import Control.Exception (try, SomeException)
import Control.Monad (forM, filterM)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeFileName, splitDirectories)
import Text.Read (readMaybe)

-- Re-export DatabaseFormat from Database.Upload (single definition)
import Database.Upload (DatabaseFormat(..))

-- | Metadata for an uploaded database
data UploadMeta = UploadMeta
    { umVersion     :: !Int           -- Meta format version (for future compatibility)
    , umDisplayName :: !Text          -- Human-readable name
    , umDescription :: !(Maybe Text)  -- Optional description
    , umFormat      :: !DatabaseFormat -- Detected database format
    , umDataPath    :: !FilePath      -- Relative path to data within upload dir
    } deriving (Show, Eq, Generic)

-- | Name of the metadata file in each upload directory
metaFileName :: FilePath
metaFileName = "meta.toml"

-- | Get the base data directory (uploads, cache, etc.)
-- Uses FPLCA_DATA_DIR env var, falls back to current directory
getDataDir :: IO FilePath
getDataDir = do
    mdir <- lookupEnv "FPLCA_DATA_DIR"
    case mdir of
        Just d  -> return d
        Nothing -> return "."

-- | Get the uploads directory
getUploadsDir :: IO FilePath
getUploadsDir = do
    base <- getDataDir
    let dir = base </> "uploads"
    createDirectoryIfMissing True dir
    return dir

-- | Check if a path belongs to the uploads directory
isUploadedPath :: FilePath -> Bool
isUploadedPath path =
    "uploads" `elem` splitDirectories path

-- | Read meta.toml from an upload directory
-- Returns Nothing if file doesn't exist or can't be parsed
readUploadMeta :: FilePath -> IO (Maybe UploadMeta)
readUploadMeta uploadDir = do
    let metaPath = uploadDir </> metaFileName
    exists <- doesFileExist metaPath
    if not exists
        then return Nothing
        else do
            result <- try $ TIO.readFile metaPath
            case result of
                Left (_ :: SomeException) -> return Nothing
                Right content -> return $ parseMetaToml content

-- | Write meta.toml to an upload directory
writeUploadMeta :: FilePath -> UploadMeta -> IO ()
writeUploadMeta uploadDir meta = do
    let metaPath = uploadDir </> metaFileName
    TIO.writeFile metaPath (formatMetaToml meta)

-- | Parse meta.toml content
-- Simple key=value parser (not a full TOML parser)
parseMetaToml :: Text -> Maybe UploadMeta
parseMetaToml content = do
    let lines' = map T.strip $ T.lines content
        kvPairs = [(T.strip k, v) | line <- lines'
                         , not (T.null line)
                         , not (T.isPrefixOf "#" line)
                         , let (k, rest) = T.breakOn "=" line
                         , not (T.null rest)
                         , let v = T.strip $ T.drop 1 rest
                         ]
        getValue key = lookup key kvPairs
        unquote t = T.dropAround (== '"') t

    version <- getValue "version" >>= readMaybe . T.unpack
    displayName <- unquote <$> getValue "displayName"
    let description = unquote <$> getValue "description"
    format <- getValue "format" >>= parseFormat . unquote
    dataPath <- T.unpack . unquote <$> getValue "dataPath"

    return UploadMeta
        { umVersion = version
        , umDisplayName = displayName
        , umDescription = description
        , umFormat = format
        , umDataPath = dataPath
        }

-- | Parse format string to DatabaseFormat
parseFormat :: Text -> Maybe DatabaseFormat
parseFormat "ecospold2" = Just EcoSpold2
parseFormat "ecospold1" = Just EcoSpold1
parseFormat "simapro" = Just SimaProCSV
parseFormat _ = Just UnknownFormat

-- | Format meta.toml content
formatMetaToml :: UploadMeta -> Text
formatMetaToml UploadMeta{..} = T.unlines $
    [ "version = " <> T.pack (show umVersion)
    , "displayName = " <> quote umDisplayName
    ] ++
    maybe [] (\d -> ["description = " <> quote d]) umDescription ++
    [ "format = " <> quote (formatToText umFormat)
    , "dataPath = " <> quote (T.pack umDataPath)
    ]
  where
    quote t = "\"" <> escapeToml t <> "\""

    escapeToml = T.concatMap escape
      where
        escape '"' = "\\\""
        escape '\\' = "\\\\"
        escape '\n' = "\\n"
        escape c = T.singleton c

    formatToText EcoSpold2 = "ecospold2"
    formatToText EcoSpold1 = "ecospold1"
    formatToText SimaProCSV = "simapro"
    formatToText UnknownFormat = "unknown"

-- | Discover all uploaded databases by scanning the uploads directory
-- Returns list of (slug, uploadDir, meta) for each valid upload
discoverUploadedDatabases :: IO [(Text, FilePath, UploadMeta)]
discoverUploadedDatabases = do
    uploadsDir <- getUploadsDir
    exists <- doesDirectoryExist uploadsDir
    if not exists
        then return []
        else do
            entries <- listDirectory uploadsDir
            let fullPaths = [(T.pack entry, uploadsDir </> entry) | entry <- entries]

            -- Filter to directories only
            dirsOnly <- filterM (doesDirectoryExist . snd) fullPaths

            -- Try to read meta.toml from each
            results <- forM dirsOnly $ \(slug, dirPath) -> do
                maybeMeta <- readUploadMeta dirPath
                return $ case maybeMeta of
                    Just meta -> Just (slug, dirPath, meta)
                    Nothing -> Nothing

            return [r | Just r <- results]
