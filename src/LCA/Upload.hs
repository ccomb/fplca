{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module LCA.Upload
    ( -- * Types
      UploadData(..)
    , UploadResult(..)
    , DatabaseFormat(..)
    , ProgressEvent(..)
      -- * Upload handling
    , handleUpload
    , detectDatabaseFormat
    , slugify
    , getUploadsDir
    ) where

import Control.Exception (try, SomeException)
import Control.Monad (forM)
import Data.Char (isAlphaNum, toLower)
import Data.List (isSuffixOf)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, listDirectory, doesDirectoryExist, doesFileExist, removeDirectoryRecursive)
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeExtension, takeDirectory)
import qualified Codec.Archive.Zip as Zip

-- | Detected database format
data DatabaseFormat
    = SimaProCSV     -- SimaPro CSV export
    | EcoSpold1      -- EcoSpold v1 XML format
    | EcoSpold2      -- EcoSpold v2 XML format
    | UnknownFormat  -- Could not detect format
    deriving (Show, Eq, Generic)

-- | Progress event for upload/loading operations
data ProgressEvent = ProgressEvent
    { pePhase   :: !Text    -- Current phase (extracting, parsing, etc.)
    , pePercent :: !Int     -- Progress percentage (0-100)
    , peMessage :: !Text    -- Human-readable message
    } deriving (Show, Eq, Generic)

-- | Data for database upload
data UploadData = UploadData
    { udName        :: !Text           -- User-provided name
    , udDescription :: !(Maybe Text)   -- Optional description
    , udZipData     :: !BL.ByteString  -- ZIP file content
    } deriving (Show, Generic)

-- | Result of successful upload
data UploadResult = UploadResult
    { urSlug      :: !Text       -- Generated slug (URL-safe identifier)
    , urPath      :: !FilePath   -- Path to extracted data
    , urFormat    :: !DatabaseFormat  -- Detected format
    , urFileCount :: !Int        -- Number of data files
    } deriving (Show, Eq, Generic)

-- | Get the uploads directory under the data directory
-- Uses FPLCA_DATA_DIR env var, falls back to current directory
getUploadsDir :: IO FilePath
getUploadsDir = do
    mdir <- lookupEnv "FPLCA_DATA_DIR"
    let base = case mdir of
            Just d  -> d
            Nothing -> "."
    let dir = base </> "uploads"
    createDirectoryIfMissing True dir
    return dir

-- | Handle a database upload
-- Extracts ZIP archive to uploads/{slug}/ and detects format
handleUpload :: UploadData -> (ProgressEvent -> IO ()) -> IO (Either Text UploadResult)
handleUpload UploadData{..} reportProgress = do
    let slug = slugify udName

    -- Check for valid slug
    if T.null slug
        then return $ Left "Invalid database name - cannot generate slug"
        else do
            -- Get uploads directory
            uploadsDir <- getUploadsDir
            let targetDir = uploadsDir </> T.unpack slug

            -- Check if directory already exists
            exists <- doesDirectoryExist targetDir
            if exists
                then return $ Left $ "Database already exists: " <> slug
                else do
                    -- Create target directory
                    createDirectoryIfMissing True targetDir

                    reportProgress $ ProgressEvent "extracting" 10 "Extracting archive..."

                    result <- try $ extractUpload udZipData targetDir
                    case result of
                        Left (e :: SomeException) -> do
                            _ <- try @SomeException $ removeDirectoryRecursive targetDir
                            return $ Left $ "Failed to extract archive: " <> T.pack (show e)
                        Right (Left err) -> do
                            _ <- try @SomeException $ removeDirectoryRecursive targetDir
                            return $ Left err
                        Right (Right ()) -> do
                            -- Find the actual data directory (may be nested)
                            reportProgress $ ProgressEvent "locating" 70 "Locating data files..."
                            dataDir <- findDataDirectory targetDir

                            -- Detect format
                            reportProgress $ ProgressEvent "detecting" 80 "Detecting database format..."
                            format <- detectDatabaseFormat dataDir

                            -- Count data files
                            reportProgress $ ProgressEvent "counting" 90 "Counting data files..."
                            fileCount <- countDataFiles dataDir format

                            reportProgress $ ProgressEvent "complete" 100 "Upload complete!"

                            return $ Right UploadResult
                                { urSlug = slug
                                , urPath = dataDir
                                , urFormat = format
                                , urFileCount = fileCount
                                }

-- | Extract upload data to target directory.
-- Supports ZIP archives (detected by magic bytes) and plain CSV files.
extractUpload :: BL.ByteString -> FilePath -> IO (Either Text ())
extractUpload content targetDir
    | isZipContent content = extractZip content targetDir
    | isPlainCSV content   = do
        -- Plain CSV: write directly
        BL.writeFile (targetDir </> "data.csv") content
        return $ Right ()
    | otherwise = return $ Left "Unsupported file format. Please upload a ZIP archive or CSV file."

-- | Check if content is a ZIP archive (magic bytes: PK)
isZipContent :: BL.ByteString -> Bool
isZipContent content =
    BL.unpack (BL.take 2 content) == [0x50, 0x4B]

-- | Check if first byte is printable ASCII (plain text / CSV)
isPlainCSV :: BL.ByteString -> Bool
isPlainCSV content
    | BL.null content = False
    | otherwise = let b = BL.head content in b == 0x7B || (b >= 0x20 && b < 0x7F)

-- | Extract a ZIP archive using the zip library (pure Haskell, no system tools)
extractZip :: BL.ByteString -> FilePath -> IO (Either Text ())
extractZip zipData targetDir = do
    case Zip.toArchiveOrFail zipData of
        Left err -> return $ Left $ "Invalid ZIP file: " <> T.pack err
        Right archive -> do
            mapM_ (extractEntry targetDir) (Zip.zEntries archive)
            return $ Right ()

-- | Extract a single ZIP entry to disk
extractEntry :: FilePath -> Zip.Entry -> IO ()
extractEntry targetDir entry = do
    let path = targetDir </> Zip.eRelativePath entry
    if "/" `isSuffixOf` Zip.eRelativePath entry
        then createDirectoryIfMissing True path
        else do
            createDirectoryIfMissing True (takeDirectory path)
            BL.writeFile path (Zip.fromEntry entry)

-- | Find the actual data directory
-- Archives often contain multiple folders (e.g., "datasets", "MasterData")
-- Prioritizes .spold files (EcoSpold2) over .csv files
findDataDirectory :: FilePath -> IO FilePath
findDataDirectory dir = do
    files <- listDirectory dir
    let fullPaths = map (dir </>) files

    -- First, check for a "datasets" directory (common in ecoinvent)
    let datasetsDir = dir </> "datasets"
    hasDatasetsDir <- doesDirectoryExist datasetsDir
    if hasDatasetsDir
        then do
            hasSpold <- hasSpoldFilesIn datasetsDir
            if hasSpold
                then return datasetsDir
                else findDataDirectory datasetsDir
        else do
            -- Check if there are .spold files directly in this directory
            hasSpold <- hasSpoldFilesIn dir
            if hasSpold
                then return dir
                else do
                    -- Check for other data files (XML, CSV)
                    hasDataFiles <- anyDataFilesIn dir
                    if hasDataFiles
                        then return dir
                        else do
                            -- Get all subdirectories and check them
                            subdirs <- filterM doesDirectoryExist fullPaths
                            dirsWithSpold <- filterM hasSpoldFilesIn subdirs
                            case dirsWithSpold of
                                (firstWithSpold:_) -> return firstWithSpold
                                [] -> do
                                    dirsWithData <- filterM anyDataFilesIn subdirs
                                    case dirsWithData of
                                        (firstWithData:_) -> return firstWithData
                                        [] -> case subdirs of
                                            [singleSubdir] -> findDataDirectory singleSubdir
                                            _ -> return dir
  where
    filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
    filterM _ [] = return []
    filterM p (x:xs) = do
        keep <- p x
        rest <- filterM p xs
        return $ if keep then x:rest else rest

-- | Check if a directory contains .spold files (EcoSpold2)
hasSpoldFilesIn :: FilePath -> IO Bool
hasSpoldFilesIn d = do
    fs <- listDirectory d
    let extensions = map (map toLower . takeExtension) fs
    return $ any (== ".spold") extensions

-- | Check if a directory contains any data files directly
anyDataFilesIn :: FilePath -> IO Bool
anyDataFilesIn d = do
    fs <- listDirectory d
    let extensions = map (map toLower . takeExtension) fs
    return $ any isDataExtension extensions
  where
    isDataExtension ext = ext `elem` [".spold", ".xml", ".csv"]

-- | Count data files based on format
countDataFiles :: FilePath -> DatabaseFormat -> IO Int
countDataFiles d format = do
    fs <- listDirectoryRecursive d
    return $ length $ filter (isDataFile format) fs
  where
    isDataFile SimaProCSV f = ".csv" `isSuffixOf` map toLower f
    isDataFile EcoSpold1 f = ".xml" `isSuffixOf` map toLower f
    isDataFile EcoSpold2 f = ".spold" `isSuffixOf` map toLower f
    isDataFile UnknownFormat _ = True

-- | Recursively list all files in a directory
listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive d = do
    entries <- listDirectory d
    results <- forM entries $ \entry -> do
        let path = d </> entry
        isDir <- doesDirectoryExist path
        if isDir
            then listDirectoryRecursive path
            else return [path]
    return $ concat results

-- | Detect database format from extracted files or a single file
detectDatabaseFormat :: FilePath -> IO DatabaseFormat
detectDatabaseFormat path = do
    isFile <- doesFileExist path
    isDir <- doesDirectoryExist path

    if isFile
        then do
            let ext = map toLower (takeExtension path)
            case ext of
                ".spold" -> return EcoSpold2
                ".xml" -> do
                    isEcoSpold1 <- checkForEcoSpold1 [path]
                    return $ if isEcoSpold1 then EcoSpold1 else UnknownFormat
                ".csv" -> do
                    isSimaPro <- checkForSimaProCSV [path]
                    return $ if isSimaPro then SimaProCSV else UnknownFormat
                _ -> return UnknownFormat
        else if isDir
            then do
                fs <- listDirectoryRecursive path
                let extensions = map (map toLower . takeExtension) fs
                let hasSpold = any (== ".spold") extensions
                    hasXml = any (== ".xml") extensions
                    hasCsv = any (== ".csv") extensions
                if hasSpold
                    then return EcoSpold2
                    else if hasXml
                        then do
                            isEcoSpold1 <- checkForEcoSpold1 fs
                            return $ if isEcoSpold1 then EcoSpold1 else UnknownFormat
                        else if hasCsv
                            then do
                                isSimaPro <- checkForSimaProCSV fs
                                return $ if isSimaPro then SimaProCSV else UnknownFormat
                            else return UnknownFormat
            else return UnknownFormat

-- | Check if XML files are EcoSpold1 format
checkForEcoSpold1 :: [FilePath] -> IO Bool
checkForEcoSpold1 fs = do
    let xmlFiles = filter (\f -> ".xml" `isSuffixOf` map toLower f) fs
    case xmlFiles of
        [] -> return False
        (f:_) -> do
            result <- try $ TIO.readFile f
            case result of
                Left (_ :: SomeException) -> return False
                Right c ->
                    return $ T.isInfixOf "EcoSpold01" c || T.isInfixOf "ecoSpold" c

-- | Check if CSV files are SimaPro format
-- Uses ByteString to avoid UTF-8 encoding issues (SimaPro files often use Latin1)
checkForSimaProCSV :: [FilePath] -> IO Bool
checkForSimaProCSV fs = do
    let csvFiles = filter (\f -> ".csv" `isSuffixOf` map toLower f) fs
    case csvFiles of
        [] -> return False
        (f:_) -> do
            result <- try $ BS.readFile f
            case result of
                Left (_ :: SomeException) -> return False
                Right c ->
                    let header = BS.take 100 c
                    in return $ "{SimaPro" `BS.isInfixOf` header

-- | Convert name to URL-safe slug
slugify :: Text -> Text
slugify = T.intercalate "-" . filter (not . T.null) . T.split (not . isValidSlugChar) . T.toLower
  where
    isValidSlugChar c = isAlphaNum c || c == '-' || c == '_'
