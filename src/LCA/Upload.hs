{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LCA.Upload
    ( -- * Types
      UploadData(..)
    , UploadResult(..)
    , DatabaseFormat(..)
    , ArchiveFormat(..)
    , ProgressEvent(..)
      -- * Upload handling
    , handleUpload
    , detectDatabaseFormat
    , detectArchiveFormat
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
import Data.Word (Word8)
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, listDirectory, doesDirectoryExist, doesFileExist, copyFile)
import System.FilePath ((</>), takeExtension, takeFileName)
import System.IO.Temp (withSystemTempFile)
import System.IO (hClose)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

-- | Detected database format
data DatabaseFormat
    = SimaProCSV     -- SimaPro CSV export
    | EcoSpold1      -- EcoSpold v1 XML format
    | EcoSpold2      -- EcoSpold v2 XML format
    | UnknownFormat  -- Could not detect format
    deriving (Show, Eq, Generic)

-- | Supported archive formats
data ArchiveFormat
    = SevenZ         -- 7-Zip archive (.7z)
    | Zip            -- ZIP archive (.zip)
    | TarGz          -- Gzipped tarball (.tar.gz, .tgz)
    | TarXz          -- XZ-compressed tarball (.tar.xz)
    | PlainCSV       -- Plain CSV file (no archive)
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

-- | Get the uploads directory (relative to config file or cwd)
getUploadsDir :: IO FilePath
getUploadsDir = do
    let dir = "uploads"
    createDirectoryIfMissing True dir
    return dir

-- | Handle a database upload
-- Extracts archive to uploads/{slug}/ and detects format
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

                    -- Save uploaded file to temp and extract using system tools
                    reportProgress $ ProgressEvent "extracting" 10 "Extracting archive..."

                    result <- try $ withSystemTempFile "upload" $ \tempPath tempHandle -> do
                        -- Write archive data to temp file
                        BL.hPut tempHandle udZipData
                        hClose tempHandle

                        -- Detect archive format from file content (magic bytes)
                        let archiveFormat = detectArchiveFormatFromContent udZipData

                        -- Extract using appropriate system tool
                        extractArchive archiveFormat tempPath targetDir

                    case result of
                        Left (e :: SomeException) -> do
                            return $ Left $ "Failed to extract archive: " <> T.pack (show e)
                        Right (Left err) -> do
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
                                , urPath = dataDir  -- Use the actual data directory
                                , urFormat = format
                                , urFileCount = fileCount
                                }

-- | Detect archive format from filename (fallback)
detectArchiveFormat :: Text -> ArchiveFormat
detectArchiveFormat filename =
    let lower = T.toLower filename
    in if ".7z" `T.isSuffixOf` lower then SevenZ
       else if ".zip" `T.isSuffixOf` lower then Zip
       else if ".tar.gz" `T.isSuffixOf` lower || ".tgz" `T.isSuffixOf` lower then TarGz
       else if ".tar.xz" `T.isSuffixOf` lower then TarXz
       else if ".csv" `T.isSuffixOf` lower then PlainCSV
       else Zip  -- Default to ZIP for backwards compatibility

-- | Magic bytes for archive format detection
sevenZipMagic :: [Word8]
sevenZipMagic = [0x37, 0x7A, 0xBC, 0xAF, 0x27, 0x1C]  -- 7z

zipMagic :: [Word8]
zipMagic = [0x50, 0x4B]  -- PK

gzipMagic :: [Word8]
gzipMagic = [0x1F, 0x8B]

xzMagic :: [Word8]
xzMagic = [0xFD, 0x37, 0x7A, 0x58, 0x5A, 0x00]

-- | Check if content starts with given magic bytes
matchesMagic :: [Word8] -> BL.ByteString -> Bool
matchesMagic magic content =
    BL.unpack (BL.take (fromIntegral $ length magic) content) == magic

-- | Check if first byte is printable ASCII (for plain text detection)
isPrintableAscii :: BL.ByteString -> Bool
isPrintableAscii content
    | BL.null content = False
    | otherwise = let b = BL.head content in b == 0x7B || (b >= 0x20 && b < 0x7F)

-- | Detect archive format from file content using magic bytes
-- This is more reliable than filename detection
detectArchiveFormatFromContent :: BL.ByteString -> ArchiveFormat
detectArchiveFormatFromContent content
    | matchesMagic sevenZipMagic content = SevenZ
    | matchesMagic zipMagic content      = Zip
    | matchesMagic gzipMagic content     = TarGz
    | matchesMagic xzMagic content       = TarXz
    | isPrintableAscii content           = PlainCSV
    | otherwise                          = Zip  -- Fallback

-- | Extract archive using system tools
extractArchive :: ArchiveFormat -> FilePath -> FilePath -> IO (Either Text ())
extractArchive format archivePath targetDir = case format of
    SevenZ -> runExtractor "7z" ["x", archivePath, "-o" <> targetDir, "-y"]
    Zip    -> runExtractor "unzip" ["-o", archivePath, "-d", targetDir]
    TarGz  -> runExtractor "tar" ["xzf", archivePath, "-C", targetDir]
    TarXz  -> runExtractor "tar" ["xJf", archivePath, "-C", targetDir]
    PlainCSV -> do
        -- For plain CSV, just copy the file
        let csvName = takeFileName archivePath
            destPath = targetDir </> (if ".csv" `isSuffixOf` map toLower csvName
                                       then csvName
                                       else csvName <> ".csv")
        copyFile archivePath destPath
        return $ Right ()

-- | Run extraction command and return result
runExtractor :: FilePath -> [String] -> IO (Either Text ())
runExtractor cmd args = do
    result <- try $ readProcessWithExitCode cmd args ""
    case result of
        Left (e :: SomeException) ->
            return $ Left $ "Failed to run " <> T.pack cmd <> ": " <> T.pack (show e)
        Right (ExitSuccess, _, _) ->
            return $ Right ()
        Right (ExitFailure code, _, stderr) ->
            return $ Left $ T.pack cmd <> " failed with code " <> T.pack (show code)
                         <> ": " <> T.pack stderr

-- | Find the actual data directory
-- Archives often contain multiple folders (e.g., "datasets", "MasterData") - we need to find where the actual data files are
-- Prioritizes .spold files (EcoSpold2) over .csv files to avoid false positives from lookup files
findDataDirectory :: FilePath -> IO FilePath
findDataDirectory dir = do
    -- List all files in the directory
    files <- listDirectory dir
    let fullPaths = map (dir </>) files

    -- First, check for a "datasets" directory (common in ecoinvent) - prioritize this
    let datasetsDir = dir </> "datasets"
    hasDatasetsDir <- doesDirectoryExist datasetsDir
    if hasDatasetsDir
        then do
            hasSpold <- hasSpoldFilesIn datasetsDir
            if hasSpold
                then return datasetsDir
                else findDataDirectory datasetsDir
        else do
            -- Check if there are .spold files directly in this directory (prioritize over CSV)
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
hasSpoldFilesIn dir = do
    files <- listDirectory dir
    let extensions = map (map toLower . takeExtension) files
    return $ any (== ".spold") extensions

-- | Check if a directory contains any data files directly (not recursively)
anyDataFilesIn :: FilePath -> IO Bool
anyDataFilesIn dir = do
    files <- listDirectory dir
    let extensions = map (map toLower . takeExtension) files
    return $ any isDataExtension extensions
  where
    isDataExtension ext = ext `elem` [".spold", ".xml", ".csv"]

-- | Count data files based on format
countDataFiles :: FilePath -> DatabaseFormat -> IO Int
countDataFiles dir format = do
    files <- listDirectoryRecursive dir
    return $ length $ filter (isDataFile format) files
  where
    isDataFile SimaProCSV f = ".csv" `isSuffixOf` map toLower f
    isDataFile EcoSpold1 f = ".xml" `isSuffixOf` map toLower f
    isDataFile EcoSpold2 f = ".spold" `isSuffixOf` map toLower f
    isDataFile UnknownFormat _ = True

-- | Recursively list all files in a directory
listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive dir = do
    entries <- listDirectory dir
    results <- forM entries $ \entry -> do
        let path = dir </> entry
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
            -- Single file: detect from extension and content
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
                files <- listDirectoryRecursive path
                let extensions = map (map toLower . takeExtension) files

                -- Check for different formats
                let hasSpold = any (== ".spold") extensions
                    hasXml = any (== ".xml") extensions
                    hasCsv = any (== ".csv") extensions

                -- EcoSpold2 uses .spold extension
                if hasSpold
                    then return EcoSpold2
                    else if hasXml
                        then do
                            -- Check if it's EcoSpold1 by looking at file content
                            isEcoSpold1 <- checkForEcoSpold1 files
                            return $ if isEcoSpold1 then EcoSpold1 else UnknownFormat
                        else if hasCsv
                            then do
                                -- Check if it's SimaPro CSV
                                isSimaPro <- checkForSimaProCSV files
                                return $ if isSimaPro then SimaProCSV else UnknownFormat
                            else return UnknownFormat
            else return UnknownFormat  -- Path doesn't exist

-- | Check if XML files are EcoSpold1 format
checkForEcoSpold1 :: [FilePath] -> IO Bool
checkForEcoSpold1 files = do
    let xmlFiles = filter (\f -> ".xml" `isSuffixOf` map toLower f) files
    case xmlFiles of
        [] -> return False
        (f:_) -> do
            result <- try $ TIO.readFile f
            case result of
                Left (_ :: SomeException) -> return False
                Right content ->
                    -- EcoSpold1 has <ecoSpold xmlns="http://www.EcoInvent.org/EcoSpold01"
                    return $ T.isInfixOf "EcoSpold01" content || T.isInfixOf "ecoSpold" content

-- | Check if CSV files are SimaPro format
-- Uses ByteString to avoid UTF-8 encoding issues (SimaPro files often use Latin1)
checkForSimaProCSV :: [FilePath] -> IO Bool
checkForSimaProCSV files = do
    let csvFiles = filter (\f -> ".csv" `isSuffixOf` map toLower f) files
    case csvFiles of
        [] -> return False
        (f:_) -> do
            -- Read only first 100 bytes as ByteString (avoids encoding issues)
            result <- try $ BS.readFile f
            case result of
                Left (_ :: SomeException) -> return False
                Right content ->
                    -- Check for SimaPro signature in first 100 bytes
                    let header = BS.take 100 content
                    in return $ "{SimaPro" `BS.isInfixOf` header

-- | Convert name to URL-safe slug
slugify :: Text -> Text
slugify = T.intercalate "-" . filter (not . T.null) . T.split (not . isValidSlugChar) . T.toLower
  where
    isValidSlugChar c = isAlphaNum c || c == '-' || c == '_'
