{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : ACV.Progress
Description : Unified progress reporting system for the LCA engine

This module provides a unified system for reporting progress during LCA calculations.
It replaces the scattered trace statements throughout the codebase with clean,
informative progress output that includes timing information.

Key features:
- Consistent formatting for all progress messages
- Automatic timing measurement for operations
- Clean output suitable for production use
- Stderr-based output that doesn't interfere with JSON responses
-}

module ACV.Progress (
    -- * Progress reporting functions
    reportProgress,
    reportProgressWithTiming,
    reportError,
    reportCacheOperation,
    reportMatrixOperation,
    reportSolverOperation,

    -- * Timing utilities
    withProgressTiming,
    formatDuration,

    -- * Performance monitoring
    reportMemoryUsage,
    formatBytes,
    reportCacheInfo,

    -- * Types
    ProgressLevel(..)
) where

import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import System.IO (stderr, hPutStrLn, hFlush)
import Text.Printf (printf)
import Control.Exception (bracket_, try, SomeException)
import GHC.Stats (getRTSStats, RTSStats(..))
import System.Directory (getFileSize, doesFileExist)
import Control.Monad (when)

-- | Progress reporting levels
data ProgressLevel
    = Info      -- ^ General information
    | Cache     -- ^ Cache operations
    | Matrix    -- ^ Matrix construction operations
    | Solver    -- ^ PETSc solver operations
    | Error     -- ^ Error messages
    deriving (Eq, Show)

-- | Report progress with consistent formatting
reportProgress :: ProgressLevel -> String -> IO ()
reportProgress level message = do
    let prefix = case level of
            Info   -> ""
            Cache  -> "[CACHE] "
            Matrix -> "[MATRIX] "
            Solver -> "[SOLVER] "
            Error  -> "[ERROR] "
    hPutStrLn stderr $ prefix ++ message
    hFlush stderr

-- | Report progress with timing information
reportProgressWithTiming :: ProgressLevel -> String -> Double -> IO ()
reportProgressWithTiming level operation duration = do
    let message = operation ++ " (" ++ formatDuration duration ++ ")"
    reportProgress level message

-- | Report error messages
reportError :: String -> IO ()
reportError = reportProgress Error

-- | Report cache-related operations
reportCacheOperation :: String -> IO ()
reportCacheOperation = reportProgress Cache

-- | Report matrix construction operations
reportMatrixOperation :: String -> IO ()
reportMatrixOperation = reportProgress Matrix

-- | Report PETSc solver operations
reportSolverOperation :: String -> IO ()
reportSolverOperation = reportProgress Solver

-- | Execute an operation with automatic timing and progress reporting
withProgressTiming :: ProgressLevel -> String -> IO a -> IO a
withProgressTiming level operationName action = do
    reportProgress level $ operationName ++ "..."
    startTime <- getCurrentTime
    result <- action
    endTime <- getCurrentTime
    let duration = realToFrac $ diffUTCTime endTime startTime
    reportProgressWithTiming level operationName duration
    return result

-- | Format duration in seconds with appropriate precision
formatDuration :: Double -> String
formatDuration duration
    | duration < 0.001 = printf "%.2fms" (duration * 1000)
    | duration < 1.0   = printf "%.0fms" (duration * 1000)
    | duration < 60.0  = printf "%.2fs" duration
    | otherwise        = printf "%.1fmin" (duration / 60)

-- | Report current memory usage (if RTS stats are enabled)
reportMemoryUsage :: String -> IO ()
reportMemoryUsage operation = do
    result <- try getRTSStats :: IO (Either SomeException RTSStats)
    case result of
        Right stats -> do
            let memoryBytes = fromIntegral (allocated_bytes stats) :: Double
            reportProgress Info $ operation ++ " - Memory allocated: " ++ formatBytes memoryBytes
        Left _ ->
            reportProgress Info $ operation ++ " - Memory stats disabled (use +RTS -T to enable)"

-- | Format bytes in human-readable format
formatBytes :: Double -> String
formatBytes bytes
    | bytes < 1024 = printf "%.0f B" bytes
    | bytes < 1024 * 1024 = printf "%.1f KB" (bytes / 1024)
    | bytes < 1024 * 1024 * 1024 = printf "%.1f MB" (bytes / (1024 * 1024))
    | otherwise = printf "%.2f GB" (bytes / (1024 * 1024 * 1024))

-- | Report cache file information
reportCacheInfo :: FilePath -> IO ()
reportCacheInfo cacheFile = do
    exists <- doesFileExist cacheFile
    when exists $ do
        size <- getFileSize cacheFile
        let sizeMB = fromIntegral size / (1024 * 1024) :: Double
        reportCacheOperation $ "Cache file: " ++ cacheFile ++ " (" ++ formatBytes (fromIntegral size) ++ ")"
