{-# LANGUAGE OverloadedStrings #-}

module ProgressSpec (spec) where

import Control.Concurrent.Async (async, wait)
import Data.List (isInfixOf)
import Progress (
    LogLine (..),
    ProgressLevel (..),
    formatBytes,
    formatDuration,
    getLogLines,
    inheritLogScope,
    reportCacheOperation,
    reportError,
    reportMatrixOperation,
    reportProgress,
    reportSolverOperation,
    withLogScope,
 )
import Test.Hspec

spec :: Spec
spec = do
    describe "formatDuration" $ do
        it "formats sub-millisecond durations with 2 decimal places" $
            formatDuration 0.0005 `shouldBe` "0.50ms"

        it "formats millisecond durations as integer ms" $
            formatDuration 0.5 `shouldBe` "500ms"

        it "formats second durations with 2 decimal places" $
            formatDuration 1.5 `shouldBe` "1.50s"

        it "formats exact 1.0s boundary as seconds" $
            formatDuration 1.0 `shouldBe` "1.00s"

        it "formats durations >= 60s as minutes" $
            formatDuration 90.0 `shouldBe` "1.5min"

        it "formats exactly 60s as 1.0min" $
            formatDuration 60.0 `shouldBe` "1.0min"

    describe "formatBytes" $ do
        it "formats bytes below 1024 as B" $
            formatBytes 512.0 `shouldBe` "512 B"

        it "formats exactly 1024 bytes as KB" $
            formatBytes 1024.0 `shouldBe` "1.0 KB"

        it "formats KB with one decimal place" $
            formatBytes 1536.0 `shouldBe` "1.5 KB"

        it "formats MB with one decimal place" $
            formatBytes (1.5 * 1024 * 1024) `shouldBe` "1.5 MB"

        it "formats GB with two decimal places" $
            formatBytes (1.5 * 1024 * 1024 * 1024) `shouldBe` "1.50 GB"

        it "formats zero bytes" $
            formatBytes 0.0 `shouldBe` "0 B"

    -- -----------------------------------------------------------------------
    -- Log buffer IO functions (read-only; global state is append-only)
    -- -----------------------------------------------------------------------
    describe "getLogLines" $ do
        it "returns a non-negative next index" $ do
            (idx, _) <- getLogLines 0
            idx `shouldSatisfy` (>= 0)

        it "returns empty list when since >= nextIndex" $ do
            (idx, _) <- getLogLines 0
            (_, lines2) <- getLogLines idx
            lines2 `shouldBe` []

    describe "reportProgress" $ do
        it "appends a line to the log buffer (Info level)" $ do
            (before, _) <- getLogLines 0
            reportProgress Info "test-info-message"
            (after, newLines) <- getLogLines before
            after `shouldSatisfy` (> before)
            concatLines newLines `shouldSatisfy` ("test-info-message" `isInfixOf`)

        it "prefixes Error level with [ERROR]" $ do
            (before, _) <- getLogLines 0
            reportError "something failed"
            (_, newLines) <- getLogLines before
            concatLines newLines `shouldSatisfy` ("[ERROR]" `isInfixOf`)

        it "prefixes Cache level with [CACHE]" $ do
            (before, _) <- getLogLines 0
            reportCacheOperation "cache hit"
            (_, newLines) <- getLogLines before
            concatLines newLines `shouldSatisfy` ("[CACHE]" `isInfixOf`)

        it "prefixes Matrix level with [MATRIX]" $ do
            (before, _) <- getLogLines 0
            reportMatrixOperation "building matrix"
            (_, newLines) <- getLogLines before
            concatLines newLines `shouldSatisfy` ("[MATRIX]" `isInfixOf`)

        it "prefixes Solver level with [SOLVER]" $ do
            (before, _) <- getLogLines 0
            reportSolverOperation "solving"
            (_, newLines) <- getLogLines before
            concatLines newLines `shouldSatisfy` ("[SOLVER]" `isInfixOf`)

    describe "log scoping" $ do
        it "tags lines emitted inside withLogScope with the database name" $ do
            let marker = "progress-spec-scoped"
            found <- emitAndCollect (withLogScope "db-a" (reportProgress Info marker)) marker
            map llScope found `shouldBe` [Just "db-a"]

        it "leaves lines emitted outside any scope untagged" $ do
            let marker = "progress-spec-unscoped"
            found <- emitAndCollect (reportProgress Info marker) marker
            map llScope found `shouldBe` [Nothing]

        it "keeps the outermost scope when scopes nest" $ do
            let marker = "progress-spec-nested"
            found <-
                emitAndCollect
                    (withLogScope "outer" (withLogScope "inner" (reportProgress Info marker)))
                    marker
            map llScope found `shouldBe` [Just "outer"]

        it "restores the unscoped state after the action" $ do
            let marker = "progress-spec-after"
            found <-
                emitAndCollect
                    ( do
                        withLogScope "db-a" (pure ())
                        reportProgress Info marker
                    )
                    marker
            map llScope found `shouldBe` [Nothing]

        it "propagates the scope to a worker thread through inheritLogScope" $ do
            let marker = "progress-spec-worker"
            found <-
                emitAndCollect
                    ( withLogScope "db-a" $ do
                        scoped <- inheritLogScope
                        worker <- async (scoped (reportProgress Info marker))
                        wait worker
                    )
                    marker
            map llScope found `shouldBe` [Just "db-a"]

        it "leaves a worker thread unscoped without inheritLogScope" $ do
            let marker = "progress-spec-worker-bare"
            found <-
                emitAndCollect
                    ( withLogScope "db-a" $ do
                        worker <- async (reportProgress Info marker)
                        wait worker
                    )
                    marker
            map llScope found `shouldBe` [Nothing]

concatLines :: [LogLine] -> String
concatLines = unlines . map llText

{- | Emit a marker line, then return the buffered lines carrying it. The log
buffer is one process-global stream shared with every other spec in this
suite, so each test reads from the cursor captured before emitting and
filters on a marker unique to itself.
-}
emitAndCollect :: IO () -> String -> IO [LogLine]
emitAndCollect emit marker = do
    (cursor, _) <- getLogLines maxBound
    emit
    (_, logLines) <- getLogLines cursor
    pure (filter ((marker `isInfixOf`) . llText) logLines)
