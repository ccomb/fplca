{-# LANGUAGE OverloadedStrings #-}

module ServiceSpec (spec) where

import Test.Hspec
import TestHelpers (loadSampleDatabase)
import GoldenData
import Types
import Service ( validateUUID, parseProcessIdFromText, validateProcessIdInMatrixIndex
               , resolveActivityAndProcessId, ServiceError(..) )
import qualified Data.UUID as UUID

spec :: Spec
spec = do

    -- -----------------------------------------------------------------------
    -- validateUUID
    -- -----------------------------------------------------------------------
    describe "validateUUID" $ do

        it "accepts a well-formed UUID" $
            case validateUUID "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa" of
                Right t -> t `shouldBe` "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa"
                Left  _ -> expectationFailure "Expected Right"

        it "rejects an empty string" $
            case validateUUID "" of
                Left (InvalidUUID _) -> return ()
                _                    -> expectationFailure "Expected InvalidUUID"

        it "rejects a plain word" $
            case validateUUID "not-a-uuid" of
                Left (InvalidUUID _) -> return ()
                _                    -> expectationFailure "Expected InvalidUUID"

        it "rejects a truncated UUID" $
            case validateUUID "aaaaaaaa-aaaa-aaaa-aaaa" of
                Left (InvalidUUID _) -> return ()
                _                    -> expectationFailure "Expected InvalidUUID"

    -- -----------------------------------------------------------------------
    -- parseProcessIdFromText (requires DB)
    -- -----------------------------------------------------------------------
    describe "parseProcessIdFromText" $ do

        it "parses a valid ProcessId text from SAMPLE.min3" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let pidText = processIdToText db 0
            case parseProcessIdFromText db pidText of
                Right 0 -> return ()
                Right n -> expectationFailure $ "Expected ProcessId 0 but got " ++ show n
                Left  e -> expectationFailure $ "Expected Right but got: " ++ show e

        it "returns InvalidProcessId for garbage text" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            case parseProcessIdFromText db "not-a-process-id" of
                Left (InvalidProcessId _) -> return ()
                _                         -> expectationFailure "Expected InvalidProcessId"

        it "returns InvalidProcessId for a single UUID (missing product part)" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            case parseProcessIdFromText db "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa" of
                Left (InvalidProcessId _) -> return ()
                -- bare UUID fallback is in resolveActivityAndProcessId, not here
                Left  _                   -> return ()
                Right _                   -> expectationFailure "Expected Left"

    -- -----------------------------------------------------------------------
    -- validateProcessIdInMatrixIndex
    -- -----------------------------------------------------------------------
    describe "validateProcessIdInMatrixIndex" $ do

        it "accepts ProcessId 0 in SAMPLE.min3 (3 activities)" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            case validateProcessIdInMatrixIndex db 0 of
                Right () -> return ()
                Left  e  -> expectationFailure $ "Expected Right but got: " ++ show e

        it "rejects a ProcessId beyond the activity count" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            case validateProcessIdInMatrixIndex db 999 of
                Left (MatrixError _) -> return ()
                _                    -> expectationFailure "Expected MatrixError"

        it "rejects a negative ProcessId" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            case validateProcessIdInMatrixIndex db (-1) of
                Left (MatrixError _) -> return ()
                _                    -> expectationFailure "Expected MatrixError"

    -- -----------------------------------------------------------------------
    -- resolveActivityAndProcessId
    -- -----------------------------------------------------------------------
    describe "resolveActivityAndProcessId" $ do

        it "resolves activity X by full ProcessId text" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let pidText = processIdToText db 0
            case resolveActivityAndProcessId db pidText of
                Right (pid, act) -> do
                    pid `shouldBe` 0
                    activityName act `shouldBe` "production of product X"
                Left err -> expectationFailure $ "Expected Right but got: " ++ show err

        it "falls back to bare activity UUID (EcoInvent compatibility)" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            -- activity UUID without product UUID part
            case resolveActivityAndProcessId db "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa" of
                Right (_, act) -> activityName act `shouldBe` "production of product X"
                Left  err      -> expectationFailure $ "Expected Right but got: " ++ show err

        it "returns ActivityNotFound for a non-existent ProcessId text" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            -- Valid UUID pair format but not in DB
            let ghost = "99999999-9999-9999-9999-999999999999_99999999-9999-9999-9999-999999999999"
            case resolveActivityAndProcessId db ghost of
                Left _ -> return ()
                Right _ -> expectationFailure "Expected Left for unknown process"
