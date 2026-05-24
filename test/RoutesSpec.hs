{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Integration tests for the HTTP API surface (Servant routes).
--
-- Each test goes through a real warp server we boot ONCE (via beforeAll_ +
-- afterAll_) to keep the suite fast. We deliberately spawn the binary as a
-- subprocess because that is the canonical entry point: it exercises Config
-- loading, route wiring, Auth, content negotiation, and the JSON error shape
-- end-to-end.
module RoutesSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Data.Aeson (Value (..), decode)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (isSpace)
import Data.IORef
import Data.List (dropWhileEnd)
import Network.HTTP.Client (
    Manager,
    Response,
    defaultManagerSettings,
    httpLbs,
    method,
    newManager,
    parseRequest,
    requestHeaders,
    responseBody,
    responseHeaders,
    responseStatus,
 )
import Network.HTTP.Types (statusCode)
import System.Directory (doesFileExist, getTemporaryDirectory, removeFile)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO (IOMode (..), hClose, openFile)
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Info as Info
import System.Process (
    CreateProcess (..),
    ProcessHandle,
    StdStream (..),
    createProcess,
    getProcessExitCode,
    interruptProcessGroupOf,
    proc,
    readProcess,
    waitForProcess,
 )
import Test.Hspec

testPort :: Int
testPort = 18299

baseUrl :: String
baseUrl = "http://127.0.0.1:" <> show testPort

findVolcaExe :: IO FilePath
findVolcaExe = do
    envExe <- lookupEnv "VOLCA_EXE"
    exe <- case envExe of
        Just p | not (null p) -> pure p
        _ -> do
            raw <- readProcess "cabal" ["list-bin", "exe:volca"] ""
            pure (dropWhileEnd isSpace raw)
    exists <- doesFileExist exe
    if exists
        then pure exe
        else error $ "volca executable not found at " <> exe <> ". Run 'cabal build' first."

bootServer :: IORef (Maybe (ProcessHandle, Manager, FilePath)) -> IO ()
bootServer ref = do
    exe <- findVolcaExe
    mgr <- newManager defaultManagerSettings
    tmp <- getTemporaryDirectory
    let logFile = tmp </> "volca-routes-test.log"
    logHandle <- openFile logFile AppendMode
    let cfg = tmp </> "volca-routes-test.toml"
    writeFile cfg ("[server]\nport = " <> show testPort <> "\nhost = \"127.0.0.1\"\n")
    let args = ["--config", cfg, "server", "--port", show testPort]
    (_, _, _, ph) <-
        createProcess
            (proc exe args)
                { std_out = UseHandle logHandle
                , std_err = UseHandle logHandle
                , create_group = True
                }
    ready <- waitForReady mgr 50
    if ready
        then writeIORef ref (Just (ph, mgr, cfg))
        else do
            interruptProcessGroupOf ph
            _ <- waitForProcess ph
            hClose logHandle
            error "Server failed to start within timeout (10s)"

teardownServer :: IORef (Maybe (ProcessHandle, Manager, FilePath)) -> IO ()
teardownServer ref = do
    state <- readIORef ref
    case state of
        Just (ph, _, cfg) -> do
            mCode <- getProcessExitCode ph
            case mCode of
                Nothing -> do
                    interruptProcessGroupOf ph
                    _ <- waitForProcess ph
                    pure ()
                Just _ -> pure ()
            _ <- try @SomeException (removeFile cfg)
            pure ()
        Nothing -> pure ()

waitForReady :: Manager -> Int -> IO Bool
waitForReady _ 0 = pure False
waitForReady mgr remaining = do
    threadDelay 200000
    alive <- isAlive mgr
    if alive then pure True else waitForReady mgr (remaining - 1)

isAlive :: Manager -> IO Bool
isAlive mgr = do
    r <- try $ do
        req <- parseRequest (baseUrl <> "/api/v1/db")
        resp <- httpLbs req mgr
        pure (statusCode (responseStatus resp))
    case r of
        Right code -> pure (code < 500)
        Left (_ :: SomeException) -> pure False

{-# NOINLINE serverRef #-}
serverRef :: IORef (Maybe (ProcessHandle, Manager, FilePath))
serverRef = unsafePerformIO (newIORef Nothing)

mgrFromRef :: IO Manager
mgrFromRef = do
    s <- readIORef serverRef
    case s of
        Just (_, m, _) -> pure m
        Nothing -> error "RoutesSpec: server was not booted"

doGet :: String -> IO (Response BL.ByteString)
doGet path = do
    mgr <- mgrFromRef
    req <- parseRequest (baseUrl <> path)
    httpLbs req mgr

doPost :: String -> IO (Response BL.ByteString)
doPost path = do
    mgr <- mgrFromRef
    req0 <- parseRequest (baseUrl <> path)
    let req = req0{method = "POST", requestHeaders = [("Content-Type", "application/json")]}
    httpLbs req mgr

spec :: Spec
spec
    | Info.os == "mingw32" =
        describe "Routes integration (skipped on Windows)" $
            it "subprocess teardown deadlocks on this platform" pending
    | otherwise = routeSpecs

routeSpecs :: Spec
routeSpecs = beforeAll_ (bootServer serverRef) $ afterAll_ (teardownServer serverRef) $ do
    describe "core listing endpoints" $ do
        let endpoints =
                [ "/api/v1/db"
                , "/api/v1/methods"
                , "/api/v1/units"
                , "/api/v1/compartment-mappings"
                ]
        mapM_
            ( \ep ->
                it ("GET " <> ep <> " returns 200") $ do
                    resp <- doGet ep
                    statusCode (responseStatus resp) `shouldBe` 200
            )
            endpoints

    describe "version + tooling dumps" $ do
        it "GET /api/v1/version returns a JSON object carrying a 'version' field" $ do
            -- Body must be a JSON object with the documented 'version' key —
            -- anything else (HTML error page, empty body, wrong wrapper) is a
            -- regression that 'body length > 0' would silently accept.
            resp <- doGet "/api/v1/version"
            statusCode (responseStatus resp) `shouldBe` 200
            decode (responseBody resp) `shouldSatisfy` \case
                Just (Object km) -> KM.member "version" km
                _ -> False

        it "GET /api/v1/openapi.json returns an OpenAPI 3 document with 'paths'" $ do
            -- The OpenAPI spec is served at /api/v1/openapi.json (not /openapi).
            -- We check the actual contract (openapi+paths keys), not the byte
            -- length — Swagger UI would break in the same way a missing 'paths'
            -- would, while a 'body length > 100' assertion would pass on any
            -- random JSON-shaped payload.
            resp <- doGet "/api/v1/openapi.json"
            statusCode (responseStatus resp) `shouldBe` 200
            decode (responseBody resp) `shouldSatisfy` \case
                Just (Object km) -> KM.member "openapi" km && KM.member "paths" km
                _ -> False

    describe "404 / unknown resource" $ do
        it "GET /api/v1/db/no-such-db/activity/whatever returns 404" $ do
            -- This hits an unambiguously absent route: even if the DB existed,
            -- 'whatever' is not a real processId, so the handler returns 404.
            resp <- doGet "/api/v1/db/no-such-db/activity/whatever"
            statusCode (responseStatus resp) `shouldBe` 404

    describe "method-not-allowed" $ do
        it "GET /api/v1/db/X returns 405 (only DELETE is defined on /db/{name})" $ do
            -- Documents the current API surface: /db/{name} accepts DELETE,
            -- not GET — so Servant rightly answers 405 for GET.
            resp <- doGet "/api/v1/db/no-such-db"
            statusCode (responseStatus resp) `shouldBe` 405

        it "POST /api/v1/db returns 405 (db listing is read-only)" $ do
            resp <- doPost "/api/v1/db"
            statusCode (responseStatus resp) `shouldBe` 405

    describe "response shape" $ do
        it "JSON endpoints carry Content-Type: application/json" $ do
            resp <- doGet "/api/v1/db"
            let ct = lookup "Content-Type" (responseHeaders resp)
            ct `shouldSatisfy` maybe False ("application/json" `BS.isPrefixOf`)
