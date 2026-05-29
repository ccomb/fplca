{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forM_, unless, when)
import Data.IORef
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Foreign.C.Types (CInt (..))
import Options.Applicative
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.Exit (die, exitFailure)
import System.FilePath ((</>))
import System.IO (hFlush, stderr, stdout)
import Text.Read (readMaybe)

-- VoLCA imports
import API.Auth (authMiddleware)
import CLI.Client (executeRemoteCommand, resolveRemoteConfig)
import CLI.Command (executeCommand)
import CLI.Parser (cliParserInfo)
import CLI.Repl (runRepl)
import CLI.Types
import Config (ClassificationPreset, Config (..), DatabaseConfig (..), HostingConfig (..), ServerConfig (..), loadConfig)
import Control.Concurrent.STM (readTVarIO)
import Database.Manager (DatabaseManager (..), initDatabaseManager)
import Matrix (initializeSolverForServer)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Progress

-- For server mode

import API.DatabaseHandlers (uploadBodyCeiling)
import API.Licenses (licensesResponse)
import API.MCP (mcpApp, toolDefinitions)
import API.Routes (lcaAPI, lcaServer, volcaOpenApi)
import App.Env (AppEnv (..))
import Data.Aeson (encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.String (fromString)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hCacheControl, hContentType, hPragma)
import Network.Wai (Application, Middleware, Request (..), Response, ResponseReceived, mapResponseHeaders, pathInfo, rawPathInfo, rawQueryString, requestHeaders, requestMethod, responseLBS, responseStream)
import Network.Wai.Application.Static (StaticSettings, defaultWebAppSettings, ssIndices, staticApp)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setPort, setTimeout)
import Network.Wai.Middleware.RequestSizeLimit (defaultRequestSizeLimitSettings, requestSizeLimitMiddleware, setMaxLengthForRequest)
import Servant (serve)
import WaiAppStatic.Types (MaxAge (..), ssMaxAge, unsafeToPiece)

-- _exit(0) bypasses Haskell RTS teardown — necessary on statically-linked
-- glibc builds (notably aarch64) where the threaded RTS's shutdown calls
-- pthread_cancel, which in turn dlopen()'s libgcc_s.so.1 to find the
-- stack unwinder and SIGILLs when that returns NULL in a static binary.
-- The 500 ms delay before calling this gives Warp time to flush the HTTP
-- response back to the caller; hFlush flushes any buffered log lines.
foreign import ccall "_exit" c_exit :: CInt -> IO ()

hardExit :: IO ()
hardExit = do
    hFlush stdout
    hFlush stderr
    c_exit 0

-- | Main entry point
main :: IO ()
main = do
    cliConfig <- execParser cliParserInfo
    validateCLIConfig cliConfig

    case (CLI.Types.command cliConfig, configFile (globalOptions cliConfig)) of
        (Just DumpOpenApi, _) -> BSL.putStrLn (encode volcaOpenApi)
        (Just DumpMcpTools, _) -> BSL.putStrLn (encode toolDefinitions)
        (Just (Server serverOpts), Just cfgFile) -> runServerWithConfig cliConfig serverOpts cfgFile
        (Just Repl, Just cfgFile) -> runReplMode cliConfig cfgFile
        (Just cmd, Just cfgFile) | isLocalCommand cmd -> runCLIWithConfig cliConfig cmd cfgFile
        (Just cmd, Just cfgFile) -> runCLIViaAPI cliConfig cmd cfgFile
        (Nothing, Just cfgFile) -> runConfigLoadOnly cliConfig cfgFile
        (Just Stop, Nothing) -> runStopWithoutConfig cliConfig
        _ -> die "--config is required"

-- | Load config or die with error message
loadConfigOrDie :: FilePath -> IO Config
loadConfigOrDie cfgFile = do
    reportProgress Info $ "Loading configuration from: " ++ cfgFile
    configResult <- loadConfig cfgFile
    case configResult of
        Left err -> do
            reportError $ "Failed to load config: " ++ T.unpack err
            exitFailure
        Right config -> return config

-- | Commands that require local database access (not available via HTTP)
isLocalCommand :: Command -> Bool
isLocalCommand (DebugMatrices _ _) = True
isLocalCommand (ExportMatrices _) = True
isLocalCommand (Plugin _) = True
isLocalCommand _ = False

-- | Run local-only CLI commands through DatabaseManager (loads DBs, matrix solver)
runCLIWithConfig :: CLIConfig -> Command -> FilePath -> IO ()
runCLIWithConfig cliConfig cmd cfgFile = do
    config <- loadConfigOrDie cfgFile
    initializeSolverForServer
    dbManager <- initDatabaseManager config (noCache (globalOptions cliConfig)) (Just cfgFile)
    executeCommand cliConfig cmd dbManager

-- | Run CLI commands via HTTP against a running server (lightweight, no DB loading)
runCLIViaAPI :: CLIConfig -> Command -> FilePath -> IO ()
runCLIViaAPI cliConfig cmd cfgFile = do
    config <- loadConfigOrDie cfgFile
    mgr <- newManager defaultManagerSettings
    rc <- resolveRemoteConfig (globalOptions cliConfig) (Just config)
    executeRemoteCommand mgr rc (globalOptions cliConfig) cmd

-- | Run interactive REPL over HTTP (auto-starts server if needed)
runReplMode :: CLIConfig -> FilePath -> IO ()
runReplMode cliConfig cfgFile = do
    config <- loadConfigOrDie cfgFile
    mgr <- newManager defaultManagerSettings
    rc <- resolveRemoteConfig (globalOptions cliConfig) (Just config)
    runRepl mgr rc (globalOptions cliConfig) cfgFile

-- | Run stop without config — resolveRemoteConfig falls back to env vars / defaults
runStopWithoutConfig :: CLIConfig -> IO ()
runStopWithoutConfig cliConfig = do
    mgr <- newManager defaultManagerSettings
    rc <- resolveRemoteConfig (globalOptions cliConfig) Nothing
    executeRemoteCommand mgr rc (globalOptions cliConfig) Stop

-- | Apply the --load override (if any) to the in-memory config.
applyLoadOverride :: ServerOptions -> Config -> Config
applyLoadOverride serverOpts config = case serverLoadDbs serverOpts of
    Nothing -> config
    Just dbNames -> config{cfgDatabases = map (overrideLoad dbNames) (cfgDatabases config)}

-- | Log loaded databases (allows starting with none for BYOL mode).
logLoadedDatabases :: DatabaseManager -> IO ()
logLoadedDatabases dbManager = do
    loadedDbs <- readTVarIO (dmLoadedDbs dbManager)
    reportProgress Info $
        if M.null loadedDbs
            then "No databases loaded - upload or load one via the web interface"
            else "Loaded databases: " ++ intercalate ", " (map T.unpack (M.keys loadedDbs))

{- | Resolve the admin password from CLI flag, config file, or env var, in that
order. Returns 'Nothing' when authentication is disabled (no source set).
-}
resolvePassword :: GlobalOptions -> ServerConfig -> IO (Maybe String)
resolvePassword globalOpts serverCfg = case CLI.Types.serverPassword globalOpts of
    Just pwd -> pure (Just pwd)
    Nothing -> case scPassword serverCfg of
        Just pwd -> pure (Just (T.unpack pwd))
        Nothing -> lookupEnv "VOLCA_PASSWORD"

{- | In desktop mode, print a machine-readable port line for the launcher
to capture and stay quiet. Otherwise emit the human-facing startup banner.
-}
logServerStartup :: ServerOptions -> Int -> Maybe String -> IO ()
logServerStartup serverOpts port password
    | serverDesktopMode serverOpts = do
        putStrLn ("VOLCA_PORT=" ++ show port)
        hFlush stdout
    | otherwise = do
        reportProgress Info ("Starting API server on port " ++ show port)
        reportProgress Info ("Tree depth: " ++ show (serverTreeDepth serverOpts))
        reportProgress Info $ case password of
            Just _ -> "Authentication: ENABLED"
            Nothing -> "Authentication: DISABLED (use --password or VOLCA_PASSWORD to enable)"
        reportProgress Info ("Web interface available at: http://localhost:" ++ show port ++ "/")

{- | Allocate the idle-tracking refs and fork the watchdog when
@--idle-timeout@ is positive. The refs are returned for both the
tracking and the shutdown middleware.
-}
setupIdleTimeout :: ServerOptions -> IO (IORef UTCTime, IORef Bool)
setupIdleTimeout serverOpts = do
    lastRequestRef <- newIORef =<< getCurrentTime
    idleActiveRef <- newIORef False
    let idleTimeout = serverIdleTimeout serverOpts
    when (idleTimeout > 0) $ do
        reportProgress Info ("Idle timeout: " ++ show idleTimeout ++ "s")
        writeIORef idleActiveRef True
        _ <- forkIO (idleWatchdog lastRequestRef idleActiveRef idleTimeout)
        pure ()
    pure (lastRequestRef, idleActiveRef)

-- | Stack idle-tracking, shutdown-endpoint and (optionally) auth middleware.
wrapWithMiddleware :: Maybe String -> IORef UTCTime -> IORef Bool -> Application -> Application
wrapWithMiddleware password lastRequestRef idleActiveRef baseApp =
    let withIdleAndShutdown =
            idleTrackingMiddleware lastRequestRef $
                shutdownEndpoint lastRequestRef idleActiveRef baseApp
     in case password of
            Just pwd -> authMiddleware (C8.pack pwd) withIdleAndShutdown
            Nothing -> withIdleAndShutdown

{- | Reject oversized upload requests at the HTTP layer, before the body is
buffered into memory. The per-request ceiling comes from 'uploadBodyCeiling',
so the unlimited tier is never bounded and only the policy-governed upload
routes are capped. This backstops the in-handler 'checkUploadSize'.
-}
uploadSizeLimitMiddleware :: Maybe HostingConfig -> Middleware
uploadSizeLimitMiddleware hostingConfig =
    requestSizeLimitMiddleware $
        setMaxLengthForRequest
            (pure . uploadBodyCeiling hostingConfig . pathInfo)
            defaultRequestSizeLimitSettings

-- | Run server with multi-database configuration file
runServerWithConfig :: CLIConfig -> ServerOptions -> FilePath -> IO ()
runServerWithConfig cliConfig serverOpts cfgFile = do
    config <- applyLoadOverride serverOpts <$> loadConfigOrDie cfgFile
    reportProgress Info "Initializing database manager..."
    dbManager <- initDatabaseManager config (noCache (globalOptions cliConfig)) (Just cfgFile)
    logLoadedDatabases dbManager
    initializeSolverForServer
    let port = fromMaybe (scPort (cfgServer config)) (serverPort serverOpts)
        staticDir = fromMaybe "web/dist" (serverStaticDir serverOpts)
    password <- resolvePassword (globalOptions cliConfig) (cfgServer config)
    logServerStartup serverOpts port password
    (lastRequestRef, idleActiveRef) <- setupIdleTimeout serverOpts
    baseApp <-
        createServerApp
            dbManager
            (serverTreeDepth serverOpts)
            staticDir
            (serverDesktopMode serverOpts)
            password
            (cfgHosting config)
            (cfgClassificationPresets config)
    let finalApp =
            uploadSizeLimitMiddleware (cfgHosting config) $
                wrapWithMiddleware password lastRequestRef idleActiveRef baseApp
        settings = setTimeout 600 (setPort port defaultSettings)
    runSettings settings finalApp

{- | Run config load-only mode (load all databases from config and exit)
Useful for cache generation, validation, and benchmarking
-}
runConfigLoadOnly :: CLIConfig -> FilePath -> IO ()
runConfigLoadOnly cliConfig cfgFile = do
    config <- loadConfigOrDie cfgFile

    -- Initialize DatabaseManager (pre-loads databases with load=true)
    reportProgress Info "Loading all databases from config..."
    _dbManager <- initDatabaseManager config (noCache (globalOptions cliConfig)) (Just cfgFile)

    -- Report success
    let loadCount = length $ filter dcLoad (cfgDatabases config)
    reportProgress Info $ "No command specified - " ++ show loadCount ++ " database(s) loaded and cached"
    reportProgress Info "Cache files ready for deployment"

-- | Override load flag for databases based on --load CLI option
overrideLoad :: [T.Text] -> DatabaseConfig -> DatabaseConfig
overrideLoad dbNames dbConfig =
    dbConfig{dcLoad = dcName dbConfig `elem` dbNames}

{- | Swagger-UI shell that pulls the OpenAPI spec from our @/api/v1/openapi.json@
endpoint. Served verbatim from @/api/v1/docs@; constant per build.
-}
swaggerHtml :: BSL.ByteString
swaggerHtml =
    "<!DOCTYPE html><html><head><title>volca API</title>\
    \<meta charset=\"utf-8\"/>\
    \<link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/swagger-ui-dist/swagger-ui.css\">\
    \</head><body>\
    \<div id=\"swagger-ui\"></div>\
    \<script src=\"https://cdn.jsdelivr.net/npm/swagger-ui-dist/swagger-ui-bundle.js\"></script>\
    \<script>SwaggerUIBundle({url:\"/api/v1/openapi.json\",dom_id:\"#swagger-ui\"})</script>\
    \</body></html>"

{- | Serve the Elm SPA bundle from @staticDir@, with the SPA's @index.html@ as
the directory index and no @max-age@ caching headers.
-}
spaStaticSettings :: FilePath -> StaticSettings
spaStaticSettings staticDir =
    (defaultWebAppSettings staticDir)
        { ssIndices = [unsafeToPiece (T.pack "index.html")]
        , ssMaxAge = NoMaxAge
        }

{- | Serve files under @/static/<rest>@ by stripping the prefix and delegating
to wai-app-static.
-}
serveStripped :: StaticSettings -> Application
serveStripped settings req respond =
    let strippedPath = BS.drop 7 (rawPathInfo req)
        newPathInfo = case pathInfo req of
            (segment : rest) | segment == T.pack "static" -> rest
            other -> other
        staticReq = req{rawPathInfo = strippedPath, pathInfo = newPathInfo}
     in staticApp settings staticReq respond

{- | Serve the SPA shell (@index.html@) for any non-API path, with cache-busting
headers so the browser always re-fetches the latest bundle.
-}
serveSpaIndex :: StaticSettings -> Application
serveSpaIndex settings req respond =
    let indexReq = req{rawPathInfo = C8.pack "/", pathInfo = []}
        noCacheRespond res =
            respond $
                mapResponseHeaders
                    ( \hs ->
                        (hCacheControl, C8.pack "no-cache, no-store, must-revalidate")
                            : (hPragma, C8.pack "no-cache")
                            : hs
                    )
                    res
     in staticApp settings indexReq noCacheRespond

{- | Path-based request dispatcher. The fixed endpoints (@/mcp@,
@/api/v1/{openapi.json,licenses,docs,logs/stream}@) match exactly; anything
under @/api/@ goes through Servant; @/static/@ serves bundled assets; the
catch-all hands back the SPA so client-side routing can handle the URL.
-}
dispatchRequest :: FilePath -> Application -> Application -> Application
dispatchRequest staticDir mcp apiApp req respond =
    let path = rawPathInfo req
        settings = spaStaticSettings staticDir
     in if
            | path == "/mcp" -> mcp req respond
            | path == "/api/v1/openapi.json" ->
                respond $ responseLBS status200 [(hContentType, "application/json")] (encode volcaOpenApi)
            | path == "/api/v1/licenses" -> respond licensesResponse
            | path == "/api/v1/docs" ->
                respond $ responseLBS status200 [(hContentType, "text/html; charset=utf-8")] swaggerHtml
            | path == "/api/v1/logs/stream" -> handleLogStream req respond
            | C8.pack "/api/" `BS.isPrefixOf` path -> apiApp req respond
            | C8.pack "/static/" `BS.isPrefixOf` path -> serveStripped settings req respond
            | otherwise -> serveSpaIndex settings req respond

-- | Per-request log line written to stdout (suppressed in desktop mode).
logRequest :: Request -> IO ()
logRequest req = do
    putStrLn $ C8.unpack (requestMethod req) ++ " " ++ C8.unpack (rawPathInfo req <> rawQueryString req)
    hFlush stdout

-- | Create a Wai application with DatabaseManager.
createServerApp :: DatabaseManager -> Int -> FilePath -> Bool -> Maybe String -> Maybe HostingConfig -> [ClassificationPreset] -> IO Application
createServerApp dbManager maxTreeDepth staticDir desktopMode password hostingConfig filterPresets = do
    -- The MCP @web_url@ deep links point at Elm SPA routes served from
    -- 'staticDir'. When the SPA is not bundled (backend-only image), those
    -- URLs would 404, so we omit 'web_url' from MCP responses entirely.
    hasFrontend <- doesFileExist (staticDir </> "index.html")
    unless (desktopMode || hasFrontend) $
        reportProgress Info "Frontend not bundled — MCP responses will omit 'web_url'"
    mcp <- mcpApp dbManager filterPresets hasFrontend
    let env =
            AppEnv
                { aeDbManager = dbManager
                , aeMaxTreeDepth = maxTreeDepth
                , aePassword = password
                , aeHostingConfig = hostingConfig
                , aeClassificationPresets = filterPresets
                }
        apiApp = serve lcaAPI (lcaServer env)
    pure $ \req respond -> do
        unless desktopMode (logRequest req)
        dispatchRequest staticDir mcp apiApp req respond

-- | SSE endpoint for real-time log streaming
handleLogStream :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleLogStream req respond = do
    let lastEventId = lookup "Last-Event-ID" (requestHeaders req)
        since = maybe 0 (\v -> fromMaybe 0 (readMaybe (C8.unpack v))) lastEventId
    respond
        $ responseStream
            status200
            [ (hContentType, "text/event-stream")
            , (hCacheControl, "no-cache")
            , ("X-Accel-Buffering", "no")
            ]
        $ \write flush -> do
            let loop !cursor = do
                    (!nextIdx, newLines) <- waitForNewLines cursor
                    forM_ newLines $ \line ->
                        write (fromString $ "id:" ++ show nextIdx ++ "\ndata:" ++ line ++ "\n\n")
                    flush
                    loop nextIdx
            loop since

-- | Validate CLI configuration for consistency
validateCLIConfig :: CLIConfig -> IO ()
validateCLIConfig (CLIConfig globalOpts _) =
    case (format globalOpts, jsonPath globalOpts) of
        (Just CSV, Nothing) ->
            die "--format csv requires --jsonpath. Examples: --jsonpath 'srResults', --jsonpath 'piActivity.pfaExchanges'"
        (Just fmt, Just _)
            | fmt /= CSV ->
                die "--jsonpath can only be used with --format csv"
        _ -> pure ()

-- | WAI middleware that updates last-request timestamp on every request
idleTrackingMiddleware :: IORef UTCTime -> Application -> Application
idleTrackingMiddleware ref app req respond = do
    getCurrentTime >>= writeIORef ref
    app req respond

{- | Middleware that handles POST /api/v1/idle-timeout/{seconds} and POST /api/v1/shutdown
0 = cancel timeout, N>0 = activate/restart idle watchdog
-}
shutdownEndpoint :: IORef UTCTime -> IORef Bool -> Application -> Application
shutdownEndpoint lastRequestRef idleActiveRef app req respond = do
    let path = rawPathInfo req
        method = requestMethod req
    case (method, BS.stripPrefix "/api/v1/idle-timeout/" path, path) of
        ("POST", _, "/api/v1/shutdown") -> do
            reportProgress Info "Shutdown requested via API"
            _ <- forkIO $ threadDelay 500000 >> hardExit
            respond $
                responseLBS
                    status200
                    [(hContentType, "application/json")]
                    "{\"ok\":true}"
        ("POST", Just secondsBS, _) -> do
            let seconds = fromMaybe 30 (readMaybe (C8.unpack secondsBS)) :: Int
            if seconds <= 0
                then do
                    writeIORef idleActiveRef False
                    reportProgress Info "Idle timeout cancelled"
                else do
                    alreadyActive <- readIORef idleActiveRef
                    writeIORef idleActiveRef True
                    getCurrentTime >>= writeIORef lastRequestRef
                    unless alreadyActive $ do
                        _ <- forkIO $ idleWatchdog lastRequestRef idleActiveRef seconds
                        pure ()
                    reportProgress Info $ "Idle timeout: " ++ show seconds ++ "s"
            respond $
                responseLBS
                    status200
                    [(hContentType, "application/json")]
                    "{\"ok\":true}"
        (_, _, _) -> app req respond

-- | Background thread that exits the server after idle timeout (in seconds)
idleWatchdog :: IORef UTCTime -> IORef Bool -> Int -> IO ()
idleWatchdog ref activeRef timeoutSecs = go
  where
    checkInterval = min (timeoutSecs * 1000000) (5 * 1000000) -- check every 5s or timeout, whichever is shorter
    go = do
        threadDelay checkInterval
        active <- readIORef activeRef
        if not active
            then pure ()
            else do
                now <- getCurrentTime
                lastReq <- readIORef ref
                let idleSeconds = realToFrac (diffUTCTime now lastReq) :: Double
                if idleSeconds >= fromIntegral timeoutSecs
                    then do
                        reportProgress Info $ "Idle for " ++ show timeoutSecs ++ "s, shutting down."
                        hardExit
                    else go
