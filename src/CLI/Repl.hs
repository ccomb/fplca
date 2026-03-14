{-# LANGUAGE OverloadedStrings #-}

module CLI.Repl (runRepl) where

import CLI.Client (RemoteConfig(..), executeRemoteCommand, apiGet)
import CLI.Parser (commandParser)
import CLI.Types
import Control.Concurrent (threadDelay)
import Control.Exception (try, SomeException)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value)
import Data.IORef
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client (Manager)
import qualified Options.Applicative as OA
import System.Console.Haskeline
import System.Environment (getExecutablePath)
import System.IO (hFlush, stdout)
import System.IO (openFile, IOMode(..))
import System.Process (createProcess, proc, CreateProcess(..), StdStream(..))
import System.Directory (getTemporaryDirectory)
import System.FilePath ((</>))

-- | REPL session state
data ReplState = ReplState
    { rsDb     :: Maybe Text          -- current --db selection (switchable via "use <db>")
    , rsFormat :: Maybe OutputFormat   -- current format override (switchable via ":format")
    }

-- | Run the interactive REPL, auto-starting the server if needed
runRepl :: Manager -> RemoteConfig -> GlobalOptions -> FilePath -> IO ()
runRepl mgr rc globalOpts cfgFile = do
    ensureServer mgr rc globalOpts cfgFile
    stateRef <- newIORef ReplState { rsDb = dbName globalOpts, rsFormat = Just Pretty }
    putStrLn "Type :help for available commands, :quit to exit."
    runInputT (setComplete (completionFunc stateRef) defaultSettings) (loop stateRef)
  where
    loop stateRef = do
        st <- liftIO $ readIORef stateRef
        let prompt = "fplca" ++ maybe "" (\db -> "[" ++ T.unpack db ++ "]") (rsDb st) ++ "> "
        minput <- getInputLine prompt
        case minput of
            Nothing    -> return ()  -- Ctrl+D
            Just input -> dispatch stateRef (words input) >> loop stateRef

    dispatch _ []          = return ()
    dispatch _ [":quit"]   = return ()
    dispatch _ [":q"]      = return ()
    dispatch _ [":help"]   = liftIO printHelp
    dispatch _ (":help":_) = liftIO printHelp
    dispatch _ ["help"]    = liftIO printHelp

    dispatch stateRef ["use", dbArg] = liftIO $ do
        modifyIORef stateRef $ \s -> s { rsDb = Just (T.pack dbArg) }
        putStrLn $ "Switched to database: " ++ dbArg

    dispatch stateRef [":format", fmtArg] = liftIO $ case parseOutputFormat fmtArg of
        Just fmt -> do
            modifyIORef stateRef $ \s -> s { rsFormat = Just fmt }
            putStrLn $ "Format: " ++ fmtArg
        Nothing -> putStrLn "Valid formats: json, pretty, table, csv"

    dispatch stateRef tokens = liftIO $ do
        st <- readIORef stateRef
        let opts = globalOpts { dbName = rsDb st, format = rsFormat st }
        case parseCommand tokens of
            Just cmd -> executeRemoteCommand mgr rc opts cmd
            Nothing  -> putStrLn "Unknown command. Type :help for usage."

    parseCommand tokens =
        OA.getParseResult $
            OA.execParserPure OA.defaultPrefs (OA.info (commandParser OA.<**> OA.helper) mempty) tokens

-- | Check if the server is reachable; if not, start it and wait
ensureServer :: Manager -> RemoteConfig -> GlobalOptions -> FilePath -> IO ()
ensureServer mgr rc globalOpts cfgFile = do
    alive <- isServerAlive mgr rc
    if alive
        then putStrLn $ "Connected to " ++ rcBaseUrl rc
        else do
            putStr $ "Starting server at " ++ rcBaseUrl rc ++ "..."
            hFlush stdout
            startServerProcess globalOpts rc cfgFile
            waitForServer mgr rc 120  -- up to 120 seconds
            putStrLn " ready."

-- | Ping the server (try without auth — any HTTP response means it's up)
isServerAlive :: Manager -> RemoteConfig -> IO Bool
isServerAlive mgr rc = do
    let noAuth = rc { rcAuth = Nothing }
    result <- try (apiGet mgr noAuth "/api/v1/database") :: IO (Either SomeException (Either String Value))
    return $ case result of
        Right (Right _) -> True   -- 2xx response
        Right (Left e)  -> not ("Cannot connect" `isPrefixOf` e)  -- 401/404 = alive, connection error = not
        Left _          -> False  -- unexpected exception

-- | Spawn the server as a background process, logging to a temp file
startServerProcess :: GlobalOptions -> RemoteConfig -> FilePath -> IO ()
startServerProcess globalOpts rc cfgFile = do
    exe <- getExecutablePath
    let port = extractPort (rcBaseUrl rc)
        args = ["--config", cfgFile, "server", "--port", show port]
            ++ maybe [] (\p -> ["--password", p]) (serverPassword globalOpts)
    tmpDir <- getTemporaryDirectory
    let logFile = tmpDir </> "fplca-server.log"
    logHandle <- openFile logFile AppendMode
    _ <- createProcess (proc exe args)
        { std_out = UseHandle logHandle
        , std_err = UseHandle logHandle
        , create_group = True    -- detach from REPL's process group
        , delegate_ctlc = False
        }
    putStr (" log: " ++ logFile ++ " ")
    hFlush stdout

-- | Extract port from URL like "http://host:port"
extractPort :: String -> Int
extractPort url =
    case reverse $ takeWhile (/= ':') $ reverse url of
        portStr | all (`elem` ("0123456789" :: String)) portStr
                , not (null portStr) -> read portStr
        _ -> 8081  -- fallback to config default

-- | Poll until the server responds, with dot progress
waitForServer :: Manager -> RemoteConfig -> Int -> IO ()
waitForServer mgr rc remaining
    | remaining <= 0 = putStrLn "\nServer failed to start within timeout."
    | otherwise = do
        threadDelay 500000  -- 0.5s
        alive <- isServerAlive mgr rc
        if alive
            then return ()
            else do
                putChar '.'
                hFlush stdout
                waitForServer mgr rc (remaining - 1)

-- | Tab completion for command names and flags
completionFunc :: IORef ReplState -> CompletionFunc IO
completionFunc _stateRef = completeWord Nothing " \t" $ \prefix ->
    return [ simpleCompletion c | c <- allCompletions, prefix `isPrefixOf` c ]

allCompletions :: [String]
allCompletions = commands ++ flags
  where
    commands =
        [ "activity", "tree", "inventory", "flow", "activities", "flows"
        , "lcia", "database", "method", "methods", "synonyms"
        , "compartment-mappings", "units", "mapping"
        , "use", ":format", ":help", ":quit"
        ]
    flags =
        [ "--name", "--geo", "--product", "--limit", "--offset"
        , "--query", "--lang", "--method", "--format", "--db"
        , "--matched", "--unmatched", "--uncharacterized"
        , "--depth", "json", "pretty", "table", "csv"
        , "list", "upload", "delete", "activities"
        ]

printHelp :: IO ()
printHelp = do
    putStrLn "Commands:"
    putStrLn "  activity UUID              Activity info"
    putStrLn "  tree UUID [--depth N]      Supply chain tree"
    putStrLn "  inventory UUID             Life cycle inventory"
    putStrLn "  flow FLOW_ID [activities]  Flow info"
    putStrLn "  activities [--name X]      Search activities"
    putStrLn "  flows [--query X]          Search flows"
    putStrLn "  lcia UUID --method M_UUID  LCIA computation"
    putStrLn "  mapping METHOD_UUID        Flow mapping coverage"
    putStrLn "  database [list|upload|delete]"
    putStrLn "  method [list|upload|delete]"
    putStrLn "  methods                    List loaded methods"
    putStrLn "  synonyms                   List synonym sources"
    putStrLn "  units                      List unit definitions"
    putStrLn ""
    putStrLn "Session:"
    putStrLn "  use <db-name>              Switch database"
    putStrLn "  :format json|pretty|table  Switch output format"
    putStrLn "  :help                      This help"
    putStrLn "  :quit / Ctrl+D             Exit"
    hFlush stdout
