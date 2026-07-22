{-# LANGUAGE OverloadedStrings #-}

module CLI.Client (
    RemoteConfig (..),
    resolveRemoteConfig,
    executeRemoteCommand,
    apiGet,
    apiPost,
    deleteSelectionBody,
) where

import CLI.Types
import Config (Config (..), ServerConfig (..))
import Control.Exception (IOException, try)
import Data.Aeson (Value (..), decode, encode, object, (.:), (.=))
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser, parseMaybe, withArray, withObject)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List (intercalate, transpose)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import Network.HTTP.Client (
    HttpException (..),
    HttpExceptionContent (..),
    Manager,
    Request (method, requestBody, requestHeaders),
    RequestBody (..),
    Response,
    httpLbs,
    parseRequest,
    responseBody,
    responseHeaders,
    responseStatus,
    setQueryString,
 )
import Network.HTTP.Types.Header (HeaderName)
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.URI (urlDecode)
import Progress (ProgressLevel (Warning), reportError, reportProgress)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (IOMode (ReadMode), hFileSize, withBinaryFile)

-- | Configuration for connecting to a remote VoLCA server
data RemoteConfig = RemoteConfig
    { rcBaseUrl :: String
    , rcAuth :: Maybe String
    }

-- | Resolve server URL and auth from CLI flags, env vars, and config (optional)
resolveRemoteConfig :: GlobalOptions -> Maybe Config -> IO RemoteConfig
resolveRemoteConfig globalOpts mbConfig = do
    url <- case serverUrl globalOpts of
        Just u -> return u
        Nothing -> do
            envUrl <- lookupEnv "VOLCA_URL"
            case envUrl of
                Just u -> return u
                Nothing -> case cfgServer <$> mbConfig of
                    Just sc -> return $ "http://" ++ T.unpack (scHost sc) ++ ":" ++ show (scPort sc)
                    Nothing -> do
                        reportError "No server URL: use --config, --url, or VOLCA_URL"
                        exitFailure
    pwd <- case serverPassword globalOpts of
        Just p -> return (Just p)
        Nothing -> case mbConfig >>= scPassword . cfgServer of
            Just p -> return (Just $ T.unpack p)
            Nothing -> lookupEnv "VOLCA_PASSWORD"
    return RemoteConfig{rcBaseUrl = url, rcAuth = pwd}

-- | Execute a CLI command via HTTP against a running server
executeRemoteCommand :: Manager -> RemoteConfig -> GlobalOptions -> Command -> IO ()
executeRemoteCommand mgr rc globalOpts cmd = do
    let fmt = fromMaybe Pretty (format globalOpts)
        jp = jsonPath globalOpts
    case cmd of
        Database DbList ->
            apiGet mgr rc "/api/v1/db" >>= output fmt jp
        Database (DbLoad name) ->
            -- Load endpoint takes no body; the empty object is ignored server-side.
            apiPost mgr rc ("/api/v1/db/" ++ T.unpack name ++ "/load") (object [])
                >>= outputLoad fmt jp
        Database (DbUnload name) ->
            apiPost mgr rc ("/api/v1/db/" ++ T.unpack name ++ "/unload") (object [])
                >>= outputStatus fmt jp "unload"
        Database (DbUpload args) ->
            executeUpload mgr rc fmt jp "/api/v1/db/upload" args
        Database (DbDelete name) ->
            apiDelete mgr rc ("/api/v1/db/" ++ T.unpack name) >>= output fmt jp
        Database (DbDeleteActivities args) ->
            apiPost mgr rc ("/api/v1/db/" ++ T.unpack (ddaDb args) ++ "/delete") (deleteSelectionBody args)
                >>= outputStatus fmt jp "delete"
        Database (DbCopy srcName newName) ->
            -- The copy endpoint takes no body (newName is a path capture); the
            -- empty object is ignored server-side.
            apiPost
                mgr
                rc
                ("/api/v1/db/" ++ T.unpack srcName ++ "/copy/" ++ T.unpack newName)
                (object [])
                >>= outputStatus fmt jp "copy"
        Database (DbRelinkMapping args) -> do
            readResult <- try (TIO.readFile (draMappingCsv args)) :: IO (Either IOException Text)
            case readResult of
                Left _ -> do
                    reportError $ "cannot read mapping file " ++ draMappingCsv args
                    exitFailure
                Right csv ->
                    apiPost
                        mgr
                        rc
                        ("/api/v1/db/" ++ T.unpack (draDb args) ++ "/relink")
                        (relinkMappingBody (draToDep args) csv)
                        >>= output fmt jp
        Database (DbExport args) ->
            executeRemoteExport mgr rc fmt jp args
        Method McList ->
            apiGet mgr rc "/api/v1/method-collections" >>= output fmt jp
        Method (McUpload args) ->
            executeUpload mgr rc fmt jp "/api/v1/method-collections/upload" args
        Method (McDelete name) ->
            apiDelete mgr rc ("/api/v1/method-collections/" ++ T.unpack name) >>= output fmt jp
        Method (McExport args) ->
            executeRemoteMethodExport mgr rc fmt jp args
        Methods ->
            apiGet mgr rc "/api/v1/methods" >>= output fmt jp
        Synonyms ->
            apiGet mgr rc "/api/v1/flow-synonyms" >>= output fmt jp
        CompartmentMappings ->
            apiGet mgr rc "/api/v1/compartment-mappings" >>= output fmt jp
        Units ->
            apiGet mgr rc "/api/v1/units" >>= output fmt jp
        Activity uuid -> do
            db <- resolveDbName mgr rc (dbName globalOpts)
            apiGet mgr rc (dbPath db ++ "/activity/" ++ T.unpack uuid) >>= output fmt jp
        Flow flowId Nothing -> do
            db <- resolveDbName mgr rc (dbName globalOpts)
            apiGet mgr rc (dbPath db ++ "/flow/" ++ T.unpack flowId) >>= output fmt jp
        Flow flowId (Just FlowActivities) -> do
            db <- resolveDbName mgr rc (dbName globalOpts)
            apiGet mgr rc (dbPath db ++ "/flow/" ++ T.unpack flowId ++ "/activities") >>= output fmt jp
        Inventory uuid -> do
            db <- resolveDbName mgr rc (dbName globalOpts)
            apiGet mgr rc (dbPath db ++ "/activity/" ++ T.unpack uuid ++ "/inventory") >>= output fmt jp
        SearchActivities opts -> do
            db <- resolveDbName mgr rc (dbName globalOpts)
            let qs =
                    buildQuery
                        [ ("name", T.unpack <$> searchName opts)
                        , ("geo", T.unpack <$> searchGeo opts)
                        , ("product", T.unpack <$> searchProduct opts)
                        , ("limit", show <$> searchLimit opts)
                        , ("offset", show <$> searchOffset opts)
                        ]
            apiGet mgr rc (dbPath db ++ "/activities" ++ qs) >>= output fmt jp
        SearchFlows opts -> do
            db <- resolveDbName mgr rc (dbName globalOpts)
            let qs =
                    buildQuery
                        [ ("q", T.unpack <$> searchQuery opts)
                        , ("lang", T.unpack <$> searchLang opts)
                        , ("limit", show <$> searchFlowsLimit opts)
                        , ("offset", show <$> searchFlowsOffset opts)
                        ]
            apiGet mgr rc (dbPath db ++ "/flows" ++ qs) >>= output fmt jp
        Impacts uuid lciaOpts -> do
            db <- resolveDbName mgr rc (dbName globalOpts)
            let methodIdText = lciaMethodId lciaOpts
            mCollection <- lookupMethodCollection mgr rc methodIdText
            case mCollection of
                Nothing -> reportError "Method not found in loaded collections" >> exitFailure
                Just col ->
                    apiGet
                        mgr
                        rc
                        ( dbPath db
                            ++ "/activity/"
                            ++ T.unpack uuid
                            ++ "/impacts/"
                            ++ T.unpack col
                            ++ "/"
                            ++ T.unpack methodIdText
                        )
                        >>= output fmt jp
        FlowMapping opts -> do
            db <- resolveDbName mgr rc (dbName globalOpts)
            let methodId = T.unpack (mappingMethodId opts)
            apiGet mgr rc (dbPath db ++ "/method/" ++ methodId ++ "/mapping") >>= output fmt jp
        Stop -> do
            result <- apiPost mgr rc "/api/v1/shutdown" (object [])
            case result of
                Right _ -> putStrLn $ "Server at " ++ rcBaseUrl rc ++ " stopped"
                Left err -> reportError err >> exitFailure

        -- Local-only commands should never reach here
        Server _ -> reportError "Server command is local-only" >> exitFailure
        DebugMatrices{} -> reportError "debug-matrices is local-only" >> exitFailure
        ExportMatrices{} -> reportError "export-matrices is local-only" >> exitFailure
        Repl -> reportError "repl should be handled in Main" >> exitFailure
        DumpOpenApi -> reportError "dump-openapi should be handled in Main" >> exitFailure
        DumpMcpTools -> reportError "dump-mcp-tools should be handled in Main" >> exitFailure

-- | Look up the collection name for a given method UUID via /api/v1/methods
lookupMethodCollection :: Manager -> RemoteConfig -> Text -> IO (Maybe Text)
lookupMethodCollection mgr rc methodId = do
    result <- apiGet mgr rc "/api/v1/methods"
    return $ either (const Nothing) (parseMaybe go) result
  where
    go :: Value -> Parser Text
    go = withArray "methods" $ \arr ->
        case mapMaybe (parseMaybe matchOne) (V.toList arr) of
            (c : _) -> pure c
            [] -> fail "method not found"
    matchOne :: Value -> Parser Text
    matchOne = withObject "method" $ \obj -> do
        uuid <- obj .: "msmId"
        col <- obj .: "msmCollection"
        if (uuid :: Text) == methodId then pure col else fail "no match"

-- | Auto-detect the single loaded database, or use the specified one
resolveDbName :: Manager -> RemoteConfig -> Maybe Text -> IO Text
resolveDbName _ _ (Just name) = return name
resolveDbName mgr rc Nothing = do
    result <- apiGet mgr rc "/api/v1/db"
    case result of
        Right val -> case extractLoadedDbNames val of
            [name] -> return name
            [] -> reportError "No databases loaded on the server" >> exitFailure
            names -> do
                reportError $
                    "Multiple databases loaded, use --db to select one: "
                        ++ unwords (map T.unpack names)
                exitFailure
        Left err -> reportError err >> exitFailure

-- | Extract names of loaded databases from the database list JSON
extractLoadedDbNames :: Value -> [Text]
extractLoadedDbNames = fromMaybe [] . parseMaybe go
  where
    go :: Value -> Parser [Text]
    go = withObject "resp" $ \obj -> do
        dbs <- obj .: "dlrDatabases"
        catMaybes <$> mapM getName dbs
    getName :: Value -> Parser (Maybe Text)
    getName = withObject "db" $ \db -> do
        status <- db .: "dsaStatus"
        name <- db .: "dsaName"
        return $ if (status :: Text) == "loaded" then Just name else Nothing

-- | Build a database-scoped API path
dbPath :: Text -> String
dbPath name = "/api/v1/db/" ++ T.unpack name

-- | JSON body for the delete-by-selection endpoint (field names match the API).
deleteSelectionBody :: DbDeleteArgs -> Value
deleteSelectionBody args =
    object
        [ "name" .= ddaName args
        , "location" .= ddaLocation args
        , "product" .= ddaProduct args
        , "classifications" .= classifications
        , "exact" .= ddaExact args
        , "keep" .= ddaKeep args
        , "extra" .= ddaExtra args
        , "ids" .= (if null (ddaIds args) then Nothing else Just (ddaIds args))
        ]
  where
    classifications = case (ddaClassSystem args, ddaClassValue args) of
        (Just sys, Just val) ->
            [object ["system" .= sys, "value" .= val, "exact" .= ddaExact args]]
        _ -> [] :: [Value]

{- | Body for POST /api/v1/db/{db}/relink with an inline alias mapping.
Keys mirror 'API.Types.RelinkRequest' after the @Stripped@ prefix transform
(@rmrDepDb@ → @depDb@, @rmrMappingCsv@ → @mappingCsv@); both are populated here,
which the handler reads as mapping mode (an empty @{}@ body is a plain relink).
-}
relinkMappingBody :: Text -> Text -> Value
relinkMappingBody depDb csv =
    object
        [ "depDb" .= depDb
        , "mappingCsv" .= csv
        ]

{- | Remote export: POST the target format and write the raw bytes the server
returns (serialization happens server-side) to @--out@. Approximation warnings
arrive percent-encoded in the @X-Volca-Export-Warnings@ header and are
reported, not fatal. Failures (bad format, db not loaded) surface loudly
rather than writing a partial/empty file.
-}
executeRemoteExport :: Manager -> RemoteConfig -> OutputFormat -> Maybe Text -> DbExportArgs -> IO ()
executeRemoteExport mgr rc fmt jp args = do
    resp <- apiPostRaw mgr rc ("/api/v1/db/" ++ T.unpack (deaDb args) ++ "/export") (object ["format" .= deaFormat args])
    case resp of
        -- A failed export (bad format, db not loaded, unexportable data) arrives
        -- as a non-2xx HTTP status, surfaced here as 'Left'.
        Left err -> reportError err >> exitFailure
        Right r -> do
            mapM_ (reportProgress Warning . T.unpack) (exportWarnings r)
            BL.writeFile (deaOut args) (responseBody r)
            output fmt jp (Right (object ["database" .= deaDb args, "format" .= deaFormat args, "out" .= deaOut args]))

{- | Remote counterpart of @method export@: same transport as the database
export, against the method-collections endpoint.
-}
executeRemoteMethodExport :: Manager -> RemoteConfig -> OutputFormat -> Maybe Text -> McExportArgs -> IO ()
executeRemoteMethodExport mgr rc fmt jp args = do
    resp <- apiPostRaw mgr rc ("/api/v1/method-collections/" ++ T.unpack (meaName args) ++ "/export") (object ["format" .= meaFormat args])
    case resp of
        Left err -> reportError err >> exitFailure
        Right r -> do
            mapM_ (reportProgress Warning . T.unpack) (exportWarnings r)
            BL.writeFile (meaOut args) (responseBody r)
            output fmt jp (Right (object ["collection" .= meaName args, "format" .= meaFormat args, "out" .= meaOut args]))

{- | Decode the @X-Volca-Export-Warnings@ response header: percent-decode, then
split on the newlines the server joined with. Absent or empty header = no
warnings.
-}
exportWarnings :: Response BL.ByteString -> [Text]
exportWarnings r =
    [ w
    | Just raw <- [lookup "X-Volca-Export-Warnings" (responseHeaders r)]
    , w <- T.splitOn "\n" (T.decodeUtf8 (urlDecode False raw))
    , not (T.null w)
    ]

-- | Build query string from optional parameters
buildQuery :: [(String, Maybe String)] -> String
buildQuery params =
    case [(k, v) | (k, Just v) <- params] of
        [] -> ""
        pairs -> "?" ++ intercalate "&" [k ++ "=" ++ urlEncode v | (k, v) <- pairs]
  where
    urlEncode = concatMap encodeChar
    encodeChar c
        | isAsciiUpper c = [c]
        | isAsciiLower c = [c]
        | isDigit c = [c]
        | c `elem` ("-_.~" :: String) = [c]
        | otherwise = '%' : showHex2 (fromEnum c)
    showHex2 n = [hexDigit (n `div` 16), hexDigit (n `mod` 16)]
    hexDigit n
        | n < 10 = toEnum (fromEnum '0' + n)
        | otherwise = toEnum (fromEnum 'A' + n - 10)

-- | Execute an upload command (database or method collection)
executeUpload :: Manager -> RemoteConfig -> OutputFormat -> Maybe Text -> String -> UploadArgs -> IO ()
executeUpload mgr rc fmt jp path args = apiUploadFile mgr rc path args >>= output fmt jp

{- | Stream a file to an upload endpoint as a raw octet-stream body, carrying the
display name and optional description as query parameters. The body is streamed
from disk in constant memory (no base64, no whole-file buffering).
-}
apiUploadFile :: Manager -> RemoteConfig -> String -> UploadArgs -> IO (Either String Value)
apiUploadFile mgr rc path args = do
    body <- fileRequestBody (uaFile args)
    let url = rcBaseUrl rc ++ path
        query =
            ("name", Just (T.encodeUtf8 (uaName args)))
                : [("description", Just (T.encodeUtf8 d)) | Just d <- [uaDescription args]]
    result <- try $ do
        req0 <- parseRequest url
        let req1 =
                setQueryString query $
                    req0
                        { Network.HTTP.Client.method = "POST"
                        , requestHeaders =
                            authHeaders rc ++ [("Content-Type", "application/octet-stream")] ++ requestHeaders req0
                        , requestBody = body
                        }
        httpLbs req1 mgr
    case result of
        Left e -> return $ Left (formatHttpError (rcBaseUrl rc) e)
        Right resp ->
            let status = statusCode (responseStatus resp)
                respBody = responseBody resp
             in if status >= 200 && status < 300
                    then return $ Right $ fromMaybe (object []) (decode respBody)
                    else return $ Left $ formatApiError status respBody

-- | Build a constant-memory streaming request body from a file on disk.
fileRequestBody :: FilePath -> IO RequestBody
fileRequestBody fp = do
    size <- withBinaryFile fp ReadMode hFileSize
    return $ RequestBodyStream (fromIntegral size) $ \needsPopper ->
        withBinaryFile fp ReadMode $ \h -> needsPopper (BS.hGetSome h 65536)

{- | Output a response whose body carries an in-band @{"success",..,"message"}@
status (the handlers return HTTP 200 even on failure, so a bare 'output' would
exit 0 on a failed copy/delete). Inspect the @success@ field and fail loudly
when it is false; otherwise render normally. Covers both 'ActivateResponse' and
'DeleteSelectionResponse', which share these keys after the @Stripped@ transform.
-}
outputStatus :: OutputFormat -> Maybe Text -> String -> Either String Value -> IO ()
outputStatus _ _ _ (Left err) = reportError err >> exitFailure
outputStatus fmt jp action (Right val) = case parseMaybe parseStatus val of
    Nothing -> reportError (action ++ ": malformed server response") >> exitFailure
    Just (False, msg) -> reportError (action ++ " failed: " ++ T.unpack msg) >> exitFailure
    Just (True, _) -> output fmt jp (Right val)
  where
    parseStatus :: Value -> Parser (Bool, Text)
    parseStatus = withObject "StatusResponse" $ \o -> (,) <$> o .: "success" <*> o .: "message"

{- | @database load@ returns HTTP 200 even on failure, with a bare
@{"error": …}@ body ('LoadDatabaseResponse'\'s @LoadFailed@) and no @success@
field for 'outputStatus' to check. Mirror 'outputStatus': fail loudly when that
key is present, so a failed remote load exits non-zero instead of printing the
error and returning success.
-}
outputLoad :: OutputFormat -> Maybe Text -> Either String Value -> IO ()
outputLoad _ _ (Left err) = reportError err >> exitFailure
outputLoad fmt jp (Right val) = case val of
    Object o
        | Just (String err) <- KM.lookup "error" o ->
            reportError ("load failed: " ++ T.unpack err) >> exitFailure
    _ -> output fmt jp (Right val)

-- | Format and output a result
output :: OutputFormat -> Maybe Text -> Either String Value -> IO ()
output _ _ (Left err) = reportError err >> exitFailure
output fmt _jp (Right val) = case fmt of
    JSON -> BSL.putStrLn $ encode val
    Pretty -> BSL.putStrLn $ encodePretty val
    Table -> putStr $ renderTable val
    CSV -> putStr $ renderCSV val

-- | Render a JSON value as an aligned text table
renderTable :: Value -> String
renderTable val =
    case findArray val of
        Just rows -> formatTable (extractTable rows)
        Nothing -> BSL.unpack (encodePretty val) ++ "\n" -- fallback for non-array

-- | Render a JSON value as CSV
renderCSV :: Value -> String
renderCSV val =
    case findArray val of
        Just rows ->
            let (headers, dataRows) = extractTable rows
             in unlines $ intercalate "," (map quote headers) : map (intercalate "," . map quote) dataRows
        Nothing -> BSL.unpack (encode val) ++ "\n"
  where
    quote s = "\"" ++ concatMap (\c -> if c == '"' then "\"\"" else [c]) s ++ "\""

-- | Find the first array in a JSON value (top-level or one level deep)
findArray :: Value -> Maybe [Value]
findArray (Array arr) = Just (V.toList arr)
findArray (Object obj) =
    -- Look for a single array field (e.g., databases, results, methods, items)
    case mapMaybe extractArr (KM.toList obj) of
        [(_, arr)] -> Just arr
        _ -> Nothing
  where
    extractArr (_, Array arr) = Just ((), V.toList arr)
    extractArr _ = Nothing
findArray _ = Nothing

-- | Extract headers and rows from a list of JSON objects
extractTable :: [Value] -> ([String], [[String]])
extractTable [] = ([], [])
extractTable rows@(Object first : _) =
    let keys = map fst (KM.toList first)
        headers = map Key.toString keys
        dataRows = map (rowValues keys) rows
     in (headers, dataRows)
extractTable rows = (["value"], map (\v -> [cellValue v]) rows)

rowValues :: [KM.Key] -> Value -> [String]
rowValues keys (Object obj) = map (\k -> cellValue (fromMaybe Null (KM.lookup k obj))) keys
rowValues _ v = [cellValue v]

-- | Convert a JSON value to a display string for table cells
cellValue :: Value -> String
cellValue (String t) = T.unpack t
cellValue (Number n) = let s = show n in if ".0" `isSuffixOf` s then take (length s - 2) s else s
cellValue (Bool True) = "yes"
cellValue (Bool False) = ""
cellValue Null = ""
cellValue v = BSL.unpack (encode v)

isSuffixOf :: String -> String -> Bool
isSuffixOf suffix str = drop (length str - length suffix) str == suffix

-- | Format headers + rows as an aligned table with separators
formatTable :: ([String], [[String]]) -> String
formatTable ([], _) = ""
formatTable (headers, rows) =
    let allRows = headers : rows
        widths = map (maximum . map length) (transpose (map (map (take maxColWidth)) allRows))
        padRow = zipWith (\w c -> take maxColWidth c ++ replicate (w - length (take maxColWidth c)) ' ') widths
        sep = intercalate "+" (map (\w -> replicate (w + 2) '-') widths)
        fmtRow r = "  " ++ intercalate " | " (padRow r)
     in unlines $ fmtRow headers : ("--" ++ sep) : map fmtRow rows
  where
    maxColWidth = 60

authHeaders :: RemoteConfig -> [(HeaderName, BS.ByteString)]
authHeaders rc = case rcAuth rc of
    Just pwd -> [("Authorization", "Bearer " <> C8.pack pwd)]
    Nothing -> []

-- | HTTP GET, POST, DELETE helpers
apiGet :: Manager -> RemoteConfig -> String -> IO (Either String Value)
apiGet mgr rc path = apiRequest mgr rc "GET" path Nothing

apiPost :: Manager -> RemoteConfig -> String -> Value -> IO (Either String Value)
apiPost mgr rc path body = apiRequest mgr rc "POST" path (Just body)

apiDelete :: Manager -> RemoteConfig -> String -> IO (Either String Value)
apiDelete mgr rc path = apiRequest mgr rc "DELETE" path Nothing

{- | POST a JSON body and return the raw response (bytes + headers), for
octet-stream endpoints like database export. Shares the error formatting of
'apiRequest' but skips its JSON decoding.
-}
apiPostRaw :: Manager -> RemoteConfig -> String -> Value -> IO (Either String (Response BL.ByteString))
apiPostRaw mgr rc path body = do
    result <- try $ do
        req0 <- parseRequest (rcBaseUrl rc ++ path)
        httpLbs
            req0
                { Network.HTTP.Client.method = "POST"
                , requestHeaders = authHeaders rc ++ [("Content-Type", "application/json")] ++ requestHeaders req0
                , requestBody = RequestBodyLBS (encode body)
                }
            mgr
    pure $ case result of
        Left e -> Left (formatHttpError (rcBaseUrl rc) e)
        Right resp ->
            let status = statusCode (responseStatus resp)
             in if status >= 200 && status < 300
                    then Right resp
                    else Left (formatApiError status (responseBody resp))

-- | Core HTTP request helper with error handling
apiRequest :: Manager -> RemoteConfig -> String -> String -> Maybe Value -> IO (Either String Value)
apiRequest mgr rc reqMethod path mBody = do
    let url = rcBaseUrl rc ++ path
    result <- try $ do
        req0 <- parseRequest url
        let req1 =
                req0
                    { Network.HTTP.Client.method = C8.pack reqMethod
                    , requestHeaders = authHeaders rc ++ contentHeaders ++ requestHeaders req0
                    }
            req2 = case mBody of
                Just body -> req1{requestBody = RequestBodyLBS (encode body)}
                Nothing -> req1
        httpLbs req2 mgr
    case result of
        Left e -> return $ Left (formatHttpError (rcBaseUrl rc) e)
        Right resp ->
            let status = statusCode (responseStatus resp)
                body = responseBody resp
             in if status >= 200 && status < 300
                    then return $ Right $ fromMaybe (object []) (decode body)
                    else return $ Left $ formatApiError status body
  where
    contentHeaders = case mBody of
        Just _ -> [("Content-Type", "application/json")]
        Nothing -> []

-- | Format HTTP exceptions into user-friendly messages
formatHttpError :: String -> HttpException -> String
formatHttpError baseUrl (HttpExceptionRequest _ (ConnectionFailure _)) =
    "Cannot connect to VoLCA server at "
        ++ baseUrl
        ++ "\nStart it with: volca --config volca.toml server"
formatHttpError _ (HttpExceptionRequest _ content) =
    "HTTP error: " ++ show content
formatHttpError _ (InvalidUrlException url reason) =
    "Invalid URL '" ++ url ++ "': " ++ reason

-- | Format API error responses
formatApiError :: Int -> BL.ByteString -> String
formatApiError 401 _ = "Authentication failed. Check --password or VOLCA_PASSWORD"
formatApiError 404 body = "Not found" ++ bodyDetail body
formatApiError status body = "Server error (HTTP " ++ show status ++ ")" ++ bodyDetail body

bodyDetail :: BL.ByteString -> String
bodyDetail body
    | BL.null body = ""
    | otherwise = ": " ++ BSL.unpack (BL.take 200 body)
