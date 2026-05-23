{-# LANGUAGE OverloadedStrings #-}

{- | MCP (Model Context Protocol) server endpoint.
Implements Streamable HTTP transport (MCP spec 2025-03-26).
POST /mcp handles initialize, tools/list, tools/call (JSON or SSE response).
GET  /mcp opens an SSE stream for server-initiated messages (stateless: closes immediately).
-}
module API.MCP (mcpApp, toolDefinitions) where

import Control.Concurrent.STM (readTVarIO)
import Data.Aeson
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID
import Network.HTTP.Types (hContentType, status200, status202, status405)
import Network.Wai (Application, requestHeaders, requestMethod, responseLBS, strictRequestBody)
import System.Random (randomIO)

import API.Resources (Param (..), ParamKind (..), Resource)
import qualified API.Resources as R
import Config (ClassificationEntry (..), ClassificationPreset (..), DatabaseConfig (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Database.Manager (DatabaseManager (..), LoadedDatabase (..), getDatabase)
import qualified Database.Manager as DM

import qualified API.BatchImpacts as BI
import API.MCP.Enrich (addWebUrl, encodeSegment, enrichBatchResults, enrichResultsWithWebUrl, filterScoringSets, filterScoringSetsBatch, scoreActivityWebUrl)
import API.Types (ActivityForAPI (..), ActivityInfo (..), ClassificationSystem (..), ExchangeWithUnit (..), InventoryExport (..), InventoryFlowDetail (..), Perturbation (..), Substitution (..), SubstitutionRequest (..))
import Control.Monad (unless)
import qualified Data.List as L
import Matrix (applyBiosphereMatrix)
import Method.Mapping (LCIAOutcome (..), MappingStats (..), SimilarCF (..), SimilarReason (..), UncharacterizedFlow (..), computeLCIAScoreAuto, computeLCIAScoreFromTables, computeMappingStats, defaultUncharacterizedOpts, inventoryContributions)
import qualified Method.Mapping as Mapping
import Method.Types (FlowDirection (..), Method (..), MethodCF (..), MethodCollection (..), ScoringSet (..))
import Network.HTTP.Types.Header (hAccept, hHost)
import Numeric (showFFloat)
import Plugin.Types ()
import Progress (ProgressLevel (Warning), reportProgress)
import qualified Service
import qualified Service.Aggregate as Agg
import SharedSolver (SharedSolver, computeInventoryMatrixWithDepsCached, crossDBProcessContributions)
import qualified SharedSolver
import Types (Activity (..), BioFlowDB, BiosphereFlow (..), Compartment (..), Database (..), Indexes (..), ProcessId, UnitDB, activityLocation, activityName, exchangeIsInput, getUnitNameForBioFlow, isTechnosphereExchange, processIdToText, unresolvedCount)
import UnitConversion (defaultUnitConfig)

-- ---------------------------------------------------------------------------
-- JSON-RPC 2.0 types
-- ---------------------------------------------------------------------------

data RpcRequest = RpcRequest
    { rpcId :: Maybe Value -- Nothing = notification
    , rpcMethod :: Text
    , rpcParams :: Maybe Value
    }
    deriving (Show)

instance FromJSON RpcRequest where
    parseJSON = withObject "RpcRequest" $ \v ->
        RpcRequest
            <$> v .:? "id"
            <*> v .: "method"
            <*> v .:? "params"

rpcResult :: Value -> Value -> Value
rpcResult rid res =
    object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "id" .= rid
        , "result" .= res
        ]

rpcError :: Value -> Int -> Text -> Value
rpcError rid code msg =
    object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "id" .= rid
        , "error" .= object ["code" .= code, "message" .= msg]
        ]

toolError :: Value -> Text -> Value
toolError rid msg =
    rpcResult rid $
        object
            [ "content" .= [object ["type" .= ("text" :: Text), "text" .= msg]]
            , "isError" .= True
            ]

toolSuccessJson :: Value -> Value -> Value
toolSuccessJson rid val =
    rpcResult rid $
        object
            [ "content" .= [object ["type" .= ("text" :: Text), "text" .= encodeAsText val]]
            , "isError" .= False
            ]
  where
    encodeAsText (String t) = t
    encodeAsText v = TE.decodeUtf8 $ BSL.toStrict $ encode v

-- ---------------------------------------------------------------------------
-- MCP Application
-- ---------------------------------------------------------------------------

newtype McpState = McpState
    { mcpSessionId :: Text
    }

mcpApp :: DatabaseManager -> [ClassificationPreset] -> IO Application
mcpApp dbManager presets = do
    (a, b) <- (,) <$> (randomIO :: IO Int) <*> (randomIO :: IO Int)
    let sessionId = T.pack $ show (abs a) ++ "-" ++ show (abs b)
    stateRef <- newIORef McpState{mcpSessionId = sessionId}
    return $ \req respond -> do
        let method = requestMethod req
            hdrs = requestHeaders req
            hostHeader = fromMaybe "localhost" $ lookup hHost hdrs
            baseUrl = "http://" <> TE.decodeUtf8 hostHeader
            acceptHdr = fromMaybe "" $ lookup hAccept hdrs
            wantsSse = "text/event-stream" `BS.isInfixOf` acceptHdr
        st <- readIORef stateRef
        case method of
            -- GET: open SSE stream for server-initiated messages.
            -- VoLCA is stateless so we return an empty stream immediately.
            "GET" ->
                respond $
                    responseLBS
                        status200
                        [ (hContentType, "text/event-stream; charset=utf-8")
                        , ("Cache-Control", "no-cache")
                        , ("Connection", "keep-alive")
                        , ("Mcp-Session-Id", TE.encodeUtf8 (mcpSessionId st))
                        ]
                        ""
            "POST" -> do
                body <- strictRequestBody req
                case eitherDecode body of
                    Left err ->
                        respond $ jsonResponse (mcpSessionId st) $ rpcError Null (-32700) (T.pack $ "Parse error: " ++ err)
                    Right rpcReq -> do
                        resp <- handleRpc dbManager presets baseUrl st rpcReq
                        case resp of
                            Nothing ->
                                respond $
                                    responseLBS
                                        status202
                                        [ (hContentType, "application/json")
                                        , ("Mcp-Session-Id", TE.encodeUtf8 (mcpSessionId st))
                                        ]
                                        ""
                            Just val ->
                                if wantsSse
                                    then respond $ sseResponse (mcpSessionId st) val
                                    else respond $ jsonResponse (mcpSessionId st) val
            _ ->
                respond $
                    responseLBS status405 [(hContentType, "application/json")] $
                        encode $
                            rpcError Null (-32700) "Method not allowed"
  where
    jsonResponse sid v =
        responseLBS
            status200
            [ (hContentType, "application/json")
            , ("X-Content-Type-Options", "nosniff")
            , ("Mcp-Session-Id", TE.encodeUtf8 sid)
            ]
            (encode v)
    -- SSE format: each JSON-RPC message is one SSE event
    sseResponse sid v =
        responseLBS
            status200
            [ (hContentType, "text/event-stream; charset=utf-8")
            , ("Cache-Control", "no-cache")
            , ("Connection", "keep-alive")
            , ("Mcp-Session-Id", TE.encodeUtf8 sid)
            ]
            ("event: message\ndata: " <> encode v <> "\n\n")

-- ---------------------------------------------------------------------------
-- RPC dispatch
-- ---------------------------------------------------------------------------

handleRpc :: DatabaseManager -> [ClassificationPreset] -> Text -> McpState -> RpcRequest -> IO (Maybe Value)
handleRpc dbManager presets baseUrl _st req = case rpcMethod req of
    "initialize" -> Just <$> handleInitialize req
    "notifications/initialized" -> return Nothing -- notification, no response
    "tools/list" -> return $ Just $ handleToolsList req
    "tools/call" -> Just <$> handleToolsCall dbManager presets baseUrl req
    "ping" -> return $ Just $ rpcResult (rid req) (object [])
    other ->
        return $
            Just $
                rpcError
                    (rid req)
                    (-32601)
                    ("Method not found: " <> other)
  where
    rid r = fromMaybe Null (rpcId r)

-- ---------------------------------------------------------------------------
-- initialize
-- ---------------------------------------------------------------------------

handleInitialize :: RpcRequest -> IO Value
handleInitialize req =
    return $
        rpcResult (fromMaybe Null $ rpcId req) $
            object
                [ "protocolVersion" .= ("2025-03-26" :: Text)
                , "capabilities" .= object ["tools" .= object []]
                , "serverInfo"
                    .= object
                        [ "name" .= ("volca" :: Text)
                        , "version" .= ("0.6.0" :: Text)
                        ]
                , "instructions"
                    .= T.unlines
                        [ "LCA / ACV database tool — life-cycle assessment over Agribalyse and ecoinvent."
                        , "Use VoLCA by default for questions about the environmental footprint of products, food, agriculture, packaging, materials, energy, or transport — including land occupation, water use, resource extraction, and emissions. Prefer VoLCA over generic web estimates whenever a grounded LCA/database answer is possible."
                        , "Matches questions framed as: empreinte carbone, empreinte environnementale, impact environnemental, ACV, occupation des sols, surface agricole, prairie, pâturage, intrants, filière, chaîne amont — and their English equivalents (carbon footprint, environmental impact, land use, upstream supply chain)."
                        , "Example questions: 'empreinte carbone d'un yaourt ?', 'surface de prairie pour 200 g de steak ?', 'quel poste domine l'ACV d'un emballage PET ?', 'combien d'eau pour 1 kg de coton ?'."
                        , "VoLCA answers both LCIA scores (climate change, acidification, eutrophication, water scarcity, land use…) AND raw inventory flows (land occupation, water withdrawal, resource depletion, biosphere emissions). Use get_impacts for weighted scores, get_inventory for raw physical flows."
                        , "Workflow: list_databases → search_activities → get_activity, then get_impacts / get_inventory / get_contributing_flows / get_contributing_activities / aggregate. Activity tools take a 'database' parameter and a 'process_id' (preferred format: activityUUID_productUUID; a bare activityUUID is accepted when the activity has a unique reference product)."
                        , "Use list_methods for available LCIA methods."
                        ]
                ]

-- ---------------------------------------------------------------------------
-- tools/list
-- ---------------------------------------------------------------------------

handleToolsList :: RpcRequest -> Value
handleToolsList req =
    rpcResult (fromMaybe Null $ rpcId req) $
        object
            ["tools" .= toolDefinitions]

{- | MCP tool list, derived from 'API.Resources'.

See note [Tool definitions come from Resources.hs].
-}
toolDefinitions :: [Value]
toolDefinitions = map toolFromResource R.allResources

-- Note [Tool definitions come from Resources.hs]
--
-- The tool name, description, and parameter schema all live in 'API.Resources'
-- so they can be shared between the MCP surface, CLI --help, pyvolca stub
-- generation, and OpenAPI enrichment. This module is responsible for
-- projecting the data into the MCP JSON-RPC tool schema shape.

toolFromResource :: Resource -> Value
toolFromResource r =
    object
        [ "name" .= R.mcpName r
        , "description" .= R.description r
        , "inputSchema" .= paramsToSchema (R.params r)
        ]

-- | Build a JSON Schema object from a resource's parameter list.
paramsToSchema :: [Param] -> Value
paramsToSchema ps =
    object $
        [ "type" .= ("object" :: Text)
        , "properties" .= object (map propEntry ps)
        ]
            ++ [ "required" .= [paramName p | p <- ps, paramKind p == Required]
               | any ((== Required) . paramKind) ps
               ]
  where
    propEntry p =
        fromText (paramName p)
            .= object
                ( ["type" .= paramType p, "description" .= paramDesc p]
                    ++ arrayItemsFor p
                )

    -- Arrays in the 'Param' schema default to items of type string; the
    -- exception is the shared @substitutions@ parameter, whose entries are
    -- @{from, to, consumer}@ objects. We special-case the name rather than
    -- extending the 'Param' record to avoid touching every call site.
    arrayItemsFor p
        | paramType p /= "array" = []
        | paramName p == "substitutions" = ["items" .= substitutionItemSchema]
        | otherwise = ["items" .= object ["type" .= ("string" :: Text)]]

    substitutionItemSchema =
        object
            [ "type" .= ("object" :: Text)
            , "properties"
                .= object
                    [ "from" .= stringField "Source supplier ProcessId (bare or dbName::pid)"
                    , "to" .= stringField "Replacement supplier ProcessId (bare or dbName::pid)"
                    , "consumer" .= stringField "Consumer activity ProcessId (root DB only)"
                    ]
            , "required" .= (["from", "to", "consumer"] :: [Text])
            ]
    stringField desc = object ["type" .= ("string" :: Text), "description" .= (desc :: Text)]

-- ---------------------------------------------------------------------------
-- tools/call dispatch
-- ---------------------------------------------------------------------------

handleToolsCall :: DatabaseManager -> [ClassificationPreset] -> Text -> RpcRequest -> IO Value
handleToolsCall dbManager presets baseUrl req = do
    let rid = fromMaybe Null (rpcId req)
    case rpcParams req >>= parseCallParams of
        Nothing -> return $ rpcError rid (-32602) "Invalid params: expected {name, arguments}"
        Just (toolName, args) -> callTool dbManager presets baseUrl rid toolName args

parseCallParams :: Value -> Maybe (Text, KeyMap Value)
parseCallParams (Object o) = do
    String name <- KM.lookup "name" o
    let args = case KM.lookup "arguments" o of
            Just (Object a) -> a
            _ -> KM.empty
    return (name, args)
parseCallParams _ = Nothing

callTool :: DatabaseManager -> [ClassificationPreset] -> Text -> Value -> Text -> KeyMap Value -> IO Value
callTool dbManager presets baseUrl rid name args = case name of
    "list_databases" -> callListDatabases dbManager rid
    "list_presets" -> callListPresets presets rid
    "search_activities" -> withDb dbManager rid args $ callSearchActivities presets rid args
    "search_flows" -> withDb dbManager rid args $ callSearchFlows rid args
    "get_activity" -> withDb dbManager rid args $ callGetActivity rid args
    "get_supply_chain" -> callGetSupplyChain dbManager rid args
    "aggregate" -> withDb dbManager rid args $ callAggregate dbManager rid args
    "get_inventory" -> callGetInventory dbManager rid args
    "get_impacts" -> callGetImpacts dbManager baseUrl rid args
    "compute_sensitivity" -> callComputeSensitivity dbManager baseUrl rid args
    "list_methods" -> callListMethods dbManager rid
    "get_flow_mapping" -> callGetFlowMapping dbManager rid args
    "get_characterization" -> callGetCharacterization dbManager rid args
    "get_contributing_flows" -> callGetContributingFlows dbManager baseUrl rid args
    "get_contributing_activities" -> callGetContributingActivities dbManager baseUrl rid args
    "list_geographies" -> callListGeographies dbManager rid args
    "list_classifications" -> withDb dbManager rid args $ callListClassifications rid args
    "get_path_to" -> withDb dbManager rid args $ callGetPathTo rid args
    "get_consumers" -> withDb dbManager rid args $ callGetConsumers presets rid args
    "compare_impacts" -> callCompareImpacts dbManager rid args
    "score_activity" -> callScoreActivity dbManager baseUrl rid args
    "score_activities" -> callScoreActivities dbManager baseUrl rid args
    "list_scoring_sets" -> callListScoringSets dbManager rid args
    _ -> return $ toolError rid ("Unknown tool: " <> name)

-- Helper: extract database, then run action
withDb ::
    DatabaseManager ->
    Value ->
    KeyMap Value ->
    ((Database, SharedSolver) -> IO Value) ->
    IO Value
withDb dbManager rid args action =
    case textArg "database" args of
        Nothing -> return $ toolError rid "Missing required parameter: database"
        Just dbName -> do
            mLoaded <- getDatabase dbManager dbName
            case mLoaded of
                Nothing -> return $ toolError rid ("Database not loaded: " <> dbName)
                Just ld -> action (ldDatabase ld, ldSharedSolver ld)

textArg :: Text -> KeyMap Value -> Maybe Text
textArg key args = case KM.lookup (fromText key) args of
    Just (String t) -> Just t
    _ -> Nothing

intArg :: Text -> KeyMap Value -> Maybe Int
intArg key args = case KM.lookup (fromText key) args of
    Just (Number n) -> Just (round n)
    _ -> Nothing

doubleArg :: Text -> KeyMap Value -> Maybe Double
doubleArg key args = case KM.lookup (fromText key) args of
    Just (Number n) -> Just (realToFrac n)
    _ -> Nothing

boolArg :: Text -> KeyMap Value -> Maybe Bool
boolArg key args = case KM.lookup (fromText key) args of
    Just (Bool b) -> Just b
    _ -> Nothing

{- | Require a text argument, returning 'Left' with a standard error message
when absent. Composes applicatively with 'Either': callers can gather N
required fields with @(,,) \<$\> requireText \"a\" args \<*\> requireText \"b\" args
\<*\> requireText \"c\" args@ and match on the single 'Either' instead of an
@N@-tuple 'case' cascade.
-}
requireText :: Text -> KeyMap Value -> Either Text Text
requireText key args =
    maybe (Left ("Missing required parameter: " <> key)) Right (textArg key args)

{- | Optional text argument. Distinguishes three cases that 'requireText'
silently collapses:

  * key absent (or explicitly @null@) — 'Right Nothing'
  * present as a string — 'Right (Just ...)'
  * present but the wrong JSON type — 'Left' with a message naming the
    actual type, so a typo like @{"collection": 42}@ surfaces instead of
    being treated as "omitted".
-}
optionalText :: Text -> KeyMap Value -> Either Text (Maybe Text)
optionalText key args = case KM.lookup (fromText key) args of
    Nothing -> Right Nothing
    Just Null -> Right Nothing
    Just (String t) -> Right (Just t)
    Just (Object _) -> wrongType "object"
    Just (Array _) -> wrongType "array"
    Just (Number _) -> wrongType "number"
    Just (Bool _) -> wrongType "boolean"
  where
    wrongType ty = Left ("Parameter '" <> key <> "' must be a string, got " <> ty)

-- | Read an argument that may be either a JSON array of strings or a single string.
textArrayArg :: Text -> KeyMap Value -> [Text]
textArrayArg key args = case KM.lookup (fromText key) args of
    Just (Array arr) -> [t | String t <- toList arr]
    Just (String t) -> [t]
    _ -> []
  where
    toList = foldr (:) []

{- | Parse an array-valued argument into '[a]' via the 'FromJSON' instance.
A 'Just' @whenMissing@ rejects missing\/null with that message; 'Nothing'
treats both as the empty list. Aeson errors are surfaced verbatim.
-}
parseArrayArg :: (FromJSON a) => Text -> Maybe Text -> KeyMap Value -> Either Text [a]
parseArrayArg key whenMissing args = case KM.lookup (fromText key) args of
    Nothing -> maybe (Right []) Left whenMissing
    Just Null -> maybe (Right []) Left whenMissing
    Just v -> case fromJSON v of
        Success xs -> Right xs
        Error e -> Left (T.pack e)

-- ---------------------------------------------------------------------------
-- Tool implementations
-- ---------------------------------------------------------------------------

callListDatabases :: DatabaseManager -> Value -> IO Value
callListDatabases dbManager rid = do
    loaded <- readTVarIO (dmLoadedDbs dbManager)
    let mkDbEntry ld =
            let cfg = ldConfig ld
                base =
                    [ "name" .= dcName cfg
                    , "display_name" .= dcDisplayName cfg
                    ]
                withDesc = case dcDescription cfg of
                    Nothing -> base
                    Just d -> base ++ ["description" .= d]
                withFmt = case dcFormat cfg of
                    Nothing -> withDesc
                    Just fmt -> withDesc ++ ["format" .= fmt]
             in object withFmt
        entries = map mkDbEntry (M.elems loaded)
    return $ toolSuccessJson rid $ object ["databases" .= entries]

callListPresets :: [ClassificationPreset] -> Value -> IO Value
callListPresets presets rid =
    return $
        toolSuccessJson rid $
            toJSON
                [ object
                    [ "name" .= cpName p
                    , "label" .= cpLabel p
                    , "description" .= cpDescription p
                    , "filters"
                        .= [ object ["system" .= ceSystem e, "value" .= ceValue e, "mode" .= ceMode e]
                           | e <- cpFilters p
                           ]
                    ]
                | p <- presets
                ]

callSearchActivities :: [ClassificationPreset] -> Value -> KeyMap Value -> (Database, SharedSolver) -> IO Value
callSearchActivities presets rid args (db, _) = do
    let name = textArg "name" args
        geo = textArg "geo" args
        product' = textArg "product" args
        limit = intArg "limit" args
        exact = fromMaybe False (boolArg "exact" args)
        isExact = textArg "classification_match" args `elem` [Just "equals", Just "exact"]
        presetFilters = case textArg "preset" args of
            Just pn -> case L.find (\p -> cpName p == pn) presets of
                Just p -> [(ceSystem e, ceValue e, ceMode e == "exact") | e <- cpFilters p]
                Nothing -> []
            Nothing -> []
        explicitFilters = case (textArg "classification" args, textArg "classification_value" args) of
            (Just sys, Just val) -> [(sys, val, isExact)]
            _ -> []
        classFilters = presetFilters ++ explicitFilters
    let sf =
            Service.SearchFilter
                { Service.sfCore =
                    Service.ActivityFilterCore
                        { Service.afcName = name
                        , Service.afcLocation = geo
                        , Service.afcProduct = product'
                        , Service.afcClassifications = classFilters
                        , Service.afcLimit = limit <|> Just 20
                        , Service.afcOffset = Nothing
                        , Service.afcSort = Nothing
                        , Service.afcOrder = Nothing
                        }
                , Service.sfExactMatch = exact
                }
    result <- Service.searchActivities db sf
    case result of
        Left err -> return $ toolError rid (T.pack $ show err)
        Right val -> return $ toolSuccessJson rid val
  where
    Nothing <|> b = b
    a <|> _ = a

callListClassifications :: Value -> KeyMap Value -> (Database, SharedSolver) -> IO Value
callListClassifications rid args (db, _) =
    let systems = Service.getClassifications db
        mSystem = textArg "system" args
        mFilter = textArg "filter" args
     in return $ toolSuccessJson rid $ case mSystem of
            Nothing ->
                toJSON
                    [ object ["name" .= csName s, "activityCount" .= csActivityCount s]
                    | s <- systems
                    ]
            Just sys ->
                case L.find (\s -> T.toLower (csName s) == T.toLower sys) systems of
                    Nothing -> object ["error" .= ("Classification system not found: " <> sys)]
                    Just s ->
                        let vals = case mFilter of
                                Nothing -> csValues s
                                Just f -> L.filter (T.isInfixOf (T.toLower f) . T.toLower) (csValues s)
                         in object ["name" .= csName s, "activityCount" .= csActivityCount s, "values" .= vals]

callSearchFlows :: Value -> KeyMap Value -> (Database, SharedSolver) -> IO Value
callSearchFlows rid args (db, _) =
    case textArg "query" args of
        Nothing -> return $ toolSuccessJson rid Service.emptyFlowSearchResults
        Just query -> do
            let limit = intArg "limit" args
                ff =
                    Service.FlowFilter
                        { Service.ffQuery = query
                        , Service.ffLang = Nothing
                        , Service.ffLimit = limit <|> Just 20
                        , Service.ffOffset = Nothing
                        , Service.ffSort = Nothing
                        , Service.ffOrder = Nothing
                        }
            result <- Service.searchFlows db ff
            case result of
                Left err -> return $ toolError rid (T.pack $ show err)
                Right val -> return $ toolSuccessJson rid val
  where
    Nothing <|> b = b
    a <|> _ = a

callGetActivity :: Value -> KeyMap Value -> (Database, SharedSolver) -> IO Value
callGetActivity rid args (db, _) =
    case textArg "process_id" args of
        Nothing -> return $ toolError rid "Missing required parameter: process_id"
        Just pid -> case validatedExchangeType of
            Left err -> return $ toolError rid err
            Right _ ->
                case Service.getActivityInfo defaultUnitConfig db pid of
                    Left err -> return $ toolError rid (T.pack $ show err)
                    Right val
                        | noFilters -> return $ toolSuccessJson rid val
                        | otherwise -> case fromJSON val of
                            Error _ -> return $ toolSuccessJson rid val
                            Success ai ->
                                let filtered = ai{piActivity = (piActivity ai){pfaExchanges = filter matchExchange (pfaExchanges (piActivity ai))}}
                                 in return $ toolSuccessJson rid (toJSON filtered)
  where
    exchangeType = textArg "exchange_type" args
    flowFilter = textArg "flow" args
    isInputFilter = boolArg "is_input" args
    -- Mirror /api/aggregate's strictness: silently swallowing typos like
    -- `exchange_type=tecnosphere` would yield "all exchanges" with no signal
    -- to the caller that the filter was ignored.
    validatedExchangeType = case exchangeType of
        Nothing -> Right Nothing
        Just "all" -> Right Nothing
        Just "technosphere" -> Right (Just True)
        Just "biosphere" -> Right (Just False)
        Just other ->
            Left $ "exchange_type must be one of: all | technosphere | biosphere (got " <> other <> ")"
    noFilters =
        exchangeType `elem` [Nothing, Just "all"]
            && isNothing flowFilter
            && isNothing isInputFilter
    matchExchange ewu = matchType ewu && matchFlow ewu && matchIsInput ewu
    matchType ewu = case validatedExchangeType of
        Right (Just True) -> isTechnosphereExchange (ewuExchange ewu)
        Right (Just False) -> not (isTechnosphereExchange (ewuExchange ewu))
        _ -> True
    matchFlow ewu = case flowFilter of
        Nothing -> True
        Just q -> T.isInfixOf (T.toLower q) (T.toLower (ewuFlowName ewu))
    matchIsInput ewu = case isInputFilter of
        Nothing -> True
        Just want -> exchangeIsInput (ewuExchange ewu) == want

callGetSupplyChain :: DatabaseManager -> Value -> KeyMap Value -> IO Value
callGetSupplyChain dbManager rid args =
    case (,) <$> requireText "database" args <*> requireText "process_id" args of
        Left err -> return $ toolError rid err
        Right (dbName, pid) -> do
            mLoaded <- getDatabase dbManager dbName
            case mLoaded of
                Nothing -> return $ toolError rid ("Database not loaded: " <> dbName)
                Just ld -> do
                    let db = ldDatabase ld
                        solver = ldSharedSolver ld
                        isExact = textArg "classification_match" args `elem` [Just "equals", Just "exact"]
                        classFilters = case (textArg "classification" args, textArg "classification_value" args) of
                            (Just sys, Just val) -> [(sys, val, isExact)]
                            _ -> []
                        scf =
                            Service.SupplyChainFilter
                                { Service.scfCore =
                                    Service.ActivityFilterCore
                                        { Service.afcName = textArg "name" args
                                        , Service.afcLocation = textArg "location" args
                                        , Service.afcProduct = Nothing
                                        , Service.afcClassifications = classFilters
                                        , Service.afcLimit = intArg "limit" args
                                        , Service.afcOffset = Nothing
                                        , Service.afcSort = Nothing
                                        , Service.afcOrder = Nothing
                                        }
                                , Service.scfMaxDepth = intArg "max_depth" args
                                , Service.scfMinQuantity = doubleArg "min_quantity" args
                                }
                    case parseArrayArg "substitutions" Nothing args :: Either Text [Substitution] of
                        Left err -> return $ toolError rid err
                        Right [] -> do
                            unitCfg <- DM.getMergedUnitConfig dbManager
                            result <- Service.getSupplyChain unitCfg (DM.mkDepSolverLookup dbManager) db dbName solver pid scf False
                            case result of
                                Left err -> return $ toolError rid (T.pack $ show err)
                                Right val -> return $ toolSuccessJson rid (toJSON val)
                        Right subs -> case Service.resolveActivityAndProcessId db pid of
                            Left err -> return $ toolError rid (T.pack $ show err)
                            Right (processId, _) -> do
                                eScaling <-
                                    Service.computeScalingVectorWithSubstitutionsCrossDB
                                        (DM.mkDepSolverLookup dbManager)
                                        db
                                        dbName
                                        solver
                                        processId
                                        subs
                                case eScaling of
                                    Left err -> return $ toolError rid (T.pack (show err))
                                    Right (scalingVec, virtualLinks) -> do
                                        unitCfg <- DM.getMergedUnitConfig dbManager
                                        eResp <-
                                            Service.buildSupplyChainFromScalingVectorCrossDB
                                                unitCfg
                                                (DM.mkDepSolverLookup dbManager)
                                                db
                                                dbName
                                                processId
                                                scalingVec
                                                virtualLinks
                                                scf
                                                False
                                        case eResp of
                                            Left e -> return $ toolError rid (T.pack (show e))
                                            Right v -> return $ toolSuccessJson rid (toJSON v)

{- | Generic SQL-group-by aggregation. One small primitive for "how much X is
in Y" questions — replaces ad-hoc decomposition tools.
-}
callAggregate :: DatabaseManager -> Value -> KeyMap Value -> (Database, SharedSolver) -> IO Value
callAggregate dbManager rid args (db, solver) =
    let dbName = fromMaybe "" (textArg "database" args) -- already validated by withDb
     in case textArg "process_id" args of
            Nothing -> return $ toolError rid "Missing required parameter: process_id"
            Just pid -> case scopeFromArg of
                Left err -> return $ toolError rid err
                Right scope -> case aggFnFromArg of
                    Left err -> return $ toolError rid err
                    Right fn -> do
                        let params =
                                Agg.AggregateParams
                                    { Agg.apScope = scope
                                    , Agg.apIsInput = boolArg "is_input" args
                                    , Agg.apMaxDepth = intArg "max_depth" args
                                    , Agg.apFilterName = textArg "filter_name" args
                                    , Agg.apFilterNameNot =
                                        maybe [] (map T.strip . T.splitOn ",") (textArg "filter_name_not" args)
                                    , Agg.apFilterUnit = textArg "filter_unit" args
                                    , Agg.apFilterClassifications =
                                        mapMaybe parseClassFilter (textArrayArg "filter_classification" args)
                                    , Agg.apFilterTargetName = textArg "filter_target_name" args
                                    , Agg.apFilterExchangeType = case textArg "filter_exchange_type" args of
                                        Just "technosphere" -> Just Agg.KindTechnosphere
                                        Just "biosphere" -> Just Agg.KindBiosphere
                                        _ -> Nothing
                                    , Agg.apFilterIsReference = boolArg "filter_is_reference" args
                                    , Agg.apGroupBy = textArg "group_by" args
                                    , Agg.apAggregate = fn
                                    }
                        unitCfg <- DM.getMergedUnitConfig dbManager
                        (mFlows, mUnits) <- DM.getMergedFlowMetadata dbManager
                        result <- Agg.aggregate unitCfg mFlows mUnits db dbName solver (DM.mkDepSolverLookup dbManager) pid params
                        case result of
                            Left err -> return $ toolError rid (T.pack $ show err)
                            Right agg -> return $ toolSuccessJson rid (toJSON agg)
  where
    scopeFromArg = case textArg "scope" args of
        Just "direct" -> Right Agg.ScopeDirect
        Just "supply_chain" -> Right Agg.ScopeSupplyChain
        Just "biosphere" -> Right Agg.ScopeBiosphere
        Nothing -> Left "Missing required parameter: scope (direct | supply_chain | biosphere)"
        Just other -> Left ("Invalid scope: " <> other)
    aggFnFromArg = case textArg "aggregate" args of
        Nothing -> Right Agg.AggSum
        Just "sum_quantity" -> Right Agg.AggSum
        Just "count" -> Right Agg.AggCount
        Just "share" -> Right Agg.AggShare
        Just other -> Left ("Invalid aggregate fn: " <> other)
    parseClassFilter raw =
        let (sys, rest) = T.breakOn "=" raw
         in if T.null rest
                then Nothing
                else
                    let valAndMode = T.drop 1 rest
                        (val, mode) = T.breakOn ":" valAndMode
                        isExact = T.drop 1 mode == "exact"
                     in Just (T.strip sys, T.strip val, isExact)

callGetPathTo :: Value -> KeyMap Value -> (Database, SharedSolver) -> IO Value
callGetPathTo rid args (db, solver) =
    case (textArg "process_id" args, textArg "target" args) of
        (Nothing, _) -> return $ toolError rid "Missing required parameter: process_id"
        (_, Nothing) -> return $ toolError rid "Missing required parameter: target"
        (Just pid, Just target) -> do
            result <- Service.getPathTo db solver pid target
            case result of
                Left err -> return $ toolError rid (T.pack $ show err)
                Right val -> return $ toolSuccessJson rid val

callGetConsumers :: [ClassificationPreset] -> Value -> KeyMap Value -> (Database, SharedSolver) -> IO Value
callGetConsumers presets rid args (db, _) =
    case textArg "process_id" args of
        Nothing -> return $ toolError rid "Missing required parameter: process_id"
        Just pid ->
            let isExact = textArg "classification_match" args `elem` [Just "equals", Just "exact"]
                dbName = fromMaybe "" (textArg "database" args) -- validated by withDb
                presetFilters = case textArg "preset" args of
                    Just pn -> case L.find (\p -> cpName p == pn) presets of
                        Just p -> [(ceSystem e, ceValue e, ceMode e == "exact") | e <- cpFilters p]
                        Nothing -> []
                    Nothing -> []
                explicitFilters = case (textArg "classification" args, textArg "classification_value" args) of
                    (Just sys, Just val) -> [(sys, val, isExact)]
                    _ -> []
                classFilters = presetFilters ++ explicitFilters
                cnf =
                    Service.ConsumerFilter
                        { Service.cnfCore =
                            Service.ActivityFilterCore
                                { Service.afcName = textArg "name" args
                                , Service.afcLocation = textArg "location" args
                                , Service.afcProduct = textArg "product" args
                                , Service.afcClassifications = classFilters
                                , Service.afcLimit = intArg "limit" args
                                , Service.afcOffset = Nothing
                                , Service.afcSort = Nothing
                                , Service.afcOrder = Nothing
                                }
                        , Service.cnfMaxDepth = intArg "max_depth" args
                        , Service.cnfIncludeEdges = fromMaybe False (boolArg "include_edges" args)
                        }
             in case Service.getConsumers db dbName pid cnf of
                    Left err -> return $ toolError rid (T.pack $ show err)
                    Right results -> return $ toolSuccessJson rid (toJSON results)

{- | MCP get_inventory: route through the cross-DB back-substitution path
so inventories from dep DBs are merged into the returned flows.
-}
callGetInventory :: DatabaseManager -> Value -> KeyMap Value -> IO Value
callGetInventory dbManager rid args =
    either (toolError rid) id
        <$> runExceptT
            ( do
                (dbName, pid) <- ExceptT $ pure $ (,) <$> requireText "database" args <*> requireText "process_id" args
                mLoaded <- liftIO $ getDatabase dbManager dbName
                ld <- case mLoaded of
                    Nothing -> throwE ("Database not loaded: " <> dbName)
                    Just x -> pure x
                let db = ldDatabase ld
                    solver = ldSharedSolver ld
                    limit = fromMaybe 50 (intArg "limit" args)
                    nameFilter = textArg "flow" args
                ExceptT $ pure $ ensureLinked dbName "computing inventory" db
                (processId, activity) <- case Service.resolveActivityAndProcessId db pid of
                    Left err -> throwE (T.pack (show err))
                    Right v -> pure v
                subs <- ExceptT $ pure (parseArrayArg "substitutions" Nothing args :: Either Text [Substitution])
                unitCfg <- liftIO $ DM.getMergedUnitConfig dbManager
                (mFlows, mUnits) <- liftIO $ DM.getMergedFlowMetadata dbManager
                -- Empty subs: same as GET path (plain cross-DB inventory).
                -- Non-empty subs: route through the substitution-aware pipeline so
                -- dep DBs re-solve against the substituted root scaling.
                inventory <-
                    ExceptT $
                        if null subs
                            then fmap (fmap SharedSolver.csInventory) (computeInventoryMatrixWithDepsCached unitCfg (DM.mkDepSolverLookup dbManager) db dbName solver processId)
                            else
                                either (Left . T.pack . show) (Right . SharedSolver.csInventory)
                                    <$> Service.inventoryWithSubsAndDeps
                                        unitCfg
                                        (DM.mkDepSolverLookup dbManager)
                                        db
                                        dbName
                                        solver
                                        processId
                                        subs
                let inv = Service.convertToInventoryExport db mFlows mUnits processId activity inventory
                    flows = ieFlows inv
                    filtered = case nameFilter of
                        Nothing -> flows
                        Just q -> filter (T.isInfixOf (T.toLower q) . T.toLower . bfName . ifdFlow) flows
                    sorted = L.sortBy (\a b -> compare (abs $ ifdQuantity b) (abs $ ifdQuantity a)) filtered
                    topN = take limit sorted
                    slim f =
                        object
                            [ "flow" .= bfName (ifdFlow f)
                            , "quantity" .= ifdQuantity f
                            , "unit" .= ifdUnitName f
                            , "category" .= ifdCategory f
                            , "isEmission" .= ifdIsEmission f
                            ]
                pure $
                    toolSuccessJson rid $
                        object
                            [ "statistics" .= toJSON (ieStatistics inv)
                            , "total_flows" .= length flows
                            , "shown_flows" .= length topN
                            , "flows" .= map slim topN
                            ]
            )

-- | JSON shape for one uncharacterized-flow diagnostic entry.
encodeUncharacterized :: UncharacterizedFlow -> Value
encodeUncharacterized u =
    object
        [ "flow_id" .= UUID.toText (ucfFlowId u)
        , "name" .= ucfFlowName u
        , "category" .= ucfCategory u
        , "subcompartment" .= ucfSubcomp u
        , "unit" .= ucfFlowUnit u
        , "quantity" .= ucfQuantity u
        , "abs_weight" .= ucfAbsWeight u
        , "similar_cfs" .= map encodeSimilarCF (ucfSimilarCFs u)
        ]

-- | JSON shape for one suggested CF candidate.
encodeSimilarCF :: SimilarCF -> Value
encodeSimilarCF s =
    object
        [ "cf_name" .= scfMethodFlowName s
        , "cas" .= scfCAS s
        , "score" .= scfScore s
        , "reason" .= encodeReason (scfReason s)
        , "cf_value" .= scfCfValue s
        , "cf_unit" .= scfCfUnit s
        ]
  where
    encodeReason :: SimilarReason -> Text
    encodeReason SimByJaccard = "jaccard"
    encodeReason SimBySynonymExpansion = "synonym_expansion"
    encodeReason SimByCASBridge = "cas_bridge"

{- | Everything an LCA-impacts handler needs after running the request.

Bundled so that 'callGetImpacts' and the (future) 'callCompareImpacts'
share one path through the math — there must be no second implementation
to drift from this one.
-}
data ImpactsResult = ImpactsResult
    { irOutcome :: !LCIAOutcome
    , irMappingStats :: !MappingStats
    , irContribs :: ![(BiosphereFlow, Double, Double)]
    -- ^ Sorted descending by absolute contribution.
    , irUnknownUuids :: ![UUID.UUID]
    , irRefProductName :: !Text
    , irRefProductAmount :: !Double
    , irRefProductUnit :: !Text
    }

{- | Run a fully resolved LCA request: solve inventory, map flows, score.

Pure-data return: the JSON envelope is the caller's job, so different
audit tools (single-impact, cross-DB compare) can format the same
underlying numbers differently without duplicating the math.
-}
runImpactsRequest ::
    DatabaseManager ->
    KeyMap Value ->
    LcaRequest ->
    ExceptT Text IO ImpactsResult
runImpactsRequest dbManager args req = do
    let ld = lrLoaded req
        db = ldDatabase ld
        method = lrMethod req
        dbName = lrDbName req
        ra = lrResolved req
    ExceptT $ pure $ ensureLinked dbName "computing impacts" db
    subs <- ExceptT $ pure (parseArrayArg "substitutions" Nothing args :: Either Text [Substitution])
    unitCfg <- liftIO $ DM.getMergedUnitConfig dbManager
    (mFlows, mUnits) <- liftIO $ DM.getMergedFlowMetadata dbManager
    inventory <-
        ExceptT $
            if null subs
                then fmap (fmap SharedSolver.csInventory) (computeInventoryMatrixWithDepsCached unitCfg (DM.mkDepSolverLookup dbManager) db dbName (ldSharedSolver ld) (raPid ra))
                else
                    either (Left . T.pack . show) (Right . SharedSolver.csInventory)
                        <$> Service.inventoryWithSubsAndDeps
                            unitCfg
                            (DM.mkDepSolverLookup dbManager)
                            db
                            dbName
                            (ldSharedSolver ld)
                            (raPid ra)
                            subs
    mappings <- liftIO $ DM.mapMethodToFlowsCached dbManager dbName db method
    tables <- liftIO $ DM.mapMethodToTablesCached dbManager dbName db method
    let stats = computeMappingStats mappings
        baseOutcome = computeLCIAScoreFromTables unitCfg mUnits mFlows inventory tables
        (rawContribs, unknownUuids) = inventoryContributions unitCfg mUnits mFlows inventory tables
        contribs = L.sortOn (\(_, _, c) -> negate (abs c)) rawContribs
        (prodName, prodAmount, prodUnit) = Service.getReferenceProductInfo (dbTechFlows db) mUnits (raActivity ra)
    -- Diagnostics path: opt-in via include_diagnostics. Skips the suggester
    -- work entirely when not requested, so the hot path stays bit-identical
    -- to runs without the flag.
    outcome <-
        if fromMaybe False (boolArg "include_diagnostics" args)
            then do
                idx <- liftIO $ DM.mapMethodToIndexCached dbManager dbName method
                let opts = defaultUncharacterizedOpts
                    diagnostics =
                        Mapping.findUncharacterized
                            unitCfg
                            mUnits
                            mFlows
                            inventory
                            tables
                            (DM.dmChemSynonyms dbManager)
                            idx
                            opts
                pure baseOutcome{loUncharacterized = diagnostics, loUnknownUuids = unknownUuids}
            else pure baseOutcome
    pure
        ImpactsResult
            { irOutcome = outcome
            , irMappingStats = stats
            , irContribs = contribs
            , irUnknownUuids = unknownUuids
            , irRefProductName = prodName
            , irRefProductAmount = prodAmount
            , irRefProductUnit = prodUnit
            }

{- | Handler for the 'get_impacts' MCP tool (computes LCIA score).
Historically named 'get_lcia' — the MCP surface now uses 'impacts'
per the naming audit; internal Haskell types keep the 'LCIA' acronym
(LCIAResult, computeLCIAScore) since they're the domain term of art.
-}
callGetImpacts :: DatabaseManager -> Text -> Value -> KeyMap Value -> IO Value
callGetImpacts dbManager baseUrl rid args =
    either (toolError rid) id
        <$> runExceptT
            ( do
                req <- loadLcaRequest dbManager args
                ir <- runImpactsRequest dbManager args req
                (_, mUnits) <- liftIO $ DM.getMergedFlowMetadata dbManager
                let topN = fromMaybe 5 (intArg "top_flows" args)
                    method = lrMethod req
                    dbName = lrDbName req
                    ra = lrResolved req
                    score = loScore (irOutcome ir)
                    stats = irMappingStats ir
                    functionalUnit =
                        T.pack (showFFloat (Just 2) (irRefProductAmount ir) "")
                            <> " "
                            <> irRefProductUnit ir
                            <> " of "
                            <> irRefProductName ir
                    contribs = irContribs ir
                    topFlows = take topN contribs
                    webUrl = baseUrl <> "/db/" <> dbName <> "/activity/" <> raText ra <> "/impacts/" <> encodeSegment (lrCollection req) <> "/" <> lrMethodIdText req
                    hasNeg = any (\(_, _, c) -> c < 0) contribs
                    unknownUuids = irUnknownUuids ir
                liftIO $
                    unless (null unknownUuids) $
                        reportProgress Warning $
                            "[MCP get_impacts "
                                <> T.unpack (methodName method)
                                <> "] "
                                <> show (length unknownUuids)
                                <> " inventory flow UUID(s) absent from merged FlowDB — characterization incomplete. Samples: "
                                <> show (take 3 unknownUuids)
                let outcome = irOutcome ir
                    diagnosticsFields =
                        [ "uncharacterized_flows" .= map encodeUncharacterized (loUncharacterized outcome)
                        , "characterized_share"
                            .= ( if loInventoryAbsSum outcome > 0
                                    then loCharacterizedSum outcome / loInventoryAbsSum outcome
                                    else 1 :: Double
                               )
                        ]
                pure $
                    toolSuccessJson rid $
                        object $
                            [ "method" .= methodName method
                            , "category" .= methodCategory method
                            , "score" .= score
                            , "unit" .= methodUnit method
                            , "functional_unit" .= functionalUnit
                            , "mapped_flows" .= (msTotal stats - msUnmatched stats)
                            , "has_negative_contributions" .= hasNeg
                            , "web_url" .= webUrl
                            , "top_flows"
                                .= [ object
                                        [ "flow_name" .= bfName f
                                        , "contribution" .= c
                                        , "contribution_percent" .= (if score /= 0 then c / score * 100 else 0 :: Double)
                                        , "flow_id" .= UUID.toText (bfId f)
                                        , "category" .= compartmentName (bfCompartment f)
                                        , "compartment" .= compartmentSub (bfCompartment f)
                                        , "cf_value" .= cfVal
                                        , "flow_unit" .= getUnitNameForBioFlow mUnits f
                                        ]
                                   | (f, cfVal, c) <- topFlows
                                   ]
                            ]
                                ++ (if fromMaybe False (boolArg "include_diagnostics" args) then diagnosticsFields else [])
            )

{- | Handler for the 'compute_sensitivity' MCP tool. Mirrors the REST
@POST /sensitivity/{collection}/{methodId}@ endpoint: runs Service.computeSensitivities
to get baseline + per-perturbation scaling vectors, then computes the LCIA score
for each. Uses 'computeLCIAScoreAuto' so regionalized methods route through the
location-hierarchy walk; non-regionalized methods stay on the classic
'computeLCIAScoreFromTables' path.
-}
callComputeSensitivity :: DatabaseManager -> Text -> Value -> KeyMap Value -> IO Value
callComputeSensitivity dbManager baseUrl rid args =
    either (toolError rid) id
        <$> runExceptT
            ( do
                req <- loadLcaRequest dbManager args
                let ld = lrLoaded req
                    db = ldDatabase ld
                    method = lrMethod req
                    dbName = lrDbName req
                    ra = lrResolved req
                ExceptT $ pure $ ensureLinked dbName "computing sensitivity" db
                perts <-
                    ExceptT $
                        pure
                            ( parseArrayArg
                                "perturbations"
                                (Just "'perturbations' is required (array of {consumer, supplier, delta, label?})")
                                args ::
                                Either Text [Perturbation]
                            )
                unitCfg <- liftIO $ DM.getMergedUnitConfig dbManager
                (mFlows, mUnits) <- liftIO $ DM.getMergedFlowMetadata dbManager
                tables <- liftIO $ DM.mapMethodToTablesCached dbManager dbName db method
                hier <- liftIO $ DM.getLocationHierarchy dbManager
                eRes <-
                    liftIO $
                        Service.computeSensitivities db (ldSharedSolver ld) (raPid ra) perts
                (baselineX, perResults) <- case eRes of
                    Left err -> throwE (T.pack (show err))
                    Right v -> pure v
                let scoreOf x =
                        let inv = applyBiosphereMatrix db x
                         in case computeLCIAScoreAuto unitCfg mUnits mFlows db x inv hier tables of
                                Right s -> Right s
                                Left e -> Left e
                baselineScore <- case scoreOf baselineX of
                    Right s -> pure s
                    Left e -> throwE ("baseline scoring failed: " <> e)
                let webUrl = baseUrl <> "/db/" <> dbName <> "/activity/" <> raText ra <> "/sensitivity/" <> encodeSegment (lrCollection req) <> "/" <> lrMethodIdText req
                    pertEntry (p, eitherX) =
                        let base =
                                [ "perturbation"
                                    .= object
                                        [ "consumer" .= perConsumer p
                                        , "supplier" .= perSupplier p
                                        , "delta" .= perDelta p
                                        ]
                                ]
                            withLabel = case perLabel p of
                                Just l -> ("label" .= l) : base
                                Nothing -> base
                         in case eitherX of
                                Left err -> object (("error" .= err) : withLabel)
                                Right x' -> case scoreOf x' of
                                    Left err -> object (("error" .= err) : withLabel)
                                    Right s ->
                                        object
                                            ( ("score" .= s)
                                                : ("delta_score" .= (s - baselineScore))
                                                : withLabel
                                            )
                pure $
                    toolSuccessJson rid $
                        object
                            [ "method" .= methodName method
                            , "category" .= methodCategory method
                            , "unit" .= methodUnit method
                            , "baseline_score" .= baselineScore
                            , "perturbed" .= map pertEntry perResults
                            , "web_url" .= webUrl
                            ]
            )

{- | Cross-database impact comparison for mapping audits.

Scores the same logical activity twice — once on @(database_a, method_a)@,
once on @(database_b, method_b)@ — and reports the per-impact-category
delta plus a per-flow drill-down. Built for the BAFU+EF3.1 vs SimaPro+EF3.1
audit: the SimaPro side is the trusted ground truth, the BAFU side is the
mapping under test, and 'delta.relative_pct' is the headline metric to
drive down.

Per-flow alignment uses (normalized name, medium, subcompartment) — NOT
UUIDs — because UUIDs differ across databases by construction (each parser
generates them in its own namespace), and that's exactly the problem this
audit is designed to expose.
-}
callCompareImpacts :: DatabaseManager -> Value -> KeyMap Value -> IO Value
callCompareImpacts dbManager rid args =
    either (toolError rid) id
        <$> runExceptT
            ( do
                argsA <- ExceptT . pure $ subArgs "_a" args
                argsB <- ExceptT . pure $ subArgs "_b" args
                reqA <- loadLcaRequest dbManager argsA
                reqB <- loadLcaRequest dbManager argsB
                irA <- runImpactsRequest dbManager argsA reqA
                irB <- runImpactsRequest dbManager argsB reqB
                let topN = fromMaybe 10 (intArg "top_flows" args)
                    scoreA = loScore (irOutcome irA)
                    scoreB = loScore (irOutcome irB)
                    delta = scoreA - scoreB
                    relPct =
                        if scoreB /= 0
                            then abs delta / abs scoreB * 100
                            else 0
                    aTop = take topN (irContribs irA)
                    bTop = take topN (irContribs irB)
                    aMap = M.fromList [(flowKey f, c) | (f, _, c) <- irContribs irA]
                    bMap = M.fromList [(flowKey f, c) | (f, _, c) <- irContribs irB]
                    common =
                        [ object
                            [ "flow_name" .= bfName f
                            , "category" .= compartmentName (bfCompartment f)
                            , "compartment" .= compartmentSub (bfCompartment f)
                            , "a_contrib" .= cA
                            , "b_contrib" .= cB
                            , "delta" .= (cA - cB)
                            ]
                        | (f, _, cA) <- aTop
                        , let k = flowKey f
                        , Just cB <- [M.lookup k bMap]
                        ]
                    aOnly =
                        [ encodeContrib f c
                        | (f, _, c) <- aTop
                        , M.notMember (flowKey f) bMap
                        ]
                    bOnly =
                        [ encodeContrib f c
                        | (f, _, c) <- bTop
                        , M.notMember (flowKey f) aMap
                        ]
                pure $
                    toolSuccessJson rid $
                        object
                            [ "a" .= sideJson reqA irA
                            , "b" .= sideJson reqB irB
                            , "delta"
                                .= object
                                    [ "absolute" .= delta
                                    , "relative_pct" .= relPct
                                    ]
                            , "common_flows" .= common
                            , "top_a_only_flows" .= aOnly
                            , "top_b_only_flows" .= bOnly
                            ]
            )
  where
    sideJson req ir =
        let outcome = irOutcome ir
            characterizedShare =
                if loInventoryAbsSum outcome > 0
                    then loCharacterizedSum outcome / loInventoryAbsSum outcome
                    else 1 :: Double
         in object
                [ "database" .= lrDbName req
                , "process_id" .= raText (lrResolved req)
                , "method" .= methodName (lrMethod req)
                , "score" .= loScore outcome
                , "unit" .= methodUnit (lrMethod req)
                , "characterized_share" .= characterizedShare
                ]
    encodeContrib f c =
        object
            [ "flow_name" .= bfName f
            , "category" .= compartmentName (bfCompartment f)
            , "compartment" .= compartmentSub (bfCompartment f)
            , "contribution" .= c
            ]

    -- Align flows across databases by (normalized name, medium, subcompartment).
    -- UUIDs differ across DBs by construction — see Method/Mapping comments.
    flowKey :: BiosphereFlow -> (Text, Text, Text)
    flowKey f =
        ( T.toLower (T.strip (bfName f))
        , T.toLower (compartmentName (bfCompartment f))
        , maybe "" T.toLower (compartmentSub (bfCompartment f))
        )

{- | Pull side-specific args (suffixed @_a@ / @_b@) up to the standard names
expected by 'loadLcaRequest'. Errors if any required side arg is missing.
-}
subArgs :: Text -> KeyMap Value -> Either Text (KeyMap Value)
subArgs suffix args = do
    db <- requireSide "database"
    pid <- requireSide "process_id"
    method <- requireSide "method_id"
    pure $
        KM.fromList
            [ (fromText "database", String db)
            , (fromText "process_id", String pid)
            , (fromText "method_id", String method)
            ]
  where
    requireSide key =
        let suffixed = key <> suffix
         in case textArg suffixed args of
                Just v -> Right v
                Nothing -> Left ("Missing required parameter: " <> suffixed)

callListMethods :: DatabaseManager -> Value -> IO Value
callListMethods dbManager rid = do
    loadedMethods <- DM.getLoadedMethods dbManager
    let summaries =
            map
                ( \(_, m) ->
                    object
                        [ "id" .= UUID.toText (methodId m)
                        , "name" .= methodName m
                        , "category" .= methodCategory m
                        , "unit" .= methodUnit m
                        ]
                )
                loadedMethods
    return $ toolSuccessJson rid $ object ["methods" .= summaries]

callGetFlowMapping :: DatabaseManager -> Value -> KeyMap Value -> IO Value
callGetFlowMapping dbManager rid args =
    case (,) <$> requireText "database" args <*> requireText "method_id" args of
        Left err -> return $ toolError rid err
        Right (dbName, methodIdText) -> do
            mLoaded <- getDatabase dbManager dbName
            case mLoaded of
                Nothing -> return $ toolError rid ("Database not loaded: " <> dbName)
                Just ld -> do
                    let db = ldDatabase ld
                    loadedMethods <- DM.getLoadedMethods dbManager
                    let allMethods = map snd loadedMethods
                    case UUID.fromText methodIdText of
                        Nothing -> return $ toolError rid "Invalid method UUID format"
                        Just uuid ->
                            case filter (\m -> methodId m == uuid) allMethods of
                                [] -> return $ toolError rid "Method not found"
                                (method : _) -> do
                                    mappings <- DM.mapMethodToFlowsCached dbManager dbName db method
                                    let stats = computeMappingStats mappings
                                        total = msTotal stats
                                        matched = total - msUnmatched stats
                                        coverage =
                                            if total > 0
                                                then fromIntegral matched / fromIntegral total * 100 :: Double
                                                else 0
                                        verbose = fromMaybe False (boolArg "verbose" args)
                                        maxUnm = fromMaybe 50 (intArg "max_unmatched" args)
                                    extra <-
                                        if not verbose
                                            then pure []
                                            else do
                                                let unmatchedCFs =
                                                        take
                                                            maxUnm
                                                            [ object
                                                                [ "name" .= mcfFlowName cf
                                                                , "cas" .= mcfCAS cf
                                                                , "compartment" .= mcfCompartment cf
                                                                , "cf_value" .= mcfValue cf
                                                                , "cf_unit" .= mcfUnit cf
                                                                ]
                                                            | (cf, Nothing) <- mappings
                                                            ]
                                                unmatchedFlows <- buildUnmatchedDbFlows dbManager dbName db method args maxUnm
                                                pure
                                                    [ "unmatched_cfs" .= unmatchedCFs
                                                    , "unmatched_db_flows" .= unmatchedFlows
                                                    ]
                                    return $
                                        toolSuccessJson rid $
                                            object $
                                                [ "method" .= methodName method
                                                , "total" .= total
                                                , "matched" .= matched
                                                , "unmatched" .= msUnmatched stats
                                                , "coverage" .= coverage
                                                ]
                                                    ++ extra

{- | Verbose-mode helper: rank unmatched DB flows for a method.

When @process_id@ is given, runs 'findUncharacterized' on that activity's
inventory — the most actionable view (which uncharacterized flows actually
contribute to the score that user is auditing). Without @process_id@, falls
back to an empty list with a hint, so callers know how to ask for the
useful version. The "scan the whole biosphere matrix" mode promised by the
plan would belong here too — left for a follow-up commit if the
process-scoped view turns out to be insufficient in practice.
-}
buildUnmatchedDbFlows ::
    DatabaseManager ->
    Text ->
    Database ->
    Method ->
    KeyMap Value ->
    Int ->
    IO [Value]
buildUnmatchedDbFlows dbManager dbName db method args maxN =
    case textArg "process_id" args of
        Nothing -> pure [] -- caller didn't pin a process; nothing actionable to rank by
        Just pidText -> do
            mLoaded <- getDatabase dbManager dbName
            case mLoaded of
                Nothing -> pure []
                Just ld -> case Service.resolveActivityAndProcessId db pidText of
                    Left _ -> pure []
                    Right (pid, _) -> do
                        unitCfg <- DM.getMergedUnitConfig dbManager
                        (mFlows, mUnits) <- DM.getMergedFlowMetadata dbManager
                        invE <-
                            computeInventoryMatrixWithDepsCached
                                unitCfg
                                (DM.mkDepSolverLookup dbManager)
                                db
                                dbName
                                (ldSharedSolver ld)
                                pid
                        case invE of
                            Left _ -> pure []
                            Right sol -> do
                                let inventory = SharedSolver.csInventory sol
                                tables <- DM.mapMethodToTablesCached dbManager dbName db method
                                idx <- DM.mapMethodToIndexCached dbManager dbName method
                                let opts =
                                        defaultUncharacterizedOpts
                                            { Mapping.uoMaxFlows = maxN
                                            , Mapping.uoMaxSimilar = 3
                                            }
                                    uncharacterized =
                                        Mapping.findUncharacterized
                                            unitCfg
                                            mUnits
                                            mFlows
                                            inventory
                                            tables
                                            (DM.dmChemSynonyms dbManager)
                                            idx
                                            opts
                                pure (map encodeUncharacterized uncharacterized)

callGetCharacterization :: DatabaseManager -> Value -> KeyMap Value -> IO Value
callGetCharacterization dbManager rid args =
    case (,) <$> requireText "database" args <*> requireText "method_id" args of
        Left err -> return $ toolError rid err
        Right (dbName, methodIdText) -> do
            mLoaded <- getDatabase dbManager dbName
            case mLoaded of
                Nothing -> return $ toolError rid ("Database not loaded: " <> dbName)
                Just ld -> do
                    eMethod <- resolveMethod dbManager methodIdText
                    case eMethod of
                        Left err -> return $ toolError rid err
                        Right (_, method) -> do
                            let db = ldDatabase ld
                                lim = fromMaybe 20 (intArg "limit" args)
                                flowQ = textArg "flow" args
                                queryLower = fmap T.toLower flowQ
                            mappings <- DM.mapMethodToFlowsCached dbManager dbName db method
                            let matched =
                                    [ (cf, f, strat)
                                    | (cf, Just (f, strat)) <- mappings
                                    , matchQuery queryLower (mcfFlowName cf) (bfName f)
                                    ]
                                sorted = L.sortOn (\(cf, _, _) -> negate (abs (mcfValue cf))) matched
                                top = take lim sorted
                                mkEntry (cf, f, strat) =
                                    object
                                        [ "cf_flow_name" .= mcfFlowName cf
                                        , "cf_value" .= mcfValue cf
                                        , "cf_unit" .= mcfUnit cf
                                        , "direction" .= (case mcfDirection cf of Input -> "Input" :: Text; Output -> "Output")
                                        , "db_flow_name" .= bfName f
                                        , "flow_id" .= UUID.toText (bfId f)
                                        , "flow_unit" .= getUnitNameForBioFlow (dbUnits db) f
                                        , "category" .= compartmentName (bfCompartment f)
                                        , "compartment" .= compartmentSub (bfCompartment f)
                                        , "match_strategy" .= show strat
                                        ]
                            return $
                                toolSuccessJson rid $
                                    object
                                        [ "method" .= methodName method
                                        , "unit" .= methodUnit method
                                        , "matches" .= length matched
                                        , "shown" .= length top
                                        , "factors" .= map mkEntry top
                                        ]
  where
    matchQuery Nothing _ _ = True
    matchQuery (Just q) cfName dbFlowName = T.isInfixOf q (T.toLower cfName) || T.isInfixOf q (T.toLower dbFlowName)

{- | Build the MCP JSON object for a cross-DB activity contribution. Dep-DB
process IDs are qualified as @"dbName::actUUID_prodUUID"@ — same convention
as the activity-detail endpoint, so the @web_url@ round-trips.
-}
mkMcpCrossDBEntry ::
    DatabaseManager ->
    -- | root DB name
    Text ->
    -- | base URL
    Text ->
    -- | method collection name
    Text ->
    -- | method UUID text
    Text ->
    BioFlowDB ->
    UnitDB ->
    -- | total score (for share %)
    Double ->
    ((Text, ProcessId), Double) ->
    IO Value
mkMcpCrossDBEntry dbManager rootDbName baseUrl colName methodIdText flowDB unitDB score ((depDbName, pid), c) = do
    mLd <- getDatabase dbManager depDbName
    let (actName, actLoc, prodName, pidText) = case mLd of
            Just ld ->
                let d = ldDatabase ld
                    mAct = Service.findActivityByProcessId d pid
                    txt =
                        if depDbName == rootDbName
                            then processIdToText d pid
                            else depDbName <> "::" <> processIdToText d pid
                    -- Reference products are technosphere; pull the supplier's tech flow map.
                    (pn, _, _) = maybe ("", 0, "") (Service.getReferenceProductInfo (dbTechFlows d) unitDB) mAct
                 in (maybe "" activityName mAct, maybe "" activityLocation mAct, pn, txt)
            Nothing ->
                ("", "", "", depDbName <> "::<unloaded>")
        procWebUrl =
            baseUrl
                <> "/db/"
                <> rootDbName
                <> "/activity/"
                <> pidText
                <> "/contributing-activities/"
                <> encodeSegment colName
                <> "/"
                <> methodIdText
    pure $
        object
            [ "process_id" .= pidText
            , "activity_name" .= actName
            , "product_name" .= prodName
            , "location" .= actLoc
            , "contribution" .= c
            , "contribution_percent" .= (if score /= 0 then c / score * 100 else 0 :: Double)
            , "web_url" .= procWebUrl
            ]

-- | Helper: resolve method from UUID text, also returning its collection name
resolveMethod :: DatabaseManager -> Text -> IO (Either Text (Text, Method))
resolveMethod dbManager methodIdText =
    case UUID.fromText methodIdText of
        Nothing -> return $ Left "Invalid method UUID format"
        Just uuid -> do
            loadedMethods <- DM.getLoadedMethods dbManager
            case filter (\(_, m) -> methodId m == uuid) loadedMethods of
                [] -> return $ Left "Method not found"
                ((col, m) : _) -> return $ Right (col, m)

{- | Raw text + its parsed 'ProcessId' + the looked-up 'Activity'. Bundled so
the three entities (which must always agree) cannot drift apart: the only
way to build a 'ResolvedActivity' is through 'resolveActivityAndProcessId'.
-}
data ResolvedActivity = ResolvedActivity
    { raText :: !Text
    , raPid :: !ProcessId
    , raActivity :: !Activity
    }

{- | Bundle of entities resolved at the start of every LCA handler (impacts,
contributing flows, contributing activities, inventory). Populated once by
'loadLcaRequest' so the handler body stays flat instead of unwrapping four
layers of 'case'.
-}
data LcaRequest = LcaRequest
    { lrDbName :: !Text
    , lrLoaded :: !LoadedDatabase
    , lrResolved :: !ResolvedActivity
    , lrMethodIdText :: !Text
    , lrCollection :: !Text
    , lrMethod :: !Method
    }

{- | Resolve every entity an LCA handler needs from raw JSON-RPC args.
Short-circuits on the first failure (missing arg, unknown DB, bad UUID,
unknown method, unresolvable process id).
-}
loadLcaRequest :: DatabaseManager -> KeyMap Value -> ExceptT Text IO LcaRequest
loadLcaRequest dbManager args = do
    (dbName, pidText, methodIdText) <-
        ExceptT $
            pure $
                (,,)
                    <$> requireText "database" args
                    <*> requireText "process_id" args
                    <*> requireText "method_id" args
    mLoaded <- liftIO $ getDatabase dbManager dbName
    ld <- case mLoaded of
        Nothing -> throwE ("Database not loaded: " <> dbName)
        Just x -> pure x
    (col, method) <- ExceptT (resolveMethod dbManager methodIdText)
    (pid, act) <- case Service.resolveActivityAndProcessId (ldDatabase ld) pidText of
        Left err -> throwE (T.pack (show err))
        Right v -> pure v
    pure
        LcaRequest
            { lrDbName = dbName
            , lrLoaded = ld
            , lrResolved = ResolvedActivity pidText pid act
            , lrMethodIdText = methodIdText
            , lrCollection = col
            , lrMethod = method
            }

{- | Bail if the database has unresolved cross-DB links. 'op' names the
user-visible operation for the error message (e.g. "computing impacts").
-}
ensureLinked :: Text -> Text -> Database -> Either Text ()
ensureLinked dbName op db =
    let n = unresolvedCount (dbLinkingStats db)
     in if n == 0
            then Right ()
            else
                Left $
                    "Database \""
                        <> dbName
                        <> "\" has "
                        <> T.pack (show n)
                        <> " unresolved cross-DB products. Load the missing dependency databases and re-link before "
                        <> op
                        <> "."

callGetContributingFlows :: DatabaseManager -> Text -> Value -> KeyMap Value -> IO Value
callGetContributingFlows dbManager baseUrl rid args =
    either (toolError rid) id
        <$> runExceptT
            ( do
                req <- loadLcaRequest dbManager args
                let ld = lrLoaded req
                    db = ldDatabase ld
                    method = lrMethod req
                    dbName = lrDbName req
                    ra = lrResolved req
                    lim = fromMaybe 20 (intArg "limit" args)
                    webUrl = baseUrl <> "/db/" <> dbName <> "/activity/" <> raText ra <> "/contributing-flows/" <> encodeSegment (lrCollection req) <> "/" <> lrMethodIdText req
                ExceptT $ pure $ ensureLinked dbName "computing contributions" db
                unitCfg <- liftIO $ DM.getMergedUnitConfig dbManager
                (mFlows, mUnits) <- liftIO $ DM.getMergedFlowMetadata dbManager
                sol <-
                    ExceptT $
                        computeInventoryMatrixWithDepsCached
                            unitCfg
                            (DM.mkDepSolverLookup dbManager)
                            db
                            dbName
                            (ldSharedSolver ld)
                            (raPid ra)
                let inventory = SharedSolver.csInventory sol
                tables <- liftIO $ DM.mapMethodToTablesCached dbManager dbName db method
                let outcome = computeLCIAScoreFromTables unitCfg mUnits mFlows inventory tables
                    score = loScore outcome
                    (rawContribs, unknownUuids) = inventoryContributions unitCfg mUnits mFlows inventory tables
                    contribs = L.sortOn (\(_, _, c) -> negate (abs c)) rawContribs
                    top = take lim contribs
                    hasNeg = any (\(_, _, c) -> c < 0) contribs
                diagnosticsFields <-
                    if fromMaybe False (boolArg "include_diagnostics" args)
                        then do
                            idx <- liftIO $ DM.mapMethodToIndexCached dbManager dbName method
                            let opts = defaultUncharacterizedOpts
                                uncharacterized =
                                    Mapping.findUncharacterized
                                        unitCfg
                                        mUnits
                                        mFlows
                                        inventory
                                        tables
                                        (DM.dmChemSynonyms dbManager)
                                        idx
                                        opts
                            pure
                                [ "uncharacterized_flows" .= map encodeUncharacterized uncharacterized
                                , "characterized_share"
                                    .= ( if loInventoryAbsSum outcome > 0
                                            then loCharacterizedSum outcome / loInventoryAbsSum outcome
                                            else 1 :: Double
                                       )
                                ]
                        else pure []
                liftIO $
                    unless (null unknownUuids) $
                        reportProgress Warning $
                            "[MCP get_contributing_flows "
                                <> T.unpack (methodName method)
                                <> "] "
                                <> show (length unknownUuids)
                                <> " inventory flow UUID(s) absent from merged FlowDB. Samples: "
                                <> show (take 3 unknownUuids)
                pure $
                    toolSuccessJson rid $
                        object $
                            [ "method" .= methodName method
                            , "unit" .= methodUnit method
                            , "total_score" .= score
                            , "has_negative_contributions" .= hasNeg
                            , "web_url" .= webUrl
                            , "top_flows"
                                .= [ object
                                        [ "flow_name" .= bfName f
                                        , "contribution" .= c
                                        , "contribution_percent" .= (if score /= 0 then c / score * 100 else 0 :: Double)
                                        , "flow_id" .= UUID.toText (bfId f)
                                        , "category" .= compartmentName (bfCompartment f)
                                        , "compartment" .= compartmentSub (bfCompartment f)
                                        , "cf_value" .= cfVal
                                        ]
                                   | (f, cfVal, c) <- top
                                   ]
                            ]
                                ++ diagnosticsFields
            )

callGetContributingActivities :: DatabaseManager -> Text -> Value -> KeyMap Value -> IO Value
callGetContributingActivities dbManager baseUrl rid args =
    either (toolError rid) id
        <$> runExceptT
            ( do
                req <- loadLcaRequest dbManager args
                let ld = lrLoaded req
                    db = ldDatabase ld
                    method = lrMethod req
                    dbName = lrDbName req
                    ra = lrResolved req
                    lim = fromMaybe 10 (intArg "limit" args)
                ExceptT $ pure $ ensureLinked dbName "computing contributions" db
                unitCfg <- liftIO $ DM.getMergedUnitConfig dbManager
                (mFlows, mUnits) <- liftIO $ DM.getMergedFlowMetadata dbManager
                tables <- liftIO $ DM.mapMethodToTablesCached dbManager dbName db method
                -- Skip separate inventory compute: contributions sum equals the score.
                contributions <-
                    ExceptT $
                        crossDBProcessContributions
                            unitCfg
                            mUnits
                            mFlows
                            (DM.mkDepSolverLookup dbManager)
                            db
                            dbName
                            (ldSharedSolver ld)
                            (raPid ra)
                            tables
                let score = sum (M.elems contributions)
                    sorted = L.sortOn (\(_, c) -> negate (abs c)) (M.toList contributions)
                    top = take lim sorted
                    hasNeg = any (\(_, c) -> c < 0) top
                rows <- liftIO $ mapM (mkMcpCrossDBEntry dbManager dbName baseUrl (lrCollection req) (lrMethodIdText req) mFlows mUnits score) top
                pure $
                    toolSuccessJson rid $
                        object
                            [ "method" .= methodName method
                            , "unit" .= methodUnit method
                            , "total_score" .= score
                            , "has_negative_contributions" .= hasNeg
                            , "processes" .= rows
                            ]
            )

callListGeographies :: DatabaseManager -> Value -> KeyMap Value -> IO Value
callListGeographies dbManager rid args =
    case textArg "database" args of
        Nothing -> return $ toolError rid "Missing required parameter: database"
        Just dbName -> do
            mLoaded <- getDatabase dbManager dbName
            case mLoaded of
                Nothing -> return $ toolError rid ("Database not loaded: " <> dbName)
                Just ld -> do
                    let db = ldDatabase ld
                        geoMap = dmGeographies dbManager
                        codes = L.sort $ M.keys (idxByLocation (dbIndexes db))
                        mkEntry code =
                            let (displayName, parents) = M.findWithDefault (code, []) code geoMap
                                parentStr = T.intercalate "|" parents
                             in object
                                    [ "geo" .= code
                                    , "display_name" .= displayName
                                    , "parent_regions" .= parentStr
                                    ]
                    return $
                        toolSuccessJson rid $
                            object
                                ["geographies" .= map mkEntry codes]

-- ============================================================================
-- score_activity / score_activities / list_scoring_sets
--
-- Wrappers around API.BatchImpacts so a single MCP call yields the full
-- LCIA panel + every configured scoring set + per-indicator breakdown,
-- removing the N round-trips of get_impacts a comparative study used to
-- need. Each response is enriched with a 'web_url' deep link to the
-- matching web UI view so a human can continue the exploration visually.
-- ============================================================================

-- | Translate a 'BI.BatchError' into the MCP 'toolError' payload.
batchErrorMsg :: BI.BatchError -> Text
batchErrorMsg err = case err of
    BI.CollectionNotLoaded name available ->
        "Collection not loaded: "
            <> name
            <> ". Available collections: "
            <> T.intercalate ", " available
    BI.DatabaseNotLoaded name -> "Database not loaded: " <> name
    BI.ActivityResolutionFailed msg -> msg
    BI.LinkingIncomplete msg -> msg
    BI.OtherBatchError code msg -> "HTTP " <> T.pack (show code) <> ": " <> msg

{- | Look up the configured scoring-set names on a loaded method collection.
Returns the empty list when the collection is not loaded; in that case
the batch runner has already returned 'BI.CollectionNotLoaded' and the
filter is never consulted, so the empty result here is harmless. We
read 'mcScoringSets' directly — not the keys of @scoringResults@ — so
that a set whose evaluation produced no scores still counts as
"configured" for the @scoring_sets@ filter.
-}
configuredScoringSetNames :: DatabaseManager -> Text -> IO [Text]
configuredScoringSetNames dbm collName = do
    loaded <- readTVarIO (dmLoadedMethods dbm)
    pure $ case M.lookup collName loaded of
        Just mc -> map ssName (mcScoringSets mc)
        Nothing -> []

{- | Handler for the 'score_activity' MCP tool.

Returns the full LCIABatchResult shape (per-method scores, per-scoring-set
aggregate scores, per-indicator breakdown, units) for a single activity,
enriched with a top-level 'web_url' for the panel view and a per-method
'web_url' in each @results@ entry. Replaces the @N@ round-trips of
'get_impacts' a comparative study used to need.
-}
callScoreActivity :: DatabaseManager -> Text -> Value -> KeyMap Value -> IO Value
callScoreActivity dbManager baseUrl rid args =
    either (toolError rid) id
        <$> runExceptT
            ( do
                dbName <- ExceptT $ pure (requireText "database" args)
                pidText <- ExceptT $ pure (requireText "process_id" args)
                coll <- ExceptT $ pure (requireText "collection" args)
                subs <- ExceptT $ pure (parseArrayArg "substitutions" Nothing args :: Either Text [Substitution])
                wantedSets <- ExceptT $ pure (parseArrayArg "scoring_sets" Nothing args :: Either Text [Text])
                let mSub = if null subs then Nothing else Just SubstitutionRequest{srSubstitutions = subs}
                res <- liftIO $ BI.runActivityLCIABatch dbManager dbName pidText coll mSub
                case res of
                    Left e -> ExceptT $ pure (Left (batchErrorMsg e))
                    Right lbr -> do
                        configured <- liftIO $ configuredScoringSetNames dbManager coll
                        let topUrl = scoreActivityWebUrl baseUrl dbName pidText coll
                            enriched =
                                addWebUrl
                                    topUrl
                                    (enrichResultsWithWebUrl topUrl (toJSON lbr))
                        ExceptT $ pure (toolSuccessJson rid <$> filterScoringSets configured wantedSets enriched)
            )

{- | Handler for the 'score_activities' MCP tool.

Scores N activities against every method in a collection in one
multi-RHS MUMPS solve plus parallel characterization. Each successful
entry carries a top-level @web_url@ (the activity-level impacts page)
and the same URL is replicated inside its @impacts@ subtree alongside
per-method @web_url@s, so clients reading either shape land on the
right link. Unresolved process IDs land in @not_found@ / @invalid@ of
the response, not as a 'BatchError'.
-}
callScoreActivities :: DatabaseManager -> Text -> Value -> KeyMap Value -> IO Value
callScoreActivities dbManager baseUrl rid args =
    either (toolError rid) id
        <$> runExceptT
            ( do
                dbName <- ExceptT $ pure (requireText "database" args)
                coll <- ExceptT $ pure (requireText "collection" args)
                pids <- ExceptT $ pure (parseArrayArg "process_ids" (Just "'process_ids' required (array of strings)") args :: Either Text [Text])
                wantedSets <- ExceptT $ pure (parseArrayArg "scoring_sets" Nothing args :: Either Text [Text])
                let topFlows = intArg "top_flows" args
                res <- liftIO $ BI.runBatchImpacts dbManager dbName coll topFlows pids
                case res of
                    Left e -> ExceptT $ pure (Left (batchErrorMsg e))
                    Right bir -> do
                        configured <- liftIO $ configuredScoringSetNames dbManager coll
                        let enriched = enrichBatchResults baseUrl dbName coll (toJSON bir)
                        ExceptT $ pure (toolSuccessJson rid <$> filterScoringSetsBatch configured wantedSets enriched)
            )

{- | Handler for the 'list_scoring_sets' MCP tool.

Returns the formula-based scoring sets configured on every loaded
'MethodCollection'. Pure read from the live TVar; no HTTP equivalent.
When 'collection' is supplied, filters to that one and errors if it is
not loaded (listing the loaded names in the message).

The projection is explicit (rather than @toJSON ss@) so the wire format
stays in snake_case and is not silently affected by a future field
addition to 'ScoringSet'.
-}
callListScoringSets :: DatabaseManager -> Value -> KeyMap Value -> IO Value
callListScoringSets dbManager rid args = do
    loaded <- readTVarIO (dmLoadedMethods dbManager)
    case optionalText "collection" args of
        Left err -> return $ toolError rid err
        Right Nothing -> return $ toolSuccessJson rid (encodeAll loaded)
        Right (Just collName) -> case M.lookup collName loaded of
            Nothing ->
                return $
                    toolError
                        rid
                        ( "Collection not loaded: "
                            <> collName
                            <> ". Available collections: "
                            <> T.intercalate ", " (M.keys loaded)
                        )
            Just mc -> return $ toolSuccessJson rid (encodeAll (M.singleton collName mc))
  where
    encodeAll :: M.Map Text MethodCollection -> Value
    encodeAll loaded =
        object
            [ "collections"
                .= [ object
                        [ "collection" .= cName
                        , "scoring_sets" .= map encodeScoringSet (mcScoringSets mc)
                        ]
                   | (cName, mc) <- M.toList loaded
                   ]
            ]

    encodeScoringSet :: ScoringSet -> Value
    encodeScoringSet ss =
        object
            [ "name" .= ssName ss
            , "unit" .= ssUnit ss
            , "variables" .= ssVariables ss
            , "computed" .= ssComputed ss
            , "normalization" .= ssNormalization ss
            , "weighting" .= ssWeighting ss
            , "scores" .= ssScores ss
            , "display_multiplier" .= ssDisplayMultiplier ss
            ]
