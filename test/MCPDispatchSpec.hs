{-# LANGUAGE OverloadedStrings #-}

{- | The database load/unload MCP tools are advertised in 'toolDefinitions'
*and* routed by 'callTool'. The two live in different places (the resource
registry vs. the dispatch case), so a name typo would compile yet strand a
tool at runtime with an "Unknown tool" reply. These tests pin both ends.
-}
module MCPDispatchSpec (spec) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), decodeStrict)
import Data.Aeson.Key (fromText, toText)
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.List (sort)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Test.Hspec

import API.MCP (RpcRequest (..), callTool, handleInitialize, mcpCountsAsActivity, toolDefinitions)
import Config (ClassificationEntry (..), ClassificationPreset (..), DatabaseConfig (..), ReadOnly (..), ServerName (..), defaultConfig)
import Database.Manager (addDatabase, initDatabaseManager, loadDatabase)
import Types (GeographyPolicy (..))

-- | The tool definition advertised under a given MCP name.
toolByName :: Text -> Maybe Value
toolByName name =
    listToMaybe
        [t | t@(Object o) <- toolDefinitions (ReadOnly False), KM.lookup "name" o == Just (String name)]

-- | Every advertised tool whose input schema declares a @preset@ parameter.
takesPreset :: [Text]
takesPreset =
    [ name
    | Object o <- toolDefinitions (ReadOnly False)
    , Just (String name) <- [KM.lookup "name" o]
    , Just (Object schema) <- [KM.lookup "inputSchema" o]
    , Just (Object props) <- [KM.lookup "properties" schema]
    , KM.member "preset" props
    ]

-- | The 'required' parameter names declared in a tool's input schema.
requiredOf :: Value -> [Text]
requiredOf (Object o) = case KM.lookup "inputSchema" o of
    Just (Object s) -> case KM.lookup "required" s of
        Just (Array arr) -> [t | String t <- toList arr]
        _ -> []
    _ -> []
requiredOf _ = []

{- | The text payload of a tool reply (@result.content[0].text@), or 'Nothing'
when the reply doesn't have that shape — so a malformed reply fails a test
instead of silently passing a @""@ that satisfies any "doesn't contain X".
-}
resultText :: Value -> Maybe Text
resultText v = do
    Object o <- Just v
    Object r <- KM.lookup "result" o
    Array arr <- KM.lookup "content" r
    Object c <- listToMaybe (toList arr)
    String t <- KM.lookup "text" c
    pure t

-- | A top-level field of a tool reply's JSON payload.
jsonField :: KM.Key -> Value -> Maybe Value
jsonField key resp = do
    t <- resultText resp
    Object o <- decodeStrict (encodeUtf8 t)
    KM.lookup key o

-- | Whether a tool reply is flagged as an error.
isError :: Value -> Bool
isError (Object o) = case KM.lookup "result" o of
    Just (Object r) -> KM.lookup "isError" r == Just (Bool True)
    _ -> False
isError _ = False

call :: Text -> IO Value
call name = do
    manager <- initDatabaseManager defaultConfig True Nothing
    callTool manager [] Nothing Nothing Null name (KM.singleton "database" (String "no-such-db"))

{- | Call the edit tool with one line named. An edit that names nothing is
refused before the database is even looked up, which would mask the refusal a
test is actually about.
-}
callEdit :: IO Value
callEdit = do
    manager <- initDatabaseManager defaultConfig True Nothing
    callTool manager [] Nothing Nothing Null "edit_exchanges" $
        KM.fromList
            [ ("database", String "no-such-db")
            , ("process_id", String "a_b")
            , ("remove", oneRemoval)
            ]
  where
    oneRemoval =
        fromMaybe Null $
            decodeStrict "[{\"kind\":\"biosphere\",\"flow\":\"00000000-0000-0000-0000-000000000003\"}]"

-- | For every array parameter a tool declares, the @type@ its items promise.
itemTypesOf :: Text -> [(Text, Text)]
itemTypesOf name =
    sort
        [ (toText key, itemType)
        | Just (Object o) <- [toolByName name]
        , Just (Object schema) <- [KM.lookup "inputSchema" o]
        , Just (Object props) <- [KM.lookup "properties" schema]
        , (key, Object prop) <- KM.toList props
        , KM.lookup "type" prop == Just (String "array")
        , Just (Object items) <- [KM.lookup "items" prop]
        , Just (String itemType) <- [KM.lookup "type" items]
        ]

spec :: Spec
spec = describe "MCP database load/unload tools" $ do
    it "are advertised with a required 'database' parameter" $ do
        fmap requiredOf (toolByName "load_database") `shouldBe` Just ["database"]
        fmap requiredOf (toolByName "unload_database") `shouldBe` Just ["database"]

    it "are routed by callTool (no 'Unknown tool' gap)" $ do
        loadResp <- call "load_database"
        unloadResp <- call "unload_database"
        resultText loadResp `shouldSatisfy` maybe False (not . T.isPrefixOf "Unknown tool:")
        resultText unloadResp `shouldSatisfy` maybe False (not . T.isPrefixOf "Unknown tool:")

    it "surface the engine error when unloading a database that is not loaded" $ do
        resp <- call "unload_database"
        isError resp `shouldBe` True
        resultText resp `shouldSatisfy` maybe False ("Database not loaded:" `T.isInfixOf`)

    describe "gap-report tool" $ do
        it "is advertised with a required 'database' parameter" $
            fmap requiredOf (toolByName "get_gap_report") `shouldBe` Just ["database"]

        it "is routed by callTool (no 'Unknown tool' gap)" $ do
            resp <- call "get_gap_report"
            resultText resp `shouldSatisfy` maybe False (not . T.isPrefixOf "Unknown tool:")

        it "surfaces the engine error for an unknown database" $ do
            resp <- call "get_gap_report"
            isError resp `shouldBe` True
            resultText resp `shouldSatisfy` maybe False ("Database not loaded:" `T.isInfixOf`)

    describe "exchange-edit tool" $ do
        it "is advertised with the database and the activity it edits" $
            fmap requiredOf (toolByName "edit_exchanges") `shouldBe` Just ["database", "process_id"]

        it "is routed by callTool (no 'Unknown tool' gap)" $ do
            resp <- callEdit
            resultText resp `shouldSatisfy` maybe False (not . T.isPrefixOf "Unknown tool:")

        it "surfaces the engine error for an unknown database" $ do
            resp <- callEdit
            isError resp `shouldBe` True
            resultText resp `shouldSatisfy` maybe False ("Database not loaded:" `T.isInfixOf`)

        it "describes each of its lists as holding objects" $
            -- A list left declaring string items would tell the assistant to
            -- send five arrays of text, every one of which is refused.
            itemTypesOf "edit_exchanges"
                `shouldBe` [ ("add_biosphere", "object")
                           , ("add_inputs", "object")
                           , ("add_waste_outputs", "object")
                           , ("remove", "object")
                           , ("set_amounts", "object")
                           ]

    describe "quality-report tool" $ do
        it "is advertised with a required 'database' parameter" $
            fmap requiredOf (toolByName "get_quality_report") `shouldBe` Just ["database"]

        it "is routed by callTool (no 'Unknown tool' gap)" $ do
            resp <- call "get_quality_report"
            resultText resp `shouldSatisfy` maybe False (not . T.isPrefixOf "Unknown tool:")

        it "surfaces the engine error for an unknown database" $ do
            resp <- call "get_quality_report"
            isError resp `shouldBe` True
            resultText resp `shouldSatisfy` maybe False ("Database not loaded:" `T.isInfixOf`)

    describe "characterization-coverage tool" $ do
        it "is advertised with a required 'database' parameter (collection is optional)" $
            fmap requiredOf (toolByName "get_characterization_coverage") `shouldBe` Just ["database"]

        it "is routed by callTool (no 'Unknown tool' gap)" $ do
            resp <- call "get_characterization_coverage"
            resultText resp `shouldSatisfy` maybe False (not . T.isPrefixOf "Unknown tool:")

        it "surfaces the engine error for an unknown database" $ do
            resp <- call "get_characterization_coverage"
            isError resp `shouldBe` True
            resultText resp `shouldSatisfy` maybe False ("Database not loaded:" `T.isInfixOf`)

    -- A preset narrows a search. A tool that advertises the parameter and then
    -- ignores an unresolvable one answers with the whole database, which reads
    -- like a result rather than like the mistake it is.
    describe "tools taking a classification preset" $ do
        let configured =
                ClassificationPreset
                    { cpName = "raw"
                    , cpLabel = "Raw"
                    , cpDescription = Nothing
                    , cpFilters = [ClassificationEntry{ceSystem = "AGB", ceValue = "Agriculture", ceMode = "exact"}]
                    }
            callWithPreset name = do
                manager <- initDatabaseManager defaultConfig True Nothing
                callTool manager [configured] Nothing Nothing Null name $
                    KM.fromList
                        [ ("database", String "no-such-db")
                        , ("process_id", String "no-such-pid")
                        , ("scope", String "direct")
                        , ("preset", String "transformed")
                        ]

        it "is a non-empty list, or this test proves nothing" $
            takesPreset `shouldSatisfy` not . null

        forM_ takesPreset $ \name ->
            it (T.unpack name <> " refuses a preset the instance does not carry") $ do
                resp <- callWithPreset name
                isError resp `shouldBe` True
                resultText resp `shouldSatisfy` maybe False ("transformed" `T.isInfixOf`)

        -- The refusal above is enforced in dispatch, so it cannot tell a
        -- handler that reads the preset from one that drops it. These pin the
        -- application: the fixture carries no classification at all, so a
        -- preset that resolves must narrow the answer to nothing — a handler
        -- ignoring it answers with the unfiltered set instead.
        describe "a preset that resolves is applied" $ do
            let sampleConfig =
                    DatabaseConfig
                        { dcName = "sample"
                        , dcDisplayName = "sample"
                        , dcPath = "test-data/SAMPLE.min"
                        , dcDescription = Nothing
                        , dcLoad = False
                        , dcDefault = False
                        , dcDepends = []
                        , dcLocationAliases = M.empty
                        , dcFormat = Nothing
                        , dcIsUploaded = False
                        , dcDeletable = False
                        , dcGeographyPolicy = GeoGlobal
                        }
                callOnSample name presetArgs = do
                    manager <- initDatabaseManager defaultConfig True Nothing
                    addDatabase manager sampleConfig
                    loadDatabase manager "sample" >>= either (expectationFailure . T.unpack) (const (pure ()))
                    callTool manager [configured] Nothing Nothing Null name $
                        KM.fromList $
                            [ ("database", String "sample")
                            , ("process_id", String "aa000001-0000-0000-0000-000000000000")
                            , ("scope", String "direct")
                            ]
                                ++ presetArgs
                positiveNumber v = case v of
                    Just (Number n) -> n > 0
                    _ -> False

            forM_ [("aggregate", "filteredCount"), ("get_supply_chain", "filteredActivities")] $
                \(tool, field) -> it (T.unpack tool <> " narrows to nothing under the preset") $ do
                    full <- callOnSample tool []
                    jsonField field full `shouldSatisfy` positiveNumber
                    narrowed <- callOnSample tool [("preset", String "raw")]
                    jsonField field narrowed `shouldBe` Just (Number 0)

    -- A server that shuts itself down when idle asks this question of every
    -- MCP request. Answering "yes" too often keeps an unused server alive for
    -- as long as an assistant stays connected, which is a bill with nobody
    -- behind it; answering "no" too often kills a server mid-conversation.
    describe "mcpCountsAsActivity" $ do
        it "accepts a tool call" $
            mcpCountsAsActivity "tools/call" `shouldBe` True

        it "refuses the calls a client makes on its own" $
            map mcpCountsAsActivity ["initialize", "notifications/initialized", "tools/list", "ping"]
                `shouldBe` [False, False, False, False]

        it "refuses an unknown method" $
            mcpCountsAsActivity "no/such/method" `shouldBe` False

    -- A client may hold several VoLCA servers at once, one per instance. The
    -- name is the only thing that tells them apart, and it has to reach the
    -- assistant, not just the client's server list.
    describe "handleInitialize" $ do
        it "introduces itself by its configured name" $ do
            resp <- handleInitialize (Just (ServerName "@ccomb/private")) initRequest
            serverInfoName resp `shouldBe` Just (String "@ccomb/private")

        it "puts the name where an assistant reads it" $ do
            resp <- handleInitialize (Just (ServerName "@ccomb/private")) initRequest
            fmap (T.isInfixOf "@ccomb/private") (instructionsOf resp) `shouldBe` Just True

        it "falls back to the plain engine name when unconfigured" $ do
            resp <- handleInitialize Nothing initRequest
            serverInfoName resp `shouldBe` Just (String "volca")

        it "says nothing about an instance it cannot name" $ do
            resp <- handleInitialize Nothing initRequest
            fmap (T.isInfixOf "instance named") (instructionsOf resp) `shouldBe` Just False

initRequest :: RpcRequest
initRequest = RpcRequest{rpcId = Just (Number 1), rpcMethod = "initialize", rpcParams = Nothing}

-- | Dig @result.serverInfo.name@ out of a JSON-RPC reply.
serverInfoName :: Value -> Maybe Value
serverInfoName v = field "result" v >>= field "serverInfo" >>= field "name"

instructionsOf :: Value -> Maybe Text
instructionsOf v = case field "result" v >>= field "instructions" of
    Just (String t) -> Just t
    _ -> Nothing

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (fromText k) o
field _ _ = Nothing
