{-# LANGUAGE OverloadedStrings #-}

{- | The database load/unload MCP tools are advertised in 'toolDefinitions'
*and* routed by 'callTool'. The two live in different places (the resource
registry vs. the dispatch case), so a name typo would compile yet strand a
tool at runtime with an "Unknown tool" reply. These tests pin both ends.
-}
module MCPDispatchSpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import API.MCP (callTool, toolDefinitions)
import Config (defaultConfig)
import Database.Manager (initDatabaseManager)

-- | The tool definition advertised under a given MCP name.
toolByName :: Text -> Maybe Value
toolByName name =
    listToMaybe
        [t | t@(Object o) <- toolDefinitions, KM.lookup "name" o == Just (String name)]

-- | The 'required' parameter names declared in a tool's input schema.
requiredOf :: Value -> [Text]
requiredOf (Object o) = case KM.lookup "inputSchema" o of
    Just (Object s) -> case KM.lookup "required" s of
        Just (Array arr) -> [t | String t <- toList arr]
        _ -> []
    _ -> []
requiredOf _ = []

-- | The text payload of a tool reply (@result.content[0].text@).
resultText :: Value -> Text
resultText (Object o) = case KM.lookup "result" o of
    Just (Object r) -> case KM.lookup "content" r of
        Just (Array arr) -> case toList arr of
            (Object c : _) -> case KM.lookup "text" c of
                Just (String t) -> t
                _ -> ""
            _ -> ""
        _ -> ""
    _ -> ""
resultText _ = ""

-- | Whether a tool reply is flagged as an error.
isError :: Value -> Bool
isError (Object o) = case KM.lookup "result" o of
    Just (Object r) -> KM.lookup "isError" r == Just (Bool True)
    _ -> False
isError _ = False

call :: Text -> IO Value
call name = do
    manager <- initDatabaseManager defaultConfig True Nothing
    callTool manager [] Nothing Null name (KM.singleton "database" (String "no-such-db"))

spec :: Spec
spec = describe "MCP database load/unload tools" $ do
    it "are advertised with a required 'database' parameter" $ do
        fmap requiredOf (toolByName "load_database") `shouldBe` Just ["database"]
        fmap requiredOf (toolByName "unload_database") `shouldBe` Just ["database"]

    it "are routed by callTool (no 'Unknown tool' gap)" $ do
        loadResp <- call "load_database"
        unloadResp <- call "unload_database"
        resultText loadResp `shouldNotSatisfy` ("Unknown tool:" `T.isPrefixOf`)
        resultText unloadResp `shouldNotSatisfy` ("Unknown tool:" `T.isPrefixOf`)

    it "surface the engine error when unloading a database that is not loaded" $ do
        resp <- call "unload_database"
        isError resp `shouldBe` True
        resultText resp `shouldSatisfy` ("Database not loaded:" `T.isInfixOf`)
