{-# LANGUAGE OverloadedStrings #-}

{- | The @\/mcp@ endpoint answers POST and refuses everything else.

A GET opens the stream a server uses to speak to a client unprompted. VoLCA
never speaks first, and an empty stream closed at once reads to a client as a
dropped connection: it reconnects, and the pair loops for as long as both are
up. One such pair sent 71 644 GETs in 21 hours. 405 says there is no stream,
and the client stops asking.
-}
module MCPStreamSpec (spec) where

import Data.IORef
import Network.HTTP.Types (Method)
import Network.HTTP.Types.Status (statusCode)
import Network.Wai (defaultRequest, requestMethod, responseStatus)
import Network.Wai.Internal (ResponseReceived (..))
import Test.Hspec

import API.MCP (mcpApp)
import Config (defaultConfig)
import Database.Manager (initDatabaseManager)

-- | Drive one request of the given method through the endpoint, report its status.
status :: Method -> IO Int
status m = do
    manager <- initDatabaseManager defaultConfig True
    app <- mcpApp manager [] False Nothing Nothing (pure ())
    ref <- newIORef Nothing
    _ <- app defaultRequest{requestMethod = m} $ \resp -> do
        writeIORef ref (Just resp)
        pure ResponseReceived
    maybe (fail "no response") (pure . statusCode . responseStatus) =<< readIORef ref

spec :: Spec
spec = describe "the /mcp endpoint" $ do
    it "refuses a GET rather than hand back a stream that closes at once" $
        status "GET" `shouldReturn` 405

    it "refuses a DELETE the same way" $
        status "DELETE" `shouldReturn` 405
