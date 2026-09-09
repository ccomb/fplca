{-# LANGUAGE OverloadedStrings #-}

{- | The idle watchdog decides when a server is unused. It must not decide that
while the server is answering.
-}
module IdleSpec (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Data.IORef (newIORef, readIORef, writeIORef)
import Network.HTTP.Types (status200)
import Network.Wai (Application, defaultRequest, responseLBS)
import Network.Wai.Internal (ResponseReceived (..))
import Test.Hspec

import App.Idle (IdleState (..), idleTrackingMiddleware, idleWatchdog, newIdleState)

spec :: Spec
spec = describe "Idle watchdog" $ do
    it "does not shut down under a request that outlasts the timeout" $ do
        idle <- armed
        fired <- newIORef False
        _ <- forkIO (idleWatchdog idle 1 (writeIORef fired True))
        runOnce (idleTrackingMiddleware idle (slowApp 2500000))
        readIORef fired `shouldReturn` False

    it "shuts down once nothing is running any more" $ do
        idle <- armed
        fired <- newIORef False
        _ <- forkIO (idleWatchdog idle 1 (writeIORef fired True))
        runOnce (idleTrackingMiddleware idle (slowApp 0))
        threadDelay 3000000
        readIORef fired `shouldReturn` True

armed :: IO IdleState
armed = do
    idle <- newIdleState
    writeIORef (idleArmed idle) True
    pure idle

-- | An application that takes its time before answering.
slowApp :: Int -> Application
slowApp micros _ respond = do
    threadDelay micros
    respond (responseLBS status200 [] "ok")

{- | Put one request through an application. It returns when the application
has answered, which is what makes the first test a test: the watchdog gets its
chance to fire while this is still waiting.
-}
runOnce :: Application -> IO ()
runOnce app = do
    ResponseReceived <- app defaultRequest (\_ -> pure ResponseReceived)
    pure ()
