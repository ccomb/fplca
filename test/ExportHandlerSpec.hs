{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Tests for the HTTP export handler's error mapping. A failed export must
surface as the right HTTP status (400 bad format / unexportable data, 404 not
loaded), never as a 200 body carrying a success flag — the response type (raw
bytes) cannot represent the latter.

The cheapest fixture that still drives the @Servant.runHandler@ boundary is an
empty 'Database.Manager.DatabaseManager' (no databases loaded), exactly as
"BatchImpactsSpec" does for its handlers.
-}
module ExportHandlerSpec (spec) where

import API.DatabaseHandlers (exportDatabaseHandler)
import API.Types (BinaryContent, ExportRequest (..))
import App.Env (AppEnv (..), runApp)
import qualified Builtin
import Config (defaultConfig)
import Data.Text (Text)
import Database.Manager (initDatabaseManager)
import Servant (Header, Headers, ServerError, errHTTPCode, runHandler)
import Test.Hspec

{- | Run the export handler against an empty database manager. The success type
has no Eq/Show, but the tests only inspect the 'Left', and the @Right _@
pattern never forces it.
-}
runExport :: Text -> Text -> IO (Either ServerError (Headers '[Header "X-Volca-Export-Warnings" Text] BinaryContent))
runExport dbName fmt = do
    dbm <- initDatabaseManager defaultConfig True
    let env =
            AppEnv
                { aeDbManager = dbm
                , aeMaxTreeDepth = 10
                , aePassword = Nothing
                , aeHostingConfig = Nothing
                , aeClassificationPresets = []
                , aeDataVersion = Builtin.builtinDataVersion
                }
    runHandler (runApp env (exportDatabaseHandler dbName (ExportRequest fmt)))

spec :: Spec
spec = describe "exportDatabaseHandler (HTTP error mapping)" $ do
    it "returns 404 when the database is not loaded" $ do
        res <- runExport "no-such-db" "simapro"
        case res of
            Left e -> errHTTPCode e `shouldBe` 404
            Right _ -> expectationFailure "expected a 404, got a successful export"

    it "returns 400 for an unknown export format" $ do
        res <- runExport "no-such-db" "not-a-format"
        case res of
            Left e -> errHTTPCode e `shouldBe` 400
            Right _ -> expectationFailure "expected a 400, got a successful export"
