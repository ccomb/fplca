{-# LANGUAGE OverloadedStrings #-}

module ACV.CLI.Command where

import ACV.CLI.Format
import ACV.CLI.Types (Command(..), FlowSubCommand(..), GlobalOptions(..), CLIConfig(..), OutputFormat(..), TreeOptions(..), ServerOptions(..), SearchActivitiesOptions(..), SearchFlowsOptions(..), LCIAOptions(..))
import ACV.Progress
import qualified ACV.Service
import ACV.Types (Database)
import ACV.Types.API hiding (SynonymStats)
import Control.Monad (when)
import Data.Aeson (Value, toJSON)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.Wai.Handler.Warp (run)
import Network.Wai (Application)
import Servant (serve)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

-- ACV.API imports
import ACV.API (ACVAPI, acvAPI, acvServer)

-- | Resolve data directory using priority: --data > $DATADIR > current directory
resolveDataDirectory :: GlobalOptions -> IO FilePath
resolveDataDirectory globalOpts = case dataDir globalOpts of
  Just path -> do
    reportProgress Info $ "Using data directory from --data: " ++ path
    return path
  Nothing -> do
    envDir <- lookupEnv "DATADIR"
    case envDir of
      Just path -> do
        reportProgress Info $ "Using data directory from $DATADIR: " ++ path
        return path
      Nothing -> do
        reportProgress Info "Using current directory as data directory"
        return "./"

-- | Resolve output format using command-specific defaults
resolveOutputFormat :: GlobalOptions -> Command -> OutputFormat
resolveOutputFormat globalOpts cmd = case format globalOpts of
  Just fmt -> fmt                    -- Explicit --format overrides everything
  Nothing -> defaultFormat cmd       -- Use command-specific default

-- | Execute a CLI command with global options
executeCommand :: CLIConfig -> Database -> IO ()
executeCommand (CLIConfig globalOpts cmd) database = do
  let outputFormat = resolveOutputFormat globalOpts cmd

  case cmd of
    -- Server mode - start web server
    Server serverOpts -> do
      ACV.Progress.reportError "Server mode should be handled in Main.hs to avoid circular imports"
      exitFailure

    -- Core resource queries
    Activity uuid ->
      executeActivityCommand outputFormat database uuid

    -- Tree and inventory (now top-level)
    Tree uuid treeOpts -> do
      let depth = maybe (treeDepth globalOpts) id (treeDepthOverride treeOpts)
      executeActivityTreeCommand outputFormat database uuid depth

    Inventory uuid ->
      executeActivityInventoryCommand outputFormat database uuid

    -- Flow commands
    Flow flowId Nothing ->
      executeFlowCommand outputFormat database flowId

    Flow flowId (Just FlowActivities) ->
      executeFlowActivitiesCommand outputFormat database flowId

    -- Search commands (now top-level)
    SearchActivities opts ->
      executeSearchActivitiesCommand outputFormat database opts

    SearchFlows opts ->
      executeSearchFlowsCommand outputFormat database opts

    -- Synonym commands (now top-level)
    SynonymLanguages ->
      executeSynonymLanguagesCommand outputFormat database

    SynonymStats ->
      executeSynonymStatsCommand outputFormat database

    -- LCIA computation
    LCIA uuid lciaOpts ->
      executeLCIACommand outputFormat database uuid lciaOpts

-- | Execute activity info command
executeActivityCommand :: OutputFormat -> Database -> T.Text -> IO ()
executeActivityCommand fmt database uuid = do
  case ACV.Service.getActivityInfo database uuid of
    Left err -> reportServiceError err
    Right result -> outputResult fmt result

-- | Execute activity flows command
executeActivityFlowsCommand :: OutputFormat -> Database -> T.Text -> IO ()
executeActivityFlowsCommand fmt database uuid = do
  case ACV.Service.getActivityFlows database uuid of
    Left err -> reportServiceError err
    Right result -> outputResult fmt result

-- | Execute activity inputs command
executeActivityInputsCommand :: OutputFormat -> Database -> T.Text -> IO ()
executeActivityInputsCommand fmt database uuid = do
  case ACV.Service.getActivityInputs database uuid of
    Left err -> reportServiceError err
    Right result -> outputResult fmt result

-- | Execute activity outputs command
executeActivityOutputsCommand :: OutputFormat -> Database -> T.Text -> IO ()
executeActivityOutputsCommand fmt database uuid = do
  case ACV.Service.getActivityOutputs database uuid of
    Left err -> reportServiceError err
    Right result -> outputResult fmt result

-- | Execute activity reference product command
executeActivityReferenceProductCommand :: OutputFormat -> Database -> T.Text -> IO ()
executeActivityReferenceProductCommand fmt database uuid = do
  case ACV.Service.getActivityReferenceProduct database uuid of
    Left err -> reportServiceError err
    Right result -> outputResult fmt result

-- | Execute activity tree command
executeActivityTreeCommand :: OutputFormat -> Database -> T.Text -> Int -> IO ()
executeActivityTreeCommand fmt database uuid depth = do
  case ACV.Service.getActivityTree database uuid depth of
    Left err -> reportServiceError err
    Right result -> outputResult fmt result

-- | Execute activity inventory command
executeActivityInventoryCommand :: OutputFormat -> Database -> T.Text -> IO ()
executeActivityInventoryCommand fmt database uuid = do
  reportProgress Info $ "Computing inventory for activity: " ++ T.unpack uuid
  case ACV.Service.getActivityInventory database uuid of
    Left err -> reportServiceError err
    Right result -> do
      reportProgress Info "Inventory computation completed"
      outputResult fmt result

-- | Execute flow info command
executeFlowCommand :: OutputFormat -> Database -> T.Text -> IO ()
executeFlowCommand fmt database flowId = do
  case ACV.Service.getFlowInfo database flowId of
    Left err -> reportServiceError err
    Right result -> outputResult fmt result

-- | Execute flow activities command
executeFlowActivitiesCommand :: OutputFormat -> Database -> T.Text -> IO ()
executeFlowActivitiesCommand fmt database flowId = do
  case ACV.Service.getFlowActivities database flowId of
    Left err -> reportServiceError err
    Right result -> outputResult fmt result

-- | Execute search activities command
executeSearchActivitiesCommand :: OutputFormat -> Database -> SearchActivitiesOptions -> IO ()
executeSearchActivitiesCommand fmt database opts = do
  case ACV.Service.searchActivities database
         (searchName opts) (searchGeo opts) (searchProduct opts)
         (searchLimit opts) (searchOffset opts) of
    Left err -> reportServiceError err
    Right result -> outputResult fmt result

-- | Execute search flows command
executeSearchFlowsCommand :: OutputFormat -> Database -> SearchFlowsOptions -> IO ()
executeSearchFlowsCommand fmt database opts = do
  case ACV.Service.searchFlows database
         (searchQuery opts) (searchLang opts)
         (searchFlowsLimit opts) (searchFlowsOffset opts) of
    Left err -> reportServiceError err
    Right result -> outputResult fmt result

-- | Execute synonym languages command
executeSynonymLanguagesCommand :: OutputFormat -> Database -> IO ()
executeSynonymLanguagesCommand fmt database = do
  case ACV.Service.getSynonymLanguages database of
    Left err -> reportServiceError err
    Right result -> outputResult fmt result

-- | Execute synonym stats command
executeSynonymStatsCommand :: OutputFormat -> Database -> IO ()
executeSynonymStatsCommand fmt database = do
  case ACV.Service.getSynonymStats database of
    Left err -> reportServiceError err
    Right result -> outputResult fmt result

-- | Execute LCIA command with method file
executeLCIACommand :: OutputFormat -> Database -> T.Text -> LCIAOptions -> IO ()
executeLCIACommand fmt database uuid opts = do
  reportProgress Info $ "Computing LCIA for activity: " ++ T.unpack uuid
  reportProgress Info $ "Using method file: " ++ ACV.CLI.Types.lciaMethod opts

  case ACV.Service.computeLCIA database uuid (ACV.CLI.Types.lciaMethod opts) of
    Left err -> reportServiceError err
    Right result -> do
      reportProgress Info "LCIA computation completed"

      -- Output to console in requested format
      outputResult fmt result

      -- Export to XML if requested
      case lciaOutput opts of
        Just outputPath -> do
          case ACV.Service.exportLCIAAsXML result outputPath of
            Left err -> ACV.Progress.reportError $ "XML export failed: " ++ show err
            Right _ -> reportProgress Info $ "Results exported to XML: " ++ outputPath
        Nothing -> return ()

      -- Export to CSV if requested
      case lciaCSV opts of
        Just csvPath -> do
          case ACV.Service.exportLCIAAsCSV result csvPath of
            Left err -> ACV.Progress.reportError $ "CSV export failed: " ++ show err
            Right _ -> reportProgress Info $ "Results exported to CSV: " ++ csvPath
        Nothing -> return ()

-- | Output result in the specified format
outputResult :: OutputFormat -> Value -> IO ()
outputResult fmt result = do
  BSL.putStrLn $ formatOutput fmt result

-- | Report service errors to stderr and exit
reportServiceError :: ACV.Service.ServiceError -> IO ()
reportServiceError err = do
  ACV.Progress.reportError $ "Error: " ++ show err
  exitFailure

-- | Report error to stderr (local function to avoid conflict)
reportCliError :: String -> IO ()
reportCliError msg = hPutStrLn stderr msg

