{-# LANGUAGE OverloadedStrings #-}

module ACV.CLI.Command where

import ACV.CLI.Format
import ACV.CLI.Types (Command(..), FlowSubCommand(..), GlobalOptions(..), CLIConfig(..), OutputFormat(..), TreeOptions(..), ServerOptions(..), SearchActivitiesOptions(..), SearchFlowsOptions(..), LCIAOptions(..), DebugMatricesOptions(..))
import ACV.Progress
import qualified ACV.Service
import ACV.Types (Database)
import Control.Monad (when)
import Data.Aeson (Value, toJSON)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Text (Text)
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
      jsonPathOpt = jsonPath globalOpts

  case cmd of
    -- Server mode - start web server
    Server serverOpts -> do
      ACV.Progress.reportError "Server mode should be handled in Main.hs to avoid circular imports"
      exitFailure

    -- Core resource queries
    Activity uuid ->
      executeActivityCommand outputFormat jsonPathOpt database uuid

    -- Tree and inventory (now top-level)
    Tree uuid treeOpts -> do
      let depth = maybe (treeDepth globalOpts) id (treeDepthOverride treeOpts)
      executeActivityTreeCommand outputFormat jsonPathOpt database uuid depth

    Inventory uuid ->
      executeActivityInventoryCommand outputFormat jsonPathOpt database uuid

    -- Flow commands
    Flow flowId Nothing ->
      executeFlowCommand outputFormat jsonPathOpt database flowId

    Flow flowId (Just FlowActivities) ->
      executeFlowActivitiesCommand outputFormat jsonPathOpt database flowId

    -- Search commands (now top-level)
    SearchActivities opts ->
      executeSearchActivitiesCommand outputFormat jsonPathOpt database opts

    SearchFlows opts ->
      executeSearchFlowsCommand outputFormat jsonPathOpt database opts

    -- No synonyms command - synonyms are included in flow responses

    -- LCIA computation
    LCIA uuid lciaOpts ->
      executeLCIACommand outputFormat jsonPathOpt database uuid lciaOpts

    -- Matrix debugging
    DebugMatrices uuid debugOpts ->
      executeDebugMatricesCommand database uuid debugOpts

-- | Execute activity info command
executeActivityCommand :: OutputFormat -> Maybe Text -> Database -> T.Text -> IO ()
executeActivityCommand fmt jsonPathOpt database uuid = do
  case ACV.Service.getActivityInfo database uuid of
    Left err -> reportServiceError err
    Right result -> outputResult fmt jsonPathOpt result

-- | Execute activity flows command
executeActivityFlowsCommand :: OutputFormat -> Maybe Text -> Database -> T.Text -> IO ()
executeActivityFlowsCommand fmt jsonPathOpt database uuid = do
  case ACV.Service.getActivityFlows database uuid of
    Left err -> reportServiceError err
    Right result -> outputResult fmt jsonPathOpt result

-- | Execute activity inputs command
executeActivityInputsCommand :: OutputFormat -> Maybe Text -> Database -> T.Text -> IO ()
executeActivityInputsCommand fmt jsonPathOpt database uuid = do
  case ACV.Service.getActivityInputs database uuid of
    Left err -> reportServiceError err
    Right result -> outputResult fmt jsonPathOpt result

-- | Execute activity outputs command
executeActivityOutputsCommand :: OutputFormat -> Maybe Text -> Database -> T.Text -> IO ()
executeActivityOutputsCommand fmt jsonPathOpt database uuid = do
  case ACV.Service.getActivityOutputs database uuid of
    Left err -> reportServiceError err
    Right result -> outputResult fmt jsonPathOpt result

-- | Execute activity reference product command
executeActivityReferenceProductCommand :: OutputFormat -> Maybe Text -> Database -> T.Text -> IO ()
executeActivityReferenceProductCommand fmt jsonPathOpt database uuid = do
  case ACV.Service.getActivityReferenceProduct database uuid of
    Left err -> reportServiceError err
    Right result -> outputResult fmt jsonPathOpt result

-- | Execute activity tree command
executeActivityTreeCommand :: OutputFormat -> Maybe Text -> Database -> T.Text -> Int -> IO ()
executeActivityTreeCommand fmt jsonPathOpt database uuid depth = do
  case ACV.Service.getActivityTree database uuid depth of
    Left err -> reportServiceError err
    Right result -> outputResult fmt jsonPathOpt result

-- | Execute activity inventory command
executeActivityInventoryCommand :: OutputFormat -> Maybe Text -> Database -> T.Text -> IO ()
executeActivityInventoryCommand fmt jsonPathOpt database uuid = do
  reportProgress Info $ "Computing inventory for activity: " ++ T.unpack uuid
  case ACV.Service.getActivityInventory database uuid of
    Left err -> reportServiceError err
    Right result -> do
      reportProgress Info "Inventory computation completed"
      outputResult fmt jsonPathOpt result

-- | Execute flow info command
executeFlowCommand :: OutputFormat -> Maybe Text -> Database -> T.Text -> IO ()
executeFlowCommand fmt jsonPathOpt database flowId = do
  case ACV.Service.getFlowInfo database flowId of
    Left err -> reportServiceError err
    Right result -> outputResult fmt jsonPathOpt result

-- | Execute flow activities command
executeFlowActivitiesCommand :: OutputFormat -> Maybe Text -> Database -> T.Text -> IO ()
executeFlowActivitiesCommand fmt jsonPathOpt database flowId = do
  case ACV.Service.getFlowActivities database flowId of
    Left err -> reportServiceError err
    Right result -> outputResult fmt jsonPathOpt result

-- | Execute search activities command
executeSearchActivitiesCommand :: OutputFormat -> Maybe Text -> Database -> SearchActivitiesOptions -> IO ()
executeSearchActivitiesCommand fmt jsonPathOpt database opts = do
  case ACV.Service.searchActivities database
         (searchName opts) (searchGeo opts) (searchProduct opts)
         (searchLimit opts) (searchOffset opts) of
    Left err -> reportServiceError err
    Right result -> outputResult fmt jsonPathOpt result

-- | Execute search flows command
executeSearchFlowsCommand :: OutputFormat -> Maybe Text -> Database -> SearchFlowsOptions -> IO ()
executeSearchFlowsCommand fmt jsonPathOpt database opts = do
  case ACV.Service.searchFlows database
         (searchQuery opts) (searchLang opts)
         (searchFlowsLimit opts) (searchFlowsOffset opts) of
    Left err -> reportServiceError err
    Right result -> outputResult fmt jsonPathOpt result


-- | Execute LCIA command with method file
executeLCIACommand :: OutputFormat -> Maybe Text -> Database -> T.Text -> LCIAOptions -> IO ()
executeLCIACommand fmt jsonPathOpt database uuid opts = do
  reportProgress Info $ "Computing LCIA for activity: " ++ T.unpack uuid
  reportProgress Info $ "Using method file: " ++ ACV.CLI.Types.lciaMethod opts

  case ACV.Service.computeLCIA database uuid (ACV.CLI.Types.lciaMethod opts) of
    Left err -> reportServiceError err
    Right result -> do
      reportProgress Info "LCIA computation completed"

      -- Output to console in requested format
      outputResult fmt jsonPathOpt result

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

-- | Execute matrix debugging command
executeDebugMatricesCommand :: Database -> T.Text -> DebugMatricesOptions -> IO ()
executeDebugMatricesCommand database uuid opts = do
  reportProgress Info $ "Extracting matrix debug data for activity: " ++ T.unpack uuid
  reportProgress Info $ "Output base: " ++ debugOutput opts

  case debugFlowFilter opts of
    Just flowFilter -> reportProgress Info $ "Flow filter: " ++ T.unpack flowFilter
    Nothing -> reportProgress Info "No flow filter specified (all biosphere flows)"

  case ACV.Service.exportMatrixDebugData database uuid opts of
    Left err -> reportServiceError err
    Right result -> do
      reportProgress Info "Matrix debug export completed"
      reportProgress Info $ "Supply chain data: " ++ debugOutput opts ++ "_supply_chain.csv"
      reportProgress Info $ "Biosphere matrix: " ++ debugOutput opts ++ "_biosphere_matrix.csv"

-- | Output result in the specified format
outputResult :: OutputFormat -> Maybe Text -> Value -> IO ()
outputResult fmt jsonPathOpt result = do
  BSL.putStrLn $ formatOutputWithPath fmt jsonPathOpt result

-- | Report service errors to stderr and exit
reportServiceError :: ACV.Service.ServiceError -> IO ()
reportServiceError err = do
  ACV.Progress.reportError $ "Error: " ++ show err
  exitFailure

-- | Report error to stderr (local function to avoid conflict)
reportCliError :: String -> IO ()
reportCliError msg = hPutStrLn stderr msg

