{-# LANGUAGE OverloadedStrings #-}

module LCA.CLI.Command where

import LCA.CLI.Types (Command(..), FlowSubCommand(..), GlobalOptions(..), CLIConfig(..), OutputFormat(..), TreeOptions(..), ServerOptions(..), SearchActivitiesOptions(..), SearchFlowsOptions(..), LCIAOptions(..), DebugMatricesOptions(..))
import LCA.Progress
import qualified LCA.Service
import LCA.Types (Database)
import LCA.UnitConversion (defaultUnitConfig)
import Control.Monad (when)
import Data.Aeson (Value, toJSON, encode)
import Data.Aeson.Encode.Pretty (encodePretty)
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

-- LCA.API imports
import LCA.API (LCAAPI, lcaAPI, lcaServer)

-- | Default output format for different command types
defaultFormat :: Command -> OutputFormat
defaultFormat (Server _) = JSON       -- Server always returns JSON
defaultFormat _ = Pretty               -- All other commands default to Pretty

-- | Format output with optional JSONPath for CSV extraction
formatOutputWithPath :: OutputFormat -> Maybe Text -> Value -> BSL.ByteString
formatOutputWithPath JSON _ value = encode value
formatOutputWithPath CSV _ value = encode value  -- Simple JSON for CSV (CLI users can pipe to jq)
formatOutputWithPath Table _ value = encodePretty value
formatOutputWithPath Pretty _ value = encodePretty value

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
executeCommand :: CLIConfig -> Command -> Database -> IO ()
executeCommand (CLIConfig globalOpts _) cmd database = do
  let outputFormat = resolveOutputFormat globalOpts cmd
      jsonPathOpt = jsonPath globalOpts

  case cmd of
    -- Server mode - start web server
    Server serverOpts -> do
      LCA.Progress.reportError "Server mode should be handled in Main.hs to avoid circular imports"
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

    -- Matrix export
    ExportMatrices outputDir ->
      executeExportMatricesCommand database outputDir

-- | Execute activity info command
executeActivityCommand :: OutputFormat -> Maybe Text -> Database -> T.Text -> IO ()
executeActivityCommand fmt jsonPathOpt database uuid = do
  case LCA.Service.getActivityInfo defaultUnitConfig database uuid of
    Left err -> reportServiceError err
    Right result -> outputResult fmt jsonPathOpt result

-- | Execute activity tree command
executeActivityTreeCommand :: OutputFormat -> Maybe Text -> Database -> T.Text -> Int -> IO ()
executeActivityTreeCommand fmt jsonPathOpt database uuid depth = do
  case LCA.Service.getActivityTree database uuid depth of
    Left err -> reportServiceError err
    Right result -> outputResult fmt jsonPathOpt result

-- | Execute activity inventory command
executeActivityInventoryCommand :: OutputFormat -> Maybe Text -> Database -> T.Text -> IO ()
executeActivityInventoryCommand fmt jsonPathOpt database uuid = do
  reportProgress Info $ "Computing inventory for activity: " ++ T.unpack uuid
  case LCA.Service.getActivityInventory database uuid of
    Left err -> reportServiceError err
    Right result -> do
      reportProgress Info "Inventory computation completed"
      outputResult fmt jsonPathOpt result

-- | Execute flow info command
executeFlowCommand :: OutputFormat -> Maybe Text -> Database -> T.Text -> IO ()
executeFlowCommand fmt jsonPathOpt database flowId = do
  case LCA.Service.getFlowInfo database flowId of
    Left err -> reportServiceError err
    Right result -> outputResult fmt jsonPathOpt result

-- | Execute flow activities command
executeFlowActivitiesCommand :: OutputFormat -> Maybe Text -> Database -> T.Text -> IO ()
executeFlowActivitiesCommand fmt jsonPathOpt database flowId = do
  case LCA.Service.getFlowActivities database flowId of
    Left err -> reportServiceError err
    Right result -> outputResult fmt jsonPathOpt result

-- | Execute search activities command
executeSearchActivitiesCommand :: OutputFormat -> Maybe Text -> Database -> SearchActivitiesOptions -> IO ()
executeSearchActivitiesCommand fmt jsonPathOpt database opts = do
  searchResult <- LCA.Service.searchActivities database
         (searchName opts) (searchGeo opts) (searchProduct opts)
         (searchLimit opts) (searchOffset opts)
  case searchResult of
    Left err -> reportServiceError err
    Right result -> outputResult fmt jsonPathOpt result

-- | Execute search flows command
executeSearchFlowsCommand :: OutputFormat -> Maybe Text -> Database -> SearchFlowsOptions -> IO ()
executeSearchFlowsCommand fmt jsonPathOpt database opts = do
  searchResult <- LCA.Service.searchFlows database
         (searchQuery opts) (searchLang opts)
         (searchFlowsLimit opts) (searchFlowsOffset opts)
  case searchResult of
    Left err -> reportServiceError err
    Right result -> outputResult fmt jsonPathOpt result


-- | Execute LCIA command with method file
executeLCIACommand :: OutputFormat -> Maybe Text -> Database -> T.Text -> LCIAOptions -> IO ()
executeLCIACommand fmt jsonPathOpt database uuid opts = do
  reportProgress Info $ "Computing LCIA for activity: " ++ T.unpack uuid
  reportProgress Info $ "Using method file: " ++ LCA.CLI.Types.lciaMethod opts

  case LCA.Service.computeLCIA database uuid (LCA.CLI.Types.lciaMethod opts) of
    Left err -> reportServiceError err
    Right result -> do
      reportProgress Info "LCIA computation completed"

      -- Output to console in requested format
      outputResult fmt jsonPathOpt result

      -- Export to XML if requested
      case lciaOutput opts of
        Just outputPath -> do
          case LCA.Service.exportLCIAAsXML result outputPath of
            Left err -> LCA.Progress.reportError $ "XML export failed: " ++ show err
            Right _ -> reportProgress Info $ "Results exported to XML: " ++ outputPath
        Nothing -> return ()

      -- Export to CSV if requested
      case lciaCSV opts of
        Just csvPath -> do
          case LCA.Service.exportLCIAAsCSV result csvPath of
            Left err -> LCA.Progress.reportError $ "CSV export failed: " ++ show err
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

  result <- LCA.Service.exportMatrixDebugData database uuid opts
  case result of
    Left err -> reportServiceError err
    Right _ -> do
      reportProgress Info "Matrix debug export completed"
      reportProgress Info $ "Supply chain data: " ++ debugOutput opts ++ "_supply_chain.csv"
      reportProgress Info $ "Biosphere matrix: " ++ debugOutput opts ++ "_biosphere_matrix.csv"

-- | Execute export matrices command
executeExportMatricesCommand :: Database -> FilePath -> IO ()
executeExportMatricesCommand database outputDir = do
  reportProgress Info $ "Exporting matrices to: " ++ outputDir
  LCA.Service.exportUniversalMatrixFormat outputDir database
  reportProgress Info "Matrix export completed"
  reportProgress Info $ "  - ie_index.csv (activity index)"
  reportProgress Info $ "  - ee_index.csv (biosphere flow index)"
  reportProgress Info $ "  - A_public.csv (technosphere matrix)"
  reportProgress Info $ "  - B_public.csv (biosphere matrix)"

-- | Output result in the specified format
outputResult :: OutputFormat -> Maybe Text -> Value -> IO ()
outputResult fmt jsonPathOpt result = do
  BSL.putStrLn $ formatOutputWithPath fmt jsonPathOpt result

-- | Report service errors to stderr and exit
reportServiceError :: LCA.Service.ServiceError -> IO ()
reportServiceError err = do
  LCA.Progress.reportError $ "Error: " ++ show err
  exitFailure

