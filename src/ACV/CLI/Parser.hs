{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module ACV.CLI.Parser where

import ACV.CLI.Types
import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative
import qualified Options.Applicative as OA
import Text.Read (readMaybe)

-- | Main CLI parser combining global options and commands
cliParser :: Parser CLIConfig
cliParser = CLIConfig <$> globalOptionsParser <*> commandParser

-- | Global options parser (applied before commands)
globalOptionsParser :: Parser GlobalOptions
globalOptionsParser = do
  dataDir <- optional $ strOption
    ( long "data"
   <> metavar "PATH"
   <> help "Data directory (overrides $DATADIR and current directory)"
    )

  format <- optional $ option outputFormatReader
    ( long "format"
   <> metavar "FORMAT"
   <> help "Output format: json|csv|table|pretty (default depends on command)"
    )

  treeDepth <- option auto
    ( long "tree-depth"
   <> value 2
   <> metavar "DEPTH"
   <> help "Maximum tree depth for tree operations (default: 2)"
    )

  noCache <- switch
    ( long "no-cache"
   <> help "Disable caching for testing and development"
    )

  pure GlobalOptions{..}

-- | Output format reader for optparse-applicative
outputFormatReader :: ReadM OutputFormat
outputFormatReader = eitherReader $ \s ->
  case parseOutputFormat s of
    Just fmt -> Right fmt
    Nothing -> Left $ "Invalid format '" ++ s ++ "'. Valid formats: json, csv, table, pretty"

-- | Main command parser - all top-level for maximum discoverability
commandParser :: Parser Command
commandParser = subparser
  ( OA.command "server" (info (serverParser <**> helper) (progDesc "Start API server"))
 <> OA.command "activity" (info (activityParser <**> helper) (progDesc "Get basic activity information"))
 <> OA.command "tree" (info (treeParser <**> helper) (progDesc "Get supply chain tree for activity"))
 <> OA.command "inventory" (info (inventoryParser <**> helper) (progDesc "Get life cycle inventory for activity"))
 <> OA.command "flow" (info (flowParser <**> helper) (progDesc "Query flow information"))
 <> OA.command "activities" (info (searchActivitiesParser <**> helper) (progDesc "Search activities"))
 <> OA.command "flows" (info (searchFlowsParser <**> helper) (progDesc "Search flows"))
 <> OA.command "synonym-languages" (info (pure SynonymLanguages) (progDesc "List available languages"))
 <> OA.command "synonym-stats" (info (pure SynonymStats) (progDesc "Show synonym statistics"))
 <> OA.command "lcia" (info (lciaParser <**> helper) (progDesc "Compute LCIA scores with characterization method"))
  )

-- | Server command parser
serverParser :: Parser Command
serverParser = Server <$> serverOptionsParser

serverOptionsParser :: Parser ServerOptions
serverOptionsParser = do
  serverPort <- option auto
    ( long "port"
   <> short 'p'
   <> value 8080
   <> metavar "PORT"
   <> help "Server port (default: 8080)"
    )
  pure ServerOptions{..}

-- | Activity command parser (basic info only now)
activityParser :: Parser Command
activityParser = do
  uuid <- argument textReader (metavar "UUID" <> help "Activity UUID")
  pure $ Activity uuid

-- | Tree command parser (now top-level)
treeParser :: Parser Command
treeParser = do
  uuid <- argument textReader (metavar "UUID" <> help "Activity UUID for tree generation")
  depthOverride <- optional $ option auto
    ( long "depth"
   <> metavar "DEPTH"
   <> help "Override global tree depth for this operation"
    )
  pure $ Tree uuid TreeOptions{treeDepthOverride = depthOverride}

-- | Inventory command parser (now top-level)
inventoryParser :: Parser Command
inventoryParser = do
  uuid <- argument textReader (metavar "UUID" <> help "Activity UUID for inventory computation")
  pure $ Inventory uuid

-- | Flow command parser
flowParser :: Parser Command
flowParser = do
  flowId <- argument textReader (metavar "FLOW_ID" <> help "Flow ID")
  subCmd <- optional flowSubCommandParser
  pure $ Flow flowId subCmd

-- | Flow sub-command parser
flowSubCommandParser :: Parser FlowSubCommand
flowSubCommandParser = subparser
  ( OA.command "activities" (info (pure FlowActivities) (progDesc "List activities using this flow"))
  )

-- | Search activities parser (now top-level)
searchActivitiesParser :: Parser Command
searchActivitiesParser = do
  searchName <- optional $ strOption
    ( long "name"
   <> metavar "TERM"
   <> help "Search by activity name"
    )

  searchGeo <- optional $ strOption
    ( long "geo"
   <> metavar "LOCATION"
   <> help "Filter by geography (exact match)"
    )

  searchProduct <- optional $ strOption
    ( long "product"
   <> metavar "PRODUCT"
   <> help "Filter by reference product"
    )

  searchLimit <- optional $ option auto
    ( long "limit"
   <> metavar "N"
   <> help "Limit number of results (max 1000, default 50)"
    )

  searchOffset <- optional $ option auto
    ( long "offset"
   <> metavar "N"
   <> help "Offset for pagination (default 0)"
    )

  pure $ SearchActivities SearchActivitiesOptions{..}

-- | Search flows parser (now top-level)
searchFlowsParser :: Parser Command
searchFlowsParser = do
  searchQuery <- optional $ strOption
    ( long "query"
   <> short 'q'
   <> metavar "TERM"
   <> help "Search term for flow names and synonyms"
    )

  searchLang <- optional $ strOption
    ( long "lang"
   <> metavar "LANG"
   <> help "Language for synonym search"
    )

  searchFlowsLimit <- optional $ option auto
    ( long "limit"
   <> metavar "N"
   <> help "Limit number of results"
    )

  searchFlowsOffset <- optional $ option auto
    ( long "offset"
   <> metavar "N"
   <> help "Offset for pagination"
    )

  pure $ SearchFlows SearchFlowsOptions{..}

-- | LCIA command parser
lciaParser :: Parser Command
lciaParser = do
  uuid <- argument textReader (metavar "UUID" <> help "Activity UUID for LCIA computation")
  options <- lciaOptionsParser
  pure $ LCIA uuid options

-- | LCIA options parser
lciaOptionsParser :: Parser LCIAOptions
lciaOptionsParser = do
  lciaMethod <- strOption
    ( long "method"
   <> short 'm'
   <> metavar "FILE"
   <> help "Characterization method file (XML, required for LCIA)"
    )

  lciaOutput <- optional $ strOption
    ( long "output"
   <> short 'o'
   <> metavar "FILE"
   <> help "Export results to XML ILCD format"
    )

  lciaCSV <- optional $ strOption
    ( long "csv"
   <> metavar "FILE"
   <> help "Export results to CSV format"
    )

  pure LCIAOptions{..}

-- | Text reader for UUID arguments
textReader :: ReadM Text
textReader = T.pack <$> str

-- | Parser info for the complete CLI
cliParserInfo :: ParserInfo CLIConfig
cliParserInfo = info (cliParser <**> helper)
  ( fullDesc
 <> progDesc "ACV Engine - Life Cycle Assessment computation engine"
 <> header "acv-cli - Command-line interface for ACV Engine"
 <> footer "Examples:\n\
           \  acv-cli --data ./ecoinvent tree uuid --depth 3\n\
           \  acv-cli activities --name electricity --limit 10\n\
           \  acv-cli inventory uuid --format json\n\
           \  acv-cli lcia uuid --method pef.xml --csv results.csv\n\
           \  acv-cli server --port 8080"
  )