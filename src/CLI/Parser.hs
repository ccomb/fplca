{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module CLI.Parser where

import CLI.Types
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative
import qualified Options.Applicative as OA
import Version (buildTarget, gitHash, gitTag, version)

-- ---------------------------------------------------------------------------
-- Option-builder helpers: collapses the @long/short/metavar/help@ boilerplate
-- around the four shapes (Text/Int option, Text/String positional arg).
-- ---------------------------------------------------------------------------

-- | @strOption@ with @long/metavar/help@, optionally a short alias.
strOpt :: String -> Maybe Char -> String -> String -> Parser String
strOpt l ms m h = strOption (long l <> maybe mempty short ms <> metavar m <> help h)

-- | @optional strOpt@ — most of the global / command parsers use this.
optStrOpt :: String -> Maybe Char -> String -> String -> Parser (Maybe String)
optStrOpt l ms m h = optional (strOpt l ms m h)

-- | Text variant: read as String then pack.
textOpt :: String -> Maybe Char -> String -> String -> Parser Text
textOpt l ms m h = T.pack <$> strOpt l ms m h

-- | @optional textOpt@.
optTextOpt :: String -> Maybe Char -> String -> String -> Parser (Maybe Text)
optTextOpt l ms m h = optional (textOpt l ms m h)

-- | @option auto@ for Int-shaped options (limit, offset, port, depth…).
intOpt :: String -> Maybe Char -> String -> String -> Parser Int
intOpt l ms m h = option auto (long l <> maybe mempty short ms <> metavar m <> help h)

-- | @optional intOpt@.
optIntOpt :: String -> Maybe Char -> String -> String -> Parser (Maybe Int)
optIntOpt l ms m h = optional (intOpt l ms m h)

-- | Positional @Text@ argument with metavar + help.
textArg :: String -> String -> Parser Text
textArg m h = T.pack <$> argument str (metavar m <> help h)

-- | Positional @String@ argument with metavar + help (for filenames).
strArg :: String -> String -> Parser String
strArg m h = argument str (metavar m <> help h)

{- | Main CLI parser combining global options and optional command
If no command is given, just load database and exit (useful for cache generation)
-}
cliParser :: Parser CLIConfig
cliParser = CLIConfig <$> globalOptionsParser <*> optional commandParser

-- | Global options parser (applied before commands)
globalOptionsParser :: Parser GlobalOptions
globalOptionsParser = do
    configFile <- optStrOpt "config" (Just 'c') "FILE" "TOML config file (server and stop run on built-in defaults without it; other commands require it)"
    dbName <- optTextOpt "db" Nothing "NAME" "Database name to query (from config file)"
    methodsDir <- optStrOpt "methods" Nothing "PATH" "Directory containing ILCD method XML files for LCIA"
    format <-
        optional $
            option
                outputFormatReader
                (long "format" <> metavar "FORMAT" <> help "Output format: json|csv|table|pretty (default depends on command)")
    jsonPath <- optTextOpt "jsonpath" Nothing "PATH" "JSONPath for CSV extraction (required with --format csv). Examples: 'results', 'activity.exchanges'"
    noCache <- switch (long "no-cache" <> help "Disable caching for testing and development")
    serverUrl <- optStrOpt "url" Nothing "URL" "Server URL for HTTP client mode (or set VOLCA_URL env var)"
    serverPassword <- optStrOpt "password" Nothing "PASSWORD" "Password for authentication (or set VOLCA_PASSWORD env var)"
    pure GlobalOptions{..}

-- | Output format reader for optparse-applicative
outputFormatReader :: ReadM OutputFormat
outputFormatReader = eitherReader $ \s ->
    case parseOutputFormat s of
        Just fmt -> Right fmt
        Nothing -> Left $ "Invalid format '" ++ s ++ "'. Valid formats: json, csv, table, pretty"

-- | Main command parser - all top-level for maximum discoverability
commandParser :: Parser Command
commandParser =
    subparser
        ( OA.command "server" (info (serverParser <**> helper) (progDesc "Start API server"))
            <> OA.command "activity" (info (activityParser <**> helper) (progDesc "Get basic activity information"))
            <> OA.command "inventory" (info (inventoryParser <**> helper) (progDesc "Get life cycle inventory for activity"))
            <> OA.command "flow" (info (flowParser <**> helper) (progDesc "Query flow information"))
            <> OA.command "activities" (info (searchActivitiesParser <**> helper) (progDesc "Search activities"))
            <> OA.command "flows" (info (searchFlowsParser <**> helper) (progDesc "Search flows"))
            <> OA.command "impacts" (info (impactsParser <**> helper) (progDesc "Compute impact assessment (LCIA) scores with a characterization method"))
            <> OA.command "debug-matrices" (info (debugMatricesParser <**> helper) (progDesc "Export targeted matrix slices for debugging"))
            <> OA.command "export-matrices" (info (exportMatricesParser <**> helper) (progDesc "Export matrices in universal format (Ecoinvent-compatible)"))
            <> OA.command "database" (info (databaseParser <**> helper) (progDesc "Manage databases (list, upload, delete)"))
            <> OA.command "method" (info (methodParser <**> helper) (progDesc "Manage method collections"))
            <> OA.command "methods" (info (pure Methods <**> helper) (progDesc "List loaded methods (flattened)"))
            <> OA.command "synonyms" (info (pure Synonyms <**> helper) (progDesc "List synonym sources"))
            <> OA.command "compartment-mappings" (info (pure CompartmentMappings <**> helper) (progDesc "List compartment mappings"))
            <> OA.command "units" (info (pure Units <**> helper) (progDesc "List unit definitions"))
            <> OA.command "flow-mapping" (info (flowMappingParser <**> helper) (progDesc "Analyze flow mapping coverage between a method and database"))
            <> OA.command "stop" (info (pure Stop <**> helper) (progDesc "Stop running server (uses --config or --url to find it)"))
            <> OA.command "repl" (info (pure Repl <**> helper) (progDesc "Interactive REPL over HTTP (connects to running server)"))
        )
        <|> subparser
            ( OA.command "dump-openapi" (info (pure DumpOpenApi) (progDesc "Dump OpenAPI spec as JSON to stdout"))
                <> OA.command "dump-mcp-tools" (info (pure DumpMcpTools) (progDesc "Dump MCP tool definitions as JSON to stdout"))
                <> internal
            )

-- | Database command parser with optional subcommand (defaults to list)
databaseParser :: Parser Command
databaseParser =
    Database . fromMaybe DbList
        <$> optional
            ( subparser
                ( OA.command "list" (info (pure DbList) (progDesc "List databases"))
                    <> OA.command "load" (info (DbLoad <$> textArg "DB" "Name of the configured database to load" <**> helper) (progDesc "Load a configured database into memory"))
                    <> OA.command "unload" (info (DbUnload <$> textArg "DB" "Name of the loaded database to unload" <**> helper) (progDesc "Unload a database from memory"))
                    <> OA.command "upload" (info (DbUpload <$> uploadArgsParser) (progDesc "Upload a database from a local file"))
                    <> OA.command "delete" (info (DbDelete <$> deleteNameParser) (progDesc "Delete a database"))
                    <> OA.command "delete-activities" (info (DbDeleteActivities <$> deleteActivitiesArgsParser <**> helper) (progDesc "Delete the whole filtered set of activities from a loaded database"))
                    <> OA.command "copy" (info (copyArgsParser <**> helper) (progDesc "Copy a loaded database under a new name"))
                    <> OA.command "relink" (info (DbRelinkMapping <$> relinkArgsParser <**> helper) (progDesc "Relink a database to a dependency using a supplier alias CSV (source/target names, optional locations)"))
                    <> OA.command "export" (info (DbExport <$> exportArgsParser <**> helper) (progDesc "Export a loaded database to a file"))
                )
            )

-- | Copy arguments parser (positional SRC and NEW_NAME)
copyArgsParser :: Parser DatabaseAction
copyArgsParser =
    DbCopy
        <$> textArg "SRC" "Name of the loaded database to copy"
        <*> textArg "NEW_NAME" "Name for the copy"

{- | Relink-with-mapping parser: positional DB, @--to@ dependency, @--mapping@
CSV path. Mirrors @db relink <db> --to <depDb> --mapping <csv>@.
-}
relinkArgsParser :: Parser DbRelinkArgs
relinkArgsParser =
    DbRelinkArgs
        <$> textArg "DB" "Name of the loaded database to relink"
        <*> textOpt "to" Nothing "DEP_DB" "Dependency database to link against"
        <*> strOpt "mapping" Nothing "CSV" "Path to the supplier alias CSV (source/target names, optional source/target locations)"

{- | Export parser: positional DB, @--format@ keyword, @--out@ file path.
Mirrors @db export <db> --format <fmt> --out <file>@.
-}
exportArgsParser :: Parser DbExportArgs
exportArgsParser =
    DbExportArgs
        <$> textArg "DB" "Name of the loaded database to export"
        <*> textOpt "format" Nothing "FMT" "Target format: simapro|ecospold1|ecospold2|ilcd|brightway"
        <*> strOpt "out" Nothing "FILE" "Output file path"

{- | Method-export parser: positional collection name, @--format@ keyword,
@--out@ file path. Mirrors @method export <name> --format <fmt> --out <file>@.
-}
mcExportArgsParser :: Parser McExportArgs
mcExportArgsParser =
    McExportArgs
        <$> textArg "NAME" "Name of the loaded method collection to export"
        <*> textOpt "format" Nothing "FMT" "Target format: simapro|csv|openlca"
        <*> strOpt "out" Nothing "FILE" "Output file path"

{- | Delete-by-selection parser: positional DB plus filter options. @--keep@ and
@--add@ may be repeated; they spare or add individual ProcessIds.
-}
deleteActivitiesArgsParser :: Parser DbDeleteArgs
deleteActivitiesArgsParser =
    DbDeleteArgs
        <$> textArg "DB" "Name of the loaded database to edit"
        <*> optTextOpt "name" Nothing "NAME" "Filter by activity name"
        <*> optTextOpt "location" Nothing "GEO" "Filter by location"
        <*> optTextOpt "product" Nothing "PRODUCT" "Filter by reference product name"
        <*> optTextOpt "class-system" Nothing "SYSTEM" "Classification system to filter on"
        <*> optTextOpt "class-value" Nothing "VALUE" "Classification value to filter on"
        <*> switch (long "exact" <> help "Exact (case-insensitive) name match instead of token-contains")
        <*> many (textOpt "keep" Nothing "PID" "Process id (activityUUID_productUUID) to spare from deletion (repeatable)")
        <*> many (textOpt "add" Nothing "PID" "Process id to add to deletion (repeatable)")
        <*> many (textOpt "id" Nothing "PID" "Delete exactly this process id (repeatable, excludes the filter options)")

-- | Method command parser with optional subcommand (defaults to list)
methodParser :: Parser Command
methodParser =
    Method . fromMaybe McList
        <$> optional
            ( subparser
                ( OA.command "list" (info (pure McList) (progDesc "List method collections"))
                    <> OA.command "upload" (info (McUpload <$> uploadArgsParser) (progDesc "Upload a method collection from a local file"))
                    <> OA.command "delete" (info (McDelete <$> deleteNameParser) (progDesc "Delete a method collection"))
                    <> OA.command "export" (info (McExport <$> mcExportArgsParser <**> helper) (progDesc "Export a loaded method collection to a file (SimaPro CSV, columnar CSV, or openLCA JSON-LD)"))
                )
            )

-- | Shared upload arguments parser (positional FILE, --name, --description)
uploadArgsParser :: Parser UploadArgs
uploadArgsParser = do
    uaFile <- strArg "FILE" "Archive or data file to upload (ZIP, 7z, tar.gz, tar.xz, XML, CSV)"
    uaName <- textOpt "name" (Just 'n') "NAME" "Display name (required)"
    uaDescription <- optTextOpt "description" Nothing "TEXT" "Optional description"
    pure UploadArgs{..}

-- | Delete name parser (positional NAME)
deleteNameParser :: Parser Text
deleteNameParser = textArg "NAME" "Name of the resource to delete"

-- | Server command parser
serverParser :: Parser Command
serverParser = Server <$> serverOptionsParser

serverOptionsParser :: Parser ServerOptions
serverOptionsParser = do
    serverPort <- optIntOpt "port" (Just 'p') "PORT" "Server port (0=OS-assigned, binds loopback only; overrides [server].port; default: 8080)"
    serverLoadDbs <-
        optional $
            option dbListReader (long "load" <> metavar "DB1,DB2,..." <> help "Comma-separated list of databases to load at startup (overrides config load=true)")
    serverDesktopMode <- switch (long "desktop" <> help "Desktop mode: print VOLCA_PORT=N on startup for launcher integration")
    serverStaticDir <- optStrOpt "static-dir" Nothing "PATH" "Override default static file directory (default: web/dist)"
    serverIdleTimeout <- option auto (long "idle-timeout" <> value 0 <> metavar "SECONDS" <> help "Shutdown after N seconds of inactivity (0=disabled, default: 0)")
    serverTreeDepth <- option auto (long "tree-depth" <> value 2 <> metavar "DEPTH" <> help "Default max depth for the /tree endpoint (default: 2)")
    pure ServerOptions{..}

-- | Reader for comma-separated list of database names
dbListReader :: ReadM [Text]
dbListReader = T.splitOn (T.pack ",") . T.pack <$> str

-- | Activity command parser (basic info only now)
activityParser :: Parser Command
activityParser = do
    uuid <- argument textReader (metavar "PROCESS_ID" <> help "ProcessId (activity_uuid_product_uuid format)")
    pure $ Activity uuid

-- | Inventory command parser (now top-level)
inventoryParser :: Parser Command
inventoryParser = do
    uuid <- argument textReader (metavar "PROCESS_ID" <> help "ProcessId (activity_uuid_product_uuid format) for inventory computation")
    pure $ Inventory uuid

-- | Flow command parser
flowParser :: Parser Command
flowParser = do
    flowId <- argument textReader (metavar "FLOW_ID" <> help "Flow ID")
    subCmd <- optional flowSubCommandParser
    pure $ Flow flowId subCmd

-- | Flow sub-command parser
flowSubCommandParser :: Parser FlowSubCommand
flowSubCommandParser =
    subparser
        (OA.command "activities" (info (pure FlowActivities) (progDesc "List activities using this flow")))

-- | Search activities parser (now top-level)
searchActivitiesParser :: Parser Command
searchActivitiesParser = do
    searchName <- optTextOpt "name" Nothing "TERM" "Search by activity name"
    searchGeo <- optTextOpt "geo" Nothing "LOCATION" "Filter by geography (exact match)"
    searchProduct <- optTextOpt "product" Nothing "PRODUCT" "Filter by reference product"
    searchLimit <- optIntOpt "limit" Nothing "N" "Limit number of results (max 1000, default 50)"
    searchOffset <- optIntOpt "offset" Nothing "N" "Offset for pagination (default 0)"
    pure $ SearchActivities SearchActivitiesOptions{..}

-- | Search flows parser (now top-level)
searchFlowsParser :: Parser Command
searchFlowsParser = do
    searchQuery <- optTextOpt "query" (Just 'q') "TERM" "Search term for flow names and synonyms"
    searchLang <- optTextOpt "lang" Nothing "LANG" "Language for synonym search"
    searchFlowsLimit <- optIntOpt "limit" Nothing "N" "Limit number of results"
    searchFlowsOffset <- optIntOpt "offset" Nothing "N" "Offset for pagination"
    pure $ SearchFlows SearchFlowsOptions{..}

-- | Impacts (LCIA) command parser
impactsParser :: Parser Command
impactsParser =
    Impacts
        <$> argument textReader (metavar "PROCESS_ID" <> help "ProcessId (activity_uuid_product_uuid format) for impact assessment")
        <*> lciaOptionsParser

-- | LCIA options parser
lciaOptionsParser :: Parser LCIAOptions
lciaOptionsParser = do
    lciaMethodId <- textOpt "method" (Just 'm') "METHOD_UUID" "Method UUID (method must be loaded on the server)"
    pure LCIAOptions{..}

-- | Debug matrices command parser
debugMatricesParser :: Parser Command
debugMatricesParser =
    DebugMatrices
        <$> argument textReader (metavar "PROCESS_ID" <> help "ProcessId (activity_uuid_product_uuid format) for matrix debugging")
        <*> debugMatricesOptionsParser

-- | Debug matrices options parser
debugMatricesOptionsParser :: Parser DebugMatricesOptions
debugMatricesOptionsParser = do
    debugOutput <- strOpt "output" (Just 'o') "FILE" "Base filename for debug output (will generate _supply_chain.csv and _biosphere_matrix.csv)"
    debugFlowFilter <- optTextOpt "flow-filter" Nothing "FLOW" "Filter to specific biosphere flow (e.g., 'Sulphur dioxide')"
    pure DebugMatricesOptions{..}

-- | Export matrices parser
exportMatricesParser :: Parser Command
exportMatricesParser = ExportMatrices <$> strArg "OUTPUT_DIR" "Output directory for matrix export"

{- | Flow mapping command parser (renamed from 'mapping' to disambiguate
from compartment-mapping and similar resources).
-}
flowMappingParser :: Parser Command
flowMappingParser = do
    methodId <- argument textReader (metavar "METHOD_UUID" <> help "UUID of the characterization method")
    showMatched <-
        switch
            ( long "matched"
                <> help "List mapped CFs with their match strategy and DB flow"
            )
    showUnmatched <-
        switch
            ( long "unmatched"
                <> help "List method CFs that found no matching DB flow"
            )
    showUncharacterized <-
        switch
            ( long "uncharacterized"
                <> help "List DB biosphere flows that no CF matched"
            )
    pure $
        FlowMapping
            MappingOptions
                { mappingMethodId = methodId
                , mappingShowMatched = showMatched
                , mappingShowUnmatched = showUnmatched
                , mappingShowUncharacterized = showUncharacterized
                }

-- | Text reader for UUID arguments
textReader :: ReadM Text
textReader = T.pack <$> str

-- | Parser info for the complete CLI
versionOption :: Parser (a -> a)
versionOption =
    infoOption
        versionString
        (long "version" <> help "Show version information")
  where
    versionString =
        "volca "
            <> version
            <> " ("
            <> gitHash
            <> (if null gitTag then "" else ", " <> gitTag)
            <> ", "
            <> buildTarget
            <> ")"

cliParserInfo :: ParserInfo CLIConfig
cliParserInfo =
    info
        (cliParser <**> versionOption <**> helper)
        ( fullDesc
            <> progDesc "VoLCA - Life Cycle Assessment computation engine"
            <> header "volca - Command-line interface for VoLCA"
            <> footer
                "Examples:\n\
                \  volca --config volca.toml server --port 8080         # Start server\n\
                \  volca --config volca.toml --db ecoinvent activities --name electricity\n\
                \  volca --config volca.toml --db ecoinvent activity UUID\n\
                \  volca --config volca.toml --db ecoinvent inventory UUID\n\
                \  volca --config volca.toml --db ecoinvent impacts UUID --method METHOD_UUID\n\
                \  volca --config volca.toml database                   # List databases\n\
                \  volca --config volca.toml database upload mydb.7z --name \"My DB\"\n\
                \  volca --config volca.toml method upload pef.zip --name \"PEF\"\n\
                \  volca --config volca.toml repl                       # Interactive mode"
        )
