{-# LANGUAGE DeriveGeneric #-}

module ACV.CLI.Types where

import Data.Text (Text)
import GHC.Generics
import Text.Read (readMaybe)

-- | Output format for CLI commands
data OutputFormat =
    JSON      -- API-compatible JSON output
  | CSV       -- Comma-separated values for data processing
  | Table     -- Human-readable table format
  | Pretty    -- Pretty-printed format with colors/formatting
  deriving (Eq, Show, Read, Generic)

-- | Global options that apply to all commands
data GlobalOptions = GlobalOptions
  { dataDir :: Maybe FilePath     -- Data directory (--data), overrides env and default
  , format :: Maybe OutputFormat  -- Output format (--format)
  , jsonPath :: Maybe Text        -- JSONPath for CSV extraction (--jsonpath)
  , treeDepth :: Int             -- Maximum tree depth (--tree-depth)
  , noCache :: Bool              -- Disable caching (--no-cache)
  } deriving (Eq, Show, Generic)

-- | Default global options
defaultGlobalOptions :: GlobalOptions
defaultGlobalOptions = GlobalOptions
  { dataDir = Nothing
  , format = Nothing
  , jsonPath = Nothing
  , treeDepth = 2
  , noCache = False
  }

-- | Main CLI commands - all top-level for maximum discoverability
data Command =
    -- Server mode
    Server ServerOptions
    -- Core resource queries
  | Activity Text                           -- Basic activity info
  | Flow Text (Maybe FlowSubCommand)       -- Flow info (keep subcommands for now)
    -- Tree and inventory promoted to top-level
  | Tree Text TreeOptions                  -- Supply chain tree
  | Inventory Text                         -- Life cycle inventory
    -- Search commands promoted to top-level
  | SearchActivities SearchActivitiesOptions   -- Search activities
  | SearchFlows SearchFlowsOptions             -- Search flows
    -- No separate synonyms command - synonyms are included in flow responses
  | LCIA Text LCIAOptions                 -- LCIA computation
  deriving (Eq, Show, Generic)

-- | Server-specific options
data ServerOptions = ServerOptions
  { serverPort :: Int           -- Server port (--port)
  } deriving (Eq, Show, Generic)

-- | Activity sub-commands (kept for flow activities only now)
data ActivitySubCommand =
    ActivityFlows                -- /activity/{uuid}/flows
  | ActivityInputs               -- /activity/{uuid}/inputs
  | ActivityOutputs              -- /activity/{uuid}/outputs
  | ActivityReferenceProduct     -- /activity/{uuid}/reference-product
  deriving (Eq, Show, Generic)

-- | Tree-specific options (can override global tree depth)
data TreeOptions = TreeOptions
  { treeDepthOverride :: Maybe Int  -- Override global --tree-depth for this tree
  } deriving (Eq, Show, Generic)

-- | Flow sub-commands
data FlowSubCommand =
    FlowActivities              -- /flow/{flowId}/activities
  deriving (Eq, Show, Generic)

-- | Search command types removed - now top-level commands

-- | Search activities options
data SearchActivitiesOptions = SearchActivitiesOptions
  { searchName :: Maybe Text      -- --name filter
  , searchGeo :: Maybe Text       -- --geo filter
  , searchProduct :: Maybe Text   -- --product filter
  , searchLimit :: Maybe Int      -- --limit for pagination
  , searchOffset :: Maybe Int     -- --offset for pagination
  } deriving (Eq, Show, Generic)

-- | Search flows options
data SearchFlowsOptions = SearchFlowsOptions
  { searchQuery :: Maybe Text     -- --query search term
  , searchLang :: Maybe Text      -- --lang language filter
  , searchFlowsLimit :: Maybe Int -- --limit for pagination
  , searchFlowsOffset :: Maybe Int -- --offset for pagination
  } deriving (Eq, Show, Generic)

-- | Synonym command types removed - now top-level commands

-- | LCIA computation options
data LCIAOptions = LCIAOptions
  { lciaMethod :: FilePath        -- --method (required for LCIA)
  , lciaOutput :: Maybe FilePath  -- --output XML export
  , lciaCSV :: Maybe FilePath     -- --csv export
  } deriving (Eq, Show, Generic)

-- | Complete CLI configuration
data CLIConfig = CLIConfig
  { globalOptions :: GlobalOptions
  , command :: Command
  } deriving (Eq, Show, Generic)

-- | Helper function to parse OutputFormat from string
parseOutputFormat :: String -> Maybe OutputFormat
parseOutputFormat s = case map toLower s of
  "json" -> Just JSON
  "csv" -> Just CSV
  "table" -> Just Table
  "pretty" -> Just Pretty
  _ -> Nothing
  where
    toLower c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c