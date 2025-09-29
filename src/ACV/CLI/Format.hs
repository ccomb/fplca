{-# LANGUAGE OverloadedStrings #-}

module ACV.CLI.Format where

import ACV.CLI.Types
import ACV.Types.API
import ACV.Types (exchangeAmount, exchangeIsInput, flowId, flowName, flowCategory)
import Data.Aeson (Value(..), encode, object, (.=))
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.List (intercalate, nub, sort)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import Text.Printf (printf)

-- | Format output according to the specified format (legacy - being phased out)
formatOutput :: OutputFormat -> Value -> BSL.ByteString
formatOutput JSON value = encode value
formatOutput CSV value = formatAsCSV Nothing value  -- Fallback without jsonPath
formatOutput Table value = formatAsTable value
formatOutput Pretty value = encodePretty value

-- | Format output with optional JSONPath for CSV extraction
formatOutputWithPath :: OutputFormat -> Maybe Text -> Value -> BSL.ByteString
formatOutputWithPath JSON _ value = encode value
formatOutputWithPath CSV jsonPathOpt value = formatAsCSV jsonPathOpt value
formatOutputWithPath Table _ value = formatAsTable value
formatOutputWithPath Pretty _ value = encodePretty value

-- | Type-specific output functions (preferred approach)

-- | Output ActivityInfo in specified format
outputActivityInfo :: OutputFormat -> ActivityInfo -> IO ()
outputActivityInfo JSON info = BSL.putStrLn $ encode info
outputActivityInfo Pretty info = BSL.putStrLn $ encodePretty info
outputActivityInfo CSV info = putStrLn $ formatActivityInfoCSV info
outputActivityInfo Table info = putStrLn $ formatActivityInfoTable info

-- | Output FlowSearchResult list in specified format
outputFlowSearchResults :: OutputFormat -> SearchResults FlowSearchResult -> IO ()
outputFlowSearchResults JSON results = BSL.putStrLn $ encode results
outputFlowSearchResults Pretty results = BSL.putStrLn $ encodePretty results
outputFlowSearchResults CSV results = putStrLn $ formatSearchResultsCSV formatFlowSearchResultCSV results
outputFlowSearchResults Table results = putStrLn $ formatFlowSearchResultsTable results

-- | Output ActivitySummary list in specified format
outputActivitySummaries :: OutputFormat -> SearchResults ActivitySummary -> IO ()
outputActivitySummaries JSON results = BSL.putStrLn $ encode results
outputActivitySummaries Pretty results = BSL.putStrLn $ encodePretty results
outputActivitySummaries CSV results = putStrLn $ formatSearchResultsCSV formatActivitySummaryCSV results
outputActivitySummaries Table results = putStrLn $ formatActivitySummariesTable results

-- | Output TreeExport in specified format
outputTreeExport :: OutputFormat -> TreeExport -> IO ()
outputTreeExport JSON tree = BSL.putStrLn $ encode tree
outputTreeExport Pretty tree = BSL.putStrLn $ encodePretty tree
outputTreeExport CSV tree = putStrLn $ formatTreeExportCSV tree
outputTreeExport Table tree = putStrLn $ formatTreeExportTable tree

-- | Output InventoryExport in specified format
outputInventoryExport :: OutputFormat -> InventoryExport -> IO ()
outputInventoryExport JSON inv = BSL.putStrLn $ encode inv
outputInventoryExport Pretty inv = BSL.putStrLn $ encodePretty inv
outputInventoryExport CSV inv = putStrLn $ formatInventoryExportCSV inv
outputInventoryExport Table inv = putStrLn $ formatInventoryExportTable inv

-- | Output FlowDetail in specified format
outputFlowDetail :: OutputFormat -> FlowDetail -> IO ()
outputFlowDetail JSON flow = BSL.putStrLn $ encode flow
outputFlowDetail Pretty flow = BSL.putStrLn $ encodePretty flow
outputFlowDetail CSV flow = putStrLn $ formatFlowDetailCSV flow
outputFlowDetail Table flow = putStrLn $ formatFlowDetailTable flow

-- | Output simple text list in specified format
outputTextList :: OutputFormat -> [Text] -> IO ()
outputTextList JSON texts = BSL.putStrLn $ encode texts
outputTextList Pretty texts = BSL.putStrLn $ encodePretty texts
outputTextList CSV texts = putStrLn $ formatTextListCSV texts
outputTextList Table texts = putStrLn $ formatTextListTable texts

-- | Default format for different command types
defaultFormat :: Command -> OutputFormat
defaultFormat (Server _) = JSON       -- Server always returns JSON
defaultFormat _ = Pretty               -- All other commands default to Pretty (CSV broken for Service JSON)

-- | JSON normalization for CSV output - flattens nested JSON objects
-- | Similar to pandas.json_normalize(), converts nested objects to flat key-value pairs with dotted names
normalizeJson :: Value -> [(Text, Text)]
normalizeJson value = flattenJson "" value

-- | Flatten a JSON value into dotted key-value pairs
flattenJson :: Text -> Value -> [(Text, Text)]
flattenJson prefix (Object obj) =
  concatMap (\(k, v) ->
    let keyText = Key.toText k
        newPrefix = if T.null prefix then keyText else prefix <> "." <> keyText
    in flattenJson newPrefix v) (KM.toList obj)
flattenJson prefix (Array arr) =
  concat $ zipWith (\i v ->
    let newPrefix = if T.null prefix then T.pack (show i) else prefix <> "." <> T.pack (show i)
    in flattenJson newPrefix v) [0..] (V.toList arr)
flattenJson prefix (String s) = [(prefix, s)]
flattenJson prefix (Number n) = [(prefix, T.pack $ show n)]
flattenJson prefix (Bool b) = [(prefix, if b then "true" else "false")]
flattenJson prefix Null = [(prefix, "")]

-- | Convert normalized JSON to CSV format
normalizedToCSV :: [(Text, Text)] -> BSL.ByteString
normalizedToCSV pairs =
  let keys = nub $ sort $ map fst pairs
      header = T.intercalate "," keys
      values = map (\key -> maybe "" id (lookup key pairs)) keys
      row = T.intercalate "," (map escapeCSVText values)
  in BSL.fromStrict $ T.encodeUtf8 $ T.unlines [header, row]

-- | Escape CSV text values (handle commas and quotes)
escapeCSVText :: Text -> Text
escapeCSVText txt
  | T.any (\c -> c == ',' || c == '"' || c == '\n' || c == '\r') txt =
      "\"" <> T.replace "\"" "\"\"" txt <> "\""
  | otherwise = txt

-- | Extract values from JSON using a simple JSONPath (dot notation)
-- Examples: "srResults", "piActivity.pfaExchanges", "teEdges"
extractJSONPath :: Text -> Value -> Either String [Value]
extractJSONPath path value = do
  let pathParts = T.splitOn "." path
  result <- followPath pathParts value
  case result of
    Array arr -> Right (V.toList arr)
    single -> Right [single]  -- Single value becomes single-item array

-- | Follow a dot-separated path through JSON structure
followPath :: [Text] -> Value -> Either String Value
followPath [] value = Right value
followPath (part:rest) value = case value of
  Object obj -> case KM.lookup (Key.fromText part) obj of
    Nothing -> Left $ "Path component '" ++ T.unpack part ++ "' not found"
    Just nextValue -> followPath rest nextValue
  _ -> Left $ "Cannot access property '" ++ T.unpack part ++ "' on non-object"

-- | Format Value as CSV using JSONPath extraction or normalization
formatAsCSV :: Maybe Text -> Value -> BSL.ByteString
formatAsCSV Nothing value =
  -- Fallback: use full JSON normalization (creates wide tables)
  case value of
    Array arr -> formatArrayAsCSV (V.toList arr)
    obj -> normalizedToCSV (normalizeJson obj)
formatAsCSV (Just jsonPath) value =
  -- Extract specific path and format as CSV rows
  case extractJSONPath jsonPath value of
    Left err -> BSL.pack $ "Error extracting JSONPath '" ++ T.unpack jsonPath ++ "': " ++ err
    Right extractedValues -> formatArrayAsCSV extractedValues

-- | Format an array of JSON objects as CSV with consistent columns
formatArrayAsCSV :: [Value] -> BSL.ByteString
formatArrayAsCSV [] = ""
formatArrayAsCSV values =
  let allPairs = concatMap normalizeJson values
      allKeys = nub $ sort $ map fst allPairs
      header = T.intercalate "," allKeys
      rows = map (formatRowForKeys allKeys) values
  in BSL.fromStrict $ T.encodeUtf8 $ T.unlines (header : rows)

-- | Format a single JSON object as a CSV row with specified column order
formatRowForKeys :: [Text] -> Value -> Text
formatRowForKeys keys value =
  let pairs = normalizeJson value
      values = map (\key -> maybe "" id (lookup key pairs)) keys
  in T.intercalate "," (map escapeCSVText values)

-- | Format Value as human-readable table
formatAsTable :: Value -> BSL.ByteString
formatAsTable value =
  -- For now, use pretty JSON format
  -- TODO: Implement proper table formatting with aligned columns
  encodePretty value

-- | Format ActivityInfo as CSV
formatActivityInfoCSV :: ActivityInfo -> String
formatActivityInfoCSV activityInfo =
  let activity = piActivity activityInfo
      metadata = piMetadata activityInfo
      stats = piStatistics activityInfo
  in unlines
    [ "field,value"
    , "id," ++ T.unpack (pfaId activity)
    , "name," ++ escapeCSV (pfaName activity)
    , "location," ++ T.unpack (pfaLocation activity)
    , "unit," ++ T.unpack (pfaUnit activity)
    , "total_flows," ++ show (pmTotalFlows metadata)
    , "technosphere_inputs," ++ show (pmTechnosphereInputs metadata)
    , "biosphere_exchanges," ++ show (pmBiosphereExchanges metadata)
    , "input_count," ++ show (psInputCount stats)
    , "output_count," ++ show (psOutputCount stats)
    ]

-- | Format TreeExport as CSV
formatTreeExportCSV :: TreeExport -> String
formatTreeExportCSV treeExport =
  let nodes = M.toList (teNodes treeExport)
      edges = teEdges treeExport
      metadata = teTree treeExport
  in unlines $
    [ "=== TREE METADATA ==="
    , "root_id," ++ T.unpack (tmRootId metadata)
    , "max_depth," ++ show (tmMaxDepth metadata)
    , "total_nodes," ++ show (tmTotalNodes metadata)
    , "leaf_nodes," ++ show (tmLeafNodes metadata)
    , ""
    , "=== NODES ==="
    , "id,name,location,depth,node_type,children_count"
    ] ++
    [ T.unpack (enId node) ++ "," ++
      escapeCSV (enName node) ++ "," ++
      T.unpack (enLocation node) ++ "," ++
      show (enDepth node) ++ "," ++
      show (enNodeType node) ++ "," ++
      show (enChildrenCount node)
    | (_, node) <- nodes
    ] ++
    [ ""
    , "=== EDGES ==="
    , "from,to,flow_name,quantity,unit"
    ] ++
    [ T.unpack (teFrom edge) ++ "," ++
      T.unpack (teTo edge) ++ "," ++
      escapeCSV (fiName (teFlow edge)) ++ "," ++
      printf "%.6f" (teQuantity edge) ++ "," ++
      T.unpack (teUnit edge)
    | edge <- edges
    ]

-- | Format InventoryExport as CSV
formatInventoryExportCSV :: InventoryExport -> String
formatInventoryExportCSV inventoryExport =
  let metadata = ieMetadata inventoryExport
      flows = ieFlows inventoryExport
      stats = ieStatistics inventoryExport
  in unlines $
    [ "=== INVENTORY METADATA ==="
    , "root_activity," ++ T.unpack (prsName (imRootActivity metadata))
    , "total_flows," ++ show (imTotalFlows metadata)
    , "emission_flows," ++ show (imEmissionFlows metadata)
    , "resource_flows," ++ show (imResourceFlows metadata)
    , ""
    , "=== FLOWS ==="
    , "flow_id,flow_name,quantity,unit,category,is_emission"
    ] ++
    [ T.unpack (flowId (ifdFlow flow)) ++ "," ++
      escapeCSV (flowName (ifdFlow flow)) ++ "," ++
      printf "%.6e" (ifdQuantity flow) ++ "," ++
      T.unpack (ifdUnitName flow) ++ "," ++
      T.unpack (ifdCategory flow) ++ "," ++
      show (ifdIsEmission flow)
    | flow <- flows
    ]

-- | Format search results as CSV
formatSearchResultsCSV :: (a -> String) -> SearchResults a -> String
formatSearchResultsCSV formatter searchResults =
  let results = srResults searchResults
      meta = unlines
        [ "=== SEARCH METADATA ==="
        , "total_results," ++ show (srTotal searchResults)
        , "offset," ++ show (srOffset searchResults)
        , "limit," ++ show (srLimit searchResults)
        , "has_more," ++ show (srHasMore searchResults)
        , ""
        , "=== RESULTS ==="
        ]
  in meta ++ unlines (map formatter results)

-- | Format ActivitySummary as CSV line
formatActivitySummaryCSV :: ActivitySummary -> String
formatActivitySummaryCSV summary =
  T.unpack (prsId summary) ++ "," ++
  escapeCSV (prsName summary) ++ "," ++
  T.unpack (prsLocation summary)

-- | Format FlowSearchResult as CSV line
formatFlowSearchResultCSV :: FlowSearchResult -> String
formatFlowSearchResultCSV result =
  T.unpack (fsrId result) ++ "," ++
  escapeCSV (fsrName result) ++ "," ++
  T.unpack (fsrCategory result) ++ "," ++
  T.unpack (fsrUnitName result) ++ "," ++
  "\"" ++ show (fsrSynonyms result) ++ "\""  -- JSON-encoded synonyms in CSV

-- | Format FlowSummaries as CSV
formatFlowSummariesCSV :: [FlowSummary] -> String
formatFlowSummariesCSV flowSummaries =
  unlines $
    "flow_id,flow_name,unit,usage_count,role" :
    [ T.unpack (flowId (fsFlow summary)) ++ "," ++
      escapeCSV (flowName (fsFlow summary)) ++ "," ++
      T.unpack (fsUnitName summary) ++ "," ++
      show (fsUsageCount summary) ++ "," ++
      show (fsRole summary)
    | summary <- flowSummaries
    ]

-- | Format ExchangeDetails as CSV
formatExchangeDetailsCSV :: [ExchangeDetail] -> String
formatExchangeDetailsCSV exchanges =
  unlines $
    "exchange_id,flow_name,quantity,unit,direction,target_activity" :
    [ -- Format exchange details
      T.unpack (flowId (edFlow exchange)) ++ "," ++
      escapeCSV (flowName (edFlow exchange)) ++ "," ++
      printf "%.6e" (exchangeAmount (edExchange exchange)) ++ "," ++
      T.unpack (edExchangeUnitName exchange) ++ "," ++
      (if exchangeIsInput (edExchange exchange) then "input" else "output") ++ "," ++
      maybe "" (T.unpack . prsName) (edTargetActivity exchange)
    | exchange <- exchanges
    ]

-- | Format FlowDetail as CSV
formatFlowDetailCSV :: FlowDetail -> String
formatFlowDetailCSV flowDetail =
  let flow = fdFlow flowDetail
  in unlines
    [ "field,value"
    , "flow_id," ++ T.unpack (flowId flow)
    , "flow_name," ++ escapeCSV (flowName flow)
    , "category," ++ T.unpack (flowCategory flow)
    , "unit," ++ T.unpack (fdUnitName flowDetail)
    , "usage_count," ++ show (fdUsageCount flowDetail)
    ]

-- | Format list of texts as CSV
formatTextListCSV :: [Text] -> String
formatTextListCSV texts =
  unlines $ "value" : map T.unpack texts

-- | Table formatting functions

-- | Format ActivityInfo as human-readable table
formatActivityInfoTable :: ActivityInfo -> String
formatActivityInfoTable activityInfo =
  let activity = piActivity activityInfo
      metadata = piMetadata activityInfo
      stats = piStatistics activityInfo
  in unlines
    [ "Activity Information"
    , "==================="
    , "ID:                    " ++ T.unpack (pfaId activity)
    , "Name:                  " ++ T.unpack (pfaName activity)
    , "Location:              " ++ T.unpack (pfaLocation activity)
    , "Unit:                  " ++ T.unpack (pfaUnit activity)
    , ""
    , "Statistics:"
    , "-----------"
    , "Total flows:           " ++ show (pmTotalFlows metadata)
    , "Technosphere inputs:   " ++ show (pmTechnosphereInputs metadata)
    , "Biosphere exchanges:   " ++ show (pmBiosphereExchanges metadata)
    , "Input count:           " ++ show (psInputCount stats)
    , "Output count:          " ++ show (psOutputCount stats)
    ]

-- | Format FlowSearchResults as table
formatFlowSearchResultsTable :: SearchResults FlowSearchResult -> String
formatFlowSearchResultsTable searchResults =
  let results = srResults searchResults
      header = printf "%-36s %-30s %-20s %-10s %-30s"
        ("Flow ID" :: String) ("Name" :: String) ("Category" :: String) ("Unit" :: String) ("Synonyms" :: String)
      separator = replicate 126 '-'
      rows = map formatFlowSearchResultTableRow results
  in unlines $
    [ "Flow Search Results (" ++ show (srTotal searchResults) ++ " total)"
    , "=" ++ replicate 48 '='
    , header
    , separator
    ] ++ rows

formatFlowSearchResultTableRow :: FlowSearchResult -> String
formatFlowSearchResultTableRow result =
  let synonymsStr = if M.null (fsrSynonyms result)
        then ""
        else show (M.elems (fsrSynonyms result))
  in printf "%-36s %-30s %-20s %-10s %-30s"
    (T.unpack $ fsrId result)
    (take 30 $ T.unpack $ fsrName result)
    (take 20 $ T.unpack $ fsrCategory result)
    (take 10 $ T.unpack $ fsrUnitName result)
    (take 30 synonymsStr)

-- | Format ActivitySummaries as table
formatActivitySummariesTable :: SearchResults ActivitySummary -> String
formatActivitySummariesTable searchResults =
  let results = srResults searchResults
      header = printf "%-36s %-40s %-20s"
        ("Activity ID" :: String) ("Name" :: String) ("Location" :: String)
      separator = replicate 96 '-'
      rows = map formatActivitySummaryTableRow results
  in unlines $
    [ "Activity Search Results (" ++ show (srTotal searchResults) ++ " total)"
    , "=" ++ replicate 49 '='
    , header
    , separator
    ] ++ rows

formatActivitySummaryTableRow :: ActivitySummary -> String
formatActivitySummaryTableRow summary =
  printf "%-36s %-40s %-20s"
    (T.unpack $ prsId summary)
    (take 40 $ T.unpack $ prsName summary)
    (take 20 $ T.unpack $ prsLocation summary)

-- | Format TreeExport as table
formatTreeExportTable :: TreeExport -> String
formatTreeExportTable treeExport =
  let metadata = teTree treeExport
      nodes = M.toList (teNodes treeExport)
  in unlines $
    [ "Supply Chain Tree"
    , "=================="
    , "Root ID:           " ++ T.unpack (tmRootId metadata)
    , "Max depth:         " ++ show (tmMaxDepth metadata)
    , "Total nodes:       " ++ show (tmTotalNodes metadata)
    , "Leaf nodes:        " ++ show (tmLeafNodes metadata)
    , ""
    , "Nodes:"
    , "------"
    , printf "%-36s %-30s %-15s %s"
        ("Node ID" :: String) ("Name" :: String) ("Location" :: String) ("Depth" :: String)
    , replicate 90 '-'
    ] ++
    [ printf "%-36s %-30s %-15s %d"
        (T.unpack (enId node))
        (take 30 $ T.unpack (enName node))
        (take 15 $ T.unpack (enLocation node))
        (enDepth node)
    | (_, node) <- take 20 nodes  -- Limit to first 20 nodes for readability
    ]

-- | Format InventoryExport as table
formatInventoryExportTable :: InventoryExport -> String
formatInventoryExportTable inventoryExport =
  let metadata = ieMetadata inventoryExport
      flows = ieFlows inventoryExport
      stats = ieStatistics inventoryExport
  in unlines $
    [ "Life Cycle Inventory"
    , "===================="
    , "Root activity:     " ++ T.unpack (prsName (imRootActivity metadata))
    , "Total flows:       " ++ show (imTotalFlows metadata)
    , "Emission flows:    " ++ show (imEmissionFlows metadata)
    , "Resource flows:    " ++ show (imResourceFlows metadata)
    , ""
    , "Flow Summary:"
    , "-------------"
    , printf "%-36s %-30s %15s %-8s %-20s"
        ("Flow ID" :: String) ("Name" :: String) ("Quantity" :: String) ("Unit" :: String) ("Category" :: String)
    , replicate 120 '-'
    ] ++
    [ printf "%-36s %-30s %15.6e %-8s %-20s"
        (T.unpack $ flowId (ifdFlow flow))
        (take 30 $ T.unpack $ flowName (ifdFlow flow))
        (ifdQuantity flow)
        (take 8 $ T.unpack $ ifdUnitName flow)
        (take 20 $ T.unpack $ ifdCategory flow)
    | flow <- take 50 flows  -- Limit to first 50 flows for readability
    ]

-- | Format FlowDetail as table
formatFlowDetailTable :: FlowDetail -> String
formatFlowDetailTable flowDetail =
  let flow = fdFlow flowDetail
  in unlines
    [ "Flow Details"
    , "============"
    , "Flow ID:       " ++ T.unpack (flowId flow)
    , "Name:          " ++ T.unpack (flowName flow)
    , "Category:      " ++ T.unpack (flowCategory flow)
    , "Unit:          " ++ T.unpack (fdUnitName flowDetail)
    , "Usage count:   " ++ show (fdUsageCount flowDetail)
    ]

-- | Format text list as table
formatTextListTable :: [Text] -> String
formatTextListTable texts =
  unlines $ "Available Values:" : "=================" : map (("  " ++) . T.unpack) texts

-- | Escape CSV values that contain commas or quotes
escapeCSV :: Text -> String
escapeCSV text
  | T.any (`elem` [',', '"', '\n']) text =
      "\"" ++ T.unpack (T.replace "\"" "\"\"" text) ++ "\""
  | otherwise = T.unpack text

-- | Dummy decode functions - these would normally use proper JSON decoders
-- For now, returning Nothing to avoid compilation errors
decodeActivityInfo :: Value -> Maybe ActivityInfo
decodeActivityInfo _ = Nothing

decodeTreeExport :: Value -> Maybe TreeExport
decodeTreeExport _ = Nothing

decodeInventoryExport :: Value -> Maybe InventoryExport
decodeInventoryExport _ = Nothing

decodeSearchResults :: Value -> Maybe (SearchResults a)
decodeSearchResults _ = Nothing

decodeFlowSummaries :: Value -> Maybe [FlowSummary]
decodeFlowSummaries _ = Nothing

decodeExchangeDetails :: Value -> Maybe [ExchangeDetail]
decodeExchangeDetails _ = Nothing

decodeFlowDetail :: Value -> Maybe FlowDetail
decodeFlowDetail _ = Nothing

decodeTextList :: Value -> Maybe [Text]
decodeTextList _ = Nothing