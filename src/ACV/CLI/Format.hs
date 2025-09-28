{-# LANGUAGE OverloadedStrings #-}

module ACV.CLI.Format where

import ACV.CLI.Types
import ACV.Types.API
import ACV.Types (exchangeAmount, exchangeIsInput, flowId, flowName, flowCategory)
import Data.Aeson (Value, encode)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Printf (printf)

-- | Format output according to the specified format
formatOutput :: OutputFormat -> Value -> BSL.ByteString
formatOutput JSON value = encode value
formatOutput CSV value = formatAsCSV value
formatOutput Table value = formatAsTable value
formatOutput Pretty value = encodePretty value

-- | Default format for different command types
defaultFormat :: Command -> OutputFormat
defaultFormat (Server _) = JSON       -- Server always returns JSON
defaultFormat _ = CSV                  -- All other commands default to CSV

-- | Format Value as CSV (simplified version for now)
formatAsCSV :: Value -> BSL.ByteString
formatAsCSV value =
  -- For now, just convert to pretty JSON until we implement proper decoders
  encodePretty value

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