{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module ACV.API where

import ACV.Types
import ACV.Query
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import GHC.Generics
import Servant

-- | API type definition - RESTful design with focused endpoints
type ACVAPI = "api" :> "v1" :> (
       "process" :> Capture "uuid" Text :> Get '[JSON] ProcessInfo
  :<|> "process" :> Capture "uuid" Text :> "flows" :> Get '[JSON] [FlowSummary]
  :<|> "process" :> Capture "uuid" Text :> "inputs" :> Get '[JSON] [ExchangeDetail]
  :<|> "process" :> Capture "uuid" Text :> "outputs" :> Get '[JSON] [ExchangeDetail]
  :<|> "process" :> Capture "uuid" Text :> "reference-product" :> Get '[JSON] FlowDetail
  :<|> "flows" :> Capture "flowId" Text :> Get '[JSON] FlowDetail
  :<|> "flows" :> Capture "flowId" Text :> "processes" :> Get '[JSON] [ProcessSummary]
  )

-- | Streamlined process information - core data only
data ProcessInfo = ProcessInfo
    { piProcess :: Process              -- Core process with exchanges
    , piMetadata :: ProcessMetadata     -- Extended metadata
    , piStatistics :: ProcessStats      -- Usage statistics
    , piLinks :: ProcessLinks           -- Links to sub-resources
    } deriving (Generic, Show)

-- | Extended process metadata
data ProcessMetadata = ProcessMetadata
    { pmTotalFlows :: Int              -- Number of unique flows used
    , pmTechnosphereInputs :: Int      -- Count of technosphere inputs
    , pmBiosphereExchanges :: Int      -- Count of biosphere exchanges  
    , pmHasReferenceProduct :: Bool    -- Whether process has reference product
    , pmReferenceProductFlow :: Maybe UUID -- Flow ID of reference product
    } deriving (Generic, Show)

-- | Links to related resources
data ProcessLinks = ProcessLinks
    { plFlowsUrl :: Text              -- URL to flows endpoint
    , plInputsUrl :: Text             -- URL to inputs endpoint  
    , plOutputsUrl :: Text            -- URL to outputs endpoint
    , plReferenceProductUrl :: Maybe Text -- URL to reference product (if exists)
    } deriving (Generic, Show)

-- | Lightweight flow information for lists
data FlowSummary = FlowSummary
    { fsFlow :: Flow                  -- Core flow data
    , fsUnitName :: Text              -- Unit name for the flow
    , fsUsageCount :: Int             -- How many processes use this flow
    , fsRole :: FlowRole              -- Role in this specific process
    } deriving (Generic, Show)

-- | Role of a flow in a specific process context
data FlowRole = InputFlow | OutputFlow | ReferenceProductFlow
    deriving (Eq, Show, Generic)

-- | Flow with additional metadata
data FlowDetail = FlowDetail
    { fdFlow :: Flow
    , fdUnitName :: Text      -- Unit name for the flow
    , fdUsageCount :: Int     -- How many processes use this flow
    } deriving (Generic, Show)

-- | Exchange with flow, unit, and target process information
data ExchangeDetail = ExchangeDetail
    { edExchange :: Exchange
    , edFlow :: Flow
    , edFlowUnitName :: Text                   -- Unit name for the flow's default unit
    , edUnit :: Unit                           -- Unit information for the exchange
    , edExchangeUnitName :: Text               -- Unit name for the exchange's specific unit
    , edTargetProcess :: Maybe ProcessSummary  -- Target process for technosphere inputs
    } deriving (Generic, Show)

-- | Minimal process information for navigation
data ProcessSummary = ProcessSummary
    { prsId :: UUID
    , prsName :: Text
    , prsLocation :: Text
    } deriving (Generic, Show)

-- | Process statistics
data ProcessStats = ProcessStats
    { psInputCount :: Int
    , psOutputCount :: Int
    , psTotalExchanges :: Int
    , psLocation :: Text
    } deriving (Generic, Show)

-- JSON instances
instance ToJSON ProcessInfo
instance ToJSON ProcessMetadata
instance ToJSON ProcessLinks
instance ToJSON FlowSummary
instance ToJSON FlowRole
instance ToJSON FlowDetail  
instance ToJSON ExchangeDetail
instance ToJSON ProcessStats
instance ToJSON ProcessSummary
instance ToJSON Process
instance ToJSON Exchange
instance ToJSON Flow
instance ToJSON FlowType
instance ToJSON Unit

-- | API server implementation with multiple focused endpoints
acvServer :: Database -> Server ACVAPI
acvServer db = getProcessInfo
         :<|> getProcessFlows  
         :<|> getProcessInputs
         :<|> getProcessOutputs
         :<|> getProcessReferenceProduct
         :<|> getFlowDetail
         :<|> getFlowProcesses
  where
    -- Core process endpoint - streamlined data
    getProcessInfo :: Text -> Handler ProcessInfo
    getProcessInfo uuid = do
      case M.lookup uuid (dbProcesses db) of
        Nothing -> throwError err404 { errBody = "Process not found" }
        Just process -> do
          let metadata = calculateProcessMetadata db process
          let stats = calculateProcessStats process
          let links = generateProcessLinks uuid
          
          return $ ProcessInfo
            { piProcess = process
            , piMetadata = metadata  
            , piStatistics = stats
            , piLinks = links
            }
    
    -- Process flows sub-resource
    getProcessFlows :: Text -> Handler [FlowSummary]
    getProcessFlows uuid = do
      case M.lookup uuid (dbProcesses db) of
        Nothing -> throwError err404 { errBody = "Process not found" }
        Just process -> return $ getProcessFlowSummaries db process
    
    -- Process inputs sub-resource  
    getProcessInputs :: Text -> Handler [ExchangeDetail]
    getProcessInputs uuid = do
      case M.lookup uuid (dbProcesses db) of
        Nothing -> throwError err404 { errBody = "Process not found" }
        Just process -> return $ getProcessInputDetails db process
    
    -- Process outputs sub-resource
    getProcessOutputs :: Text -> Handler [ExchangeDetail] 
    getProcessOutputs uuid = do
      case M.lookup uuid (dbProcesses db) of
        Nothing -> throwError err404 { errBody = "Process not found" }
        Just process -> return $ getProcessOutputDetails db process
    
    -- Process reference product sub-resource
    getProcessReferenceProduct :: Text -> Handler FlowDetail
    getProcessReferenceProduct uuid = do
      case M.lookup uuid (dbProcesses db) of
        Nothing -> throwError err404 { errBody = "Process not found" }
        Just process -> do
          case getProcessReferenceProductDetail db process of
            Nothing -> throwError err404 { errBody = "No reference product found" }
            Just refProduct -> return refProduct
    
    -- Flow detail endpoint
    getFlowDetail :: Text -> Handler FlowDetail
    getFlowDetail flowId = do
      case M.lookup flowId (dbFlows db) of
        Nothing -> throwError err404 { errBody = "Flow not found" }
        Just flow -> do
          let usageCount = getFlowUsageCount db flowId
          let unitName = getUnitNameForFlow (dbUnits db) flow
          return $ FlowDetail flow unitName usageCount
    
    -- Processes using a specific flow
    getFlowProcesses :: Text -> Handler [ProcessSummary]
    getFlowProcesses flowId = do
      case M.lookup flowId (dbFlows db) of
        Nothing -> throwError err404 { errBody = "Flow not found" }
        Just _ -> return $ getProcessesUsingFlow db flowId

-- | Calculate extended metadata for a process  
calculateProcessMetadata :: Database -> Process -> ProcessMetadata
calculateProcessMetadata db process =
    let allExchanges = exchanges process
        uniqueFlows = length $ M.fromList [(exchangeFlowId ex, ()) | ex <- allExchanges]
        techInputs = length [ex | ex <- allExchanges, isTechnosphereExchange ex, exchangeIsInput ex, not (exchangeIsReference ex)]
        bioExchanges = length [ex | ex <- allExchanges, isBiosphereExchange ex]
        refProduct = case [ex | ex <- allExchanges, exchangeIsReference ex] of
                       [] -> Nothing
                       (ex:_) -> Just (exchangeFlowId ex)
    in ProcessMetadata
        { pmTotalFlows = uniqueFlows
        , pmTechnosphereInputs = techInputs  
        , pmBiosphereExchanges = bioExchanges
        , pmHasReferenceProduct = refProduct /= Nothing
        , pmReferenceProductFlow = refProduct
        }

-- | Generate links to sub-resources for a process
generateProcessLinks :: Text -> ProcessLinks
generateProcessLinks uuid = ProcessLinks
    { plFlowsUrl = "/api/v1/process/" <> uuid <> "/flows"
    , plInputsUrl = "/api/v1/process/" <> uuid <> "/inputs"
    , plOutputsUrl = "/api/v1/process/" <> uuid <> "/outputs"  
    , plReferenceProductUrl = Just ("/api/v1/process/" <> uuid <> "/reference-product")
    }

-- | Get flows used by a process as lightweight summaries
getProcessFlowSummaries :: Database -> Process -> [FlowSummary]
getProcessFlowSummaries db process =
    [ FlowSummary flow (getUnitNameForFlow (dbUnits db) flow) (getFlowUsageCount db (flowId flow)) (determineFlowRole exchange)
    | exchange <- exchanges process
    , Just flow <- [M.lookup (exchangeFlowId exchange) (dbFlows db)]
    ]
  where
    determineFlowRole ex
        | exchangeIsReference ex = ReferenceProductFlow
        | exchangeIsInput ex = InputFlow  
        | otherwise = OutputFlow

-- | Get reference product as FlowDetail (if exists)
getProcessReferenceProductDetail :: Database -> Process -> Maybe FlowDetail
getProcessReferenceProductDetail db process = do
    refExchange <- case filter exchangeIsReference (exchanges process) of
                     [] -> Nothing
                     (ex:_) -> Just ex
    flow <- M.lookup (exchangeFlowId refExchange) (dbFlows db)
    let usageCount = getFlowUsageCount db (flowId flow)
    let unitName = getUnitNameForFlow (dbUnits db) flow
    return $ FlowDetail flow unitName usageCount

-- | Get processes that use a specific flow as ProcessSummary list
getProcessesUsingFlow :: Database -> UUID -> [ProcessSummary]  
getProcessesUsingFlow db flowUUID =
    case M.lookup flowUUID (idxByFlow $ dbIndexes db) of
        Nothing -> []
        Just processUUIDs -> 
            [ ProcessSummary (processId proc) (processName proc) (processLocation proc)
            | procUUID <- processUUIDs
            , Just proc <- [M.lookup procUUID (dbProcesses db)]
            ]

-- | Get all flows used by a process with usage statistics (legacy function - kept for compatibility)
getProcessFlows :: Database -> Process -> [FlowDetail]
getProcessFlows db process = 
    [ FlowDetail flow (getUnitNameForFlow (dbUnits db) flow) (getFlowUsageCount db (flowId flow))
    | exchange <- exchanges process
    , Just flow <- [M.lookup (exchangeFlowId exchange) (dbFlows db)]
    ]

-- | Get flow usage count across all processes
getFlowUsageCount :: Database -> UUID -> Int
getFlowUsageCount db flowUUID = 
    case M.lookup flowUUID (idxByFlow $ dbIndexes db) of
        Nothing -> 0
        Just processUUIDs -> length processUUIDs

-- | Get detailed input exchanges
getProcessInputDetails :: Database -> Process -> [ExchangeDetail]
getProcessInputDetails db process =
    [ ExchangeDetail exchange flow (getUnitNameForFlow (dbUnits db) flow) unit (getUnitNameForExchange (dbUnits db) exchange) (getTargetProcess db exchange)
    | exchange <- exchanges process
    , exchangeIsInput exchange
    , Just flow <- [M.lookup (exchangeFlowId exchange) (dbFlows db)]
    , Just unit <- [M.lookup (exchangeUnitId exchange) (dbUnits db)]
    ]

-- | Get detailed output exchanges  
getProcessOutputDetails :: Database -> Process -> [ExchangeDetail]
getProcessOutputDetails db process =
    [ ExchangeDetail exchange flow (getUnitNameForFlow (dbUnits db) flow) unit (getUnitNameForExchange (dbUnits db) exchange) (getTargetProcess db exchange)
    | exchange <- exchanges process
    , not (exchangeIsInput exchange)
    , Just flow <- [M.lookup (exchangeFlowId exchange) (dbFlows db)]
    , Just unit <- [M.lookup (exchangeUnitId exchange) (dbUnits db)]
    ]


-- | Get target process for technosphere navigation
getTargetProcess :: Database -> Exchange -> Maybe ProcessSummary
getTargetProcess db exchange = do
    targetId <- exchangeActivityLinkId exchange
    targetProcess <- M.lookup targetId (dbProcesses db)
    return $ ProcessSummary
        { prsId = processId targetProcess
        , prsName = processName targetProcess
        , prsLocation = processLocation targetProcess
        }

-- | Calculate process statistics
calculateProcessStats :: Process -> ProcessStats
calculateProcessStats process = ProcessStats
    { psInputCount = length $ filter exchangeIsInput (exchanges process)
    , psOutputCount = length $ filter (not . exchangeIsInput) (exchanges process)  
    , psTotalExchanges = length (exchanges process)
    , psLocation = processLocation process
    }

-- | Proxy for the API
acvAPI :: Proxy ACVAPI
acvAPI = Proxy