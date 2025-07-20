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

-- | API type definition
type ACVAPI = "api" :> "v1" :> (
    "process" :> Capture "uuid" Text :> Get '[JSON] ProcessDetail
  )

-- | Detailed process information for API response
data ProcessDetail = ProcessDetail
    { pdProcess :: Process
    , pdFlows :: [FlowDetail]
    , pdInputs :: [ExchangeDetail] 
    , pdOutputs :: [ExchangeDetail]
    , pdReferenceProduct :: Maybe FlowDetail
    , pdStatistics :: ProcessStats
    } deriving (Generic, Show)

-- | Flow with additional metadata
data FlowDetail = FlowDetail
    { fdFlow :: Flow
    , fdUsageCount :: Int  -- How many processes use this flow
    } deriving (Generic, Show)

-- | Exchange with flow and target process information
data ExchangeDetail = ExchangeDetail
    { edExchange :: Exchange
    , edFlow :: Flow
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
instance ToJSON ProcessDetail
instance ToJSON FlowDetail  
instance ToJSON ExchangeDetail
instance ToJSON ProcessStats
instance ToJSON ProcessSummary
instance ToJSON Process
instance ToJSON Exchange
instance ToJSON Flow
instance ToJSON FlowType

-- | API server implementation
acvServer :: Database -> Server ACVAPI
acvServer db = getProcessDetail
  where
    getProcessDetail :: Text -> Handler ProcessDetail
    getProcessDetail uuid = do
      case M.lookup uuid (dbProcesses db) of
        Nothing -> throwError err404 { errBody = "Process not found" }
        Just process -> do
          let processFlows = getProcessFlows db process
          let inputs = getProcessInputDetails db process
          let outputs = getProcessOutputDetails db process  
          let refProduct = getProcessReferenceProduct db process
          let stats = calculateProcessStats process
          
          return $ ProcessDetail
            { pdProcess = process
            , pdFlows = processFlows
            , pdInputs = inputs
            , pdOutputs = outputs
            , pdReferenceProduct = refProduct
            , pdStatistics = stats
            }

-- | Get all flows used by a process with usage statistics
getProcessFlows :: Database -> Process -> [FlowDetail]
getProcessFlows db process = 
    [ FlowDetail flow (getFlowUsageCount db (flowId flow))
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
    [ ExchangeDetail exchange flow (getTargetProcess db exchange)
    | exchange <- exchanges process
    , exchangeIsInput exchange
    , Just flow <- [M.lookup (exchangeFlowId exchange) (dbFlows db)]
    ]

-- | Get detailed output exchanges  
getProcessOutputDetails :: Database -> Process -> [ExchangeDetail]
getProcessOutputDetails db process =
    [ ExchangeDetail exchange flow (getTargetProcess db exchange)
    | exchange <- exchanges process
    , not (exchangeIsInput exchange)
    , Just flow <- [M.lookup (exchangeFlowId exchange) (dbFlows db)]
    ]

-- | Get reference product details
getProcessReferenceProduct :: Database -> Process -> Maybe FlowDetail
getProcessReferenceProduct db process = do
    refExchange <- findReferenceExchange process
    flow <- M.lookup (exchangeFlowId refExchange) (dbFlows db)
    let usageCount = getFlowUsageCount db (flowId flow)
    return $ FlowDetail flow usageCount
  where
    findReferenceExchange proc = 
        case filter exchangeIsReference (exchanges proc) of
            [] -> Nothing
            (ex:_) -> Just ex

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