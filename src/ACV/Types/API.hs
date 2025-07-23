{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ACV.Types.API where

import ACV.Types (UUID, Flow, Exchange, Unit)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import qualified Data.Map as M
import qualified Data.Set as S

-- | Search response combining results and count
data SearchResults a = SearchResults
    { srResults :: [a]      -- The actual search results
    , srTotal :: Int        -- Total count of all matching items (before pagination)
    , srOffset :: Int       -- Starting offset for pagination
    , srLimit :: Int        -- Maximum number of results requested
    , srHasMore :: Bool     -- Whether there are more results available
    }
    deriving (Generic, Show)

-- | Minimal activity information for navigation
data ActivitySummary = ActivitySummary
    { prsId :: UUID
    , prsName :: Text
    , prsLocation :: Text
    }
    deriving (Generic, Show)

-- | Minimal flow information for search results
data FlowSearchResult = FlowSearchResult
    { fsrId :: UUID
    , fsrName :: Text
    , fsrCategory :: Text
    , fsrUnitName :: Text
    }
    deriving (Generic, Show)

-- | Inventory export data structures
data InventoryExport = InventoryExport
    { ieMetadata :: InventoryMetadata
    , ieFlows :: [InventoryFlowDetail]
    , ieStatistics :: InventoryStatistics
    }
    deriving (Generic, Show)

data InventoryMetadata = InventoryMetadata
    { imRootActivity :: ActivitySummary
    , imCalculationDepth :: Int
    , imTotalFlows :: Int
    , imEmissionFlows :: Int -- Biosphere outputs (negative environmental impact)
    , imResourceFlows :: Int -- Biosphere inputs (resource extraction)
    }
    deriving (Generic, Show)

data InventoryFlowDetail = InventoryFlowDetail
    { ifdFlow :: Flow
    , ifdQuantity :: Double
    , ifdUnitName :: Text
    , ifdIsEmission :: Bool -- True for emissions, False for resource extraction
    , ifdCategory :: Text -- Flow category for grouping
    }
    deriving (Generic, Show)

data InventoryStatistics = InventoryStatistics
    { isTotalQuantity :: Double -- Sum of absolute values
    , isEmissionQuantity :: Double -- Sum of emissions (should be positive)
    , isResourceQuantity :: Double -- Sum of resource extraction (should be positive)
    , isTopCategories :: [(Text, Int)] -- Top flow categories by count
    }
    deriving (Generic, Show)

-- | Tree export data structures for visualization
data TreeExport = TreeExport
    { teTree :: TreeMetadata
    , teNodes :: M.Map UUID ExportNode
    , teEdges :: [TreeEdge]
    }
    deriving (Generic, Show)

data TreeMetadata = TreeMetadata
    { tmRootId :: UUID
    , tmMaxDepth :: Int
    , tmTotalNodes :: Int
    , tmLoopNodes :: Int
    , tmLeafNodes :: Int
    , tmExpandableNodes :: Int -- Nodes that could expand further
    }
    deriving (Generic, Show)

data ExportNode = ExportNode
    { enId :: UUID
    , enName :: Text
    , enDescription :: [Text]
    , enLocation :: Text
    , enUnit :: Text
    , enNodeType :: NodeType
    , enDepth :: Int
    , enLoopTarget :: Maybe UUID
    , enParentId :: Maybe UUID -- For navigation back up
    , enChildrenCount :: Int -- Number of potential children for expandability
    }
    deriving (Generic, Show)

data NodeType = ActivityNode | LoopNode
    deriving (Eq, Generic, Show)

data TreeEdge = TreeEdge
    { teFrom :: UUID
    , teTo :: UUID
    , teFlow :: FlowInfo
    , teQuantity :: Double
    , teUnit :: Text
    }
    deriving (Generic, Show)

data FlowInfo = FlowInfo
    { fiId :: UUID
    , fiName :: Text
    , fiCategory :: Text
    }
    deriving (Generic, Show)

-- | Lightweight flow information for lists
data FlowSummary = FlowSummary
    { fsFlow :: Flow -- Core flow data
    , fsUnitName :: Text -- Unit name for the flow
    , fsUsageCount :: Int -- How many activities use this flow
    , fsRole :: FlowRole -- Role in this specific activity
    }
    deriving (Generic, Show)

-- | Role of a flow in a specific activity context
data FlowRole = InputFlow | OutputFlow | ReferenceProductFlow
    deriving (Eq, Show, Generic)

-- | Exchange with unit and flow information for API responses
data ExchangeWithUnit = ExchangeWithUnit
    { ewuExchange :: Exchange
    , ewuUnitName :: Text           -- Unit name for the exchange
    , ewuFlowName :: Text           -- Name of the flow being exchanged
    , ewuFlowCategory :: Text       -- Category/compartment (for biosphere) or "technosphere"
    , ewuTargetActivity :: Maybe Text  -- For technosphere: name of target activity
    }
    deriving (Generic, Show)

-- | Activity information optimized for API responses
data ActivityForAPI = ActivityForAPI
    { pfaId :: UUID
    , pfaName :: Text
    , pfaDescription :: [Text] -- Description par paragraphes
    , pfaSynonyms :: M.Map Text (S.Set Text) -- Synonymes par langue
    , pfaClassifications :: M.Map Text Text -- Classifications (ISIC, CPC, etc.)
    , pfaLocation :: Text
    , pfaUnit :: Text -- Unité de référence
    , pfaExchanges :: [ExchangeWithUnit] -- Exchanges with unit names
    }
    deriving (Generic, Show)

-- | Streamlined activity information - core data only
data ActivityInfo = ActivityInfo
    { piActivity :: ActivityForAPI -- Enhanced activity with unit names
    , piMetadata :: ActivityMetadata -- Extended metadata
    , piStatistics :: ActivityStats -- Usage statistics
    , piLinks :: ActivityLinks -- Links to sub-resources
    }
    deriving (Generic, Show)

-- | Extended activity metadata
data ActivityMetadata = ActivityMetadata
    { pmTotalFlows :: Int -- Number of unique flows used
    , pmTechnosphereInputs :: Int -- Count of technosphere inputs
    , pmBiosphereExchanges :: Int -- Count of biosphere exchanges
    , pmHasReferenceProduct :: Bool -- Whether activity has reference product
    , pmReferenceProductFlow :: Maybe UUID -- Flow ID of reference product
    }
    deriving (Generic, Show)

-- | Links to related resources
data ActivityLinks = ActivityLinks
    { plFlowsUrl :: Text -- URL to flows endpoint
    , plInputsUrl :: Text -- URL to inputs endpoint
    , plOutputsUrl :: Text -- URL to outputs endpoint
    , plReferenceProductUrl :: Maybe Text -- URL to reference product (if exists)
    }
    deriving (Generic, Show)

-- | Activity statistics
data ActivityStats = ActivityStats
    { psInputCount :: Int
    , psOutputCount :: Int
    , psTotalExchanges :: Int
    , psLocation :: Text
    }
    deriving (Generic, Show)

-- | Flow with additional metadata
data FlowDetail = FlowDetail
    { fdFlow :: Flow
    , fdUnitName :: Text -- Unit name for the flow
    , fdUsageCount :: Int -- How many activities use this flow
    }
    deriving (Generic, Show)

-- | Exchange with flow, unit, and target activity information
data ExchangeDetail = ExchangeDetail
    { edExchange :: Exchange
    , edFlow :: Flow
    , edFlowUnitName :: Text -- Unit name for the flow's default unit
    , edUnit :: Unit -- Unit information for the exchange
    , edExchangeUnitName :: Text -- Unit name for the exchange's specific unit
    , edTargetActivity :: Maybe ActivitySummary -- Target activity for technosphere inputs
    }
    deriving (Generic, Show)

-- JSON instances
instance ToJSON a => ToJSON (SearchResults a)
instance ToJSON ActivitySummary
instance ToJSON FlowSearchResult
instance ToJSON InventoryExport
instance ToJSON InventoryMetadata
instance ToJSON InventoryFlowDetail
instance ToJSON InventoryStatistics
instance ToJSON TreeExport
instance ToJSON TreeMetadata
instance ToJSON ExportNode
instance ToJSON NodeType
instance ToJSON TreeEdge
instance ToJSON FlowInfo
instance ToJSON FlowSummary
instance ToJSON FlowRole
instance ToJSON ExchangeWithUnit
instance ToJSON ActivityForAPI
instance ToJSON ActivityInfo
instance ToJSON ActivityMetadata
instance ToJSON ActivityLinks
instance ToJSON ActivityStats
instance ToJSON FlowDetail
instance ToJSON ExchangeDetail

-- FromJSON instances needed for API conversion
instance FromJSON ActivityInfo
instance FromJSON ActivityForAPI  
instance FromJSON ActivityMetadata
instance FromJSON ActivityLinks
instance FromJSON ActivityStats
instance FromJSON ExchangeWithUnit