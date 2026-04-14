{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | OpenAPI 3.0 schema instances and enrichment for the volca REST API.
--
-- This module collects the orphan 'ToSchema' instances for domain types
-- (avoids scattering them across all domain modules) and defines the
-- 'enrichWithResources' post-processor that stamps @operationId@,
-- @summary@, and long @description@ onto each operation with a matching
-- entry in 'API.Resources'. The actual spec derivation from 'LCAAPI'
-- lives in 'API.Routes' to break an otherwise-circular dependency
-- (API.Routes -> API.OpenApi -> API.Routes).
module API.OpenApi (enrichWithResources) where

import qualified API.Resources as R
import API.Resources (Resource)
import API.Types
import Control.Lens ((&), (?~), (%~), (^.))
import Data.Aeson (Value)
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import Data.OpenApi
import qualified Data.OpenApi.Lens as OA
import Data.Text (Text)
import qualified Data.Text as T
import Database.Manager (DatabaseSetupInfo, MissingSupplier, DependencySuggestion)
import Network.HTTP.Types.Method (StdMethod (..))
import Types (Exchange, Flow, FlowType, Unit)

-- | Orphan schema instance forward declaration for the login request body.
-- The real type lives in "API.Routes"; this is defined there and re-imported
-- here would create a cycle. Instead, the instance is declared adjacent to
-- the type in "API.Routes" — see 'instance ToSchema LoginRequest' there.

-- Aeson Value: used for untyped JSON endpoints (logs, version, stats, hosting)
instance ToSchema Value where
    declareNamedSchema _ = pure $ NamedSchema (Just "JsonValue") mempty

-- Domain types
instance ToSchema FlowType
instance ToSchema Unit
instance ToSchema Flow
instance ToSchema Exchange

-- Database.Manager types
instance ToSchema MissingSupplier
instance ToSchema DependencySuggestion
instance ToSchema DatabaseSetupInfo

-- API.Types
instance ToSchema ClassificationSystem
instance (ToSchema a) => ToSchema (SearchResults a)
instance ToSchema ActivitySummary
instance ToSchema ConsumerResult
instance ToSchema FlowSearchResult
instance ToSchema InventoryExport
instance ToSchema InventoryMetadata
instance ToSchema InventoryFlowDetail
instance ToSchema InventoryStatistics
instance ToSchema TreeExport
instance ToSchema TreeMetadata
instance ToSchema ExportNode
instance ToSchema NodeType
instance ToSchema EdgeType
instance ToSchema TreeEdge
instance ToSchema FlowInfo
instance ToSchema FlowSummary
instance ToSchema FlowRole
instance ToSchema GraphExport
instance ToSchema GraphNode
instance ToSchema GraphEdge
instance ToSchema LCIARequest
instance ToSchema LCIAResult
instance ToSchema LCIABatchResult
instance ToSchema FlowContributionEntry
instance ToSchema ContributingFlowsResult
instance ToSchema ActivityContribution
instance ToSchema ContributingActivitiesResult
instance ToSchema MappingStatus
instance ToSchema UnmappedFlowAPI
instance ToSchema FlowCFMapping
instance ToSchema FlowCFEntry
instance ToSchema CharacterizationResult
instance ToSchema CharacterizationEntry
instance ToSchema DatabaseListResponse
instance ToSchema DatabaseStatusAPI
instance ToSchema ActivateResponse
instance ToSchema DepLoadResult
instance ToSchema LoadDatabaseResponse
instance ToSchema UploadRequest
instance ToSchema UploadResponse
instance ToSchema MethodCollectionListResponse
instance ToSchema MethodCollectionStatusAPI
instance ToSchema RefDataListResponse
instance ToSchema RefDataStatusAPI
instance ToSchema SynonymGroupsResponse
instance ToSchema MethodSummary
instance ToSchema MethodDetail
instance ToSchema MethodFactorAPI
instance ToSchema SupplyChainResponse
instance ToSchema SupplyChainEntry
instance ToSchema SupplyChainEdge
instance ToSchema Aggregation
instance ToSchema AggregationGroup
instance ToSchema SubstitutionRequest
instance ToSchema Substitution
instance ToSchema ExchangeDetail
instance ToSchema ExchangeWithUnit
instance ToSchema ActivityForAPI
instance ToSchema ActivityInfo
instance ToSchema ActivityMetadata
instance ToSchema ActivityLinks
instance ToSchema ActivityStats
instance ToSchema FlowDetail
instance ToSchema ClassificationEntryInfo
instance ToSchema ClassificationPresetInfo
instance ToSchema BinaryContent where
    declareNamedSchema _ = pure $ NamedSchema (Just "OctetStream") $
        mempty & type_ ?~ OpenApiString & format ?~ "binary"

-- | Walk the spec and stamp metadata from 'API.Resources' onto each
-- resource-backed operation. Operations without a matching 'Resource'
-- (e.g. infrastructure endpoints like @/auth@, @/logs@, @/version@) are
-- left unchanged.
--
-- Operations with parameters also get their parameter @description@ fields
-- populated from 'Resources.params'.
enrichWithResources :: OpenApi -> OpenApi
enrichWithResources spec0 = foldr stampResource spec0 R.allResources
  where
    stampResource :: Resource -> OpenApi -> OpenApi
    stampResource r spec = case R.apiPathText r of
        Nothing             -> spec  -- MCP-only resource, no HTTP route to stamp
        Just (method, path) -> spec & OA.paths %~ InsOrdHashMap.adjust (stampPathItem r method) (T.unpack path)

    stampPathItem :: Resource -> StdMethod -> PathItem -> PathItem
    stampPathItem r method = case method of
        GET    -> OA.get    %~ fmap (stampOperation r)
        POST   -> OA.post   %~ fmap (stampOperation r)
        PUT    -> OA.put    %~ fmap (stampOperation r)
        DELETE -> OA.delete %~ fmap (stampOperation r)
        _      -> id  -- HEAD/OPTIONS/TRACE/PATCH/CONNECT: not used by VoLCA today

    stampOperation :: Resource -> Operation -> Operation
    stampOperation r op = op
        & OA.operationId ?~ R.mcpName r
        & OA.summary     ?~ firstSentence (R.description r)
        & OA.description ?~ R.description r
        & OA.parameters  %~ enrichParameters (R.params r)

-- | Update parameter descriptions in-place, keyed on name. Any parameter
-- whose name doesn't appear in the resource's param list is left as-is
-- (this covers implicit Servant-generated query params like @sort@/@order@
-- that we don't describe in 'API.Resources').
enrichParameters :: [R.Param] -> [Referenced Param] -> [Referenced Param]
enrichParameters resParams = map enrich
  where
    paramMap :: [(Text, Text)]
    paramMap = [(R.paramName p, R.paramDesc p) | p <- resParams]

    enrich :: Referenced Param -> Referenced Param
    enrich (Inline p) =
        case lookup (p ^. OA.name) paramMap of
            Just desc -> Inline (p & OA.description ?~ desc)
            Nothing   -> Inline p
    enrich ref = ref  -- $ref-style parameters (rare in servant-openapi3 output) left alone

-- | First sentence of a description, for the OpenAPI 'summary' field
-- (which should fit on one line in Swagger UI).
firstSentence :: Text -> Text
firstSentence t =
    let (before, _) = T.breakOn ". " t
    in if T.null before then t else before <> "."
