{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Flow Mapping Engine
--
-- Maps characterization factor flows from LCIA methods to database flows
-- using multiple matching strategies:
-- 1. UUID match (exact)
-- 2. Name match (normalized)
-- 3. Synonym match (via synonym group)
-- 4. Fuzzy match (string similarity)
module Method.Mapping
    ( -- * Mapping functions
      mapMethodFlows
    , mapSingleFlow
      -- * LCIA scoring
    , computeLCIAScore
      -- * Matching strategies
    , MatchStrategy(..)
    , findFlowByUUID
    , findFlowByName
    , findFlowBySynonym
      -- * Statistics
    , MappingStats(..)
    , computeMappingStats
    ) where

import Data.List (find, sortBy)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Ord (comparing, Down(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)

import Matrix (Inventory)
import Types (Flow(..), FlowType(..))
import Method.Types
import SynonymDB

-- | Matching strategy used to find a flow
data MatchStrategy
    = ByUUID       -- ^ Exact UUID match
    | ByName       -- ^ Normalized name match
    | BySynonym    -- ^ Via synonym group
    | ByFuzzy      -- ^ Fuzzy string matching
    | NoMatch      -- ^ No match found
    deriving (Eq, Show)

-- | Statistics about mapping results
data MappingStats = MappingStats
    { msTotal      :: !Int  -- ^ Total CFs in method
    , msByUUID     :: !Int  -- ^ Matched by UUID
    , msByName     :: !Int  -- ^ Matched by name
    , msBySynonym  :: !Int  -- ^ Matched by synonym
    , msByFuzzy    :: !Int  -- ^ Matched by fuzzy
    , msUnmatched  :: !Int  -- ^ Not matched
    } deriving (Eq, Show)

-- | Map all method flows to database flows
--
-- Returns a list of (MethodCF, Maybe (Flow, MatchStrategy)) pairs
mapMethodFlows
    :: SynonymDB
    -> M.Map UUID Flow       -- ^ Flow database indexed by UUID
    -> M.Map Text [Flow]     -- ^ Flow database indexed by normalized name
    -> Method
    -> [(MethodCF, Maybe (Flow, MatchStrategy))]
mapMethodFlows synDB flowsByUUID flowsByName method =
    map (\cf -> (cf, mapSingleFlow synDB flowsByUUID flowsByName cf))
        (methodFactors method)

-- | Map a single method CF to a database flow
--
-- Tries matching strategies in order:
-- 1. UUID match
-- 2. Name match
-- 3. Synonym match
-- Returns the first successful match with its strategy
mapSingleFlow
    :: SynonymDB
    -> M.Map UUID Flow
    -> M.Map Text [Flow]
    -> MethodCF
    -> Maybe (Flow, MatchStrategy)
mapSingleFlow synDB flowsByUUID flowsByName cf =
    -- Try strategies in order of preference
    case findFlowByUUID flowsByUUID (mcfFlowRef cf) of
        Just flow -> Just (flow, ByUUID)
        Nothing ->
            case findFlowByName flowsByName (mcfFlowName cf) of
                Just flow -> Just (flow, ByName)
                Nothing ->
                    case findFlowBySynonym synDB flowsByName (mcfFlowName cf) of
                        Just flow -> Just (flow, BySynonym)
                        Nothing -> Nothing  -- Could add fuzzy matching here

-- | Find flow by exact UUID match
findFlowByUUID :: M.Map UUID Flow -> UUID -> Maybe Flow
findFlowByUUID flowsByUUID uuid = M.lookup uuid flowsByUUID

-- | Find flow by normalized name match
findFlowByName :: M.Map Text [Flow] -> Text -> Maybe Flow
findFlowByName flowsByName name =
    let normalized = normalizeName name
    in case M.lookup normalized flowsByName of
        Just (f:_) -> Just f  -- Take first match
        _ -> Nothing

-- | Find flow via synonym group
--
-- 1. Look up the method flow name in synonym DB
-- 2. Get all synonyms in that group
-- 3. Try to find a flow matching any synonym
findFlowBySynonym :: SynonymDB -> M.Map Text [Flow] -> Text -> Maybe Flow
findFlowBySynonym synDB flowsByName name =
    case lookupSynonymGroup synDB name of
        Nothing -> Nothing
        Just groupId ->
            case getSynonyms synDB groupId of
                Nothing -> Nothing
                Just synonyms ->
                    -- Try each synonym until we find a match
                    listToMaybe $ mapMaybe (findFlowByName flowsByName) synonyms

-- | Compute statistics about mapping results
computeMappingStats :: [(MethodCF, Maybe (Flow, MatchStrategy))] -> MappingStats
computeMappingStats mappings = MappingStats
    { msTotal = length mappings
    , msByUUID = count ByUUID
    , msByName = count ByName
    , msBySynonym = count BySynonym
    , msByFuzzy = count ByFuzzy
    , msUnmatched = length $ filter (isNothing . snd) mappings
    }
  where
    count strategy = length $ filter ((== Just strategy) . fmap snd . snd) mappings
    isNothing Nothing = True
    isNothing _ = False

-- | Build a name index for flows
--
-- Groups flows by their normalized names for efficient lookup
buildFlowNameIndex :: [Flow] -> M.Map Text [Flow]
buildFlowNameIndex flows =
    M.fromListWith (++)
        [(normalizeName (flowName f), [f]) | f <- flows]

-- | Compute LCIA score from inventory and flow mappings
--
-- For each mapped flow: score += inventory[flow_uuid] * CF_value
computeLCIAScore :: Inventory -> [(MethodCF, Maybe (Flow, MatchStrategy))] -> Double
computeLCIAScore inventory mappings =
    sum [qty * mcfValue cf | (cf, Just (flow, _)) <- mappings
                           , Just qty <- [M.lookup (flowId flow) inventory]]

-- | Convert mapping result to FlowMapping type
toFlowMapping :: (MethodCF, Maybe (Flow, MatchStrategy)) -> FlowMapping
toFlowMapping (cf, result) = FlowMapping
    { fmMethodFlowRef = mcfFlowRef cf
    , fmMethodFlowName = mcfFlowName cf
    , fmDbFlowId = fmap (flowId . fst) result
    , fmMatchType = case result of
        Nothing -> Unmatched
        Just (_, ByUUID) -> ExactUUID
        Just (_, ByName) -> ExactName
        Just (_, BySynonym) -> SynonymMatch 0  -- Could include actual group ID
        Just (_, ByFuzzy) -> FuzzyMatch 0.0
        Just (_, NoMatch) -> Unmatched
    , fmConfidence = case result of
        Nothing -> 0.0
        Just (_, ByUUID) -> 1.0
        Just (_, ByName) -> 1.0
        Just (_, BySynonym) -> 0.9
        Just (_, ByFuzzy) -> 0.5
        Just (_, NoMatch) -> 0.0
    }
