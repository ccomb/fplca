{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}

-- | Types for LCIA characterization methods.
--
-- LCIA methods define how to convert inventory results (LCI) into
-- impact assessment scores by applying characterization factors (CFs)
-- to biosphere flows.
module LCA.Method.Types
    ( -- * Method
      Method(..)
    , MethodCF(..)
    , FlowDirection(..)
      -- * Flow Mapping
    , FlowMapping(..)
    , MatchType(..)
    ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

-- | Direction of a biosphere flow (input from or output to environment)
data FlowDirection
    = Input   -- ^ Resource from environment (e.g., water, minerals)
    | Output  -- ^ Emission to environment (e.g., CO2, pollutants)
    deriving (Eq, Show, Generic, NFData, ToJSON, FromJSON)

-- | A characterization factor from a method file
--
-- Each CF defines how much impact a unit of a specific flow contributes
-- to the impact category.
data MethodCF = MethodCF
    { mcfFlowRef   :: !UUID          -- ^ ILCD flow UUID from method file
    , mcfFlowName  :: !Text          -- ^ Flow name (for matching & display)
    , mcfDirection :: !FlowDirection -- ^ Input (resource) or Output (emission)
    , mcfValue     :: !Double        -- ^ Characterization factor value
    } deriving (Eq, Show, Generic, NFData, ToJSON, FromJSON)

-- | An LCIA characterization method (loaded from ILCD XML)
--
-- Methods contain a list of characterization factors that convert
-- inventory flows into impact scores.
data Method = Method
    { methodId          :: !UUID        -- ^ Method UUID
    , methodName        :: !Text        -- ^ Human-readable name
    , methodDescription :: !(Maybe Text) -- ^ Optional description
    , methodUnit        :: !Text        -- ^ Reference unit (e.g., "kg CO2 eq")
    , methodCategory    :: !Text        -- ^ Impact category (e.g., "Climate change")
    , methodMethodology :: !(Maybe Text) -- ^ Methodology (e.g., "Environmental Footprint")
    , methodFactors     :: ![MethodCF]  -- ^ List of characterization factors
    } deriving (Eq, Show, Generic, NFData, ToJSON, FromJSON)

-- | How a method flow was matched to a database flow
data MatchType
    = ExactUUID           -- ^ Same UUID
    | ExactName           -- ^ Same normalized name
    | SynonymMatch !Int   -- ^ Via synonym group ID
    | FuzzyMatch !Double  -- ^ Fuzzy similarity score (0-1)
    | Unmatched           -- ^ No match found
    deriving (Eq, Show, Generic, NFData, ToJSON, FromJSON)

-- | Mapping between a method flow and a database flow
--
-- Used to track how method CFs are linked to the actual flows
-- in the database being analyzed.
data FlowMapping = FlowMapping
    { fmMethodFlowRef  :: !UUID         -- ^ Flow UUID from method file
    , fmMethodFlowName :: !Text         -- ^ Flow name in method
    , fmDbFlowId       :: !(Maybe UUID) -- ^ Matched database flow (if found)
    , fmMatchType      :: !MatchType    -- ^ How the match was determined
    , fmConfidence     :: !Double       -- ^ Match confidence (0.0-1.0)
    } deriving (Eq, Show, Generic, NFData, ToJSON, FromJSON)
