{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}

-- | Types for the synonym database used for flow matching.
--
-- The synonym database maps flow names to synonym group IDs, allowing
-- us to match flows across different nomenclatures (ILCD, ecoinvent, SimaPro).
module LCA.SynonymDB.Types
    ( SynonymDB(..)
    , emptySynonymDB
    ) where

import Data.Binary (Binary)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

-- | Synonym database with bidirectional lookups.
--
-- - @synNameToId@: Maps normalized flow names to synonym group IDs (O(1) lookup)
-- - @synIdToNames@: Maps group IDs back to all names in that group
--
-- All names are pre-normalized (lowercase, punctuation stripped).
data SynonymDB = SynonymDB
    { synNameToId  :: !(Map Text Int)      -- ^ Name → Group ID (for lookup)
    , synIdToNames :: !(Map Int [Text])    -- ^ Group ID → All names (for display)
    } deriving (Eq, Show, Generic, NFData, Binary)

-- | Empty synonym database (for testing or when not available)
emptySynonymDB :: SynonymDB
emptySynonymDB = SynonymDB M.empty M.empty
