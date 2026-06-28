{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

-- | Types for the synonym database used for flow matching.
module SynonymDB.Types (
    SynonymDB (..),
    emptySynonymDB,
) where

import Control.DeepSeq (NFData)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Store (Store)
import Data.Text (Text)
import GHC.Generics (Generic)

{- | Synonym database with bidirectional lookups.

- @synNameToId@: Maps normalized flow names to synonym group IDs
- @synIdToNames@: Maps group IDs back to all names in that group
- @synEdges@: the normalized @SameAs@ pairs the classes were closed from, kept
  so the relation can be re-closed on a restricted node set (an induced
  subgraph on the used flow names) at fan-out time — a closed class cannot be
  re-split once its internal edges are gone.
-}
data SynonymDB = SynonymDB
    { synNameToId :: !(Map Text Int)
    , synIdToNames :: !(Map Int [Text])
    , synEdges :: ![(Text, Text)]
    }
    deriving (Eq, Show, Generic, NFData, Store)

-- | Empty synonym database
emptySynonymDB :: SynonymDB
emptySynonymDB = SynonymDB M.empty M.empty []
