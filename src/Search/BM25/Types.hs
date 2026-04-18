{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | BM25 index type. Lives in its own module so that Types.hs (which
-- defines Database) can reference it without pulling in the builder,
-- which in turn depends on Types.
module Search.BM25.Types
    ( BM25Index(..)
    ) where

import Control.DeepSeq (NFData)
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Vector.Unboxed as VU
import GHC.Generics (Generic)

data BM25Index = BM25Index
    { bm25Postings   :: !(Map Text (VU.Vector (Int, Int))) -- term → sorted vector of (docId, tf)
    , bm25DocLengths :: !(VU.Vector Int)                    -- docId → token count
    , bm25AvgDL      :: !Double
    , bm25DocCount   :: !Int
    }
    deriving (Generic, NFData)
