{-# LANGUAGE BangPatterns #-}

-- | Pure BM25 ranker for activity search.
--
-- The index is built once at database load time and queried per-request.
-- Documents are identified by ProcessId-equivalent Ints (0..N-1), matching
-- the activity vector layout the rest of the engine uses.
module Search.BM25
    ( BM25Index
    , buildIndex
    , addBM25Index
    , score
    ) where

import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Control.Monad.ST (runST)
import Control.Monad (forM_)

import Search.Normalize (tokenize)
import Search.BM25.Types (BM25Index(..))
import Types (Activity, Database(..), Flow, activityName, activityLocation, exchanges, exchangeIsReference, exchangeIsInput, exchangeFlowId, flowName)
import Data.UUID (UUID)

-- | Populate the BM25 index field on a Database. Called after
-- initializeRuntimeFields during database load. Idempotent.
addBM25Index :: Database -> Database
addBM25Index db = db { dbBM25Index = Just (buildIndex (dbActivities db) (dbFlows db)) }

-- BM25 hyperparameters. Defaults from the canonical Okapi BM25 paper.
k1 :: Double
k1 = 1.2

b :: Double
b = 0.75

-- | Build the BM25 index for a vector of activities.
-- Document text per activity = activity name + location + reference product name.
buildIndex :: V.Vector Activity -> Map UUID Flow -> BM25Index
buildIndex activities flowDb =
    let n = V.length activities
        tokensByDoc :: V.Vector [Text]
        tokensByDoc = V.map (documentTokens flowDb) activities

        docLengths :: VU.Vector Int
        docLengths = VU.generate n (\i -> length (tokensByDoc V.! i))

        totalLen = VU.sum docLengths
        avgDL = if n == 0 then 0 else fromIntegral totalLen / fromIntegral n

        -- Accumulate (docId, tf) entries per term, keeping the list sorted by docId
        -- by construction (we walk docs in order).
        postingsMap :: Map Text [(Int, Int)]
        postingsMap = V.ifoldl' addDoc M.empty tokensByDoc
          where
            addDoc !acc i toks =
                let tfs = termFreqs toks
                in M.foldlWithKey'
                    (\m t f -> M.insertWith prepend t [(i, f)] m)
                    acc
                    tfs
            -- new entry comes first; final toVector reverses to ascending docId order
            prepend new old = new ++ old

        toVector = VU.fromList . reverse
    in BM25Index
        { bm25Postings   = M.map toVector postingsMap
        , bm25DocLengths = docLengths
        , bm25AvgDL      = avgDL
        , bm25DocCount   = n
        }

-- | Extract searchable tokens for one activity: name + location + reference product name(s).
documentTokens :: Map UUID Flow -> Activity -> [Text]
documentTokens flowDb a =
    tokenize (activityName a)
    ++ tokenize (activityLocation a)
    ++ concatMap productTokens (exchanges a)
  where
    productTokens ex
        | exchangeIsReference ex
        , not (exchangeIsInput ex)
        , Just flow <- M.lookup (exchangeFlowId ex) flowDb
        = tokenize (flowName flow)
        | otherwise = []

-- | Count term frequencies in a document.
termFreqs :: [Text] -> Map Text Int
termFreqs = foldl' (\m t -> M.insertWith (+) t 1 m) M.empty

-- | Score all documents against a list of (already-normalized) query terms.
-- Returns a dense vector of scores indexed by docId. Documents not matching
-- any query term score 0.
score :: BM25Index -> [Text] -> VU.Vector Double
score idx queryTerms = runST $ do
    acc <- VUM.replicate (bm25DocCount idx) 0.0
    let uniqueTerms = M.keys (termFreqs queryTerms)  -- dedupe, IDF per distinct term
        n = fromIntegral (bm25DocCount idx) :: Double
        avgdl = bm25AvgDL idx
        dlFor i = fromIntegral (bm25DocLengths idx VU.! i) :: Double
    forM_ uniqueTerms $ \t ->
        case M.lookup t (bm25Postings idx) of
            Nothing -> pure ()
            Just postings -> do
                let df = fromIntegral (VU.length postings) :: Double
                    idf = log ((n - df + 0.5) / (df + 0.5) + 1)
                VU.forM_ postings $ \(docId, tf) -> do
                    let tfd = fromIntegral tf :: Double
                        dl = dlFor docId
                        denom = tfd + k1 * (1 - b + b * dl / avgdl)
                        contrib = idf * tfd * (k1 + 1) / denom
                    VUM.modify acc (+ contrib) docId
    VU.freeze acc
