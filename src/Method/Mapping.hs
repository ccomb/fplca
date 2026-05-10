{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{- | Flow Mapping Engine

Maps characterization factor flows from LCIA methods to database flows
using a configurable cascade of MapperHandles (plugin architecture).
Default cascade: UUID → CAS → Name → Synonym.
-}
module Method.Mapping (
    -- * Mapping functions
    mapMethodFlows,
    mapMethodToFlows,
    mapSingleFlow,
    buildMapContext,

    -- * LCIA scoring
    MethodTables (..),
    MethodIndex (..),
    LCIAOutcome (..),
    UncharacterizedFlow (..),
    SimilarCF (..),
    SimilarReason (..),
    UncharacterizedOpts (..),
    defaultUncharacterizedOpts,
    buildMethodTables,
    buildMethodIndex,
    computeLCIAScore,
    computeLCIAScoreFromTables,
    computeLCIAScoreWithDiagnostics,
    findUncharacterized,
    findSimilarCFs,
    inventoryContributions,
    processContributionsFromTables,
    lookupCFForFlow,
    lookupCFForFlowAt,
    expandSynonymMappings,

    -- * Matching strategies
    MatchStrategy (..),
    strategyFromText,
    findFlowByUUID,
    findFlowByName,
    findFlowByNameComp,
    findFlowBySynonym,
    findFlowBySynonymComp,
    findFlowByCAS,

    -- * Statistics
    MappingStats (..),
    computeMappingStats,
) where

import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON)
import Data.List (find, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isNothing)
import Data.Ord (Down (..))
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import GHC.Generics (Generic)

import Matrix (Inventory)
import qualified Matrix
import Method.ChemSynonyms (ChemSynonyms, expandedTokens)
import Method.Types
import Plugin.Types (MapContext (..), MapQuery (..), MapResult (..), MapperHandle (..))
import SynonymDB
import Types (Database (..), Flow (..), FlowDB, ProcessId, SparseTriple (..), Unit (..), UnitDB)
import UnitConversion (UnitConfig, convertUnit)

-- | Matching strategy used to find a flow
data MatchStrategy
    = -- | Exact UUID match
      ByUUID
    | -- | CAS number match
      ByCAS
    | -- | Normalized name match
      ByName
    | -- | Via synonym group
      BySynonym
    | -- | Fuzzy string matching
      ByFuzzy
    | -- | No match found
      NoMatch
    deriving (Eq, Show)

-- | Statistics about mapping results
data MappingStats = MappingStats
    { msTotal :: !Int
    -- ^ Total CFs in method
    , msByUUID :: !Int
    -- ^ Matched by UUID
    , msByCAS :: !Int
    -- ^ Matched by CAS
    , msByName :: !Int
    -- ^ Matched by name
    , msBySynonym :: !Int
    -- ^ Matched by synonym
    , msByFuzzy :: !Int
    -- ^ Matched by fuzzy
    , msUnmatched :: !Int
    -- ^ Not matched
    }
    deriving (Eq, Show)

-- | Build a MapContext from a Database (convenience for callers)
buildMapContext :: Database -> MapContext
buildMapContext db =
    MapContext
        { mcFlowsByUUID = dbFlows db
        , mcFlowsByName = dbFlowsByName db
        , mcFlowsByCAS = dbFlowsByCAS db
        , mcSynonymDB = fromMaybe emptySynonymDB (dbSynonymDB db)
        , mcActivities = M.empty
        }

{- | Map all method flows to database flows using mapper handles.
Mappers are tried in priority order (assumed pre-sorted).
-}
mapMethodFlows ::
    [MapperHandle] ->
    MapContext ->
    Method ->
    IO [(MethodCF, Maybe (Flow, MatchStrategy))]
mapMethodFlows mappers ctx method =
    mapM (\cf -> fmap (cf,) (mapSingleFlow mappers ctx cf)) (methodFactors method)

{- | Map a single CF using the mapper handle cascade.
Each mapper is tried in order; the first match wins.
-}
mapSingleFlow ::
    [MapperHandle] ->
    MapContext ->
    MethodCF ->
    IO (Maybe (Flow, MatchStrategy))
mapSingleFlow mappers ctx cf = go mappers
  where
    go [] = pure Nothing
    go (m : ms) = do
        result <- mhMatch m ctx (MatchCF cf)
        case result of
            Just mr
                | Just flow <- M.lookup (mrTargetId mr) (mcFlowsByUUID ctx) ->
                    pure $ Just (flow, strategyFromText (mrStrategy mr))
            _ -> go ms

-- | Convenience wrapper: map method CFs using the given mappers + DB.
mapMethodToFlows :: [MapperHandle] -> Database -> Method -> IO [(MethodCF, Maybe (Flow, MatchStrategy))]
mapMethodToFlows mappers db = mapMethodFlows mappers (buildMapContext db)

-- | Convert strategy text back to MatchStrategy
strategyFromText :: Text -> MatchStrategy
strategyFromText t = case T.toLower t of
    "uuid" -> ByUUID
    "cas" -> ByCAS
    "name" -> ByName
    "synonym" -> BySynonym
    "fuzzy" -> ByFuzzy
    _ -> ByFuzzy -- Unknown strategies map to fuzzy

-- ──────────────────────────────────────────────
-- Low-level matching functions (used by built-in MapperHandles)
-- ──────────────────────────────────────────────

-- | Find flow by exact UUID match
findFlowByUUID :: M.Map UUID Flow -> UUID -> Maybe Flow
findFlowByUUID flowsByUUID uuid = M.lookup uuid flowsByUUID

-- | Find flow by CAS number with compartment preference
findFlowByCAS :: M.Map Text [Flow] -> Text -> Maybe Compartment -> Maybe Flow
findFlowByCAS flowsByCAS cas mComp =
    M.lookup cas flowsByCAS >>= \flows -> pickByCompartment flows mComp

-- | Find flow by normalized name match (compartment-aware)
findFlowByName :: M.Map Text [Flow] -> Text -> Maybe Flow
findFlowByName flowsByName name = findFlowByNameComp flowsByName name Nothing

-- | Find flow by normalized name with compartment preference
findFlowByNameComp :: M.Map Text [Flow] -> Text -> Maybe Compartment -> Maybe Flow
findFlowByNameComp flowsByName name mComp =
    M.lookup (normalizeName name) flowsByName >>= \flows -> pickByCompartment flows mComp

-- | Find flow via synonym group (compartment-aware)
findFlowBySynonym :: SynonymDB -> M.Map Text [Flow] -> Text -> Maybe Flow
findFlowBySynonym synDB flowsByName name = findFlowBySynonymComp synDB flowsByName name Nothing

-- | Find flow via synonym group with compartment preference
findFlowBySynonymComp :: SynonymDB -> M.Map Text [Flow] -> Text -> Maybe Compartment -> Maybe Flow
findFlowBySynonymComp synDB flowsByName name mComp =
    case lookupSynonymGroup synDB name of
        Nothing -> Nothing
        Just gid ->
            getSynonyms synDB gid >>= \synonyms ->
                pickByCompartment (concatMap (lookupFlows flowsByName) synonyms) mComp
  where
    lookupFlows fbn syn = M.findWithDefault [] (normalizeName syn) fbn

{- | Pick the best flow match based on compartment preference.
Returns Nothing for an empty candidate list.
-}
pickByCompartment :: [Flow] -> Maybe Compartment -> Maybe Flow
pickByCompartment [] _ = Nothing
pickByCompartment (f : _) Nothing = Just f
pickByCompartment (f : fs) (Just comp) = Just $
    case find (exactCompMatch comp) (f : fs) of
        Just m -> m
        Nothing -> fromMaybe f (find (mediumMatch comp) (f : fs))
  where
    exactCompMatch (Compartment med sub _) fl =
        let cat = T.toLower (flowCategory fl)
            subcomp = maybe "" T.toLower (flowSubcompartment fl)
         in matchMedium med cat && (T.null sub || sub == subcomp || sub `T.isInfixOf` subcomp)

    mediumMatch (Compartment med _ _) fl =
        matchMedium med (T.toLower (flowCategory fl))

    matchMedium med cat
        | T.null med = True
        | med == cat = True
        | med `T.isInfixOf` cat = True
        | otherwise = False

-- | Compute statistics about mapping results
computeMappingStats :: [(MethodCF, Maybe (Flow, MatchStrategy))] -> MappingStats
computeMappingStats mappings =
    MappingStats
        { msTotal = length mappings
        , msByUUID = count ByUUID
        , msByCAS = count ByCAS
        , msByName = count ByName
        , msBySynonym = count BySynonym
        , msByFuzzy = count ByFuzzy
        , msUnmatched = length $ filter (isNothing . snd) mappings
        }
  where
    count strategy = length $ filter ((== Just strategy) . fmap snd . snd) mappings

{- | Precomputed CF lookup tables for one (database, method) pair.
Building these from raw mappings is O(n log n) over thousands of CFs, so they
should be computed once per method and reused across inventories.
-}
data MethodTables = MethodTables
    { mtUuidCF :: !(M.Map UUID (Double, Text))
    -- ^ UUID-matched CFs: exact flow id → (CF value, CF unit)
    , mtExactCF :: !(M.Map (Text, Text, Text, Maybe Text) (Double, Text))
    {- ^ (normalized name, medium, subcompartment, location) → (CF, unit).
    Location is 'Nothing' for the method's global default and 'Just XX'
    for regionalized variants (EF3.1's per-country CFs). Multiple
    locations coexist as separate keys — the arbitrary-winner collapse
    that plagued the old @(name, med, sub)@-only key is gone.
    -}
    , mtFallbackCF :: !(M.Map (Text, Text, Maybe Text) (Double, Text))
    -- ^ (normalized name, medium, location) → (CF, unit) for entries with unspecified subcompartment
    , mtCompartmentMap :: !CompartmentMap
    {- ^ Compartment normalization rules (e.g. "Emissions to air" → "air").
    Applied to both CF compartments at build time and database flow
    compartments at query time, so both sides converge to the same
    canonical form. Empty map = identity, no normalization.
    -}
    }

{- | Inverted indices over a 'Method' for the post-scoring suggester.

Built from the raw 'Method' (not 'MethodTables', which has lost the source CF
metadata after the lookup tables are constructed). Cached separately because
the suggester is opt-in and only consulted on the small uncharacterized tail.

* 'miCFs' — all CFs in source order; vector-indexed for cheap parallel arrays.
* 'miCFTokens' — parallel to 'miCFs', each CF's normalized-name tokens.
* 'miByMedium' — lowercase normalized medium → indices into 'miCFs', for
  short-circuiting candidate scans to the same compartment medium.
  Empty key holds CFs without compartment metadata.
* 'miByCAS' — normalized CAS → indices into 'miCFs'. Multiple CFs can share a
  CAS (same substance in different compartments); caller picks the best.
-}
data MethodIndex = MethodIndex
    { miCFs :: !(V.Vector MethodCF)
    , miCFTokens :: !(V.Vector (S.Set Text))
    , miByMedium :: !(M.Map Text [Int])
    , miByCAS :: !(M.Map Text [Int])
    }

{- | Result of scoring an inventory: the score plus diagnostics.

* 'loScore' — the LCIA score in the method's reference unit. Bit-equivalent
  to the previous Double-only return.
* 'loCharacterizedSum' / 'loInventoryAbsSum' — together they reveal how much
  of the inventory was actually characterized (by absolute mass). The
  difference is the silent-omission tail; small ratio means the score is
  trustworthy, large ratio means many flows had no CF.
* 'loUncharacterized' — flows with non-trivial inventory weight but no
  matching CF, ranked by 'ucfAbsWeight'. Cap and threshold via
  'UncharacterizedOpts'. Empty list ⇒ no diagnostics requested OR no flows
  above threshold.
* 'loUnknownUuids' — non-zero inventory UUIDs with no record in 'flowDB'.
  These indicate merged-metadata gaps, not mapping bugs; surface
  separately so the caller can react (per the "no silent errors" rule).
-}
data LCIAOutcome = LCIAOutcome
    { loScore :: !Double
    , loCharacterizedSum :: !Double
    , loInventoryAbsSum :: !Double
    , loUncharacterized :: ![UncharacterizedFlow]
    , loUnknownUuids :: ![UUID]
    }
    deriving (Eq, Show, Generic, NFData, ToJSON)

{- | A flow that carries inventory weight but found no matching CF.

The 'ucfSimilarCFs' field lets consumers distinguish the two cases:

* @[]@ — the method genuinely has no CF resembling this flow → legitimate gap.
* non-empty — the method has CFs that look homologous → likely mapping bug,
  worth a synonym entry in @data/flows.csv@ or a fresh PubChem regen.
-}
data UncharacterizedFlow = UncharacterizedFlow
    { ucfFlowId :: !UUID
    , ucfFlowName :: !Text
    , ucfCategory :: !Text
    , ucfSubcomp :: !(Maybe Text)
    , ucfFlowUnit :: !Text
    , ucfQuantity :: !Double
    , ucfAbsWeight :: !Double
    -- ^ |qty| / Σ|qty| over the whole inventory, in [0, 1].
    , ucfSimilarCFs :: ![SimilarCF]
    }
    deriving (Eq, Show, Generic, NFData, ToJSON)

-- | A candidate CF flagged by the suggester for an uncharacterized flow.
data SimilarCF = SimilarCF
    { scfMethodFlowName :: !Text
    , scfCAS :: !(Maybe Text)
    , scfCompartment :: !(Maybe Compartment)
    , scfScore :: !Double
    -- ^ Combined similarity in [0, 1] (max of the three signals).
    , scfReason :: !SimilarReason
    -- ^ Which signal produced this candidate — guides human validation.
    , scfCfValue :: !Double
    , scfCfUnit :: !Text
    }
    deriving (Eq, Show, Generic, NFData, ToJSON)

{- | Why a 'SimilarCF' was flagged. Carried through to the audit JSON so a
human reviewer knows what to verify (formula, CAS, or just the names).
-}
data SimilarReason
    = {- | Token-overlap Jaccard on normalized names. Catches word-order /
      punctuation variants like "Methane, biogenic" ↔ "Methane biogenic".
      -}
      SimByJaccard
    | {- | Token Jaccard after expanding via PubChem synonyms. Catches
      formula↔name pairs like "CO2" ↔ "Carbon dioxide".
      -}
      SimBySynonymExpansion
    | {- | The flow's CAS matched a CF's CAS even though names diverged.
      Highest-confidence reason.
      -}
      SimByCASBridge
    deriving (Eq, Show, Generic, NFData, ToJSON)

{- | Tunable knobs for uncharacterized-flow diagnostics.

* 'uoMinAbsWeight' — drop flows below this share of total |qty|. Defaults
  to 0.001 (0.1%) so noise doesn't drown signal.
* 'uoMaxSimilar' — top-N candidate CFs per uncharacterized flow. 0 disables
  the similarity scan (useful in hot paths).
* 'uoMaxFlows' — cap on the diagnostics list size, so payloads stay bounded
  even on inventories with many tiny gaps.
-}
data UncharacterizedOpts = UncharacterizedOpts
    { uoMinAbsWeight :: !Double
    , uoMaxSimilar :: !Int
    , uoMaxFlows :: !Int
    }
    deriving (Eq, Show)

defaultUncharacterizedOpts :: UncharacterizedOpts
defaultUncharacterizedOpts =
    UncharacterizedOpts
        { uoMinAbsWeight = 0.001
        , uoMaxSimilar = 3
        , uoMaxFlows = 50
        }

-- | Build a 'MethodIndex' from a raw 'Method'. Run once per method, cache.
buildMethodIndex :: Method -> MethodIndex
buildMethodIndex method =
    let cfs = V.fromList (methodFactors method)
        tokens = V.map cfTokens cfs
        indexed = zip [0 ..] (methodFactors method)
     in MethodIndex
            { miCFs = cfs
            , miCFTokens = tokens
            , miByMedium = M.fromListWith (++) [(cfMedium cf, [i]) | (i, cf) <- indexed]
            , miByCAS = M.fromListWith (++) [(cas, [i]) | (i, cf) <- indexed, Just cas <- [mcfCAS cf]]
            }
  where
    cfTokens :: MethodCF -> S.Set Text
    cfTokens = S.fromList . T.words . normalizeName . mcfFlowName

    cfMedium :: MethodCF -> Text
    cfMedium cf = case mcfCompartment cf of
        Nothing -> ""
        Just (Compartment med _ _) -> normalizeMedium (T.toLower med)

{- | Build 'MethodTables' from raw mappings and a 'CompartmentMap'.

The map is applied to each CF's compartment before keying the lookup
tables, so all keys land in canonical form. The same map is then stored
in the result so 'lookupCFForFlow' can normalize inventory-side
compartments at query time and meet at the same canonical keys.

Pass 'M.empty' for the compartment map when no normalization is desired
(behaves identically to the pre-CompartmentMap implementation).
-}

{- | Fan out one-to-many synonym matches: for each CF, look up the
synonym group of the CF name and emit one extra @(cf, Just (peerFlow,
BySynonym))@ row per BAFU flow whose name is in the group and whose
compartment medium matches the CF's compartment. The original mapping
list is preserved.

This is the missing piece when a method-side CF (e.g. ILCD's bare
@copper@) covers many inventory-side variants (BAFU's @Copper, 2.19% in
sulfide, …@, @Copper, 1.18% in sulfide, …@, etc.). Without expansion,
@buildMethodTables@ would key the CF under a single matched-flow name
and the other variants would silently look up as @Nothing@. With
expansion, the CF is keyed under every group member, so every inventory
variant of the same substance finds the same CF.

Duplicates are harmless — 'buildMethodTables' uses @fromListWith
preferBetter@.
-}
expandSynonymMappings ::
    SynonymDB ->
    M.Map Text [Flow] ->
    [(MethodCF, Maybe (Flow, MatchStrategy))] ->
    [(MethodCF, Maybe (Flow, MatchStrategy))]
expandSynonymMappings synDB flowsByName mappings =
    mappings ++ concatMap expand mappings
  where
    -- One-shot inverse index: normalized name → set of direct partners
    -- (union of all groups containing the name, without recursing). Stays
    -- inside 'SynonymDB''s star-topology semantics — no chained
    -- inferences across hubs — but does see every direct pair, which
    -- 'lookupSynonymGroup' alone misses when many pairs converge on the
    -- same hub name and @M.fromList@ keeps only the last-inserted group.
    directPeers :: M.Map Text (S.Set Text)
    directPeers =
        M.fromListWith
            S.union
            [ (normalizeName m, S.fromList (map normalizeName members))
            | members <- M.elems (synIdToNames synDB)
            , m <- members
            ]

    expand (cf, _) =
        let cfName = normalizeName (mcfFlowName cf)
            cfMed = case mcfCompartment cf of
                Just (Compartment med _ _) -> Just (T.toLower med)
                Nothing -> Nothing
            peers = M.findWithDefault S.empty cfName directPeers
         in [ (cf, Just (flow, BySynonym))
            | syn <- S.toList peers
            , flow <- M.findWithDefault [] syn flowsByName
            , mediumCompat cfMed (flowCategory flow)
            ]

    -- Loose medium compatibility: empty/None matches anything; otherwise
    -- check the flow's category starts with the CF medium (e.g. CF medium
    -- "natural resource" must match flow category "resources/in ground"
    -- which lowercases to "resources/...").
    mediumCompat Nothing _ = True
    mediumCompat (Just med) cat =
        let lcat = T.toLower cat
         in med == "natural resource"
                && ("resource" `T.isPrefixOf` lcat)
                || (med /= "natural resource" && med `T.isInfixOf` lcat)

buildMethodTables :: CompartmentMap -> [(MethodCF, Maybe (Flow, MatchStrategy))] -> MethodTables
buildMethodTables cmap mappings =
    MethodTables
        { mtUuidCF =
            M.fromList
                [ (flowId flow, (mcfValue cf, mcfUnit cf))
                | (cf, Just (flow, ByUUID)) <- mappings
                ]
        , mtExactCF =
            stripStrategy $
                M.fromListWith
                    preferBetter
                    [ ((nameKey cf mflow, normMed, normSub, mcfLocation cf), (mcfValue cf, mcfUnit cf, matchStrategy mflow))
                    | (cf, mflow) <- mappings
                    , Just comp <- [mcfCompartment cf]
                    , let Compartment normMedRaw normSub _ = normalizeCompartment cmap comp
                    , let normMed = normalizeMedium (T.toLower normMedRaw)
                    , not (T.null normSub)
                    ]
        , mtFallbackCF =
            stripStrategy $
                M.fromListWith
                    preferBetter
                    [ ((nameKey cf mflow, normMed, mcfLocation cf), (mcfValue cf, mcfUnit cf, matchStrategy mflow))
                    | (cf, mflow) <- mappings
                    , Just comp <- [mcfCompartment cf]
                    , let Compartment normMedRaw normSub _ = normalizeCompartment cmap comp
                    , let normMed = normalizeMedium (T.toLower normMedRaw)
                    , T.null normSub
                    ]
        , mtCompartmentMap = cmap
        }
  where
    stripStrategy = M.map (\(v, u, _) -> (v, u))

    preferBetter (v1, u1, s1) (v2, u2, s2)
        | stratPriority s1 < stratPriority s2 = (v1, u1, s1)
        | stratPriority s1 > stratPriority s2 = (v2, u2, s2)
        | v1 >= v2 = (v1, u1, s1)
        | otherwise = (v2, u2, s2)
    stratPriority ByUUID = 0 :: Int
    stratPriority ByCAS = 1
    stratPriority ByName = 2
    stratPriority BySynonym = 3
    stratPriority _ = 4

    matchStrategy mflow = case mflow of
        Just (_, s) -> s
        Nothing -> NoMatch

    -- Use matched flow's name only for name/synonym matches
    nameKey cf mflow = normalizeName $ case mflow of
        Just (flow, ByName) -> flowName flow
        Just (flow, BySynonym) -> flowName flow
        _ -> mcfFlowName cf

{- | Normalize medium names between method CFs and database flows.
Shared by 'buildMethodTables' (when keying CF lookups) and 'lookupCFForFlow'
(when looking flows up). Keep these two sites in lockstep — divergence
silently breaks every (medium, _) lookup.
-}
normalizeMedium :: Text -> Text
normalizeMedium m
    | m == "natural resource" = "resource"
    | otherwise = m

{- | Look up the characterization factor for a flow in precomputed
'MethodTables'. Cascade: UUID → (normalized name, medium, subcompartment) →
(name, medium) fallback. Returns @Nothing@ when the method has no CF for
this flow under any of those keys — caller decides how to treat that.

Takes 'Maybe Flow' so the UUID hit branch can succeed even when the flow
metadata isn't loaded (the score-only path doesn't need 'flowDB' for
UUID-mapped CFs); pass 'Nothing' if you can't resolve the flow.

This shim preserves the pre-location-aware signature and is equivalent
to @'lookupCFForFlowAt' tables fid mFlow Nothing@ — i.e. it consults
only the global (location-less) entries in the tables. Callers that can
supply an activity geography should switch to 'lookupCFForFlowAt'.

The three scoring/contribution helpers below all share this logic; keeping
it in one place is what guarantees they agree on what "matched" means.
-}
lookupCFForFlow :: MethodTables -> UUID -> Maybe Flow -> Maybe (Double, Text)
lookupCFForFlow tables fid mFlow = lookupCFForFlowAt tables fid mFlow Nothing

{- | Location-aware variant of 'lookupCFForFlow'. Prefers a CF whose
location matches the supplied activity geography, then falls back to
the global ('Nothing') entry. Passing @Nothing@ for the activity
location reproduces the legacy "global only" behaviour.
-}
lookupCFForFlowAt ::
    MethodTables ->
    UUID ->
    Maybe Flow ->
    Maybe Text ->
    Maybe (Double, Text)
lookupCFForFlowAt tables fid mFlow activityLoc =
    case M.lookup fid (mtUuidCF tables) of
        Just cfv -> Just cfv
        Nothing -> case mFlow of
            Nothing -> Nothing
            Just flow ->
                let name = normalizeName (flowName flow)
                    -- Build the flow's raw compartment from flowCategory ("medium/sub")
                    -- and flowSubcompartment, then normalize via the same map applied
                    -- when buildMethodTables keyed the CF tables. Both sides converge
                    -- to the canonical form, so a "Emissions to air" rule in
                    -- compartments.csv suffices to bridge BAFU's prefix vs EF's bare
                    -- medium without listing every (medium, sub) pair explicitly.
                    rawCategory = T.toLower (flowCategory flow)
                    (rawMed, rawSubFromCat) = case T.breakOn "/" rawCategory of
                        (m, rest)
                            | T.null rest -> (m, T.empty)
                            | otherwise -> (m, T.drop 1 rest)
                    rawSub =
                        let s = T.toLower (fromMaybe T.empty (flowSubcompartment flow))
                         in if T.null s then rawSubFromCat else s
                    Compartment normMedRaw normSub _ =
                        normalizeCompartment (mtCompartmentMap tables) (Compartment rawMed rawSub T.empty)
                    baseMed = normalizeMedium normMedRaw
                    subcomp = normSub
                    -- Location preference: matching loc first, then global ('Nothing').
                    -- 'Nothing' activity loc still reads only the global entry.
                    tryExact loc = M.lookup (name, baseMed, subcomp, loc) (mtExactCF tables)
                    tryFallback loc = M.lookup (name, baseMed, loc) (mtFallbackCF tables)
                    exact = firstJust (tryExact activityLoc) (tryExact Nothing)
                 in case exact of
                        Just _ -> exact
                        Nothing -> firstJust (tryFallback activityLoc) (tryFallback Nothing)
  where
    firstJust (Just x) _ = Just x
    firstJust Nothing y = y

{- | Score an inventory against precomputed 'MethodTables'.
Hot path: O(|inventory|) per call, no map construction.

Returns 'LCIAOutcome' carrying the score plus characterized-vs-total
inventory mass (so callers can detect tail erosion when many small flows
go uncharacterized). 'loUncharacterized' and 'loUnknownUuids' are empty
in this commit; the suggester populating them lands separately so this
change stays bit-equivalent for the score number itself.
-}
computeLCIAScoreFromTables :: UnitConfig -> UnitDB -> FlowDB -> Inventory -> MethodTables -> Maybe Text -> LCIAOutcome
computeLCIAScoreFromTables unitConfig unitDB flowDB inventory tables activityLoc =
    let (!score, !charSum, !invSum) = M.foldlWithKey' step (0, 0, 0) inventory
     in LCIAOutcome
            { loScore = score
            , loCharacterizedSum = charSum
            , loInventoryAbsSum = invSum
            , loUncharacterized = []
            , loUnknownUuids = []
            }
  where
    step (!s, !cs, !is) fid qty
        | qty == 0 = (s, cs, is)
        | otherwise =
            let !absQty = abs qty
                !is' = is + absQty
             in case lookupCFForFlowAt tables fid (M.lookup fid flowDB) activityLoc of
                    Nothing -> (s, cs, is')
                    Just (cfVal, cfUnit) ->
                        let flowUnit = maybe "" unitName (M.lookup fid flowDB >>= \f -> M.lookup (flowUnitId f) unitDB)
                            converted =
                                if flowUnit == cfUnit || T.null cfUnit
                                    then qty
                                    else fromMaybe qty (convertUnit unitConfig flowUnit cfUnit qty)
                         in (s + converted * cfVal, cs + absQty, is')

{- | Back-compat wrapper: build tables on the fly. Prefer the cached path
('mapMethodToTablesCached' + 'computeLCIAScoreFromTables') in hot loops.
-}
computeLCIAScore :: UnitConfig -> UnitDB -> FlowDB -> Inventory -> [(MethodCF, Maybe (Flow, MatchStrategy))] -> LCIAOutcome
computeLCIAScore unitConfig unitDB flowDB inventory mappings =
    computeLCIAScoreFromTables unitConfig unitDB flowDB inventory (buildMethodTables M.empty mappings) Nothing

{- | Per-flow contributions over an 'Inventory', keyed by flow UUID (possibly
cross-DB-merged). Walks the inventory directly (not the mappings) so any
flow with a matchable CF contributes — including flows from dep DBs that
don't appear in the root-DB method mapping.

Returns @(contributions, unknownUuids)@:
  * @contributions@ — @[(flow, cfValue, contributionInMethodUnit)]@
    for every non-zero inventory entry whose UUID resolves in 'flowDB'
    AND whose (name, medium, subcompartment) matches a CF.
  * @unknownUuids@ — non-zero inventory UUIDs with no record in 'flowDB'.
    Callers should surface these (per the "no silent errors" rule): a
    missing record means the merged metadata is incomplete for this
    inventory and some flows are invisible to characterization.

Flows that resolve in 'flowDB' but have no matching CF are legitimately
uncharacterized and silently omitted — that matches the behaviour of
'computeLCIAScoreFromTables' and is not a data-integrity concern.
-}
inventoryContributions ::
    UnitConfig ->
    UnitDB ->
    FlowDB ->
    Inventory ->
    MethodTables ->
    Maybe Text ->
    ([(Flow, Double, Double)], [UUID])
inventoryContributions unitConfig unitDB flowDB inventory tables activityLoc =
    M.foldlWithKey' step ([], []) inventory
  where
    -- Strict fold over the inventory Map: the old 'foldr' over 'M.toList'
    -- built a thunk chain the size of the inventory on every call, and
    -- characterization runs this 27-wide over K-activity batches — the
    -- dominant garbage source. Strict pair + accumulator prevents the leak.
    -- Result list order is reversed vs. the old version, but every caller
    -- already 'sortOn's by |contribution|.
    step (!contribs, !unknowns) fid qty
        | qty == 0 = (contribs, unknowns)
        | otherwise = case M.lookup fid flowDB of
            Nothing -> (contribs, fid : unknowns) -- metadata missing — surface it
            Just flow -> case lookupCFForFlowAt tables fid (Just flow) activityLoc of
                Nothing -> (contribs, unknowns) -- no CF match — legitimately uncharacterized
                Just (cfVal, cfUnit) ->
                    let flowUnit = maybe "" unitName (M.lookup (flowUnitId flow) unitDB)
                        converted =
                            if flowUnit == cfUnit || T.null cfUnit
                                then qty
                                else fromMaybe qty (convertUnit unitConfig flowUnit cfUnit qty)
                        !contribution = converted * cfVal
                     in ((flow, cfVal, contribution) : contribs, unknowns)

{- | Per-process LCIA contributions for one DB + one method, driven by
'MethodTables' + a merged 'FlowDB'. Mirrors
'Matrix.computeProcessLCIAContributions' but lets dep-DB flows land a CF
via (name, medium, subcompartment) fallback — same lookup path as
'inventoryContributions' — so this helper can be called per-DB while
walking a cross-DB dependency graph and still characterize every flow.

Iterates @dbBiosphereTriples db@; for each @(flowRow, colIdx, bioVal)@ it
attributes @bioVal * scaling[col] * CF@ (in the method's unit, after
flow-unit → CF-unit conversion) to the process owning @colIdx@.
-}
processContributionsFromTables ::
    UnitConfig ->
    UnitDB ->
    FlowDB ->
    Database ->
    Matrix.Vector ->
    MethodTables ->
    M.Map ProcessId Double
processContributionsFromTables unitConfig unitDB flowDB db scalingVec tables =
    U.foldl' step M.empty (dbBiosphereTriples db)
  where
    actIdx = dbActivityIndex db
    bioFlows = dbBiosphereFlows db
    nFlows = V.length bioFlows
    nActs = V.length actIdx

    -- Precompute the effective CF (CF value × flow→CF unit conversion factor)
    -- per biosphere-matrix row so the triple loop becomes pure arithmetic.
    -- O(|bioFlows|) once, vs O(|triples| × map-lookups) before.
    effectiveCF :: U.Vector Double
    effectiveCF = U.generate nFlows $ \i ->
        let flowUUID = bioFlows V.! i
         in case M.lookup flowUUID flowDB of
                Nothing -> 0
                Just flow -> case lookupCFForFlow tables flowUUID (Just flow) of
                    Nothing -> 0
                    Just (cfVal, cfUnit) ->
                        let flowUnit = maybe "" unitName (M.lookup (flowUnitId flow) unitDB)
                            factor =
                                if flowUnit == cfUnit || T.null cfUnit
                                    then 1.0
                                    else fromMaybe 1.0 (convertUnit unitConfig flowUnit cfUnit 1.0)
                         in factor * cfVal

    -- Invert dbActivityIndex (pid -> col) into (col -> pid) as an unboxed
    -- vector for O(1) per-triple lookup. Assumes matrix cols are dense in
    -- [0..nActs-1], which matches the existing index construction.
    colToProc :: U.Vector Int
    colToProc = U.create $ do
        mv <- MU.replicate nActs (-1 :: Int)
        V.imapM_ (\pid col -> MU.write mv (fromIntegral col) pid) actIdx
        pure mv

    step acc (SparseTriple flowRow colIdx bioVal) =
        let cf = effectiveCF U.! fromIntegral flowRow
         in if cf == 0
                then acc
                else
                    let colI = fromIntegral colIdx :: Int
                        pid = colToProc U.! colI
                     in if pid < 0
                            then acc
                            else
                                let scale = scalingVec U.! colI
                                    pid' = fromIntegral pid :: ProcessId
                                 in M.insertWith (+) pid' (bioVal * scale * cf) acc

-- ──────────────────────────────────────────────
-- Post-scoring suggester
-- ──────────────────────────────────────────────

{- | Find the top-N method CFs that most resemble a database flow.

Three signals are stacked, the highest-scoring reason wins:

1. Plain Jaccard on normalized-name tokens (cheap baseline; catches
   word-order and punctuation variants).
2. Jaccard after expanding tokens via the PubChem snapshot
   ('expandedTokens'). This is what bridges \"CO2\" and
   \"Carbon dioxide\" — pure tokenization can never see they relate.
3. CAS bridge: when the flow's CAS matches a CF's CAS, the candidate is
   surfaced at score 0.95 regardless of name overlap. Highest-confidence
   reason; catches cases where one side has CAS and the other doesn't,
   so the existing CAS-cascade match already failed.

Candidate space is pre-filtered to the same compartment medium when the
flow has one (via 'miByMedium'), plus any CAS-bridge hits. This keeps the
scan cheap even on methods with thousands of CFs.

Empty result list = no signal above zero. Caller should treat that as
\"this flow is genuinely uncharacterized by the method\", not a bug.
-}
findSimilarCFs :: ChemSynonyms -> MethodIndex -> Flow -> Int -> [SimilarCF]
findSimilarCFs syns idx flow maxN
    | maxN <= 0 = []
    | otherwise =
        let flowName' = flowName flow
            flowCAS' = flowCAS flow
            flowMedium = normalizeMedium . T.takeWhile (/= '/') . T.toLower $ flowCategory flow

            flowRawTokens = S.fromList (T.words (normalizeName flowName'))
            flowExpTokens = expandedTokens syns flowName'

            -- Same-medium candidates (cheap scan); fall back to whole index
            -- only when we have no medium info to filter by.
            mediumIdxs = case M.lookup flowMedium (miByMedium idx) of
                Just is -> is
                Nothing -> [0 .. V.length (miCFs idx) - 1]

            casBridgeIdxs = case flowCAS' of
                Nothing -> []
                Just cas -> M.findWithDefault [] cas (miByCAS idx)

            -- Score one CF index. Two Jaccards: raw (plain tokens) vs
            -- expanded (synonyms folded in). The reason follows whichever
            -- signal won, so the audit JSON tells the reviewer what to
            -- verify.
            scoreCandidate i =
                let cfRawTokens = miCFTokens idx V.! i
                    cf = miCFs idx V.! i
                    cfExpTokens = expandedTokens syns (mcfFlowName cf)
                    rawJ = jaccard flowRawTokens cfRawTokens
                    expJ = jaccard flowExpTokens cfExpTokens
                 in if expJ > rawJ
                        then (i, expJ, SimBySynonymExpansion)
                        else (i, rawJ, SimByJaccard)

            mediumScored = map scoreCandidate mediumIdxs
            casScored = [(i, 0.95, SimByCASBridge) | i <- casBridgeIdxs]

            -- Merge: same CF can be in both lists (medium hit AND CAS hit);
            -- keep the higher-scoring entry so we don't show duplicates.
            mergedMap =
                M.fromListWith
                    pickHigher
                    [(i, (s, r)) | (i, s, r) <- mediumScored ++ casScored]

            ranked =
                take maxN $
                    sortOn (\(_, (s, _)) -> Down s) $
                        filter (\(_, (s, _)) -> s > 0) $
                            M.toList mergedMap
         in [ SimilarCF
                { scfMethodFlowName = mcfFlowName cf
                , scfCAS = mcfCAS cf
                , scfCompartment = mcfCompartment cf
                , scfScore = s
                , scfReason = r
                , scfCfValue = mcfValue cf
                , scfCfUnit = mcfUnit cf
                }
            | (i, (s, r)) <- ranked
            , let cf = miCFs idx V.! i
            ]
  where
    pickHigher (s1, r1) (s2, r2) = if s1 >= s2 then (s1, r1) else (s2, r2)

    jaccard :: S.Set Text -> S.Set Text -> Double
    jaccard a b
        | S.null a || S.null b = 0
        | otherwise =
            let inter = S.size (S.intersection a b)
                uni = S.size (S.union a b)
             in fromIntegral inter / fromIntegral uni

{- | Collect uncharacterized flows from an inventory, ranked by their share of
total inventory mass. For each, surface the top-N similar CFs from the
method (so the caller can tell genuine method gaps from mapping bugs).

This walks the inventory once for the totals and once for the unmatched
collection — two cheap O(|inventory|) passes. The expensive part (the
suggester) only runs on the surviving flows after weight filtering and
'uoMaxFlows' truncation.

Returns @[]@ when no flow exceeds 'uoMinAbsWeight' or when the method's
diagnostics are disabled (@uoMaxFlows == 0@ / @uoMaxSimilar == 0@).
-}
findUncharacterized ::
    UnitConfig ->
    UnitDB ->
    FlowDB ->
    Inventory ->
    MethodTables ->
    ChemSynonyms ->
    MethodIndex ->
    UncharacterizedOpts ->
    Maybe Text ->
    [UncharacterizedFlow]
findUncharacterized _ unitDB flowDB inventory tables syns idx opts activityLoc
    | uoMaxFlows opts <= 0 = []
    | totalAbs == 0 = []
    | otherwise =
        let unmatched =
                [ (flow, qty, w)
                | (fid, qty) <- M.toList inventory
                , qty /= 0
                , Just flow <- [M.lookup fid flowDB]
                , isNothing (lookupCFForFlowAt tables fid (Just flow) activityLoc)
                , let w = abs qty / totalAbs
                , w >= uoMinAbsWeight opts
                ]
            ranked =
                take (uoMaxFlows opts) $
                    sortOn (\(_, _, w) -> Down w) unmatched
         in [ UncharacterizedFlow
                { ucfFlowId = flowId flow
                , ucfFlowName = flowName flow
                , ucfCategory = flowCategory flow
                , ucfSubcomp = flowSubcompartment flow
                , ucfFlowUnit = flowUnitText flow
                , ucfQuantity = qty
                , ucfAbsWeight = w
                , ucfSimilarCFs = findSimilarCFs syns idx flow (uoMaxSimilar opts)
                }
            | (flow, qty, w) <- ranked
            ]
  where
    !totalAbs = M.foldr (\q s -> s + abs q) 0 inventory
    flowUnitText flow = maybe "" unitName (M.lookup (flowUnitId flow) unitDB)

{- | Score an inventory and attach diagnostics in one call.

Convenience wrapper that runs 'computeLCIAScoreFromTables' and then
'findUncharacterized' on the same inputs, splicing the results into one
'LCIAOutcome'. Use this on the diagnostics path; stick to
'computeLCIAScoreFromTables' on the hot scoring path where the extra
suggester work is wasted.
-}
computeLCIAScoreWithDiagnostics ::
    UnitConfig ->
    UnitDB ->
    FlowDB ->
    Inventory ->
    MethodTables ->
    ChemSynonyms ->
    MethodIndex ->
    UncharacterizedOpts ->
    Maybe Text ->
    LCIAOutcome
computeLCIAScoreWithDiagnostics unitConfig unitDB flowDB inventory tables syns idx opts activityLoc =
    let outcome = computeLCIAScoreFromTables unitConfig unitDB flowDB inventory tables activityLoc
        diagnostics = findUncharacterized unitConfig unitDB flowDB inventory tables syns idx opts activityLoc
     in outcome{loUncharacterized = diagnostics}
