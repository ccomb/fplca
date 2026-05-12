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
    fillBroadcastVector,
    fillRegionalActivityWeights,
    RegionalActivityWeights (..),
    computeLCIAScore,
    computeLCIAScoreFromTables,
    computeLCIAScoreAuto,
    computeRegionalizedLCIAScore,
    computeLCIAScoreWithDiagnostics,
    findUncharacterized,
    findSimilarCFs,
    inventoryContributions,
    processContributionsFromTables,
    convertForCharacterization,
    expandSynonymMappings,

    -- * Multi-method scoring
    MethodSetTables (..),
    MethodSetEntry (..),
    buildMethodSetTables,
    computeLCIAScoreSetFromTables,

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
import Control.Monad.ST (runST)
import Data.Aeson (ToJSON)
import Data.List (find, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isNothing)
import Data.Ord (Down (..))
import Data.STRef (modifySTRef', newSTRef, readSTRef)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word8)
import GHC.Generics (Generic)

import qualified Data.Set as S
import qualified Data.Set as Set
import Matrix (Inventory, Vector)
import qualified Matrix
import Method.ChemSynonyms (ChemSynonyms, expandedTokens)
import Method.Types
import Plugin.Types (MapContext (..), MapQuery (..), MapResult (..), MapperHandle (..))
import SynonymDB
import Types (Activity (..), Database (..), Flow (..), FlowDB, ProcessId, SparseTriple (..), Unit (..), UnitDB)
import UnitConversion (UnitConfig, convertUnit, isKnownUnit)

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
    , mtExactCF :: !(M.Map (Text, Text, Text) (Double, Text))
    -- ^ (normalized name, medium, subcompartment) → (CF, unit)
    , mtFallbackCF :: !(M.Map (Text, Text) (Double, Text))
    -- ^ (normalized name, medium) → (CF, unit) for entries with unspecified subcompartment
    , mtRegionalizedCF :: !(M.Map (UUID, Text) (Double, Text))
    {- ^ Regionalized cells of the C matrix: (DB flow UUID, consumer location) → (CF, unit).
    Empty for non-regionalized methods. When non-empty, callers should dispatch
    to the regionalized scoring path (see 'Matrix.computeRegionalizedLCIAScore').
    -}
    , mtCompartmentMap :: !CompartmentMap
    {- ^ Compartment-normalization rules (e.g. @"Emissions to air" → "air"@).
    Applied to both CF compartments at build time and database flow
    compartments at query time, so both sides converge to the same
    canonical form. Empty map = identity, no normalization.
    -}
    , mtBroadcast :: !(M.Map UUID Double)
    {- ^ Pre-multiplied broadcast CFs: flow UUID → effective CF (CF value × flow→CF unit conversion).
    Collapses the UUID/exact/fallback cascade into a single Map and absorbs unit conversion
    so the scoring hot path is a pure multiply-accumulate. Empty when 'buildMethodTables' is
    called directly without DB-level dependencies; fill with 'fillBroadcastVector' to enable
    the fast path (scoring falls back to the cascade when this Map is empty).
    -}
    , mtRegionalActivityWeights :: !(Maybe RegionalActivityWeights)
    {- ^ Optional per-activity-column precomputed weights for regionalized methods.
    When present, regionalized LCIA score collapses to a single dot product
    @w · s@ instead of walking the whole biosphere triples for every pid.
    Built by 'fillRegionalActivityWeights' against a specific 'Database'; Nothing
    when 'mtRegionalizedCF' is empty or precomputation hasn't been run.
    -}
    }

{- | Per-matrix-column precomputed contributions for a regionalized method.

For every activity column @a@ in the technosphere matrix, @rawWeights[a]@ is
the inventory-weighted sum of regionalized CFs that activity emits in its own
location:

  @rawWeights[a] = Σ_f B[f,a] · CF(f, loc(a))@

With this in hand, a regionalized LCIA score for any scaling vector @s_k@
reduces to one dot product:

  @score_k = Σ_a rawWeights[a] · s_k[a]@

instead of re-walking the full biosphere triples once per pid.

@rawTainted[a] == 1@ means at least one biosphere triple at column @a@ has a
regionalized flow with no CF for @loc(a)@; the partial weight is still stored
(matching the existing surface where missing-CF activities under-count), but
callers can surface a 'Left' when any tainted column carries non-zero scaling.

@rawMissingPairs@ accumulates the deduplicated (flow, location) gaps so the
caller can emit one warning per gap rather than per pid × per method.
-}
data RegionalActivityWeights = RegionalActivityWeights
    { rawWeights :: !(U.Vector Double)
    , rawTainted :: !(U.Vector Word8)
    , rawMissingPairs :: ![(UUID, Text)]
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
        Just (Compartment med _ _) -> normalizeMediumIdx (T.toLower med)

    normalizeMediumIdx m
        | m == "natural resource" = "resource"
        | otherwise = m

{- | Build 'MethodTables' from raw mappings and a 'CompartmentMap'.

The map is applied to each CF's compartment before keying the lookup
tables, so all keys land in canonical form. The same map is then stored
in the result so 'computeLCIAScoreFromTables' can normalize inventory-side
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
            peers = M.findWithDefault S.empty cfName directPeers
         in [ (cf, Just (flow, BySynonym))
            | syn <- S.toList peers
            , flow <- M.findWithDefault [] syn flowsByName
            ]

-- No compartment filter here on purpose: 'buildMethodTables' keys
-- entries by the CF's compartment (after 'normalizeCompartment'), and
-- 'lookupCFForFlowAt' looks up by the inventory flow's compartment, so
-- mismatched (cf, peer) pairs simply land at keys nothing ever queries.
-- A pre-filter would have to mirror 'normalizeCompartment' to be
-- correct (e.g. ILCD's @land occupation@ medium → BAFU's
-- @resources/land@ via the compartment map), so it's simpler to let
-- the table keys do the filtering.

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
                    [ ((nameKey cf mflow, normMed, normSub), (mcfValue cf, mcfUnit cf, matchStrategy mflow))
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
                    [ ((nameKey cf mflow, normMed), (mcfValue cf, mcfUnit cf, matchStrategy mflow))
                    | (cf, mflow) <- mappings
                    , Just comp <- [mcfCompartment cf]
                    , let Compartment normMedRaw normSub _ = normalizeCompartment cmap comp
                    , let normMed = normalizeMedium (T.toLower normMedRaw)
                    , T.null normSub
                    ]
        , mtRegionalizedCF =
            M.fromList
                [ ((flowId flow, loc), (mcfValue cf, mcfUnit cf))
                | (cf, Just (flow, _)) <- mappings
                , Just loc <- [mcfConsumerLocation cf]
                ]
        , mtCompartmentMap = cmap
        , mtBroadcast = M.empty -- fill via 'fillBroadcastVector' to enable the fast path
        , mtRegionalActivityWeights = Nothing -- fill via 'fillRegionalActivityWeights' for regional fast path
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

{- | Convert @qty@ from @flowUnit@ to @cfUnit@ for characterization.

Bypass cases (returns @qty@ unchanged):
  * Units match by name, or either side has no unit metadata.
  * Either unit is unknown to the 'UnitConfig' (e.g. LCIA result expressions
    like "kg CO2 eq"). The CF author already chose values consistent with
    their declared unit; we trust them rather than penalize.

Hard fail (returns @0@): both units are known to the 'UnitConfig' but
dimensionally incompatible (e.g. flow in @m@, CF in @kg@). Silently using
@qty@ here would inject wrong-dimension data into the score; we refuse.
-}
convertForCharacterization :: UnitConfig -> Text -> Text -> Double -> Double
convertForCharacterization cfg flowUnit cfUnit qty
    | flowUnit == cfUnit || T.null cfUnit || T.null flowUnit = qty
    | not (isKnownUnit cfg flowUnit) || not (isKnownUnit cfg cfUnit) = qty
    | otherwise = fromMaybe 0 (convertUnit cfg flowUnit cfUnit qty)

{- | Pre-compute the broadcast CF Map: each flow UUID covered by the method maps
to its effective CF (CF value × flow-unit→CF-unit conversion). Collapses the
UUID/exact/fallback cascade into a single Map and absorbs the unit conversion
so the scoring hot path becomes pure multiply-accumulate.

Walks every flow in @flowDB@ once. Flows with no CF match are not inserted
(sparse Map). Conversions route through 'convertForCharacterization', so
dimensionally-incompatible flow/CF unit pairs land an effective CF of @0@
(matching the per-flow scoring path) rather than silently keeping the
unconverted quantity and contaminating the score.
-}
fillBroadcastVector :: UnitConfig -> UnitDB -> FlowDB -> MethodTables -> MethodTables
fillBroadcastVector unitConfig unitDB flowDB tables =
    tables{mtBroadcast = M.mapMaybeWithKey buildEntry flowDB}
  where
    buildEntry fid flow = case lookupCascadeCF tables flowDB fid of
        Nothing -> Nothing
        Just cfTuple -> Just (convertAndMultiply unitConfig unitDB (Just flow) cfTuple 1.0)

{- | Precompute per-activity-column contributions for a regionalized method.

This is the regionalized analogue of 'fillBroadcastVector': it walks the
'Database' biosphere triples ONCE and stores, per matrix column @a@,

  @w[a] = Σ_f B[f,a] · CF(f, loc(a)) · unit-conversion(f)@

So any later regionalized score reduces to @w · s_k@ — one dot product per
pid instead of one biosphere-triple walk per pid. For agribalyse this
trades 632 walks of ~50K triples for 632 dot products over 21K activities.

CF lookups follow the same hierarchical fallback as 'resolveRegionalCF'
(exact → parents → universal broadcast). When a regionalized flow has no CF
for that activity's location even after walking parents, the activity is
marked tainted ('rawTainted[a] = 1') and the missing @(flow, location)@
pair is accumulated in 'rawMissingPairs' for deduplicated warning emission
by the caller — instead of one warning per pid × per method × per missing CF
under the old per-call path.

Score-time semantics: callers can choose between returning a partial score
that under-counts tainted activities (matching the broadcast path's
silent-omission behaviour for non-regio flows) or a 'Left' when any tainted
activity has non-zero scaling (matching the old strict per-call surface).
'computeRegionalizedLCIAScore' picks the strict surface, mirroring the
existing contract documented on 'computeLCIAScoreAuto'.

No-op when 'mtRegionalizedCF tables' is empty — non-regionalized methods
keep 'mtRegionalActivityWeights = Nothing' so the broadcast fast path stays
the right answer.
-}
fillRegionalActivityWeights ::
    UnitConfig ->
    UnitDB ->
    FlowDB ->
    Database ->
    M.Map Text [Text] ->
    MethodTables ->
    MethodTables
fillRegionalActivityWeights unitCfg unitDB flowDB db hier tables
    | M.null (mtRegionalizedCF tables) = tables
    | otherwise = tables{mtRegionalActivityWeights = Just precomputed}
  where
    nCols = fromIntegral (dbActivityCount db) :: Int
    actIdx = dbActivityIndex db
    activities = dbActivities db
    bioFlows = dbBiosphereFlows db
    bioTriples = dbBiosphereTriples db
    regional = mtRegionalizedCF tables
    regionalizedFlows = Set.fromList [f | (f, _) <- M.keys regional]

    -- ProcessId → matrix column index → activity's reference location.
    -- Built once (O(nActivities)) and indexed by column inside the hot loop.
    colLoc :: V.Vector Text
    colLoc =
        V.replicate nCols T.empty
            V.// [ (fromIntegral (actIdx V.! pid), activityLocation (activities V.! pid))
                 | pid <- [0 .. V.length actIdx - 1]
                 , let !col = fromIntegral (actIdx V.! pid) :: Int
                 , col >= 0
                 , col < nCols
                 ]

    -- Walk biosphere triples once; build weights, tainted flags and the
    -- deduplicated missing-(flow, location) set in a single ST action.
    precomputed :: RegionalActivityWeights
    precomputed = runST $ do
        ws <- MU.replicate nCols (0 :: Double)
        ts <- MU.replicate nCols (0 :: Word8)
        missRef <- newSTRef (Set.empty :: Set.Set (UUID, Text))
        U.forM_ bioTriples $ \(SparseTriple flowRow colIdx bioVal) -> do
            let !col = fromIntegral colIdx :: Int
                !flowUUID = bioFlows V.! fromIntegral flowRow
                !loc = colLoc V.! col
            case resolveRegionalCF tables flowDB regionalizedFlows hier flowUUID loc of
                Right Nothing -> pure ()
                Right (Just cfTuple) -> do
                    let !contribution =
                            convertAndMultiply
                                unitCfg
                                unitDB
                                (M.lookup flowUUID flowDB)
                                cfTuple
                                bioVal
                    MU.unsafeModify ws (+ contribution) col
                Left _ -> do
                    MU.unsafeWrite ts col 1
                    modifySTRef' missRef (Set.insert (flowUUID, loc))
        wsF <- U.unsafeFreeze ws
        tsF <- U.unsafeFreeze ts
        miss <- readSTRef missRef
        pure
            RegionalActivityWeights
                { rawWeights = wsF
                , rawTainted = tsF
                , rawMissingPairs = Set.toAscList miss
                }

{- | Score an inventory against precomputed 'MethodTables'.
Hot path: O(|inventory|) per call, no map construction.

Returns 'LCIAOutcome' carrying the score plus characterized-vs-total
inventory mass (so callers can detect tail erosion when many small flows
go uncharacterized).

Uses 'mtBroadcast' (single Map lookup, conversion pre-multiplied) when filled,
falling back to the legacy UUID→exact→fallback cascade with on-the-fly unit
conversion when 'mtBroadcast' is empty. Tests and back-compat callers that use
'buildMethodTables' directly hit the legacy path; the cached
'mapMethodToTablesCached' fills the broadcast and gets the fast path.
-}
computeLCIAScoreFromTables :: UnitConfig -> UnitDB -> FlowDB -> Inventory -> MethodTables -> LCIAOutcome
computeLCIAScoreFromTables unitConfig unitDB flowDB inventory tables =
    let (!score, !charSum, !invSum) = M.foldlWithKey' step (0, 0, 0) inventory
     in LCIAOutcome
            { loScore = score
            , loCharacterizedSum = charSum
            , loInventoryAbsSum = invSum
            , loUncharacterized = []
            , loUnknownUuids = []
            }
  where
    useFast = not (M.null (mtBroadcast tables))

    step (!s, !cs, !is) fid qty
        | qty == 0 = (s, cs, is)
        | otherwise =
            let !absQty = abs qty
                !is' = is + absQty
             in case scoreFlow fid qty of
                    Nothing -> (s, cs, is')
                    Just contribution -> (s + contribution, cs + absQty, is')

    scoreFlow fid qty
        | useFast = case M.lookup fid (mtBroadcast tables) of
            Just cf -> Just (qty * cf)
            -- Inventory may reference flows not in the broadcast (e.g. cross-DB
            -- merged flows beyond the original flowDB at build time): fall back
            -- to the cascade so we don't silently drop them.
            Nothing -> legacyScoreFlow fid qty
        | otherwise = legacyScoreFlow fid qty

    legacyScoreFlow fid qty = case lookupCascadeCF tables flowDB fid of
        Nothing -> Nothing
        Just cfTuple -> Just (convertAndMultiply unitConfig unitDB (M.lookup fid flowDB) cfTuple qty)

{- | Back-compat wrapper: build tables on the fly. Prefer the cached path
('mapMethodToTablesCached' + 'computeLCIAScoreFromTables') in hot loops.
-}
computeLCIAScore :: UnitConfig -> UnitDB -> FlowDB -> Inventory -> [(MethodCF, Maybe (Flow, MatchStrategy))] -> LCIAOutcome
computeLCIAScore unitConfig unitDB flowDB inventory mappings =
    computeLCIAScoreFromTables unitConfig unitDB flowDB inventory (buildMethodTables M.empty mappings)

{- | LCIA score with automatic dispatch.

If the method has no regionalized CFs ('mtRegionalizedCF' empty), uses the
classic vector path ('computeLCIAScoreFromTables'). Otherwise switches to
the matrix path ('computeRegionalizedLCIAScore').

The caller is expected to provide both an 'Inventory' (cheap if already
computed for other purposes) and a scaling 'Vector' (cheap if the MUMPS
factorization is cached). Pass them both and let this function pick.

Returns 'Either' so regionalized methods can surface coverage gaps as
explicit errors instead of silently under-counting.
-}
computeLCIAScoreAuto ::
    UnitConfig ->
    UnitDB ->
    FlowDB ->
    Database ->
    -- | Scaling vector @s@ (only consulted if the method is regionalized)
    Vector ->
    -- | Pre-computed inventory @g = B · s@ (only used in the classic path)
    Inventory ->
    -- | Location hierarchy: child → ordered list of parents
    M.Map Text [Text] ->
    MethodTables ->
    Either Text Double
computeLCIAScoreAuto unitCfg unitDB flowDB db scalingVec inventory hier tables
    | M.null (mtRegionalizedCF tables) =
        Right (loScore (computeLCIAScoreFromTables unitCfg unitDB flowDB inventory tables))
    | otherwise =
        computeRegionalizedLCIAScore unitCfg unitDB flowDB db scalingVec hier tables

{- | Streaming regionalized LCIA score over the biosphere matrix.

@score = Σ_{(f, a)} B[f, a] · s[a] · C[f, loc(a)]@

Where @C[f, l]@ is resolved by hierarchical fallback:

  1. Exact regional cell @(f, l)@ in 'mtRegionalizedCF'.
  2. Cell at any parent region of @l@ (walked via the location hierarchy).
  3. Universal broadcast: the same lookup that 'computeLCIAScoreFromTables' uses
     ('mtUuidCF' / 'mtExactCF' / 'mtFallbackCF').
  4. If none of the above and @f@ is regionalized in this method (i.e. the
     regional table mentions @f@ for some other location), fail with a 'Left'
     surfacing the gap — silent zero would under-count.
  5. If @f@ is not covered at all by the method, contribute 0.

The hierarchy is the same one used by 'Database.CrossLinking.isSubregionOf':
@Map ChildLocation [ParentLocation]@ from broader to broadest.

This path is selected by 'Service' when 'mtRegionalizedCF' is non-empty;
non-regionalized methods continue to use 'computeLCIAScoreFromTables'.
-}
computeRegionalizedLCIAScore ::
    UnitConfig ->
    UnitDB ->
    FlowDB ->
    Database ->
    -- | Scaling vector @s@ from 'Matrix.computeScalingVector'
    Vector ->
    -- | Location hierarchy: child → ordered list of parents
    M.Map Text [Text] ->
    MethodTables ->
    Either Text Double
computeRegionalizedLCIAScore unitConfig unitDB flowDB db scalingVec hier tables =
    case mtRegionalActivityWeights tables of
        Just raw -> scoreFromPrecomputed raw scalingVec
        Nothing -> scoreFromBiosphereTriples
  where
    -- Fast path: one dot product over precomputed per-column weights.
    -- If any tainted activity carries non-zero scaling, surface the gap
    -- as a 'Left' to match the strict-Either contract callers depend on;
    -- the human-readable per-(flow, location) detail was already emitted
    -- as a single warning when the table was built.
    scoreFromPrecomputed raw s =
        let !weights = rawWeights raw
            !tainted = rawTainted raw
            !n = U.length weights
            !sLen = U.length s
         in if sLen /= n
                then
                    Left $
                        "Regionalized score: scaling/weights length mismatch ("
                            <> T.pack (show sLen)
                            <> " vs "
                            <> T.pack (show n)
                            <> "). Activity index and precomputed weights are built from the same database — this means the cache is stale or the wrong tables were paired."
                else
                    let go !i !acc !taintHits
                            | i >= n = (acc, taintHits)
                            | otherwise =
                                let !sv = U.unsafeIndex s i
                                 in if sv == 0
                                        then go (i + 1) acc taintHits
                                        else
                                            let !taintHits' =
                                                    if U.unsafeIndex tainted i /= 0
                                                        then taintHits + 1
                                                        else taintHits
                                                !acc' = acc + sv * U.unsafeIndex weights i
                                             in go (i + 1) acc' taintHits'
                        (!score, !touchedTaintedCount) = go 0 0 (0 :: Int)
                     in if touchedTaintedCount > 0
                            then
                                Left $
                                    "Regionalized CF lookup failed on "
                                        <> T.pack (show touchedTaintedCount)
                                        <> " tainted activity column(s) reached by this inventory"
                                        <> " — see warnings emitted at table-build time for the missing (flow, location) pairs."
                            else Right score

    -- Slow path (unchanged) — kept as a fallback for cases where the
    -- precomputed weights are absent (direct callers of
    -- 'buildMethodTables' that skip the fill step).
    scoreFromBiosphereTriples =
        let actIdx = dbActivityIndex db
            bioTriples = dbBiosphereTriples db
            bioFlows = dbBiosphereFlows db
            activities = dbActivities db
            regional = mtRegionalizedCF tables
            regionalizedFlows = Set.fromList [f | (f, _) <- M.keys regional]
            colToActivity :: M.Map Int Activity
            colToActivity =
                M.fromList
                    [ (fromIntegral (actIdx V.! pid), activities V.! pid)
                    | pid <- [0 .. V.length actIdx - 1]
                    ]
            step :: Either Text Double -> SparseTriple -> Either Text Double
            step acc (SparseTriple flowRow colIdx bioVal) = do
                running <- acc
                let s = scalingVec U.! fromIntegral colIdx
                    contribution = bioVal * s
                if contribution == 0
                    then Right running
                    else case M.lookup (fromIntegral colIdx :: Int) colToActivity of
                        Nothing -> Right running
                        Just act ->
                            let flowUUID = bioFlows V.! fromIntegral flowRow
                                loc = activityLocation act
                             in case resolveRegionalCF tables flowDB regionalizedFlows hier flowUUID loc of
                                    Right Nothing -> Right running
                                    Right (Just cfTuple) ->
                                        Right (running + convertAndMultiply unitConfig unitDB (M.lookup flowUUID flowDB) cfTuple contribution)
                                    Left err -> Left err
         in U.foldl' step (Right 0) bioTriples

{- | Resolve a CF for a (flow, location) pair through the hierarchy + broadcast
fallback. See 'computeRegionalizedLCIAScore' for the rules.

* @Right Nothing@: the flow is not covered by this method (silent OK).
* @Right (Just v)@: a CF was found.
* @Left err@: the flow IS regionalized in this method but no CF could be
  resolved for the given location even after walking parents — surfacing the
  gap prevents silent under-counting.
-}
resolveRegionalCF ::
    MethodTables ->
    FlowDB ->
    Set.Set UUID ->
    M.Map Text [Text] ->
    UUID ->
    Text ->
    Either Text (Maybe (Double, Text))
resolveRegionalCF tables flowDB regionalizedFlows hier flowUUID loc =
    case M.lookup (flowUUID, loc) (mtRegionalizedCF tables) of
        Just v -> Right (Just v)
        Nothing ->
            let parents = M.findWithDefault [] loc hier
                fromParents = firstJust [M.lookup (flowUUID, p) (mtRegionalizedCF tables) | p <- parents]
             in case fromParents of
                    Just v -> Right (Just v)
                    Nothing -> case lookupCascadeCF tables flowDB flowUUID of
                        Just v -> Right (Just v)
                        Nothing
                            | Set.member flowUUID regionalizedFlows ->
                                Left $
                                    "Regionalized CF lookup failed: flow "
                                        <> T.pack (show flowUUID)
                                        <> " has regional CFs in this method but none for location '"
                                        <> loc
                                        <> "' (after walking "
                                        <> T.pack (show (length parents))
                                        <> " parent regions) and no universal broadcast."
                            | otherwise -> Right Nothing

{- | Cascade CF lookup: UUID → exact (name, medium, subcomp) → fallback (name, medium).
The same logic is baked into 'mtBroadcast' once unit conversion is available;
this helper is the source of truth for the legacy / cross-DB / regionalized
fallback paths that don't go through the broadcast.

Both the CF table keys (built by 'buildMethodTables') and the inventory flow
compartments are normalized through @tables.'mtCompartmentMap'@ so each side
converges on the same canonical form — a compartments.csv rule like
@"Emissions to air,,,air,,"@ then bridges BAFU-style prefixed compartments
against ILCD-style bare media without requiring an explicit (medium, sub)
pair for every combination.
-}
lookupCascadeCF :: MethodTables -> FlowDB -> UUID -> Maybe (Double, Text)
lookupCascadeCF tables flowDB fid = case M.lookup fid (mtUuidCF tables) of
    Just cfv -> Just cfv
    Nothing -> case M.lookup fid flowDB of
        Nothing -> Nothing
        Just flow ->
            let name = normalizeName (flowName flow)
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
                exact = M.lookup (name, baseMed, subcomp) (mtExactCF tables)
             in case exact of
                    Just _ -> exact
                    Nothing -> M.lookup (name, baseMed) (mtFallbackCF tables)

-- | Normalize medium names between method CFs and database flows.
normalizeMedium :: Text -> Text
normalizeMedium m
    | m == "natural resource" = "resource"
    | otherwise = m

{- | Apply the flow→CF unit conversion factor and multiply by the CF value.

Delegates to 'convertForCharacterization' for the conversion step, so a
dimensional mismatch between flow and CF units lands an effective @0@
(refuse to score wrong-dimension data) rather than silently passing the
unconverted quantity through. Pass @qty = 1.0@ to obtain the effective-CF
factor used at build time; pass an actual quantity for inline scoring.
-}
convertAndMultiply ::
    UnitConfig ->
    UnitDB ->
    {- | Pre-resolved flow if the caller already has it; @Nothing@ defaults to
    the identity factor (no flow record means no flow unit known).
    -}
    Maybe Flow ->
    -- | (CF value, CF unit)
    (Double, Text) ->
    Double ->
    Double
convertAndMultiply unitConfig unitDB mflow (cfVal, cfUnit) qty =
    let flowUnit = maybe "" unitName (mflow >>= \f -> M.lookup (flowUnitId f) unitDB)
        converted = convertForCharacterization unitConfig flowUnit cfUnit qty
     in converted * cfVal

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Just x : _) = Just x
firstJust (Nothing : xs) = firstJust xs

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
    ([(Flow, Double, Double)], [UUID])
inventoryContributions unitConfig unitDB flowDB inventory tables =
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
            Just flow -> case lookupCascadeCF tables flowDB fid of
                Nothing -> (contribs, unknowns) -- no CF match — legitimately uncharacterized
                Just cfTuple@(cfVal, _) ->
                    let !contribution = convertAndMultiply unitConfig unitDB (Just flow) cfTuple qty
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
            mflow = M.lookup flowUUID flowDB
         in case mflow of
                Nothing -> 0
                Just _ -> case lookupCascadeCF tables flowDB flowUUID of
                    Nothing -> 0
                    Just cfTuple -> convertAndMultiply unitConfig unitDB mflow cfTuple 1.0

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

-- ────────────────────────────────────────────────────────────────────────────
-- Multi-method scoring (set-level)
-- ────────────────────────────────────────────────────────────────────────────

{- | One method's worth of data inside a 'MethodSetTables'. Carries the full
'MethodTables' plus the original 'Method' so the scoring path can fall back
to the per-method dispatcher ('computeLCIAScoreAuto') for regionalized
methods without having to re-look up anything.
-}
data MethodSetEntry = MethodSetEntry
    { mseMethodId :: !UUID
    , mseMethod :: !Method
    , mseTables :: !MethodTables
    }

{- | Stacked CF tables for scoring the same inventory against many methods at
once. Built once per (db, sortedMethodIds) and cached.

When no method in the set is regionalized ('msAnyRegional == False'), scoring
collapses to a single dense matrix–vector product: one walk over the
inventory, m FMAs per non-zero entry, no per-method inventory traversal. For
PEF (~27 non-regionalized methods) this is the path that wins.

When at least one method is regionalized, the matrix is empty and scoring
falls back to per-method 'computeLCIAScoreAuto' — the regionalized path
needs the biosphere-triple stream and the location hierarchy walk, which
don't compose with the broadcast matvec. Non-regio methods in a mixed set
still benefit indirectly via 'mtBroadcast' (filled in 'fillBroadcastVector').
-}
data MethodSetTables = MethodSetTables
    { msEntries :: !(V.Vector MethodSetEntry)
    -- ^ Per-method full data, in canonical (sorted) order.
    , msAnyRegional :: !Bool
    {- ^ True if any method has a non-empty 'mtRegionalizedCF'. Disables the
    batched matvec path; per-method dispatch is used instead.
    -}
    , msUuidIndex :: !(M.Map UUID Int)
    -- ^ Flow UUID → row in 'msBroadcastMat'. Empty when 'msAnyRegional'.
    , msNFlows :: !Int
    -- ^ @M.size msUuidIndex@. Cached for the matvec inner loop.
    , msBroadcastMat :: !(U.Vector Double)
    {- ^ Flat column-major dense broadcast: @j * m + i@ holds the effective
    CF for method @i@ at flow row @j@. Length @msNFlows * m@. Empty when
    'msAnyRegional'.

    The layout is chosen to match the scoring access pattern: an
    inventory entry maps to one flow row @j@; reading the @m@ CFs across
    methods for that row is then a contiguous slice. With a sparse
    inventory (typical: hundreds of non-zero flows out of thousands)
    this is far cheaper than a row-major dense matvec, which would
    multiply across every flow regardless.
    -}
    , msNMethods :: !Int
    -- ^ @V.length msEntries@ cached for the scoring inner loop.
    }

{- | Build 'MethodSetTables' from per-method 'MethodTables'. The list order
defines the row order of the broadcast matrix and is preserved in
'msEntries'. Callers should sort by 'methodId' for cache-key canonicality.
-}
buildMethodSetTables :: [(Method, MethodTables)] -> MethodSetTables
buildMethodSetTables pairs =
    let entries =
            V.fromList
                [MethodSetEntry (methodId m) m t | (m, t) <- pairs]
        anyRegio = any (not . M.null . mtRegionalizedCF . snd) pairs
        nMethods = V.length entries
     in if anyRegio
            then
                MethodSetTables
                    { msEntries = entries
                    , msAnyRegional = True
                    , msUuidIndex = M.empty
                    , msNFlows = 0
                    , msBroadcastMat = U.empty
                    , msNMethods = nMethods
                    }
            else
                let
                    -- Union of all UUIDs covered by any method's broadcast,
                    -- assigned dense Int row indices.
                    uuidSet =
                        Set.unions
                            [Set.fromList (M.keys (mtBroadcast t)) | (_, t) <- pairs]
                    uuidList = Set.toAscList uuidSet
                    uuidIndex = M.fromList (zip uuidList [0 ..])
                    nFlows = length uuidList
                    -- Column-major: cell (i, j) = mat[j * nMethods + i].
                    -- All m CFs for a single flow j are contiguous so the
                    -- sparse-inventory scoring loop walks contiguous memory.
                    mat = U.create $ do
                        mv <- MU.replicate (nFlows * nMethods) (0.0 :: Double)
                        V.iforM_ entries $ \i e ->
                            mapM_
                                ( \(uuid, cf) -> case M.lookup uuid uuidIndex of
                                    Just j -> MU.unsafeWrite mv (j * nMethods + i) cf
                                    Nothing -> pure ()
                                )
                                (M.toList (mtBroadcast (mseTables e)))
                        pure mv
                 in
                    MethodSetTables
                        { msEntries = entries
                        , msAnyRegional = False
                        , msUuidIndex = uuidIndex
                        , msNFlows = nFlows
                        , msBroadcastMat = mat
                        , msNMethods = nMethods
                        }

{- | Score an inventory against every method in a 'MethodSetTables'.
Returns @(methodId, Right score)@ on success, @(methodId, Left err)@ for a
regionalized method whose coverage is incomplete (mirrors the existing
'computeLCIAScoreAuto' contract).

When the set is fully non-regionalized, this collapses to a single dense
matvec over a stacked broadcast matrix — no per-method inventory walk, no
per-flow unit conversion (already pre-multiplied in 'mtBroadcast').
Otherwise dispatches per-method to preserve the regionalized path's
hierarchy walk + coverage check.
-}
computeLCIAScoreSetFromTables ::
    UnitConfig ->
    UnitDB ->
    FlowDB ->
    Database ->
    Vector ->
    Inventory ->
    M.Map Text [Text] ->
    MethodSetTables ->
    [(UUID, Either Text Double)]
computeLCIAScoreSetFromTables unitCfg unitDB flowDB db scalingVec inventory hier mst
    | msAnyRegional mst = perMethod
    | otherwise = batched
  where
    entries = V.toList (msEntries mst)

    perMethod =
        [ ( mseMethodId e
          , computeLCIAScoreAuto unitCfg unitDB flowDB db scalingVec inventory hier (mseTables e)
          )
        | e <- entries
        ]

    batched =
        let nMethods = msNMethods mst
            mat = msBroadcastMat mst
            uuidIdx = msUuidIndex mst
            entriesV = msEntries mst
            -- Per-method cascade fallback for flows missing from the dense
            -- broadcast index. The mono-method 'fastScore' has the same
            -- fallback to 'lookupCascadeCF' (see 'computeLCIAScoreFromTables');
            -- replicating it here keeps the batched path numerically
            -- equivalent on inventories carrying flows whose UUIDs weren't
            -- in the root flowDB at table-build time — typically cross-DB
            -- merged inventories. Without this fallback the batched path
            -- silently drops those flows while the per-method path catches
            -- them via name/compartment cascade.
            cascadeContrib !tables !uuid !qty =
                case lookupCascadeCF tables flowDB uuid of
                    Nothing -> 0
                    Just cfTuple ->
                        convertAndMultiply unitCfg unitDB (M.lookup uuid flowDB) cfTuple qty
            -- Sparse walk: for each non-zero (uuid, qty) in the inventory,
            -- find its dense row j (if any), then accumulate
            --   scores[i] += qty * mat[j * nMethods + i]
            -- across the m contiguous CFs at that row. nnz × m FMAs total,
            -- contiguous reads — vs a dense matvec which would do
            -- nFlows × m even when the inventory touches a small fraction.
            scores = U.create $ do
                mv <- MU.replicate nMethods (0.0 :: Double)
                mapM_
                    ( \(uuid, qty) ->
                        if qty == 0
                            then pure ()
                            else case M.lookup uuid uuidIdx of
                                Just j -> do
                                    let rowStart = j * nMethods
                                        loop !i
                                            | i >= nMethods = pure ()
                                            | otherwise = do
                                                let !cf = U.unsafeIndex mat (rowStart + i)
                                                MU.unsafeModify mv (+ qty * cf) i
                                                loop (i + 1)
                                    loop 0
                                Nothing ->
                                    -- Out-of-broadcast flow: ask each method's
                                    -- cascade individually. O(m) per missing
                                    -- flow vs O(m) row-read — same cost — but
                                    -- without the silent zero.
                                    V.imapM_
                                        ( \i e -> do
                                            let !c = cascadeContrib (mseTables e) uuid qty
                                            MU.unsafeModify mv (+ c) i
                                        )
                                        entriesV
                    )
                    (M.toList inventory)
                pure mv
         in [ (mseMethodId e, Right (scores U.! i))
            | (i, e) <- zip [0 ..] entries
            ]

-- ──────────────────────────────────────────────
-- Post-scoring suggester
-- ──────────────────────────────────────────────

{- | Top-level CF lookup helper used by the suggester. Same cascade as the
per-function @lookupCF@ helpers inlined elsewhere in this module, but
exposed so 'findUncharacterized' can ask whether a flow has any CF at all.
-}
lookupCFForFlow :: MethodTables -> UUID -> Maybe Flow -> Maybe (Double, Text)
lookupCFForFlow tables fid mFlow = case M.lookup fid (mtUuidCF tables) of
    Just cfv -> Just cfv
    Nothing -> case mFlow of
        Nothing -> Nothing
        Just flow ->
            let name = normalizeName (flowName flow)
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
                baseMed = normalizeMediumTop normMedRaw
                subcomp = normSub
                exact = M.lookup (name, baseMed, subcomp) (mtExactCF tables)
             in case exact of
                    Just _ -> exact
                    Nothing -> M.lookup (name, baseMed) (mtFallbackCF tables)

-- | Top-level variant of the @normalizeMedium@ helper used by the suggester.
normalizeMediumTop :: Text -> Text
normalizeMediumTop m
    | m == "natural resource" = "resource"
    | otherwise = m

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
            flowMedium = normalizeMediumTop . T.takeWhile (/= '/') . T.toLower $ flowCategory flow

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
    [UncharacterizedFlow]
findUncharacterized _ unitDB flowDB inventory tables syns idx opts
    | uoMaxFlows opts <= 0 = []
    | totalAbs == 0 = []
    | otherwise =
        let unmatched =
                [ (flow, qty, w)
                | (fid, qty) <- M.toList inventory
                , qty /= 0
                , Just flow <- [M.lookup fid flowDB]
                , isNothing (lookupCFForFlow tables fid (Just flow))
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
    LCIAOutcome
computeLCIAScoreWithDiagnostics unitConfig unitDB flowDB inventory tables syns idx opts =
    let outcome = computeLCIAScoreFromTables unitConfig unitDB flowDB inventory tables
        diagnostics = findUncharacterized unitConfig unitDB flowDB inventory tables syns idx opts
     in outcome{loUncharacterized = diagnostics}
