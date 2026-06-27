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
    sumRegionalizedLCIAScoreCrossDB,
    computeLCIAScoreWithDiagnostics,
    findUncharacterized,
    findSimilarCFs,
    inventoryContributions,
    processContributionsFromTables,
    lookupCFForFlow,
    convertForCharacterization,
    expandSynonymMappings,
    ProxyTargets (..),
    expandProxyEdges,

    -- * Multi-method scoring
    MethodSetTables (..),
    MethodSetEntry (..),
    BatchedTables (..),
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
    findFlowBySynonymMemo,
    findFlowByCAS,

    -- * Statistics
    MappingStats (..),
    computeMappingStats,
    strategyPriority,
) where

import Control.Applicative ((<|>))
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (mapConcurrently)
import Control.DeepSeq (NFData)
import Control.Monad.ST (runST)
import Data.Aeson (ToJSON)
import Data.Either (lefts, rights)
import Data.List (find, nub, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
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

import qualified Data.Set as Set
import Matrix (Inventory, Vector)
import Method.ChemSynonyms (ChemSynonyms, expandedTokens)
import Method.Types
import Plugin.Types (MapContext (..), MapQuery (..), MapResult (..), MapperHandle (..))
import qualified SubstanceRegistry as SR
import SynonymDB
import Types (Activity (..), BioFlowDB, BiosphereFlow (..), Database (..), ProcessId, SparseTriple (..), Unit (..), UnitDB)
import qualified Types as VT
import UnitConversion (UnitConfig, UnitDef (..), convertUnit, isKnownUnit, lookupUnitDef, normalizeUnit)

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
    | {- | Via a typed @ProxyFor@ edge: a CF borrowed from another flow, scaled
      by the edge's conversion factor. An approximation, ranked below every
      direct match so an explicit CF always wins.
      -}
      ByProxy
    | -- | No match found
      NoMatch
    deriving (Eq, Show)

{- | Per-strategy mapping counters. Forms a 'Monoid' (field-wise sum, all-zero
identity) so per-batch stats compose with '<>' and 'computeMappingStats'
is a single 'foldMap' pass over the mapping list.
-}
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
    , msByProxy :: !Int
    -- ^ Matched via a @ProxyFor@ edge
    , msUnmatched :: !Int
    -- ^ Not matched
    }
    deriving (Eq, Show)

instance Semigroup MappingStats where
    a <> b =
        MappingStats
            (msTotal a + msTotal b)
            (msByUUID a + msByUUID b)
            (msByCAS a + msByCAS b)
            (msByName a + msByName b)
            (msBySynonym a + msBySynonym b)
            (msByFuzzy a + msByFuzzy b)
            (msByProxy a + msByProxy b)
            (msUnmatched a + msUnmatched b)

instance Monoid MappingStats where
    mempty = MappingStats 0 0 0 0 0 0 0 0

-- | Build a MapContext from a Database (convenience for callers)
buildMapContext :: Database -> MapContext
buildMapContext db =
    MapContext
        { mcBioFlowsByUUID = dbBioFlows db
        , mcBioFlowsByName = dbFlowsByName db
        , mcBioFlowsByCAS = dbFlowsByCAS db
        , mcSynonymDB = fromMaybe emptySynonymDB (dbSynonymDB db)
        , mcActivities = M.empty
        , mcSynGroupFlows = M.empty
        }

{- | Map all method flows to database flows using mapper handles.
Mappers are tried in priority order (assumed pre-sorted).
-}
mapMethodFlows ::
    [MapperHandle] ->
    MapContext ->
    Method ->
    IO [(MethodCF, Maybe (BiosphereFlow, MatchStrategy))]
mapMethodFlows mappers ctx0 method = do
    caps <- getNumCapabilities
    let cfs = methodFactors method
        n = length cfs
        -- Precompute the synonym-group → flows memo for the groups this method's
        -- CFs reference (each group expanded once), then resolve every CF against
        -- it. Without it the synonym matcher re-expands a shared group — often a
        -- large closure class — once per CF whose name lands in it.
        ctx = ctx0{mcSynGroupFlows = buildSynGroupFlows ctx0 cfs}
        resolve cf = fmap (cf,) (mapSingleFlow mappers ctx cf)
    -- Each CF resolves independently (first-matching mapper wins, no cross-CF
    -- state), so chunk the CFs across capabilities and resolve the chunks
    -- concurrently. 'mapConcurrently' preserves order and the chunks are
    -- concatenated in order, so the result list — and every table built from it
    -- — is identical to the serial 'mapM'; this is a pure speedup, not a
    -- behaviour change. Chunking (rather than one task per CF) caps the live
    -- thread count at 'caps' on methods with tens of thousands of CFs.
    if caps <= 1 || n < parCfThreshold
        then mapM resolve cfs
        else concat <$> mapConcurrently (mapM resolve) (chunksOf (max 1 ((n + caps - 1) `div` caps)) cfs)
  where
    parCfThreshold = 1000
    chunksOf _ [] = []
    chunksOf k xs = let (h, t) = splitAt k xs in h : chunksOf k t

{- | Map a single CF using the mapper handle cascade.
Each mapper is tried in order; the first match wins.
-}
mapSingleFlow ::
    [MapperHandle] ->
    MapContext ->
    MethodCF ->
    IO (Maybe (BiosphereFlow, MatchStrategy))
mapSingleFlow mappers ctx cf = go mappers
  where
    go [] = pure Nothing
    go (m : ms) = do
        result <- mhMatch m ctx (MatchCF cf)
        case result of
            Just mr
                | Just flow <- M.lookup (mrTargetId mr) (mcBioFlowsByUUID ctx) ->
                    pure $ Just (flow, strategyFromText (mrStrategy mr))
            _ -> go ms

-- | Convenience wrapper: map method CFs using the given mappers + DB.
mapMethodToFlows :: [MapperHandle] -> Database -> Method -> IO [(MethodCF, Maybe (BiosphereFlow, MatchStrategy))]
mapMethodToFlows mappers db = mapMethodFlows mappers (buildMapContext db)

-- | Convert strategy text back to MatchStrategy
strategyFromText :: Text -> MatchStrategy
strategyFromText t = case T.toLower t of
    "uuid" -> ByUUID
    "cas" -> ByCAS
    "name" -> ByName
    "synonym" -> BySynonym
    "fuzzy" -> ByFuzzy
    "proxy" -> ByProxy
    _ -> ByFuzzy -- Unknown strategies map to fuzzy

-- ──────────────────────────────────────────────
-- Low-level matching functions (used by built-in MapperHandles)
-- ──────────────────────────────────────────────

-- | Find flow by exact UUID match
findFlowByUUID :: M.Map UUID BiosphereFlow -> UUID -> Maybe BiosphereFlow
findFlowByUUID flowsByUUID uuid = M.lookup uuid flowsByUUID

-- | Find flow by CAS number with compartment preference
findFlowByCAS :: M.Map Text [BiosphereFlow] -> Text -> Maybe Compartment -> Maybe BiosphereFlow
findFlowByCAS flowsByCAS cas mComp =
    M.lookup cas flowsByCAS >>= \flows -> pickByCompartment flows mComp

-- | Find flow by normalized name match (compartment-aware)
findFlowByName :: M.Map Text [BiosphereFlow] -> Text -> Maybe BiosphereFlow
findFlowByName flowsByName name = findFlowByNameComp flowsByName name Nothing

-- | Find flow by normalized name with compartment preference
findFlowByNameComp :: M.Map Text [BiosphereFlow] -> Text -> Maybe Compartment -> Maybe BiosphereFlow
findFlowByNameComp flowsByName name mComp =
    M.lookup (normalizeName name) flowsByName >>= \flows -> pickByCompartment flows mComp

-- | Find flow via synonym group (compartment-aware)
findFlowBySynonym :: SynonymDB -> M.Map Text [BiosphereFlow] -> Text -> Maybe BiosphereFlow
findFlowBySynonym synDB flowsByName name = findFlowBySynonymComp synDB flowsByName name Nothing

-- | Find flow via synonym group with compartment preference
findFlowBySynonymComp :: SynonymDB -> M.Map Text [BiosphereFlow] -> Text -> Maybe Compartment -> Maybe BiosphereFlow
findFlowBySynonymComp synDB flowsByName name mComp =
    case lookupSynonymGroup synDB name of
        Nothing -> Nothing
        Just gid ->
            getSynonyms synDB gid >>= \synonyms ->
                pickByCompartment (concatMap (lookupFlows flowsByName) synonyms) mComp
  where
    lookupFlows fbn syn = M.findWithDefault [] (normalizeName syn) fbn

{- | Synonym match that reads a precomputed group → flows memo. When
'mapMethodFlows' has expanded this CF's group (the common path), the resolution
is a single map lookup plus the compartment pick; otherwise it falls back to
'findFlowBySynonymComp'. The memoized flow list is exactly that function's
inline @concatMap@ (same elements, same order), so the pick — and the flow it
returns — is identical: a pure speedup.
-}
findFlowBySynonymMemo ::
    SynonymDB ->
    M.Map Int [BiosphereFlow] ->
    M.Map Text [BiosphereFlow] ->
    Text ->
    Maybe Compartment ->
    Maybe BiosphereFlow
findFlowBySynonymMemo synDB memo flowsByName name mComp =
    case lookupSynonymGroup synDB name of
        Nothing -> Nothing
        Just gid -> case M.lookup gid memo of
            Just flows -> pickByCompartment flows mComp
            Nothing -> findFlowBySynonymComp synDB flowsByName name mComp

{- | Expand, once per synonym group, the candidate flows reachable from that
group's names — for exactly the groups the given CFs reference. 'mapMethodFlows'
runs this before resolving a method's CFs, so the synonym matcher reads a shared
(often large) group's flows from the memo instead of re-expanding it per CF. The
per-group value is identical to 'findFlowBySynonymComp''s inline @concatMap@.
-}
buildSynGroupFlows :: MapContext -> [MethodCF] -> M.Map Int [BiosphereFlow]
buildSynGroupFlows ctx cfs =
    M.fromList
        [ (gid, maybe [] (concatMap groupFlows) (getSynonyms synDB gid))
        | gid <- gids
        ]
  where
    synDB = mcSynonymDB ctx
    flowsByName = mcBioFlowsByName ctx
    groupFlows syn = M.findWithDefault [] (normalizeName syn) flowsByName
    gids =
        S.toList $
            S.fromList
                [gid | cf <- cfs, Just gid <- [lookupSynonymGroup synDB (mcfFlowName cf)]]

{- | Pick the best flow match based on compartment preference. The flow's own
compartment now lives in 'bfCompartment' as a structured 'Types.Compartment'
(medium + optional sub); we compare against the method-side 3-field
'Method.Types.Compartment' here.
-}
pickByCompartment :: [BiosphereFlow] -> Maybe Compartment -> Maybe BiosphereFlow
pickByCompartment [] _ = Nothing
pickByCompartment (f : _) Nothing = Just f
pickByCompartment (f : fs) (Just comp) = Just $
    case find (exactCompMatch comp) (f : fs) of
        Just m -> m
        Nothing -> fromMaybe f (find (mediumMatch comp) (f : fs))
  where
    exactCompMatch (Compartment med sub _) fl =
        let cat = T.toLower (VT.bfCompartmentName fl)
            subcomp = maybe "" T.toLower (VT.bfCompartmentSub fl)
         in matchMedium med cat && (T.null sub || sub == subcomp || sub `T.isInfixOf` subcomp)

    mediumMatch (Compartment med _ _) fl =
        matchMedium med (T.toLower (VT.bfCompartmentName fl))

    matchMedium med cat
        | T.null med = True
        | med == cat = True
        | med `T.isInfixOf` cat = True
        | otherwise = False

{- | Per-strategy counts of mapping results in one pass.
Each 'MatchStrategy' must be named below — adding a new variant is a
compile error here until it gets a row.
-}
computeMappingStats :: [(MethodCF, Maybe (BiosphereFlow, MatchStrategy))] -> MappingStats
computeMappingStats = foldMap (tally . fmap snd . snd)
  where
    one = mempty{msTotal = 1}
    tally Nothing = one{msUnmatched = 1}
    tally (Just ByUUID) = one{msByUUID = 1}
    tally (Just ByCAS) = one{msByCAS = 1}
    tally (Just ByName) = one{msByName = 1}
    tally (Just BySynonym) = one{msBySynonym = 1}
    tally (Just ByFuzzy) = one{msByFuzzy = 1}
    tally (Just ByProxy) = one{msByProxy = 1}
    -- 'NoMatch' is not produced by the current matchers; this row exists only
    -- to keep the match exhaustive. Counts as unmatched if ever introduced.
    tally (Just NoMatch) = one{msUnmatched = 1}

{- | Precomputed CF lookup tables for one (database, method) pair.
Building these from raw mappings is O(n log n) over thousands of CFs, so they
should be computed once per method and reused across inventories.
-}
data MethodTables = MethodTables
    { mtUuidCF :: !(M.Map UUID (Double, Text))
    -- ^ UUID-matched CFs: exact flow id → (CF value, CF unit)
    , mtExactCF :: !(M.Map (SR.NormName, Medium, Subcompartment) (Double, Text))
    -- ^ (normalized name, medium, subcompartment) → (CF, unit)
    , mtFallbackCF :: !(M.Map (SR.NormName, Medium) (Double, Text))
    -- ^ (normalized name, medium) → (CF, unit) for entries with unspecified subcompartment
    , mtSubBlindCF :: !(M.Map (SR.NormName, Medium) (Double, Text))
    {- ^ (normalized name, medium) → (CF, unit), but only where the substance's
    factor is the SAME across every subcompartment — i.e. the subcompartment
    genuinely doesn't change it (mineral/metal extraction: "Cadmium, in ground"
    == any sub). Lets a flow whose sub matches no exact CF and has no
    unspecified fallback still resolve, without guessing for a substance whose
    factor DOES vary by sub (water by source), which is omitted as ambiguous.
    -}
    , mtCasCF :: !(M.Map (SR.CASNumber, Medium) (Double, Text))
    {- ^ (CAS, normalized medium) → (CF, unit), from non-regionalized CFs.
    Read-path fallback after UUID and name. Without it, a CF resolves to a
    single database flow at build time, so when many flows share one CAS in a
    compartment (e.g. every water flow shares 7732-18-5) only that one flow is
    characterized and the rest score zero. Keyed by the CF's own CAS+medium so
    the read path can reach every same-CAS flow by its own CAS+medium. The
    bridge is deliberately subcompartment-blind: a resource flow and the CF
    that characterizes it routinely disagree on subcompartment after
    normalization, so requiring agreement zeroes whole resource categories
    (minerals are reachable only through this bridge). Empty for methods whose
    CFs carry no CAS.
    -}
    , mtRegionalCasCF :: !(M.Map (SR.CASNumber, Medium) (M.Map Text (Double, Text)))
    {- ^ (CAS, normalized medium) → (location → (CF, unit)), from regionalized
    CFs. The regionalized analogue of 'mtCasCF': lets the regionalized build
    characterize every same-CAS flow per location, not just the one a CF
    resolved to. Empty for methods with no regionalized CAS-bearing CFs.
    -}
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
    , mtEnergyDensities :: !EnergyDensityMap
    {- ^ Normalized flow name → energy density (MJ per native flow unit).
    Lets an energy-denominated CF (dimension @energy@, e.g. a JRC fossil CF in
    MJ) characterize a mass/volume inventory flow (kg, Sm3): the flow quantity
    is converted to energy via its density before the CF multiply. Flows with
    no entry behave exactly as before — an energy CF vs. a mass flow still
    yields a zero effective CF. Empty map = feature inactive.
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
    M.Map Text [BiosphereFlow] ->
    [(MethodCF, Maybe (BiosphereFlow, MatchStrategy))] ->
    [(MethodCF, Maybe (BiosphereFlow, MatchStrategy))]
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

{- | Database flow indexes a @ProxyFor@ edge's @to@ side resolves against, keyed
the three ways a proxy can name it: normalized name, CAS, and flow UUID.
-}
data ProxyTargets = ProxyTargets
    { ptByName :: !(M.Map Text [BiosphereFlow])
    , ptByCAS :: !(M.Map Text [BiosphereFlow])
    , ptByUUID :: !(M.Map UUID BiosphereFlow)
    }

{- | Apply @ProxyFor@ edges at method-table build — the directional counterpart of
'expandSynonymMappings'. For each edge @from -(f)-> to@, every method CF identified
by @from@ contributes a CF scaled by @f@ to every database flow identified by @to@,
tagged 'ByProxy' so it keys under that flow's name and loses to any direct match.

Only 'SR.ProxyFor' edges act here; @SameAs@/@Subsumes@/@DistinctFrom@ live in the
class/diagnostics layer. An empty edge list is the identity, so the method tables are
unchanged when no @substance_edges.csv@ is loaded.
-}
expandProxyEdges ::
    ProxyTargets ->
    [SR.SubstanceEdge] ->
    [(MethodCF, Maybe (BiosphereFlow, MatchStrategy))] ->
    [(MethodCF, Maybe (BiosphereFlow, MatchStrategy))]
expandProxyEdges targets edges mappings =
    mappings
        ++ [ (cf{mcfValue = mcfValue cf * f}, Just (toFlow, ByProxy))
           | (from, to, f) <- proxies
           , cf <- cfsMatching from
           , toFlow <- flowsMatching to
           ]
  where
    proxies =
        [ (SR.seFrom e, SR.seTo e, f)
        | e <- edges
        , SR.ProxyFor (SR.ConversionFactor f) <- [SR.seRelation e]
        ]

    cfs = map fst mappings
    cfsByName = M.fromListWith (++) [(normalizeName (mcfFlowName cf), [cf]) | cf <- cfs]
    cfsByCAS = M.fromListWith (++) [(cas, [cf]) | cf <- cfs, Just cas <- [mcfCAS cf], not (T.null cas)]
    cfsByUUID = M.fromListWith (++) [(mcfFlowRef cf, [cf]) | cf <- cfs]

    cfsMatching (SR.ByName _ (SR.NormName n)) = M.findWithDefault [] n cfsByName
    cfsMatching (SR.ByCAS (SR.CASNumber c)) = M.findWithDefault [] c cfsByCAS
    cfsMatching (SR.ByUUID (SR.FlowUUID u)) = M.findWithDefault [] u cfsByUUID

    flowsMatching (SR.ByName _ (SR.NormName n)) = M.findWithDefault [] n (ptByName targets)
    flowsMatching (SR.ByCAS (SR.CASNumber c)) = M.findWithDefault [] c (ptByCAS targets)
    flowsMatching (SR.ByUUID (SR.FlowUUID u)) = maybe [] pure (M.lookup u (ptByUUID targets))

{- | Cascade-order rank of a match strategy (UUID → name → synonym → CAS →
heuristic/expanded): when two CFs collide on one flow or table key, the lower
rank — the more discriminating match — wins. Exported so diagnostics dedup
with the same preference the score tables use.
-}
strategyPriority :: MatchStrategy -> Int
strategyPriority ByUUID = 0
strategyPriority ByName = 1
strategyPriority BySynonym = 2
strategyPriority ByCAS = 3
strategyPriority ByFuzzy = 4
strategyPriority ByProxy = 4
strategyPriority NoMatch = 4

buildMethodTables :: CompartmentMap -> EnergyDensityMap -> [(MethodCF, Maybe (BiosphereFlow, MatchStrategy))] -> MethodTables
buildMethodTables cmap energyDensities mappings =
    MethodTables
        { mtUuidCF =
            -- Non-regionalized rows only, like the name tables below: a
            -- location-specific row landing here would let one arbitrary
            -- location's value stand for the flow everywhere ('M.fromList'
            -- keeps the last row). Regionalized UUID-matched rows reach
            -- 'mtRegionalizedCF' keyed by flow UUID + location.
            M.fromList
                [ (bfId flow, (mcfValue cf, mcfUnit cf))
                | (cf, Just (flow, ByUUID)) <- mappings
                , Nothing <- [mcfConsumerLocation cf]
                ]
        , -- The broadcast name tables (exact + fallback) hold only
          -- non-regionalized CFs. Location-specific rows belong to
          -- 'mtRegionalizedCF' / 'mtRegionalCasCF'; letting them in here makes
          -- every location compete for one name key and 'preferBetter' crowns
          -- an arbitrary location's value (e.g. a region whose factor is 0
          -- silently erases the global credit).
          mtExactCF =
            stripStrategy $
                M.fromListWith
                    preferBetter
                    [ ((SR.NormName (nameKey cf mflow), Medium normMed, Subcompartment normSub), (mcfValue cf, mcfUnit cf, matchStrategy mflow))
                    | (cf, mflow) <- mappings
                    , Nothing <- [mcfConsumerLocation cf]
                    , Just comp <- [mcfCompartment cf]
                    , let Compartment normMedRaw normSub _ = normalizeCompartment cmap comp
                    , let normMed = normalizeMedium (T.toLower normMedRaw)
                    , not (T.null normSub)
                    ]
        , mtFallbackCF =
            -- Medium-level default CF for a flow whose specific subcompartment
            -- has no exact CF. Holds CFs whose own subcompartment is empty or
            -- "unspecified" — the value a method intends as the catch-all for
            -- that (name, medium). A flow at an uncovered subcompartment (e.g.
            -- a radionuclide emitted to "low population density, long-term",
            -- which EF leaves uncharacterized) thus picks up the unspecified CF,
            -- as ecoinvent's own implementation does, instead of scoring zero.
            -- This is safe against the long-term toxicity case: a substance
            -- that DOES carry a "(long-term)" CF keeps it in 'mtExactCF', which
            -- is consulted before this fallback, so long-term emissions still
            -- resolve to their explicit (often zero) factor.
            stripStrategy $
                M.fromListWith
                    preferBetter
                    [ ((SR.NormName (nameKey cf mflow), Medium normMed), (mcfValue cf, mcfUnit cf, matchStrategy mflow))
                    | (cf, mflow) <- mappings
                    , Nothing <- [mcfConsumerLocation cf]
                    , Just comp <- [mcfCompartment cf]
                    , let Compartment normMedRaw normSub _ = normalizeCompartment cmap comp
                    , let normMed = normalizeMedium (T.toLower normMedRaw)
                    , isUnspecifiedSub normSub
                    ]
        , mtSubBlindCF =
            -- Keep a (name, medium) entry only when every subcompartment's CF
            -- agrees: then the sub is irrelevant and a sub-mismatched flow can
            -- safely borrow it. A substance whose factor varies by sub is
            -- dropped here, so this never guesses across a real distinction.
            M.mapMaybe agreedValue $
                M.fromListWith
                    (++)
                    [ ((SR.NormName (nameKey cf mflow), Medium normMed), [(mcfValue cf, mcfUnit cf)])
                    | (cf, mflow) <- mappings
                    , Nothing <- [mcfConsumerLocation cf]
                    , Just comp <- [mcfCompartment cf]
                    , let Compartment normMedRaw _ _ = normalizeCompartment cmap comp
                    , let normMed = normalizeMedium (T.toLower normMedRaw)
                    ]
        , mtCasCF =
            -- Keyed by the CF's own CAS + medium (not by a matched flow), so the
            -- read path reaches every database flow sharing that CAS+medium —
            -- the fix for many flows collapsing onto one CAS (e.g. water).
            -- Subcompartment-blind on purpose: a resource flow and the CF that
            -- characterizes it routinely disagree on subcompartment, so a
            -- subcomp-strict bridge would zero whole resource categories.
            -- Non-regionalized CFs only; the regionalized ones go to
            -- 'mtRegionalCasCF'.
            --
            -- Only CFs that matched by CAS populate this table. A CF whose name
            -- or synonym pinned a specific flow (e.g. "methane (biogenic)" →
            -- "Methane, non-fossil") is name-discriminated: broadcasting it to
            -- every same-CAS flow would leak it onto a sibling the method
            -- distinguishes (fossil methane), so it stays out of the CAS bridge.
            --
            -- When several CFs share one (CAS, medium), broadcast the
            -- substance's medium-level default — its unspecified-subcompartment
            -- factor — rather than the largest. The largest is often a niche
            -- subcompartment (indoor air can be ~100x the outdoor value) that
            -- would over-characterize every same-CAS flow the bridge reaches.
            M.map snd $
                M.fromListWith
                    preferUnspecifiedCas
                    [ ((SR.CASNumber cas, Medium normMed), (casSubRank normSub, (mcfValue cf, mcfUnit cf)))
                    | (cf, Just (_, ByCAS)) <- mappings
                    , Just cas <- [mcfCAS cf]
                    , not (T.null cas)
                    , Nothing <- [mcfConsumerLocation cf]
                    , Just comp <- [mcfCompartment cf]
                    , let Compartment normMedRaw normSub _ = normalizeCompartment cmap comp
                    , let normMed = normalizeMedium (T.toLower normMedRaw)
                    ]
        , mtRegionalCasCF =
            -- Same unspecified-default preference as 'mtCasCF', applied per
            -- location: when several subcompartment CFs collide on one
            -- (CAS, medium, location) the medium-level default wins over a
            -- niche subcompartment, rather than the largest magnitude.
            M.map (M.map snd) $
                M.fromListWith
                    (M.unionWith preferUnspecifiedCas)
                    [ ((SR.CASNumber cas, Medium normMed), M.singleton loc (casSubRank normSub, (mcfValue cf, mcfUnit cf)))
                    | (cf, Just (_, ByCAS)) <- mappings
                    , Just cas <- [mcfCAS cf]
                    , not (T.null cas)
                    , Just loc <- [mcfConsumerLocation cf]
                    , Just comp <- [mcfCompartment cf]
                    , let Compartment normMedRaw normSub _ = normalizeCompartment cmap comp
                    , let normMed = normalizeMedium (T.toLower normMedRaw)
                    ]
        , mtRegionalizedCF =
            -- Filter: a CF whose own compartment carries a specific subcomp
            -- (e.g. "groundwater, long-term" or "ocean") must only apply to
            -- flows in that exact subcomp — otherwise a CF=0 set explicitly for
            -- a niche subcomp leaks onto flows in other subcomps via
            -- ByName/synonym fan-out and clobbers the correct (unspecified)
            -- fallback CF. CFs with no specific subcomp ('isUnspecifiedSub')
            -- are wildcards and match any flow subcomp.
            M.fromList
                [ ((bfId flow, loc), (mcfValue cf, mcfUnit cf))
                | (cf, Just (flow, _)) <- mappings
                , cfSubcompMatchesFlow cf flow
                , Just loc <- [mcfConsumerLocation cf]
                ]
        , mtCompartmentMap = cmap
        , mtEnergyDensities = energyDensities
        , mtBroadcast = M.empty -- fill via 'fillBroadcastVector' to enable the fast path
        , mtRegionalActivityWeights = Nothing -- fill via 'fillRegionalActivityWeights' for regional fast path
        }
  where
    stripStrategy = M.map (\(v, u, _) -> (v, u))

    -- All subcompartments of a (name, medium) agree on the CF ⇒ the sub is
    -- irrelevant; return that common value. Disagreement ⇒ Nothing (ambiguous).
    agreedValue vus = case nub vus of
        [vu] -> Just vu
        _ -> Nothing

    -- For the CAS bridge: rank the unspecified / empty subcompartment ahead of
    -- any specific one, so 'preferUnspecifiedCas' keeps the medium-level
    -- default value when several subcompartment CFs collide on one key. On a
    -- tie (no unspecified CF present) keep the larger magnitude — the bridge is
    -- a last-resort fallback, so an overstated factor surfaces in validation
    -- while an understated one would be invisible.
    casSubRank s
        | isUnspecifiedSub s = 0 :: Int
        | otherwise = 1
    preferUnspecifiedCas a@(ra, (va, _)) b@(rb, (vb, _))
        | ra /= rb = if ra < rb then a else b
        | abs va >= abs vb = a
        | otherwise = b

    -- A CF whose subcomp names no specific subcompartment ('isUnspecifiedSub')
    -- is a wildcard. A CF with a specific subcomp must match the flow's subcomp
    -- exactly — otherwise an explicit-zero niche-subcomp CF would clobber the
    -- correct (unspecified) CF for flows in other subcomps via ByName/synonym
    -- fan-out.
    --
    -- Both sides go through 'normalizeCompartment' so a compartments.csv rule
    -- that rewrites a subcomp can't desynchronise the filter from the sibling
    -- 'mtExactCF' / 'mtFallbackCF' tables or the 'lookupCascadeCF' read path.
    -- Flow subcomp resolution mirrors 'lookupCascadeCF': prefer the explicit
    -- 'compartmentSub' field, fall back to the tail of "<medium>/<sub>"
    -- parsed from the compartment name.
    cfSubcompMatchesFlow cf flow = case mcfCompartment cf of
        Nothing -> True
        Just comp ->
            let Compartment _ cfSubRaw _ = normalizeCompartment cmap comp
                !cfSubN = T.toLower (T.strip cfSubRaw)
                rawCategory = T.toLower (VT.bfCompartmentName flow)
                (rawMed, rawSubFromCat) = case T.breakOn "/" rawCategory of
                    (m, rest)
                        | T.null rest -> (m, T.empty)
                        | otherwise -> (m, T.drop 1 rest)
                rawSub =
                    let s = T.toLower (fromMaybe T.empty (VT.bfCompartmentSub flow))
                     in if T.null s then rawSubFromCat else s
                Compartment _ flowSubRaw _ =
                    normalizeCompartment cmap (Compartment rawMed rawSub T.empty)
                !flowSubN = T.toLower (T.strip flowSubRaw)
             in isUnspecifiedSub cfSubN
                    || cfSubN == flowSubN

    preferBetter (v1, u1, s1) (v2, u2, s2)
        | strategyPriority s1 < strategyPriority s2 = (v1, u1, s1)
        | strategyPriority s1 > strategyPriority s2 = (v2, u2, s2)
        | v1 >= v2 = (v1, u1, s1)
        | otherwise = (v2, u2, s2)

    matchStrategy mflow = case mflow of
        Just (_, s) -> s
        Nothing -> NoMatch

    -- Use matched flow's name only for name/synonym/proxy matches: those key
    -- the CF under the database flow it resolved to, not the method CF's own name.
    nameKey cf mflow = normalizeName $ case mflow of
        Just (flow, ByName) -> bfName flow
        Just (flow, BySynonym) -> bfName flow
        Just (flow, ByProxy) -> bfName flow
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
fillBroadcastVector :: UnitConfig -> UnitDB -> BioFlowDB -> MethodTables -> MethodTables
fillBroadcastVector unitConfig unitDB flowDB tables =
    tables{mtBroadcast = M.mapMaybeWithKey buildEntry flowDB}
  where
    buildEntry fid flow = case lookupCascadeCF tables flowDB fid of
        Nothing -> Nothing
        Just cfTuple -> Just (convertAndMultiply unitConfig unitDB (mtEnergyDensities tables) (Just flow) cfTuple 1.0)

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

Score-time semantics: 'computeRegionalizedLCIAScore' returns a partial
score that under-counts tainted activities (matching the broadcast path's
silent-omission behaviour for non-regio flows, and SimaPro). The coverage
gap is surfaced once via 'rawMissingPairs' + the build-time WARN, not by
'Left'-ing the whole method — that contract used to be strict but masked
every other column's valid contribution as soon as one (flow, location)
pair was uncovered.

No-op when 'mtRegionalizedCF tables' is empty — non-regionalized methods
keep 'mtRegionalActivityWeights = Nothing' so the broadcast fast path stays
the right answer.
-}
fillRegionalActivityWeights ::
    UnitConfig ->
    UnitDB ->
    BioFlowDB ->
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
    bioFlows = dbBiosphereOrder db
    bioTriples = dbBiosphereTriples db
    regional = mtRegionalizedCF tables

    -- Flow-row-indexed view of 'regional'. For each biosphere flow row index
    -- @r@, @regionalByRow V.! r@ is @Just locMap@ when that flow has any
    -- regionalized CF, @Nothing@ otherwise. Most biosphere flows in a
    -- typical inventory are not regionalized, so the hot loop's outer test
    -- becomes a single 'V.!' + Nothing match instead of two
    -- @M.lookup (UUID, Text)@ walks (exact + parents) against the big shared
    -- @(UUID, Text)@-keyed map. Subsumes the old
    -- @regionalizedFlows :: Set UUID@ check — @Just _@ here is the
    -- "this flow is regionalized" signal needed at the taint branch.
    regionalByRow :: V.Vector (Maybe (M.Map Text (Double, Text)))
    regionalByRow =
        let perFlow :: M.Map UUID (M.Map Text (Double, Text))
            perFlow =
                M.fromListWith
                    M.union
                    [(f, M.singleton loc cf) | ((f, loc), cf) <- M.toList regional]
            regionalCas = mtRegionalCasCF tables
            cmap = mtCompartmentMap tables
            -- Direct flow→locMap, then fall back to the flow's own CAS+medium so
            -- every flow sharing a CAS is regionalized per location — not just
            -- the one a CF resolved to at build time.
            lookupRow fid =
                M.lookup fid perFlow
                    <|> ( M.lookup fid flowDB >>= \flow ->
                            bfCAS flow
                                >>= \cas -> M.lookup (SR.CASNumber cas, fst (flowMediumSub cmap flow)) regionalCas
                        )
         in V.map lookupRow bioFlows

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

    -- Cached locally so the hot loop reads from one stable pointer per field.
    broadcast = mtBroadcast tables

    -- Walk biosphere triples once; build weights, tainted flags and the
    -- deduplicated missing-(flow, location) set in a single ST action.
    --
    -- The CF cascade is inlined here so the universal-fallback branch can
    -- read from 'mtBroadcast' (pre-multiplied by unit conversion at
    -- 'fillBroadcastVector' time) instead of re-running 'lookupCascadeCF'
    -- which calls 'normalizeName' per triple. Profiling on EF31-biomaps ×
    -- agribalyse showed 'normalizeName' eating ~83% of fillRegional time
    -- across 90M biosphere triples; the broadcast map already has the
    -- answer the cascade was recomputing.
    --
    -- The parent-region fallback uses a manual short-circuit recursion
    -- ('lookupParents') rather than @firstJust [M.lookup .. | p <- parents]@:
    -- the list comprehension materialised a cons cell per parent per triple,
    -- accounting for ~30% of warmup heap allocations on EF31-biomaps. The
    -- manual recursion stops at the first hit and allocates nothing.
    precomputed :: RegionalActivityWeights
    precomputed = runST $ do
        ws <- MU.replicate nCols (0 :: Double)
        ts <- MU.replicate nCols (0 :: Word8)
        missRef <- newSTRef (Set.empty :: Set.Set (UUID, Text))
        U.forM_ bioTriples $ \(SparseTriple flowRow colIdx bioVal) -> do
            let !col = fromIntegral colIdx :: Int
                !row = fromIntegral flowRow :: Int
                !flowUUID = bioFlows V.! row
                applyRaw cfTuple =
                    let !contribution =
                            convertAndMultiply
                                unitCfg
                                unitDB
                                (mtEnergyDensities tables)
                                (M.lookup flowUUID flowDB)
                                cfTuple
                                bioVal
                     in MU.unsafeModify ws (+ contribution) col
                -- 'mtBroadcast' is the unit-converted CF per unit of flow, so
                -- the contribution is @bioVal * preMultipliedCF@ — same
                -- algebra as 'computeLCIAScoreFromTables's fast path.
                applyBroadcast = case M.lookup flowUUID broadcast of
                    Just preMultipliedCF ->
                        MU.unsafeModify ws (+ bioVal * preMultipliedCF) col
                    Nothing -> pure ()
            case regionalByRow V.! row of
                -- Common case: flow is not regionalized at all. No exact /
                -- parent walk; just broadcast (universal CF) if present.
                Nothing -> applyBroadcast
                Just locMap ->
                    let !loc = colLoc V.! col
                        -- Walk the parent locations until one yields a CF.
                        -- Allocates no list cells — replaces
                        -- @firstJust [.. | p <- parents]@.
                        lookupParents [] = Nothing
                        lookupParents (p : ps) = case M.lookup p locMap of
                            Just cf -> Just cf
                            Nothing -> lookupParents ps
                     in case M.lookup loc locMap of
                            Just cfTuple -> applyRaw cfTuple
                            Nothing -> case lookupParents (M.findWithDefault [] loc hier) of
                                Just cfTuple -> applyRaw cfTuple
                                Nothing -> case M.lookup flowUUID broadcast of
                                    Just preMultipliedCF ->
                                        MU.unsafeModify ws (+ bioVal * preMultipliedCF) col
                                    Nothing -> do
                                        -- Flow IS regionalized (locMap is
                                        -- @Just _@) but has no CF for this
                                        -- location even after parents and
                                        -- no universal broadcast — taint.
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
computeLCIAScoreFromTables :: UnitConfig -> UnitDB -> BioFlowDB -> Inventory -> MethodTables -> LCIAOutcome
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
        Just cfTuple -> Just (convertAndMultiply unitConfig unitDB (mtEnergyDensities tables) (M.lookup fid flowDB) cfTuple qty)

{- | Back-compat wrapper: build tables on the fly. Prefer the cached path
('mapMethodToTablesCached' + 'computeLCIAScoreFromTables') in hot loops.
-}
computeLCIAScore :: UnitConfig -> UnitDB -> BioFlowDB -> Inventory -> [(MethodCF, Maybe (BiosphereFlow, MatchStrategy))] -> LCIAOutcome
computeLCIAScore unitConfig unitDB flowDB inventory mappings =
    computeLCIAScoreFromTables unitConfig unitDB flowDB inventory (buildMethodTables M.empty M.empty mappings)

{- | LCIA score with automatic dispatch.

If the method has no regionalized CFs ('mtRegionalizedCF' empty), uses the
classic vector path ('computeLCIAScoreFromTables'). Otherwise switches to
the matrix path ('computeRegionalizedLCIAScore').

The caller is expected to provide both an 'Inventory' (cheap if already
computed for other purposes) and a scaling 'Vector' (cheap if the MUMPS
factorization is cached). Pass them both and let this function pick.

Returns 'Either' so the regionalized path can surface integrity errors
(scaling/weights length mismatch, weights absent) explicitly. Coverage
gaps are NOT surfaced here — they appear once at table-build time as a
WARN listing the uncovered (flow, location) pairs; score-time returns
the partial 'Right'.
-}
computeLCIAScoreAuto ::
    UnitConfig ->
    UnitDB ->
    BioFlowDB ->
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
    BioFlowDB ->
    Database ->
    -- | Scaling vector @s@ from 'Matrix.computeScalingVector'
    Vector ->
    -- | Location hierarchy: child → ordered list of parents
    M.Map Text [Text] ->
    MethodTables ->
    Either Text Double
computeRegionalizedLCIAScore _unitConfig _unitDB _flowDB _db scalingVec _hier tables =
    case mtRegionalActivityWeights tables of
        Just raw -> scoreFromPrecomputed raw scalingVec
        Nothing
            -- Cross-DB scoring passes per-DB tables in; a dep DB whose flow
            -- mappings caught none of this method's regional CFs has empty
            -- 'mtRegionalizedCF', so 'fillRegionalActivityWeights' left
            -- 'mtRegionalActivityWeights' unfilled. Treat as a 0
            -- contribution rather than erroring — non-regional emissions
            -- from that DB are handled by the broadcast pass for which the
            -- DB built 'rawWeights' from its own broadcast cell.
            | M.null (mtRegionalizedCF tables) -> Right 0
            | otherwise ->
                Left
                    "Regionalized score requested but precomputed activity weights\
                    \ are absent. Call 'fillRegionalActivityWeights' on the\
                    \ MethodTables before scoring (mapMethodToTablesCached does\
                    \ this automatically)."
  where
    -- Fast path: one dot product over precomputed per-column weights.
    -- Tainted columns contribute 0 by construction in
    -- 'fillRegionalActivityWeights' (weights[i] == 0 when no CF matched),
    -- so summing them yields the correct partial score. The coverage gap is
    -- surfaced once at table-build time via 'rawMissingPairs' + the WARN
    -- in 'Database.Manager' — not by collapsing the whole method to a
    -- 'Left', which forced every category with even one uncovered
    -- (flow, location) pair to 0 µPt and masked the partial score.
    -- Matches SimaPro behaviour.
    --
    -- 'Left' is reserved here for genuine integrity errors (length mismatch
    -- below; "weights absent" in the outer 'case'), not coverage gaps.
    scoreFromPrecomputed raw s =
        let !weights = rawWeights raw
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
                    let go !i !acc
                            | i >= n = acc
                            | otherwise =
                                let !sv = U.unsafeIndex s i
                                 in if sv == 0
                                        then go (i + 1) acc
                                        else go (i + 1) (acc + sv * U.unsafeIndex weights i)
                        !score = go 0 0
                     in Right score

{- | Cross-DB regionalized LCIA score.

For each participating database @d@ (root + each dep DB reached at request
time), call 'computeRegionalizedLCIAScore' against THAT DB's scaling
vector and MethodTables, then sum the per-DB scores. Equivalent to one
dot product over the concatenated activity space
@[s_root, s_dep1, …] · [w_root, w_dep1, …]@ — just computed per-DB so
each side keeps its own activity index, hierarchy walks and tainted-column
diagnostics local.

Closes the gap where dep-DB biosphere emissions are present in the merged
inventory but invisible to the regional dot product (which was previously
keyed on the root DB's activity columns alone).

Coverage gaps no longer show up here: 'computeRegionalizedLCIAScore'
returns 'Right' with tainted columns contributing 0, so an incomplete-
coverage DB just contributes its partial score to the sum. The build-time
WARN + 'rawMissingPairs' per @(db, method)@ is the single source of truth
for what's missing — score time stays quiet.

Per-DB 'Left' is tolerated for genuine integrity errors (scaling/weights
length mismatch, weights absent on a regionalized method): that DB drops
to a 0 contribution and the function keeps summing the rest, preserving
the partial answer over the other DBs.

Returns 'Left' only when 'every' triple's score is 'Left' — i.e. every
participating DB hit an integrity error and there is no salvageable
contribution. The concatenated error messages are surfaced so the caller
can act on them.
-}
sumRegionalizedLCIAScoreCrossDB ::
    UnitConfig ->
    UnitDB ->
    BioFlowDB ->
    M.Map Text [Text] ->
    {- | Per-DB triples: one per database participating in the cross-DB solve
    (root + dep DBs in the same order returned by 'SharedSolver.csScalings').
    -}
    [(Database, Vector, MethodTables)] ->
    Either Text Double
sumRegionalizedLCIAScoreCrossDB unitCfg unitDB flowDB hier triples =
    let results = [computeRegionalizedLCIAScore unitCfg unitDB flowDB db sv hier t | (db, sv, t) <- triples]
        oks = rights results
        errs = lefts results
     in case (oks, errs) of
            ([], es) | not (null es) -> Left (T.intercalate "; " es)
            _ -> Right (sum oks)

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
lookupCascadeCF :: MethodTables -> BioFlowDB -> UUID -> Maybe (Double, Text)
lookupCascadeCF tables flowDB fid =
    M.lookup fid (mtUuidCF tables)
        <|> (M.lookup fid flowDB >>= byNameOrCas)
  where
    byNameOrCas flow =
        let name = SR.NormName (normalizeName (bfName flow))
            (baseMed, normSub) = flowMediumSub (mtCompartmentMap tables) flow
         in -- UUID/name miss → fall back to the flow's own CAS + medium, so
            -- every flow sharing a CAS in a compartment is characterized, not
            -- just the one a CF resolved to at build time.
            M.lookup (name, baseMed, normSub) (mtExactCF tables)
                <|> M.lookup (name, baseMed) (mtFallbackCF tables)
                <|> (bfCAS flow >>= \cas -> M.lookup (SR.CASNumber cas, baseMed) (mtCasCF tables))
                <|> M.lookup (name, baseMed) (mtSubBlindCF tables)
                <|> regionBaseFallback flow baseMed normSub
                <|> energyResourceFallback flow baseMed normSub

    -- A SimaPro region-suffixed flow ("Ammonia, FR") whose region the method
    -- doesn't tag falls back to the base substance's CF: an unregionalized CF
    -- for "Ammonia" applies to the emission wherever it occurs. Only fires
    -- after every direct lookup misses, and only when the suffix is a real
    -- region code (extractLocationSuffix leaves "Methane, fossil" untouched).
    regionBaseFallback flow baseMed normSub =
        case extractLocationSuffix (bfName flow) of
            (base, Just _) ->
                let bname = SR.NormName (normalizeName base)
                 in M.lookup (bname, baseMed, normSub) (mtExactCF tables)
                        <|> M.lookup (bname, baseMed) (mtFallbackCF tables)
            _ -> Nothing

    -- An energy-resource flow whose name encodes its density ("Coal, 18 MJ per
    -- kg") borrows the CF of its resource family (coal/oil/gas/uranium…) — the
    -- generic per-MJ resource CF. The density itself is applied downstream by
    -- 'convertAndMultiply', which name-parses the same suffix, so here we only
    -- return the base CF. The resource family is resolved through the known
    -- energy resources ('mtEnergyDensities'), so an unknown resource never
    -- borrows a CF. Last in the cascade: only fires when all else misses.
    energyResourceFallback flow baseMed normSub =
        case parseEnergyDensitySuffix (bfName flow) of
            Nothing -> Nothing
            Just (base, _) ->
                let fam = firstWord (normalizeName base)
                    candidates =
                        [ cf
                        | rname <- M.keys (mtEnergyDensities tables)
                        , firstWord rname == fam
                        , Just cf <- [resourceCF (SR.NormName rname) baseMed normSub]
                        ]
                 in -- Borrow only when the family's resolving CFs agree (the generic
                    -- per-MJ factor). If "Coal, hard" and "Coal, brown" disagree the
                    -- family CF is ambiguous, so drop rather than pick one arbitrarily
                    -- by Map order — same "never guess" rule as 'agreedValue'.
                    case nub candidates of
                        [cf] -> Just cf
                        _ -> Nothing

    resourceCF rname baseMed normSub =
        M.lookup (rname, baseMed, normSub) (mtExactCF tables)
            <|> M.lookup (rname, baseMed) (mtFallbackCF tables)

    firstWord = T.takeWhile (/= ' ') . T.strip

-- | Normalize medium names between method CFs and database flows.
normalizeMedium :: Text -> Text
normalizeMedium m
    | m == "natural resource" = "resource"
    | otherwise = m

{- | A subcompartment that names no specific subcompartment — empty, or either
spelling of unspecified. Such a CF is the medium-level default: it
characterizes any flow in that medium whose own subcompartment the method
doesn't cover. The single source of truth for "is this the catch-all subcomp",
so the fallback table, the CAS-bridge rank, and the regionalized wildcard
filter can't drift apart on which spellings count (they did: the filter once
omitted bare @unspecified@). Inputs are expected already lower-cased via
'normalizeCompartment'.
-}
isUnspecifiedSub :: Text -> Bool
isUnspecifiedSub s = T.null s || s == "unspecified" || s == "(unspecified)"

{- | The @(normalized medium, subcompartment)@ a database flow resolves to after
compartment normalization. Shared by the name/CAS read path
('lookupCascadeCF') and the regionalized CAS fallback so both key a flow the
same way. Subcomp resolution prefers the explicit 'compartmentSub' field,
falling back to the tail of a @"medium/sub"@ category name.
-}
flowMediumSub :: CompartmentMap -> BiosphereFlow -> (Medium, Subcompartment)
flowMediumSub cmap flow =
    let rawCategory = T.toLower (VT.bfCompartmentName flow)
        (rawMed, rawSubFromCat) = case T.breakOn "/" rawCategory of
            (m, rest)
                | T.null rest -> (m, T.empty)
                | otherwise -> (m, T.drop 1 rest)
        rawSub =
            let s = T.toLower (fromMaybe T.empty (VT.bfCompartmentSub flow))
             in if T.null s then rawSubFromCat else s
        Compartment normMedRaw normSub _ =
            normalizeCompartment cmap (Compartment rawMed rawSub T.empty)
     in (Medium (normalizeMedium normMedRaw), Subcompartment normSub)

{- | True when @unit@ is dimensionally an energy unit (same exponent vector as
the reference @mj@). Used to detect the energy-CF-vs-non-energy-flow case the
energy-density bridge targets, without hardcoding any unit string.
-}
isEnergyUnit :: UnitConfig -> Text -> Bool
isEnergyUnit cfg unit =
    case (lookupUnitDef cfg unit, lookupUnitDef cfg "MJ") of
        (Just a, Just energyRef) -> udDimension a == udDimension energyRef
        _ -> False

{- | Flow→CF conversion factor for @qty@ units of flow, applying the
energy-density bridge when it is needed and available.

The bridge fires only when the CF is energy-denominated, the flow is __not__
(its unit isn't an energy unit), and the flow carries an 'EnergyDensity'. The
inventory quantity is first brought into the density's native unit, then turned
into energy: @qNative × E@, where @E@ is the density value converted from its
declared energy unit into the CF's unit. Either conversion failing — the flow
unit can't reach the native unit, or the energy unit can't reach the CF unit —
yields @0@: we refuse a wrong-basis or wrong-dimension factor rather than
silently using the raw value.

Every other case (matching dimensions, no density, or an energy flow) defers
to 'convertForCharacterization', so flows without a density are unchanged.
-}
energyAwareConversion :: UnitConfig -> Text -> Text -> Maybe EnergyDensity -> Double -> Double
energyAwareConversion cfg flowUnit cfUnit mDensity qty =
    case mDensity of
        Just (EnergyDensity ev energyUnit nativeUnit)
            | isEnergyUnit cfg cfUnit
            , not (isEnergyUnit cfg flowUnit) ->
                fromMaybe 0 $ do
                    qtyNative <- toNativeQty flowUnit nativeUnit
                    factor <- convertUnit cfg energyUnit cfUnit ev
                    pure (qtyNative * factor)
        _ -> convertForCharacterization cfg flowUnit cfUnit qty
  where
    -- Bring the inventory quantity into the unit the density is denominated
    -- against: identical units need no table entry, otherwise a same-dimension
    -- conversion. An incompatible or unknown unit yields 'Nothing' → 0.
    toNativeQty fu nu
        | normalizeUnit fu == normalizeUnit nu = Just qty
        | otherwise = convertUnit cfg fu nu qty

{- | Apply the flow→CF unit conversion factor and multiply by the CF value.

Delegates to 'energyAwareConversion', which falls back to
'convertForCharacterization' unless the energy-density bridge applies — so a
dimensional mismatch between flow and CF units lands an effective @0@ (refuse
to score wrong-dimension data) rather than silently passing the unconverted
quantity through. Pass @qty = 1.0@ to obtain the effective-CF factor used at
build time; pass an actual quantity for inline scoring.
-}
convertAndMultiply ::
    UnitConfig ->
    UnitDB ->
    -- | Normalized-flow-name → energy density, for the energy-CF bridge.
    EnergyDensityMap ->
    {- | Pre-resolved flow if the caller already has it; @Nothing@ defaults to
    the identity factor (no flow record means no flow unit known).
    -}
    Maybe BiosphereFlow ->
    -- | (CF value, CF unit)
    (Double, Text) ->
    Double ->
    Double
convertAndMultiply unitConfig unitDB energyDensities mflow (cfVal, cfUnit) qty =
    let flowUnit = maybe "" unitName (mflow >>= \f -> M.lookup (bfUnitId f) unitDB)
        -- Density from the curated CSV first; else parse it from the flow name
        -- ("Coal, 18 MJ per kg" → 18 MJ), so energy-resource variants the CSV
        -- doesn't enumerate still convert mass/volume → energy.
        mDensity =
            mflow >>= \f ->
                M.lookup (normalizeName (bfName f)) energyDensities
                    <|> fmap snd (parseEnergyDensitySuffix (bfName f))
        converted = energyAwareConversion unitConfig flowUnit cfUnit mDensity qty
     in converted * cfVal

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
    BioFlowDB ->
    Inventory ->
    MethodTables ->
    ([(BiosphereFlow, Double, Double)], [UUID])
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
                    let !contribution = convertAndMultiply unitConfig unitDB (mtEnergyDensities tables) (Just flow) cfTuple qty
                     in ((flow, cfVal, contribution) : contribs, unknowns)

{- | Per-process LCIA contributions for one DB + one method, driven by
'MethodTables' + a merged 'BioFlowDB'. Mirrors
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
    BioFlowDB ->
    Database ->
    Vector ->
    MethodTables ->
    M.Map ProcessId Double
processContributionsFromTables unitConfig unitDB flowDB db scalingVec tables =
    U.foldl' step M.empty (dbBiosphereTriples db)
  where
    actIdx = dbActivityIndex db
    bioFlows = dbBiosphereOrder db
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
                    Just cfTuple -> convertAndMultiply unitConfig unitDB (mtEnergyDensities tables) mflow cfTuple 1.0

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

The set is partitioned at build time: non-regional methods land in
'msBatched' (one shared dense broadcast matrix, scored via a single
matvec); regional methods land in 'msRegional' (per-method dispatch through
'computeLCIAScoreAuto', which after the PR #39 precompute is itself a tight
dot product). Mixing the two regimes was previously a hard either/or — a
single regional method in a set of 17 would force all 17 down the slow
per-method walk. The partition recovers the batched matvec for the
non-regional half of mixed sets like EF 3.1 (4 regional + 13 non-regional).
-}
data MethodSetTables = MethodSetTables
    { msAllMethods :: !(V.Vector MethodSetEntry)
    {- ^ Every method in the set, in canonical (sorted) order. Used to
    emit results in the caller's order regardless of partition.
    -}
    , msBatched :: !BatchedTables
    -- ^ Non-regional half. May be empty (@btNMethods == 0@) for all-regional sets.
    , msRegional :: !(V.Vector MethodSetEntry)
    -- ^ Regional half. May be empty for all-non-regional sets (the PR #29 case).
    }

{- | Matvec data for the non-regional subset of a 'MethodSetTables'.

Cell @(method i, flow j)@ lives at @btMat[j * btNMethods + i]@: all CFs for
a single flow row are contiguous so the sparse-inventory scoring loop reads
one cache line per non-zero inventory entry.
-}
data BatchedTables = BatchedTables
    { btMethods :: !(V.Vector MethodSetEntry)
    -- ^ Non-regional methods, in caller-given order.
    , btUuidIndex :: !(M.Map UUID Int)
    -- ^ Flow UUID → row in 'btMat'.
    , btNFlows :: !Int
    -- ^ @M.size btUuidIndex@. Cached for the matvec inner loop.
    , btNMethods :: !Int
    -- ^ @V.length btMethods@. Cached for the matvec inner loop.
    , btMat :: !(U.Vector Double)
    {- ^ Column-major dense broadcast, length @btNFlows * btNMethods@. Empty
    when @btNMethods == 0@.
    -}
    }

{- | Build 'MethodSetTables' from per-method 'MethodTables'. The list order
is preserved in 'msAllMethods' and drives the order results come back in;
the same order, restricted to non-regional methods, is also the column
order in 'btMat'. Callers should sort by 'methodId' for cache-key
canonicality.
-}
buildMethodSetTables :: [(Method, MethodTables)] -> MethodSetTables
buildMethodSetTables pairs =
    let entries = V.fromList [MethodSetEntry (methodId m) m t | (m, t) <- pairs]
        isRegional = not . M.null . mtRegionalizedCF . mseTables
        (regional, batched) = V.partition isRegional entries
     in MethodSetTables
            { msAllMethods = entries
            , msBatched = buildBatchedTables batched
            , msRegional = regional
            }

{- | Build the broadcast-matrix payload for the non-regional subset of a
method set. Returns an empty 'BatchedTables' (zero-length matrix, zero
methods) when the subset is empty, so the scoring path can skip it
without a special case.

Column-major layout: @btMat[j * nMethods + i]@ holds method @i@'s effective
CF for flow row @j@. All @nMethods@ CFs for a single flow are contiguous so
the sparse-inventory scoring loop walks one cache line per non-zero entry.
-}
buildBatchedTables :: V.Vector MethodSetEntry -> BatchedTables
buildBatchedTables entries =
    let nMethods = V.length entries
        uuidSet =
            Set.unions
                [Set.fromList (M.keys (mtBroadcast (mseTables e))) | e <- V.toList entries]
        uuidList = Set.toAscList uuidSet
        uuidIndex = M.fromList (zip uuidList [0 ..])
        nFlows = length uuidList
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
     in BatchedTables
            { btMethods = entries
            , btUuidIndex = uuidIndex
            , btNFlows = nFlows
            , btNMethods = nMethods
            , btMat = mat
            }

{- | Score an inventory against every method in a 'MethodSetTables', emitting
@(methodId, Right score)@ on success and @(methodId, Left err)@ for a
regional method whose coverage is incomplete (mirrors 'computeLCIAScoreAuto').

The two halves of the set are scored by separate code paths and merged by
methodId so the result list follows the input order ('msAllMethods'):

  * non-regional half ('msBatched' on root): one matvec over the shared
    dense broadcast — the PR #29 fast path, restored for mixed sets like
    EF 3.1. Reads the merged cross-DB inventory, so dep-DB flows are
    characterized without any per-DB bookkeeping.
  * regional half ('msRegional' on root): per-method cross-DB sum
    ('sumRegionalizedLCIAScoreCrossDB'). For each regional method, the
    score is @Σ_d rawWeights_d · scaling_d@ across every participating
    database — root + each dep DB reached at request time. Closes the
    gap where dep-DB regional CFs were previously invisible.

Callers pass the per-DB triples as a 'NonEmpty' with the ROOT triple at
'NE.head'. The non-regional matvec is keyed off the root entry's
'msBatched'; the regional cross-DB sum walks every entry. The 'NonEmpty'
constraint makes the impossible state (no DBs participated) unrepresentable.
-}
computeLCIAScoreSetFromTables ::
    UnitConfig ->
    UnitDB ->
    BioFlowDB ->
    Inventory ->
    M.Map Text [Text] ->
    {- | Non-empty per-DB triples: 'NE.head' is root, tail is each participating
    dep DB. Building order matches 'SharedSolver.csScalings'.
    -}
    NonEmpty (Database, Vector, MethodSetTables) ->
    [(UUID, Either Text Double)]
computeLCIAScoreSetFromTables unitCfg unitDB flowDB inventory hier perDb =
    let (_, _, mstRoot) = NE.head perDb
        batched = scoreBatched unitCfg unitDB flowDB (msBatched mstRoot) inventory
        regional = scoreRegionalCrossDB unitCfg unitDB flowDB hier (msRegional mstRoot) perDb
        byId = M.fromList (batched ++ regional)
     in [ (mseMethodId e, byId M.! mseMethodId e)
        | e <- V.toList (msAllMethods mstRoot)
        ]

{- | Per-method cross-DB regional sum. For each regional method, scores
@Σ_d rawWeights_d · scaling_d@ across all participating DBs by looking
up that method's tables in each DB's 'MethodSetTables' and summing the
per-DB dot products via 'sumRegionalizedLCIAScoreCrossDB'.

A DB whose 'MethodSetTables' has no entry for a given methodId (mismatched
sets) contributes 0 for that method — the same neutral element as a DB
whose mappings caught none of the method's regional CFs.
-}
scoreRegionalCrossDB ::
    UnitConfig ->
    UnitDB ->
    BioFlowDB ->
    M.Map Text [Text] ->
    V.Vector MethodSetEntry ->
    NonEmpty (Database, Vector, MethodSetTables) ->
    [(UUID, Either Text Double)]
scoreRegionalCrossDB unitCfg unitDB flowDB hier ms perDb =
    [ ( mseMethodId e
      , sumRegionalizedLCIAScoreCrossDB unitCfg unitDB flowDB hier (triples (mseMethodId e))
      )
    | e <- V.toList ms
    ]
  where
    -- Per-method per-DB triples; skip DBs that don't carry this method.
    triples mid =
        [ (db, sv, t)
        | (db, sv, mst) <- NE.toList perDb
        , Just t <- [lookupMethodTables mid mst]
        ]
    lookupMethodTables mid mst =
        mseTables
            <$> V.find ((== mid) . mseMethodId) (msAllMethods mst)

{- | One sparse-inventory matvec over the non-regional half of a method set.
For each non-zero @(uuid, qty)@ in the inventory, accumulates
@qty * btMat[j*nMethods + i]@ into score @i@ for every method @i@, where
@j@ is the flow's row. The inner loop walks contiguous CF cells (column-
major layout) so the cost is @nnz × nMethods@ FMAs with cache-friendly
reads. An out-of-broadcast UUID falls back to each non-regional method's
'lookupCascadeCF' (same fix as the mono-method 'fastScore') so cross-DB
merged inventories don't silently lose flows that the per-method path
would have caught via name/compartment cascade.
-}
scoreBatched ::
    UnitConfig ->
    UnitDB ->
    BioFlowDB ->
    BatchedTables ->
    Inventory ->
    [(UUID, Either Text Double)]
scoreBatched unitCfg unitDB flowDB bt inventory
    | btNMethods bt == 0 = []
    | otherwise =
        let nMethods = btNMethods bt
            mat = btMat bt
            uuidIdx = btUuidIndex bt
            entriesV = btMethods bt
            cascadeContrib !tables !uuid !qty =
                case lookupCascadeCF tables flowDB uuid of
                    Nothing -> 0
                    Just cfTuple ->
                        convertAndMultiply unitCfg unitDB (mtEnergyDensities tables) (M.lookup uuid flowDB) cfTuple qty
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
            | (i, e) <- zip [0 ..] (V.toList entriesV)
            ]

-- ──────────────────────────────────────────────
-- Post-scoring suggester
-- ──────────────────────────────────────────────

{- | Top-level CF lookup helper used by the suggester. Delegates to
'lookupCascadeCF' (with the caller's already-resolved flow as a singleton
DB) so the suggester sees exactly what scoring sees — including the CAS
bridge — and 'findUncharacterized' never flags a flow the score path
characterizes. Cold path; the singleton allocation is irrelevant here.
-}
lookupCFForFlow :: MethodTables -> UUID -> Maybe BiosphereFlow -> Maybe (Double, Text)
lookupCFForFlow tables fid mFlow =
    lookupCascadeCF tables (maybe M.empty (M.singleton fid) mFlow) fid

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
findSimilarCFs :: ChemSynonyms -> MethodIndex -> BiosphereFlow -> Int -> [SimilarCF]
findSimilarCFs syns idx flow maxN
    | maxN <= 0 = []
    | otherwise =
        let flowName' = bfName flow
            flowCAS' = bfCAS flow
            flowMedium = normalizeMedium . T.takeWhile (/= '/') . T.toLower $ VT.bfCompartmentName flow

            flowRawTokens = S.fromList (T.words (normalizeName flowName'))
            flowExpTokens = expandedTokens syns flowName'

            -- Same-medium candidates (cheap scan); fall back to whole index
            -- only when we have no medium info to filter by.
            mediumIdxs = fromMaybe [0 .. V.length (miCFs idx) - 1] (M.lookup flowMedium (miByMedium idx))

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
    BioFlowDB ->
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
                { ucfFlowId = bfId flow
                , ucfFlowName = bfName flow
                , ucfCategory = VT.bfCompartmentName flow
                , ucfSubcomp = VT.bfCompartmentSub flow
                , ucfFlowUnit = flowUnitText flow
                , ucfQuantity = qty
                , ucfAbsWeight = w
                , ucfSimilarCFs = findSimilarCFs syns idx flow (uoMaxSimilar opts)
                }
            | (flow, qty, w) <- ranked
            ]
  where
    !totalAbs = M.foldr (\q s -> s + abs q) 0 inventory
    flowUnitText flow = maybe "" unitName (M.lookup (bfUnitId flow) unitDB)

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
    BioFlowDB ->
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
