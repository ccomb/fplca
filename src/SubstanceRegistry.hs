{-# LANGUAGE ScopedTypeVariables #-}

{- | Canonical flow registry — foundation.

This module owns the entity-resolution core of the canonical flow registry:
given undirected @SameAs@ assertions between flow identities, it computes the
equivalence classes (connected components / transitive closure) — the "set of
sets" that unifies a substance's many names across nomenclatures (ILCD,
ecoinvent, SimaPro, BAFU).

Identity is currently resolved by name pairs (see "SynonymDB"). The typed,
provenanced edge layer (Subsumes / ProxyFor split-ratios, and CAS/UUID anchor
nodes that fuse a substance's names across sources) builds on top of this
closure primitive — none of which changes the primitive itself.
-}
module SubstanceRegistry (
    -- * Closure primitive
    equivalenceClasses,

    -- * Typed-edge model (the registry's substance layer)
    CASNumber (..),
    FlowUUID (..),
    NormName (..),
    SourceId (..),
    SplitWeight (..),
    ConversionFactor (..),
    ClassId (..),
    SubstanceKey (..),
    Relation (..),
    SubstanceEdge (..),
    ClassResult (..),
    classesFromEdges,
) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import Data.UUID (UUID)

{- | Connected components of the undirected graph whose edges are the given
@SameAs@ pairs, by union-find. Each component is the transitive closure of the
identities reachable through those pairs: @A↔B@ and @B↔C@ ⟹ one class @{A,B,C}@.

Only identities that appear in at least one pair are returned; an isolated
identity has no class here. Order within and across classes is unspecified.

Union always points one root at another /distinct/ root, so the parent forest
stays acyclic and 'root' always terminates.
-}
equivalenceClasses :: forall a. (Ord a) => [(a, a)] -> [[a]]
equivalenceClasses pairs =
    M.elems $ M.fromListWith (++) [(root finalParent x, [x]) | x <- elems]
  where
    elems :: [a]
    elems = S.toList $ S.fromList $ concatMap (\(a, b) -> [a, b]) pairs

    finalParent :: Map a a
    finalParent = foldl' unite (M.fromList [(x, x) | x <- elems]) pairs

    unite :: Map a a -> (a, a) -> Map a a
    unite p (a, b) =
        let ra = root p a
            rb = root p b
         in if ra == rb then p else M.insert ra rb p

    root :: Map a a -> a -> a
    root p x = case M.lookup x p of
        Just px
            | px == x -> x
            | otherwise -> root p px
        Nothing -> x

-- | A CAS registry number (e.g. @50-00-0@). A global substance anchor.
newtype CASNumber = CASNumber Text
    deriving (Eq, Ord, Show)

-- | An elementary-flow UUID. Globally unique, so also a global anchor.
newtype FlowUUID = FlowUUID UUID
    deriving (Eq, Ord, Show)

{- | A normalized flow name (the output of @SynonymDB.normalizeName@). A bare
name is only meaningful within the source that uses it, hence 'ByName' scopes it
by 'SourceId'.
-}
newtype NormName = NormName Text
    deriving (Eq, Ord, Show)

-- | A flow-definition source/version, e.g. @ecoinvent-3.11@, @ef-3.1-jrc@.
newtype SourceId = SourceId Text
    deriving (Eq, Ord, Show)

-- | A derived class identity (a representative member is chosen for display).
newtype ClassId = ClassId Int
    deriving (Eq, Ord, Show)

{- | How a source identifies a substance occurrence. CAS and UUID are /global/
anchors — the same value from any source is the same node, which is exactly what
makes the CAS bridge fall out of closure for free (two flows sharing a CAS share
the node, no edge needed). A bare name collides across sources, so 'ByName' is
scoped by its 'SourceId'.
-}
data SubstanceKey
    = ByCAS !CASNumber
    | ByUUID !FlowUUID
    | ByName !SourceId !NormName
    deriving (Eq, Ord, Show)

{- | A narrower flow's share of a broader one in a 'Subsumes' fan-out; the
siblings under one parent are expected to sum to ~1.
-}
newtype SplitWeight = SplitWeight Double
    deriving (Eq, Show)

-- | A unit/scale factor carried by a 'ProxyFor' stand-in.
newtype ConversionFactor = ConversionFactor Double
    deriving (Eq, Show)

{- | A typed relation between two substance anchors. Only 'SameAs' forms
equivalence classes; 'Subsumes' (broader ⊃ narrower, weighted) and 'ProxyFor'
(approximate, scaled) are a separate directional layer consumed at score time;
'DistinctFrom' is recorded negative evidence that blocks accidental closure.

The weight lives inside 'Subsumes' and the factor inside 'ProxyFor', so a
'SameAs' can never carry a scale — that would contradict "identical".
-}
data Relation
    = SameAs
    | Subsumes !SplitWeight
    | ProxyFor !ConversionFactor
    | DistinctFrom
    deriving (Eq, Show)

-- | One typed assertion relating two substance anchors.
data SubstanceEdge = SubstanceEdge
    { seFrom :: !SubstanceKey
    , seTo :: !SubstanceKey
    , seRelation :: !Relation
    }
    deriving (Eq, Show)

{- | Resolving a set of edges yields the @SameAs@-closure classes plus any
'DistinctFrom' the closure /violated/ (two keys declared distinct that some
@SameAs@ chain nonetheless merged). A contradicting edge set is a data bug to
fix, so conflicts are surfaced here, never silently resolved.
-}
data ClassResult = ClassResult
    { crClasses :: ![[SubstanceKey]]
    , crConflicts :: ![(SubstanceKey, SubstanceKey)]
    }
    deriving (Eq, Show)

{- | Resolve typed edges into substance classes. Classes are the transitive
closure of the @SameAs@ edges /only/; 'Subsumes' and 'ProxyFor' connect distinct
classes and are ignored here (a later score-time layer applies them). Each
'DistinctFrom' whose endpoints the @SameAs@ closure placed in one class is
reported in 'crConflicts'.
-}
classesFromEdges :: [SubstanceEdge] -> ClassResult
classesFromEdges edges = ClassResult classes conflicts
  where
    classes = equivalenceClasses [(seFrom e, seTo e) | e <- edges, seRelation e == SameAs]
    classOf = M.fromList [(k, i) | (i, ks) <- zip [0 :: Int ..] classes, k <- ks]
    conflicts =
        [ (a, b)
        | e <- edges
        , seRelation e == DistinctFrom
        , let a = seFrom e
        , let b = seTo e
        , Just ia <- [M.lookup a classOf]
        , Just ib <- [M.lookup b classOf]
        , ia == ib
        ]
