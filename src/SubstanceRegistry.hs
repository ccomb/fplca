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
    equivalenceClasses,
) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

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
