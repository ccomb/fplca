{-# LANGUAGE OverloadedStrings #-}

{- | The database cache is bytes, and naming a shape it holds must not move them.

'Database.Loader.schemaSignature' hashes the identity of 'Types.Database' and
nothing inside it, so a layout that shifted under a cache built before the shift
would not be rejected, it would be decoded as the layout it is not. Giving a
tuple a constructor and field names is meant to cost exactly nothing here: a
type with one constructor carries no tag, so it pokes the fields of the tuple it
replaces, in the order it replaced them. This says that out loud, and fails the
day it stops being true.
-}
module CacheLayoutSpec (spec) where

import Data.Store (encode)
import Test.Hspec

import Types (LinkBlocker (..), UnresolvedProduct (..))

spec :: Spec
spec =
    describe "the shapes the database cache holds" $
        it "records an unsupplied product as the pair that record replaced" $
            encode unresolved `shouldBe` encode (upDemands unresolved, upBlocker unresolved)
  where
    unresolved :: UnresolvedProduct
    unresolved = UnresolvedProduct{upDemands = 7, upBlocker = LocationUnavailable "FR"}
