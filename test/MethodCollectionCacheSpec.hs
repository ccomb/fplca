{-# LANGUAGE OverloadedStrings #-}

{- | Regression test for the collection-scoped per-method CF caches.

A method's engine UUID is a UUIDv5 of the method NAME only, so a method with
the same name in two different LCIA collections (e.g. "Environmental Footprint
3.1 (adapted) 1.0" vs "… 1.03") collides on UUID while carrying *different* CF
lists. The per-method lazy caches in 'DatabaseManager' must therefore key on
(dbName, collection, methodId): if the collection is dropped from the key, the
first collection to score a (db, UUID) populates the cache and the second
silently reuses the wrong CF table.

This test drives 'mapMethodToTablesCached' directly with one in-memory database
and two same-UUID methods that differ only in their UUID-matched CF value. It
asserts the built 'MethodTables' (specifically 'mtUuidCF') differ per collection
— which can only hold when the collection is part of the cache key. Without the
key change, the second lookup returns the first collection's cached tables and
the assertion fails.
-}
module MethodCollectionCacheSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import Test.Hspec

import Config (defaultConfig)
import qualified Database.Manager as DM
import Method.Mapping (CF (..), CFUnit (..), mtUuidCF, teCF)
import Method.Types (FlowDirection (..), Method (..), MethodCF (..))
import Types (UUID)

import CrossDBRegionalLCIAFixture (flowUUID, mkDB, mkUUID)

{- | Shared method UUID — same in both collections, mirroring the UUIDv5-of-name
collision the cache must disambiguate by collection.
-}
sharedMethodId :: UUID
sharedMethodId = mkUUID 4242

{- | A method named "Resource use, fossils" carrying a single UUID-matched CF on
the fixture's biosphere flow, with the given CF value.
-}
mkFossilsMethod :: Double -> Method
mkFossilsMethod cfValue =
    Method
        { methodId = sharedMethodId
        , methodName = "Resource use, fossils"
        , methodDescription = Nothing
        , methodUnit = "MJ"
        , methodCategory = "Resource use, fossils"
        , methodMethodology = Nothing
        , methodFactors = [fossilsCF cfValue]
        }

-- | A CF that resolves by UUID against the fixture's single biosphere flow.
fossilsCF :: Double -> MethodCF
fossilsCF cfValue =
    MethodCF
        { mcfFlowRef = flowUUID
        , mcfFlowName = "Oil, crude, 43.4 MJ per kg"
        , mcfDirection = Output
        , mcfValue = cfValue
        , mcfCompartment = Nothing
        , mcfCAS = Nothing
        , mcfUnit = "MJ"
        , mcfConsumerLocation = Nothing
        }

collectionA :: Text
collectionA = "Environmental Footprint 3.1 (adapted) 1.0"

collectionB :: Text
collectionB = "Environmental Footprint 3.1 (adapted) 1.03"

{- | CF value distinguishing the two collections (analogous to the missing
"Oil, crude, 43.4 MJ per kg" CF that only the 1.03 collection defines).
-}
cfA, cfB :: Double
cfA = 43.2
cfB = 43.4

spec :: Spec
spec = do
    describe "mapMethodToTablesCached (collection-scoped cache)" $ do
        it "returns per-collection CF tables for same-UUID methods (A then B)" $ do
            mgr <- DM.initDatabaseManager defaultConfig False Nothing
            let db = mkDB 0 ["FR"] []
            tablesA <- DM.mapMethodToTablesCached mgr "db" collectionA db (mkFossilsMethod cfA)
            tablesB <- DM.mapMethodToTablesCached mgr "db" collectionB db (mkFossilsMethod cfB)
            fmap teCF (M.lookup flowUUID (mtUuidCF tablesA)) `shouldBe` Just (CF cfA (CFUnit "MJ"))
            fmap teCF (M.lookup flowUUID (mtUuidCF tablesB)) `shouldBe` Just (CF cfB (CFUnit "MJ"))

        it "returns per-collection CF tables for same-UUID methods (B then A)" $ do
            mgr <- DM.initDatabaseManager defaultConfig False Nothing
            let db = mkDB 0 ["FR"] []
            tablesB <- DM.mapMethodToTablesCached mgr "db" collectionB db (mkFossilsMethod cfB)
            tablesA <- DM.mapMethodToTablesCached mgr "db" collectionA db (mkFossilsMethod cfA)
            fmap teCF (M.lookup flowUUID (mtUuidCF tablesB)) `shouldBe` Just (CF cfB (CFUnit "MJ"))
            fmap teCF (M.lookup flowUUID (mtUuidCF tablesA)) `shouldBe` Just (CF cfA (CFUnit "MJ"))
