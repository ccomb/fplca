{-# LANGUAGE OverloadedStrings #-}

{- | An index built with 'M.fromList' asserts that its key determines its value.
Where that assertion is false the extra rows vanish with no error and every
reader answers with an arbitrary one of them. These examples hold the product
index to the rows the database actually contains.
-}
module IndexInjectivitySpec (spec) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Vector as V
import Test.Hspec
import TestHelpers (loadSampleDatabase)
import Types

{- | The rows whose reference product is a technosphere flow, which is exactly
what 'Database.buildProductIndex' indexes.
-}
producingRows :: Database -> [(ProcessId, UUID)]
producingRows db =
    [ (pid, prodUUID)
    | (pid, (_, prodUUID)) <- zip [0 ..] (V.toList (dbProcessIdTable db))
    , M.member prodUUID (dbTechFlows db)
    ]

spec :: Spec
spec = describe "product index" $ do
    -- SAMPLE.ilcd holds two activities producing one and the same product flow,
    -- the ordinary shape of a product made in more than one geography.
    it "lists every row producing a product flow" $ do
        db <- loadSampleDatabase "SAMPLE.ilcd"
        let listed = sum (map NE.length (M.elems (piByUUID (dbProductIndex db))))
        listed `shouldBe` length (producingRows db)

    it "names no supplier when several rows produce the flow" $ do
        db <- loadSampleDatabase "SAMPLE.ilcd"
        let shared = M.keys (M.filter ((> 1) . NE.length) (piByUUID (dbProductIndex db)))
        case shared of
            [] -> expectationFailure "fixture no longer has a product flow with several producers"
            (flowUUID : _) -> findProcessIdByProductFlow db flowUUID `shouldBe` Nothing

    it "names the supplier when one row produces the flow" $ do
        db <- loadSampleDatabase "SAMPLE.min"
        let named = [findProcessIdByProductFlow db f | (_, f) <- producingRows db]
        named `shouldBe` [Just pid | (pid, _) <- producingRows db]
