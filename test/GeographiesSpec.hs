{-# LANGUAGE OverloadedStrings #-}

{- | Regression gate for the geography cascade defined in
@data/geographies.csv@. The cascade is the chain of fallback locations
that the LCIA matcher walks when a CF for the requested location is
absent — a missing edge silently demotes a location to the legacy
global behavior.
-}
module GeographiesSpec (spec) where

import qualified Data.Map.Strict as M
import Database.Manager (parseGeographiesCSV)
import Test.Hspec

spec :: Spec
spec = do
    describe "data/geographies.csv cascade" $ do
        it "RoW cascades to GLO (commit 7df7921)" $ do
            -- Before this fix, RoW had an empty parents column, so any
            -- "Rest of World" CF lookup failed instead of falling back to
            -- the global value — silently underestimating impact for every
            -- ecoinvent activity whose location is RoW.
            geos <- parseGeographiesCSV "data/geographies.csv"
            case M.lookup "RoW" geos of
                Just (display, parents) -> do
                    display `shouldBe` "Rest of World"
                    parents `shouldBe` ["GLO"]
                Nothing -> expectationFailure "RoW missing from geographies.csv"

        it "GLO cascades to RoW (existing fallback, regression gate)" $ do
            geos <- parseGeographiesCSV "data/geographies.csv"
            case M.lookup "GLO" geos of
                Just (_, parents) -> parents `shouldBe` ["RoW"]
                Nothing -> expectationFailure "GLO missing from geographies.csv"

        it "every non-leaf entry has at least one parent" $ do
            -- Sanity check that no other row regresses to the empty-parents
            -- state RoW used to have. The only leaves are GLO and RoW
            -- themselves (mutual fallback between the two).
            geos <- parseGeographiesCSV "data/geographies.csv"
            let orphaned =
                    [ code
                    | (code, (_, parents)) <- M.toList geos
                    , null parents
                    ]
            orphaned `shouldBe` []
