{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec

-- Import test modules (will be created)
import qualified MatrixConstructionSpec
import qualified InventorySpec
import qualified ParserSpec
import qualified MatrixExportSpec

main :: IO ()
main = hspec $ do
    describe "ACV Engine Test Suite" $ do
        describe "Matrix Construction" MatrixConstructionSpec.spec
        describe "Inventory Calculation" InventorySpec.spec
        describe "Parser" ParserSpec.spec
        describe "Matrix Export" MatrixExportSpec.spec
