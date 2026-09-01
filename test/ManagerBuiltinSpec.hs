{-# LANGUAGE OverloadedStrings #-}

module ManagerBuiltinSpec (spec) where

import Config (defaultConfig)
import Control.Concurrent.STM (readTVarIO)
import qualified Data.Map.Strict as M
import Database.Manager (DatabaseManager (..), initDatabaseManager)
import Test.Hspec

spec :: Spec
spec = describe "initDatabaseManager with the built-in defaults" $
    it "loads the four built-in tables and the geographies without a file in sight" $ do
        manager <- initDatabaseManager defaultConfig True
        comps <- readTVarIO (dmLoadedCompMaps manager)
        units <- readTVarIO (dmLoadedUnitDefs manager)
        syns <- readTVarIO (dmLoadedFlowSyns manager)
        eds <- readTVarIO (dmLoadedEnergyDensities manager)
        M.keys comps `shouldBe` ["Default compartment mapping"]
        M.keys units `shouldBe` ["Default units"]
        M.keys syns `shouldBe` ["Default flow synonyms"]
        M.keys eds `shouldBe` ["Default energy densities"]
        M.size (dmGeographies manager) `shouldSatisfy` (> 500)
