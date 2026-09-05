{-# LANGUAGE OverloadedStrings #-}

module ManagerBuiltinSpec (spec) where

import Config (defaultConfig)
import Control.Concurrent.STM (readTVarIO)
import qualified Data.Map.Strict as M
import Database.Manager (CachePolicy (..), DatabaseManager (..), initDatabaseManager)
import Test.Hspec

spec :: Spec
spec = describe "initDatabaseManager with the built-in defaults" $
    it "loads the four built-in tables and the geographies without a file in sight" $ do
        manager <- initDatabaseManager defaultConfig NoCache
        comps <- readTVarIO (dmLoadedCompMaps manager)
        units <- readTVarIO (dmLoadedUnitDefs manager)
        syns <- readTVarIO (dmLoadedFlowSyns manager)
        eds <- readTVarIO (dmLoadedEnergyDensities manager)
        -- Membership, not the exact key set: startup also picks up whatever
        -- sits under uploads/<kind>/ in the working directory.
        comps `shouldSatisfy` M.member "Default compartment mapping"
        units `shouldSatisfy` M.member "Default units"
        syns `shouldSatisfy` M.member "Default flow synonyms"
        eds `shouldSatisfy` M.member "Default energy densities"
        M.size (dmGeographies manager) `shouldSatisfy` (> 500)
