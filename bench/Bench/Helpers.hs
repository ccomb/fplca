{-# LANGUAGE OverloadedStrings #-}

{- | Shared helpers used by Loader / Solve / real-data Lcia benches.

Loading a 'Database' takes seconds and produces hundreds of MB of
in-memory state, so all the heavy benches that share a fixture build it
once at registration time and only bench the operation under study.
-}
module Bench.Helpers (
    loadFullDatabase,
) where

import Data.Text (Text)

import Database (buildDatabaseWithMatrices)
import qualified Database.Loader as Loader
import Types (BuildInputs (..), Database, sdbActivities, sdbBioFlows, sdbTechFlows, sdbUnits, sdbWasteFlows)
import qualified UnitConversion as UC

{- | Parse a fixture path and build the indexed 'Database' with matrices.
This is the slow step every Solve / real-LCIA bench shares; the caller is
expected to invoke it once at registration time and reuse the result.

Returns 'Left' with a human-readable message if the parse or matrix build
fails. The bench module is responsible for skipping its specs in that case.
-}
loadFullDatabase :: UC.UnitConfig -> FilePath -> IO (Either Text Database)
loadFullDatabase unitCfg path = do
    res <- Loader.loadDatabase unitCfg path
    case res of
        Left err -> pure (Left ("loadDatabase failed: " <> err))
        Right sdb -> do
            built <-
                buildDatabaseWithMatrices
                    (BuildInputs unitCfg mempty)
                    (sdbActivities sdb)
                    (sdbTechFlows sdb)
                    (sdbBioFlows sdb)
                    (sdbWasteFlows sdb)
                    (sdbUnits sdb)
            case built of
                Left err -> pure (Left ("buildDatabaseWithMatrices failed: " <> err))
                Right db -> pure (Right db)
