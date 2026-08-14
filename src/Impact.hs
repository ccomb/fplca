{-# LANGUAGE OverloadedStrings #-}

{- | Scoring one method against one solved inventory.

The step every surface needs and none of them should own. A method carrying
regional characterization factors is scored from the per-database scaling
vectors rather than from the merged inventory, because a factor that depends
on where a flow occurs cannot be applied to a total that has forgotten. Which
of the two paths a method takes is a property of the method, not of who is
asking — so the choice lives here, and the REST routes and the assistant tools
both come through.
-}
module Impact (
    scoreSolution,
) where

import Control.Exception (evaluate)
import Control.Monad (forM)
import Data.Bifunctor (first)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Text (Text)

import Database.Manager (DatabaseManager (..), getMergedFlowMetadata, getMergedUnitConfig, mapMethodToTablesCached)
import Matrix (Inventory)
import Method.Mapping (
    LCIAOutcome (..),
    MethodTables (..),
    computeLCIAScoreFromTables,
    sumRegionalizedLCIAScoreCrossDB,
 )
import Method.Types (Method (..))
import qualified SharedSolver

{- | The score of one method against a cross-database solution.

@inventory@ is passed separately from @sol@ because a caller may have filtered
it (long-term emissions) after solving; the regionalized path reads the
solution's scaling vectors instead and is unaffected by that filtering, as it
was before this was shared.

A 'Left' is a scoring integrity error — a regionalized method with a gap it
cannot fill. It propagates rather than collapsing to a zero the consumer could
not tell from a real score.
-}
scoreSolution ::
    DatabaseManager ->
    -- | Method collection name, to reach each dependency database's tables
    Text ->
    Method ->
    MethodTables ->
    SharedSolver.CrossDBSolution ->
    -- | The inventory to score, after any filtering the caller applied
    Inventory ->
    IO (Either Text Double)
scoreSolution dbManager collection method tables sol inventory = do
    unitCfg <- getMergedUnitConfig dbManager
    (mFlows, mUnits) <- getMergedFlowMetadata dbManager
    fmap (first (("[LCIA " <> methodName method <> "] ") <>)) $
        if M.null (mtRegionalizedCF tables)
            then Right <$> evaluate (loScore (computeLCIAScoreFromTables unitCfg mUnits mFlows inventory tables))
            else do
                let hier = dmLocationHierarchy dbManager
                perDb <-
                    forM (NE.toList (SharedSolver.csScalings sol)) $ \(n, d, sv) -> do
                        tbls <- mapMethodToTablesCached dbManager n collection d method
                        pure (d, sv, tbls)
                traverse evaluate (sumRegionalizedLCIAScoreCrossDB unitCfg mUnits mFlows hier perDb)
