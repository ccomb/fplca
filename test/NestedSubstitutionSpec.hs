{-# LANGUAGE OverloadedStrings #-}

-- | Per-level substitution (Phase A) tests.
--
-- Verifies the new invariant that 'applySubstitutionsAt' filters subs by
-- consumer DB, and that 'goWithSubsAndDeps' matches the baseline cross-DB
-- path when no subs apply at a given level. The single-DB test fixture
-- limits algebraic verification for true multi-level chains; those cases
-- rely on the shared classifier body already covered by
-- 'SubstitutionSpec' and 'CrossDBSubstitutionSpec'.
module NestedSubstitutionSpec (spec) where

import Test.Hspec
import TestHelpers (loadSampleDatabase)
import Types
import Service
    ( ServiceError(..)
    , applySubstitutionsAt
    , inventoryWithSubsAndDeps
    )
import SharedSolver
    ( SharedSolver
    , createSharedSolver
    , computeInventoryMatrixWithDepsCached
    , solveWithSharedSolver
    )
import API.Types (Substitution(..))
import Matrix (buildDemandVectorFromIndex)
import UnitConversion (defaultUnitConfig)
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as U
import qualified Data.Text as T

spec :: Spec
spec = do
    describe "applySubstitutionsAt consumer-DB filter" $ do

        it "skips subs whose consumer is qualified to another DB" $ do
            -- Regression gate: the per-level filter must not touch subs that
            -- belong to a different level. This is the mechanism that makes
            -- nested substitutions work — applying a sub at the wrong level
            -- would be silent miscounting.
            db <- loadSampleDatabase "SAMPLE.min3"
            solver <- mkSolver db "root"
            let pid = 0
                demandVec = buildDemandVectorFromIndex (dbActivityIndex db) pid
            baselineX <- solveWithSharedSolver solver demandVec
            let qualifiedPid = "other-db::aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa_bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb"
                sub = Substitution
                    { subFrom     = qualifiedPid
                    , subTo       = qualifiedPid
                    , subConsumer = qualifiedPid
                    }
                noDeps _ = pure Nothing
            res <- applySubstitutionsAt noDeps db "root" solver [baselineX] [sub]
            case res of
                Right ([x'], links) -> do
                    links `shouldBe` []
                    U.toList x' `shouldBe` U.toList baselineX
                Right other -> expectationFailure ("unexpected shape: " <> show other)
                Left e      -> expectationFailure ("filter should skip, got: " <> show e)

        it "empty-subs fast path returns scalings unchanged with no virtual links" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            solver <- mkSolver db "root"
            let pid = 0
                demandVec = buildDemandVectorFromIndex (dbActivityIndex db) pid
            baselineX <- solveWithSharedSolver solver demandVec
            let noDeps _ = pure Nothing
            res <- applySubstitutionsAt noDeps db "root" solver [baselineX] []
            case res of
                Right (xs, links) -> do
                    links `shouldBe` []
                    length xs `shouldBe` 1
                    U.toList (head xs) `shouldBe` U.toList baselineX
                Left e -> expectationFailure ("empty path failed: " <> show e)

        it "preserves K>1 input shape when all subs are filtered" $ do
            -- At dep level K = number of root demands. The filter must not
            -- collapse the batch.
            db <- loadSampleDatabase "SAMPLE.min3"
            solver <- mkSolver db "dep"
            let pid = 0
                demandVec = buildDemandVectorFromIndex (dbActivityIndex db) pid
            baselineX <- solveWithSharedSolver solver demandVec
            let qualifiedToRoot = "root::aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa_bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb"
                sub = Substitution
                    { subFrom     = qualifiedToRoot
                    , subTo       = qualifiedToRoot
                    , subConsumer = qualifiedToRoot
                    }
                noDeps _ = pure Nothing
            res <- applySubstitutionsAt noDeps db "dep" solver [baselineX, baselineX, baselineX] [sub]
            case res of
                Right (xs, _) -> length xs `shouldBe` 3
                Left e        -> expectationFailure ("K>1 filter failed: " <> show e)

    describe "inventoryWithSubsAndDeps (Phase A recursion)" $ do

        it "with empty subs matches the baseline cross-DB inventory path" $ do
            -- Regression gate for the commit that replaced the old one-shot
            -- root-path body with the recursive goWithSubsAndDeps. Bit-for-bit
            -- parity against computeInventoryMatrixWithDepsCached.
            db <- loadSampleDatabase "SAMPLE.min3"
            solver <- mkSolver db "SAMPLE.min3"
            let pid = 0
                noDeps _ = pure Nothing
            eBase <- computeInventoryMatrixWithDepsCached defaultUnitConfig noDeps db solver pid
            eSub  <- inventoryWithSubsAndDeps defaultUnitConfig noDeps db "SAMPLE.min3" solver pid []
            case (eBase, eSub) of
                (Right base, Right subInv) -> M.toList subInv `shouldBe` M.toList base
                (Left e, _) -> expectationFailure ("baseline failed: " <> T.unpack e)
                (_, Left e) -> expectationFailure ("subs path failed: " <> show e)

        it "rejects dep-consumer subs whose DB is not loaded (422 surface)" $ do
            -- No-silent-errors invariant: a sub with a consumer qualified to
            -- an unloaded DB used to be silently filtered at every level.
            -- 'validateConsumerDbs' now surfaces it before the recursion.
            db <- loadSampleDatabase "SAMPLE.min3"
            solver <- mkSolver db "SAMPLE.min3"
            let pid = 0
                noDeps _ = pure Nothing
                realRootPid = "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa_bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb"
                subWithPhantomConsumer = Substitution
                    { subFrom     = realRootPid
                    , subTo       = realRootPid
                    , subConsumer = "phantom-dep::" <> realRootPid
                    }
            eFiltered <- inventoryWithSubsAndDeps defaultUnitConfig noDeps db "SAMPLE.min3" solver pid [subWithPhantomConsumer]
            case eFiltered of
                Left (MatrixError msg) ->
                    msg `shouldSatisfy` T.isInfixOf "unloaded database"
                Left other -> expectationFailure ("wrong error: " <> show other)
                Right _    -> expectationFailure "expected Left for unloaded consumer DB"

-- | Build a fresh 'SharedSolver' from a database's technosphere triples.
mkSolver :: Database -> T.Text -> IO SharedSolver
mkSolver db name =
    let tech = [ (fromIntegral i, fromIntegral j, v)
               | SparseTriple i j v <- U.toList (dbTechnosphereTriples db) ]
        n    = fromIntegral (dbActivityCount db)
    in createSharedSolver name tech n
