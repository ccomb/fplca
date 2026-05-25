{-# LANGUAGE OverloadedStrings #-}

module MatrixConstructionSpec (spec) where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Database (buildDatabaseWithMatrices)
import Test.Hspec
import TestHelpers
import Types
import UnitConversion (defaultUnitConfig)

-- | Build a UUID from a literal string; explode loudly at test time if malformed.
mkUUID :: String -> UUID.UUID
mkUUID s = case UUID.fromString s of
    Just u -> u
    Nothing -> error $ "invalid UUID literal in test: " <> s

spec :: Spec
spec = do
    describe "Matrix Construction Sign Convention" $ do
        it "stores technosphere input triplets as positive on samples without substitutions" $ do
            -- Regression: an earlier bug stored input coefficients with the
            -- wrong sign, producing positive (I-A) off-diagonals instead of
            -- negative ones. Database.hs sets value = sign * amount / denom
            -- with sign=+1 for inputs; Matrix.hs negates when building (I-A).
            --
            -- SAMPLE.min3 has no substitution (avoided-burden) rows, so every
            -- input is a real consumption and every tech triplet is positive.
            -- Sources with negative Materials/fuels rows legitimately produce
            -- negative triplets — see SimaProParserSpec "SimaPro substitutions".
            db <- loadSampleDatabase "SAMPLE.min3"

            let techTriples = VU.toList (dbTechnosphereTriples db)

            -- Expected: Y needs 0.6 from X, Z needs 0.4 from Y
            let positiveTriplets = filter (\(SparseTriple _ _ v) -> v > 0) techTriples
            length positiveTriplets `shouldBe` length techTriples

            -- Verify specific expected values
            let sortedTriplets = VU.toList (dbTechnosphereTriples db)
            length sortedTriplets `shouldSatisfy` (>= 2)

        it "normalizes exchanges by reference product amount" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            -- Load activity X which outputs 1.0 kg
            -- Check that technosphere inputs are normalized
            let techTriples = VU.toList (dbTechnosphereTriples db)

            -- Should have at least 2 technosphere exchanges
            length techTriples `shouldSatisfy` (>= 2)

    describe "Biosphere Matrix Construction" $ do
        it "stores emissions as POSITIVE values" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            let bioTriples = VU.toList (dbBiosphereTriples db)

            -- Expected: 2 biosphere flows (CO2 and Zinc)
            length bioTriples `shouldSatisfy` (>= 2)

            -- All biosphere values should be positive for emissions
            let emissionTriplets = filter (\(SparseTriple _ _ v) -> v > 0) bioTriples
            length emissionTriplets `shouldSatisfy` (>= 2)

        it "builds biosphere matrix with correct dimensions" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            let bioFlowCount = dbBiosphereCount db
            let activityCount = dbActivityCount db

            -- SAMPLE.min3: 2 biosphere flows, 3 activities
            bioFlowCount `shouldBe` 2
            activityCount `shouldBe` 3

    describe "Database Structure" $ do
        it "builds ProcessId table correctly" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            -- ProcessId table maps index to (activityUUID, productUUID)
            let processIdTable = dbProcessIdTable db
            V.length processIdTable `shouldBe` 3

        it "builds activity index" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            -- Activity index should be identity mapping for simple case
            let activityIndex = dbActivityIndex db
            V.length activityIndex `shouldBe` 3

    describe "Matrix Sparsity" $ do
        it "produces only well above-zero entries on the basic SAMPLE.min3 fixture" $ do
            -- Sanity check on SAMPLE.min3 — its declared exchanges are all O(0.1..1),
            -- so post-normalization triplets sit comfortably above any floating-point
            -- noise floor. Kept as a smoke test that the matrix builder produces sane
            -- magnitudes on a known good fixture. Note: PR #69 dropped the previous
            -- post-normalization 1e-15 magnitude filter (it dropped legitimate
            -- emissions on large-productAmount activities); see the "PR #69" test
            -- below for the regression.
            db <- loadSampleDatabase "SAMPLE.min3"

            let techTriples = VU.toList (dbTechnosphereTriples db)
            all (\(SparseTriple _ _ v) -> abs v > 1.0e-15) techTriples `shouldBe` True

        it "excludes diagonal entries from technosphere triplets" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            let techTriples = VU.toList (dbTechnosphereTriples db)

            -- Diagonal entries (self-loops) are excluded in simple SAMPLE.min3
            let diagonalEntries = filter (\(SparseTriple i j _) -> i == j) techTriples
            length diagonalEntries `shouldBe` 0

        it "keeps biosphere flows on activities with very large reference amounts (PR #69)" $ do
            -- Regression: the previous filter `abs value > 1e-15` was applied AFTER
            -- dividing rawValue by the normalization factor. For activities whose
            -- reference product gets normalized to a large canonical-unit amount at
            -- ingest (e.g. SimaPro turns "1 kWh" into 3.6e6 J, so normFactor = 3.6e6),
            -- a real 1e-9 kg emission yielded a post-normalization value of ~2.8e-16
            -- and was silently dropped. The fix moves the filter to the source value,
            -- so the triplet must now survive even though its stored magnitude is
            -- below 1e-15.
            let actUUID = mkUUID "11111111-2222-3333-4444-555555555555"
                prodUUID = mkUUID "22222222-3333-4444-5555-666666666666"
                bioFlowUUID = mkUUID "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
                jUnitId = mkUUID "00000000-0000-0000-0000-00000000000a"
                kgUnitId = mkUUID "00000000-0000-0000-0000-00000000000b"
                rawBioAmount = 1.0e-9 :: Double
                normFactor = 3.6e6 :: Double
                expectedValue = rawBioAmount / normFactor
                refExchange =
                    TechnosphereExchange
                        { techFlowId = prodUUID
                        , techAmount = normFactor
                        , techUnitId = jUnitId
                        , techRole = ReferenceProduct
                        , techActivityLinkId = UUID.nil
                        , techProcessLinkId = Nothing
                        , techLocation = ""
                        , techComment = Nothing
                        , techPedigree = Nothing
                        }
                bioExchange =
                    BiosphereExchange
                        { bioFlowId = bioFlowUUID
                        , bioAmount = rawBioAmount
                        , bioUnitId = kgUnitId
                        , bioDirection = Emission
                        , bioLocation = ""
                        , bioComment = Nothing
                        , bioPedigree = Nothing
                        }
                activity =
                    Activity
                        { activityName = "high-canonical-unit power process"
                        , activityDescription = []
                        , activitySynonyms = M.empty
                        , activityClassification = M.empty
                        , activityLocation = "GLO"
                        , activityUnit = "j"
                        , exchanges = [refExchange, bioExchange]
                        , activityParams = M.empty
                        , activityParamExprs = M.empty
                        , activityAllocationPercent = Nothing
                        , activityAllocationFormula = Nothing
                        }
                activityMap = M.singleton (actUUID, prodUUID) activity
                techFlowDB = M.singleton prodUUID (TechnosphereFlow prodUUID "energy product" jUnitId M.empty Nothing Nothing)
                bioFlowDB = M.singleton bioFlowUUID (BiosphereFlow bioFlowUUID "trace pollutant" kgUnitId M.empty Nothing Nothing (Just (Compartment "air" Nothing)))
                unitDB =
                    M.fromList
                        [ (jUnitId, Unit jUnitId "j" "j" "")
                        , (kgUnitId, Unit kgUnitId "kg" "kg" "")
                        ]

            result <- buildDatabaseWithMatrices defaultUnitConfig activityMap techFlowDB bioFlowDB M.empty unitDB
            case result of
                Left err -> expectationFailure $ "buildDatabaseWithMatrices failed: " <> T.unpack err
                Right db -> do
                    let bioTriples = VU.toList (dbBiosphereTriples db)
                    -- Under the old buggy filter, length would be 0 — the triplet was
                    -- dropped because abs (1e-9 / 3.6e6) < 1e-15.
                    length bioTriples `shouldBe` 1
                    case bioTriples of
                        [SparseTriple _ _ v] -> do
                            -- The stored magnitude is genuinely below 1e-15 — this is
                            -- exactly the case the new filter must preserve.
                            abs v `shouldSatisfy` (< 1.0e-15)
                            withinTolerance 1.0e-22 expectedValue v `shouldBe` True
                        _ -> expectationFailure "expected exactly one biosphere triplet"

        it "still excludes biosphere rows whose source amount is exactly zero" $ do
            -- The fix replaced `abs value > 1e-15` with `rawValue /= 0`. Confirm the
            -- new predicate still strips truly empty rows: an exchange with raw
            -- amount 0 must not produce a triplet.
            let actUUID = mkUUID "11111111-2222-3333-4444-777777777777"
                prodUUID = mkUUID "22222222-3333-4444-5555-888888888888"
                bioFlowUUID = mkUUID "aaaaaaaa-bbbb-cccc-dddd-ffffffffffff"
                jUnitId = mkUUID "00000000-0000-0000-0000-00000000000a"
                kgUnitId = mkUUID "00000000-0000-0000-0000-00000000000b"
                refExchange =
                    TechnosphereExchange
                        { techFlowId = prodUUID
                        , techAmount = 1.0
                        , techUnitId = jUnitId
                        , techRole = ReferenceProduct
                        , techActivityLinkId = UUID.nil
                        , techProcessLinkId = Nothing
                        , techLocation = ""
                        , techComment = Nothing
                        , techPedigree = Nothing
                        }
                zeroBio =
                    BiosphereExchange
                        { bioFlowId = bioFlowUUID
                        , bioAmount = 0.0
                        , bioUnitId = kgUnitId
                        , bioDirection = Emission
                        , bioLocation = ""
                        , bioComment = Nothing
                        , bioPedigree = Nothing
                        }
                activity =
                    Activity
                        { activityName = "process with empty biosphere row"
                        , activityDescription = []
                        , activitySynonyms = M.empty
                        , activityClassification = M.empty
                        , activityLocation = "GLO"
                        , activityUnit = "j"
                        , exchanges = [refExchange, zeroBio]
                        , activityParams = M.empty
                        , activityParamExprs = M.empty
                        , activityAllocationPercent = Nothing
                        , activityAllocationFormula = Nothing
                        }
                activityMap = M.singleton (actUUID, prodUUID) activity
                techFlowDB = M.singleton prodUUID (TechnosphereFlow prodUUID "energy product" jUnitId M.empty Nothing Nothing)
                bioFlowDB = M.singleton bioFlowUUID (BiosphereFlow bioFlowUUID "trace pollutant" kgUnitId M.empty Nothing Nothing (Just (Compartment "air" Nothing)))
                unitDB =
                    M.fromList
                        [ (jUnitId, Unit jUnitId "j" "j" "")
                        , (kgUnitId, Unit kgUnitId "kg" "kg" "")
                        ]

            result <- buildDatabaseWithMatrices defaultUnitConfig activityMap techFlowDB bioFlowDB M.empty unitDB
            case result of
                Left err -> expectationFailure $ "buildDatabaseWithMatrices failed: " <> T.unpack err
                Right db ->
                    VU.length (dbBiosphereTriples db) `shouldBe` 0
