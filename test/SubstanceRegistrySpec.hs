module SubstanceRegistrySpec (spec) where

import qualified Data.Set as S
import qualified Data.Text as T
import Test.Hspec

import SubstanceRegistry (
    CASNumber (..),
    ClassResult (..),
    NormName (..),
    Relation (..),
    SourceId (..),
    SplitWeight (..),
    SubstanceEdge (..),
    SubstanceKey (..),
    classesFromEdges,
    equivalenceClasses,
 )

-- | Normalize the result to a set of sets, so assertions are order-independent.
classesOf :: (Ord a) => [(a, a)] -> S.Set (S.Set a)
classesOf = S.fromList . map S.fromList . equivalenceClasses

-- Helpers for the typed-edge tests (avoid OverloadedStrings so the String-keyed
-- equivalenceClasses tests keep their defaulting).
nm :: String -> String -> SubstanceKey
nm s n = ByName (SourceId (T.pack s)) (NormName (T.pack n))

cas :: String -> SubstanceKey
cas c = ByCAS (CASNumber (T.pack c))

sameAs :: SubstanceKey -> SubstanceKey -> SubstanceEdge
sameAs a b = SubstanceEdge a b SameAs

spec :: Spec
spec = do
    describe "equivalenceClasses" $ do
        it "takes the transitive closure of chained pairs (A=B, B=C ⟹ {A,B,C})" $
            classesOf [("a", "b"), ("b", "c")]
                `shouldBe` S.fromList [S.fromList ["a", "b", "c"]]

        it "keeps disjoint pairs in separate classes" $
            classesOf [("a", "b"), ("c", "d")]
                `shouldBe` S.fromList [S.fromList ["a", "b"], S.fromList ["c", "d"]]

        it "collapses a cycle into a single class" $
            classesOf [("a", "b"), ("b", "c"), ("c", "a")]
                `shouldBe` S.fromList [S.fromList ["a", "b", "c"]]

        it "is unaffected by duplicate or reversed pairs" $
            classesOf [("a", "b"), ("b", "a"), ("a", "b")]
                `shouldBe` S.fromList [S.fromList ["a", "b"]]

        it "merges two chains that share a midpoint, leaving others apart" $
            classesOf [("a", "b"), ("c", "b"), ("d", "e")]
                `shouldBe` S.fromList [S.fromList ["a", "b", "c"], S.fromList ["d", "e"]]

        it "returns no classes for no pairs" $
            equivalenceClasses ([] :: [(Int, Int)]) `shouldBe` []

    describe "classesFromEdges" $ do
        it "fuses a name and a CAS joined by SameAs into one class" $ do
            let r = classesFromEdges [sameAs (nm "ei" "formaldehyde") (cas "50-00-0")]
            map S.fromList (crClasses r)
                `shouldBe` [S.fromList [nm "ei" "formaldehyde", cas "50-00-0"]]
            crConflicts r `shouldBe` []

        it "bridges two source-scoped names through a shared CAS anchor" $ do
            let r =
                    classesFromEdges
                        [ sameAs (nm "ei" "formaldehyde") (cas "50-00-0")
                        , sameAs (nm "sp" "methanal") (cas "50-00-0")
                        ]
            map S.fromList (crClasses r)
                `shouldBe` [S.fromList [nm "ei" "formaldehyde", nm "sp" "methanal", cas "50-00-0"]]

        it "does not fuse a Subsumes fan-out (SOx ⊃ SO2, SO) into a class" $ do
            let sox = nm "ef" "sulfur oxides"
                so2 = nm "ef" "sulfur dioxide"
                so1 = nm "ef" "sulfur monoxide"
                r =
                    classesFromEdges
                        [ SubstanceEdge sox so2 (Subsumes (SplitWeight 0.9))
                        , SubstanceEdge sox so1 (Subsumes (SplitWeight 0.1))
                        ]
            crClasses r `shouldBe` []
            crConflicts r `shouldBe` []

        it "surfaces a DistinctFrom that the SameAs closure violates" $ do
            let a = nm "ei" "carbon"
                b = nm "ei" "toc total organic carbon"
                r = classesFromEdges [sameAs a b, SubstanceEdge a b DistinctFrom]
            crConflicts r `shouldBe` [(a, b)]

        it "reports no conflict for a DistinctFrom the closure respects" $ do
            let a = nm "ei" "sulfur dioxide"
                b = nm "ei" "sulfur monoxide"
                r = classesFromEdges [SubstanceEdge a b DistinctFrom]
            crClasses r `shouldBe` []
            crConflicts r `shouldBe` []
