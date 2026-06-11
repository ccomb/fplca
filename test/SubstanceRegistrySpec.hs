module SubstanceRegistrySpec (spec) where

import qualified Data.Set as S
import Test.Hspec

import SubstanceRegistry (equivalenceClasses)

-- | Normalize the result to a set of sets, so assertions are order-independent.
classesOf :: (Ord a) => [(a, a)] -> S.Set (S.Set a)
classesOf = S.fromList . map S.fromList . equivalenceClasses

spec :: Spec
spec = describe "equivalenceClasses" $ do
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
