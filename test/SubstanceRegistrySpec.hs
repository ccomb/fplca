module SubstanceRegistrySpec (spec) where

import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Either (isLeft)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.UUID (nil)
import Test.Hspec

import SubstanceRegistry (
    CASNumber (..),
    ClassResult (..),
    ConversionFactor (..),
    FlowUUID (..),
    NormName (..),
    Relation (..),
    SourceId (..),
    SplitWeight (..),
    SubstanceEdge (..),
    SubstanceKey (..),
    classesFromEdges,
    equivalenceClasses,
    parseSubstanceEdges,
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

-- A trivial normalizer for the parser tests (the real one is SynonymDB.normalizeName).
testNorm :: T.Text -> NormName
testNorm = NormName . T.toLower . T.strip

-- Prefix the header row so each body line is a data row.
edgesCsv :: String -> BLC.ByteString
edgesCsv body =
    BLC.pack ("from_keytype,from_source,from_key,to_keytype,to_source,to_key,relation,scale\n" <> body)

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

    describe "parseSubstanceEdges" $ do
        it "parses a name↔CAS SameAs edge, normalizing the name" $
            parseSubstanceEdges testNorm (edgesCsv "name,simapro,Formaldehyde,cas,,50-00-0,sameas,\n")
                `shouldBe` Right [SubstanceEdge (nm "simapro" "formaldehyde") (cas "50-00-0") SameAs]

        it "parses a ProxyFor edge with its conversion factor" $
            parseSubstanceEdges testNorm (edgesCsv "cas,,1336-36-3,name,jrc,PCB,proxyfor,0.5\n")
                `shouldBe` Right [SubstanceEdge (cas "1336-36-3") (nm "jrc" "pcb") (ProxyFor (ConversionFactor 0.5))]

        it "parses a Subsumes edge with its split weight" $
            parseSubstanceEdges testNorm (edgesCsv "name,ef,sulfur oxides,name,ef,sulfur dioxide,subsumes,0.9\n")
                `shouldBe` Right [SubstanceEdge (nm "ef" "sulfur oxides") (nm "ef" "sulfur dioxide") (Subsumes (SplitWeight 0.9))]

        it "resolves a UUID anchor (ignoring its source annotation)" $
            fmap (map seFrom) (parseSubstanceEdges testNorm (edgesCsv "uuid,ecoinvent-3.11,00000000-0000-0000-0000-000000000000,cas,,1-1-1,sameas,\n"))
                `shouldBe` Right [ByUUID (FlowUUID nil)]

        it "parses several rows" $
            fmap length (parseSubstanceEdges testNorm (edgesCsv "name,a,x,name,a,y,sameas,\nname,a,y,name,a,z,sameas,\n"))
                `shouldBe` Right 2

        it "rejects a name key with no source" $
            parseSubstanceEdges testNorm (edgesCsv "name,,x,cas,,1-1-1,sameas,\n") `shouldSatisfy` isLeft

        it "rejects a Subsumes edge with no weight" $
            parseSubstanceEdges testNorm (edgesCsv "name,a,x,name,a,y,subsumes,\n") `shouldSatisfy` isLeft

        it "rejects a Subsumes weight outside (0,1]" $
            parseSubstanceEdges testNorm (edgesCsv "name,a,x,name,a,y,subsumes,1.5\n") `shouldSatisfy` isLeft

        it "rejects a ProxyFor factor of zero" $
            parseSubstanceEdges testNorm (edgesCsv "name,a,x,name,a,y,proxyfor,0\n") `shouldSatisfy` isLeft

        it "rejects a SameAs edge carrying a scale" $
            parseSubstanceEdges testNorm (edgesCsv "name,a,x,name,a,y,sameas,0.5\n") `shouldSatisfy` isLeft

        it "rejects an unknown relation" $
            parseSubstanceEdges testNorm (edgesCsv "name,a,x,name,a,y,equals,\n") `shouldSatisfy` isLeft

        it "rejects an unknown key type" $
            parseSubstanceEdges testNorm (edgesCsv "thing,a,x,name,a,y,sameas,\n") `shouldSatisfy` isLeft

        it "rejects a malformed UUID" $
            parseSubstanceEdges testNorm (edgesCsv "uuid,,not-a-uuid,cas,,1-1-1,sameas,\n") `shouldSatisfy` isLeft

        it "rejects a row with the wrong field count" $
            parseSubstanceEdges testNorm (edgesCsv "name,a,x,cas,,1-1-1,sameas\n") `shouldSatisfy` isLeft
