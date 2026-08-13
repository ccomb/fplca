module SubstanceRegistrySpec (spec) where

import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Either (isLeft)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.UUID (nil)
import Test.Hspec

import SubstanceRegistry (
    CASNumber (..),
    ClassResult (..),
    ConversionFactor (..),
    FlowUUID (..),
    KeyNormalizers (..),
    NormName (..),
    Relation (..),
    SourceId (..),
    SplitWeight (..),
    SubstanceEdge (..),
    SubstanceKey (..),
    casBindingsFromEdges,
    classesFromEdges,
    equivalenceClasses,
    nonEmptyCAS,
    normalizeCAS,
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

-- A singleton name→CAS binding map, the shape 'casBindingsFromEdges' returns.
byName1 :: String -> String -> M.Map NormName CASNumber
byName1 n c = M.singleton (NormName (T.pack n)) (CASNumber (T.pack c))

-- A trivial normalizer for the parser tests (the real one is SynonymDB.normalizeName).
testNorm :: T.Text -> NormName
testNorm = NormName . T.toLower . T.strip

testCas :: T.Text -> CASNumber
testCas = CASNumber . normalizeCAS

testNorms :: KeyNormalizers
testNorms = KeyNormalizers testNorm testCas

-- Prefix the header row so each body line is a data row.
edgesCsv :: String -> BLC.ByteString
edgesCsv body =
    BLC.pack ("from_keytype,from_source,from_key,to_keytype,to_source,to_key,relation,scale\n" <> body)

-- CAS canonicalization asserted on String, so these read without OverloadedStrings.
normCas :: String -> String
normCas = T.unpack . normalizeCAS . T.pack

nonEmptyCas :: String -> Maybe String
nonEmptyCas = fmap T.unpack . nonEmptyCAS . T.pack

spec :: Spec
spec = do
    describe "normalizeCAS" $ do
        it "strips the zero-padding ecoinvent writes on the registry number" $
            normCas "001309-36-0" `shouldBe` "1309-36-0"

        it "leaves a canonical CAS alone" $
            normCas "7732-18-5" `shouldBe` "7732-18-5"

        it "strips a single leading zero from the registry number" $
            normCas "0074-98-6" `shouldBe` "74-98-6"

        it "keeps the fixed-width group segment padded (formaldehyde is 50-00-0)" $
            normCas "50-00-0" `shouldBe` "50-00-0"

        it "keeps a zero check digit" $
            normCas "1309-36-0" `shouldBe` "1309-36-0"

        it "keeps one zero when the registry number is all zeros" $
            normCas "000-00-0" `shouldBe` "0-00-0"

        it "passes a non-CAS string through stripped rather than mangling it" $
            normCas "  not-valid  " `shouldBe` "not-valid"

        it "agrees on a CAS however it was padded, so the bridge meets" $
            normCas "0000050-00-0" `shouldBe` normCas "50-00-0"

    describe "nonEmptyCAS" $ do
        it "canonicalizes a stated CAS" $
            nonEmptyCas "001309-36-0" `shouldBe` Just "1309-36-0"

        it "reads an empty field as no CAS" $
            nonEmptyCas "" `shouldBe` Nothing

        it "reads an all-zeros placeholder as no CAS" $
            nonEmptyCas "000-00-0" `shouldBe` Nothing

        it "reads a bare dash placeholder as no CAS" $
            nonEmptyCas "-" `shouldBe` Nothing

        -- A SimaPro-format method used to key formaldehyde as "50-0-0" while
        -- every flow carried "50-00-0", so the CAS rung compared two spellings
        -- of the same substance and never bridged.
        it "keeps the group segment padded, so a method CAS meets a flow CAS" $
            nonEmptyCas "50-00-0" `shouldBe` Just (normCas "50-00-0")

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
            parseSubstanceEdges testNorms (edgesCsv "name,simapro,Formaldehyde,cas,,50-00-0,sameas,\n")
                `shouldBe` Right [SubstanceEdge (nm "simapro" "formaldehyde") (cas "50-00-0") SameAs]

        it "parses a ProxyFor edge with its conversion factor" $
            parseSubstanceEdges testNorms (edgesCsv "cas,,1336-36-3,name,jrc,PCB,proxyfor,0.5\n")
                `shouldBe` Right [SubstanceEdge (cas "1336-36-3") (nm "jrc" "pcb") (ProxyFor (ConversionFactor 0.5))]

        it "parses a Subsumes edge with its split weight" $
            parseSubstanceEdges testNorms (edgesCsv "name,ef,sulfur oxides,name,ef,sulfur dioxide,subsumes,0.9\n")
                `shouldBe` Right [SubstanceEdge (nm "ef" "sulfur oxides") (nm "ef" "sulfur dioxide") (Subsumes (SplitWeight 0.9))]

        it "resolves a UUID anchor (ignoring its source annotation)" $
            fmap (map seFrom) (parseSubstanceEdges testNorms (edgesCsv "uuid,ecoinvent-3.11,00000000-0000-0000-0000-000000000000,cas,,1-1-1,sameas,\n"))
                `shouldBe` Right [ByUUID (FlowUUID nil)]

        it "parses several rows" $
            fmap length (parseSubstanceEdges testNorms (edgesCsv "name,a,x,name,a,y,sameas,\nname,a,y,name,a,z,sameas,\n"))
                `shouldBe` Right 2

        it "rejects a name key with no source" $
            parseSubstanceEdges testNorms (edgesCsv "name,,x,cas,,1-1-1,sameas,\n") `shouldSatisfy` isLeft

        it "rejects a Subsumes edge with no weight" $
            parseSubstanceEdges testNorms (edgesCsv "name,a,x,name,a,y,subsumes,\n") `shouldSatisfy` isLeft

        it "rejects a Subsumes weight outside (0,1]" $
            parseSubstanceEdges testNorms (edgesCsv "name,a,x,name,a,y,subsumes,1.5\n") `shouldSatisfy` isLeft

        it "rejects a ProxyFor factor of zero" $
            parseSubstanceEdges testNorms (edgesCsv "name,a,x,name,a,y,proxyfor,0\n") `shouldSatisfy` isLeft

        it "rejects a SameAs edge carrying a scale" $
            parseSubstanceEdges testNorms (edgesCsv "name,a,x,name,a,y,sameas,0.5\n") `shouldSatisfy` isLeft

        it "rejects an unknown relation" $
            parseSubstanceEdges testNorms (edgesCsv "name,a,x,name,a,y,equals,\n") `shouldSatisfy` isLeft

        it "rejects an unknown key type" $
            parseSubstanceEdges testNorms (edgesCsv "thing,a,x,name,a,y,sameas,\n") `shouldSatisfy` isLeft

        it "rejects a malformed UUID" $
            parseSubstanceEdges testNorms (edgesCsv "uuid,,not-a-uuid,cas,,1-1-1,sameas,\n") `shouldSatisfy` isLeft

        it "rejects a row with the wrong field count" $
            parseSubstanceEdges testNorms (edgesCsv "name,a,x,cas,,1-1-1,sameas\n") `shouldSatisfy` isLeft

        it "canonicalizes the CAS through the injected normalizer (leading zeros)" $
            fmap (map seTo) (parseSubstanceEdges testNorms (edgesCsv "name,simapro,Lead,cas,,007439-92-1,sameas,\n"))
                `shouldBe` Right [cas "7439-92-1"]

    describe "casBindingsFromEdges" $ do
        let bindings = fst . casBindingsFromEdges
            conflicts = snd . casBindingsFromEdges
        it "binds a name to a CAS from a SameAs edge, both orientations" $ do
            bindings [sameAs (nm "agb" "2,4-d") (cas "94-75-7")]
                `shouldBe` byName1 "2,4-d" "94-75-7"
            bindings [sameAs (cas "94-75-7") (nm "agb" "2,4-d")]
                `shouldBe` byName1 "2,4-d" "94-75-7"

        it "ignores ProxyFor, Subsumes, DistinctFrom and name↔name edges" $
            bindings
                [ SubstanceEdge (cas "1-1-1") (nm "a" "x") (ProxyFor (ConversionFactor 2))
                , SubstanceEdge (nm "a" "x") (nm "a" "y") (Subsumes (SplitWeight 0.5))
                , SubstanceEdge (nm "a" "x") (cas "1-1-1") DistinctFrom
                , sameAs (nm "a" "x") (nm "a" "y")
                ]
                `shouldBe` mempty

        it "reports a name bound to two distinct CAS (first kept)" $ do
            let es = [sameAs (nm "a" "x") (cas "1-1-1"), sameAs (nm "a" "x") (cas "2-2-2")]
            bindings es `shouldBe` byName1 "x" "1-1-1"
            map fst (conflicts es) `shouldBe` [NormName (T.pack "x")]
