{-# LANGUAGE OverloadedStrings #-}

module OlcaSchemaSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import qualified Data.UUID as UUID
import Test.Hspec

import Method.Mapping (MethodTables (..), buildMethodTables)
import Method.Parser.OlcaSchema (isOlcaImpactCategoryJson, parseOlcaImpactCategoryBytes)
import Method.Types

spec :: Spec
spec = do
    describe "isOlcaImpactCategoryJson" $ do
        it "recognizes an openLCA ImpactCategory" $ do
            bytes <- BS.readFile "test-data/olca-schema-mini/impact-category-mini.json"
            isOlcaImpactCategoryJson bytes `shouldBe` True

        it "rejects unrelated JSON" $ do
            isOlcaImpactCategoryJson "{\"foo\": 1}" `shouldBe` False
            isOlcaImpactCategoryJson "[1,2,3]" `shouldBe` False
            isOlcaImpactCategoryJson "not json" `shouldBe` False

        it "rejects another openLCA entity type" $
            -- The auto-detection must not pull in Process / Flow / etc. files
            -- that may sit in the same method directory.
            isOlcaImpactCategoryJson "{\"@type\":\"Process\",\"name\":\"x\"}"
                `shouldBe` False

    describe "parseOlcaImpactCategoryBytes" $ do
        it "parses the mini fixture and yields one MethodCF per ImpactFactor" $ do
            bytes <- BS.readFile "test-data/olca-schema-mini/impact-category-mini.json"
            case parseOlcaImpactCategoryBytes bytes of
                Left err -> expectationFailure ("parse failed: " ++ err)
                Right method -> do
                    methodName method `shouldBe` "Regional LCIA Mini"
                    methodUnit method `shouldBe` "m2*year"
                    length (methodFactors method) `shouldBe` 4

        it "preserves location code, value, and flow UUID on each cell" $ do
            bytes <- BS.readFile "test-data/olca-schema-mini/impact-category-mini.json"
            case parseOlcaImpactCategoryBytes bytes of
                Left err -> expectationFailure ("parse failed: " ++ err)
                Right method -> do
                    let factors = methodFactors method
                        landFR =
                            head
                                [ f
                                | f <- factors
                                , mcfFlowName f == "Occupation, agriculture"
                                , mcfConsumerLocation f == Just "FR"
                                ]
                        landGLO =
                            head
                                [ f
                                | f <- factors
                                , mcfFlowName f == "Occupation, agriculture"
                                , mcfConsumerLocation f == Just "GLO"
                                ]
                    mcfValue landFR `shouldBe` 22.15
                    mcfValue landGLO `shouldBe` 10.0
                    -- The fixture's flow @id round-trips into mcfFlowRef
                    UUID.toText (mcfFlowRef landFR)
                        `shouldBe` "0305b169-255d-4041-8f5d-6e095bcb6358"

        it "rejects a non-object top level" $
            case parseOlcaImpactCategoryBytes "[1,2,3]" of
                Left _ -> pure ()
                Right _ -> expectationFailure "expected parse failure on array root"

        it "rejects a wrong @type" $
            case parseOlcaImpactCategoryBytes "{\"@type\":\"Process\",\"name\":\"x\"}" of
                Left _ -> pure ()
                Right _ -> expectationFailure "expected parse failure on @type=Process"

    describe "parseImpactFactor flow.category → Compartment" $ do
        -- Regression gate for d8a054d & 3d2f7e5: openLCA flows with identical
        -- 'name' values but different category paths (e.g. Agribalyse has 3
        -- "Occupation, annual crop" flows under resource, resource/land and
        -- resource/biotic) must surface their full compartment path so the
        -- DB-flow matcher can disambiguate. Without this, only one of the
        -- three is reachable and ~30% of regionalized factors miss their
        -- target flow silently.
        it "extracts (medium, subcompartment) from a slash-separated path" $ do
            let bytes =
                    "{\"@type\":\"ImpactCategory\",\"name\":\"M\",\"referenceUnitName\":\"u\",\
                    \\"impactFactors\":[{\"@type\":\"ImpactFactor\",\"value\":1.0,\
                    \\"flow\":{\"@type\":\"Flow\",\"name\":\"Occupation, annual crop\",\
                    \\"category\":{\"@type\":\"Category\",\"name\":\"resource/land\"}}}]}"
            case parseOlcaImpactCategoryBytes bytes of
                Left err -> expectationFailure ("parse failed: " ++ err)
                Right method ->
                    case methodFactors method of
                        [cf] -> mcfCompartment cf `shouldBe` Just (Compartment "resource" "land" "")
                        _ -> expectationFailure "expected exactly one factor"

        it "keeps deeper subpaths intact (e.g. resource/in air / long-term)" $ do
            let bytes =
                    "{\"@type\":\"ImpactCategory\",\"name\":\"M\",\"referenceUnitName\":\"u\",\
                    \\"impactFactors\":[{\"@type\":\"ImpactFactor\",\"value\":1.0,\
                    \\"flow\":{\"@type\":\"Flow\",\"name\":\"f\",\
                    \\"category\":{\"@type\":\"Category\",\"name\":\"resource/in air/upper stratosphere\"}}}]}"
            case parseOlcaImpactCategoryBytes bytes of
                Right method ->
                    case methodFactors method of
                        [cf] ->
                            mcfCompartment cf
                                `shouldBe` Just (Compartment "resource" "in air/upper stratosphere" "")
                        _ -> expectationFailure "expected exactly one factor"
                Left err -> expectationFailure ("parse failed: " ++ err)

        it "leaves mcfCompartment Nothing when the flow has no category" $ do
            -- The mini fixture has no category fields, so Compartment must
            -- stay 'Nothing' — the matcher falls back to the legacy name-only
            -- path. Regression gate against the disambiguation breaking
            -- non-openLCA / non-Agribalyse methods that ship without category.
            bytes <- BS.readFile "test-data/olca-schema-mini/impact-category-mini.json"
            case parseOlcaImpactCategoryBytes bytes of
                Left err -> expectationFailure ("parse failed: " ++ err)
                Right method ->
                    map mcfCompartment (methodFactors method)
                        `shouldBe` replicate (length (methodFactors method)) Nothing

        it "single-segment category resolves to medium with empty subcompartment" $ do
            let bytes =
                    "{\"@type\":\"ImpactCategory\",\"name\":\"M\",\"referenceUnitName\":\"u\",\
                    \\"impactFactors\":[{\"@type\":\"ImpactFactor\",\"value\":1.0,\
                    \\"flow\":{\"@type\":\"Flow\",\"name\":\"f\",\
                    \\"category\":{\"@type\":\"Category\",\"name\":\"air\"}}}]}"
            case parseOlcaImpactCategoryBytes bytes of
                Right method ->
                    case methodFactors method of
                        [cf] -> mcfCompartment cf `shouldBe` Just (Compartment "air" "" "")
                        _ -> expectationFailure "expected exactly one factor"
                Left err -> expectationFailure ("parse failed: " ++ err)

    describe "buildMethodTables on parsed openLCA methods" $ do
        it "leaves mtRegionalizedCF empty when no flow matched (Nothing in mappings)" $ do
            -- Without database flows to match against, every CF stays unmapped, so
            -- the regionalized table is empty (it only indexes successfully-matched
            -- cells). Documents the contract: regional indexing requires a DB match
            -- by name/UUID/CAS/synonym first.
            bytes <- BS.readFile "test-data/olca-schema-mini/impact-category-mini.json"
            case parseOlcaImpactCategoryBytes bytes of
                Left err -> expectationFailure ("parse failed: " ++ err)
                Right method -> do
                    let mappings = [(cf, Nothing) | cf <- methodFactors method]
                        tables = buildMethodTables M.empty mappings
                    M.size (mtRegionalizedCF tables) `shouldBe` 0
