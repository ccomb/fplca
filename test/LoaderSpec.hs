{-# LANGUAGE OverloadedStrings #-}

module LoaderSpec (spec) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import Test.Hspec

import Database.Loader
import TestHelpers (loadSampleDatabase)
import Types

-- ---------------------------------------------------------------------------
-- Minimal fixtures
-- ---------------------------------------------------------------------------

flowUUID1, flowUUID2, actUUID1 :: UUID.UUID
flowUUID1 = read "aaaaaaaa-0000-0000-0000-000000000001"
flowUUID2 = read "bbbbbbbb-0000-0000-0000-000000000002"
actUUID1 = read "cccccccc-0000-0000-0000-000000000001"

minimalFlow :: UUID.UUID -> Text -> TechnosphereFlow
minimalFlow fid name =
    TechnosphereFlow
        { tfId = fid
        , tfName = name
        , tfUnitId = UUID.nil
        , tfSynonyms = M.empty
        , tfCAS = Nothing
        , tfSubstanceId = Nothing
        }

minimalActivity :: Text -> Text -> [Exchange] -> Activity
minimalActivity name loc exs =
    Activity
        { activityName = name
        , activityDescription = []
        , activitySynonyms = M.empty
        , activityClassification = M.empty
        , activityLocation = loc
        , activityUnit = "kg"
        , exchanges = exs
        , activityParams = M.empty
        , activityParamExprs = M.empty
        , activityAllocationPercent = Nothing
        , activityAllocationFormula = Nothing
        , activityNativeType = Nothing
        }

refExchange :: UUID.UUID -> Exchange
refExchange fid =
    TechnosphereExchange
        { techFlowId = fid
        , techAmount = 1.0
        , techUnitId = UUID.nil
        , techRole = ReferenceProduct
        , techActivityLinkId = UUID.nil
        , techProcessLinkId = Nothing
        , techLocation = "GLO"
        , techComment = Nothing
        , techPedigree = Nothing
        }

inputExchange :: UUID.UUID -> Text -> Exchange
inputExchange fid loc =
    TechnosphereExchange
        { techFlowId = fid
        , techAmount = 0.5
        , techUnitId = UUID.nil
        , techRole = Input
        , techActivityLinkId = UUID.nil
        , techProcessLinkId = Nothing
        , techLocation = loc
        , techComment = Nothing
        , techPedigree = Nothing
        }

actUUID2, missingActUUID :: UUID.UUID
actUUID2 = read "cccccccc-0000-0000-0000-000000000002"
missingActUUID = read "dddddddd-0000-0000-0000-000000000099"

-- | An input linked (non-nil) to producer activity @actId@ producing @prodId@.
linkedInput :: UUID.UUID -> UUID.UUID -> Exchange
linkedInput actId prodId = (inputExchange prodId "GLO"){techActivityLinkId = actId}

{- | A treatment process's reference input (the waste it treats): an input-side
reference exchange that the matrix builder skips, so it is no supplier demand.
-}
referenceInput :: UUID.UUID -> Exchange
referenceInput fid = (inputExchange fid "GLO"){techRole = ReferenceInput}

simpleDBOf :: [((UUID.UUID, UUID.UUID), Activity)] -> [(UUID.UUID, Text)] -> SimpleDatabase
simpleDBOf acts flows =
    SimpleDatabase
        { sdbActivities = M.fromList acts
        , sdbTechFlows = M.fromList [(fid, minimalFlow fid name) | (fid, name) <- flows]
        , sdbBioFlows = M.empty
        , sdbWasteFlows = M.empty
        , sdbUnits = M.empty
        }

-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
    -- -----------------------------------------------------------------------
    -- normalizeText
    -- -----------------------------------------------------------------------
    describe "normalizeText" $ do
        it "lowercases text" $
            normalizeText "WHEAT Production" `shouldBe` "wheat production"

        it "strips leading and trailing whitespace" $
            normalizeText "  wheat  " `shouldBe` "wheat"

        it "handles empty text" $
            normalizeText "" `shouldBe` ""

    -- -----------------------------------------------------------------------
    -- mergeTechFlows
    -- -----------------------------------------------------------------------
    describe "mergeTechFlows" $ do
        it "unions synonyms from both flows" $ do
            let a = (minimalFlow flowUUID1 "CO2"){tfSynonyms = M.singleton "en" (S.fromList ["carbon dioxide"])}
                b = (minimalFlow flowUUID1 "CO2"){tfSynonyms = M.singleton "en" (S.fromList ["CO2"])}
                merged = mergeTechFlows a b
            M.lookup "en" (tfSynonyms merged) `shouldBe` Just (S.fromList ["carbon dioxide", "CO2"])

        it "keeps all other fields from the first flow" $ do
            let a = minimalFlow flowUUID1 "flow-a"
                b = minimalFlow flowUUID2 "flow-b"
            tfName (mergeTechFlows a b) `shouldBe` "flow-a"

    -- -----------------------------------------------------------------------
    -- generateActivityUUIDFromActivity
    -- -----------------------------------------------------------------------
    describe "generateActivityUUIDFromActivity" $ do
        it "is deterministic for the same activity" $ do
            let act = minimalActivity "wheat production" "GLO" []
            generateActivityUUIDFromActivity act
                `shouldBe` generateActivityUUIDFromActivity act

        it "differs for different name" $ do
            let a = minimalActivity "wheat production" "GLO" []
                b = minimalActivity "barley production" "GLO" []
            generateActivityUUIDFromActivity a
                `shouldNotBe` generateActivityUUIDFromActivity b

        it "differs for different location" $ do
            let a = minimalActivity "wheat production" "GLO" []
                b = minimalActivity "wheat production" "FR" []
            generateActivityUUIDFromActivity a
                `shouldNotBe` generateActivityUUIDFromActivity b

    -- -----------------------------------------------------------------------
    -- getReferenceProductUUID
    -- -----------------------------------------------------------------------
    describe "getReferenceProductUUID" $ do
        it "returns the flow UUID of the reference exchange" $ do
            let act = minimalActivity "prod" "GLO" [refExchange flowUUID1]
            getReferenceProductUUID act `shouldBe` flowUUID1

        it "returns UUID.nil when there is no reference exchange" $ do
            let act = minimalActivity "prod" "GLO" []
            getReferenceProductUUID act `shouldBe` UUID.nil

    -- -----------------------------------------------------------------------
    -- UnlinkedSummary Monoid (product of monoids: Map-union + 3× Int addition)
    -- -----------------------------------------------------------------------
    describe "UnlinkedSummary Monoid" $ do
        it "sums all counters via (<>)" $ do
            let s1 = UnlinkedSummary M.empty 10 8 2
                s2 = UnlinkedSummary M.empty 5 3 2
                m = s1 <> s2
            usTotalLinks m `shouldBe` 15
            usFoundLinks m `shouldBe` 11
            usMissingLinks m `shouldBe` 4

        it "unions activity maps via (<>)" $ do
            let s1 = UnlinkedSummary (M.singleton "actA" []) 1 0 1
                s2 = UnlinkedSummary (M.singleton "actB" []) 1 0 1
                m = s1 <> s2
            M.size (usActivities m) `shouldBe` 2

        it "mempty is the identity" $ do
            let s = UnlinkedSummary M.empty 3 2 1
                m = s <> mempty
            usTotalLinks m `shouldBe` 3
            usFoundLinks m `shouldBe` 2
            usMissingLinks m `shouldBe` 1

    -- -----------------------------------------------------------------------
    -- buildSupplierIndex (name+location keyed, EcoSpold1 style)
    -- -----------------------------------------------------------------------
    describe "buildSupplierIndex" $ do
        it "indexes reference exchanges by (normalizedName, location)" $ do
            let act =
                    minimalActivity
                        "wheat production"
                        "GLO"
                        [refExchange flowUUID1]
                acts = M.fromList [((actUUID1, flowUUID1), act)]
                flows = M.fromList [(flowUUID1, minimalFlow flowUUID1 "Wheat")]
                idx = buildSupplierIndex acts flows
            M.lookup ("wheat", "GLO") idx `shouldBe` Just (actUUID1, flowUUID1)

        it "does not index input (non-reference) exchanges" $ do
            let act =
                    minimalActivity
                        "consumer"
                        "DE"
                        [inputExchange flowUUID1 "GLO"]
                acts = M.fromList [((actUUID1, flowUUID1), act)]
                flows = M.fromList [(flowUUID1, minimalFlow flowUUID1 "Wheat")]
                idx = buildSupplierIndex acts flows
            M.null idx `shouldBe` True

    -- -----------------------------------------------------------------------
    -- buildSupplierIndexByName (name-only keyed, SimaPro style)
    -- -----------------------------------------------------------------------
    describe "buildSupplierIndexByName" $ do
        it "indexes reference exchanges by normalized flow name" $ do
            let act =
                    minimalActivity
                        "wheat production"
                        "GLO"
                        [refExchange flowUUID1]
                acts = M.fromList [((actUUID1, flowUUID1), act)]
                flows = M.fromList [(flowUUID1, minimalFlow flowUUID1 "Wheat Production")]
                idx = buildSupplierIndexByName acts flows
            M.lookup "wheat production" idx `shouldBe` Just (actUUID1, flowUUID1)

        it "does not index non-reference exchanges" $ do
            let act =
                    minimalActivity
                        "consumer"
                        "DE"
                        [inputExchange flowUUID1 "GLO"]
                acts = M.fromList [((actUUID1, flowUUID1), act)]
                flows = M.fromList [(flowUUID1, minimalFlow flowUUID1 "Wheat")]
                idx = buildSupplierIndexByName acts flows
            M.null idx `shouldBe` True

    -- -----------------------------------------------------------------------
    -- fixExchangeLinkByName (SimaPro-style name-only linking)
    -- -----------------------------------------------------------------------
    describe "fixExchangeLinkByName" $ do
        it "resolves input exchange when supplier in index" $ do
            let flows = M.fromList [(flowUUID1, minimalFlow flowUUID1 "wheat")]
                idx = M.fromList [("wheat", (actUUID1, flowUUID2))]
                ex = inputExchange flowUUID1 "GLO"
                (fixed, summary) = fixExchangeLinkByName idx flows "consumer" ex
            techActivityLinkId fixed `shouldBe` actUUID1
            usFoundLinks summary `shouldBe` 1
            usMissingLinks summary `shouldBe` 0

        it "leaves exchange unlinked when supplier not in index" $ do
            let flows = M.fromList [(flowUUID1, minimalFlow flowUUID1 "wheat")]
                idx = M.empty
                ex = inputExchange flowUUID1 "GLO"
                (fixed, summary) = fixExchangeLinkByName idx flows "consumer" ex
            techActivityLinkId fixed `shouldBe` UUID.nil
            usMissingLinks summary `shouldBe` 1

        it "leaves exchange unlinked when flow not in flowDB" $ do
            let flows = M.empty
                idx = M.empty
                ex = inputExchange flowUUID1 "GLO"
                (fixed, summary) = fixExchangeLinkByName idx flows "consumer" ex
            techActivityLinkId fixed `shouldBe` UUID.nil
            usMissingLinks summary `shouldBe` 1

        it "does not touch output reference exchanges" $ do
            let flows = M.fromList [(flowUUID1, minimalFlow flowUUID1 "wheat")]
                idx = M.fromList [("wheat", (actUUID1, flowUUID2))]
                ex = refExchange flowUUID1
                (fixed, summary) = fixExchangeLinkByName idx flows "producer" ex
            techActivityLinkId fixed `shouldBe` UUID.nil -- unchanged
            usTotalLinks summary `shouldBe` 0 -- not counted
        it "does not touch biosphere exchanges" $ do
            let flows = M.empty
                idx = M.empty
                bioEx =
                    BiosphereExchange
                        { bioFlowId = flowUUID1
                        , bioAmount = 1.0
                        , bioUnitId = UUID.nil
                        , bioDirection = Emission
                        , bioLocation = ""
                        , bioComment = Nothing
                        , bioPedigree = Nothing
                        }
                (fixed, summary) = fixExchangeLinkByName idx flows "act" bioEx
            -- BiosphereExchange is returned unchanged: verify it is still biosphere
            isBiosphereExchange fixed `shouldBe` True
            usTotalLinks summary `shouldBe` 0

    -- -----------------------------------------------------------------------
    -- countTotalTechInputs / countUnlinkedExchanges / collectUnlinkedProductNames
    -- (integration tests via SAMPLE.min3)
    -- -----------------------------------------------------------------------
    describe "countTotalTechInputs" $ do
        it "counts all technosphere inputs in SAMPLE.min3" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let sdb = Types.toSimpleDatabase db
            countTotalTechInputs sdb `shouldSatisfy` (> 0)

    describe "countUnlinkedExchanges" $ do
        it "returns 0 for fully linked SAMPLE.min3" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let sdb = Types.toSimpleDatabase db
            countUnlinkedExchanges sdb `shouldBe` 0

    describe "collectUnlinkedProductNames" $ do
        it "returns empty map for fully linked SAMPLE.min3" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let sdb = Types.toSimpleDatabase db
            M.null (collectUnlinkedProductNames sdb) `shouldBe` True

    -- A partial EcoSpold2 import carries non-nil activityLinkIds that point at
    -- background activities it doesn't ship. Those links must read as unlinked,
    -- not silently masquerade as resolved internal links (the matrix builder
    -- drops them, so the score would otherwise undercount with no warning).
    describe "countUnlinkedExchanges (producer presence)" $ do
        it "counts a non-nil link to an absent activity as unlinked" $ do
            let consumer = minimalActivity "lyocell fibre" "GLO" [refExchange flowUUID1, linkedInput missingActUUID flowUUID2]
                sdb = simpleDBOf [((actUUID1, flowUUID1), consumer)] [(flowUUID1, "lyocell fibre"), (flowUUID2, "chemical, inorganic")]
            countUnlinkedExchanges sdb `shouldBe` 1

        it "does not count a non-nil link to a present activity" $ do
            let consumer = minimalActivity "lyocell fibre" "GLO" [refExchange flowUUID1, linkedInput actUUID2 flowUUID2]
                supplier = minimalActivity "chemical, inorganic" "GLO" [refExchange flowUUID2]
                sdb = simpleDBOf [((actUUID1, flowUUID1), consumer), ((actUUID2, flowUUID2), supplier)] [(flowUUID1, "lyocell fibre"), (flowUUID2, "chemical, inorganic")]
            countUnlinkedExchanges sdb `shouldBe` 0

    describe "collectUnlinkedProductNames (producer presence)" $ do
        it "surfaces the product of a dangling non-nil link" $ do
            let consumer = minimalActivity "lyocell fibre" "GLO" [refExchange flowUUID1, linkedInput missingActUUID flowUUID2]
                sdb = simpleDBOf [((actUUID1, flowUUID1), consumer)] [(flowUUID1, "lyocell fibre"), (flowUUID2, "chemical, inorganic")]
            collectUnlinkedProductNames sdb `shouldBe` M.fromList [("chemical, inorganic", 1)]

    -- A treatment process's reference input (ReferenceInput) is a self-edge the
    -- matrix builder skips, not a supplier demand. Counting it would drag a
    -- solvable treatment database below 100% complete and wrongly refuse
    -- finalize, so it must stay out of both the total and the unlinked tally.
    describe "reference inputs are not supplier demands" $ do
        it "excludes a treatment ReferenceInput from the input total" $ do
            let treatment = minimalActivity "waste treatment" "GLO" [referenceInput flowUUID1, linkedInput actUUID2 flowUUID2]
                supplier = minimalActivity "electricity" "GLO" [refExchange flowUUID2]
                sdb = simpleDBOf [((actUUID1, flowUUID1), treatment), ((actUUID2, flowUUID2), supplier)] [(flowUUID1, "waste"), (flowUUID2, "electricity")]
            -- only the linked electricity input is a demand; the ReferenceInput is not
            countTotalTechInputs sdb `shouldBe` 1

        it "does not count a nil-link ReferenceInput as unlinked" $ do
            let treatment = minimalActivity "waste treatment" "GLO" [referenceInput flowUUID1]
                sdb = simpleDBOf [((actUUID1, flowUUID1), treatment)] [(flowUUID1, "waste")]
            countUnlinkedExchanges sdb `shouldBe` 0

    -- ---------------------------------------------------------------------
    -- activityNormFactor — exercises every TechRole branch so the
    -- treatment-process (ReferenceInput) case can't silently regress to
    -- the "no reference output" 1.0 fallback.
    -- ---------------------------------------------------------------------
    describe "activityNormFactor" $ do
        let actUUID = actUUID1
            prodUUID = flowUUID1
            wasteUUID = flowUUID2
            withRole role amt fid =
                TechnosphereExchange
                    { techFlowId = fid
                    , techAmount = amt
                    , techUnitId = UUID.nil
                    , techRole = role
                    , techActivityLinkId = UUID.nil
                    , techProcessLinkId = Nothing
                    , techLocation = ""
                    , techComment = Nothing
                    , techPedigree = Nothing
                    }
        it "returns the reference output amount for a normal producer" $ do
            let act = minimalActivity "producer" "GLO" [withRole ReferenceProduct 3.0 prodUUID]
            activityNormFactor act (actUUID, prodUUID) `shouldBe` 3.0

        it "returns abs(reference-input amount) for a treatment process" $ do
            -- ReferenceInput is the only role that drives the refInputs fallback;
            -- SimaPro waste-treatment processes encode a negative amount.
            let act = minimalActivity "incineration" "GLO" [withRole ReferenceInput (-2.5) wasteUUID]
            activityNormFactor act (actUUID, wasteUUID) `shouldBe` 2.5

        it "falls back to 1.0 when no reference exchange is present" $ do
            let act = minimalActivity "empty" "GLO" [withRole Input 1.0 wasteUUID]
            activityNormFactor act (actUUID, prodUUID) `shouldBe` 1.0

        it "subtracts self-loop consumption from the reference output" $ do
            let selfInput =
                    (withRole Input 0.2 prodUUID){techActivityLinkId = actUUID}
                refOut = withRole ReferenceProduct 1.0 prodUUID
                act = minimalActivity "self-looper" "GLO" [refOut, selfInput]
            activityNormFactor act (actUUID, prodUUID) `shouldBe` 0.8

        it "ignores Coproduct exchanges when computing the norm" $ do
            -- Coproducts are outputs but don't contribute to the activity's
            -- reference-output sum. An activity with only Coproducts (no
            -- ReferenceProduct, no ReferenceInput) hits the 1.0 fallback,
            -- not "sum of all outputs".
            let coproduct = withRole Coproduct 7.0 wasteUUID
                act = minimalActivity "coproduct-only" "GLO" [coproduct]
            activityNormFactor act (actUUID, prodUUID) `shouldBe` 1.0

    describe "activity classifications (full-load integration)" $ do
        it "EcoSpold2 SAMPLE.min3: every activity carries a non-empty classification map" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let activities = V.toList $ dbActivities db
            any (M.null . activityClassification) activities `shouldBe` False

        it "EcoSpold2 SAMPLE.min3: surfaces ISIC rev.4 ecoinvent values" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let activities = V.toList $ dbActivities db
                isicValues =
                    [ v
                    | a <- activities
                    , Just v <- [M.lookup "ISIC rev.4 ecoinvent" (activityClassification a)]
                    ]
            isicValues `shouldContain` ["2394:Manufacture of cement"]
            isicValues `shouldContain` ["0810:Quarrying of stone, sand and clay"]

        it "EcoSpold2 SAMPLE.min3: surfaces CPC values" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let activities = V.toList $ dbActivities db
                cpcValues =
                    [ v
                    | a <- activities
                    , Just v <- [M.lookup "CPC" (activityClassification a)]
                    ]
            cpcValues `shouldBe` ["3744:Cement"]

        it "EcoSpold1 SAMPLE.ecospold1: category and subCategory promoted to classifications" $ do
            db <- loadSampleDatabase "SAMPLE.ecospold1"
            let activities = V.toList $ dbActivities db
            length activities `shouldSatisfy` (>= 1)
            let cls = activityClassification (head activities)
            M.lookup "Category" cls `shouldBe` Just "Energy"
            M.lookup "SubCategory" cls `shouldBe` Just "Electricity"
