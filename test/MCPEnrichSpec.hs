{-# LANGUAGE OverloadedStrings #-}

{- | Pure-function tests for "API.MCP.Enrich".

These helpers do all the JSON munging that turns the serialized
'LCIABatchResult' / 'BatchImpactsResponse' into the shape the MCP
client sees: @web_url@ deep links at every level, and (optionally) a
restricted subset of the configured scoring sets. They're easy to
regress on a JSON-shape change, so each branch is covered here
independently of the live server.
-}
module MCPEnrichSpec (spec) where

import API.MCP.Enrich (
    addWebUrl,
    encodeSegment,
    filterScoringSets,
    filterScoringSetsBatch,
    scoreActivityWebUrl,
    slimLCIAPanel,
    summarizeBatchResults,
    summarizeLCIAPanel,
 )
import Control.Monad (forM_)
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.Key (fromText)
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Vector as V
import Test.Hspec

-- ---------------------------------------------------------------------------
-- Fixtures
--
-- These mirror the camelCase JSON shapes that 'strippedToJSON' emits for
-- LCIABatchResult / BatchImpactsEntry, not the snake_case names that
-- 'list_scoring_sets' uses for its own projection. Stay aligned with
-- those producers or these tests stop catching real regressions.
-- ---------------------------------------------------------------------------

{- | Minimal LCIABatchResult-shaped Value: one method entry, three
scoring sets, scoringResults missing one of them (simulating a
set whose formula failed to evaluate).
-}
sampleLBR :: Value
sampleLBR =
    object
        [ "results"
            .= [ object
                    [ "methodId" .= ("uuid-method-1" :: Text)
                    , "score" .= (1.23 :: Double)
                    ]
               ]
        , "scoringResults"
            .= object
                [ "PEF" .= object ["score" .= (1.0 :: Double)]
                , "ECS" .= object ["score" .= (2.0 :: Double)]
                ]
        , "scoringUnits"
            .= object
                [ "PEF" .= ("Pt" :: Text)
                , "ECS" .= ("Pt" :: Text)
                , "FAILED" .= ("Pt" :: Text)
                ]
        , "scoringIndicators"
            .= object
                [ "PEF" .= object []
                , "ECS" .= object []
                , "FAILED" .= object []
                ]
        ]

{- | Minimal BatchImpactsResponse-shaped Value: two entries with
impacts, one entry without impacts (defensive — see enrichBatchResults).
-}
sampleBatch :: Value
sampleBatch =
    object
        [ "results"
            .= [ object
                    [ "processId" .= ("pidA" :: Text)
                    , "impacts" .= sampleLBR
                    ]
               , object
                    [ "processId" .= ("pidB" :: Text)
                    , "impacts" .= sampleLBR
                    ]
               , object
                    -- malformed: no impacts subtree
                    ["processId" .= ("pidC" :: Text)]
               ]
        , "not_found" .= ([] :: [Text])
        , "invalid" .= ([] :: [Text])
        ]

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
    describe "scoreActivityWebUrl" $ do
        it "encodes every dynamic segment (no raw slashes leak through)" $
            scoreActivityWebUrl "https://volca.run" "db/with-slash" "pid/with-slash" "EF 3.1"
                `shouldBe` "https://volca.run/db/db%2Fwith-slash/activity/pid%2Fwith-slash/impacts/EF%203.1"

        it "round-trips ASCII-safe segments unchanged" $
            scoreActivityWebUrl "https://x" "agribalyse" "abc_def" "EF31"
                `shouldBe` "https://x/db/agribalyse/activity/abc_def/impacts/EF31"

    describe "encodeSegment" $
        it "percent-encodes unsafe URL characters" $ do
            encodeSegment "a/b" `shouldBe` "a%2Fb"
            encodeSegment "a b" `shouldBe` "a%20b"

    describe "addWebUrl" $ do
        it "inserts web_url into an Object" $
            addWebUrl "https://x" (object ["a" .= (1 :: Int)])
                `shouldBe` object ["a" .= (1 :: Int), "web_url" .= ("https://x" :: Text)]

        it "passes Array through unchanged" $
            addWebUrl "https://x" (Array (V.fromList [Null])) `shouldBe` Array (V.fromList [Null])

        it "passes String/Number/Bool/Null through unchanged" $ do
            addWebUrl "https://x" (String "s") `shouldBe` String "s"
            addWebUrl "https://x" (Number 42) `shouldBe` Number 42
            addWebUrl "https://x" (Bool True) `shouldBe` Bool True
            addWebUrl "https://x" Null `shouldBe` Null

    describe "slimLCIAPanel" $ do
        let panelWithFnUnit =
                object
                    [ "results"
                        .= [ object
                                [ "methodId" .= ("uuid-method-1" :: Text)
                                , "functionalUnit" .= ("1.0 kg of butter" :: Text)
                                , "web_url" .= ("https://x/should-be-dropped" :: Text)
                                , "score" .= (1.0 :: Double)
                                ]
                           , object
                                [ "methodId" .= ("uuid-method-2" :: Text)
                                , "functionalUnit" .= ("1.0 kg of butter" :: Text)
                                , "web_url" .= ("https://x/should-be-dropped-too" :: Text)
                                , "score" .= (2.0 :: Double)
                                ]
                           ]
                    , "scoringResults" .= object []
                    ]

        it "hoists functionalUnit from results[0] to the top level" $ do
            let Object km = slimLCIAPanel panelWithFnUnit
            KM.lookup (fromText "functionalUnit") km
                `shouldBe` Just (String "1.0 kg of butter")

        it "drops functionalUnit from every entry in results" $ do
            let Object km = slimLCIAPanel panelWithFnUnit
                Just (Array rs) = KM.lookup (fromText "results") km
            forM_ (V.toList rs) $ \(Object e) ->
                KM.lookup (fromText "functionalUnit") e `shouldBe` Nothing

        it "drops web_url from every entry in results" $ do
            let Object km = slimLCIAPanel panelWithFnUnit
                Just (Array rs) = KM.lookup (fromText "results") km
            forM_ (V.toList rs) $ \(Object e) ->
                KM.lookup (fromText "web_url") e `shouldBe` Nothing

        it "preserves the other top-level fields (scoringResults, etc.)" $ do
            let Object km = slimLCIAPanel panelWithFnUnit
            KM.lookup (fromText "scoringResults") km `shouldBe` Just (object [])

        it "is a no-op on a panel without results" $
            slimLCIAPanel (object ["scoringResults" .= object []])
                `shouldBe` object ["scoringResults" .= object []]

        it "handles a panel with an empty results array (no fn unit to lift)" $ do
            let v = object ["results" .= ([] :: [Value]), "scoringResults" .= object []]
                Object km = slimLCIAPanel v
            KM.lookup (fromText "functionalUnit") km `shouldBe` Nothing

        it "leaves entries without a functionalUnit untouched apart from web_url" $ do
            let v = object ["results" .= [object ["score" .= (1 :: Int)]]]
                Object km = slimLCIAPanel v
            KM.lookup (fromText "functionalUnit") km `shouldBe` Nothing

    describe "summarizeLCIAPanel" $ do
        let panel =
                object
                    [ "results"
                        .= [ object
                                [ "methodId" .= ("m1" :: Text)
                                , "functionalUnit" .= ("1.0 kg of butter" :: Text)
                                , "score" .= (1.0 :: Double)
                                ]
                           , object
                                [ "methodId" .= ("m2" :: Text)
                                , "functionalUnit" .= ("1.0 kg of butter" :: Text)
                                , "score" .= (2.0 :: Double)
                                ]
                           ]
                    , "scoringResults" .= object ["PEF" .= object ["score" .= (3.0 :: Double)]]
                    , "scoringIndicators" .= object []
                    ]

        it "drops the results array entirely" $ do
            let Object km = summarizeLCIAPanel panel
            KM.lookup (fromText "results") km `shouldBe` Nothing

        it "hoists functionalUnit from results[0] to the top level" $ do
            let Object km = summarizeLCIAPanel panel
            KM.lookup (fromText "functionalUnit") km
                `shouldBe` Just (String "1.0 kg of butter")

        it "preserves scoringResults and scoringIndicators" $ do
            let Object km = summarizeLCIAPanel panel
            KM.lookup (fromText "scoringResults") km
                `shouldBe` Just (object ["PEF" .= object ["score" .= (3.0 :: Double)]])
            KM.lookup (fromText "scoringIndicators") km
                `shouldBe` Just (object [])

        it "handles an empty results array (no fn unit to hoist)" $ do
            let v = object ["results" .= ([] :: [Value]), "scoringResults" .= object []]
                Object km = summarizeLCIAPanel v
            KM.lookup (fromText "functionalUnit") km `shouldBe` Nothing
            KM.lookup (fromText "results") km `shouldBe` Nothing

    describe "summarizeBatchResults" $ do
        it "adds web_url at the entry level (the documented shape)" $ do
            let Object km = summarizeBatchResults "https://x" "agribalyse" "EF31" sampleBatch
                Just (Array rs) = KM.lookup (fromText "results") km
                [Object e0, _, _] = V.toList rs
            KM.lookup (fromText "web_url") e0
                `shouldBe` Just (String "https://x/db/agribalyse/activity/pidA/impacts/EF31")

        it "also adds web_url inside the entry's impacts subtree" $ do
            let Object km = summarizeBatchResults "https://x" "agribalyse" "EF31" sampleBatch
                Just (Array rs) = KM.lookup (fromText "results") km
                [Object e0, _, _] = V.toList rs
                Just (Object impacts) = KM.lookup (fromText "impacts") e0
            KM.lookup (fromText "web_url") impacts
                `shouldBe` Just (String "https://x/db/agribalyse/activity/pidA/impacts/EF31")

        it "drops impacts.results from every entry (summary, not drill-down)" $ do
            let Object km = summarizeBatchResults "https://x" "agribalyse" "EF31" sampleBatch
                Just (Array rs) = KM.lookup (fromText "results") km
                [Object e0, Object e1, _] = V.toList rs
            forM_ [e0, e1] $ \entry -> case KM.lookup (fromText "impacts") entry of
                Just (Object impacts) ->
                    KM.lookup (fromText "results") impacts `shouldBe` Nothing
                other ->
                    expectationFailure ("expected an impacts object, got " <> show other)

        it "does NOT materialise an empty impacts object for entries that lack one" $ do
            let Object km = summarizeBatchResults "https://x" "agribalyse" "EF31" sampleBatch
                Just (Array rs) = KM.lookup (fromText "results") km
                [_, _, Object e2] = V.toList rs
            -- Entry without impacts should still gain web_url but not a fake impacts: {}
            KM.lookup (fromText "web_url") e2
                `shouldBe` Just (String "https://x/db/agribalyse/activity/pidC/impacts/EF31")
            KM.lookup (fromText "impacts") e2 `shouldBe` Nothing

    describe "filterScoringSets" $ do
        it "is a no-op when requested is empty" $
            filterScoringSets ["PEF", "ECS"] [] sampleLBR `shouldBe` Right sampleLBR

        it "accepts configured names that are missing from scoringResults" $ do
            -- 'FAILED' is configured (in scoringUnits / scoringIndicators) but
            -- did not produce a score; requesting it must succeed.
            case filterScoringSets ["PEF", "ECS", "FAILED"] ["FAILED"] sampleLBR of
                Right (Object km) -> do
                    KM.lookup (fromText "scoringResults") km
                        `shouldBe` Just (object [])
                    KM.lookup (fromText "scoringUnits") km
                        `shouldBe` Just (object ["FAILED" .= ("Pt" :: Text)])
                Right v -> expectationFailure ("expected Object, got " <> show v)
                Left e -> expectationFailure ("unexpected Left: " <> show e)

        it "rejects names not in configured" $
            filterScoringSets ["PEF", "ECS"] ["typo"] sampleLBR
                `shouldBe` Left "Unknown scoring set(s): typo. Configured on this collection: PEF, ECS"

        it "restricts all three scoring maps to the requested keys" $ do
            case filterScoringSets ["PEF", "ECS", "FAILED"] ["PEF"] sampleLBR of
                Right (Object km) -> do
                    KM.lookup (fromText "scoringResults") km
                        `shouldBe` Just (object ["PEF" .= object ["score" .= (1.0 :: Double)]])
                    KM.lookup (fromText "scoringUnits") km
                        `shouldBe` Just (object ["PEF" .= ("Pt" :: Text)])
                    KM.lookup (fromText "scoringIndicators") km
                        `shouldBe` Just (object ["PEF" .= object []])
                _ -> expectationFailure "expected an Object"

    describe "filterScoringSetsBatch" $ do
        it "validates against configured even when results is empty" $ do
            -- The fix: a score_activities call where every PID is unresolved
            -- still surfaces an unknown-name filter error instead of silently
            -- returning a zero-entry response.
            let emptyBatch = object ["results" .= ([] :: [Value])]
            filterScoringSetsBatch ["PEF"] ["typo"] emptyBatch
                `shouldBe` Left "Unknown scoring set(s): typo. Configured on this collection: PEF"

        it "is a no-op when requested is empty" $
            filterScoringSetsBatch ["PEF"] [] sampleBatch `shouldBe` Right sampleBatch

        it "restricts the impacts subtree of each entry" $ do
            case filterScoringSetsBatch ["PEF", "ECS", "FAILED"] ["PEF"] sampleBatch of
                Right (Object km) -> case KM.lookup (fromText "results") km of
                    Just (Array rs) -> case V.toList rs of
                        Object e0 : _ -> case KM.lookup (fromText "impacts") e0 of
                            Just (Object impacts) ->
                                KM.lookup (fromText "scoringResults") impacts
                                    `shouldBe` Just (object ["PEF" .= object ["score" .= (1.0 :: Double)]])
                            _ -> expectationFailure "expected impacts to be an object"
                        _ -> expectationFailure "expected entries"
                    _ -> expectationFailure "expected results array"
                Left e -> expectationFailure ("unexpected Left: " <> show e)
                _ -> expectationFailure "expected an object"
