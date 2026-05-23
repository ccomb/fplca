{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Pure JSON munging used by the @score_activity@ / @score_activities@
MCP tools.

Two responsibilities:

  * Decorate the serialized 'LCIABatchResult' / 'BatchImpactsResponse'
    with @web_url@ deep links so MCP clients can hand a human a clickable
    follow-up.
  * Restrict the response to a user-requested subset of scoring sets,
    failing loudly on names that are not configured on the collection.

Every traversal goes through 'overObject' / 'overObjectE' / 'overArray' /
'overArrayE', the single place that has to enumerate every constructor
of Aeson's 'Value'. Callers stay free of wildcard patterns on the sum.
-}
module API.MCP.Enrich (
    -- * URL helpers
    encodeSegment,
    scoreActivityWebUrl,

    -- * web_url enrichment
    addWebUrl,
    enrichResultsWithWebUrl,
    enrichBatchResults,

    -- * payload slimming
    slimLCIAPanel,

    -- * scoring_sets filter
    filterScoringSets,
    filterScoringSetsBatch,

    -- * Value combinators (exported for tests)
    overObject,
    overObjectE,
    overArray,
    valueText,
) where

import Data.Aeson (Value (..))
import Data.Aeson.Key (fromText)
import qualified Data.Aeson.Key as Key
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Network.URI (escapeURIString, isUnreserved)

-- ---------------------------------------------------------------------------
-- Value combinators — single point of exhaustive Aeson pattern matching
-- ---------------------------------------------------------------------------

{- | Apply 'f' to the contents of a JSON Object; pass any other 'Value'
shape through unchanged. One exhaustive match lives here so individual
callers do not sprinkle wildcards.
-}
overObject :: (KeyMap Value -> KeyMap Value) -> Value -> Value
overObject f = \case
    Object km -> Object (f km)
    Array a -> Array a
    String s -> String s
    Number n -> Number n
    Bool b -> Bool b
    Null -> Null

-- | Same as 'overObject' but the transformation may fail with 'Text'.
overObjectE :: (KeyMap Value -> Either Text (KeyMap Value)) -> Value -> Either Text Value
overObjectE f = \case
    Object km -> Object <$> f km
    Array a -> Right (Array a)
    String s -> Right (String s)
    Number n -> Right (Number n)
    Bool b -> Right (Bool b)
    Null -> Right Null

-- | Apply 'f' to each element of a JSON Array; pass any other shape through.
overArray :: (Value -> Value) -> Value -> Value
overArray f = \case
    Array v -> Array (V.map f v)
    Object km -> Object km
    String s -> String s
    Number n -> Number n
    Bool b -> Bool b
    Null -> Null

-- | Pull a 'Text' out of a 'Value' (only 'String' is admitted).
valueText :: Value -> Maybe Text
valueText = \case
    String s -> Just s
    Object _ -> Nothing
    Array _ -> Nothing
    Number _ -> Nothing
    Bool _ -> Nothing
    Null -> Nothing

-- ---------------------------------------------------------------------------
-- URL helpers
-- ---------------------------------------------------------------------------

-- | Percent-encode a 'Text' value for use as a URL path segment.
encodeSegment :: Text -> Text
encodeSegment = T.pack . escapeURIString isUnreserved . T.unpack

{- | Activity-level impacts page URL (the LCIA batch view in the web UI).
Every dynamic segment is percent-encoded so a 'dbName' \/ 'processId' \/
'collection' that contains @/@ or @?@ does not silently fracture the
URL.
-}
scoreActivityWebUrl :: Text -> Text -> Text -> Text -> Text
scoreActivityWebUrl baseUrl dbName pidText coll =
    baseUrl
        <> "/db/"
        <> encodeSegment dbName
        <> "/activity/"
        <> encodeSegment pidText
        <> "/impacts/"
        <> encodeSegment coll

-- ---------------------------------------------------------------------------
-- web_url enrichment
-- ---------------------------------------------------------------------------

webUrlKey, resultsKey, impactsKey, processIdKey, methodIdKey, functionalUnitKey :: Key.Key
webUrlKey = fromText "web_url"
resultsKey = fromText "results"
impactsKey = fromText "impacts"
-- 'strippedToJSON' on the API record drops the field prefix and keeps
-- camelCase; stay aligned with what 'LCIABatchResult' / 'BatchImpactsEntry'
-- actually serialize to ('methodId', not 'method_id').
processIdKey = fromText "processId"
methodIdKey = fromText "methodId"
functionalUnitKey = fromText "functionalUnit"

-- | Add a 'web_url' field to a JSON object at the top level.
addWebUrl :: Text -> Value -> Value
addWebUrl url = overObject (KM.insert webUrlKey (String url))

{- | Enrich the @results@ array of a serialized 'LCIABatchResult' with a
per-method 'web_url' built from each entry's @methodId@. Entries
without a string @methodId@ are passed through unchanged.
-}
enrichResultsWithWebUrl :: Text -> Value -> Value
enrichResultsWithWebUrl baseUrlForCategory =
    overObject (adjustKey resultsKey (overArray (enrichResultEntry baseUrlForCategory)))

enrichResultEntry :: Text -> Value -> Value
enrichResultEntry baseUrlForCategory =
    overObject $ \km ->
        case KM.lookup methodIdKey km >>= valueText of
            Just mid ->
                KM.insert webUrlKey (String (baseUrlForCategory <> "/" <> mid)) km
            Nothing -> km

{- | Enrich each entry of a serialized 'BatchImpactsResponse' with a
@web_url@ pointing at the activity-level impacts page.

The URL is attached at two levels of each entry:

  * @results[i].web_url@ — promised by the @score_activities@ resource
    description; a client reading the entry shape directly lands on it.
  * @results[i].impacts.web_url@ (plus per-method @web_url@s under
    @impacts.results@) — same shape as a standalone @score_activity@
    response.

An entry that lacks an @impacts@ subtree (defensive — the
'BatchImpactsEntry' record guarantees one) is left untouched in that
subtree, rather than gaining an empty placeholder.
-}
enrichBatchResults :: Text -> Text -> Text -> Value -> Value
enrichBatchResults baseUrl dbName coll =
    overObject (adjustKey resultsKey (overArray (enrichBatchEntry baseUrl dbName coll)))

enrichBatchEntry :: Text -> Text -> Text -> Value -> Value
enrichBatchEntry baseUrl dbName coll =
    overObject $ \km ->
        case KM.lookup processIdKey km >>= valueText of
            Just pidText ->
                let url = scoreActivityWebUrl baseUrl dbName pidText coll
                    withImpacts = case KM.lookup impactsKey km of
                        Just impactsValue ->
                            KM.insert
                                impactsKey
                                (addWebUrl url (enrichResultsWithWebUrl url impactsValue))
                                km
                        Nothing -> km
                 in KM.insert webUrlKey (String url) withImpacts
            Nothing -> km

-- ---------------------------------------------------------------------------
-- payload slimming
-- ---------------------------------------------------------------------------

{- | Slim down a serialized 'LCIABatchResult' for MCP transport. Two
edits, both purely about wire weight (no information lost):

  * Hoist @functionalUnit@ from @results[0]@ to the top level. Every
    entry in @results@ shares the same value (one panel = one activity
    = one functional unit), so repeating it 27 times is pure bloat.
    The lifted copy lives next to @results@ and the per-entry copies
    are removed.
  * Drop @web_url@ from every entry. The panel-level @web_url@ added
    by 'addWebUrl' already lands on the page that lists every method;
    a deep link per method would be redundant.

Defensive on the shape: missing @results@, empty @results@, or entries
without a @functionalUnit@ are all passed through cleanly.
-}
slimLCIAPanel :: Value -> Value
slimLCIAPanel = overObject $ \km ->
    let fnUnit = case KM.lookup resultsKey km of
            Just (Array rs) -> firstFunctionalUnit rs
            _ -> Nothing
        slimmedResults = adjustKey resultsKey (overArray stripEntry) km
        stripEntry = overObject (KM.delete functionalUnitKey . KM.delete webUrlKey)
     in case fnUnit of
            Just fu -> KM.insert functionalUnitKey fu slimmedResults
            Nothing -> slimmedResults

firstFunctionalUnit :: V.Vector Value -> Maybe Value
firstFunctionalUnit rs = case V.toList rs of
    Object km : _ -> KM.lookup functionalUnitKey km
    _ -> Nothing

-- ---------------------------------------------------------------------------
-- scoring_sets filter
-- ---------------------------------------------------------------------------

{- | Restrict the scoring-related maps on a serialized 'LCIABatchResult'
to the scoring-set names in 'requested'.

'configured' is the authoritative list of scoring-set names defined on
the method collection (e.g. @map ssName . mcScoringSets@). Validation
runs against it — not against the keys actually present in
@scoringResults@ — so:

  * A configured scoring set whose evaluation produced no scores (and
    is therefore absent from @scoringResults@) is still accepted by
    the filter, instead of being incorrectly reported as unknown.
  * The "unknown name" error message lists the same set of legal
    values regardless of evaluation outcomes.

An empty 'requested' is a no-op; an unknown name is a hard error.
-}
filterScoringSets :: [Text] -> [Text] -> Value -> Either Text Value
filterScoringSets _ [] v = Right v
filterScoringSets configured requested v = do
    validateRequested configured requested
    Right (restrictScoringSets requested v)

{- | Apply 'filterScoringSets' to every entry's @impacts@ subtree in a
serialized 'BatchImpactsResponse'.

Validation runs once at the top level — not inside each entry — so a
batch with zero successful entries still surfaces an unknown-name
error. Otherwise a @score_activities@ call where every PID is invalid
would silently swallow a typo'd scoring-set filter.
-}
filterScoringSetsBatch :: [Text] -> [Text] -> Value -> Either Text Value
filterScoringSetsBatch _ [] v = Right v
filterScoringSetsBatch configured requested v = do
    validateRequested configured requested
    overObjectE (traverseKey resultsKey (overArrayE (filterEntry requested))) v

{- | Check 'requested' against 'configured', failing with a message that
lists the missing names and the legal options.
-}
validateRequested :: [Text] -> [Text] -> Either Text ()
validateRequested configured requested =
    case [r | r <- requested, r `notElem` configured] of
        [] -> Right ()
        missing ->
            Left
                ( "Unknown scoring set(s): "
                    <> T.intercalate ", " missing
                    <> ". Configured on this collection: "
                    <> T.intercalate ", " configured
                )

{- | Filter the three scoring maps to keys in 'requested'. Validation has
already run in the caller.
-}
restrictScoringSets :: [Text] -> Value -> Value
restrictScoringSets requested =
    overObject (restrictAt scoringIndicatorsKey . restrictAt scoringUnitsKey . restrictAt scoringResultsKey)
  where
    scoringResultsKey = fromText "scoringResults"
    scoringUnitsKey = fromText "scoringUnits"
    scoringIndicatorsKey = fromText "scoringIndicators"
    keep k _ = Key.toText k `elem` requested
    restrict = overObject (KM.filterWithKey keep)
    restrictAt k = adjustKey k restrict

-- | Restrict one entry's @impacts@ subtree; entries without one pass through.
filterEntry :: [Text] -> Value -> Either Text Value
filterEntry requested =
    overObjectE $ \km ->
        case KM.lookup impactsKey km of
            Just impactsValue ->
                Right (KM.insert impactsKey (restrictScoringSets requested impactsValue) km)
            Nothing -> Right km

{- | Apply a 'Value' → 'Either' transformation to each element of an
Array; non-Array values pass through.
-}
overArrayE :: (Value -> Either Text Value) -> Value -> Either Text Value
overArrayE f = \case
    Array v -> Array . V.fromList <$> traverse f (V.toList v)
    Object km -> Right (Object km)
    String s -> Right (String s)
    Number n -> Right (Number n)
    Bool b -> Right (Bool b)
    Null -> Right Null

{- | Update one key of a 'KeyMap' via a transformation that may fail; if
the key is absent, the map is returned unchanged.
-}
traverseKey ::
    Key.Key ->
    (Value -> Either Text Value) ->
    KeyMap Value ->
    Either Text (KeyMap Value)
traverseKey k f km =
    case KM.lookup k km of
        Just v -> do
            v' <- f v
            pure (KM.insert k v' km)
        Nothing -> Right km

{- | Update one key of a 'KeyMap' via a pure transformation; if the key
is absent, the map is returned unchanged. (Aeson's 'KeyMap' has no
@adjust@ of its own.)
-}
adjustKey :: Key.Key -> (Value -> Value) -> KeyMap Value -> KeyMap Value
adjustKey k f km =
    case KM.lookup k km of
        Just v -> KM.insert k (f v) km
        Nothing -> km
