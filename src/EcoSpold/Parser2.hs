{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module EcoSpold.Parser2 (streamParseActivityAndFlowsFromFile, normalizeCAS) where

import qualified Data.ByteString as BS
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID
import qualified Data.UUID.V5 as UUID5
import EcoSpold.Common (bsToDouble, bsToInt, bsToIntMaybe, bsToText, isElement)
import Progress (ProgressLevel (..), reportProgress)
import System.FilePath (takeBaseName)
import Types
import qualified Xeno.SAX as X

{- | Normalize CAS number by stripping leading zeros from first segment.
Ecoinvent zero-pads: "001309-36-0" → "1309-36-0". ILCD uses canonical form.
-}
normalizeCAS :: Text -> Text
normalizeCAS cas = case T.splitOn "-" cas of
    [a, b, c] ->
        let a' = T.dropWhile (== '0') a
         in (if T.null a' then "0" else a') <> "-" <> b <> "-" <> c
    _ -> T.strip cas

{- | Namespace UUID for generating deterministic UUIDs from invalid text
Using UUID v5 (SHA1-based) with a custom namespace for test data compatibility
-}
testDataNamespace :: UUID
testDataNamespace = UUID5.generateNamed UUID5.namespaceURL (BS.unpack $ TE.encodeUtf8 "acvengine.test")

{- | Helper to safely parse UUID from Text, generating deterministic UUID for invalid formats
This ensures test data with invalid UUIDs like "productX-uuid" get unique UUIDs
Returns (UUID, Maybe warning) to avoid unsafePerformIO in pure context
-}
parseUUID :: Text -> (UUID, Maybe String)
parseUUID txt = case UUID.fromText txt of
    Just uuid -> (uuid, Nothing)
    Nothing ->
        -- Generate deterministic UUID from the text using UUID v5
        -- This prevents deduplication issues where all invalid UUIDs would map to nil
        let generatedUUID = UUID5.generateNamed testDataNamespace (BS.unpack $ TE.encodeUtf8 txt)
            -- Only warn for non-empty invalid UUIDs (empty is expected for optional fields)
            warning =
                if T.null txt
                    then Nothing
                    else Just $ "Invalid UUID format: " ++ T.unpack txt ++ " - generated UUID: " ++ show generatedUUID
         in (generatedUUID, warning)

{- | Parse ProcessId from filename (no Database needed here)
Expects format: activity_uuid_product_uuid.spold
-}
parseProcessId :: Text -> Maybe ProcessId
parseProcessId filename = case T.splitOn "_" filename of
    [_, _]
        | not (T.null filename) ->
            -- During parsing we don't have ProcessId yet, just return a placeholder
            -- The actual ProcessId will be assigned during database construction
            Just 0 -- Temporary ProcessId, will be replaced during DB construction
    _ -> Nothing

-- ============================================================================
-- Xeno SAX Parser Implementation (8-15x faster than xml-conduit)
-- ============================================================================

-- | Element context tracker - what element are we currently parsing?
data ElementContext
    = InActivityName
    | InGeographyShortname
    | InIntermediateExchange !IntermediateData
    | InElementaryExchange !ElementaryData
    | InGeneralCommentText !Int -- Track index
    | Other
    deriving (Eq)

-- | Intermediate exchange accumulator
data IntermediateData = IntermediateData
    { idFlowId :: !Text
    , idAmount :: !Double
    , idUnitId :: !Text
    , idFlowName :: !Text
    , idUnitName :: !Text
    , idInputGroup :: !Text
    , idOutputGroup :: !Text
    , idActivityLinkId :: !Text
    , idSynonyms :: !(M.Map Text (S.Set Text))
    , idComment :: !(Maybe (Text, Text)) -- (xml:lang, comment text) — English wins
    , idClassifications :: !(M.Map Text Text) -- per-exchange classifications (e.g. By-product classification → Waste)
    }
    deriving (Eq)

-- | Elementary exchange accumulator
data ElementaryData = ElementaryData
    { edFlowId :: !Text
    , edAmount :: !Double
    , edUnitId :: !Text
    , edFlowName :: !Text
    , edUnitName :: !Text
    , edInputGroup :: !Text
    , edOutputGroup :: !Text
    , edCompartments :: ![Text]
    , edSubcompartments :: ![Text]
    , edSynonyms :: !(M.Map Text (S.Set Text))
    , edCAS :: !(Maybe Text)
    , edComment :: !(Maybe (Text, Text))
    }
    deriving (Eq)

{- | Update a comment slot with a newly seen `<comment xml:lang="…">` text.
Prefer English; otherwise keep the first non-empty entry. Empty / blank
incoming text never overwrites an existing slot.
-}
pickComment :: Maybe (Text, Text) -> Text -> Text -> Maybe (Text, Text)
pickComment existing lang txt =
    let s = T.strip txt
     in if T.null s
            then existing
            else case existing of
                Just ("en", _) -> existing
                _ | lang == "en" -> Just ("en", s)
                Nothing -> Just (lang, s)
                Just _ -> existing

-- | Parsing state accumulator for SAX parsing
data ParseState = ParseState
    { psActivityName :: !(Maybe Text)
    , psLocation :: !(Maybe Text)
    , psRefUnit :: !(Maybe Text)
    , psDescription :: ![Text]
    , psExchanges :: ![Exchange]
    , psTechFlows :: ![TechnosphereFlow]
    , psBioFlows :: ![BiosphereFlow]
    , psWasteFlows :: ![WasteFlow]
    , psUnits :: ![Unit]
    , psPath :: ![BS.ByteString] -- Element path stack
    , psContext :: !ElementContext
    , psTextAccum :: ![BS.ByteString] -- Accumulated text content
    , psPendingInputGroup :: !Text -- Pending inputGroup value from child element
    , psPendingOutputGroup :: !Text -- Pending outputGroup value from child element
    , psWarnings :: ![String] -- Accumulated warnings (emitted in IO after fold)
    , psClassifications :: !(M.Map Text Text) -- Classification system -> value
    , psPendingClassSystem :: !Text -- Current classification system name
    , psPendingCommentLang :: !Text -- xml:lang on the currently-open <comment>
    , psActivityType :: !(Maybe Int) -- ecospold2 <activity activityType="1..8"> attribute
    , psSpecialActivityType :: !(Maybe Int) -- ecospold2 <activity specialActivityType="…"> attribute
    }

-- | Initial parsing state
initialParseState :: ParseState
initialParseState =
    ParseState
        { psActivityName = Nothing
        , psLocation = Nothing
        , psRefUnit = Nothing
        , psDescription = []
        , psExchanges = []
        , psTechFlows = []
        , psBioFlows = []
        , psWasteFlows = []
        , psUnits = []
        , psPath = []
        , psContext = Other
        , psTextAccum = []
        , psPendingInputGroup = ""
        , psPendingOutputGroup = ""
        , psWarnings = []
        , psClassifications = M.empty
        , psPendingClassSystem = ""
        , psPendingCommentLang = ""
        , psActivityType = Nothing
        , psSpecialActivityType = Nothing
        }

{- | Build the source-native activity-type record from the ecospold2
@activityType@ and @specialActivityType@ attribute values, both verbatim
integers from the XML. Returns 'Nothing' when the primary @activityType@
attribute is absent (we never fabricate a value).

Labels are the ecoinvent v3 schema's documented strings. Unknown codes
keep the integer and yield an empty label rather than a guess.
-}
ecoSpoldNativeType :: Maybe Int -> Maybe Int -> Maybe NativeActivityType
ecoSpoldNativeType Nothing _ = Nothing
ecoSpoldNativeType (Just code) special =
    Just
        EcoSpoldActivityType
            { eatCode = code
            , eatLabel = ecoSpoldActivityTypeLabel code
            , eatSpecialCode = special
            , eatSpecialLabel = ecoSpoldSpecialActivityTypeLabel <$> special
            }

-- | ecospold2 activityType enum labels (v3 schema).
ecoSpoldActivityTypeLabel :: Int -> Text
ecoSpoldActivityTypeLabel = \case
    1 -> "Ordinary transforming activity"
    2 -> "Market activity"
    3 -> "IO activity"
    4 -> "Residual activity"
    5 -> "Production mix"
    6 -> "Import activity"
    7 -> "Correction activity"
    8 -> "Market group"
    _ -> ""

-- | ecospold2 specialActivityType enum labels (v3 schema).
ecoSpoldSpecialActivityTypeLabel :: Int -> Text
ecoSpoldSpecialActivityTypeLabel = \case
    0 -> "Default"
    1 -> "Hard link"
    2 -> "Pre-aggregation"
    3 -> "Combined production with byproducts"
    4 -> "Combined production without byproducts"
    5 -> "Combined production"
    6 -> "Import activity"
    _ -> ""

-- | Xeno SAX parser implementation
parseWithXeno :: BS.ByteString -> ProcessId -> Either String ((Activity, [TechnosphereFlow], [BiosphereFlow], [WasteFlow], [Unit]), [String])
parseWithXeno xmlContent processId =
    case X.fold openTag attribute endOpen text closeTag cdata initialParseState xmlContent of
        Left err -> Left (show err)
        Right finalState -> case buildResult finalState processId of
            Left err -> Left err
            Right result -> Right (result, reverse $ psWarnings finalState)
  where
    -- Open tag handler - update path and context
    openTag state tagName =
        let newPath = tagName : psPath state
            cleanState
                | isElement tagName "intermediateExchange" || isElement tagName "elementaryExchange" =
                    state{psPendingInputGroup = "", psPendingOutputGroup = ""}
                | isElement tagName "comment" =
                    state{psPendingCommentLang = ""}
                | otherwise = state
            newContext
                | isElement tagName "activityName" = InActivityName
                | isElement tagName "shortname" && any (isElement "geography") (psPath cleanState) = InGeographyShortname
                | isElement tagName "intermediateExchange" =
                    InIntermediateExchange (IntermediateData "" 0.0 "" "" "" "" "" "" M.empty Nothing M.empty)
                | isElement tagName "elementaryExchange" =
                    InElementaryExchange (ElementaryData "" 0.0 "" "" "" "" "" [] [] M.empty Nothing Nothing)
                | isElement tagName "text" && any (isElement "generalComment") (psPath cleanState) = InGeneralCommentText 0
                -- Classification elements: don't switch context. Handled via psTextAccum + psPendingClassSystem.
                -- Switching context here would destroy InIntermediateExchange when classifications appear inside exchanges.
                -- DON'T switch context for child elements (synonym, compartment, etc) - keep parent exchange context
                | otherwise = psContext cleanState
         in cleanState{psPath = newPath, psContext = newContext, psTextAccum = []}

    -- Attribute handler - extract attributes
    attribute state name value =
        let isInsideProperty = case psPath state of
                [] -> False
                (current : _) -> isElement current "property"
            isOnComment = case psPath state of
                [] -> False
                (current : _) -> isElement current "comment"
            -- xml:lang on the currently-open <comment>; remembered until closeTag.
            -- Attribute order is not significant for entity ref selection — we
            -- only need the lang at close-time.
            withLang st
                | isOnComment && isElement name "xml:lang" = st{psPendingCommentLang = bsToText value}
                | otherwise = st
         in case psContext state of
                InIntermediateExchange idata ->
                    let updated
                            | isElement name "intermediateExchangeId" = idata{idFlowId = bsToText value}
                            | isElement name "amount" && not isInsideProperty = idata{idAmount = bsToDouble value}
                            | isElement name "unitId" && not isInsideProperty = idata{idUnitId = bsToText value}
                            | isElement name "inputGroup" = idata{idInputGroup = bsToText value}
                            | isElement name "outputGroup" = idata{idOutputGroup = bsToText value}
                            | isElement name "activityLinkId" = idata{idActivityLinkId = bsToText value}
                            | otherwise = idata
                     in withLang state{psContext = InIntermediateExchange updated}
                InElementaryExchange edata ->
                    let updated
                            | isElement name "elementaryExchangeId" = edata{edFlowId = bsToText value}
                            | isElement name "amount" && not isInsideProperty = edata{edAmount = bsToDouble value}
                            | isElement name "unitId" && not isInsideProperty = edata{edUnitId = bsToText value}
                            | isElement name "inputGroup" = edata{edInputGroup = bsToText value}
                            | isElement name "outputGroup" = edata{edOutputGroup = bsToText value}
                            | isElement name "casNumber" = edata{edCAS = Just (normalizeCAS (bsToText value))}
                            | otherwise = edata
                     in withLang state{psContext = InElementaryExchange updated}
                InGeneralCommentText _ ->
                    let idx = if isElement name "index" then bsToInt value else 0
                     in withLang state{psContext = InGeneralCommentText idx}
                _ ->
                    -- Attributes on the <activity> opening tag carry the
                    -- ecospold2 activityType and specialActivityType enums.
                    let onActivity = case psPath state of
                            (current : _) -> isElement current "activity"
                            [] -> False
                        captured
                            | onActivity && isElement name "activityType" =
                                state{psActivityType = bsToIntMaybe value}
                            | onActivity && isElement name "specialActivityType" =
                                state{psSpecialActivityType = bsToIntMaybe value}
                            | otherwise = state
                     in withLang captured

    -- End of opening tag - no action needed for SAX
    endOpen state _tagName = state

    -- Text content handler - accumulate text
    text state content =
        let trimmed = BS.dropWhile (== 32) $ BS.dropWhileEnd (== 32) content -- Trim spaces
         in if BS.null trimmed
                then state
                else state{psTextAccum = trimmed : psTextAccum state}

    -- Close tag handler - finalize elements
    closeTag state tagName
        | isElement tagName "activityName" =
            let txt = T.concat $ reverse $ map bsToText (psTextAccum state)
             in state{psActivityName = Just txt, psContext = Other, psPath = drop 1 (psPath state), psTextAccum = []}
        | isElement tagName "comment" =
            -- Capture <comment> text only when the immediate parent is the
            -- exchange itself, not a nested <property>. Property comments
            -- describe the property (e.g. "carbon content"), not the exchange.
            let parent = case drop 1 (psPath state) of
                    (p : _) -> p
                    [] -> ""
                txt = T.concat $ reverse $ map bsToText (psTextAccum state)
                lang = psPendingCommentLang state
                popPath = state{psPath = drop 1 (psPath state), psTextAccum = [], psPendingCommentLang = ""}
             in case psContext state of
                    InIntermediateExchange idata
                        | isElement parent "intermediateExchange" ->
                            popPath{psContext = InIntermediateExchange idata{idComment = pickComment (idComment idata) lang txt}}
                    InElementaryExchange edata
                        | isElement parent "elementaryExchange" ->
                            popPath{psContext = InElementaryExchange edata{edComment = pickComment (edComment edata) lang txt}}
                    _ -> popPath
        | isElement tagName "shortname" && psContext state == InGeographyShortname =
            let txt = T.concat $ reverse $ map bsToText (psTextAccum state)
             in state{psLocation = Just txt, psContext = Other, psPath = drop 1 (psPath state), psTextAccum = []}
        | isElement tagName "intermediateExchange" =
            case psContext state of
                InIntermediateExchange idata ->
                    -- Use pending group values if attribute values are empty
                    let finalInputGroup = if T.null (idInputGroup idata) then psPendingInputGroup state else idInputGroup idata
                        finalOutputGroup = if T.null (idOutputGroup idata) then psPendingOutputGroup state else idOutputGroup idata
                        isInput = not $ T.null finalInputGroup
                        isOutput = T.null finalInputGroup
                        -- Reference flow identification:
                        -- Reference products are identified ONLY by outputGroup="0"
                        -- This works for both normal production (positive amount) and waste treatment (negative amount)
                        -- Negative inputs (like wastewater discharge) should NOT be considered reference products
                        -- outputGroup valid values: 0=reference product, 1-3=byproducts, 4=allocated byproduct, 5=recyclable
                        isReferenceProduct = isOutput && finalOutputGroup == "0"
                        -- Pattern B: intermediateExchange tagged as Waste via classification
                        -- (System='By-product classification', Value='Waste'). When set, the flow
                        -- represents a waste output that consumers treat via a treatment activity.
                        isWasteFlow = M.lookup "By-product classification" (idClassifications idata) == Just "Waste"
                        -- Parse UUIDs and collect warnings
                        (flowUUID, flowWarn) = parseUUID (idFlowId idata)
                        (unitUUID, unitWarn) = parseUUID (idUnitId idata)
                        (linkUUID, linkWarn) =
                            if T.null (idActivityLinkId idata)
                                then (UUID.nil, Nothing)
                                else parseUUID (idActivityLinkId idata)
                        uuidWarnings = catMaybes [flowWarn, unitWarn, linkWarn]
                        techRoleFor
                            | isReferenceProduct = ReferenceProduct
                            | isInput = Input
                            | otherwise = Coproduct
                        resolvedFlowName = if T.null (idFlowName idata) then idFlowId idata else idFlowName idata
                        techExchange =
                            TechnosphereExchange
                                { techFlowId = flowUUID
                                , techAmount = idAmount idata
                                , techUnitId = unitUUID
                                , techRole = techRoleFor
                                , techActivityLinkId = linkUUID
                                , techProcessLinkId = Nothing
                                , techLocation = "" -- EcoSpold2: no per-exchange location
                                , techComment = snd <$> idComment idata
                                , techPedigree = Nothing
                                }
                        techFlow =
                            TechnosphereFlow
                                flowUUID
                                resolvedFlowName
                                unitUUID
                                (idSynonyms idata)
                                Nothing -- CAS
                                Nothing -- substanceId
                        wasteExchange =
                            WasteExchange
                                { waFlowId = flowUUID
                                , waAmount = idAmount idata
                                , waUnitId = unitUUID
                                , waIsInput = isInput
                                , waActivityLinkId = linkUUID
                                , waProcessLinkId = Nothing
                                , waLocation = ""
                                , waComment = snd <$> idComment idata
                                , waPedigree = Nothing
                                }
                        wasteFlow =
                            WasteFlow
                                flowUUID
                                resolvedFlowName
                                unitUUID
                                (idSynonyms idata)
                                Nothing -- CAS
                                Nothing -- substanceId
                        unitNameWarning =
                            [ "[WARNING] Missing unit name for intermediate exchange with flow ID: "
                                ++ T.unpack (idFlowId idata)
                                ++ " - using 'UNKNOWN_UNIT' placeholder"
                            | T.null (idUnitName idata)
                            ]
                        unit =
                            Unit
                                unitUUID
                                (if T.null (idUnitName idata) then "UNKNOWN_UNIT" else idUnitName idata)
                                (if T.null (idUnitName idata) then "?" else idUnitName idata)
                                ""
                        -- Reference product unit: only meaningful when the flow stays in the technosphere
                        -- (waste outputs are never the reference product of a producing process).
                        newRefUnit =
                            if isReferenceProduct && not isWasteFlow && not (T.null (idUnitName idata))
                                then Just (idUnitName idata)
                                else psRefUnit state
                        baseState =
                            state
                                { psContext = Other
                                , psPath = drop 1 (psPath state)
                                , psTextAccum = []
                                , psPendingInputGroup = ""
                                , psPendingOutputGroup = ""
                                , psRefUnit = newRefUnit
                                , psUnits = unit : psUnits state
                                , psWarnings = uuidWarnings ++ unitNameWarning ++ psWarnings state
                                }
                     in if isWasteFlow
                            then
                                baseState
                                    { psExchanges = wasteExchange : psExchanges state
                                    , psWasteFlows = wasteFlow : psWasteFlows state
                                    }
                            else
                                baseState
                                    { psExchanges = techExchange : psExchanges state
                                    , psTechFlows = techFlow : psTechFlows state
                                    }
                _ -> state{psPath = drop 1 (psPath state)}
        | isElement tagName "elementaryExchange" =
            case psContext state of
                InElementaryExchange edata ->
                    -- Use pending group values if attribute values are empty
                    let finalInputGroup = if T.null (edInputGroup edata) then psPendingInputGroup state else edInputGroup edata
                        finalOutputGroup = if T.null (edOutputGroup edata) then psPendingOutputGroup state else edOutputGroup edata
                        -- A missing compartment becomes 'Nothing', not an empty
                        -- 'Compartment ""' sentinel — the latter used to silently
                        -- collide with method-side empty mediums.
                        mCompName = case edCompartments edata of
                            (c : _) | not (T.null c) -> Just c
                            _ -> Nothing
                        subCompartment = case edSubcompartments edata of
                            (s : _) | not (T.null s) -> Just s
                            _ -> Nothing
                        compartment = case (mCompName, subCompartment) of
                            (Nothing, Nothing) -> Nothing
                            (Just c, sc) -> Just (Compartment c sc)
                            (Nothing, Just _) -> Nothing -- sub without medium is meaningless; drop
                            -- Determine the biosphere direction.
                            -- Primary: use inputGroup/outputGroup if present.
                            -- Fallback: compartment heuristic — natural-resource flows are extractions.
                        direction
                            | not (T.null finalInputGroup) = Resource
                            | not (T.null finalOutputGroup) = Emission
                            | otherwise = case edCompartments edata of
                                (comp : _) | T.toLower comp == "natural resource" -> Resource
                                _ -> Emission
                        -- Pattern A: elementaryExchange with compartment="inventory indicator"
                        -- subcompartment="waste". Waste outputs surfaced through the
                        -- elementary axis but semantically technosphere waste — route them
                        -- to WasteExchange instead of BiosphereExchange.
                        isInventoryIndicatorWaste = case (mCompName, subCompartment) of
                            (Just c, Just s) ->
                                T.toLower (T.strip c) == "inventory indicator"
                                    && T.toLower (T.strip s) == "waste"
                            _ -> False
                        -- Parse UUIDs and collect warnings
                        (flowUUID, flowWarn) = parseUUID (edFlowId edata)
                        (unitUUID, unitWarn) = parseUUID (edUnitId edata)
                        uuidWarnings = catMaybes [flowWarn, unitWarn]
                        resolvedFlowName = if T.null (edFlowName edata) then edFlowId edata else edFlowName edata
                        bioExchange =
                            BiosphereExchange
                                { bioFlowId = flowUUID
                                , bioAmount = edAmount edata
                                , bioUnitId = unitUUID
                                , bioDirection = direction
                                , bioLocation = "" -- EcoSpold2: no per-exchange location
                                , bioComment = snd <$> edComment edata
                                , bioPedigree = Nothing
                                }
                        bioFlow =
                            BiosphereFlow
                                flowUUID
                                resolvedFlowName
                                unitUUID
                                (edSynonyms edata)
                                (edCAS edata)
                                Nothing -- substanceId - to be filled later
                                compartment
                        wasteExchange =
                            WasteExchange
                                { waFlowId = flowUUID
                                , waAmount = edAmount edata
                                , waUnitId = unitUUID
                                , waIsInput = not (T.null finalInputGroup)
                                , waActivityLinkId = UUID.nil
                                , waProcessLinkId = Nothing
                                , waLocation = ""
                                , waComment = snd <$> edComment edata
                                , waPedigree = Nothing
                                }
                        wasteFlow =
                            WasteFlow
                                flowUUID
                                resolvedFlowName
                                unitUUID
                                (edSynonyms edata)
                                (edCAS edata)
                                Nothing -- substanceId
                        unitNameWarning =
                            [ "[WARNING] Missing unit name for elementary exchange with flow ID: "
                                ++ T.unpack (edFlowId edata)
                                ++ " - using 'UNKNOWN_UNIT' placeholder"
                            | T.null (edUnitName edata)
                            ]
                        unit =
                            Unit
                                unitUUID
                                (if T.null (edUnitName edata) then "UNKNOWN_UNIT" else edUnitName edata)
                                (if T.null (edUnitName edata) then "?" else edUnitName edata)
                                ""
                        baseState =
                            state
                                { psContext = Other
                                , psPath = drop 1 (psPath state)
                                , psTextAccum = []
                                , psPendingInputGroup = ""
                                , psPendingOutputGroup = ""
                                , psUnits = unit : psUnits state
                                , psWarnings = uuidWarnings ++ unitNameWarning ++ psWarnings state
                                }
                     in if isInventoryIndicatorWaste
                            then
                                baseState
                                    { psExchanges = wasteExchange : psExchanges state
                                    , psWasteFlows = wasteFlow : psWasteFlows state
                                    }
                            else
                                baseState
                                    { psExchanges = bioExchange : psExchanges state
                                    , psBioFlows = bioFlow : psBioFlows state
                                    }
                _ -> state{psPath = drop 1 (psPath state)}
        | isElement tagName "text" =
            case psContext state of
                InGeneralCommentText _idx ->
                    let txt = T.concat $ reverse $ map bsToText (psTextAccum state)
                     in -- Store as (index, text) pair for later sorting
                        if T.null txt
                            then state{psContext = Other, psTextAccum = []}
                            else state{psDescription = txt : psDescription state, psContext = Other, psTextAccum = []}
                _ -> state{psPath = drop 1 (psPath state), psTextAccum = []}
        | isElement tagName "name" =
            let txt = T.concat $ reverse $ map bsToText (psTextAccum state)
                isInsideProperty = case psPath state of
                    (_ : parent : _) -> isElement parent "property"
                    _ -> False
             in case psContext state of
                    InIntermediateExchange idata
                        | not isInsideProperty ->
                            state{psContext = InIntermediateExchange idata{idFlowName = txt}, psPath = drop 1 (psPath state), psTextAccum = []}
                    InElementaryExchange edata
                        | not isInsideProperty ->
                            state{psContext = InElementaryExchange edata{edFlowName = txt}, psPath = drop 1 (psPath state), psTextAccum = []}
                    _ -> state{psPath = drop 1 (psPath state), psTextAccum = []}
        | isElement tagName "unitName" =
            let txt = T.concat $ reverse $ map bsToText (psTextAccum state)
                isInsideProperty = case psPath state of
                    (_ : parent : _) -> isElement parent "property"
                    _ -> False
             in case psContext state of
                    InIntermediateExchange idata
                        | not isInsideProperty ->
                            state{psContext = InIntermediateExchange idata{idUnitName = txt}, psPath = drop 1 (psPath state), psTextAccum = []}
                    InElementaryExchange edata
                        | not isInsideProperty ->
                            state{psContext = InElementaryExchange edata{edUnitName = txt}, psPath = drop 1 (psPath state), psTextAccum = []}
                    _ -> state{psPath = drop 1 (psPath state), psTextAccum = []}
        | isElement tagName "synonym" =
            let txt = T.strip $ T.concat $ reverse $ map bsToText (psTextAccum state)
             in case psContext state of
                    InIntermediateExchange idata
                        | not (T.null txt) ->
                            let syns = M.insertWith S.union "en" (S.singleton txt) (idSynonyms idata)
                             in state{psContext = InIntermediateExchange idata{idSynonyms = syns}, psPath = drop 1 (psPath state), psTextAccum = []}
                    InElementaryExchange edata
                        | not (T.null txt) ->
                            let syns = M.insertWith S.union "en" (S.singleton txt) (edSynonyms edata)
                             in state{psContext = InElementaryExchange edata{edSynonyms = syns}, psPath = drop 1 (psPath state), psTextAccum = []}
                    _ -> state{psPath = drop 1 (psPath state), psTextAccum = []}
        | isElement tagName "inputGroup" =
            let txt = T.strip $ T.concat $ reverse $ map bsToText (psTextAccum state)
             in -- DON'T change psContext - preserve the parent exchange context
                state{psPendingInputGroup = txt, psPath = drop 1 (psPath state), psTextAccum = []}
        | isElement tagName "outputGroup" =
            let txt = T.strip $ T.concat $ reverse $ map bsToText (psTextAccum state)
             in -- DON'T change psContext - preserve the parent exchange context
                state{psPendingOutputGroup = txt, psPath = drop 1 (psPath state), psTextAccum = []}
        | isElement tagName "compartment" =
            let txt = T.strip $ T.concat $ reverse $ map bsToText (psTextAccum state)
             in case psContext state of
                    InElementaryExchange edata
                        | not (T.null txt) ->
                            state{psContext = InElementaryExchange edata{edCompartments = txt : edCompartments edata}, psPath = drop 1 (psPath state), psTextAccum = []}
                    _ ->
                        state{psPath = drop 1 (psPath state), psTextAccum = []}
        | isElement tagName "subcompartment" =
            let txt = T.strip $ T.concat $ reverse $ map bsToText (psTextAccum state)
             in case psContext state of
                    InElementaryExchange edata
                        | not (T.null txt) ->
                            state{psContext = InElementaryExchange edata{edSubcompartments = txt : edSubcompartments edata}, psPath = drop 1 (psPath state), psTextAccum = []}
                    _ ->
                        state{psPath = drop 1 (psPath state), psTextAccum = []}
        | isElement tagName "classificationSystem" =
            let txt = T.strip $ T.concat $ reverse $ map bsToText (psTextAccum state)
             in state{psPendingClassSystem = txt, psPath = drop 1 (psPath state), psTextAccum = []}
        | isElement tagName "classificationValue" =
            let txt = T.strip $ T.concat $ reverse $ map bsToText (psTextAccum state)
                sys = psPendingClassSystem state
                emptyPair = T.null sys || T.null txt
             in case psContext state of
                    InIntermediateExchange idata
                        | not emptyPair ->
                            state
                                { psContext =
                                    InIntermediateExchange
                                        idata{idClassifications = M.insert sys txt (idClassifications idata)}
                                , psPath = drop 1 (psPath state)
                                , psTextAccum = []
                                }
                    _ ->
                        state
                            { psClassifications =
                                if emptyPair
                                    then psClassifications state
                                    else M.insert sys txt (psClassifications state)
                            , psPath = drop 1 (psPath state)
                            , psTextAccum = []
                            }
        | otherwise =
            state{psPath = drop 1 (psPath state)}

    -- CDATA handler - treat as text
    cdata = text

    -- Build final result from parse state
    buildResult :: ParseState -> ProcessId -> Either String (Activity, [TechnosphereFlow], [BiosphereFlow], [WasteFlow], [Unit])
    buildResult st _pid =
        let name = fromMaybe "Unknown Activity" (psActivityName st)
            location = fromMaybe "GLO" (psLocation st)
            description = reverse (psDescription st) -- Reverse to get correct order
            refUnit = fromMaybe "UNKNOWN_UNIT" (psRefUnit st)
            nativeType = ecoSpoldNativeType (psActivityType st) (psSpecialActivityType st)
            -- Apply cutoff strategy to exchanges
            activity = Activity name description M.empty (psClassifications st) location refUnit (reverse $ psExchanges st) M.empty M.empty Nothing Nothing nativeType
            techs = reverse (psTechFlows st)
            bios = reverse (psBioFlows st)
            wastes = reverse (psWasteFlows st)
            units = reverse (psUnits st)
         in case applyCutoffStrategy activity of
                Right act -> Right (act, techs, bios, wastes, units)
                Left err -> Left err

-- | Parse EcoSpold file using Xeno SAX parser
streamParseActivityAndFlowsFromFile :: FilePath -> IO (Either String (Activity, [TechnosphereFlow], [BiosphereFlow], [WasteFlow], [Unit]))
streamParseActivityAndFlowsFromFile path = do
    !xmlContent <- BS.readFile path
    let filenameBase = T.pack $ takeBaseName path
    case EcoSpold.Parser2.parseProcessId filenameBase of
        Nothing -> return $ Left $ "Invalid filename format for ProcessId: " ++ path
        Just pid -> case parseWithXeno xmlContent pid of
            Left err -> return $ Left err
            Right (result, warnings) -> do
                mapM_ (reportProgress Warning) warnings
                return $ Right result

{- | Apply cut-off strategy
1. Remove zero-amount production exchanges (co-products)
2. Assign single non-zero product as reference product
3. Ensure single-output process structure
4. VALIDATION: Fail if no reference product can be established
-}
applyCutoffStrategy :: Activity -> Either String Activity
applyCutoffStrategy activity =
    let filteredExchanges = removeZeroAmountCoproducts (exchanges activity)
        updatedActivity = activity{exchanges = filteredExchanges}
        finalActivity =
            if hasReferenceProduct updatedActivity
                then updatedActivity
                else assignSingleProductAsReference updatedActivity
     in if hasReferenceProduct finalActivity
            then Right finalActivity
            else Left $ "Activity has no reference product: " ++ T.unpack (activityName activity)

-- | Check if activity has any reference product
hasReferenceProduct :: Activity -> Bool
hasReferenceProduct activity = any exchangeIsReference (exchanges activity)

-- | Remove production exchanges with zero amounts
removeZeroAmountCoproducts :: [Exchange] -> [Exchange]
removeZeroAmountCoproducts = filter keepExchange
  where
    keepExchange TechnosphereExchange{techRole = ReferenceProduct} = True
    keepExchange TechnosphereExchange{techRole = ReferenceInput} = True
    keepExchange TechnosphereExchange{techRole = Input} = True
    keepExchange TechnosphereExchange{techRole = Coproduct, techAmount = amount} = amount /= 0.0
    keepExchange BiosphereExchange{} = True
    keepExchange WasteExchange{} = True

-- | Assign single product as reference product
assignSingleProductAsReference :: Activity -> Activity
assignSingleProductAsReference activity =
    let productionExchanges = [ex | ex <- exchanges activity, isProductionExchange ex]
        nonZeroProduction = [ex | ex <- productionExchanges, exchangeAmount ex /= 0.0]
     in case nonZeroProduction of
            [singleProduct] ->
                -- Update the single product to be reference product
                let updatedExchanges = map (updateReferenceProduct singleProduct) (exchanges activity)
                 in activity{exchanges = updatedExchanges}
            [] -> activity -- No production exchanges, leave as-is
            _ -> activity -- Multiple production exchanges, leave as-is (shouldn't happen after cutoff)

-- | Check if exchange is production exchange (technosphere output)
isProductionExchange :: Exchange -> Bool
isProductionExchange TechnosphereExchange{techRole = ReferenceProduct} = True
isProductionExchange TechnosphereExchange{techRole = Coproduct} = True
isProductionExchange TechnosphereExchange{techRole = Input} = False
isProductionExchange TechnosphereExchange{techRole = ReferenceInput} = False
isProductionExchange BiosphereExchange{} = False
isProductionExchange WasteExchange{} = False

-- | Update reference product flag for the specified exchange
updateReferenceProduct :: Exchange -> Exchange -> Exchange
updateReferenceProduct target current
    | exchangeFlowId target == exchangeFlowId current = markAsReference current
    | otherwise = unmarkAsReference current

-- | Promote a production exchange to reference product
markAsReference :: Exchange -> Exchange
markAsReference ex@TechnosphereExchange{} = ex{techRole = ReferenceProduct}
markAsReference ex@BiosphereExchange{} = ex
markAsReference ex@WasteExchange{} = ex

-- | Demote a reference role back to non-reference (preserving input/output direction)
unmarkAsReference :: Exchange -> Exchange
unmarkAsReference ex@TechnosphereExchange{techRole = role} = ex{techRole = demote role}
  where
    demote ReferenceProduct = Coproduct
    demote ReferenceInput = Input
    demote Coproduct = Coproduct
    demote Input = Input
unmarkAsReference ex@BiosphereExchange{} = ex
unmarkAsReference ex@WasteExchange{} = ex
