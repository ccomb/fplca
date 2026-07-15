{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module EcoSpold.Parser2 (streamParseActivityAndFlowsFromFile, normalizeCAS) where

import Amount (readAmount)
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID
import qualified Data.UUID.V5 as UUID5
import EcoSpold.Common (bsToDouble, bsToInt, bsToIntMaybe, bsToText, isElement, nonEmptyText)
import EcoSpold.Cutoff (applyCutoffStrategy)
import qualified Expr
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
    , idVariableName :: !Text -- variableName attribute (referencable from other formulas in the dataset)
    , idMathRel :: !Text -- mathematicalRelation attribute (formula defining the amount)
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
    , edVariableName :: !Text -- variableName attribute (referencable from other formulas in the dataset)
    , edMathRel :: !Text -- mathematicalRelation attribute (formula defining the amount)
    }
    deriving (Eq)

{- | Dataset-local formula metadata carried by an exchange: its own
@variableName@ (referencable from other formulas in the same dataset) and its
@mathematicalRelation@ (re-evaluated once the whole dataset is parsed, since
the variables it references may be declared after it in the file).
-}
data ExchangeFormula = ExchangeFormula
    { efVariableName :: !(Maybe Text)
    , efMathRel :: !(Maybe Text)
    }

-- | Attribute accumulator for the currently-open @\<parameter\>@ element.
data PendingParam = PendingParam
    { ppVariableName :: !Text
    , ppAmount :: !(Maybe Double)
    , ppMathRel :: !Text
    }

emptyPendingParam :: PendingParam
emptyPendingParam = PendingParam "" Nothing ""

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
    , psExchanges :: ![(Exchange, ExchangeFormula)]
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
    , psParams :: !(M.Map Text Double) -- <parameter> variableName → amount (amounts are pre-evaluated in the source)
    , psParamExprs :: !(M.Map Text Text) -- <parameter> variableName → mathematicalRelation (raw formula, for inspection)
    , psPendingParam :: !PendingParam -- attribute accumulator for the open <parameter>
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
        , psParams = M.empty
        , psParamExprs = M.empty
        , psPendingParam = emptyPendingParam
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

{- | ecospold2 activityType enum labels (v3 schema). Unknown codes yield an
explicit "Unknown (code N)" sentinel so a future spec extension or a
parser bug is visible to consumers, not silently empty.
-}
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
    n -> "Unknown (code " <> T.pack (show n) <> ")"

{- | ecospold2 specialActivityType enum labels (v3 schema). Same unknown-code
treatment as 'ecoSpoldActivityTypeLabel'.
-}
ecoSpoldSpecialActivityTypeLabel :: Int -> Text
ecoSpoldSpecialActivityTypeLabel = \case
    0 -> "Default"
    1 -> "Hard link"
    2 -> "Pre-aggregation"
    3 -> "Combined production with byproducts"
    4 -> "Combined production without byproducts"
    5 -> "Combined production"
    6 -> "Import activity"
    n -> "Unknown (code " <> T.pack (show n) <> ")"

-- ============================================================================
-- SAX state combinators (pure, allocation-neutral — inlined into the fold)
-- ============================================================================

-- | Concatenated, entity-decoded text accumulated since the element opened.
accumText :: ParseState -> Text
accumText = T.concat . reverse . map bsToText . psTextAccum

-- | Pop one element off the path stack.
popPath :: ParseState -> ParseState
popPath st = st{psPath = drop 1 (psPath st)}

-- | The common close-tag epilogue: pop the path and discard accumulated text.
popText :: ParseState -> ParseState
popText st = (popPath st){psTextAccum = []}

-- | Is the element at the given depth (0 = currently-open tag) named @name@?
pathAt :: Int -> BS.ByteString -> ParseState -> Bool
pathAt depth name st = case drop depth (psPath st) of
    (e : _) -> isElement e name
    [] -> False

{- | Transform the open exchange accumulator, leaving non-exchange contexts
untouched. The exhaustive match lives here so the call sites stay wildcard-free.
-}
mapExchange ::
    (IntermediateData -> IntermediateData) ->
    (ElementaryData -> ElementaryData) ->
    ElementContext ->
    ElementContext
mapExchange fi fe = \case
    InIntermediateExchange d -> InIntermediateExchange (fi d)
    InElementaryExchange d -> InElementaryExchange (fe d)
    InActivityName -> InActivityName
    InGeographyShortname -> InGeographyShortname
    InGeneralCommentText i -> InGeneralCommentText i
    Other -> Other

-- | Apply per-kind updates to the open exchange accumulator, then pop path+text.
onExchange ::
    (IntermediateData -> IntermediateData) ->
    (ElementaryData -> ElementaryData) ->
    ParseState ->
    ParseState
onExchange fi fe st = popText st{psContext = mapExchange fi fe (psContext st)}

-- | The currently-open intermediate exchange, if any.
currentIntermediate :: ParseState -> Maybe IntermediateData
currentIntermediate st = case psContext st of
    InIntermediateExchange d -> Just d
    InElementaryExchange _ -> Nothing
    InActivityName -> Nothing
    InGeographyShortname -> Nothing
    InGeneralCommentText _ -> Nothing
    Other -> Nothing

-- | The currently-open elementary exchange, if any.
currentElementary :: ParseState -> Maybe ElementaryData
currentElementary st = case psContext st of
    InElementaryExchange d -> Just d
    InIntermediateExchange _ -> Nothing
    InActivityName -> Nothing
    InGeographyShortname -> Nothing
    InGeneralCommentText _ -> Nothing
    Other -> Nothing

-- | Are we inside a @generalComment@ @\<text\>@ element?
inGeneralComment :: ParseState -> Bool
inGeneralComment st = case psContext st of
    InGeneralCommentText _ -> True
    InIntermediateExchange _ -> False
    InElementaryExchange _ -> False
    InActivityName -> False
    InGeographyShortname -> False
    Other -> False

-- | Build a 'Unit', substituting placeholders for a missing unit name.
mkUnit :: UUID -> Text -> Unit
mkUnit uuid name
    | T.null name = Unit uuid "UNKNOWN_UNIT" "?" ""
    | otherwise = Unit uuid name name ""

{- | Resolve in/out group: prefer the attribute value, fall back to the pending
value captured from the child @\<inputGroup\>@ / @\<outputGroup\>@ element.
-}
resolveGroups :: Text -> Text -> ParseState -> (Text, Text)
resolveGroups inG outG st =
    ( if T.null inG then psPendingInputGroup st else inG
    , if T.null outG then psPendingOutputGroup st else outG
    )

-- | Warning emitted (as a singleton, else empty) when an exchange has no unit name.
missingUnitWarning :: String -> Text -> Text -> [String]
missingUnitWarning kind flowId unitNm =
    [ "[WARNING] Missing unit name for "
        ++ kind
        ++ " exchange with flow ID: "
        ++ T.unpack flowId
        ++ " - using 'UNKNOWN_UNIT' placeholder"
    | T.null unitNm
    ]

-- | Parse a UUID, treating the empty string as the nil UUID (no warning).
parseUUIDOrNil :: Text -> (UUID, Maybe String)
parseUUIDOrNil t
    | T.null t = (UUID.nil, Nothing)
    | otherwise = parseUUID t

-- | Second argument when non-blank, otherwise the fallback.
nonBlankOr :: Text -> Text -> Text
nonBlankOr fallback t = if T.null t then fallback else t

addExchange :: Exchange -> ExchangeFormula -> ParseState -> ParseState
addExchange ex ef st = st{psExchanges = (ex, ef) : psExchanges st}

addTechFlow :: TechnosphereFlow -> ParseState -> ParseState
addTechFlow f st = st{psTechFlows = f : psTechFlows st}

addBioFlow :: BiosphereFlow -> ParseState -> ParseState
addBioFlow f st = st{psBioFlows = f : psBioFlows st}

addWasteFlow :: WasteFlow -> ParseState -> ParseState
addWasteFlow f st = st{psWasteFlows = f : psWasteFlows st}

{- | Common exchange-close bookkeeping: leave the exchange context, pop the
path/text, clear pending groups, record the unit and any warnings.
-}
finishExchange :: Unit -> [String] -> ParseState -> ParseState
finishExchange unit warns st =
    (popText st)
        { psContext = Other
        , psPendingInputGroup = ""
        , psPendingOutputGroup = ""
        , psUnits = unit : psUnits st
        , psWarnings = warns ++ psWarnings st
        }

-- | Replace an exchange's amount, whatever its variant.
setAmount :: Double -> Exchange -> Exchange
setAmount v ex = case ex of
    TechnosphereExchange{} -> ex{techAmount = v}
    BiosphereExchange{} -> ex{bioAmount = v}
    WasteExchange{} -> ex{waAmount = v}

{- | Re-evaluate exchange @mathematicalRelation@ formulas against the
dataset-local environment: the dataset's @\<parameter\>@ variables plus every
exchange's own @variableName@ bound to its stored amount. Stored amounts are
already evaluated in EcoSpold2 sources, so binding them directly needs no
fixpoint iteration.

A formula that evaluates replaces the stored amount (normally to the same
value — a divergence means the stored amount was stale and is reported). One
that doesn't evaluate — unknown variable, unsupported function, cross-dataset
reference — keeps the stored amount and yields a warning: never zero, never a
crash.
-}
evaluateFormulas :: M.Map Text Double -> [(Exchange, ExchangeFormula)] -> ([Exchange], [String])
evaluateFormulas params pairs = (map fst resolved, mapMaybe snd resolved)
  where
    -- Left-biased union: a <parameter> wins over an exchange variable of the same name.
    env = M.union params (M.fromList [(v, exchangeAmount ex) | (ex, ef) <- pairs, Just v <- [efVariableName ef]])
    resolved = map apply pairs
    apply (ex, ef) = case efMathRel ef of
        Nothing -> (ex, Nothing)
        Just rel -> case Expr.evaluate env (Expr.normalizeExpr '.' rel) of
            Right v -> (setAmount v ex, mismatchWarning rel v ex)
            Left _ ->
                ( ex
                , Just $
                    "[WARNING] Cannot evaluate mathematicalRelation \""
                        ++ T.unpack rel
                        ++ "\" for exchange flow "
                        ++ UUID.toString (exchangeFlowId ex)
                        ++ " - keeping stored amount "
                        ++ show (exchangeAmount ex)
                )
    mismatchWarning rel v ex
        | nearlyEqual v (exchangeAmount ex) = Nothing
        | otherwise =
            Just $
                "[WARNING] mathematicalRelation \""
                    ++ T.unpack rel
                    ++ "\" for exchange flow "
                    ++ UUID.toString (exchangeFlowId ex)
                    ++ " evaluates to "
                    ++ show v
                    ++ " but the dataset stores amount "
                    ++ show (exchangeAmount ex)
                    ++ " - using the formula result"
    nearlyEqual a b = abs (a - b) <= 1e-9 * max 1 (max (abs a) (abs b))

-- | Xeno SAX parser implementation
parseWithXeno :: BS.ByteString -> ProcessId -> Either String ((Activity, [TechnosphereFlow], [BiosphereFlow], [WasteFlow], [Unit]), [String])
parseWithXeno xmlContent processId = do
    finalState <- first show (X.fold openTag attribute endOpen text closeTag cdata initialParseState xmlContent)
    (result, formulaWarns) <- buildResult finalState processId
    pure (result, reverse (psWarnings finalState) ++ formulaWarns)
  where
    -- Open tag handler - update path and context
    openTag state tagName =
        let newPath = tagName : psPath state
            cleanState
                | isElement tagName "intermediateExchange" || isElement tagName "elementaryExchange" =
                    state{psPendingInputGroup = "", psPendingOutputGroup = ""}
                | isElement tagName "comment" =
                    state{psPendingCommentLang = ""}
                | isElement tagName "parameter" =
                    state{psPendingParam = emptyPendingParam}
                | otherwise = state
            newContext
                | isElement tagName "activityName" = InActivityName
                | isElement tagName "shortname" && any (isElement "geography") (psPath cleanState) = InGeographyShortname
                | isElement tagName "intermediateExchange" =
                    InIntermediateExchange (IntermediateData "" 0.0 "" "" "" "" "" "" M.empty Nothing M.empty "" "")
                | isElement tagName "elementaryExchange" =
                    InElementaryExchange (ElementaryData "" 0.0 "" "" "" "" "" [] [] M.empty Nothing Nothing "" "")
                | isElement tagName "text" && any (isElement "generalComment") (psPath cleanState) = InGeneralCommentText 0
                -- Classification elements: don't switch context. Handled via psTextAccum + psPendingClassSystem.
                -- Switching context here would destroy InIntermediateExchange when classifications appear inside exchanges.
                -- DON'T switch context for child elements (synonym, compartment, etc) - keep parent exchange context
                | otherwise = psContext cleanState
         in cleanState{psPath = newPath, psContext = newContext, psTextAccum = []}

    -- Attribute handler - extract attributes
    attribute state name value =
        let isInsideProperty = pathAt 0 "property" state
            -- xml:lang on the currently-open <comment>; remembered until closeTag.
            -- Attribute order is not significant for entity ref selection — we
            -- only need the lang at close-time.
            withLang st
                | pathAt 0 "comment" state && isElement name "xml:lang" = st{psPendingCommentLang = bsToText value}
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
                            | isElement name "variableName" && not isInsideProperty = idata{idVariableName = bsToText value}
                            | isElement name "mathematicalRelation" && not isInsideProperty = idata{idMathRel = bsToText value}
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
                            | isElement name "variableName" && not isInsideProperty = edata{edVariableName = bsToText value}
                            | isElement name "mathematicalRelation" && not isInsideProperty = edata{edMathRel = bsToText value}
                            | otherwise = edata
                     in withLang state{psContext = InElementaryExchange updated}
                InGeneralCommentText _ ->
                    let idx = if isElement name "index" then bsToInt value else 0
                     in withLang state{psContext = InGeneralCommentText idx}
                _ ->
                    -- Attributes on the <activity> opening tag carry the
                    -- ecospold2 activityType and specialActivityType enums;
                    -- attributes on a <parameter> carry its variableName,
                    -- pre-evaluated amount and mathematicalRelation formula.
                    let onActivity = pathAt 0 "activity" state
                        onParameter = pathAt 0 "parameter" state
                        pending = psPendingParam state
                        captured
                            | onActivity && isElement name "activityType" =
                                state{psActivityType = bsToIntMaybe value}
                            | onActivity && isElement name "specialActivityType" =
                                state{psSpecialActivityType = bsToIntMaybe value}
                            | onParameter && isElement name "variableName" =
                                state{psPendingParam = pending{ppVariableName = bsToText value}}
                            | onParameter && isElement name "amount" =
                                state{psPendingParam = pending{ppAmount = readAmount (bsToText value)}}
                            | onParameter && isElement name "mathematicalRelation" =
                                state{psPendingParam = pending{ppMathRel = bsToText value}}
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
            (popText state){psActivityName = Just (accumText state), psContext = Other}
        | isElement tagName "comment" =
            -- Capture <comment> text only when the immediate parent is the
            -- exchange itself, not a nested <property>. Property comments
            -- describe the property (e.g. "carbon content"), not the exchange.
            let txt = accumText state
                lang = psPendingCommentLang state
                st' =
                    onExchange
                        (\d -> if pathAt 1 "intermediateExchange" state then d{idComment = pickComment (idComment d) lang txt} else d)
                        (\d -> if pathAt 1 "elementaryExchange" state then d{edComment = pickComment (edComment d) lang txt} else d)
                        state
             in st'{psPendingCommentLang = ""}
        | isElement tagName "shortname" && psContext state == InGeographyShortname =
            (popText state){psLocation = Just (accumText state), psContext = Other}
        | isElement tagName "intermediateExchange" =
            case currentIntermediate state of
                Nothing -> popPath state
                Just idata ->
                    let (finalInputGroup, finalOutputGroup) = resolveGroups (idInputGroup idata) (idOutputGroup idata) state
                        isInput = not (T.null finalInputGroup)
                        isOutput = T.null finalInputGroup
                        -- Reference products are identified ONLY by outputGroup="0"; this holds for
                        -- normal production (positive amount) and waste treatment (negative amount).
                        -- Negative inputs (e.g. wastewater discharge) are never reference products.
                        isReferenceProduct = isOutput && finalOutputGroup == "0"
                        -- Pattern B: intermediateExchange tagged Waste via classification
                        -- (System='By-product classification', Value='Waste') — a waste output that
                        -- consumers treat via a treatment activity.
                        isWasteFlow = M.lookup "By-product classification" (idClassifications idata) == Just "Waste"
                        (flowUUID, flowWarn) = parseUUID (idFlowId idata)
                        (unitUUID, unitWarn) = parseUUID (idUnitId idata)
                        (linkUUID, linkWarn) = parseUUIDOrNil (idActivityLinkId idata)
                        warns =
                            catMaybes [flowWarn, unitWarn, linkWarn]
                                ++ missingUnitWarning "intermediate" (idFlowId idata) (idUnitName idata)
                        techRoleFor
                            | isReferenceProduct = ReferenceProduct
                            | isInput = Input
                            | otherwise = Coproduct
                        resolvedFlowName = nonBlankOr (idFlowId idata) (idFlowName idata)
                        unit = mkUnit unitUUID (idUnitName idata)
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
                        techFlow = TechnosphereFlow flowUUID resolvedFlowName unitUUID (idSynonyms idata) Nothing Nothing
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
                        wasteFlow = WasteFlow flowUUID resolvedFlowName unitUUID (idSynonyms idata) Nothing Nothing
                        -- A waste-classified flow normally routes to the waste axis. The one
                        -- exception is the reference flow of a waste-treatment / market-for-waste
                        -- activity: it is itself waste (negative amount, outputGroup="0") yet it
                        -- IS the reference product. Diverting it to the waste axis leaves the
                        -- activity with no reference product, so 'applyCutoffStrategy' rejects it
                        -- and the whole dataset is dropped — silently severing every input that
                        -- links into the treatment subsystem.
                        refOnWasteAxis = isWasteFlow && not isReferenceProduct
                        newRefUnit =
                            if isReferenceProduct && not (T.null (idUnitName idata))
                                then Just (idUnitName idata)
                                else psRefUnit state
                        formula = ExchangeFormula (nonEmptyText (idVariableName idata)) (nonEmptyText (idMathRel idata))
                        base = (finishExchange unit warns state){psRefUnit = newRefUnit}
                     in if refOnWasteAxis
                            then addExchange wasteExchange formula (addWasteFlow wasteFlow base)
                            else addExchange techExchange formula (addTechFlow techFlow base)
        | isElement tagName "elementaryExchange" =
            case currentElementary state of
                Nothing -> popPath state
                Just edata ->
                    let (finalInputGroup, finalOutputGroup) = resolveGroups (edInputGroup edata) (edOutputGroup edata) state
                        -- A missing compartment becomes 'Nothing', not an empty 'Compartment ""'
                        -- sentinel — the latter used to silently collide with method-side empty mediums.
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
                            -- Biosphere direction: prefer inputGroup/outputGroup, else fall back to the
                            -- compartment heuristic (natural-resource flows are extractions).
                        direction
                            | not (T.null finalInputGroup) = Resource
                            | not (T.null finalOutputGroup) = Emission
                            | otherwise = case edCompartments edata of
                                (comp : _) | T.toLower comp == "natural resource" -> Resource
                                _ -> Emission
                        -- Pattern A: compartment="inventory indicator" / subcompartment="waste".
                        -- Surfaced through the elementary axis but semantically technosphere waste —
                        -- route to WasteExchange instead of BiosphereExchange.
                        isInventoryIndicatorWaste = case (mCompName, subCompartment) of
                            (Just c, Just s) ->
                                T.toLower (T.strip c) == "inventory indicator"
                                    && T.toLower (T.strip s) == "waste"
                            _ -> False
                        (flowUUID, flowWarn) = parseUUID (edFlowId edata)
                        (unitUUID, unitWarn) = parseUUID (edUnitId edata)
                        warns =
                            catMaybes [flowWarn, unitWarn]
                                ++ missingUnitWarning "elementary" (edFlowId edata) (edUnitName edata)
                        resolvedFlowName = nonBlankOr (edFlowId edata) (edFlowName edata)
                        unit = mkUnit unitUUID (edUnitName edata)
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
                        bioFlow = BiosphereFlow flowUUID resolvedFlowName unitUUID (edSynonyms edata) (edCAS edata) Nothing compartment
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
                        wasteFlow = WasteFlow flowUUID resolvedFlowName unitUUID (edSynonyms edata) (edCAS edata) Nothing
                        formula = ExchangeFormula (nonEmptyText (edVariableName edata)) (nonEmptyText (edMathRel edata))
                        base = finishExchange unit warns state
                     in if isInventoryIndicatorWaste
                            then addExchange wasteExchange formula (addWasteFlow wasteFlow base)
                            else addExchange bioExchange formula (addBioFlow bioFlow base)
        | isElement tagName "text" =
            -- The generalComment <text> branch deliberately does NOT pop the path.
            if inGeneralComment state
                then
                    let txt = accumText state
                        withDesc = if T.null txt then state else state{psDescription = txt : psDescription state}
                     in withDesc{psContext = Other, psTextAccum = []}
                else popText state
        | isElement tagName "name" =
            let txt = accumText state
             in if pathAt 1 "property" state
                    then popText state
                    else onExchange (\d -> d{idFlowName = txt}) (\d -> d{edFlowName = txt}) state
        | isElement tagName "unitName" =
            let txt = accumText state
             in if pathAt 1 "property" state
                    then popText state
                    else onExchange (\d -> d{idUnitName = txt}) (\d -> d{edUnitName = txt}) state
        | isElement tagName "synonym" =
            let txt = T.strip (accumText state)
                ins m = if T.null txt then m else M.insertWith S.union "en" (S.singleton txt) m
             in onExchange (\d -> d{idSynonyms = ins (idSynonyms d)}) (\d -> d{edSynonyms = ins (edSynonyms d)}) state
        -- inputGroup / outputGroup: stash the pending value, keep the parent exchange context.
        | isElement tagName "inputGroup" =
            (popText state){psPendingInputGroup = T.strip (accumText state)}
        | isElement tagName "outputGroup" =
            (popText state){psPendingOutputGroup = T.strip (accumText state)}
        | isElement tagName "compartment" =
            let txt = T.strip (accumText state)
                add d = if T.null txt then d else d{edCompartments = txt : edCompartments d}
             in onExchange id add state
        | isElement tagName "subcompartment" =
            let txt = T.strip (accumText state)
                add d = if T.null txt then d else d{edSubcompartments = txt : edSubcompartments d}
             in onExchange id add state
        -- A <parameter> is referencable from formulas only through its
        -- variableName; its amount is pre-evaluated in the source, so an
        -- entry without either has nothing to contribute and is skipped.
        | isElement tagName "parameter" =
            let PendingParam var amt rel = psPendingParam state
                committed = case (nonEmptyText var, amt) of
                    (Just v, Just a) ->
                        state
                            { psParams = M.insert v a (psParams state)
                            , psParamExprs = maybe (psParamExprs state) (\r -> M.insert v r (psParamExprs state)) (nonEmptyText rel)
                            }
                    (_, _) -> state
             in popText committed{psPendingParam = emptyPendingParam}
        | isElement tagName "classificationSystem" =
            (popText state){psPendingClassSystem = T.strip (accumText state)}
        | isElement tagName "classificationValue" =
            let txt = T.strip (accumText state)
                sys = psPendingClassSystem state
             in popText $
                    if T.null sys || T.null txt
                        then state
                        else case currentIntermediate state of
                            -- Exchange-scoped classification (e.g. By-product → Waste).
                            Just d -> state{psContext = InIntermediateExchange d{idClassifications = M.insert sys txt (idClassifications d)}}
                            -- Otherwise an activity-level classification.
                            Nothing -> state{psClassifications = M.insert sys txt (psClassifications state)}
        | otherwise =
            popPath state

    -- CDATA handler - treat as text
    cdata = text

    -- Build final result from parse state
    buildResult :: ParseState -> ProcessId -> Either String ((Activity, [TechnosphereFlow], [BiosphereFlow], [WasteFlow], [Unit]), [String])
    buildResult st _pid =
        let name = fromMaybe "Unknown Activity" (psActivityName st)
            location = fromMaybe "GLO" (psLocation st)
            description = reverse (psDescription st) -- Reverse to get correct order
            refUnit = fromMaybe "UNKNOWN_UNIT" (psRefUnit st)
            nativeType = ecoSpoldNativeType (psActivityType st) (psSpecialActivityType st)
            (resolvedExchanges, formulaWarns) = evaluateFormulas (psParams st) (reverse (psExchanges st))
            -- Apply cutoff strategy to exchanges
            activity = Activity name description M.empty (psClassifications st) location refUnit resolvedExchanges (psParams st) (psParamExprs st) Nothing Nothing nativeType Nothing
            techs = reverse (psTechFlows st)
            bios = reverse (psBioFlows st)
            wastes = reverse (psWasteFlows st)
            units = reverse (psUnits st)
         in (,formulaWarns) . (,techs,bios,wastes,units) <$> applyCutoffStrategy activity

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
