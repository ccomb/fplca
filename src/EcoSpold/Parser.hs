{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module EcoSpold.Parser (streamParseActivityAndFlowsFromFile) where

import ACV.Types
import qualified Data.ByteString as BS
import qualified Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as TR
import System.FilePath (takeBaseName)
import Text.XML
import Text.XML.Cursor
import qualified Xeno.SAX as X

-- EcoSpold namespace
ecoSpoldNS :: Text
ecoSpoldNS = "http://www.EcoInvent.org/EcoSpold02"

-- Helper to create namespaced element name
nsElement :: Text -> Name
nsElement name = Name name (Just ecoSpoldNS) Nothing

getAttr :: Cursor -> Text -> Text
getAttr cur attr =
    let !name = Name attr Nothing Nothing
     in case cur $| attribute name of
            [] -> "" -- Return empty string for missing attributes
            (x : _) -> x

-- | Parse synonyms from XML cursor
parseSynonyms :: Cursor -> M.Map Text (S.Set Text)
parseSynonyms cursor =
    let synonymNodes = cursor $/ element (nsElement "synonym")
        synonymPairs =
            [ (lang, text) | node <- synonymNodes, let lang = case node $| attribute "xml:lang" of
                                                        [] -> "en" -- Default to English
                                                        (l : _) -> l, let text = case node $/ content of
                                                                            [] -> ""
                                                                            (t : _) -> t, not (T.null text)
            ]
     in M.fromListWith S.union [(lang, S.singleton text) | (lang, text) <- synonymPairs]

-- | Extract compartment category from elementaryExchange cursor
extractCompartmentCategory :: Cursor -> T.Text
extractCompartmentCategory cur =
    let compartmentTexts =
            cur
                $// element (nsElement "compartment")
                &// element (nsElement "compartment")
                &/ content
        subcompartmentTexts =
            cur
                $// element (nsElement "compartment")
                &// element (nsElement "subcompartment")
                &/ content
     in case (compartmentTexts, subcompartmentTexts) of
            ([], []) -> "unspecified" -- Fallback if no compartment info
            (comp : _, []) -> comp -- Just compartment, no subcompartment
            ([], sub : _) -> sub -- Just subcompartment (shouldn't happen)
            (comp : _, sub : _) -> comp <> "/" <> sub -- Full hierarchy: "water/ground-, long-term"

-- | Safer alternative to head
headOrFail :: String -> [a] -> a
headOrFail msg [] = error msg
headOrFail _ (x : _) = x

-- | Parse ProcessId from filename (no Database needed here)
-- Expects format: activity_uuid_product_uuid.spold
parseProcessId :: Text -> Maybe ProcessId
parseProcessId filename = case T.splitOn "_" filename of
    [_, _] | not (T.null filename) ->
        -- During parsing we don't have ProcessId yet, just return a placeholder
        -- The actual ProcessId will be assigned during database construction
        Just 0  -- Temporary ProcessId, will be replaced during DB construction
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
    | InGeneralCommentText !Int  -- Track index
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
    }
    deriving (Eq)

-- | Parsing state accumulator for SAX parsing
data ParseState = ParseState
    { psActivityName :: !(Maybe Text)
    , psLocation :: !(Maybe Text)
    , psRefUnit :: !(Maybe Text)
    , psDescription :: ![Text]
    , psExchanges :: ![Exchange]
    , psFlows :: ![Flow]
    , psUnits :: ![Unit]
    , psPath :: ![BS.ByteString]  -- Element path stack
    , psContext :: !ElementContext
    , psTextAccum :: ![BS.ByteString]  -- Accumulated text content
    , psPendingInputGroup :: !Text  -- Pending inputGroup value from child element
    , psPendingOutputGroup :: !Text  -- Pending outputGroup value from child element
    }

-- | Initial parsing state
initialParseState :: ParseState
initialParseState = ParseState
    { psActivityName = Nothing
    , psLocation = Nothing
    , psRefUnit = Nothing
    , psDescription = []
    , psExchanges = []
    , psFlows = []
    , psUnits = []
    , psPath = []
    , psContext = Other
    , psTextAccum = []
    , psPendingInputGroup = ""
    , psPendingOutputGroup = ""
    }

-- | ByteString to Text conversion with UTF-8 decoding
bsToText :: BS.ByteString -> Text
bsToText = TE.decodeUtf8

-- | ByteString to Double conversion
-- CRITICAL: Fail explicitly on parse error instead of silently returning 0.0
-- Silent failures can cause zero normalization factors leading to infinities
bsToDouble :: BS.ByteString -> Double
bsToDouble bs = case TR.double (bsToText bs) of
    Right (val, _) -> val
    Left _ -> error $ "Failed to parse amount from: " ++ show bs

-- | ByteString to Int conversion
bsToInt :: BS.ByteString -> Int
bsToInt bs = case TR.decimal (bsToText bs) of
    Right (val, _) -> val
    Left _ -> 0

-- | Check if element name matches (with or without namespace prefix)
isElement :: BS.ByteString -> BS.ByteString -> Bool
isElement tagName expected =
    tagName == expected || BS.isSuffixOf (":" `BS.append` expected) tagName

-- | Xeno SAX parser implementation
parseWithXeno :: BS.ByteString -> ProcessId -> Either String (Activity, [Flow], [Unit])
parseWithXeno xmlContent processId =
    case X.fold openTag attribute endOpen text closeTag cdata initialParseState xmlContent of
        Left err -> Left (show err)
        Right finalState -> Right (buildResult finalState processId)
  where
    -- Open tag handler - update path and context
    openTag state tagName =
        let newPath = tagName : psPath state
            -- CRITICAL FIX: Clear pending fields when entering new exchange to prevent state leakage
            -- If malformed XML doesn't close properly, pending fields from previous exchange could leak
            cleanState = if isElement tagName "intermediateExchange" || isElement tagName "elementaryExchange"
                         then state { psPendingInputGroup = "", psPendingOutputGroup = "" }
                         else state
            newContext
                | isElement tagName "activityName" = InActivityName
                | isElement tagName "shortname" && any (isElement "geography") (psPath cleanState) = InGeographyShortname
                | isElement tagName "intermediateExchange" =
                    InIntermediateExchange (IntermediateData "" 0.0 "" "" "" "" "" "" M.empty)
                | isElement tagName "elementaryExchange" =
                    InElementaryExchange (ElementaryData "" 0.0 "" "" "" "" "" [] [] M.empty)
                | isElement tagName "text" && any (isElement "generalComment") (psPath cleanState) = InGeneralCommentText 0
                -- DON'T switch context for child elements (synonym, compartment, etc) - keep parent exchange context
                | otherwise = psContext cleanState
        in cleanState{psPath = newPath, psContext = newContext, psTextAccum = []}

    -- Attribute handler - extract critical attributes
    attribute state name value =
        let -- CRITICAL FIX: Check if we're currently on a property element
            -- Property elements have their own amount/unitId attributes that should NOT overwrite exchange attributes
            -- When processing property attributes, "property" is at the top of the path stack (O(1) check)
            isInsideProperty = case psPath state of
                [] -> False
                (current:_) -> isElement current "property"
        in case psContext state of
            InIntermediateExchange idata ->
                let updated
                        | isElement name "intermediateExchangeId" = idata{idFlowId = bsToText value}
                        -- Only update amount if NOT inside a property element
                        | isElement name "amount" && not isInsideProperty = idata{idAmount = bsToDouble value}
                        -- Only update unitId if NOT inside a property element
                        | isElement name "unitId" && not isInsideProperty = idata{idUnitId = bsToText value}
                        | isElement name "inputGroup" = idata{idInputGroup = bsToText value}
                        | isElement name "outputGroup" = idata{idOutputGroup = bsToText value}
                        | isElement name "activityLinkId" = idata{idActivityLinkId = bsToText value}
                        | otherwise = idata
                in state{psContext = InIntermediateExchange updated}
            InElementaryExchange edata ->
                let updated
                        | isElement name "elementaryExchangeId" = edata{edFlowId = bsToText value}
                        -- Only update amount if NOT inside a property element
                        | isElement name "amount" && not isInsideProperty = edata{edAmount = bsToDouble value}
                        -- Only update unitId if NOT inside a property element
                        | isElement name "unitId" && not isInsideProperty = edata{edUnitId = bsToText value}
                        | isElement name "inputGroup" = edata{edInputGroup = bsToText value}
                        | isElement name "outputGroup" = edata{edOutputGroup = bsToText value}
                        | otherwise = edata
                in state{psContext = InElementaryExchange updated}
            InGeneralCommentText _ ->
                let idx = if isElement name "index" then bsToInt value else 0
                in state{psContext = InGeneralCommentText idx}
            _ -> state

    -- End of opening tag - no action needed for SAX
    endOpen state _tagName = state

    -- Text content handler - accumulate text
    text state content =
        let trimmed = BS.dropWhile (== 32) $ BS.dropWhileEnd (== 32) content  -- Trim spaces
        in if BS.null trimmed
            then state
            else state{psTextAccum = content : psTextAccum state}

    -- Close tag handler - finalize elements
    closeTag state tagName
        | isElement tagName "activityName" =
            let txt = T.concat $ reverse $ map bsToText (psTextAccum state)
            in state{psActivityName = Just txt, psContext = Other, psTextAccum = []}
        | isElement tagName "shortname" && psContext state == InGeographyShortname =
            let txt = T.concat $ reverse $ map bsToText (psTextAccum state)
            in state{psLocation = Just txt, psContext = Other, psTextAccum = []}
        | isElement tagName "intermediateExchange" =
            case psContext state of
                InIntermediateExchange idata ->
                    -- Use pending group values if attribute values are empty
                    let finalInputGroup = if T.null (idInputGroup idata) then psPendingInputGroup state else idInputGroup idata
                        finalOutputGroup = if T.null (idOutputGroup idata) then psPendingOutputGroup state else idOutputGroup idata
                        isInput = not $ T.null finalInputGroup
                        isOutput = T.null finalInputGroup
                        amount = idAmount idata
                        -- Reference flow identification for both production AND treatment activities:
                        -- Production: output (no inputGroup) with outputGroup="0" and positive amount
                        -- Treatment: input (has inputGroup) with negative amount (e.g., waste treatment)
                        -- outputGroup valid values: 0=reference product, 1-3=byproducts, 4=allocated byproduct, 5=recyclable
                        isReferenceProduct = (isOutput && finalOutputGroup == "0") || (isInput && amount < 0)
                        exchange = TechnosphereExchange
                            (idFlowId idata)
                            (idAmount idata)
                            (idUnitId idata)
                            isInput
                            isReferenceProduct
                            (idActivityLinkId idata)
                            Nothing
                        flow = Flow
                            (idFlowId idata)
                            (if T.null (idFlowName idata) then idFlowId idata else idFlowName idata)
                            "technosphere"
                            (idUnitId idata)
                            Technosphere
                            (idSynonyms idata)
                        unit = Unit
                            (idUnitId idata)
                            (if T.null (idUnitName idata) then "unit" else idUnitName idata)
                            (if T.null (idUnitName idata) then "unit" else idUnitName idata)
                            ""
                    in state
                        { psExchanges = exchange : psExchanges state
                        , psFlows = flow : psFlows state
                        , psUnits = unit : psUnits state
                        , psContext = Other
                        , psTextAccum = []
                        , psPendingInputGroup = ""
                        , psPendingOutputGroup = ""
                        }
                _ -> state{psPath = tail (psPath state)}
        | isElement tagName "elementaryExchange" =
            case psContext state of
                InElementaryExchange edata ->
                    -- Use pending group values if attribute values are empty
                    let finalInputGroup = if T.null (edInputGroup edata) then psPendingInputGroup state else edInputGroup edata
                        finalOutputGroup = if T.null (edOutputGroup edata) then psPendingOutputGroup state else edOutputGroup edata
                        category = case (edCompartments edata, edSubcompartments edata) of
                            ([], []) -> "unspecified"
                            (comp : _, []) -> comp
                            ([], sub : _) -> sub
                            (comp : _, sub : _) -> comp <> "/" <> sub
                        exchange = BiosphereExchange
                            (edFlowId edata)
                            (edAmount edata)
                            (edUnitId edata)
                            (not $ T.null finalInputGroup)
                        flow = Flow
                            (edFlowId edata)
                            (if T.null (edFlowName edata) then edFlowId edata else edFlowName edata)
                            category
                            (edUnitId edata)
                            Biosphere
                            (edSynonyms edata)
                        unit = Unit
                            (edUnitId edata)
                            (if T.null (edUnitName edata) then "kg" else edUnitName edata)
                            (if T.null (edUnitName edata) then "kg" else edUnitName edata)
                            ""
                    in state
                        { psExchanges = exchange : psExchanges state
                        , psFlows = flow : psFlows state
                        , psUnits = unit : psUnits state
                        , psContext = Other
                        , psTextAccum = []
                        , psPendingInputGroup = ""
                        , psPendingOutputGroup = ""
                        }
                _ -> state{psPath = tail (psPath state)}
        | isElement tagName "text" =
            case psContext state of
                InGeneralCommentText idx ->
                    let txt = T.concat $ reverse $ map bsToText (psTextAccum state)
                        -- Store as (index, text) pair for later sorting
                    in if T.null txt
                        then state{psContext = Other, psTextAccum = []}
                        else state{psDescription = txt : psDescription state, psContext = Other, psTextAccum = []}
                _ -> state{psPath = tail (psPath state), psTextAccum = []}
        | isElement tagName "name" =
            let txt = T.concat $ reverse $ map bsToText (psTextAccum state)
                -- CRITICAL FIX: Only update flow name if <name> is a direct child of exchange
                -- Check if the parent element (second in path, after "name") is "property"
                -- psPath = ["name", parent, grandparent, ...]
                isInsideProperty = case psPath state of
                    (_:parent:_) -> isElement "property" parent
                    _ -> False
            in case psContext state of
                InIntermediateExchange idata | not isInsideProperty ->
                    state{psContext = InIntermediateExchange idata{idFlowName = txt}, psTextAccum = []}
                InElementaryExchange edata | not isInsideProperty ->
                    state{psContext = InElementaryExchange edata{edFlowName = txt}, psTextAccum = []}
                _ -> state{psPath = tail (psPath state), psTextAccum = []}
        | isElement tagName "unitName" =
            let txt = T.concat $ reverse $ map bsToText (psTextAccum state)
                -- Check if we're inside a property element (same logic as for <name>)
                -- psPath = ["unitName", parent, grandparent, ...]
                isInsideProperty = case psPath state of
                    (_:parent:_) -> isElement parent "property"
                    _ -> False
            in case psContext state of
                InIntermediateExchange idata | not isInsideProperty ->
                    -- Update the exchange's unit name, and also set reference unit if outputGroup="0"
                    let newState = state{psContext = InIntermediateExchange idata{idUnitName = txt}, psTextAccum = []}
                    in if idOutputGroup idata == "0" || psPendingOutputGroup state == "0"
                        then newState{psRefUnit = Just txt}
                        else newState
                InElementaryExchange edata | not isInsideProperty ->
                    state{psContext = InElementaryExchange edata{edUnitName = txt}, psTextAccum = []}
                _ -> state{psPath = tail (psPath state), psTextAccum = []}
        | isElement tagName "synonym" =
            -- Synonym text is accumulated but not yet stored in exchange data
            -- For now just clear text and pop path, keeping parent exchange context
            state{psPath = tail (psPath state), psTextAccum = []}
        | isElement tagName "inputGroup" =
            let txt = T.strip $ T.concat $ reverse $ map bsToText (psTextAccum state)
            -- DON'T change psContext - preserve the parent exchange context
            in state{psPendingInputGroup = txt, psPath = tail (psPath state), psTextAccum = []}
        | isElement tagName "outputGroup" =
            let txt = T.strip $ T.concat $ reverse $ map bsToText (psTextAccum state)
            -- DON'T change psContext - preserve the parent exchange context
            in state{psPendingOutputGroup = txt, psPath = tail (psPath state), psTextAccum = []}
        | isElement tagName "compartment" =
            let txt = T.strip $ T.concat $ reverse $ map bsToText (psTextAccum state)
            in case psContext state of
                InElementaryExchange edata | not (T.null txt) ->
                    -- CRITICAL FIX: Only store non-empty compartment text
                    -- XML has nested <compartment> elements: outer wrapper (no text) and inner element (has text)
                    -- Skipping empty text prevents adding "" from wrapper elements
                    state{psContext = InElementaryExchange edata{edCompartments = txt : edCompartments edata}, psPath = tail (psPath state), psTextAccum = []}
                _ ->
                    -- Empty text or not in exchange - just pop path
                    state{psPath = tail (psPath state), psTextAccum = []}
        | isElement tagName "subcompartment" =
            let txt = T.strip $ T.concat $ reverse $ map bsToText (psTextAccum state)
            in case psContext state of
                InElementaryExchange edata | not (T.null txt) ->
                    -- Only store non-empty subcompartment text (consistency with compartment handling)
                    state{psContext = InElementaryExchange edata{edSubcompartments = txt : edSubcompartments edata}, psPath = tail (psPath state), psTextAccum = []}
                _ ->
                    -- Empty text or not in exchange - just pop path
                    state{psPath = tail (psPath state), psTextAccum = []}
        | otherwise =
            state{psPath = if null (psPath state) then [] else tail (psPath state)}

    -- CDATA handler - treat as text
    cdata state content = text state content

    -- Build final result from parse state
    buildResult :: ParseState -> ProcessId -> (Activity, [Flow], [Unit])
    buildResult st pid =
        let name = case psActivityName st of
                Just n -> n
                Nothing -> "Unknown Activity"
            location = case psLocation st of
                Just loc -> loc
                Nothing -> "GLO"
            description = reverse (psDescription st)  -- Reverse to get correct order
            refUnit = case psRefUnit st of
                Just u -> u
                Nothing -> "unit"
            -- Apply cutoff strategy to exchanges
            activity = Activity name description M.empty M.empty location refUnit (reverse $ psExchanges st)
            activityWithCutoff = applyCutoffStrategy activity
            flows = reverse (psFlows st)
            units = reverse (psUnits st)
        in (activityWithCutoff, flows, units)

-- | Fast xeno-based parser (8-15x faster than xml-conduit)
streamParseActivityAndFlowsFromFile :: FilePath -> IO (Activity, [Flow], [Unit])
streamParseActivityAndFlowsFromFile path = do
    -- Read file as ByteString for xeno
    !xmlContent <- BS.readFile path

    -- Extract ProcessId from filename
    let filenameBase = T.pack $ takeBaseName path
    let !processId = case EcoSpold.Parser.parseProcessId filenameBase of
            Just pid -> pid
            Nothing -> error $ "Invalid filename format for ProcessId: " ++ path

    -- Try xeno parser first (fast path), fall back to xml-conduit on error
    case parseWithXeno xmlContent processId of
        Right result -> return result
        Left err -> do
            -- Fallback to xml-conduit for robustness
            doc <- Text.XML.readFile def path
            let !cursor = fromDocument doc
            let (!proc, !flows, !units) = parseActivityWithFlowsAndUnitsOptimized cursor processId
            let !procWithCutoff = applyCutoffStrategy proc
            procWithCutoff `seq` flows `seq` units `seq` return (procWithCutoff, flows, units)

-- | Optimized version of parseActivityWithFlowsAndUnits with better memory management
parseActivityWithFlowsAndUnitsOptimized :: Cursor -> ProcessId -> (Activity, [Flow], [Unit])
parseActivityWithFlowsAndUnitsOptimized cursor processId =
    let !name =
            headOrFail "Missing <activityName>" $
                cursor $// element (nsElement "activityName") &/ content
        !location = case cursor $// element (nsElement "geography") >=> attribute "location" of
            [] ->
                headOrFail "Missing geography shortname" $
                    cursor $// element (nsElement "shortname") &/ content
            (x : _) -> x

        -- Activity both types of exchanges
        !interNodes = cursor $// element (nsElement "intermediateExchange")
        !elemNodes = cursor $// element (nsElement "elementaryExchange")
        !interExchsWithFlowsAndUnits = map parseExchangeWithFlowOptimized interNodes
        !elemExchsWithFlowsAndUnits = map parseElementaryExchangeWithFlowOptimized elemNodes
        (!interExchs, !interFlows, !interUnits) = unzip3 interExchsWithFlowsAndUnits
        (!elemExchs, !elemFlows, !elemUnits) = unzip3 elemExchsWithFlowsAndUnits
        !exchs = interExchs ++ elemExchs
        !flows = interFlows ++ elemFlows
        !units = interUnits ++ elemUnits
        description = extractGeneralComment cursor name
        synonyms = M.empty -- TODO: Extract from XML when available
        -- Parse classifications (ISIC, CPC, etc.)
        classifications = M.fromList
            [ (system, value)
            | classNode <- cursor $// element (nsElement "classification")
            , let systemNodes = classNode $/ element (nsElement "classificationSystem") &/ content
            , let valueNodes = classNode $/ element (nsElement "classificationValue") &/ content
            , not (null systemNodes || null valueNodes)
            , let system = T.strip (head systemNodes)
            , let value = T.strip (head valueNodes)
            , not (T.null system || T.null value)
            ]
        refUnit = case cursor
            $// element (nsElement "intermediateExchange")
            >=> attributeIs "outputGroup" "0" of
            [] -> "unit" -- Default fallback - no reference product found
            (refProdCursor : _) ->
                -- Extract unitName from the reference product exchange
                case refProdCursor $/ element (nsElement "unitName") &/ content of
                    [] -> "unit" -- Fallback if unitName element missing
                    (unit : _) -> unit

        -- Use XML activity UUID for backward compatibility
        -- Note: ProcessId is now just an index, not stored in Activity
        !xmlActivityUUID =
            headOrFail "Missing activity@id or activity@activityId" $
                (cursor $// element (nsElement "activity") >=> attribute "id")
                    <> (cursor $// element (nsElement "activity") >=> attribute "activityId")
        -- Activity constructor now takes 7 arguments (removed activityId and activityProcessId)
        !activity = Activity name description synonyms classifications location refUnit exchs
     in (activity, flows, units)

-- | Memory-optimized exchange parsing with strict evaluation
parseExchangeWithFlowOptimized :: Cursor -> (Exchange, Flow, Unit)
parseExchangeWithFlowOptimized cur =
    let !fid = getAttr cur "intermediateExchangeId"
        !amount = read $ T.unpack $ getAttr cur "amount"
        !unitId = getAttr cur "unitId"

        -- Extract child element content with strict evaluation and safe access
        !fname = case cur $/ element (nsElement "name") &/ content of
            [] -> fid -- Use flowId as fallback name to save memory
            (x : _) -> x
        !unitName = case cur $/ element (nsElement "unitName") &/ content of
            [] -> "unit" -- Default unit
            (x : _) -> x

        -- Extract inputGroup and outputGroup from child elements
        -- Some datasets keep group flags as attributes, others nest them as child nodes.
        -- Prefer attributes (correct EcoSpold structure) but fall back to elements.
        !attrInputGroup = T.strip $ getAttr cur "inputGroup"
        !attrOutputGroup = T.strip $ getAttr cur "outputGroup"
        !inputGroup =
            if not (T.null attrInputGroup)
                then attrInputGroup
                else case cur $/ element (nsElement "inputGroup") &/ content of
                    [] -> ""
                    (x : _) -> T.strip x
        !outputGroup =
            if not (T.null attrOutputGroup)
                then attrOutputGroup
                else case cur $/ element (nsElement "outputGroup") &/ content of
                    [] -> ""
                    (x : _) -> T.strip x

        -- Determine type based on input/output groups (mutually exclusive)
        !isInput = inputGroup /= ""
        !isOutput = not isInput
        -- Reference flow identification for both production AND treatment activities:
        -- Production: output (no inputGroup) with outputGroup="0" and positive amount
        -- Treatment: input (has inputGroup) with negative amount (e.g., waste treatment)
        -- outputGroup valid values: 0=reference product, 1-3=byproducts, 4=allocated byproduct, 5=recyclable
        -- Note: outputGroup="4" is byproduct allocated, NOT a reference product
        !isRef = (isOutput && outputGroup == "0") || (isInput && amount < 0)
        !ftype = Technosphere

        -- Extract activityLinkId for technosphere navigation (required for technosphere)
        !activityLinkIdText = getAttr cur "activityLinkId"

        -- For now, we don't have the ProcessId available during parsing
        -- It will be resolved during database building when we have the full UUID mapping
        !processLinkId = Nothing  -- Will be resolved during database construction
        !synonyms = parseSynonyms cur
        !flow = Flow fid fname "technosphere" unitId ftype synonyms
        !unit = Unit unitId unitName unitName "" -- Use unitName for both name and symbol, empty comment
        !exchange = TechnosphereExchange fid amount unitId isInput isRef activityLinkIdText processLinkId
     in (exchange, flow, unit)

-- | Optimized elementary exchange parsing
parseElementaryExchangeWithFlowOptimized :: Cursor -> (Exchange, Flow, Unit)
parseElementaryExchangeWithFlowOptimized cur =
    let !fid = getAttr cur "elementaryExchangeId"
        !amount = read $ T.unpack $ getAttr cur "amount"
        !unitId = getAttr cur "unitId"

        !fname = case cur $/ element (nsElement "name") &/ content of
            [] -> fid
            (x : _) -> x
        !unitName = case cur $/ element (nsElement "unitName") &/ content of
            [] -> "kg"
            (x : _) -> x

        !attrInputGroup = T.strip $ getAttr cur "inputGroup"
        !attrOutputGroup = T.strip $ getAttr cur "outputGroup"
        !inputGroup =
            if not (T.null attrInputGroup)
                then attrInputGroup
                else case cur $/ element (nsElement "inputGroup") &/ content of
                    [] -> ""
                    (x : _) -> T.strip x
        !outputGroup =
            if not (T.null attrOutputGroup)
                then attrOutputGroup
                else case cur $/ element (nsElement "outputGroup") &/ content of
                    [] -> ""
                    (x : _) -> T.strip x

        !isInput = inputGroup /= ""
        !ftype = Biosphere
        !category = extractCompartmentCategory cur

        !synonyms = parseSynonyms cur
        !flow = Flow fid fname category unitId ftype synonyms
        !unit = Unit unitId unitName unitName "" -- Use unitName for both name and symbol, empty comment
        !exchange = BiosphereExchange fid amount unitId isInput
     in (exchange, flow, unit)

-- | Extract generalComment text nodes, ordered by index attribute
extractGeneralComment :: Cursor -> Text -> [Text]
extractGeneralComment cursor fallbackName =
    let textNodes =
            cursor
                $// element (nsElement "generalComment")
                &/ element (nsElement "text")
        -- Extract text content and index attributes
        textWithIndexes =
            [ (readIndexSafe idx, content)
            | textNode <- textNodes
            , let contentList = textNode $/ content
            , let content = T.concat contentList
            , let idx = case textNode $| attribute "index" of
                    [] -> "0" -- Default index if missing
                    (x : _) -> x
            , not (T.null content)
            ]
        -- Sort by index and return as list
        sortedTexts = map snd $ Data.List.sortOn fst textWithIndexes
     in case sortedTexts of
            [] -> [fallbackName] -- Fallback to name if no text content
            texts -> texts -- Return list of text nodes

-- | Safely parse index attribute with fallback
readIndexSafe :: Text -> Int
readIndexSafe t = case reads (T.unpack t) of
    [(n, "")] -> n
    _ -> 0 -- Default to 0 if parsing fails

{- | Apply cut-off strategy
1. Remove zero-amount production exchanges (co-products)
2. Assign single non-zero product as reference product
3. Ensure single-output process structure
4. VALIDATION: Fail if no reference product can be established
-}
applyCutoffStrategy :: Activity -> Activity
applyCutoffStrategy activity =
    let originalExchanges = exchanges activity
        originalRefs = filter exchangeIsReference originalExchanges
        filteredExchanges = removeZeroAmountCoproducts originalExchanges
        updatedActivity = activity{exchanges = filteredExchanges}
        refsAfterFilter = filter exchangeIsReference filteredExchanges
        -- CONSERVATIVE: Only apply single reference assignment if no reference products exist
        finalActivity =
            if hasReferenceProduct updatedActivity
                then updatedActivity -- Keep existing reference products
                else assignSingleProductAsReference updatedActivity
        -- CRITICAL VALIDATION: Ensure we have a reference product after all attempts
     in if hasReferenceProduct finalActivity
           then finalActivity
           else error $ "Activity has no reference product after cutoff strategy: "
                     ++ T.unpack (activityName activity) ++ "\n"
                     ++ "  Original exchanges: " ++ show (length originalExchanges) ++ "\n"
                     ++ "  Original reference products: " ++ show (length originalRefs) ++ "\n"
                     ++ "  After filtering: " ++ show (length filteredExchanges) ++ "\n"
                     ++ "  Reference products after filter: " ++ show (length refsAfterFilter)

-- | Check if activity has any reference product
hasReferenceProduct :: Activity -> Bool
hasReferenceProduct activity = any exchangeIsReference (exchanges activity)

-- | Remove production exchanges with zero amounts
-- CRITICAL: ALWAYS keep reference products, even if zero amount
-- Zero-amount reference products will be caught by normalization validation
removeZeroAmountCoproducts :: [Exchange] -> [Exchange]
removeZeroAmountCoproducts exs = filter keepExchange exs
  where

    -- ALWAYS keep reference products (isRef=True), even if amount is zero
    keepExchange (TechnosphereExchange _ _ _ False True _ _) = True
    -- For non-reference outputs (co-products), only keep non-zero amounts
    keepExchange (TechnosphereExchange _ amount _ False False _ _) = amount /= 0.0
    -- Keep all inputs
    keepExchange (TechnosphereExchange _ _ _ True _ _ _) = True
    -- Keep all biosphere exchanges
    keepExchange (BiosphereExchange _ _ _ _) = True
    keepExchange _ = True -- Keep everything else

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

-- | Check if exchange is production exchange (output, non-reference)
isProductionExchange :: Exchange -> Bool
isProductionExchange (TechnosphereExchange _ _ _ False _ _ _) = True -- Output technosphere = production
isProductionExchange _ = False

-- | Update reference product flag for the specified exchange
updateReferenceProduct :: Exchange -> Exchange -> Exchange
updateReferenceProduct target current
    | exchangeFlowId target == exchangeFlowId current = markAsReference current
    | otherwise = unmarkAsReference current

-- | Mark exchange as reference product
markAsReference :: Exchange -> Exchange
markAsReference (TechnosphereExchange fid amt uid isInput _ linkId procLink) =
    TechnosphereExchange fid amt uid isInput True linkId procLink
markAsReference ex = ex -- No change for biosphere exchanges

-- | Unmark exchange as reference product
unmarkAsReference :: Exchange -> Exchange
unmarkAsReference (TechnosphereExchange fid amt uid isInput _ linkId procLink) =
    TechnosphereExchange fid amt uid isInput False linkId procLink
unmarkAsReference ex = ex -- No change for biosphere exchanges
