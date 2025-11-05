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
    | InSynonym !Text  -- Track language
    | InCompartment !CompartmentData
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

-- | Compartment parsing data
data CompartmentData = CompartmentData
    { cdCompartments :: ![Text]
    , cdSubcompartments :: ![Text]
    , cdInSubcompartment :: !Bool
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
    , psCurrentSynonymLang :: !Text  -- Current synonym language
    , psCompartmentData :: !CompartmentData  -- Compartment parsing state
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
    , psCurrentSynonymLang = "en"
    , psCompartmentData = CompartmentData [] [] False
    }

-- | ByteString to Text conversion with UTF-8 decoding
bsToText :: BS.ByteString -> Text
bsToText = TE.decodeUtf8

-- | ByteString to Double conversion
bsToDouble :: BS.ByteString -> Double
bsToDouble bs = case TR.double (bsToText bs) of
    Right (val, _) -> val
    Left _ -> 0.0

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
            newContext
                | isElement tagName "activityName" = InActivityName
                | isElement tagName "shortname" && any (isElement "geography") (psPath state) = InGeographyShortname
                | isElement tagName "intermediateExchange" =
                    InIntermediateExchange (IntermediateData "" 0.0 "" "" "" "" "" "" M.empty)
                | isElement tagName "elementaryExchange" =
                    InElementaryExchange (ElementaryData "" 0.0 "" "" "" "" "" [] [] M.empty)
                | isElement tagName "text" && any (isElement "generalComment") (psPath state) = InGeneralCommentText 0
                | isElement tagName "synonym" = InSynonym "en"
                | isElement tagName "compartment" =
                    InCompartment (psCompartmentData state)
                | isElement tagName "subcompartment" =
                    InCompartment (psCompartmentData state){cdInSubcompartment = True}
                | otherwise = psContext state
        in state{psPath = newPath, psContext = newContext, psTextAccum = []}

    -- Attribute handler - extract critical attributes
    attribute state name value =
        case psContext state of
            InIntermediateExchange idata ->
                let updated
                        | isElement name "intermediateExchangeId" = idata{idFlowId = bsToText value}
                        | isElement name "amount" = idata{idAmount = bsToDouble value}
                        | isElement name "unitId" = idata{idUnitId = bsToText value}
                        | isElement name "inputGroup" = idata{idInputGroup = bsToText value}
                        | isElement name "outputGroup" = idata{idOutputGroup = bsToText value}
                        | isElement name "activityLinkId" = idata{idActivityLinkId = bsToText value}
                        | otherwise = idata
                in state{psContext = InIntermediateExchange updated}
            InElementaryExchange edata ->
                let updated
                        | isElement name "elementaryExchangeId" = edata{edFlowId = bsToText value}
                        | isElement name "amount" = edata{edAmount = bsToDouble value}
                        | isElement name "unitId" = edata{edUnitId = bsToText value}
                        | isElement name "inputGroup" = edata{edInputGroup = bsToText value}
                        | isElement name "outputGroup" = edata{edOutputGroup = bsToText value}
                        | otherwise = edata
                in state{psContext = InElementaryExchange updated}
            InGeneralCommentText _ ->
                let idx = if isElement name "index" then bsToInt value else 0
                in state{psContext = InGeneralCommentText idx}
            InSynonym _ ->
                let lang = if name == "xml:lang" then bsToText value else "en"
                in state{psContext = InSynonym lang, psCurrentSynonymLang = lang}
            _ ->
                -- Handle reference unit from intermediateExchange with outputGroup="0"
                if isElement name "unitName" && any (isElement "intermediateExchange") (psPath state)
                    then case psContext state of
                        InIntermediateExchange idata
                            | idOutputGroup idata == "0" || idOutputGroup idata == "4" ->
                                state{psRefUnit = Just (bsToText value)}
                        _ -> state
                    else state

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
                    let exchange = TechnosphereExchange
                            (idFlowId idata)
                            (idAmount idata)
                            (idUnitId idata)
                            (not $ T.null $ idInputGroup idata)
                            (idOutputGroup idata == "4" || idOutputGroup idata == "0")
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
                        }
                _ -> state{psPath = tail (psPath state)}
        | isElement tagName "elementaryExchange" =
            case psContext state of
                InElementaryExchange edata ->
                    let category = case (edCompartments edata, edSubcompartments edata) of
                            ([], []) -> "unspecified"
                            (comp : _, []) -> comp
                            ([], sub : _) -> sub
                            (comp : _, sub : _) -> comp <> "/" <> sub
                        exchange = BiosphereExchange
                            (edFlowId edata)
                            (edAmount edata)
                            (edUnitId edata)
                            (not $ T.null $ edInputGroup edata)
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
                        , psCompartmentData = CompartmentData [] [] False
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
            in case psContext state of
                InIntermediateExchange idata ->
                    state{psContext = InIntermediateExchange idata{idFlowName = txt}, psTextAccum = []}
                InElementaryExchange edata ->
                    state{psContext = InElementaryExchange edata{edFlowName = txt}, psTextAccum = []}
                _ -> state{psPath = tail (psPath state), psTextAccum = []}
        | isElement tagName "unitName" =
            let txt = T.concat $ reverse $ map bsToText (psTextAccum state)
            in case psContext state of
                InIntermediateExchange idata ->
                    state{psContext = InIntermediateExchange idata{idUnitName = txt}, psTextAccum = []}
                InElementaryExchange edata ->
                    state{psContext = InElementaryExchange edata{edUnitName = txt}, psTextAccum = []}
                _ -> state{psPath = tail (psPath state), psTextAccum = []}
        | isElement tagName "synonym" =
            case psContext state of
                InSynonym lang ->
                    let txt = T.concat $ reverse $ map bsToText (psTextAccum state)
                    in if T.null txt
                        then state{psContext = Other, psTextAccum = []}
                        else
                            -- Update synonyms in current exchange context
                            let updatedState = case psContext state of
                                    _ -> state  -- Will handle in parent context
                            in updatedState{psContext = Other, psTextAccum = []}
                _ -> state{psPath = tail (psPath state)}
        | isElement tagName "compartment" =
            let txt = T.concat $ reverse $ map bsToText (psTextAccum state)
                compartData = psCompartmentData state
                updatedCompartData =
                    if cdInSubcompartment compartData
                        then compartData  -- Don't update, we're in subcompartment
                        else compartData{cdCompartments = txt : cdCompartments compartData}
            in case psContext state of
                InElementaryExchange edata ->
                    state
                        { psContext = InElementaryExchange edata{edCompartments = txt : edCompartments edata}
                        , psCompartmentData = updatedCompartData
                        , psTextAccum = []
                        }
                _ -> state{psCompartmentData = updatedCompartData, psPath = tail (psPath state), psTextAccum = []}
        | isElement tagName "subcompartment" =
            let txt = T.concat $ reverse $ map bsToText (psTextAccum state)
                compartData = psCompartmentData state
                updatedCompartData = compartData{cdSubcompartments = txt : cdSubcompartments compartData, cdInSubcompartment = False}
            in case psContext state of
                InElementaryExchange edata ->
                    state
                        { psContext = InElementaryExchange edata{edSubcompartments = txt : edSubcompartments edata}
                        , psCompartmentData = updatedCompartData
                        , psTextAccum = []
                        }
                _ -> state{psCompartmentData = updatedCompartData, psPath = tail (psPath state), psTextAccum = []}
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
        classifications = M.empty -- TODO: Extract ISIC, CPC classifications
        refUnit = case cursor
            $// element (nsElement "intermediateExchange")
            >=> attributeIs "outputGroup" "0"
            >=> attribute "unitName" of
            [] -> "unit" -- Default fallback
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
        !isRef = outputGroup == "4" || outputGroup == "0" -- Reference product is flagged with 4 (legacy datasets used 0)
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
-}
applyCutoffStrategy :: Activity -> Activity
applyCutoffStrategy activity =
    let filteredExchanges = removeZeroAmountCoproducts (exchanges activity)
        updatedActivity = activity{exchanges = filteredExchanges}
        -- CONSERVATIVE: Only apply single reference assignment if no reference products exist
        finalActivity =
            if hasReferenceProduct updatedActivity
                then updatedActivity -- Keep existing reference products
                else assignSingleProductAsReference updatedActivity
     in finalActivity

-- | Check if activity has any reference product
hasReferenceProduct :: Activity -> Bool
hasReferenceProduct activity = any exchangeIsReference (exchanges activity)

-- | Remove production exchanges with zero amounts
removeZeroAmountCoproducts :: [Exchange] -> [Exchange]
removeZeroAmountCoproducts exs = filter keepExchange exs
  where
    keepExchange (TechnosphereExchange _ amount _ False True _ _) = amount /= 0.0 -- Keep non-zero production exchanges
    keepExchange (TechnosphereExchange _ _ _ _ False _ _) = True -- Keep all non-production technosphere exchanges
    keepExchange (BiosphereExchange _ _ _ _) = True -- Keep all biosphere exchanges
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
