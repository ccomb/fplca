{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module EcoSpold.Parser (streamParseActivityAndFlowsFromFile) where

import ACV.Types
import qualified Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (takeBaseName)
import Text.XML
import Text.XML.Cursor

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

-- | Memory-optimized parser - forces early evaluation and cleanup
streamParseActivityAndFlowsFromFile :: FilePath -> IO (Activity, [Flow], [Unit])
streamParseActivityAndFlowsFromFile path = do
    -- Read and parse with strict evaluation
    doc <- Text.XML.readFile def path
    let !cursor = fromDocument doc
    -- Extract ProcessId from filename (activity_uuid_product_uuid.spold)
    let filenameBase = T.pack $ takeBaseName path
    let !processId = case parseProcessId filenameBase of
            Just pid -> pid
            Nothing -> error $ "Invalid filename format for ProcessId: " ++ path ++ " (expected: activity_uuid_product_uuid.spold)"
    let (!proc, !flows, !units) = parseActivityWithFlowsAndUnitsOptimized cursor processId
    -- Apply cut-off strategy (conservative version)
    let !procWithCutoff = applyCutoffStrategy proc
    -- Force full evaluation before returning
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

        -- Use XML activity UUID for backward compatibility, store ProcessId separately
        !xmlActivityUUID =
            headOrFail "Missing activity@id or activity@activityId" $
                (cursor $// element (nsElement "activity") >=> attribute "id")
                    <> (cursor $// element (nsElement "activity") >=> attribute "activityId")
        !activity = Activity xmlActivityUUID (Just processId) name description synonyms classifications location refUnit exchs
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

        -- For now, create ProcessId with same UUID for both activity and product
        -- TODO: Need to resolve actual product UUID from flow reference
        !processLinkId =
            if T.null activityLinkIdText
                then Nothing
                else Just $ ProcessId activityLinkIdText activityLinkIdText -- Temporary: use same UUID for both
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
