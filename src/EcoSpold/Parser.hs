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
    let (!proc, !flows, !units) = parseActivityWithFlowsAndUnitsOptimized cursor
    -- Force full evaluation before returning
    proc `seq` flows `seq` units `seq` return (proc, flows, units)

-- | Optimized version of parseActivityWithFlowsAndUnits with better memory management
parseActivityWithFlowsAndUnitsOptimized :: Cursor -> (Activity, [Flow], [Unit])
parseActivityWithFlowsAndUnitsOptimized cursor =
    let !name =
            headOrFail "Missing <activityName>" $
                cursor $// element (nsElement "activityName") &/ content
        !location = case cursor $// element (nsElement "geography") >=> attribute "location" of
            [] ->
                headOrFail "Missing geography shortname" $
                    cursor $// element (nsElement "shortname") &/ content
            (x : _) -> x
        !uuid =
            headOrFail "Missing activity@id or activity@activityId" $
                (cursor $// element (nsElement "activity") >=> attribute "id")
                    <> (cursor $// element (nsElement "activity") >=> attribute "activityId")

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

        !activity = Activity uuid name description synonyms classifications location refUnit exchs
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
        !inputGroup = case cur $/ element (nsElement "inputGroup") &/ content of
            [] -> ""
            (x : _) -> x
        !outputGroup = case cur $/ element (nsElement "outputGroup") &/ content of
            [] -> ""
            (x : _) -> x

        -- Determine type based on input/output groups (mutually exclusive)
        !isInput = inputGroup /= ""
        !isRef = outputGroup == "0" -- Reference product has outputGroup="0"
        !ftype = Technosphere

        -- Extract activityLinkId for technosphere navigation (required for technosphere)
        !activityLinkId = getAttr cur "activityLinkId"

        !synonyms = parseSynonyms cur
        !flow = Flow fid fname "technosphere" unitId ftype synonyms
        !unit = Unit unitId unitName unitName "" -- Use unitName for both name and symbol, empty comment
        !exchange = TechnosphereExchange fid amount unitId isInput isRef activityLinkId
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

        !inputGroup = case cur $/ element (nsElement "inputGroup") &/ content of
            [] -> ""
            (x : _) -> x
        !outputGroup = case cur $/ element (nsElement "outputGroup") &/ content of
            [] -> ""
            (x : _) -> x

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
