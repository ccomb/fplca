{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module EcoSpold.Parser (parseProcessFromFile, parseProcessAndFlowsFromFile, streamParseProcessAndFlowsFromFile) where

import ACV.Types
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import Text.XML
import Text.XML.Cursor

-- EcoSpold namespace
ecoSpoldNS :: Text
ecoSpoldNS = "http://www.EcoInvent.org/EcoSpold02"

-- Helper to create namespaced element name
nsElement :: Text -> Name
nsElement name = Name name (Just ecoSpoldNS) Nothing

parseProcessFromFile :: FilePath -> IO Process
parseProcessFromFile path = do
    print path
    doc <- Text.XML.readFile def path
    let cursor = fromDocument doc
    let !proc = parseProcess cursor
    -- Force deep evaluation to avoid memory leaks
    return proc

parseProcess :: Cursor -> Process
parseProcess cursor =
    let name =
                headOrFail "Missing <activityName>" $
                    cursor $// element (nsElement "activityName") &/ content
        location =
            case cursor $// element (nsElement "geography") >=> attribute "location" of
                [] -> headOrFail "Missing geography shortname" $
                        cursor $// element (nsElement "shortname") &/ content
                (x:_) -> x
        uuid =
                headOrFail "Missing activity@id or activity@activityId" $
                    (cursor $// element (nsElement "activity") >=> attribute "id") <>
                    (cursor $// element (nsElement "activity") >=> attribute "activityId")
        -- Parse both intermediate and elementary exchanges
        interNodes = cursor $// element (nsElement "intermediateExchange")
        elemNodes = cursor $// element (nsElement "elementaryExchange")
        !interExchs = map parseExchange interNodes
        !elemExchs = map parseElementaryExchange elemNodes
        !exchs = interExchs ++ elemExchs
     in Process uuid name location exchs

-- | Parse un procédé et extrait les flux pour la déduplication
parseProcessAndFlowsFromFile :: FilePath -> IO (Process, [Flow])
parseProcessAndFlowsFromFile path = do
    print path
    doc <- Text.XML.readFile def path
    let cursor = fromDocument doc
    let (proc, flows) = parseProcessWithFlows cursor
    return (proc, flows)

parseProcessWithFlows :: Cursor -> (Process, [Flow])
parseProcessWithFlows cursor =
    let name =
                headOrFail "Missing <activityName>" $
                    cursor $// element (nsElement "activityName") &/ content
        location =
            case cursor $// element (nsElement "geography") >=> attribute "location" of
                [] -> headOrFail "Missing geography shortname" $
                        cursor $// element (nsElement "shortname") &/ content
                (x:_) -> x
        uuid =
                headOrFail "Missing activity@id or activity@activityId" $
                    (cursor $// element (nsElement "activity") >=> attribute "id") <>
                    (cursor $// element (nsElement "activity") >=> attribute "activityId")
        -- Parse both intermediate and elementary exchanges
        interNodes = cursor $// element (nsElement "intermediateExchange")
        elemNodes = cursor $// element (nsElement "elementaryExchange")
        !interExchsWithFlows = map parseExchangeWithFlow interNodes
        !elemExchsWithFlows = map parseElementaryExchangeWithFlow elemNodes
        (!interExchs, interFlows) = unzip interExchsWithFlows
        (!elemExchs, elemFlows) = unzip elemExchsWithFlows
        !exchs = interExchs ++ elemExchs
        !flows = interFlows ++ elemFlows
     in (Process uuid name location exchs, flows)

parseExchange :: Cursor -> Exchange
parseExchange cur =
    let get = getAttr cur
        !fid = get "intermediateExchangeId"
        !amount = read $ T.unpack $ get "amount"
        
        -- Extract inputGroup and outputGroup from child elements (not attributes!)
        !inputGroup = case cur $/ element (nsElement "inputGroup") &/ content of
                       [] -> ""
                       (x:_) -> x
        !outputGroup = case cur $/ element (nsElement "outputGroup") &/ content of
                        [] -> ""
                        (x:_) -> x
        
        -- Extract activityLinkId for technosphere navigation
        !activityLinkId = case getAttr cur "activityLinkId" of
                           "" -> Nothing
                           aid -> Just aid
        
        -- Input if inputGroup exists, output if outputGroup exists (mutually exclusive)
        !isInput = inputGroup /= ""
        !isRef = outputGroup == "0"  -- Reference product has outputGroup="0"
     in Exchange fid amount isInput isRef activityLinkId

-- | Parse un échange et extrait aussi le flux pour la déduplication
parseExchangeWithFlow :: Cursor -> (Exchange, Flow)
parseExchangeWithFlow cur =
    let get = getAttr cur
        -- Extract attributes with strict evaluation
        !fid = get "intermediateExchangeId"
        !amount = read $ T.unpack $ get "amount"
        
        -- Extract child element content with strict evaluation and safe access
        !fname = case cur $/ element (nsElement "name") &/ content of
                   [] -> fid  -- Use flowId as fallback name to save memory
                   (x:_) -> x
        !unitName = case cur $/ element (nsElement "unitName") &/ content of
                      [] -> "unit"  -- Default unit
                      (x:_) -> x
        
        -- Extract inputGroup and outputGroup from child elements (not attributes!)
        !inputGroup = case cur $/ element (nsElement "inputGroup") &/ content of
                       [] -> ""
                       (x:_) -> x
        !outputGroup = case cur $/ element (nsElement "outputGroup") &/ content of
                        [] -> ""
                        (x:_) -> x
        
        -- Determine type based on input/output groups (mutually exclusive)
        !isInput = inputGroup /= ""
        !isRef = outputGroup == "0"  -- Reference product has outputGroup="0"
        
        -- For now, assume all flows are Technosphere since we don't have biosphere emissions in this data
        !ftype = Technosphere
        
        -- Extract activityLinkId for technosphere navigation
        !activityLinkId = case getAttr cur "activityLinkId" of
                           "" -> Nothing
                           aid -> Just aid
        
        !flow = Flow fid fname "technosphere" unitName ftype
        !exchange = Exchange fid amount isInput isRef activityLinkId
     in (exchange, flow)

getAttr :: Cursor -> Text -> Text
getAttr cur attr =
    let !name = Name attr Nothing Nothing
     in case cur $| attribute name of
         [] -> ""  -- Return empty string for missing attributes
         (x:_) -> x

-- | Safer alternative to head
headOrFail :: String -> [a] -> a
headOrFail msg [] = error msg
headOrFail _ (x : _) = x

-- | Memory-optimized parser - forces early evaluation and cleanup
streamParseProcessAndFlowsFromFile :: FilePath -> IO (Process, [Flow])
streamParseProcessAndFlowsFromFile path = do
    -- Read and parse with strict evaluation
    doc <- Text.XML.readFile def path
    let !cursor = fromDocument doc
    let (!proc, !flows) = parseProcessWithFlowsOptimized cursor
    -- Force full evaluation before returning
    proc `seq` flows `seq` return (proc, flows)

-- | Optimized version of parseProcessWithFlows with better memory management
parseProcessWithFlowsOptimized :: Cursor -> (Process, [Flow])
parseProcessWithFlowsOptimized cursor =
    let !name = headOrFail "Missing <activityName>" $
                    cursor $// element (nsElement "activityName") &/ content
        !location = case cursor $// element (nsElement "geography") >=> attribute "location" of
                     [] -> headOrFail "Missing geography shortname" $
                           cursor $// element (nsElement "shortname") &/ content
                     (x:_) -> x
        !uuid = headOrFail "Missing activity@id or activity@activityId" $
                    (cursor $// element (nsElement "activity") >=> attribute "id") <>
                    (cursor $// element (nsElement "activity") >=> attribute "activityId")
        
        -- Process both types of exchanges
        !interNodes = cursor $// element (nsElement "intermediateExchange")
        !elemNodes = cursor $// element (nsElement "elementaryExchange")
        !interExchsWithFlows = map parseExchangeWithFlowOptimized interNodes
        !elemExchsWithFlows = map parseElementaryExchangeWithFlowOptimized elemNodes
        (!interExchs, !interFlows) = unzip interExchsWithFlows
        (!elemExchs, !elemFlows) = unzip elemExchsWithFlows
        !exchs = interExchs ++ elemExchs
        !flows = interFlows ++ elemFlows
        
        !process = Process uuid name location exchs
     in (process, flows)

-- | Memory-optimized exchange parsing with strict evaluation
parseExchangeWithFlowOptimized :: Cursor -> (Exchange, Flow)
parseExchangeWithFlowOptimized cur =
    let !fid = getAttr cur "intermediateExchangeId"
        !amount = read $ T.unpack $ getAttr cur "amount"
        
        -- Extract child element content with strict evaluation and safe access
        !fname = case cur $/ element (nsElement "name") &/ content of
                   [] -> fid  -- Use flowId as fallback name to save memory
                   (x:_) -> x
        !unitName = case cur $/ element (nsElement "unitName") &/ content of
                      [] -> "unit"  -- Default unit
                      (x:_) -> x
        
        -- Extract inputGroup and outputGroup from child elements
        !inputGroup = case cur $/ element (nsElement "inputGroup") &/ content of
                       [] -> ""
                       (x:_) -> x
        !outputGroup = case cur $/ element (nsElement "outputGroup") &/ content of
                        [] -> ""
                        (x:_) -> x
        
        -- Determine type based on input/output groups (mutually exclusive)
        !isInput = inputGroup /= ""
        !isRef = outputGroup == "0"  -- Reference product has outputGroup="0"
        !ftype = Technosphere
        
        -- Extract activityLinkId for technosphere navigation
        !activityLinkId = case getAttr cur "activityLinkId" of
                           "" -> Nothing
                           aid -> Just aid
        
        !flow = Flow fid fname "technosphere" unitName ftype
        !exchange = Exchange fid amount isInput isRef activityLinkId
     in (exchange, flow)

-- | Parse elementary exchange (biosphere flows)
parseElementaryExchange :: Cursor -> Exchange
parseElementaryExchange cur =
    let get = getAttr cur
        !fid = get "elementaryExchangeId"
        !amount = read $ T.unpack $ get "amount"
        
        -- Elementary exchanges use outputGroup for emissions/waste, inputGroup for resources
        !inputGroup = case cur $/ element (nsElement "inputGroup") &/ content of
                       [] -> ""
                       (x:_) -> x
        !outputGroup = case cur $/ element (nsElement "outputGroup") &/ content of
                        [] -> ""
                        (x:_) -> x
        
        -- Elementary exchanges don't have activityLinkId (they link to biosphere)
        !activityLinkId = Nothing
        
        -- For elementary exchanges: inputGroup = resource extraction, outputGroup = emission
        !isInput = inputGroup /= ""
        !isRef = False  -- Elementary exchanges are never reference products
     in Exchange fid amount isInput isRef activityLinkId

-- | Parse elementary exchange with flow extraction
parseElementaryExchangeWithFlow :: Cursor -> (Exchange, Flow)
parseElementaryExchangeWithFlow cur =
    let get = getAttr cur
        !fid = get "elementaryExchangeId"
        !amount = read $ T.unpack $ get "amount"
        
        -- Extract child element content
        !fname = case cur $/ element (nsElement "name") &/ content of
                   [] -> fid
                   (x:_) -> x
        !unitName = case cur $/ element (nsElement "unitName") &/ content of
                      [] -> "kg"  -- Common default for biosphere flows
                      (x:_) -> x
        
        -- Elementary exchanges use outputGroup for emissions, inputGroup for resources
        !inputGroup = case cur $/ element (nsElement "inputGroup") &/ content of
                       [] -> ""
                       (x:_) -> x
        !outputGroup = case cur $/ element (nsElement "outputGroup") &/ content of
                        [] -> ""
                        (x:_) -> x
        
        !isInput = inputGroup /= ""
        !isRef = False
        !ftype = Biosphere
        
        -- Determine category based on input/output group
        !category = if isInput then "resource" else "emission"
        
        -- Elementary exchanges don't have activityLinkId
        !activityLinkId = Nothing
        
        !flow = Flow fid fname category unitName ftype
        !exchange = Exchange fid amount isInput isRef activityLinkId
     in (exchange, flow)

-- | Optimized elementary exchange parsing
parseElementaryExchangeWithFlowOptimized :: Cursor -> (Exchange, Flow)
parseElementaryExchangeWithFlowOptimized cur =
    let !fid = getAttr cur "elementaryExchangeId"
        !amount = read $ T.unpack $ getAttr cur "amount"
        
        !fname = case cur $/ element (nsElement "name") &/ content of
                   [] -> fid
                   (x:_) -> x
        !unitName = case cur $/ element (nsElement "unitName") &/ content of
                      [] -> "kg"
                      (x:_) -> x
        
        !inputGroup = case cur $/ element (nsElement "inputGroup") &/ content of
                       [] -> ""
                       (x:_) -> x
        !outputGroup = case cur $/ element (nsElement "outputGroup") &/ content of
                        [] -> ""
                        (x:_) -> x
        
        !isInput = inputGroup /= ""
        !isRef = False
        !ftype = Biosphere
        !category = if isInput then "resource" else "emission"
        
        -- Elementary exchanges don't have activityLinkId
        !activityLinkId = Nothing
        
        !flow = Flow fid fname category unitName ftype
        !exchange = Exchange fid amount isInput isRef activityLinkId
     in (exchange, flow)
