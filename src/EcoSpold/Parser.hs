{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module EcoSpold.Parser (parseProcessFromFile, parseProcessAndFlowsFromFile) where

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
        exNodes = cursor $// element (nsElement "intermediateExchange")
        !exchs = map parseExchange exNodes  -- Keep this strict to avoid retaining large lists
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
        exNodes = cursor $// element (nsElement "intermediateExchange")
        !exchsWithFlows = map parseExchangeWithFlow exNodes
        (!exchs, flows) = unzip exchsWithFlows
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
        
        -- Input if inputGroup exists, output if outputGroup exists (mutually exclusive)
        !isInput = inputGroup /= ""
        !isRef = outputGroup == "0"  -- Reference product has outputGroup="0"
     in Exchange fid amount isInput isRef

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
        
        !flow = Flow fid fname "technosphere" unitName ftype
        !exchange = Exchange fid amount isInput isRef
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
