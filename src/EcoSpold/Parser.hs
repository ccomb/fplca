{-# LANGUAGE OverloadedStrings #-}

module EcoSpold.Parser (parseProcessFromFile) where

import ACV.Types
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

parseProcessFromFile :: FilePath -> IO Process
parseProcessFromFile path = do
    print path
    doc <- Text.XML.readFile def path
    let cursor = fromDocument doc
    let proc = parseProcess cursor
    -- Force deep evaluation to avoid memory leaks
    proc `seq` return proc

parseProcess :: Cursor -> Process
parseProcess cursor =
    let name =
            T.unpack $
                headOrFail "Missing <activityName>" $
                    cursor $// element (nsElement "activityName") &/ content
        location =
            case cursor $// element (nsElement "geography") >=> attribute "location" of
                [] -> T.unpack $ headOrFail "Missing geography shortname" $
                        cursor $// element (nsElement "shortname") &/ content
                (x:_) -> T.unpack x
        uuid =
            T.unpack $
                headOrFail "Missing activity@id or activity@activityId" $
                    (cursor $// element (nsElement "activity") >=> attribute "id") <>
                    (cursor $// element (nsElement "activity") >=> attribute "activityId")
        exNodes = cursor $// element (nsElement "exchange")
        exchs = map parseExchange exNodes
     in Process uuid name location exchs

parseExchange :: Cursor -> Exchange
parseExchange cur =
    let get = getAttr cur
        fid = get "flowId"
        fname = get "name"
        cat = get "category"
        unit = get "unit"
        group = get "inputGroup"
        amount = read $ get "meanAmount"
        ftype = if cat `elem` ["air", "water", "soil", "resource"] then Biosphere else Technosphere
        isInput = group == "1"
        isRef = group == "0"
     in Exchange (Flow fid fname cat unit ftype) amount isInput isRef

getAttr :: Cursor -> Text -> String
getAttr cur attr =
    let name = Name attr Nothing Nothing
     in T.unpack $ headOrFail ("Missing attribute: " ++ T.unpack attr) (cur $| attribute name)

-- | Safer alternative to head
headOrFail :: String -> [a] -> a
headOrFail msg [] = error msg
headOrFail _ (x : _) = x
