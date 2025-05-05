{-# LANGUAGE OverloadedStrings #-}

module EcoSpold.Parser (parseProcessFromFile) where

import ACV.Types
import Data.Text (Text)
import qualified Data.Text as T
import Text.XML
import Text.XML.Cursor

parseProcessFromFile :: FilePath -> IO Process
parseProcessFromFile path = do
    print path
    doc <- Text.XML.readFile def path
    let cursor = fromDocument doc
    let proc = parseProcess cursor
    proc `seq` return proc

parseProcess :: Cursor -> Process
parseProcess cursor =
    let name =
            T.unpack $
                headOrFail "Missing <activityName>" $
                    cursor $// element "activityName" &/ content
        location =
            T.unpack $
                headOrFail "Missing geography@location" $
                    cursor $// element "geography" >=> attribute "location"
        uuid =
            T.unpack $
                headOrFail "Missing activity@activityId" $
                    cursor $// element "activity" >=> attribute "activityId"
        exNodes = cursor $// element "exchange"
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
