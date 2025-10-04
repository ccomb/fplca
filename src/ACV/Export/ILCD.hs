{-# LANGUAGE OverloadedStrings #-}

module ACV.Export.ILCD (exportInventoryAsILCD) where

import ACV.Matrix (Inventory)
import ACV.Types
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Text.XML (Document (Document), Element (Element), Node (NodeContent, NodeElement), Prologue (Prologue), def, renderText)
import qualified Text.XML (renderText)

-- | Exporte l'inventaire ACV au format XML simplifié (type ILCD)
exportInventoryAsILCD :: FilePath -> Inventory -> IO ()
exportInventoryAsILCD outPath inv = do
    let doc = Document (Prologue [] Nothing []) (toElement inv) []
    writeFile outPath $ T.unpack $ toStrict (renderText def doc)

-- | Racine XML du fichier exporté
toElement :: Inventory -> Element
toElement inv =
    Element
        "activityDataSet"
        M.empty
        [ NodeElement $
            Element
                "inventoryResult"
                M.empty
                (map toExchange (M.toList inv))
        ]

-- | Convertit un flux en un bloc <exchange>
toExchange :: (UUID, Double) -> Node
toExchange (fid, amt) =
    NodeElement $
        Element
            "exchange"
            M.empty
            [ NodeElement $ Element "flow" (M.fromList [("id", fid)]) []
            , NodeElement $ Element "amount" M.empty [NodeContent (T.pack (show amt))]
            ]
