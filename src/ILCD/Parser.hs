{-# LANGUAGE OverloadedStrings #-}

module ILCD.Parser (parseMethodFromFile) where

import           Data.Text (Text)
import qualified Data.Text as T
import           Text.XML
import           Text.XML.Cursor
import           ACV.Types

-- | Parse un fichier de méthode ILCD (ex : PEF) et extrait la liste des CF
parseMethodFromFile :: FilePath -> IO [CF]
parseMethodFromFile path = do
  doc <- Text.XML.readFile def path
  let cursor = fromDocument doc
  return $ parseMethod cursor

-- | Parse l'ensemble des catégories d'impact
parseMethod :: Cursor -> [CF]
parseMethod c = concatMap parseCategory (c $// element "impactCategory")

-- | Parse une catégorie d'impact et ses CF
parseCategory :: Cursor -> [CF]
parseCategory catCursor =
  let catName  = T.unpack . head $ catCursor $/ element "name" &/ content
      category = ImpactCategory catName catName
  in map (parseCF category) (catCursor $/ element "CF")

-- | Parse un facteur de caractérisation
parseCF :: ImpactCategory -> Cursor -> CF
parseCF category cfCursor =
  let fid    = T.unpack . head $ cfCursor $/ element "flowId" &/ content
      factor = read . T.unpack . head $ cfCursor $/ element "meanValue" &/ content
  in CF fid category factor

