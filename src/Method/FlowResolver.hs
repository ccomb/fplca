{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Parse ILCD flow XML files to extract baseName, compartment, and CAS.
--
-- ILCD method packages contain a @flows/@ directory with one XML per flow.
-- Each XML has: UUID, baseName, elementaryFlowCategorization (compartment),
-- and optionally a CASNumber. This module builds a lookup map from UUID
-- to enrichment data, used to annotate MethodCFs during method parsing.
module Method.FlowResolver
    ( ILCDFlowInfo(..)
    , resolveFlowDirectory
    , parseFlowDirectory
    , parseFlowXML
    ) where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Xeno.SAX as X
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeExtension)
import Data.Char (toLower)

import EcoSpold.Common (bsToText, isElement)
import EcoSpold.Parser2 (normalizeCAS)
import Method.Types (Compartment(..))

-- | Enrichment data extracted from an ILCD flow XML
data ILCDFlowInfo = ILCDFlowInfo
    { ilcdBaseName    :: !Text
    , ilcdCompartment :: !(Maybe Compartment)
    , ilcdCAS         :: !(Maybe Text)  -- normalized CAS
    , ilcdSynonyms    :: ![Text]        -- from <common:synonyms xml:lang="en">, semicolon-split
    } deriving (Show)

-- | Given a method directory (containing lciamethods/), find the sibling flows/ directory.
-- ILCD packages have structure: ILCD/{lciamethods/, flows/, ...}
resolveFlowDirectory :: FilePath -> IO (Maybe FilePath)
resolveFlowDirectory methodDir = do
    -- methodDir is the directory containing method XMLs (e.g., .../ILCD/lciamethods)
    -- flows/ is a sibling: .../ILCD/flows
    let parent = takeParentDir methodDir
        flowsDir = parent </> "flows"
    exists <- doesDirectoryExist flowsDir
    return $ if exists then Just flowsDir else Nothing
  where
    -- Go up one directory level
    takeParentDir = reverse . dropWhile (/= '/') . dropWhile (== '/') . reverse

-- | Parse all flow XMLs in a directory, returning UUID → ILCDFlowInfo map
parseFlowDirectory :: FilePath -> IO (M.Map UUID ILCDFlowInfo)
parseFlowDirectory dir = do
    files <- listDirectory dir
    let xmlFiles = [dir </> f | f <- files, map toLower (takeExtension f) == ".xml"]
    results <- mapM parseOneFile xmlFiles
    return $! M.fromList [(uuid, info) | Just (uuid, info) <- results]
  where
    parseOneFile path = do
        bytes <- BS.readFile path
        return $ parseFlowXML bytes

-- | SAX parse state for flow XML
data FlowParseState = FlowParseState
    { fpsUUID        :: !Text
    , fpsBaseName    :: !Text
    , fpsCAS         :: !Text
    , fpsCategories  :: ![Text]  -- category level="0", "1", "2" in order
    , fpsPath        :: ![BS.ByteString]
    , fpsTextAccum   :: ![BS.ByteString]
    , fpsInBaseName  :: !Bool
    , fpsSynonyms    :: ![Text]  -- accumulated synonym names
    , fpsInSynonyms  :: !Bool    -- inside <common:synonyms>
    , fpsLangIsEn    :: !Bool    -- current synonyms element has xml:lang="en"
    }

initialFlowState :: FlowParseState
initialFlowState = FlowParseState "" "" "" [] [] [] False [] False False

-- | Parse a single ILCD flow XML from bytes
parseFlowXML :: BS.ByteString -> Maybe (UUID, ILCDFlowInfo)
parseFlowXML bytes =
    case X.fold openTag attr endOpen txt closeTag cdata initialFlowState bytes of
        Left _  -> Nothing
        Right s -> buildFlowInfo s
  where
    openTag s tag =
        let !inSyn = isElement tag "synonyms" || fpsInSynonyms s
            !inBase = isElement tag "baseName" || fpsInBaseName s
        in s { fpsPath = tag : fpsPath s, fpsTextAccum = []
             , fpsInBaseName = inBase, fpsInSynonyms = inSyn
             , fpsLangIsEn = if isElement tag "synonyms" then False else fpsLangIsEn s }

    attr s name value
        | fpsInSynonyms s && isElement name "lang" && bsToText value == "en" =
            s { fpsLangIsEn = True }
        | isElement name "level" = s
        | otherwise = s

    endOpen s _ = s

    txt s content =
        let trimmed = BS.dropWhile (== 32) $ BS.dropWhileEnd (== 32) content
        in if BS.null trimmed then s
           else s { fpsTextAccum = trimmed : fpsTextAccum s }

    closeTag s tag
        | isElement tag "UUID" && T.null (fpsUUID s) =
            s { fpsPath = dropPath s, fpsUUID = accum s, fpsTextAccum = [] }
        | isElement tag "baseName" =
            s { fpsPath = dropPath s, fpsBaseName = accum s
              , fpsTextAccum = [], fpsInBaseName = False }
        | isElement tag "CASNumber" =
            s { fpsPath = dropPath s, fpsCAS = accum s, fpsTextAccum = [] }
        | isElement tag "synonyms" =
            let !syns = if fpsLangIsEn s
                        then filter (not . T.null) $ map T.strip $ T.splitOn ";" (accum s)
                        else []
            in s { fpsPath = dropPath s, fpsTextAccum = []
                 , fpsInSynonyms = False, fpsLangIsEn = False
                 , fpsSynonyms = fpsSynonyms s ++ syns }
        | isElement tag "category" =
            s { fpsPath = dropPath s
              , fpsCategories = fpsCategories s ++ [accum s]
              , fpsTextAccum = [] }
        | otherwise =
            s { fpsPath = dropPath s, fpsTextAccum = [] }

    cdata = txt

    dropPath s = case fpsPath s of
        (_:rest) -> rest
        []       -> []

    accum s = T.strip $ T.concat $ reverse $ map bsToText (fpsTextAccum s)

    buildFlowInfo s = do
        uuid <- UUID.fromText (fpsUUID s)
        let baseName = fpsBaseName s
        if T.null baseName then Nothing
        else Just (uuid, ILCDFlowInfo
            { ilcdBaseName    = baseName
            , ilcdCompartment = parseCompartment (fpsCategories s)
            , ilcdCAS         = if T.null (fpsCAS s) then Nothing
                                else Just (normalizeCAS (fpsCAS s))
            , ilcdSynonyms    = fpsSynonyms s
            })

-- | Parse compartment from ILCD category levels.
-- Level 0: "Emissions" or "Resources"
-- Level 1: "Emissions to air" → medium = "air"
-- Level 2: "Emissions to air, indoor" → subcompartment = "indoor"
parseCompartment :: [Text] -> Maybe Compartment
parseCompartment [] = Nothing
parseCompartment cats =
    let -- Level 1 typically contains medium: "Emissions to air", "Emissions to water", etc.
        medium = case drop 1 cats of
            (lvl1:_) -> extractMedium lvl1
            []       -> extractMedium (head cats)  -- cats is non-empty ([] case handled above)
        -- Level 2 is the subcompartment
        subcomp = case drop 2 cats of
            (lvl2:_) -> extractSubcompartment lvl2
            []       -> ""
    in if T.null medium then Nothing
       else Just (Compartment medium subcomp "")
  where
    -- Extract medium from "Emissions to air" → "air", "Resources from ground" → "natural resource"
    extractMedium txt
        | "Emissions to air" `T.isPrefixOf` txt      = "air"
        | "Emissions to water" `T.isPrefixOf` txt    = "water"
        | "Emissions to soil" `T.isPrefixOf` txt     = "soil"
        | "Resources" `T.isPrefixOf` txt             = "natural resource"
        | "Emissions to fresh water" `T.isPrefixOf` txt = "water"
        | "Emissions to sea water" `T.isPrefixOf` txt  = "water"
        | otherwise = T.toLower $ T.strip txt

    -- Extract subcompartment: take the last part after comma or "to X,"
    extractSubcompartment txt
        | "Emissions to air, " `T.isPrefixOf` txt =
            T.strip $ T.drop (T.length "Emissions to air, ") txt
        | "Emissions to water, " `T.isPrefixOf` txt =
            T.strip $ T.drop (T.length "Emissions to water, ") txt
        | "Emissions to soil, " `T.isPrefixOf` txt =
            T.strip $ T.drop (T.length "Emissions to soil, ") txt
        | "Emissions to " `T.isPrefixOf` txt =
            T.strip $ T.drop (T.length "Emissions to ") txt
        | "Resources " `T.isPrefixOf` txt =
            T.strip $ T.drop (T.length "Resources ") txt
        | otherwise = T.toLower $ T.strip txt
