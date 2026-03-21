{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Parser for SimaPro method CSV exports.
--
-- SimaPro can export LCIA methods as CSV files with file type @{methods}@.
-- Each file contains one method with multiple impact categories, each listing
-- characterization factors as substance rows.
module Method.ParserSimaPro
    ( parseSimaProMethodCSV
    , parseSimaProMethodCSVBytes
    , isSimaProMethodCSV
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.UUID (UUID)
import qualified Data.UUID.V5 as UUID5
import Data.Char (toLower)

import Method.Types
import SimaPro.Parser (SimaProConfig(..), defaultConfig, simaproNamespace,
                       ensureUtf8, splitCSV, parseAmount, decodeBS)

-- ============================================================================
-- Public API
-- ============================================================================

-- | Parse a SimaPro method CSV file from disk.
parseSimaProMethodCSV :: FilePath -> IO (Either String [Method])
parseSimaProMethodCSV path = parseSimaProMethodCSVBytes <$> BS.readFile path

-- | Pure parser for SimaPro method CSV bytes.
parseSimaProMethodCSVBytes :: BS.ByteString -> Either String [Method]
parseSimaProMethodCSVBytes raw =
    let !utf8 = ensureUtf8 raw
        lns = BS8.lines utf8
        cfg = parseConfig lns
        methodName' = parseMethodName cfg lns
        result = foldl' (step cfg methodName') initState lns
    in Right (finalize result)

-- | Detect whether bytes are a SimaPro method CSV export.
isSimaProMethodCSV :: BS.ByteString -> Bool
isSimaProMethodCSV bs =
    BS8.isPrefixOf "{SimaPro" bs && "{methods}" `BS.isInfixOf` BS.take 200 bs

-- ============================================================================
-- Parser State
-- ============================================================================

data ParseState = ParseState
    { psPhase      :: !Phase
    , psCatName    :: !Text          -- current impact category name
    , psCatUnit    :: !Text          -- current impact category unit
    , psFactors    :: ![MethodCF]    -- CFs accumulated (reversed) for current category
    , psMethods    :: ![Method]      -- completed methods (reversed)
    }

data Phase
    = PhHeader          -- reading {key: value} header lines
    | PhMethodMeta      -- reading method-level metadata (Name, Version, etc.)
    | PhExpectCategory  -- expecting "Impact category" or "Damage category" etc.
    | PhExpectCatLine   -- expecting the "Name;Unit" line after "Impact category"
    | PhExpectSubst     -- expecting blank or "Substances" marker
    | PhReadingCFs      -- reading substance/CF rows
    | PhSkip            -- in Damage/Normalization/Weighting sections (ignored)

initState :: ParseState
initState = ParseState PhHeader "" "" [] []

-- ============================================================================
-- State Machine
-- ============================================================================

step :: SimaProConfig -> Text -> ParseState -> BS.ByteString -> ParseState
step cfg methodologyName st line = case psPhase st of
    PhHeader
        | BS8.isPrefixOf "{" line -> st  -- skip header lines
        | isBlank line -> st { psPhase = PhMethodMeta }
        | otherwise -> st { psPhase = PhMethodMeta }

    PhMethodMeta
        | stripped == "Impact category" -> st { psPhase = PhExpectCatLine }
        | stripped == "Damage category" -> st { psPhase = PhSkip }
        | BS8.isPrefixOf "Normalization" stripped -> st { psPhase = PhSkip }
        | BS8.isPrefixOf "Weighting" stripped -> st { psPhase = PhSkip }
        | otherwise -> st  -- skip metadata lines (Name, Version, Comment, etc.)

    PhExpectCategory
        | stripped == "Impact category" -> st { psPhase = PhExpectCatLine }
        | stripped == "Damage category" -> st { psPhase = PhSkip }
        | BS8.isPrefixOf "Normalization" stripped -> st { psPhase = PhSkip }
        | BS8.isPrefixOf "Weighting" stripped -> st { psPhase = PhSkip }
        | stripped == "End" -> st
        | otherwise -> st

    PhExpectCatLine
        | isBlank line -> st
        | otherwise ->
            let fields = splitCSV (spDelimiter cfg) line
                catName = decodeBS (BS8.strip (head' fields))
                catUnit = if length fields > 1
                          then decodeBS (BS8.strip (fields !! 1))
                          else ""
            in st { psPhase = PhExpectSubst
                  , psCatName = catName
                  , psCatUnit = catUnit
                  , psFactors = []
                  }

    PhExpectSubst
        | isBlank line -> st
        | stripped == "Substances" -> st { psPhase = PhReadingCFs }
        | otherwise -> st  -- unexpected, skip

    PhReadingCFs
        | isBlank line -> finishCategory st
        | stripped == "Impact category" ->
            (finishCategory st) { psPhase = PhExpectCatLine }
        | stripped == "Damage category" ->
            (finishCategory st) { psPhase = PhSkip }
        | BS8.isPrefixOf "Normalization" stripped ->
            (finishCategory st) { psPhase = PhSkip }
        | stripped == "End" -> finishCategory st
        | otherwise ->
            let fields = splitCSV (spDelimiter cfg) line
            in case fields of
                (comp:sub:name:cas:cfVal:_unit:_) ->
                    let !cf = MethodCF
                            { mcfFlowRef     = makeFlowUUID name comp sub
                            , mcfFlowName    = decodeBS (BS8.strip name)
                            , mcfDirection   = direction comp
                            , mcfValue       = parseAmount (spDecimal cfg) (BS8.strip cfVal)
                            , mcfCompartment = mkCompartment comp sub
                            , mcfCAS         = normalizeCAS (decodeBS (BS8.strip cas))
                            }
                    in st { psFactors = cf : psFactors st }
                _ -> st  -- malformed row, skip

    PhSkip
        | stripped == "Impact category" ->
            st { psPhase = PhExpectCatLine }
        | stripped == "End" -> st
        | otherwise -> st
  where
    stripped = BS8.strip line
    methodologyName' = methodologyName

    finishCategory s =
        let !m = Method
                { methodId          = UUID5.generateNamed simaproNamespace
                    (BS.unpack $ TE.encodeUtf8 $ "method:" <> psCatName s)
                , methodName        = psCatName s
                , methodDescription = Nothing
                , methodUnit        = psCatUnit s
                , methodCategory    = psCatName s
                , methodMethodology = Just methodologyName'
                , methodFactors     = reverse (psFactors s)
                }
        in s { psPhase = PhExpectCategory
             , psFactors = []
             , psMethods = m : psMethods s
             }

finalize :: ParseState -> [Method]
finalize st = case psPhase st of
    PhReadingCFs | not (null (psFactors st)) ->
        -- flush last category if file ended without trailing blank
        let !m = Method
                { methodId          = UUID5.generateNamed simaproNamespace
                    (BS.unpack $ TE.encodeUtf8 $ "method:" <> psCatName st)
                , methodName        = psCatName st
                , methodDescription = Nothing
                , methodUnit        = psCatUnit st
                , methodCategory    = psCatName st
                , methodMethodology = Nothing
                , methodFactors     = reverse (psFactors st)
                }
        in reverse (m : psMethods st)
    _ -> reverse (psMethods st)

-- ============================================================================
-- Helpers
-- ============================================================================

-- | Parse SimaPro config from header lines.
parseConfig :: [BS.ByteString] -> SimaProConfig
parseConfig = foldl' go defaultConfig
  where
    go cfg line
        | "{CSV separator: Semicolon}" `BS.isInfixOf` line = cfg { spDelimiter = ';' }
        | "{CSV separator: Comma}" `BS.isInfixOf` line     = cfg { spDelimiter = ',' }
        | "{CSV separator: Tab}" `BS.isInfixOf` line       = cfg { spDelimiter = '\t' }
        | "{Decimal separator: .}" `BS.isInfixOf` line     = cfg { spDecimal = '.' }
        | "{Decimal separator: ,}" `BS.isInfixOf` line     = cfg { spDecimal = ',' }
        | otherwise = cfg

-- | Extract the method name from the metadata section.
parseMethodName :: SimaProConfig -> [BS.ByteString] -> Text
parseMethodName _cfg = go False
  where
    go _ [] = "SimaPro Method"
    go True (l:_) = decodeBS (BS8.strip l)
    go False (l:ls)
        | BS8.strip l == "Name" = go True ls
        | otherwise = go False ls

isBlank :: BS.ByteString -> Bool
isBlank = BS.null . BS8.strip

head' :: [a] -> a
head' (x:_) = x
head' [] = error "Method.ParserSimaPro: unexpected empty field list"

-- | Generate flow UUID matching the SimaPro convention.
makeFlowUUID :: BS.ByteString -> BS.ByteString -> BS.ByteString -> UUID
makeFlowUUID name comp sub =
    let compartment = decodeBS (BS8.strip comp) <> "/" <> decodeBS (BS8.strip sub)
    in UUID5.generateNamed simaproNamespace
        (BS.unpack $ TE.encodeUtf8 $ "flow:" <> decodeBS (BS8.strip name) <> ":" <> compartment <> ":" <> "kg")

-- | Map compartment string to FlowDirection.
direction :: BS.ByteString -> FlowDirection
direction comp
    | lc == "raw" || lc == "resources" || "raw" `BS8.isPrefixOf` lc = Input
    | otherwise = Output
  where lc = BS8.map toLower (BS8.strip comp)

-- | Build Compartment from SimaPro compartment/subcompartment.
mkCompartment :: BS.ByteString -> BS.ByteString -> Maybe Compartment
mkCompartment comp sub =
    let medium = case BS8.map toLower (BS8.strip comp) of
            "air"       -> "air"
            "water"     -> "water"
            "soil"      -> "soil"
            "raw"       -> "natural resource"
            "resources" -> "natural resource"
            c           -> decodeBS c
        subcomp = let s = decodeBS (BS8.strip sub)
                  in if s == "(unspecified)" then "" else s
    in Just (Compartment medium subcomp "")

-- | Normalize CAS number: strip leading zeros from each segment.
-- "007664-41-7" -> "7664-41-7", empty/missing -> Nothing
normalizeCAS :: Text -> Maybe Text
normalizeCAS cas
    | T.null cas = Nothing
    | otherwise =
        let segments = T.splitOn "-" cas
            stripped = map (T.dropWhile (== '0')) segments
            fixed = map (\s -> if T.null s then "0" else s) stripped
            result = T.intercalate "-" fixed
        in if T.all (\c -> c == '-' || c == '0') cas then Nothing
           else Just result

