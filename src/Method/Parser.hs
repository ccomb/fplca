{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Parser for ILCD LCIA Method XML files using Xeno SAX parser.
--
-- Parses the Environmental Footprint LCIA method format to extract
-- characterization factors for impact assessment.
module Method.Parser
    ( parseMethodFile
    , parseMethodBytes
    ) where

import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.UUID as UUID
import qualified Xeno.SAX as X

import EcoSpold.Common (bsToText, isElement)
import Method.Types

-- | Parse an ILCD LCIA Method XML file
parseMethodFile :: FilePath -> IO (Either String Method)
parseMethodFile path = do
    bytes <- BS.readFile path
    return $ parseMethodBytes bytes

-- | Parse ILCD LCIA Method XML from ByteString
parseMethodBytes :: BS.ByteString -> Either String Method
parseMethodBytes bytes =
    case X.fold openTag attribute endOpen textHandler closeTag cdataHandler initialState bytes of
        Left err -> Left $ "XML parse error: " ++ show err
        Right state -> buildMethod state

-- | Parser state accumulator
data ParseState = ParseState
    { psMethodId      :: !Text
    , psMethodName    :: !Text
    , psDescription   :: !Text
    , psUnit          :: !Text
    , psCategory      :: !Text
    , psMethodology   :: !Text
    , psFactors       :: ![MethodCF]
    -- Current factor being parsed
    , psCurrentFlowId :: !Text
    , psCurrentFlowName :: !Text
    , psCurrentDirection :: !Text
    , psCurrentValue  :: !Text
    -- Context tracking
    , psPath          :: ![BS.ByteString]  -- Stack of element names
    , psInFactor      :: !Bool
    , psInRefQuantity :: !Bool             -- Inside referenceQuantity element
    , psTextAccum     :: ![BS.ByteString]  -- Accumulated text content
    }

initialState :: ParseState
initialState = ParseState
    { psMethodId = ""
    , psMethodName = ""
    , psDescription = ""
    , psUnit = ""
    , psCategory = ""
    , psMethodology = ""
    , psFactors = []
    , psCurrentFlowId = ""
    , psCurrentFlowName = ""
    , psCurrentDirection = ""
    , psCurrentValue = ""
    , psPath = []
    , psInFactor = False
    , psInRefQuantity = False
    , psTextAccum = []
    }

-- | Handle opening tags - update path and context
openTag :: ParseState -> BS.ByteString -> ParseState
openTag state tagName =
    let newPath = tagName : psPath state
        newInFactor = psInFactor state || isElement tagName "factor"
        newInRefQuantity = psInRefQuantity state || isElement tagName "referenceQuantity"
    in state { psPath = newPath
             , psInFactor = newInFactor
             , psInRefQuantity = newInRefQuantity
             , psTextAccum = []
             }

-- | Handle attributes
attribute :: ParseState -> BS.ByteString -> BS.ByteString -> ParseState
attribute state attrName attrValue
    -- Capture flow UUID from referenceToFlowDataSet
    | psInFactor state && attrName == "refObjectId" && inFlowRef =
        state { psCurrentFlowId = bsToText attrValue }
    | otherwise = state
  where
    inFlowRef = case psPath state of
        (x:_) -> isElement x "referenceToFlowDataSet"
        _ -> False

-- | Handle end of opening tag (after all attributes processed)
endOpen :: ParseState -> BS.ByteString -> ParseState
endOpen state _tagName = state

-- | Handle text content
textHandler :: ParseState -> BS.ByteString -> ParseState
textHandler state text =
    state { psTextAccum = text : psTextAccum state }

-- | Handle CDATA sections (treat same as text)
cdataHandler :: ParseState -> BS.ByteString -> ParseState
cdataHandler = textHandler

-- | Handle closing tags
closeTag :: ParseState -> BS.ByteString -> ParseState
closeTag state tagName
    -- End of factor element - build the CF
    | isElement tagName "factor" && psInFactor state =
        let cf = MethodCF
                { mcfFlowRef = parseUUIDSafe (psCurrentFlowId state)
                , mcfFlowName = extractFlowName (psCurrentFlowName state)
                , mcfDirection = parseDirection (psCurrentDirection state)
                , mcfValue = parseDoubleSafe (psCurrentValue state)
                }
        in state { psPath = dropPath
                 , psInFactor = False
                 , psFactors = cf : psFactors state
                 , psCurrentFlowId = ""
                 , psCurrentFlowName = ""
                 , psCurrentDirection = ""
                 , psCurrentValue = ""
                 , psTextAccum = []
                 }

    -- UUID element - capture method UUID (only the first one, not flow UUIDs)
    | isElement tagName "UUID" && T.null (psMethodId state) && not (psInFactor state) =
        state { psPath = dropPath
              , psMethodId = accumulatedText
              , psTextAccum = []
              }

    -- Method name (only first one at top level)
    | isElement tagName "name" && T.null (psMethodName state) && not (psInFactor state) =
        state { psPath = dropPath
              , psMethodName = accumulatedText
              , psTextAccum = []
              }

    -- General comment (description)
    | isElement tagName "generalComment" =
        state { psPath = dropPath
              , psDescription = accumulatedText
              , psTextAccum = []
              }

    -- Impact category
    | isElement tagName "impactCategory" =
        state { psPath = dropPath
              , psCategory = accumulatedText
              , psTextAccum = []
              }

    -- Methodology
    | isElement tagName "methodology" =
        state { psPath = dropPath
              , psMethodology = accumulatedText
              , psTextAccum = []
              }

    -- Short description - could be unit or flow name depending on context
    | isElement tagName "shortDescription" =
        if psInFactor state
            then state { psPath = dropPath
                       , psCurrentFlowName = accumulatedText
                       , psTextAccum = []
                       }
            -- Only capture unit when inside referenceQuantity element
            else if psInRefQuantity state && T.null (psUnit state)
                then state { psPath = dropPath
                           , psUnit = accumulatedText
                           , psTextAccum = []
                           }
                else state { psPath = dropPath, psTextAccum = [] }

    -- End of referenceQuantity element
    | isElement tagName "referenceQuantity" =
        state { psPath = dropPath
              , psInRefQuantity = False
              , psTextAccum = []
              }

    -- Exchange direction (Input/Output)
    | isElement tagName "exchangeDirection" =
        state { psPath = dropPath
              , psCurrentDirection = accumulatedText
              , psTextAccum = []
              }

    -- Mean value (CF value)
    | isElement tagName "meanValue" =
        state { psPath = dropPath
              , psCurrentValue = accumulatedText
              , psTextAccum = []
              }

    -- Default: just pop the path
    | otherwise =
        state { psPath = dropPath, psTextAccum = [] }
  where
    dropPath = case psPath state of
        (_:rest) -> rest
        [] -> []
    accumulatedText = T.strip $ T.concat $ reverse $ map bsToText (psTextAccum state)

-- | Build the final Method from parsed state
buildMethod :: ParseState -> Either String Method
buildMethod state = do
    when (T.null (psMethodId state)) $
        Left "Missing method UUID"
    when (T.null (psMethodName state)) $
        Left "Missing method name"

    methodUUID <- case UUID.fromText (psMethodId state) of
        Just u -> Right u
        Nothing -> Left $ "Invalid method UUID: " ++ T.unpack (psMethodId state)

    Right $ Method
        { methodId = methodUUID
        , methodName = psMethodName state
        , methodDescription = if T.null (psDescription state)
                                then Nothing
                                else Just (psDescription state)
        , methodUnit = if T.null (psUnit state) then "unknown" else psUnit state
        , methodCategory = if T.null (psCategory state) then "unknown" else psCategory state
        , methodMethodology = if T.null (psMethodology state)
                                then Nothing
                                else Just (psMethodology state)
        , methodFactors = reverse (psFactors state)  -- Restore original order
        }

-- | Parse UUID from text, return nil UUID on failure
parseUUIDSafe :: Text -> UUID.UUID
parseUUIDSafe txt = fromMaybe UUID.nil (UUID.fromText txt)

-- | Parse Double from text, return 0 on failure
parseDoubleSafe :: Text -> Double
parseDoubleSafe txt = case TR.double txt of
    Right (val, _) -> val
    Left _ -> 0.0

-- | Parse flow direction from text
parseDirection :: Text -> FlowDirection
parseDirection txt
    | "Input" `T.isPrefixOf` txt = Input
    | "input" `T.isPrefixOf` txt = Input
    | otherwise = Output

-- | Extract flow name from shortDescription
-- Format: "flow name (Mass, kg, Emissions to ...)"
extractFlowName :: Text -> Text
extractFlowName txt =
    let -- Find first opening paren
        nameEnd = T.takeWhile (/= '(') txt
    in T.strip nameEnd
