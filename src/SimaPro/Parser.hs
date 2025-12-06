{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | SimaPro CSV Parser for fplca
-- Parses SimaPro CSV exports (like Agribalyse) into fplca data structures
module SimaPro.Parser
    ( parseSimaProCSV
    , SimaProConfig(..)
    , ProcessBlock(..)
    , ProductRow(..)
    , TechExchangeRow(..)
    , BioExchangeRow(..)
    , generateActivityUUID
    , generateFlowUUID
    , generateUnitUUID
    ) where

import LCA.Types
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.List (foldl')
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Data.Time (diffUTCTime, getCurrentTime)
import qualified Data.UUID as UUID
import qualified Data.UUID.V5 as UUID5
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

-- ============================================================================
-- Configuration Types
-- ============================================================================

-- | SimaPro file configuration extracted from header
data SimaProConfig = SimaProConfig
    { spVersion :: !Text           -- SimaPro version (e.g. "9.6.0.1")
    , spFileType :: !Text          -- "processes", "methods", "product stages"
    , spDelimiter :: !Char         -- CSV delimiter (';', ',', '\t')
    , spDecimal :: !Char           -- Decimal separator (',' or '.')
    , spDateFormat :: !Text        -- Date format string
    }
    deriving (Show, Eq)

-- | Default configuration (semicolon delimiter, comma decimal)
defaultConfig :: SimaProConfig
defaultConfig = SimaProConfig
    { spVersion = ""
    , spFileType = "processes"
    , spDelimiter = ';'
    , spDecimal = ','
    , spDateFormat = "dd/MM/yyyy"
    }

-- ============================================================================
-- Intermediate Row Types
-- ============================================================================

-- | Product row (reference output)
data ProductRow = ProductRow
    { prName :: !Text
    , prUnit :: !Text
    , prAmount :: !Double
    , prAllocation :: !Double
    , prWasteType :: !Text
    , prCategory :: !Text
    , prComment :: !Text
    }
    deriving (Show, Eq)

-- | Technosphere exchange row (inputs from other processes)
data TechExchangeRow = TechExchangeRow
    { terName :: !Text
    , terUnit :: !Text
    , terAmount :: !Double
    , terUncertainty :: !Text
    , terComment :: !Text
    }
    deriving (Show, Eq)

-- | Biosphere exchange row (emissions/resources)
data BioExchangeRow = BioExchangeRow
    { berName :: !Text
    , berCompartment :: !Text
    , berUnit :: !Text
    , berAmount :: !Double
    , berUncertainty :: !Text
    , berComment :: !Text
    }
    deriving (Show, Eq)

-- ============================================================================
-- Process Block Accumulator
-- ============================================================================

-- | Accumulated data for a single process block
data ProcessBlock = ProcessBlock
    { pbIdentifier :: !Text
    , pbName :: !Text
    , pbCategoryType :: !Text
    , pbType :: !Text             -- "Unit process" or "System"
    , pbLocation :: !Text
    , pbStatus :: !Text
    , pbTimePeriod :: !Text
    , pbTechnology :: !Text
    , pbRecord :: !Text
    , pbComment :: !Text
    , pbProducts :: ![ProductRow]
    , pbAvoidedProducts :: ![ProductRow]
    , pbMaterials :: ![TechExchangeRow]
    , pbElectricity :: ![TechExchangeRow]
    , pbWasteToTreatment :: ![TechExchangeRow]
    , pbResources :: ![BioExchangeRow]
    , pbEmissionsAir :: ![BioExchangeRow]
    , pbEmissionsWater :: ![BioExchangeRow]
    , pbEmissionsSoil :: ![BioExchangeRow]
    , pbFinalWaste :: ![BioExchangeRow]
    }
    deriving (Show, Eq)

-- | Empty process block
emptyProcessBlock :: ProcessBlock
emptyProcessBlock = ProcessBlock
    { pbIdentifier = ""
    , pbName = ""
    , pbCategoryType = ""
    , pbType = ""
    , pbLocation = ""
    , pbStatus = ""
    , pbTimePeriod = ""
    , pbTechnology = ""
    , pbRecord = ""
    , pbComment = ""
    , pbProducts = []
    , pbAvoidedProducts = []
    , pbMaterials = []
    , pbElectricity = []
    , pbWasteToTreatment = []
    , pbResources = []
    , pbEmissionsAir = []
    , pbEmissionsWater = []
    , pbEmissionsSoil = []
    , pbFinalWaste = []
    }

-- ============================================================================
-- Parser State Machine
-- ============================================================================

-- | Section types within a process block
data SectionType
    = SecProducts
    | SecAvoidedProducts
    | SecMaterials
    | SecElectricity
    | SecWasteToTreatment
    | SecResources
    | SecEmissionsAir
    | SecEmissionsWater
    | SecEmissionsSoil
    | SecFinalWaste
    | SecInputParams
    | SecCalcParams
    | SecNone
    deriving (Show, Eq)

-- | Parser state
data ParseState
    = InHeader
    | InProcessMeta !Text  -- Current metadata key being read
    | InSection !SectionType
    | BetweenBlocks
    deriving (Show, Eq)

-- | Parse state accumulator
data ParseAcc = ParseAcc
    { paConfig :: !SimaProConfig
    , paState :: !ParseState
    , paCurrentBlock :: !ProcessBlock
    , paBlocks :: ![ProcessBlock]
    , paLineNum :: !Int
    }

-- ============================================================================
-- Header Parsing
-- ============================================================================

-- | Parse a header line like "{key: value}" or "{value}"
parseHeaderLine :: Text -> Maybe (Text, Text)
parseHeaderLine line
    | T.isPrefixOf "{" line && T.isSuffixOf "}" line =
        let content = T.dropEnd 1 (T.drop 1 line)
        in case T.breakOn ": " content of
            (key, rest) | not (T.null rest) -> Just (T.strip key, T.strip (T.drop 2 rest))
            _ -> Just (T.strip content, "")
    | otherwise = Nothing

-- | Update config from header line
updateConfigFromHeader :: SimaProConfig -> Text -> Text -> SimaProConfig
updateConfigFromHeader cfg key value = case T.toLower key of
    k | "simapro" `T.isPrefixOf` k -> cfg { spVersion = key }
    "processes" -> cfg { spFileType = "processes" }
    "methods" -> cfg { spFileType = "methods" }
    "product stages" -> cfg { spFileType = "product stages" }
    "csv separator" -> cfg { spDelimiter = parseDelimiter value }
    "decimal separator" -> cfg { spDecimal = if T.null value then ',' else T.head value }
    "short date format" -> cfg { spDateFormat = value }
    _ -> cfg
  where
    parseDelimiter v = case T.toLower v of
        "semicolon" -> ';'
        "comma" -> ','
        "tab" -> '\t'
        _ -> ';'

-- ============================================================================
-- Section Detection
-- ============================================================================

-- | Detect section type from line
detectSection :: Text -> Maybe SectionType
detectSection line = case T.strip line of
    "Products" -> Just SecProducts
    "Avoided products" -> Just SecAvoidedProducts
    "Materials/fuels" -> Just SecMaterials
    "Electricity/heat" -> Just SecElectricity
    "Waste to treatment" -> Just SecWasteToTreatment
    "Resources" -> Just SecResources
    "Emissions to air" -> Just SecEmissionsAir
    "Emissions to water" -> Just SecEmissionsWater
    "Emissions to soil" -> Just SecEmissionsSoil
    "Final waste flows" -> Just SecFinalWaste
    "Input parameters" -> Just SecInputParams
    "Calculated parameters" -> Just SecCalcParams
    "Non material emissions" -> Just SecNone  -- Ignore
    "Social issues" -> Just SecNone
    "Economic issues" -> Just SecNone
    _ -> Nothing

-- | Known metadata keys in process block
isMetadataKey :: Text -> Bool
isMetadataKey key = key `elem`
    [ "Category type", "Process identifier", "Type", "Process name"
    , "Status", "Time period", "Geography", "Technology"
    , "Representativeness", "Multiple output allocation", "Substitution allocation"
    , "Cut off rules", "Capital goods", "Boundary with nature"
    , "Infrastructure", "Date", "Record", "Generator"
    , "External documents", "Literature references", "Collection method"
    , "Data treatment", "Verification", "Comment", "Allocation rules"
    , "System description", "PlatformId"
    ]

-- ============================================================================
-- Row Parsing
-- ============================================================================

-- | Parse amount with configurable decimal separator
parseAmount :: Char -> Text -> Double
parseAmount decimalSep txt
    | T.null txt = 0.0
    | otherwise =
        let normalized = if decimalSep == ','
                         then T.replace "," "." txt
                         else txt
        in case reads (T.unpack normalized) of
            [(val, "")] -> val
            [(val, _)] -> val  -- Allow trailing characters
            _ -> 0.0

-- | Split a CSV line by delimiter (simple, doesn't handle quoted fields)
splitCSV :: Char -> Text -> [Text]
splitCSV delim = T.split (== delim)

-- | Parse a product row
parseProductRow :: SimaProConfig -> Text -> Maybe ProductRow
parseProductRow cfg line =
    let fields = splitCSV (spDelimiter cfg) line
    in case fields of
        (name:unit:amount:alloc:waste:cat:rest) -> Just ProductRow
            { prName = T.strip name
            , prUnit = T.strip unit
            , prAmount = parseAmount (spDecimal cfg) (T.strip amount)
            , prAllocation = parseAmount (spDecimal cfg) (T.strip alloc)
            , prWasteType = T.strip waste
            , prCategory = T.strip cat
            , prComment = T.intercalate ";" rest
            }
        _ -> Nothing

-- | Parse a technosphere exchange row
parseTechRow :: SimaProConfig -> Text -> Maybe TechExchangeRow
parseTechRow cfg line =
    let fields = splitCSV (spDelimiter cfg) line
    in case fields of
        (name:unit:amount:unc:_:_:_:rest) -> Just TechExchangeRow
            { terName = T.strip name
            , terUnit = T.strip unit
            , terAmount = parseAmount (spDecimal cfg) (T.strip amount)
            , terUncertainty = T.strip unc
            , terComment = T.intercalate ";" rest
            }
        (name:unit:amount:rest) -> Just TechExchangeRow
            { terName = T.strip name
            , terUnit = T.strip unit
            , terAmount = parseAmount (spDecimal cfg) (T.strip amount)
            , terUncertainty = ""
            , terComment = T.intercalate ";" rest
            }
        _ -> Nothing

-- | Parse a biosphere exchange row
parseBioRow :: SimaProConfig -> Text -> Maybe BioExchangeRow
parseBioRow cfg line =
    let fields = splitCSV (spDelimiter cfg) line
    in case fields of
        (name:compartment:unit:amount:unc:_:_:_:rest) -> Just BioExchangeRow
            { berName = T.strip name
            , berCompartment = T.strip compartment
            , berUnit = T.strip unit
            , berAmount = parseAmount (spDecimal cfg) (T.strip amount)
            , berUncertainty = T.strip unc
            , berComment = T.intercalate ";" rest
            }
        (name:compartment:unit:amount:rest) -> Just BioExchangeRow
            { berName = T.strip name
            , berCompartment = T.strip compartment
            , berUnit = T.strip unit
            , berAmount = parseAmount (spDecimal cfg) (T.strip amount)
            , berUncertainty = ""
            , berComment = T.intercalate ";" rest
            }
        _ -> Nothing

-- ============================================================================
-- State Machine Processing
-- ============================================================================

-- | Process a single line
processLine :: ParseAcc -> Text -> ParseAcc
processLine acc@ParseAcc{..} line
    -- Empty line handling
    | T.null (T.strip line) = case paState of
        InProcessMeta _ -> acc { paState = BetweenBlocks }
        InSection _ -> acc { paState = BetweenBlocks }
        _ -> acc

    -- Header lines
    | Just (key, value) <- parseHeaderLine line, paState == InHeader =
        acc { paConfig = updateConfigFromHeader paConfig key value }

    -- Process block start
    | T.strip line == "Process" =
        acc { paState = BetweenBlocks
            , paCurrentBlock = emptyProcessBlock
            }

    -- End of block
    | T.strip line == "End" =
        let block = paCurrentBlock
            -- A block is valid if it has at least one product (process name not required)
            isValid = not (null (pbProducts block))
        in acc { paState = BetweenBlocks
               , paBlocks = if isValid then block : paBlocks else paBlocks
               , paCurrentBlock = emptyProcessBlock
               }

    -- Section detection
    | Just sec <- detectSection line =
        acc { paState = InSection sec }

    -- In a section, parse row
    | InSection sec <- paState, not (T.null (T.strip line)) =
        acc { paCurrentBlock = addRowToBlock paConfig sec line paCurrentBlock }

    -- Metadata key-value pairs
    | paState == BetweenBlocks || isMetadataKey (T.strip line) =
        if isMetadataKey (T.strip line)
        then acc { paState = InProcessMeta (T.strip line) }
        else case paState of
            InProcessMeta key -> acc { paCurrentBlock = setMetadata key line paCurrentBlock
                                     , paState = BetweenBlocks
                                     }
            _ -> acc

    -- Value for metadata key
    | InProcessMeta key <- paState =
        acc { paCurrentBlock = setMetadata key line paCurrentBlock
            , paState = BetweenBlocks
            }

    | otherwise = acc { paLineNum = paLineNum + 1 }

-- | Add a row to the appropriate list in the block
addRowToBlock :: SimaProConfig -> SectionType -> Text -> ProcessBlock -> ProcessBlock
addRowToBlock cfg sec line block = case sec of
    SecProducts -> case parseProductRow cfg line of
        Just row -> block { pbProducts = row : pbProducts block }
        Nothing -> block
    SecAvoidedProducts -> case parseProductRow cfg line of
        Just row -> block { pbAvoidedProducts = row : pbAvoidedProducts block }
        Nothing -> block
    SecMaterials -> case parseTechRow cfg line of
        Just row -> block { pbMaterials = row : pbMaterials block }
        Nothing -> block
    SecElectricity -> case parseTechRow cfg line of
        Just row -> block { pbElectricity = row : pbElectricity block }
        Nothing -> block
    SecWasteToTreatment -> case parseTechRow cfg line of
        Just row -> block { pbWasteToTreatment = row : pbWasteToTreatment block }
        Nothing -> block
    SecResources -> case parseBioRow cfg line of
        Just row -> block { pbResources = row : pbResources block }
        Nothing -> block
    SecEmissionsAir -> case parseBioRow cfg line of
        Just row -> block { pbEmissionsAir = row : pbEmissionsAir block }
        Nothing -> block
    SecEmissionsWater -> case parseBioRow cfg line of
        Just row -> block { pbEmissionsWater = row : pbEmissionsWater block }
        Nothing -> block
    SecEmissionsSoil -> case parseBioRow cfg line of
        Just row -> block { pbEmissionsSoil = row : pbEmissionsSoil block }
        Nothing -> block
    SecFinalWaste -> case parseBioRow cfg line of
        Just row -> block { pbFinalWaste = row : pbFinalWaste block }
        Nothing -> block
    _ -> block

-- | Set metadata field in block
setMetadata :: Text -> Text -> ProcessBlock -> ProcessBlock
setMetadata key value block = case key of
    "Category type" -> block { pbCategoryType = T.strip value }
    "Process identifier" -> block { pbIdentifier = T.strip value }
    "Type" -> block { pbType = T.strip value }
    "Process name" -> block { pbName = T.strip value }
    "Status" -> block { pbStatus = T.strip value }
    "Time period" -> block { pbTimePeriod = T.strip value }
    "Geography" -> block { pbLocation = T.strip value }
    "Technology" -> block { pbTechnology = T.strip value }
    "Record" -> block { pbRecord = T.strip value }
    "Comment" -> block { pbComment = T.strip value }
    _ -> block

-- ============================================================================
-- UUID Generation
-- ============================================================================

-- | Namespace for SimaPro UUIDs
simaproNamespace :: UUID
simaproNamespace = UUID5.generateNamed UUID5.namespaceURL (BS.unpack $ TE.encodeUtf8 "simapro.pre.nl")

-- | Generate deterministic activity UUID from identifier
generateActivityUUID :: Text -> UUID
generateActivityUUID identifier =
    UUID5.generateNamed simaproNamespace (BS.unpack $ TE.encodeUtf8 $ "activity:" <> identifier)

-- | Generate deterministic flow UUID from name, compartment, unit
generateFlowUUID :: Text -> Text -> Text -> UUID
generateFlowUUID name compartment unit =
    UUID5.generateNamed simaproNamespace (BS.unpack $ TE.encodeUtf8 $ "flow:" <> name <> ":" <> compartment <> ":" <> unit)

-- | Generate deterministic unit UUID from name
generateUnitUUID :: Text -> UUID
generateUnitUUID unitName =
    UUID5.generateNamed simaproNamespace (BS.unpack $ TE.encodeUtf8 $ "unit:" <> unitName)

-- ============================================================================
-- Conversion to fplca Types
-- ============================================================================

-- | Extract location from SimaPro-style names
-- Handles: "Name {FR}", "Name {FR}| market for...", "Name {FR} U"
extractLocation :: Text -> (Text, Text)
extractLocation name =
    case T.breakOn "{" name of
        (_, rest) | not (T.null rest) ->
            case T.breakOn "}" (T.drop 1 rest) of
                (loc, afterBrace) | not (T.null afterBrace) || not (T.null loc) ->
                    -- Found {LOC} pattern - extract location
                    let cleanLoc = T.strip loc
                    in if T.length cleanLoc <= 5 && T.length cleanLoc >= 2  -- Valid: FR, GLO, RER, etc.
                       then (T.strip name, cleanLoc)
                       else (name, "")  -- Probably not a location code
                _ -> (name, "")
        _ -> (name, "")

-- | Convert ProcessBlock to list of Activities (one per product)
-- This matches EcoSpold behavior where multi-product processes create multiple activities
processBlockToActivity :: ProcessBlock -> [(Activity, [Flow], [Unit])]
processBlockToActivity ProcessBlock{..} =
    let -- Extract location from process name if not specified
        (_, locFromName) = extractLocation pbName
        location = if T.null pbLocation then locFromName else pbLocation

        -- Convert avoided products (shared across all activities)
        avoidedExchanges = map (productToExchange False) pbAvoidedProducts

        -- Convert technosphere inputs (shared across all activities)
        techExchanges = concatMap (techRowToExchange True) (pbMaterials ++ pbElectricity)
        wasteExchanges = concatMap (techRowToExchange False) pbWasteToTreatment

        -- Convert biosphere exchanges (shared across all activities)
        resourceExchanges = map (bioRowToExchange True "resource") pbResources
        airExchanges = map (bioRowToExchange False "air") pbEmissionsAir
        waterExchanges = map (bioRowToExchange False "water") pbEmissionsWater
        soilExchanges = map (bioRowToExchange False "soil") pbEmissionsSoil
        wasteFlowExchanges = map (bioRowToExchange False "waste") pbFinalWaste

        sharedExchanges = avoidedExchanges ++
                          techExchanges ++ wasteExchanges ++
                          resourceExchanges ++ airExchanges ++ waterExchanges ++
                          soilExchanges ++ wasteFlowExchanges

        -- Collect all flows and units (shared)
        allFlows = collectFlows (pbProducts ++ pbAvoidedProducts)
                                (pbMaterials ++ pbElectricity ++ pbWasteToTreatment)
                                (pbResources ++ pbEmissionsAir ++ pbEmissionsWater ++
                                 pbEmissionsSoil ++ pbFinalWaste)
        allUnits = collectUnits (pbProducts ++ pbAvoidedProducts)
                                (pbMaterials ++ pbElectricity ++ pbWasteToTreatment)
                                (pbResources ++ pbEmissionsAir ++ pbEmissionsWater ++
                                 pbEmissionsSoil ++ pbFinalWaste)

        -- Create one activity per product
        makeActivity :: ProductRow -> (Activity, [Flow], [Unit])
        makeActivity prod =
            let productExchange = productToExchange True prod
                activity = Activity
                    { activityName = prName prod
                    , activityDescription = if T.null pbComment then [] else [pbComment]
                    , activitySynonyms = M.empty
                    , activityClassification = M.empty
                    , activityLocation = location
                    , activityUnit = prUnit prod
                    , exchanges = productExchange : sharedExchanges
                    }
            in (activity, allFlows, allUnits)

    in map makeActivity pbProducts

-- | Convert product row to exchange
productToExchange :: Bool -> ProductRow -> Exchange
productToExchange isRef ProductRow{..} =
    let flowUUID = generateFlowUUID prName "" prUnit
        unitUUID = generateUnitUUID prUnit
    in TechnosphereExchange
        { techFlowId = flowUUID
        , techAmount = prAmount
        , techUnitId = unitUUID
        , techIsInput = False  -- Products are outputs
        , techIsReference = isRef
        , techActivityLinkId = UUID.nil
        , techProcessLinkId = Nothing
        , techLocation = ""
        }

-- | Convert technosphere row to exchange
techRowToExchange :: Bool -> TechExchangeRow -> [Exchange]
techRowToExchange isInput TechExchangeRow{..}
    | terAmount == 0 = []
    | otherwise =
        let (_, location) = extractLocation terName
            flowUUID = generateFlowUUID terName "" terUnit
            unitUUID = generateUnitUUID terUnit
        in [TechnosphereExchange
            { techFlowId = flowUUID
            , techAmount = abs terAmount
            , techUnitId = unitUUID
            , techIsInput = isInput
            , techIsReference = False
            , techActivityLinkId = UUID.nil
            , techProcessLinkId = Nothing
            , techLocation = location
            }]

-- | Convert biosphere row to exchange
bioRowToExchange :: Bool -> Text -> BioExchangeRow -> Exchange
bioRowToExchange isInput compartment BioExchangeRow{..} =
    let flowUUID = generateFlowUUID berName berCompartment berUnit
        unitUUID = generateUnitUUID berUnit
    in BiosphereExchange
        { bioFlowId = flowUUID
        , bioAmount = berAmount
        , bioUnitId = unitUUID
        , bioIsInput = isInput
        , bioLocation = ""
        }

-- | Collect flows from all exchange types
collectFlows :: [ProductRow] -> [TechExchangeRow] -> [BioExchangeRow] -> [Flow]
collectFlows products techs bios =
    let productFlows = map productToFlow products
        techFlows = map techToFlow techs
        bioFlows = map bioToFlow bios
    in productFlows ++ techFlows ++ bioFlows
  where
    productToFlow ProductRow{..} = Flow
        { flowId = generateFlowUUID prName "" prUnit
        , flowName = prName
        , flowCategory = prCategory
        , flowUnitId = generateUnitUUID prUnit
        , flowType = Technosphere
        , flowSynonyms = M.empty
        }
    techToFlow TechExchangeRow{..} = Flow
        { flowId = generateFlowUUID terName "" terUnit
        , flowName = terName
        , flowCategory = ""
        , flowUnitId = generateUnitUUID terUnit
        , flowType = Technosphere
        , flowSynonyms = M.empty
        }
    bioToFlow BioExchangeRow{..} = Flow
        { flowId = generateFlowUUID berName berCompartment berUnit
        , flowName = berName
        , flowCategory = berCompartment
        , flowUnitId = generateUnitUUID berUnit
        , flowType = Biosphere
        , flowSynonyms = M.empty
        }

-- | Collect units from all exchange types
collectUnits :: [ProductRow] -> [TechExchangeRow] -> [BioExchangeRow] -> [Unit]
collectUnits products techs bios =
    let allUnits = map prUnit products ++ map terUnit techs ++ map berUnit bios
        uniqueUnits = S.toList $ S.fromList allUnits
    in map toUnit uniqueUnits
  where
    toUnit name = Unit
        { unitId = generateUnitUUID name
        , unitName = name
        , unitSymbol = name
        , unitComment = ""
        }

-- ============================================================================
-- Main Parser
-- ============================================================================

-- | Parse a SimaPro CSV file
-- Handles Windows-1252/Latin-1 encoding common in SimaPro exports
parseSimaProCSV :: FilePath -> IO ([Activity], FlowDB, UnitDB)
parseSimaProCSV path = do
    hPutStrLn stderr $ "Loading SimaPro CSV file: " ++ path
    startTime <- getCurrentTime

    -- Read as ByteString and split into lines (more efficient than Text for 9M lines)
    rawContent <- BS.readFile path
    let bsLines = BS8.lines rawContent
        -- Decode each line to Text on demand (lazy), stripping Windows \r
        lines' = map decodeLine bsLines
        initialAcc = ParseAcc
            { paConfig = defaultConfig
            , paState = InHeader
            , paCurrentBlock = emptyProcessBlock
            , paBlocks = []
            , paLineNum = 0
            }
    -- Use strict foldl' to process lines
    finalAcc <- evaluate $ foldl' processLine initialAcc lines'
    let blocks = reverse (paBlocks finalAcc)

    -- Convert all blocks to activities (one activity per product)
    let converted = concatMap processBlockToActivity blocks
        activities = map (\(a,_,_) -> a) converted
        allFlows = concatMap (\(_,f,_) -> f) converted
        allUnits = concatMap (\(_,_,u) -> u) converted

    -- Build deduplicated maps
    let flowDB = M.fromList [(flowId f, f) | f <- allFlows]
        unitDB = M.fromList [(unitId u, u) | u <- allUnits]

    -- Force evaluation before returning
    let !numActivities = length activities
    let !numFlows = M.size flowDB
    let !numUnits = M.size unitDB

    endTime <- getCurrentTime
    let duration = realToFrac (diffUTCTime endTime startTime) :: Double
    hPutStrLn stderr $ printf "SimaPro parsing completed in %.2fs:" duration
    hPutStrLn stderr $ printf "  Activities: %d processes" numActivities
    hPutStrLn stderr $ printf "  Flows: %d unique" numFlows
    hPutStrLn stderr $ printf "  Units: %d unique" numUnits

    return (activities, flowDB, unitDB)
  where
    -- Decode a ByteString line to Text, handling Windows line endings
    decodeLine :: BS.ByteString -> Text
    decodeLine bs =
        let decoded = TE.decodeUtf8With TEE.lenientDecode bs
        in T.dropWhileEnd (== '\r') decoded
