{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Parse ILCD process datasets into VoLCA's SimpleDatabase.

ILCD packages have structure: ILCD/{processes/, flows/, flowproperties/, unitgroups/}
This module parses all four to build a complete activity database.
-}
module ILCD.Parser (
    parseILCDDirectory,
    parseProcessXML,
    ILCDProcessRaw (..),
    ILCDExchangeRaw (..),
    buildSupplierIndex,
    fixActivityExchanges,
) where

import Amount (readAmount)
import Control.Applicative ((<|>))
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (mapConcurrently)
import qualified Data.ByteString as BS
import Data.Char (toLower)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.UUID as UUID
import Database.Allocation (AllocationKey (..), allocate)
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))
import Text.Printf (printf)
import qualified Xeno.SAX as X

import EcoSpold.Common (bsToText, distributeFiles, isElement)
import Method.FlowResolver (ILCDFlowInfo (..), parseFlowDirectory)
import qualified Method.Types as MT
import Progress (ProgressLevel (..), reportProgress)
import Types

-- | Raw parsed ILCD process (before conversion to VoLCA types)
data ILCDProcessRaw = ILCDProcessRaw
    { iprUUID :: !UUID
    , iprName :: !Text
    , iprLocation :: !Text
    , iprRefFlowIdx :: !Int -- dataSetInternalID of reference exchange
    , iprExchanges :: ![ILCDExchangeRaw]
    , iprClassifications :: !(M.Map Text Text)
    , iprProcessType :: !Text -- ILCD <processType> element value (e.g. "Unit process, single operation"); "" when absent
    }

{- | Update a comment slot with a newly-seen `<common:generalComment>`.
Prefer English; otherwise keep the first non-empty translation.
-}
pickILCDComment :: Maybe (Text, Text) -> Text -> Text -> Maybe (Text, Text)
pickILCDComment existing lang txt =
    let s = T.strip txt
     in if T.null s
            then existing
            else case existing of
                Just ("en", _) -> existing
                _ | lang == "en" -> Just ("en", s)
                Nothing -> Just (lang, s)
                Just _ -> existing

data ILCDExchangeRaw = ILCDExchangeRaw
    { ierInternalId :: !Int
    , ierFlowRef :: !UUID
    , ierDirection :: !Text -- "Input" / "Output"
    , ierAmount :: !Double
    , ierLocation :: !Text
    , ierComment :: !(Maybe Text) -- per-exchange <common:generalComment>
    , ierShare :: !(Maybe Double) -- <allocation allocatedFraction>, in percent, when the exchange allocates to itself
    }

{- | Parse an ILCD directory into a SimpleDatabase.
Expects subdirectories: processes/, flows/, flowproperties/, unitgroups/
-}
parseILCDDirectory :: FilePath -> IO (Either Text SimpleDatabase)
parseILCDDirectory dir = do
    reportProgress Info $ "Loading ILCD database from: " ++ dir

    -- Step 1: Parse unit groups and flow properties (small, sequential)
    unitGroupMap <- parseUnitGroups (dir </> "unitgroups")
    flowPropMap <- parseFlowProperties (dir </> "flowproperties")
    reportProgress Info $ printf "Parsed %d unit groups, %d flow properties" (M.size unitGroupMap) (M.size flowPropMap)

    -- Step 2: Parse flows (reuse FlowResolver, parallel + cached)
    flowInfoMap <- parseFlowDirectory (dir </> "flows")
    reportProgress Info $ printf "Parsed %d flows" (M.size flowInfoMap)

    -- Step 3: Build TechFlowDB, BioFlowDB, WasteFlowDB and UnitDB from parsed data
    let (techFlowDB, bioFlowDB, wasteFlowDB, unitDB) = buildFlowAndUnitDB flowInfoMap flowPropMap unitGroupMap

    -- Step 4: Parse process XMLs in parallel
    processFiles <- listXMLFiles (dir </> "processes")
    reportProgress Info $ printf "Parsing %d ILCD process files..." (length processFiles)
    rawProcesses <- parseProcessFilesParallel processFiles

    reportProgress Info $ printf "Parsed %d processes, building activity map..." (length rawProcesses)

    -- Step 5: Build ActivityMap
    let activityMap = buildActivityMap flowInfoMap techFlowDB bioFlowDB wasteFlowDB unitDB rawProcesses

    -- Step 6: Fix supplier links (name-based, like SimaPro)
    let simpleDb = SimpleDatabase activityMap techFlowDB bioFlowDB wasteFlowDB unitDB
    fixedDb <- fixILCDActivityLinks simpleDb
    reportProgress Info $
        printf
            "ILCD database loaded: %d activities, %d tech flows, %d bio flows, %d units"
            (M.size $ sdbActivities fixedDb)
            (M.size $ sdbTechFlows fixedDb)
            (M.size $ sdbBioFlows fixedDb)
            (M.size $ sdbUnits fixedDb)
    return $ Right fixedDb

-- | List XML files in a directory
listXMLFiles :: FilePath -> IO [FilePath]
listXMLFiles d = do
    fs <- listDirectory d
    return [d </> f | f <- fs, map toLower (takeExtension f) == ".xml"]

--------------------------------------------------------------------------------
-- Unit Groups: unitGroupUUID → (refUnitName, refUnitInternalId)
--------------------------------------------------------------------------------

parseUnitGroups :: FilePath -> IO (M.Map UUID (Text, Int))
parseUnitGroups dir = do
    files <- listXMLFiles dir
    results <- mapM (fmap parseUnitGroupXML . BS.readFile) files
    return $ M.fromList (Data.Maybe.catMaybes results)

data UGState = UGState
    { ugUUID :: !Text
    , ugRefIdx :: !Int
    , ugUnits :: ![(Int, Text)]
    , ugCurId :: !Int
    , ugCurName :: !Text
    , ugInUnit :: !Bool
    , ugTextAccum :: ![BS.ByteString]
    }

parseUnitGroupXML :: BS.ByteString -> Maybe (UUID, (Text, Int))
parseUnitGroupXML bytes =
    case X.fold openTag attr endOpen txt closeTag cdata (UGState "" 0 [] (-1) "" False []) bytes of
        Left _ -> Nothing
        Right s -> do
            uuid <- UUID.fromText (ugUUID s)
            case lookup (ugRefIdx s) (ugUnits s) of
                Just name -> Just (uuid, (name, ugRefIdx s))
                Nothing -> Nothing
  where
    openTag s tag
        | isElement tag "unit" = s{ugInUnit = True, ugTextAccum = [], ugCurId = -1, ugCurName = ""}
        | otherwise = s{ugTextAccum = []}

    attr s name value
        | isElement name "dataSetInternalID" && ugInUnit s =
            case TR.decimal (bsToText value) of Right (n, _) -> s{ugCurId = n}; Left _ -> s
        | otherwise = s

    endOpen s _ = s

    txt s content =
        let trimmed = BS.dropWhile (== 32) $ BS.dropWhileEnd (== 32) content
         in if BS.null trimmed then s else s{ugTextAccum = trimmed : ugTextAccum s}

    closeTag s tag
        | isElement tag "UUID" && T.null (ugUUID s) =
            s{ugUUID = accum s, ugTextAccum = []}
        | isElement tag "referenceToReferenceUnit" =
            case TR.decimal (accum s) of
                Right (n, _) -> s{ugRefIdx = n, ugTextAccum = []}
                Left _ -> s{ugTextAccum = []}
        | isElement tag "name" && ugInUnit s =
            s{ugCurName = accum s, ugTextAccum = []}
        | isElement tag "unit" =
            s
                { ugInUnit = False
                , ugTextAccum = []
                , ugUnits = (ugCurId s, ugCurName s) : ugUnits s
                }
        | otherwise = s{ugTextAccum = []}

    cdata = txt
    accum s = T.strip $ T.concat $ reverse $ map bsToText (ugTextAccum s)

--------------------------------------------------------------------------------
-- Flow Properties: flowPropertyUUID → unitGroupUUID
--------------------------------------------------------------------------------

parseFlowProperties :: FilePath -> IO (M.Map UUID UUID)
parseFlowProperties dir = do
    files <- listXMLFiles dir
    results <- mapM (fmap parseFlowPropertyXML . BS.readFile) files
    return $ M.fromList (Data.Maybe.catMaybes results)

data FPState = FPState
    { fpUUID :: !Text
    , fpUnitGroupRef :: !Text
    , fpInQuantRef :: !Bool
    , fpTextAccum :: ![BS.ByteString]
    }

parseFlowPropertyXML :: BS.ByteString -> Maybe (UUID, UUID)
parseFlowPropertyXML bytes =
    case X.fold openTag attr endOpen txt closeTag cdata (FPState "" "" False []) bytes of
        Left _ -> Nothing
        Right s -> (,) <$> UUID.fromText (fpUUID s) <*> UUID.fromText (fpUnitGroupRef s)
  where
    openTag s tag
        | isElement tag "quantitativeReference" = s{fpInQuantRef = True, fpTextAccum = []}
        | otherwise = s{fpTextAccum = []}

    attr s name value
        | fpInQuantRef s && isElement name "refObjectId" && T.null (fpUnitGroupRef s) =
            s{fpUnitGroupRef = bsToText value}
        | otherwise = s

    endOpen s _ = s

    txt s content =
        let trimmed = BS.dropWhile (== 32) $ BS.dropWhileEnd (== 32) content
         in if BS.null trimmed then s else s{fpTextAccum = trimmed : fpTextAccum s}

    closeTag s tag
        | isElement tag "UUID" && T.null (fpUUID s) =
            s{fpUUID = accum s, fpTextAccum = []}
        | isElement tag "quantitativeReference" =
            s{fpInQuantRef = False, fpTextAccum = []}
        | otherwise = s{fpTextAccum = []}

    cdata = txt
    accum s = T.strip $ T.concat $ reverse $ map bsToText (fpTextAccum s)

--------------------------------------------------------------------------------
-- Build FlowDB and UnitDB from ILCD data
--------------------------------------------------------------------------------

buildFlowAndUnitDB ::
    M.Map UUID ILCDFlowInfo ->
    M.Map UUID UUID -> -- flowProperty UUID → unitGroup UUID
    M.Map UUID (Text, Int) -> -- unitGroup UUID → (refUnitName, refIdx)
    (TechFlowDB, BioFlowDB, WasteFlowDB, UnitDB)
buildFlowAndUnitDB flowInfoMap fpMap ugMap = (techFlows, bioFlows, wasteFlows, allUnits)
  where
    -- Partition flows by type at construction time. ILCD's standard flowType
    -- enumeration includes "Elementary flow", "Product flow", "Waste flow",
    -- "Other flow", and "Impact category". Everything that is neither
    -- elementary nor explicit-waste lands in the technosphere bucket.
    techFlows = M.fromList [(uuid, mkTechFlow uuid info) | (uuid, info) <- M.toList flowInfoMap, classify info == TechClass]
    bioFlows = M.fromList [(uuid, mkBioFlow uuid info) | (uuid, info) <- M.toList flowInfoMap, classify info == BioClass]
    wasteFlows = M.fromList [(uuid, mkWasteFlow uuid info) | (uuid, info) <- M.toList flowInfoMap, classify info == WasteClass]

    classify = classifyFlowType . ilcdFlowType

    -- Collect all unique units (keyed by unitGroup UUID)
    allUnits =
        M.fromList
            [ (ugId, Unit ugId uName uName "")
            | (ugId, (uName, _)) <- M.toList ugMap
            ]

    mkTechFlow uuid info =
        TechnosphereFlow
            { tfId = uuid
            , tfName = ilcdBaseName info
            , tfUnitId = resolveUnit info
            , tfSynonyms = M.empty
            , tfCAS = ilcdCAS info
            , tfSubstanceId = Nothing
            }

    mkBioFlow uuid info =
        BiosphereFlow
            { bfId = uuid
            , bfName = ilcdBaseName info
            , bfUnitId = resolveUnit info
            , bfSynonyms = M.empty
            , bfCAS = ilcdCAS info
            , bfSubstanceId = Nothing
            , bfCompartment = toCompartment (ilcdCompartment info)
            }

    mkWasteFlow uuid info =
        WasteFlow
            { wfId = uuid
            , wfName = ilcdBaseName info
            , wfUnitId = resolveUnit info
            , wfSynonyms = M.empty
            , wfCAS = ilcdCAS info
            , wfSubstanceId = Nothing
            }

    toCompartment Nothing = Nothing
    toCompartment (Just (MT.Compartment m sc _)) =
        Just $ Compartment m (if T.null sc then Nothing else Just sc)

    resolveUnit info =
        Data.Maybe.fromMaybe UUID.nil $
            ilcdFlowPropertyRef info >>= (`M.lookup` fpMap) >>= \ugId ->
                ugId <$ M.lookup ugId ugMap -- use unitGroup UUID as unit key

{- | ILCD flow classification: drives both flowDB partitioning and exchange
shape in buildActivity. Keep this enum in sync between the two sites.
-}
data FlowClass = TechClass | BioClass | WasteClass
    deriving (Eq)

{- | Classify an ILCD @flowType@ string. Tolerates whitespace and casing
variants ("Elementary flow", "ELEMENTARY_FLOW", "  waste  flow ", …) —
ILCD exporters in the wild are not strict about either, and the standard
enumeration values differ between ILCD 1.1 and newer formats. Anything
unrecognized lands in the technosphere bucket, matching the partition's
"product/other" default arm.
-}
classifyFlowType :: Text -> FlowClass
classifyFlowType raw =
    case T.unwords (T.words (T.toLower (T.replace "_" " " raw))) of
        "elementary flow" -> BioClass
        "waste flow" -> WasteClass
        _ -> TechClass

--------------------------------------------------------------------------------
-- Process XML Parsing
--------------------------------------------------------------------------------

data ProcState = ProcState
    { psUUID :: !Text
    , psBaseName :: !Text
    , psLocation :: !Text
    , psRefFlowIdx :: !Int
    , psExchanges :: ![ILCDExchangeRaw]
    , psInExchange :: !Bool
    , psExInternalId :: !Int
    , psExFlowRef :: !Text
    , psExDirection :: !Text
    , psExAmount :: !Double
    , psExLocation :: !Text
    , psExComment :: !(Maybe (Text, Text))
    {- ^ (xml:lang, comment text) for the open `<exchange>`. English wins;
    otherwise first non-empty. Reset on each `<exchange>` open.
    -}
    , psExAllocations :: ![(Int, Double)]
    {- ^ Every (internalReferenceToCoProduct, allocatedFraction) of the open
    `<exchange>`. All of them, because how many there are is what says
    which of the format's two meanings the file is using.
    -}
    , psAllocRef :: !(Maybe Int)
    {- ^ The attributes of one `<allocation>` arrive separately, so they are
    paired at its close.
    -}
    , psAllocFraction :: !(Maybe Double)
    , psPendingCommentLang :: !Text
    {- ^ xml:lang on the currently-open `<common:generalComment>`. Reset
    on every comment open.
    -}
    , psTextAccum :: ![BS.ByteString]
    , psInName :: !Bool
    , psClassifications :: !(M.Map Text Text)
    , psPendingClassName :: !Text
    , psInClass :: !Bool
    , psProcessType :: !Text -- ILCD <processType> element text (empty when absent)
    }

parseProcessXML :: BS.ByteString -> Maybe ILCDProcessRaw
parseProcessXML bytes =
    case X.fold
        openTag
        attr
        endOpen
        txt
        closeTag
        cdata
        ( ProcState
            { psUUID = ""
            , psBaseName = ""
            , psLocation = ""
            , psRefFlowIdx = 0
            , psExchanges = []
            , psInExchange = False
            , psExInternalId = -1
            , psExFlowRef = ""
            , psExDirection = ""
            , psExAmount = 0
            , psExLocation = ""
            , psExComment = Nothing
            , psExAllocations = []
            , psAllocRef = Nothing
            , psAllocFraction = Nothing
            , psPendingCommentLang = ""
            , psTextAccum = []
            , psInName = False
            , psClassifications = M.empty
            , psPendingClassName = ""
            , psInClass = False
            , psProcessType = ""
            }
        )
        bytes of
        Left _ -> Nothing
        Right s -> buildProcess s
  where
    openTag s tag
        | isElement tag "exchange" =
            s
                { psInExchange = True
                , psTextAccum = []
                , psExInternalId = -1
                , psExFlowRef = ""
                , psExDirection = ""
                , psExAmount = 0
                , psExLocation = ""
                , psExComment = Nothing
                , psExAllocations = []
                , psAllocRef = Nothing
                , psAllocFraction = Nothing
                }
        | isElement tag "generalComment" =
            s{psPendingCommentLang = "", psTextAccum = []}
        | isElement tag "name" && not (psInExchange s) =
            s{psInName = True, psTextAccum = []}
        | isElement tag "class" && not (psInExchange s) =
            s{psInClass = True, psTextAccum = []}
        | otherwise = s{psTextAccum = []}

    attr s name value
        | isElement name "dataSetInternalID" && psInExchange s =
            case TR.decimal (bsToText value) of
                Right (n, _) -> s{psExInternalId = n}
                Left _ -> s
        | isElement name "refObjectId" && psInExchange s && T.null (psExFlowRef s) =
            s{psExFlowRef = bsToText value}
        | isElement name "internalReferenceToCoProduct" && psInExchange s =
            case TR.decimal (bsToText value) of
                Right (n, _) -> s{psAllocRef = Just n}
                Left _ -> s
        | isElement name "allocatedFraction" && psInExchange s =
            s{psAllocFraction = readAmount (bsToText value)}
        | isElement name "location" && not (psInExchange s) && T.null (psLocation s) =
            s{psLocation = bsToText value}
        | isElement name "name" && not (psInExchange s) && not (psInName s) =
            s{psPendingClassName = bsToText value}
        | isElement name "xml:lang" && psInExchange s =
            -- Capture lang on `<common:generalComment>` (and other tags), only
            -- inside an exchange. We use it at closeTag time to pick the best
            -- comment translation.
            s{psPendingCommentLang = bsToText value}
        | otherwise = s

    endOpen s _ = s

    txt s content =
        let trimmed = BS.dropWhile (== 32) $ BS.dropWhileEnd (== 32) content
         in if BS.null trimmed then s else s{psTextAccum = trimmed : psTextAccum s}

    closeTag s tag
        | isElement tag "generalComment" && psInExchange s =
            -- Capture per-exchange comment only. Process-level
            -- <generalComment> (inside processInformation, NOT inside an
            -- exchange) hits the `otherwise` branch and is harmlessly dropped.
            s
                { psExComment = pickILCDComment (psExComment s) (psPendingCommentLang s) (accum s)
                , psPendingCommentLang = ""
                , psTextAccum = []
                }
        | isElement tag "UUID" && T.null (psUUID s) =
            s{psUUID = accum s, psTextAccum = []}
        | isElement tag "baseName" && psInName s =
            s{psBaseName = accum s, psTextAccum = []}
        | isElement tag "name" && not (psInExchange s) =
            s{psInName = False, psTextAccum = []}
        | isElement tag "referenceToReferenceFlow" =
            case TR.decimal (accum s) of
                Right (n, _) -> s{psRefFlowIdx = n, psTextAccum = []}
                Left _ -> s{psTextAccum = []}
        | isElement tag "exchangeDirection" && psInExchange s =
            s{psExDirection = accum s, psTextAccum = []}
        | isElement tag "resultingAmount" && psInExchange s =
            s{psExAmount = parseDouble (accum s), psTextAccum = []}
        | isElement tag "meanAmount" && psInExchange s && psExAmount s == 0 =
            s{psExAmount = parseDouble (accum s), psTextAccum = []}
        | isElement tag "location" && psInExchange s =
            s{psExLocation = accum s, psTextAccum = []}
        | isElement tag "class" && psInClass s =
            let classVal = accum s
                key = psPendingClassName s
                existing = M.findWithDefault "" key (psClassifications s)
                val = if T.null existing then classVal else existing <> "/" <> classVal
             in s
                    { psClassifications =
                        if T.null classVal
                            then psClassifications s
                            else M.insert key val (psClassifications s)
                    , psInClass = False
                    , psTextAccum = []
                    }
        | isElement tag "classification" =
            s{psPendingClassName = "", psTextAccum = []}
        | isElement tag "processType" && not (psInExchange s) && T.null (psProcessType s) =
            -- ILCD <processType> lives at <modellingAndValidation><LCIMethodAndAllocation><processType>.
            -- Guard psInExchange just in case a future ILCD revision reuses the tag name
            -- elsewhere; first occurrence wins to be deterministic.
            s{psProcessType = accum s, psTextAccum = []}
        | isElement tag "allocation" && psInExchange s =
            let paired = (,) <$> psAllocRef s <*> psAllocFraction s
             in s
                    { psExAllocations = maybe (psExAllocations s) (: psExAllocations s) paired
                    , psAllocRef = Nothing
                    , psAllocFraction = Nothing
                    , psTextAccum = []
                    }
        | isElement tag "exchange" =
            let ex =
                    ILCDExchangeRaw
                        { ierInternalId = psExInternalId s
                        , ierFlowRef = Data.Maybe.fromMaybe UUID.nil (UUID.fromText (psExFlowRef s))
                        , ierDirection = psExDirection s
                        , ierAmount = psExAmount s
                        , ierLocation = psExLocation s
                        , ierComment = snd <$> psExComment s
                        , ierShare = soleSelfAllocation (psExInternalId s) (psExAllocations s)
                        }
             in s{psInExchange = False, psExchanges = ex : psExchanges s, psTextAccum = []}
        | otherwise = s{psTextAccum = []}

    -- \| The share an exchange declares for itself, and only when it is the
    --    only thing that exchange allocates.
    --
    --    The attribute carries two different meanings in ILCD. One entry pointing
    --    at the exchange itself is the ordinary form: "this product takes this
    --    share of the process". Several entries are the general form, where the
    --    exchange is distributed across the co-products and the entry pointing at
    --    itself is its own share of itself, which is 100 and says nothing about
    --    allocation. Reading that as a declared share would give every product
    --    100 % and hand each one the whole inventory.
    --
    --    So several entries yield no share at all, and the allocation gate refuses
    --    the dataset. Refused is the right answer for a form we cannot represent.
    --
    soleSelfAllocation :: Int -> [(Int, Double)] -> Maybe Double
    soleSelfAllocation ownId allocations = case allocations of
        [(ref, fraction)] | ref == ownId -> Just fraction
        _ -> Nothing

    cdata = txt
    accum s = T.strip $ T.concat $ reverse $ map bsToText (psTextAccum s)

    parseDouble t = Data.Maybe.fromMaybe 0 (readAmount t)

    buildProcess s = do
        uuid <- UUID.fromText (psUUID s)
        if T.null (psBaseName s)
            then Nothing
            else
                Just
                    ILCDProcessRaw
                        { iprUUID = uuid
                        , iprName = psBaseName s
                        , iprLocation = psLocation s
                        , iprRefFlowIdx = psRefFlowIdx s
                        , iprExchanges = reverse (psExchanges s)
                        , iprClassifications = psClassifications s
                        , iprProcessType = psProcessType s
                        }

-- | Parse process files in parallel using worker pattern
parseProcessFilesParallel :: [FilePath] -> IO [ILCDProcessRaw]
parseProcessFilesParallel files = do
    numWorkers <- getNumCapabilities
    let workers = distributeFiles numWorkers files
    workerResults <- mapConcurrently parseWorker workers
    return $ concat workerResults
  where
    parseWorker paths = do
        results <- mapM (fmap parseProcessXML . BS.readFile) paths
        return (Data.Maybe.catMaybes results)

--------------------------------------------------------------------------------
-- Build ActivityMap from raw processes
--------------------------------------------------------------------------------

buildActivityMap ::
    M.Map UUID ILCDFlowInfo ->
    TechFlowDB ->
    BioFlowDB ->
    WasteFlowDB ->
    UnitDB ->
    [ILCDProcessRaw] ->
    ActivityMap
buildActivityMap flowInfoMap techFlowDB bioFlowDB wasteFlowDB unitDB procs =
    M.fromList
        [ ((iprUUID p, productKey p activity), activity)
        | p <- procs
        , activity <- NE.toList (allocate Declared unitDB (buildActivity flowInfoMap techFlowDB bioFlowDB wasteFlowDB unitDB p))
        ]
  where
    -- The process is keyed on its reference product; a dataset the gate will
    -- refuse for having none keeps the flow its file points at, or its own id.
    productKey :: ILCDProcessRaw -> Activity -> UUID
    productKey p activity =
        Data.Maybe.fromMaybe
            (maybe (iprUUID p) ierFlowRef (findRefExchange p))
            (Data.Maybe.listToMaybe [exchangeFlowId ex | ex <- exchanges activity, exchangeIsReference ex])

findRefExchange :: ILCDProcessRaw -> Maybe ILCDExchangeRaw
findRefExchange p =
    case filter (\e -> ierInternalId e == iprRefFlowIdx p) (iprExchanges p) of
        (e : _) -> Just e
        [] -> Nothing

buildActivity :: M.Map UUID ILCDFlowInfo -> TechFlowDB -> BioFlowDB -> WasteFlowDB -> UnitDB -> ILCDProcessRaw -> Activity
buildActivity flowInfoMap techFlowDB bioFlowDB wasteFlowDB unitDB p =
    Activity
        { activityName = iprName p
        , activityDescription = []
        , activityDocumentation = [] -- ILCD states its provenance too; not read yet
        , activitySynonyms = M.empty
        , activityClassification = iprClassifications p
        , activityLocation = iprLocation p
        , activityLocationSource = declaredLocationSource (iprLocation p)
        , activityUnit = refUnit
        , exchanges = map (mkExchange (iprRefFlowIdx p)) (iprExchanges p)
        , activityParams = M.empty
        , activityParamExprs = M.empty
        , activityNativeType =
            if T.null (iprProcessType p)
                then Nothing
                else Just (ILCDProcessType{iptLabel = iprProcessType p})
        , activityNativeId = Nothing
        , activityFormulaCheck = Nothing
        }
  where
    -- \| The share the source declared for one product output, read from its
    --    own @<allocation allocatedFraction>@.
    --
    --    ILCD lets an exchange allocate to any co-product by internal id, which
    --    would be a matrix of shares. Only the entry pointing at the exchange
    --    itself is read, because that is the one that says "this product's share
    --    of the process" and the only one 'DeclaredShare' can hold. A dataset
    --    using the general form keeps no share here and the allocation gate
    --    refuses it, which is the honest outcome: better refused than split on a
    --    number that meant something else.
    --
    declaredShareOf :: TechRole -> ILCDExchangeRaw -> Maybe DeclaredShare
    declaredShareOf role raw = case role of
        ReferenceProduct -> shareOf raw
        Coproduct -> shareOf raw
        ReferenceInput -> Nothing
        Input -> Nothing
        AvoidedProduct -> Nothing

    -- \| The number itself, once the role says a share belongs on the exchange.
    shareOf :: ILCDExchangeRaw -> Maybe DeclaredShare
    shareOf raw = flip DeclaredShare Nothing <$> ierShare raw

    -- Look up the reference exchange's flow unit. Reference exchange is typically
    -- a technosphere product, but for waste-treatment processes it may be a
    -- biosphere input — try both maps before falling back to "kg".
    refUnit = case findRefExchange p of
        Nothing -> "kg"
        Just re ->
            let fid = ierFlowRef re
                uId =
                    Data.Maybe.fromMaybe UUID.nil $
                        (tfUnitId <$> M.lookup fid techFlowDB)
                            <|> (bfUnitId <$> M.lookup fid bioFlowDB)
             in maybe "kg" unitName (M.lookup uId unitDB)

    mkExchange refIdx raw =
        let flowUUID = ierFlowRef raw
            isInput = ierDirection raw == "Input"
            isRef = ierInternalId raw == refIdx
            flowClass = maybe TechClass (classifyFlowType . ilcdFlowType) (M.lookup flowUUID flowInfoMap)
            fUnitId =
                Data.Maybe.fromMaybe UUID.nil $
                    (tfUnitId <$> M.lookup flowUUID techFlowDB)
                        <|> (bfUnitId <$> M.lookup flowUUID bioFlowDB)
                        <|> (wfUnitId <$> M.lookup flowUUID wasteFlowDB)
            techRoleFor
                | isRef && isInput = ReferenceInput
                | isRef = ReferenceProduct
                | isInput = Input
                | otherwise = Coproduct
         in case flowClass of
                BioClass ->
                    BiosphereExchange
                        { bioFlowId = flowUUID
                        , bioAmount = ierAmount raw
                        , bioUnitId = fUnitId
                        , bioDirection = if isInput then Resource else Emission
                        , bioLocation = ierLocation raw
                        , bioComment = ierComment raw
                        , bioPedigree = Nothing
                        }
                WasteClass ->
                    WasteExchange
                        { waFlowId = flowUUID
                        , waAmount = ierAmount raw
                        , waUnitId = fUnitId
                        , waIsInput = isInput
                        , waActivityLinkId = UUID.nil
                        , waProcessLinkId = Nothing
                        , waLocation = ierLocation raw
                        , waComment = ierComment raw
                        , waPedigree = Nothing
                        }
                TechClass ->
                    TechnosphereExchange
                        { techFlowId = flowUUID
                        , techAmount = ierAmount raw
                        , techUnitId = fUnitId
                        , techRole = techRoleFor
                        , techActivityLinkId = UUID.nil
                        , techProcessLinkId = Nothing
                        , techLocation = ierLocation raw
                        , techComment = ierComment raw
                        , techPedigree = Nothing
                        , techShare = declaredShareOf techRoleFor raw
                        , techClassification = M.empty
                        }

--------------------------------------------------------------------------------
-- Fix activity links (supplier resolution by name)
--------------------------------------------------------------------------------

-- | Flow UUID → (activityUUID, productUUID) for reference exchanges
type SupplierIndex = M.Map UUID (UUID, UUID)

fixILCDActivityLinks :: SimpleDatabase -> IO SimpleDatabase
fixILCDActivityLinks db = do
    let idx = buildSupplierIndex (sdbActivities db)
    reportProgress Info $ printf "Built supplier index with %d entries for ILCD linking" (M.size idx)
    return db{sdbActivities = M.map (fixActivityExchanges idx) (sdbActivities db)}

{- | Build a flow-UUID-keyed index of (activityUUID, productUUID) from reference exchanges.
UUID-based: no name collisions, no indirection through flowDB.
-}
buildSupplierIndex :: ActivityMap -> SupplierIndex
buildSupplierIndex activities =
    M.fromList
        [ (exchangeFlowId ex, (actUUID, prodUUID))
        | ((actUUID, prodUUID), act) <- M.toList activities
        , ex <- exchanges act
        , exchangeIsReference ex
        ]

fixActivityExchanges :: SupplierIndex -> Activity -> Activity
fixActivityExchanges idx act =
    act{exchanges = map fixEx (exchanges act)}
  where
    -- Only relink plain @Input@ exchanges. @ReferenceInput@ is the activity's
    -- own waste-treatment reference flow — it appears in the supplier index
    -- (it is a reference exchange) but rewriting it would point the activity
    -- at itself and erase the role, breaking 'activityNormFactor'.
    fixEx ex@TechnosphereExchange{techFlowId = fid, techRole = Input} =
        case M.lookup fid idx of
            Just (actUUID, prodUUID) ->
                ex
                    { techFlowId = prodUUID
                    , techActivityLinkId = actUUID
                    }
            Nothing -> ex
    fixEx ex@TechnosphereExchange{} = ex
    fixEx ex@BiosphereExchange{} = ex
    -- A waste input awaiting treatment-activity resolution follows the same
    -- name-lookup logic as a technosphere Input.
    fixEx ex@WasteExchange{waFlowId = fid, waIsInput = True} =
        case M.lookup fid idx of
            Just (actUUID, prodUUID) ->
                ex
                    { waFlowId = prodUUID
                    , waActivityLinkId = actUUID
                    }
            Nothing -> ex
    fixEx ex@WasteExchange{} = ex
