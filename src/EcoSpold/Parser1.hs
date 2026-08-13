{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- | EcoSpold1 SAX Parser for Ecoinvent 2.x formats
This parser handles the older EcoSpold1 XML format (.XML files)
used in Ecoinvent versions 2.x (e.g., 2.2)
-}
module EcoSpold.Parser1 (
    streamParseActivityAndFlowsFromFile1,
    streamParseAllDatasetsFromFile1,

    -- * XML parsing (exported for testing)
    parseWithXeno,
    parseAllWithXeno,

    -- * Pure helpers (exported for testing)
    generateFlowUUID,
    generateUnitUUID,
) where

import Control.Monad (forM_)
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import Data.Either (lefts, rights)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID
import qualified Data.UUID.V5 as UUID5
import EcoSpold.Common (bsToDouble, bsToInt, bsToText, isElement, nonEmptyText)
import EcoSpold.Cutoff (applyCutoffStrategy)
import Progress (ProgressLevel (..), reportProgress)
import Types
import qualified Xeno.SAX as X

{- | Namespace UUID for generating deterministic UUIDs from EcoSpold1 numeric IDs
Using UUID v5 (SHA1-based) with a custom namespace
-}
ecospold1Namespace :: UUID
ecospold1Namespace = UUID5.generateNamed UUID5.namespaceURL (BS.unpack $ TE.encodeUtf8 "ecospold1.ecoinvent.org")

{- | Generate deterministic UUID from the exchange number, the full compartment
(category + subCategory) and the unit.

The exchange number is already the identity EcoSpold1 gives a flow across the
whole export: an elementary flow carries its substance number (@Water, fossil@
is 62793 in every dataset that draws it), and a technosphere input carries the
number of the dataset producing it, the same number that dataset stamps on its
own reference product (true of all 11947 datasets of the export measured here).
So the fields below name one flow, once, for the whole database.

The unit is in the key because a matrix row is summed without conversion. Real
exports record one substance in two units: the measured one writes @Heat,
waste@ in MJ in some datasets and in kWh in others, and @Natural gas, at
production@ in m3 and Nm3, 193 such flows in all. Merging those onto one row
would add MJ to kWh and report the total under whichever unit won the merge.
The SimaPro parser keys on the unit for the same reason.

The dataset a flow was *read from* is deliberately not part of the key. Keying
on it splintered every shared substance into one flow per dataset: the export
measured here carried 27935 biosphere flows for 2515 substances, and a single
inventory of it listed @Lead@ 150 times, once per dataset its supply chain
crossed that emits lead.

The subCategory is part of the key because it is part of a flow's identity: an
emission of one substance to two subcompartments (e.g. a leachate to both
@river@ and @groundwater, long-term@) is two distinct flows with distinct
environmental fates and distinct characterization factors. Dropping subCategory
collapsed them onto one UUID, so the matrix summed their amounts into a single
row carrying whichever subcompartment label happened to win the flow-map merge —
silently scoring gated groundwater/ocean mass at a surface-freshwater CF (or the
reverse). Keying on the full compartment keeps each subcompartment a separate row
scored at its own CF.
-}
generateFlowUUID :: Int -> Text -> Text -> Text -> Text -> UUID
generateFlowUUID exchangeNumber flowName category subCategory unitName =
    let key =
            T.intercalate
                ":"
                [ T.pack (show exchangeNumber)
                , flowName
                , category
                , subCategory
                , unitName
                ]
     in UUID5.generateNamed ecospold1Namespace (BS.unpack $ TE.encodeUtf8 key)

-- | Generate deterministic UUID for unit from unit name
generateUnitUUID :: Text -> UUID
generateUnitUUID unitName =
    UUID5.generateNamed ecospold1Namespace (BS.unpack $ TE.encodeUtf8 $ "unit:" <> unitName)

-- ============================================================================
-- Xeno SAX Parser Implementation for EcoSpold1
-- ============================================================================

-- | Element context tracker
data ElementContext
    = InReferenceFunction
    | InGeography
    | InExchange !ExchangeData
    | InInputGroup !ExchangeData -- Keep parent exchange data
    | InOutputGroup !ExchangeData -- Keep parent exchange data
    | Other
    deriving (Eq)

{- | Exchange accumulator for EcoSpold1 format
All data comes from attributes on the <exchange> element
-}
data ExchangeData = ExchangeData
    { exNumber :: !Int -- Exchange number (numeric ID)
    , exName :: !Text -- Flow name
    , exCategory :: !Text -- Category
    , exSubCategory :: !Text -- Subcategory
    , exLocation :: !Text -- Location (for technosphere)
    , exUnit :: !Text -- Unit name
    , exMeanValue :: !Double -- Amount
    , exInputGroup :: !Text -- Input group (1-4 = technosphere input, 4 = resource)
    , exOutputGroup :: !Text -- Output group (0 = reference, 1-3 = byproduct, 4 = emission)
    , exCASNumber :: !Text -- CAS number (optional)
    , exFormula :: !Text -- Chemical formula (optional)
    , exInfrastructure :: !Bool -- Infrastructure process flag
    , exComment :: !Text -- Free-text comment from `generalComment` attribute
    }
    deriving (Eq)

-- | Initial exchange data
emptyExchangeData :: ExchangeData
emptyExchangeData = ExchangeData 0 "" "" "" "" "" 0.0 "" "" "" "" False ""

-- | Parsing state accumulator
data ParseState = ParseState
    { psDatasetNumber :: !Int
    , psActivityName :: !(Maybe Text)
    , psActivityCategory :: !Text
    , psActivitySubCategory :: !Text
    , psLocation :: !(Maybe Text)
    , psRefUnit :: !(Maybe Text)
    , psDescription :: ![Text]
    , psExchanges :: ![Exchange]
    , psTechFlows :: ![TechnosphereFlow]
    , psBioFlows :: ![BiosphereFlow]
    , psWasteFlows :: ![WasteFlow]
    , psUnits :: ![Unit]
    , psPath :: ![BS.ByteString]
    , psContext :: !ElementContext
    , psTextAccum :: ![BS.ByteString]
    , psSupplierLinks :: !(M.Map UUID Int) -- flowId → supplier dataset number (technosphere inputs)
    , psCompletedActivities :: ![Either String (Activity, [TechnosphereFlow], [BiosphereFlow], [WasteFlow], [Unit], Int, M.Map UUID Int)]
    }

-- | Initial parsing state
initialParseState :: ParseState
initialParseState =
    ParseState
        { psDatasetNumber = 0
        , psActivityName = Nothing
        , psActivityCategory = ""
        , psActivitySubCategory = ""
        , psLocation = Nothing
        , psRefUnit = Nothing
        , psDescription = []
        , psExchanges = []
        , psTechFlows = []
        , psBioFlows = []
        , psWasteFlows = []
        , psUnits = []
        , psPath = []
        , psContext = Other
        , psTextAccum = []
        , psSupplierLinks = M.empty
        , psCompletedActivities = []
        }

-- ----------------------------------------------------------------------------
-- Shared SAX handlers (used by both parseWithXeno and parseAllWithXeno)
-- ----------------------------------------------------------------------------

-- | Drop the current element from the path, keeping accumulated text.
popPath :: ParseState -> ParseState
popPath s = s{psPath = drop 1 (psPath s)}

-- | Drop the current element from the path and clear accumulated text.
popElement :: ParseState -> ParseState
popElement s = s{psPath = drop 1 (psPath s), psTextAccum = []}

{- | Open tag: push the element onto the path and switch context on the
structural elements we care about. Groups only enter their context from
within an exchange; any other current context is preserved.
-}
onOpenTag :: ParseState -> BS.ByteString -> ParseState
onOpenTag state tagName =
    state{psPath = tagName : psPath state, psContext = newContext, psTextAccum = []}
  where
    enterGroup wrap = case psContext state of
        InExchange edata -> wrap edata
        ctx -> ctx
    newContext
        | isElement tagName "referenceFunction" = InReferenceFunction
        | isElement tagName "geography" = InGeography
        | isElement tagName "exchange" = InExchange emptyExchangeData
        | isElement tagName "inputGroup" = enterGroup InInputGroup
        | isElement tagName "outputGroup" = enterGroup InOutputGroup
        | otherwise = psContext state

-- | Attribute: route by current context to the matching field setter.
onAttribute :: ParseState -> BS.ByteString -> BS.ByteString -> ParseState
onAttribute state name value = case psContext state of
    InReferenceFunction -> setRefFunctionAttr name value state
    InGeography
        | isElement name "location" -> state{psLocation = Just (bsToText value)}
        | otherwise -> state
    InExchange edata -> state{psContext = InExchange (setExchangeAttr name value edata)}
    InInputGroup _ -> datasetNumberAttr
    InOutputGroup _ -> datasetNumberAttr
    Other -> datasetNumberAttr
  where
    -- The dataset's numeric id lives on the <dataset> element itself, which is
    -- the head of the path. Accepting it anywhere under <dataset> let the
    -- metadata's own numbered elements (<source>, <person>) overwrite it, so
    -- what was recorded was really the last data generator's id.
    datasetNumberAttr
        | isElement name "number"
        , currentElement : _ <- psPath state
        , isElement currentElement "dataset" =
            state{psDatasetNumber = bsToInt value}
        | otherwise = state

-- | Apply a single referenceFunction attribute to the parse state.
setRefFunctionAttr :: BS.ByteString -> BS.ByteString -> ParseState -> ParseState
setRefFunctionAttr name value st
    | isElement name "name" = st{psActivityName = Just (bsToText value)}
    | isElement name "unit" = st{psRefUnit = Just (bsToText value)}
    | isElement name "category" = st{psActivityCategory = bsToText value}
    | isElement name "subCategory" = st{psActivitySubCategory = bsToText value}
    | isElement name "generalComment"
    , not (BS.null value) =
        st{psDescription = bsToText value : psDescription st}
    | otherwise = st

-- | Apply a single exchange attribute to the in-progress exchange.
setExchangeAttr :: BS.ByteString -> BS.ByteString -> ExchangeData -> ExchangeData
setExchangeAttr name value e
    | isElement name "number" = e{exNumber = bsToInt value}
    | isElement name "name" = e{exName = bsToText value}
    | isElement name "category" = e{exCategory = bsToText value}
    | isElement name "subCategory" = e{exSubCategory = bsToText value}
    | isElement name "location" = e{exLocation = bsToText value}
    | isElement name "unit" = e{exUnit = bsToText value}
    | isElement name "meanValue" = e{exMeanValue = bsToDouble value}
    | isElement name "CASNumber" = e{exCASNumber = bsToText value}
    | isElement name "formula" = e{exFormula = bsToText value}
    | isElement name "infrastructureProcess" = e{exInfrastructure = bsToText value == "true"}
    | isElement name "generalComment" = e{exComment = bsToText value}
    | otherwise = e

-- | End of an opening tag: nothing to do for this format.
onEndOpen :: ParseState -> BS.ByteString -> ParseState
onEndOpen state _tagName = state

-- | Accumulate non-blank text content (also used for CDATA).
onText :: ParseState -> BS.ByteString -> ParseState
onText state content =
    let trimmed = BS.dropWhile (== 32) $ BS.dropWhileEnd (== 32) content
     in if BS.null trimmed
            then state
            else state{psTextAccum = trimmed : psTextAccum state}

-- | Close tag: finalise the element that is ending.
onCloseTag :: ParseState -> BS.ByteString -> ParseState
onCloseTag state tagName
    | isElement tagName "inputGroup" = closeGroup restoreInputGroup (\e t -> e{exInputGroup = t}) state
    | isElement tagName "outputGroup" = closeGroup restoreOutputGroup (\e t -> e{exOutputGroup = t}) state
    | isElement tagName "exchange" = closeExchange state
    | isElement tagName "referenceFunction" = (popElement state){psContext = Other}
    | isElement tagName "geography" = (popElement state){psContext = Other}
    | isElement tagName "dataset" = closeDataset state
    | otherwise = popPath state

{- | Close an input/output group: fold its accumulated text into the parent
exchange's matching group field and return to the exchange context.
@ownGroup@ yields the exchange data to restore when the current context is
the group being closed (or, defensively, a bare exchange); any other context
just pops the element. The opposite group never restores, so a stray
</inputGroup> inside an <outputGroup> (malformed) is ignored, not merged.
-}
closeGroup :: (ElementContext -> Maybe ExchangeData) -> (ExchangeData -> Text -> ExchangeData) -> ParseState -> ParseState
closeGroup ownGroup setField state =
    case ownGroup (psContext state) of
        Just edata -> (popElement state){psContext = InExchange (setField edata txt)}
        Nothing -> popElement state
  where
    txt = T.strip $ T.concat $ reverse $ map bsToText (psTextAccum state)

{- | Exchange data to restore when closing an <inputGroup>: the group we opened,
or (defensively) a bare exchange. The opposite group does not restore.
-}
restoreInputGroup :: ElementContext -> Maybe ExchangeData
restoreInputGroup (InInputGroup edata) = Just edata
restoreInputGroup (InExchange edata) = Just edata
restoreInputGroup InOutputGroup{} = Nothing
restoreInputGroup InReferenceFunction = Nothing
restoreInputGroup InGeography = Nothing
restoreInputGroup Other = Nothing

{- | Exchange data to restore when closing an <outputGroup>: the group we opened,
or (defensively) a bare exchange. The opposite group does not restore.
-}
restoreOutputGroup :: ElementContext -> Maybe ExchangeData
restoreOutputGroup (InOutputGroup edata) = Just edata
restoreOutputGroup (InExchange edata) = Just edata
restoreOutputGroup InInputGroup{} = Nothing
restoreOutputGroup InReferenceFunction = Nothing
restoreOutputGroup InGeography = Nothing
restoreOutputGroup Other = Nothing

{- | Close an exchange: build its exchange/flow/unit, accumulate them, and
record the supplier link for technosphere inputs.
-}
closeExchange :: ParseState -> ParseState
closeExchange state = case psContext state of
    InExchange edata ->
        let (exchange, parsedFlow, unit) = buildExchange (psLocation state) edata
            !supplierLinks = case exchange of
                TechnosphereExchange{techRole = Input}
                    | exNumber edata /= 0 ->
                        M.insert (exchangeFlowId exchange) (exNumber edata) (psSupplierLinks state)
                TechnosphereExchange{} -> psSupplierLinks state
                BiosphereExchange{} -> psSupplierLinks state
                WasteExchange{} -> psSupplierLinks state
            (techs, bios, wastes) = case parsedFlow of
                ParsedTech tf -> (tf : psTechFlows state, psBioFlows state, psWasteFlows state)
                ParsedBio bf -> (psTechFlows state, bf : psBioFlows state, psWasteFlows state)
                ParsedWaste wf -> (psTechFlows state, psBioFlows state, wf : psWasteFlows state)
         in (popElement state)
                { psExchanges = exchange : psExchanges state
                , psTechFlows = techs
                , psBioFlows = bios
                , psWasteFlows = wastes
                , psUnits = unit : psUnits state
                , psSupplierLinks = supplierLinks
                , psContext = Other
                }
    InInputGroup _ -> popPath state
    InOutputGroup _ -> popPath state
    InReferenceFunction -> popPath state
    InGeography -> popPath state
    Other -> popPath state

{- | Close a dataset: snapshot the completed activity and reset per-dataset
accumulators for the next one (multi-dataset files).
-}
closeDataset :: ParseState -> ParseState
closeDataset state =
    let !result = buildResult state
     in popPath ((resetDataset state){psCompletedActivities = result : psCompletedActivities state})

{- | Clear per-dataset accumulators, preserving cross-dataset state
(psPath and psCompletedActivities).
-}
resetDataset :: ParseState -> ParseState
resetDataset state =
    state
        { psDatasetNumber = 0
        , psActivityName = Nothing
        , psActivityCategory = ""
        , psActivitySubCategory = ""
        , psLocation = Nothing
        , psRefUnit = Nothing
        , psDescription = []
        , psExchanges = []
        , psTechFlows = []
        , psBioFlows = []
        , psWasteFlows = []
        , psUnits = []
        , psContext = Other
        , psTextAccum = []
        , psSupplierLinks = M.empty
        }

{- | Build exchange, flow, and unit from exchange data.
@activityLoc@ is the activity's location, used as a biosphere fallback.

EcoSpold1 groups:
  Input:  1-3 = technosphere, 4 = resource (biosphere)
  Output: 0 = reference product, 1-3 = byproduct/co-product, 4 = emission (biosphere)
-}
buildExchange :: Maybe Text -> ExchangeData -> (Exchange, ParsedFlow, Unit)
buildExchange activityLoc edata
    | isWasteFlow = (wasteEx, ParsedWaste wasteFlow, unit)
    | isBiosphere = (bioEx, ParsedBio bioFlow, unit)
    | otherwise = (techEx, ParsedTech techFlow, unit)
  where
    flowId = generateFlowUUID (exNumber edata) (exName edata) (exCategory edata) (exSubCategory edata) (exUnit edata)
    unitId = generateUnitUUID (exUnit edata)
    unit = Unit unitId (exUnit edata) (exUnit edata) ""

    inputGroup = exInputGroup edata
    outputGroup = exOutputGroup edata
    isBiosphere = inputGroup == "4" || outputGroup == "4"
    isInput = not (T.null inputGroup)
    isReferenceProduct = outputGroup == "0"
    -- SimaPro's third flow class ('Final waste flows'). EcoSpold1 exports
    -- surface them on inputGroup=5 to fit the 4-type input/output model, but
    -- the category attribute survives. Routed to WasteExchange so they bypass
    -- cross-DB technosphere linking (orphan outputs, not demands).
    isWasteFlow = exCategory edata == "Final waste flows"

    -- Technosphere: leave empty if unspecified so the Loader can do name-only
    -- lookup. Biosphere: fall back to the activity location (no supplier link).
    exchangeLocation
        | not (T.null (exLocation edata)) = exLocation edata
        | isBiosphere = fromMaybe "" activityLoc
        | otherwise = ""

    cas = if T.null (exCASNumber edata) then Nothing else Just (exCASNumber edata)

    -- EcoSpold1 never emits ReferenceInput (no waste-treatment encoding here).
    techRoleFor
        | isReferenceProduct = ReferenceProduct
        | isInput = Input
        | otherwise = Coproduct

    -- activityLinkId stays nil; the Loader resolves it later via
    -- (flowName, exchangeLocation) against supplier activities.
    wasteFlow = WasteFlow flowId (exName edata) unitId M.empty cas Nothing
    wasteEx =
        WasteExchange
            { waFlowId = flowId
            , waAmount = exMeanValue edata
            , waUnitId = unitId
            , -- Mirror isInput: final waste flows surface on inputGroup=5
              -- (consumer's POV: input from a hypothetical treatment service).
              waIsInput = isInput
            , waActivityLinkId = UUID.nil
            , waProcessLinkId = Nothing
            , waLocation = exchangeLocation
            , waComment = nonEmptyText (exComment edata)
            , waPedigree = Nothing
            }

    subCat = if T.null (exSubCategory edata) then Nothing else Just (exSubCategory edata)
    compartment =
        if T.null (exCategory edata) && isNothing subCat
            then Nothing
            else Just (Compartment (exCategory edata) subCat)
    bioFlow = BiosphereFlow flowId (exName edata) unitId M.empty cas Nothing compartment
    bioEx =
        BiosphereExchange
            { bioFlowId = flowId
            , bioAmount = exMeanValue edata
            , bioUnitId = unitId
            , bioDirection = if inputGroup == "4" then Resource else Emission
            , bioLocation = exchangeLocation
            , bioComment = nonEmptyText (exComment edata)
            , bioPedigree = Nothing
            }

    techFlow = TechnosphereFlow flowId (exName edata) unitId M.empty cas Nothing
    techEx =
        TechnosphereExchange
            { techFlowId = flowId
            , techAmount = exMeanValue edata
            , techUnitId = unitId
            , techRole = techRoleFor
            , techActivityLinkId = UUID.nil
            , techProcessLinkId = Nothing
            , techLocation = exchangeLocation
            , techComment = nonEmptyText (exComment edata)
            , techPedigree = Nothing
            }

-- | Build the final per-dataset result, applying the cut-off strategy.
buildResult :: ParseState -> Either String (Activity, [TechnosphereFlow], [BiosphereFlow], [WasteFlow], [Unit], Int, M.Map UUID Int)
buildResult st =
    let name = fromMaybe "Unknown Activity" (psActivityName st)
        location = fromMaybe "GLO" (psLocation st)
        -- "GLO" above is this loader's stand-in, not something the dataset said:
        -- a dataset without a geography is recorded as declaring none.
        locationSource = maybe LocationUnspecified declaredLocationSource (psLocation st)
        refUnit = fromMaybe "UNKNOWN_UNIT" (psRefUnit st)
        description = reverse (psDescription st)
        classifications =
            M.fromList $
                filter
                    (not . T.null . snd)
                    [("Category", psActivityCategory st), ("SubCategory", psActivitySubCategory st)]
        activity = Activity name description M.empty classifications location locationSource refUnit (reverse $ psExchanges st) M.empty M.empty Nothing Nothing Nothing Nothing Nothing
        pack act =
            ( act
            , reverse (psTechFlows st)
            , reverse (psBioFlows st)
            , reverse (psWasteFlows st)
            , reverse (psUnits st)
            , psDatasetNumber st
            , psSupplierLinks st
            )
     in pack <$> applyCutoffStrategy activity

-- | Run the shared SAX fold, surfacing any Xeno error as a String.
foldEcoSpold1 :: BS.ByteString -> Either String ParseState
foldEcoSpold1 =
    first show . X.fold onOpenTag onAttribute onEndOpen onText onCloseTag onText initialParseState

-- | Xeno SAX parser for EcoSpold1 — first dataset in the file.
parseWithXeno :: BS.ByteString -> Either String (Activity, [TechnosphereFlow], [BiosphereFlow], [WasteFlow], [Unit], Int, M.Map UUID Int)
parseWithXeno xmlContent = do
    finalState <- foldEcoSpold1 xmlContent
    case psCompletedActivities finalState of
        (result : _) -> result
        [] -> buildResult finalState

{- | Parse ALL datasets from an EcoSpold1 file (multi-dataset support).
Outer Either = XML parse failure; inner Either = per-activity failure.
-}
parseAllWithXeno :: BS.ByteString -> Either String [Either String (Activity, [TechnosphereFlow], [BiosphereFlow], [WasteFlow], [Unit], Int, M.Map UUID Int)]
parseAllWithXeno = fmap (reverse . psCompletedActivities) . foldEcoSpold1

-- | Parse EcoSpold1 file using Xeno SAX parser
streamParseActivityAndFlowsFromFile1 :: FilePath -> IO (Either String (Activity, [TechnosphereFlow], [BiosphereFlow], [WasteFlow], [Unit], Int, M.Map UUID Int))
streamParseActivityAndFlowsFromFile1 path = do
    !xmlContent <- BS.readFile path
    return (parseWithXeno xmlContent)

-- ============================================================================
-- Multi-dataset file support
-- ============================================================================

{- | Parse ALL datasets from a single EcoSpold1 file
Used for multi-dataset files where <ecoSpold> contains multiple <dataset> elements
Skips activities that fail (e.g. no reference product) and logs warnings
-}
streamParseAllDatasetsFromFile1 :: FilePath -> IO [(Activity, [TechnosphereFlow], [BiosphereFlow], [WasteFlow], [Unit], Int, M.Map UUID Int)]
streamParseAllDatasetsFromFile1 path = do
    !xmlContent <- BS.readFile path
    case parseAllWithXeno xmlContent of
        Right results -> do
            forM_ (lefts results) $ \e ->
                reportProgress Warning $ "Skipping dataset in " ++ path ++ ": " ++ e
            return (rights results)
        Left err -> do
            reportProgress Warning $ "Failed to parse " ++ path ++ ": " ++ err
            return []
