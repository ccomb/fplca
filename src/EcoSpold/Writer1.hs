{-# LANGUAGE OverloadedStrings #-}

{- | EcoSpold1 export writer — the inverse of "EcoSpold.Parser1".

Serializes a 'Database' / 'SimpleDatabase' back to the EcoSpold1 XML format
(Ecoinvent 2.x, @http://www.EcoInvent.org/EcoSpold01@). The output is
__canonical and deterministic__:

  * activities are emitted in a stable order (sorted by @(activityName,
    location)@), each in its own @\<dataset\>@ inside one @\<ecoSpold\>@;
  * dataset numbers are assigned sequentially (1-based) in that order so a
    re-parse regenerates the exact same flow UUIDs (the parser derives
    flow UUIDs from @datasetNumber@, exchange number, name and category);
  * exchange numbers are sequential (1-based) in exchange order;
  * attributes appear in a fixed order, classification maps are sorted by
    key, numbers use a fixed textual form, and there is no insignificant
    whitespace beyond a single newline between top-level lines;
  * volatile metadata (@generator@, @timestamp@) is pinned or omitted via
    'WriterOptions' so a write→parse→write round-trip is byte-stable.

The mapping mirrors 'EcoSpold.Parser1.buildExchange' exactly:

  * 'ReferenceProduct' / 'ReferenceInput' → @\<outputGroup\>0\</outputGroup\>@
  * 'Coproduct'                          → @\<outputGroup\>1\</outputGroup\>@
  * technosphere 'Input'                 → @\<inputGroup\>5\</inputGroup\>@
    (parser treats any non-empty inputGroup as a tech input)
  * biosphere 'Resource'                 → @\<inputGroup\>4\</inputGroup\>@
  * biosphere 'Emission'                 → @\<outputGroup\>4\</outputGroup\>@
  * waste output (waIsInput=False)       → @\<outputGroup\>1\</outputGroup\>@,
    category="Final waste flows"
  * waste input  (waIsInput=True)        → @\<inputGroup\>5\</inputGroup\>@,
    category="Final waste flows"

Flow names, categories, CAS numbers and units are not stored on the
'Exchange'; they live on the flow / unit tables and are resolved by UUID.
A biosphere flow's @category@ / @subCategory@ come from its 'Compartment';
technosphere flows carry no category (the parser only used it to seed the
UUID), and waste flows always re-emit @category="Final waste flows"@ so the
parser's waste-routing rule fires again.
-}
module EcoSpold.Writer1 (
    -- * Options
    WriterOptions (..),
    defaultWriterOptions,
    canonicalWriterOptions,

    -- * Writers
    writeDatabase,
    writeSimpleDatabase,
    writeActivities,

    -- * Pure helpers (exported for testing)
    escapeXmlAttr,
    formatAmount,
) where

import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Numeric (showFFloat)
import Types

-- ----------------------------------------------------------------------------
-- Options
-- ----------------------------------------------------------------------------

{- | Knobs controlling the volatile, non-semantic parts of the output.
Keeping these out of band lets a round-trip be byte-stable: write with
'canonicalWriterOptions' (no @generator@/@timestamp@) and the second write
reproduces the first exactly.
-}
data WriterOptions = WriterOptions
    { woGenerator :: !(Maybe Text)
    -- ^ Value of the @\<dataset generator=...\>@ attribute, or 'Nothing' to omit.
    , woTimestamp :: !(Maybe Text)
    -- ^ Value of the @\<dataset timestamp=...\>@ attribute, or 'Nothing' to omit.
    }
    deriving (Eq, Show)

-- | Self-describing default: pins generator to "VoLCA", omits the timestamp.
defaultWriterOptions :: WriterOptions
defaultWriterOptions = WriterOptions (Just "VoLCA") Nothing

{- | Fully canonical: omits both volatile attributes. Use this for stable
round-trip / golden tests.
-}
canonicalWriterOptions :: WriterOptions
canonicalWriterOptions = WriterOptions Nothing Nothing

-- ----------------------------------------------------------------------------
-- Top-level writers
-- ----------------------------------------------------------------------------

-- | Serialize a built 'Database' (via 'toSimpleDatabase').
writeDatabase :: WriterOptions -> Database -> Text
writeDatabase opts = writeSimpleDatabase opts . toSimpleDatabase

-- | Serialize a 'SimpleDatabase'. Flow / unit names are resolved from its tables.
writeSimpleDatabase :: WriterOptions -> SimpleDatabase -> Text
writeSimpleDatabase opts sdb =
    writeActivities
        opts
        (sdbTechFlows sdb)
        (sdbBioFlows sdb)
        (sdbWasteFlows sdb)
        (sdbUnits sdb)
        (M.elems (sdbActivities sdb))

{- | Serialize a list of activities against the flow / unit tables that
resolve their exchange UUIDs.
-}
writeActivities ::
    WriterOptions ->
    TechFlowDB ->
    BioFlowDB ->
    WasteFlowDB ->
    UnitDB ->
    [Activity] ->
    Text
writeActivities opts techs bios wastes units activities =
    T.unlines $
        [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        , "<ecoSpold xmlns=\"http://www.EcoInvent.org/EcoSpold01\">"
        ]
            ++ concat (zipWith (datasetLines opts res) [1 ..] ordered)
            ++ ["</ecoSpold>"]
  where
    res = Resolvers techs bios wastes units
    -- Stable, content-derived order so dataset numbers (and thus flow UUIDs)
    -- are reproducible regardless of the source Map's ordering.
    ordered = sortOn (\a -> (activityName a, activityLocation a)) activities

-- | Bundle of lookup tables threaded through the per-exchange renderers.
data Resolvers = Resolvers
    { rTech :: !TechFlowDB
    , rBio :: !BioFlowDB
    , rWaste :: !WasteFlowDB
    , rUnits :: !UnitDB
    }

-- ----------------------------------------------------------------------------
-- Per-dataset rendering
-- ----------------------------------------------------------------------------

-- | Render one @\<dataset\>@ block (already indented), as a list of lines.
datasetLines :: WriterOptions -> Resolvers -> Int -> Activity -> [Text]
datasetLines opts res num act =
    [ indent 1 <> "<dataset" <> datasetAttrs opts num <> ">"
    , indent 2 <> "<metaInformation>"
    , indent 3 <> "<processInformation>"
    , indent 4 <> "<referenceFunction" <> refFunctionAttrs act <> "/>"
    , indent 4 <> "<geography location=" <> attr (activityLocation act) <> "/>"
    , indent 3 <> "</processInformation>"
    , indent 2 <> "</metaInformation>"
    , indent 2 <> "<flowData>"
    ]
        ++ concat (zipWith (exchangeLines res) [1 ..] (exchanges act))
        ++ [ indent 2 <> "</flowData>"
           , indent 1 <> "</dataset>"
           ]

{- | @\<dataset\>@ attributes: @number@ always, then the volatile @generator@
and @timestamp@ only when the options provide them.
-}
datasetAttrs :: WriterOptions -> Int -> Text
datasetAttrs opts num =
    " number="
        <> attr (T.pack (show num))
        <> maybe "" (\g -> " generator=" <> attr g) (woGenerator opts)
        <> maybe "" (\t -> " timestamp=" <> attr t) (woTimestamp opts)

{- | @\<referenceFunction\>@ attributes in a fixed order. @category@ /
@subCategory@ come from 'activityClassification' (the parser's @Category@ /
@SubCategory@ keys); a non-empty activity description is joined and emitted
as @generalComment@.
-}
refFunctionAttrs :: Activity -> Text
refFunctionAttrs act =
    " name="
        <> attr (activityName act)
        <> optAttr "category" (classif "Category")
        <> optAttr "subCategory" (classif "SubCategory")
        <> " unit="
        <> attr (activityUnit act)
        <> optAttr "generalComment" generalComment
  where
    classif k = M.findWithDefault "" k (activityClassification act)
    generalComment = T.intercalate "\n" (activityDescription act)

-- ----------------------------------------------------------------------------
-- Exchange rendering
-- ----------------------------------------------------------------------------

{- | Render one @\<exchange\>@ as opening tag + group element + closing tag.
The group element (@inputGroup@ / @outputGroup@) is what the parser keys on
to classify the exchange, so it is derived from the exchange variant +
role/direction, never guessed.
-}
exchangeLines :: Resolvers -> Int -> Exchange -> [Text]
exchangeLines res num ex =
    [ indent 3 <> "<exchange" <> exchangeAttrs res num ex <> ">"
    , indent 4 <> groupElement ex
    , indent 3 <> "</exchange>"
    ]

{- | Common exchange attributes in fixed order: number, name, category,
subCategory, location, unit, meanValue, then optional CAS / generalComment.
Name and category are the inputs the parser hashes into the flow UUID, so
they must be reproduced verbatim for a stable round-trip.
-}
exchangeAttrs :: Resolvers -> Int -> Exchange -> Text
exchangeAttrs res num ex =
    " number="
        <> attr (T.pack (show num))
        <> " name="
        <> attr name
        <> optAttr "category" category
        <> optAttr "subCategory" subCategory
        <> optAttr "location" (exchangeLocation ex)
        <> " unit="
        <> attr (unitNameFor (rUnits res) (exchangeUnitId ex))
        <> " meanValue="
        <> attr (formatAmount (exchangeAmount ex))
        <> maybe "" (\c -> " CASNumber=" <> attr c) cas
        <> maybe "" (\c -> " generalComment=" <> attr c) (exchangeComment ex)
  where
    FlowFields name category subCategory cas = flowFields res ex

-- | The @\<inputGroup\>@ / @\<outputGroup\>@ element for an exchange.
groupElement :: Exchange -> Text
groupElement ex = case ex of
    TechnosphereExchange{techRole = role} -> case role of
        ReferenceProduct -> wrapOut "0"
        ReferenceInput -> wrapOut "0"
        Coproduct -> wrapOut "1"
        Input -> wrapIn "5"
    BiosphereExchange{bioDirection = dir} -> case dir of
        Resource -> wrapIn "4"
        Emission -> wrapOut "4"
    WasteExchange{waIsInput = isInput} ->
        if isInput then wrapIn "5" else wrapOut "1"
  where
    wrapIn g = "<inputGroup>" <> g <> "</inputGroup>"
    wrapOut g = "<outputGroup>" <> g <> "</outputGroup>"

-- ----------------------------------------------------------------------------
-- Flow-field resolution (name / category / subCategory / CAS by UUID)
-- ----------------------------------------------------------------------------

{- | The four flow-derived attribute values the parser stored on a flow:
name, category, subCategory, CAS. Positional — always consumed as a whole.
-}
data FlowFields = FlowFields !Text !Text !Text !(Maybe Text)

{- | Resolve a flow's serialised fields from the matching table. A biosphere
flow's compartment becomes category/subCategory; a waste flow always carries
@category="Final waste flows"@ so the parser re-routes it to the waste
bucket; a technosphere flow has no category. A UUID absent from its table
yields empty/Nothing — never a crash.
-}
flowFields :: Resolvers -> Exchange -> FlowFields
flowFields res ex = case ex of
    TechnosphereExchange{techFlowId = fid} ->
        case M.lookup fid (rTech res) of
            Just tf -> FlowFields (tfName tf) "" "" (tfCAS tf)
            Nothing -> FlowFields "" "" "" Nothing
    BiosphereExchange{bioFlowId = fid} ->
        case M.lookup fid (rBio res) of
            Just bf ->
                FlowFields
                    (bfName bf)
                    (bfCompartmentName bf)
                    (maybe "" id (bfCompartmentSub bf))
                    (bfCAS bf)
            Nothing -> FlowFields "" "" "" Nothing
    WasteExchange{waFlowId = fid} ->
        case M.lookup fid (rWaste res) of
            Just wf -> FlowFields (wfName wf) "Final waste flows" "" (wfCAS wf)
            Nothing -> FlowFields "" "Final waste flows" "" Nothing

{- | Resolve a unit UUID to its name. The parser stored both unit name and
symbol as the source unit string, so the name field is the faithful echo.
-}
unitNameFor :: UnitDB -> UUID -> Text
unitNameFor units uid = maybe "" unitName (M.lookup uid units)

-- ----------------------------------------------------------------------------
-- Canonical encoding helpers
-- ----------------------------------------------------------------------------

-- | Two-space indentation, @n@ levels deep.
indent :: Int -> Text
indent n = T.replicate (n * 2) " "

-- | A double-quoted, XML-escaped attribute value.
attr :: Text -> Text
attr v = "\"" <> escapeXmlAttr v <> "\""

-- | Emit @ name="value"@ only when the value is non-empty; otherwise nothing.
optAttr :: Text -> Text -> Text
optAttr name v
    | T.null v = ""
    | otherwise = " " <> name <> "=" <> attr v

{- | Escape the five XML metacharacters plus the newline / carriage-return
control characters, matching the entity set 'EcoSpold.Common.decodeXmlEntities'
decodes on the way in. Order matters: @&@ is escaped first so we don't
double-escape the entities we introduce.
-}
escapeXmlAttr :: Text -> Text
escapeXmlAttr =
    T.replace "\r" "&#13;"
        . T.replace "\n" "&#10;"
        . T.replace "'" "&apos;"
        . T.replace "\"" "&quot;"
        . T.replace ">" "&gt;"
        . T.replace "<" "&lt;"
        . T.replace "&" "&amp;"

{- | Deterministic textual form for a @meanValue@. @show@ on a 'Double' is
already round-trippable and stable across platforms, but renders whole
numbers as @1.0@ (which the parser reads back identically), so it is the
simplest canonical choice. Negative zero is normalised to @0.0@.
-}
formatAmount :: Double -> Text
formatAmount x
    | x == 0 = "0.0"
    | isNaN x || isInfinite x = T.pack (showFFloat Nothing x "")
    | otherwise = T.pack (show x)
