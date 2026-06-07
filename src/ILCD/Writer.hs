{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Serialize a VoLCA 'Database'/'SimpleDatabase' to an ILCD process-dataset
package — the inverse of "ILCD.Parser".

The output is a canonical, deterministic ILCD directory tree (and, optionally,
a zip of it) with four subdirectories:

@
  processes/      one XML per activity (keyed by activity UUID)
  flows/          one XML per tech/bio/waste flow
  flowproperties/ one XML per unit group (1:1 with units)
  unitgroups/     one XML per unit
@

Determinism is the contract:

* every Map/Set-derived list is sorted by key before emission;
* every 'Double' is formatted through one fixed formatter ('formatDouble');
* the only volatile field an ILCD reader/writer round-trip could disagree on
  — the export timestamp / generator string — is /omitted/ entirely unless a
  caller passes one explicitly via 'WriteOptions'. We never inject @now@, so
  @write (parse (write d)) == write d@ holds byte-for-byte.

What round-trips: process UUID, name, location, classifications, processType,
every exchange (flow ref, direction, amount, location, per-exchange comment), and the
full flow + unit catalog (names, CAS, biosphere compartment, flow type, the
flow→unit reference). These are exactly the fields "ILCD.Parser" reads back;
fields the parser drops (activity description, synonyms, params, allocation,
pedigree) are not representable in this ILCD profile and are not emitted.

The flow→unit indirection mirrors the parser's resolution chain
@flow → flowProperty → unitGroup@: we emit one flowProperty and one unitGroup
per VoLCA unit, all sharing the unit's UUID, so the parser reconstructs the
same unit key.
-}
module ILCD.Writer (
    -- * Options
    WriteOptions (..),
    defaultWriteOptions,

    -- * Writing
    writeILCDDatabase,
    writeILCDArchive,
    ilcdFiles,
    checkILCDExportable,

    -- * Pure helpers (exported for testing)
    escapeXml,
    escapeXmlAttr,
    formatDouble,
    processXML,
    flowXML,
    flowPropertyXML,
    unitGroupXML,
) where

import Codec.Archive.Zip (addEntryToArchive, emptyArchive, fromArchive, toEntry)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as TR
import qualified Data.UUID as UUID
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import EcoSpold.Common (showFFloatTrim)
import Types

--------------------------------------------------------------------------------
-- Options
--------------------------------------------------------------------------------

{- | Knobs that pin or omit volatile metadata. Defaults omit everything
volatile, so a write→parse→write round-trip is byte-stable.
-}
data WriteOptions = WriteOptions
    { woTimestamp :: !(Maybe Text)
    {- ^ Pinned export timestamp, emitted as @<common:timeStamp>@. 'Nothing'
    omits the element entirely (the parser ignores it either way).
    -}
    , woGenerator :: !(Maybe Text)
    -- ^ Pinned generator / tool-version string. 'Nothing' omits it.
    }

-- | Omit all volatile metadata. The right default for reproducible exports.
defaultWriteOptions :: WriteOptions
defaultWriteOptions = WriteOptions{woTimestamp = Nothing, woGenerator = Nothing}

--------------------------------------------------------------------------------
-- Top-level: directory / archive
--------------------------------------------------------------------------------

{- | Produce the full set of @(relativePath, contents)@ pairs for the ILCD
package. Pure and deterministic. The result is sorted by path.
-}
ilcdFiles :: WriteOptions -> SimpleDatabase -> [(FilePath, BS.ByteString)]
ilcdFiles opts db = sortOn fst (processes ++ flows ++ flowProps ++ unitGroups)
  where
    processes =
        [ ("processes" </> uuidStr actUUID <> ".xml", render (processXML opts db key act))
        | (key@(actUUID, _prodUUID), act) <- M.toAscList (sdbActivities db)
        ]

    flows =
        [ ("flows" </> uuidStr (flowKindId fk) <> ".xml", render (flowXML fk unitRef))
        | (fk, unitRef) <- allFlows db
        ]

    flowProps =
        [ ("flowproperties" </> uuidStr uid <> ".xml", render (flowPropertyXML u))
        | (uid, u) <- M.toAscList (sdbUnits db)
        ]

    unitGroups =
        [ ("unitgroups" </> uuidStr uid <> ".xml", render (unitGroupXML u))
        | (uid, u) <- M.toAscList (sdbUnits db)
        ]

    -- Render the list of lines to canonical UTF-8 bytes.
    render = TE.encodeUtf8 . renderLines

{- | Guard an ILCD export against data the parser cannot read back losslessly:

* /Multi-output activities./ ILCD identifies a process by a single dataset UUID
  with one process per file, keyed here on the activity UUID alone. A
  multi-output activity — several @(actUUID, prodUUID)@ entries sharing one
  @actUUID@ — would therefore write two processes to the same filename and the
  same @common:UUID@, and re-import could only keep one.

* /Non-canonical biosphere media./ 'compartmentBlock' emits @"Emissions to
  <medium>"@ for any non-resource medium, but the parser's @extractMedium@ only
  inverts the canonical air\/water\/soil\/natural-resource phrasings; any other
  medium (e.g. @"resource"@ from ES1\/ES2, or @"fresh water"@) re-imports under a
  different compartment, silently shifting LCIA scores.

* /Empty classification levels./ 'classificationBlock' joins levels with @"/"@
  and the parser splits on it, dropping empty parts. A value with an empty level
  — @""@ (key vanishes), or @"a//b"@ \/ @"a/"@ (collapses to @"a/b"@ \/ @"a"@) —
  therefore does not round-trip.

* /Amounts that do not re-parse./ 'formatDouble' is fixed-point, so a non-finite
  amount (@NaN@\/@Infinity@, which the parser can propagate from an out-of-range
  literal like @"1e400"@) or a subnormal near @5e-324@ would re-import as a
  different number (or fail to parse), silently shifting LCIA scores. We reject
  any amount that does not re-parse to itself.

Rather than silently corrupting any of these, report the first offending flow,
activity or exchange so the caller can fail loudly. Databases whose activity
UUIDs are all unique, whose biosphere media are all canonical, whose
classification levels are all non-empty and whose amounts all re-parse pass
unchanged.
-}
checkILCDExportable :: SimpleDatabase -> Either Text ()
checkILCDExportable db =
    checkMedia >> checkMultiOutput >> checkClassifications >> checkAmounts
  where
    -- Media the parser's @extractMedium@ inverts back to the same string.
    canonicalMedia = ["air", "water", "soil", "natural resource"]
    checkMedia =
        case [f | f <- M.elems (sdbBioFlows db), notInvertible f] of
            [] -> Right ()
            (f : _) ->
                Left $
                    "ILCD export cannot represent biosphere flow \""
                        <> bfName f
                        <> "\" (UUID "
                        <> uuidText (bfId f)
                        <> "): compartment medium \""
                        <> bfCompartmentName f
                        <> "\" is not one the ILCD parser can read back; "
                        <> "only air, water, soil and natural resource round-trip."
    notInvertible f = case bfCompartment f of
        Nothing -> False
        Just c -> compartmentName c `notElem` canonicalMedia

    checkMultiOutput =
        case M.toList collisions of
            [] -> Right ()
            ((actUUID, n) : _) ->
                Left $
                    "ILCD export cannot represent multi-output activity \""
                        <> nameOf actUUID
                        <> "\" (UUID "
                        <> uuidText actUUID
                        <> "): "
                        <> T.pack (show n)
                        <> " reference products share one activity UUID, which ILCD keys a process by."
    counts = M.fromListWith (+) [(actUUID, 1 :: Int) | (actUUID, _prodUUID) <- M.keys (sdbActivities db)]
    collisions = M.filter (> 1) counts
    nameOf actUUID =
        case [activityName act | ((a, _), act) <- M.toList (sdbActivities db), a == actUUID] of
            (nm : _) -> nm
            [] -> "?"

    -- A classification value round-trips only when none of its "/"-joined levels
    -- is empty; the parser splits on "/" and drops empty parts.
    checkClassifications =
        case [ (actUUID, name, value)
             | ((actUUID, _), act) <- M.toList (sdbActivities db)
             , (name, value) <- M.toList (activityClassification act)
             , any T.null (T.splitOn "/" value)
             ] of
            [] -> Right ()
            ((actUUID, name, value) : _) ->
                Left $
                    "ILCD export cannot represent classification \""
                        <> name
                        <> "\" = \""
                        <> value
                        <> "\" on activity \""
                        <> nameOf actUUID
                        <> "\" (UUID "
                        <> uuidText actUUID
                        <> "): an empty classification level does not round-trip "
                        <> "because the ILCD parser drops empty levels."

    -- An amount round-trips only when its fixed-point rendering re-parses to the
    -- same value through the parser's 'Data.Text.Read.double' (consuming all of
    -- it). Rejects non-finite amounts and the subnormal tail near 5e-324.
    checkAmounts =
        case [ (actUUID, exchangeAmount ex)
             | ((actUUID, _), act) <- M.toList (sdbActivities db)
             , ex <- exchanges act
             , not (amountRoundTrips (exchangeAmount ex))
             ] of
            [] -> Right ()
            ((actUUID, amt) : _) ->
                Left $
                    "ILCD export cannot represent amount "
                        <> T.pack (show amt)
                        <> " on activity \""
                        <> nameOf actUUID
                        <> "\" (UUID "
                        <> uuidText actUUID
                        <> "): it does not re-parse to the same value through the "
                        <> "ILCD parser (non-finite or near-underflow subnormal)."
    amountRoundTrips amt = case TR.double (formatDouble amt) of
        Right (v, rest) -> v == amt && T.null rest
        Left _ -> False

{- | Write the ILCD package as a directory tree rooted at @dir@, or return the
export guard's 'Left' without touching disk.
-}
writeILCDDatabase :: WriteOptions -> FilePath -> SimpleDatabase -> IO (Either Text ())
writeILCDDatabase opts dir db =
    case checkILCDExportable db of
        Left err -> pure (Left err)
        Right () -> do
            createDirectoryIfMissing True (dir </> "processes")
            createDirectoryIfMissing True (dir </> "flows")
            createDirectoryIfMissing True (dir </> "flowproperties")
            createDirectoryIfMissing True (dir </> "unitgroups")
            mapM_ (\(rel, bytes) -> BS.writeFile (dir </> rel) bytes) (ilcdFiles opts db)
            pure (Right ())

{- | Build a deterministic zip 'Archive' of the ILCD package and return its
serialized bytes. Entry modification times are pinned to epoch 0 so the
archive bytes are reproducible. Runs 'checkILCDExportable' first and returns its
'Left' on a database the format cannot represent faithfully, so an unguarded
caller cannot silently emit a corrupt archive.
-}
writeILCDArchive :: WriteOptions -> SimpleDatabase -> Either Text BL.ByteString
writeILCDArchive opts db = do
    checkILCDExportable db
    pure (fromArchive (buildArchive (ilcdFiles opts db)))
  where
    buildArchive = foldl addOne emptyArchive
    -- Fixed epoch (0) keeps archive bytes stable across runs.
    addOne arc (path, bytes) =
        addEntryToArchive (toEntry path 0 (BL.fromStrict bytes)) arc

--------------------------------------------------------------------------------
-- Flow enumeration (tech ∪ bio ∪ waste), tagged with their unit id
--------------------------------------------------------------------------------

-- | All flows in the database as 'FlowKind' + unit id, sorted by flow UUID.
allFlows :: SimpleDatabase -> [(FlowKind, UUID)]
allFlows db =
    sortOn (flowKindId . fst) $
        [(TechKind f, tfUnitId f) | f <- M.elems (sdbTechFlows db)]
            ++ [(BioKind f, bfUnitId f) | f <- M.elems (sdbBioFlows db)]
            ++ [(WasteKind f, wfUnitId f) | f <- M.elems (sdbWasteFlows db)]

--------------------------------------------------------------------------------
-- Process XML
--------------------------------------------------------------------------------

{- | Render one ILCD process dataset for an activity. The reference exchange
gets @dataSetInternalID@ matching @referenceToReferenceFlow@; remaining
exchanges follow in their list order, so the parser reads them back in the
same order it would re-serialize.
-}
processXML :: WriteOptions -> SimpleDatabase -> (UUID, UUID) -> Activity -> [Text]
processXML opts _db (actUUID, _prodUUID) act =
    [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    , "<processDataSet xmlns=\"http://lca.jrc.it/ILCD/Process\" xmlns:common=\"http://lca.jrc.it/ILCD/Common\">"
    , "  <processInformation>"
    , "    <dataSetInformation>"
    , elem' "common:UUID" (uuidText actUUID)
    , "      <name>"
    , attrElem "baseName" [("xml:lang", "en")] (activityName act)
    , "      </name>"
    ]
        ++ classificationBlock (activityClassification act)
        ++ [ "    </dataSetInformation>"
           , attrOnly "geography" [("location", activityLocation act)]
           , "    <quantitativeReference>"
           , elem' "referenceToReferenceFlow" (T.pack (show refIdx))
           , "    </quantitativeReference>"
           ]
        ++ timeStampBlock opts
        ++ [ "  </processInformation>"
           ]
        ++ processTypeBlock (activityNativeType act)
        ++ generatorBlock opts
        ++ ["  <exchanges>"]
        ++ concatMap (uncurry exchangeXML) indexedExchanges
        ++ [ "  </exchanges>"
           , "</processDataSet>"
           ]
  where
    -- Exchanges are emitted in list order; reference index is the position of
    -- the (first) reference exchange. When none is marked, emit a sentinel past
    -- the last internalID so the parser's findRefExchange matches no exchange
    -- and preserves the "no reference" state instead of fabricating one at 0.
    indexedExchanges = zip [0 ..] (exchanges act)
    refIdx :: Int
    refIdx = case [i | (i, ex) <- indexedExchanges, exchangeIsReference ex] of
        (i : _) -> i
        [] -> length indexedExchanges

-- | One @<exchange>@ block. @i@ is the @dataSetInternalID@.
exchangeXML :: Int -> Exchange -> [Text]
exchangeXML i ex =
    [ "    <exchange dataSetInternalID=\"" <> T.pack (show i) <> "\">"
    , attrOnly "referenceToFlowDataSet" [("refObjectId", uuidText (exchangeFlowId ex)), ("type", "flow data set")]
    , elem' "exchangeDirection" direction
    , elem' "resultingAmount" (formatDouble (exchangeAmount ex))
    ]
        ++ locationBlock (exchangeLocation ex)
        ++ commentBlock (exchangeComment ex)
        ++ ["    </exchange>"]
  where
    direction = if exchangeIsInput ex then "Input" else "Output"

{- | Per-exchange @<location>@, which the parser reads back into the exchange's
location field. Omitted when empty — the common case, since ILCD geography lives
at the process level — so it never perturbs the byte-stable round-trip.
-}
locationBlock :: Text -> [Text]
locationBlock loc
    | T.null loc = []
    | otherwise = [elem' "location" loc]

-- | Per-exchange comment, English-tagged to match the parser's preference.
commentBlock :: Maybe Text -> [Text]
commentBlock Nothing = []
commentBlock (Just c) = [attrElem "common:generalComment" [("xml:lang", "en")] c]

{- | Classification block. Each classification system becomes one
@<common:classification name="...">@ with one @<common:class>@ per
"/"-joined level, preserving the parser's join semantics. Systems are
sorted by name for determinism.
-}
classificationBlock :: M.Map Text Text -> [Text]
classificationBlock cls
    | M.null cls = []
    | otherwise =
        ["      <classificationInformation>"]
            ++ concatMap system (M.toAscList cls)
            ++ ["      </classificationInformation>"]
  where
    system (name, value) =
        [attrOnlyOpen "common:classification" [("name", name)]]
            ++ [ attrElem "common:class" [("level", T.pack (show lvl))] part
               | (lvl :: Int, part) <- zip [0 ..] (T.splitOn "/" value)
               ]
            ++ ["        </common:classification>"]

{- | ILCD @<processType>@, nested where the parser expects it. @<processType>@
is free text, so a foreign native-type label (SimaPro @Type@, ecospold
@activityType@) is carried through rather than dropped. Omitted when no native
type is set or its label is empty.
-}
processTypeBlock :: Maybe NativeActivityType -> [Text]
processTypeBlock nt = case nativeTypeLabel nt of
    Just label
        | not (T.null label) ->
            [ "  <modellingAndValidation>"
            , "    <LCIMethodAndAllocation>"
            , elem' "processType" label
            , "    </LCIMethodAndAllocation>"
            , "  </modellingAndValidation>"
            ]
    Just _ -> []
    Nothing -> []

{- | Native-type display label, across all source formats (the value carried
into @<processType>@). 'Nothing' when no native type is set.
-}
nativeTypeLabel :: Maybe NativeActivityType -> Maybe Text
nativeTypeLabel nt = case nt of
    Just (ILCDProcessType label) -> Just label
    Just (SimaProProcessType label) -> Just label
    Just (EcoSpoldActivityType{eatLabel = label}) -> Just label
    Nothing -> Nothing

-- | Optional pinned timestamp inside @<processInformation>@ (omitted by default).
timeStampBlock :: WriteOptions -> [Text]
timeStampBlock opts = case woTimestamp opts of
    Nothing -> []
    Just ts -> [elem' "common:timeStamp" ts]

-- | Optional pinned generator string (omitted by default).
generatorBlock :: WriteOptions -> [Text]
generatorBlock opts = case woGenerator opts of
    Nothing -> []
    Just g ->
        [ "  <administrativeInformation>"
        , "    <dataGenerator>"
        , elem' "common:referenceToDataGenerator" g
        , "    </dataGenerator>"
        , "  </administrativeInformation>"
        ]

--------------------------------------------------------------------------------
-- Flow XML
--------------------------------------------------------------------------------

{- | Render one ILCD flow dataset. @typeOfDataSet@ is set so the parser's
'classifyFlowType' re-buckets the flow into the same kind. Biosphere flows
carry their compartment categories; CAS round-trips when present.
-}
flowXML :: FlowKind -> UUID -> [Text]
flowXML fk unitRef =
    [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    , "<flowDataSet xmlns=\"http://lca.jrc.it/ILCD/Flow\" xmlns:common=\"http://lca.jrc.it/ILCD/Common\">"
    , "  <flowInformation>"
    , "    <dataSetInformation>"
    , elem' "common:UUID" (uuidText (flowKindId fk))
    , "      <name>"
    , attrElem "baseName" [("xml:lang", "en")] (flowKindName fk)
    , "      </name>"
    ]
        ++ compartmentBlock (flowKindCompartment fk)
        ++ casBlock (flowKindCAS fk)
        ++ [ "    </dataSetInformation>"
           , "    <quantitativeReference>"
           , elem' "referenceToReferenceFlowProperty" "0"
           , "    </quantitativeReference>"
           , "  </flowInformation>"
           , "  <modellingAndValidation>"
           , "    <LCIMethod>"
           , elem' "typeOfDataSet" (flowTypeText fk)
           , "    </LCIMethod>"
           , "  </modellingAndValidation>"
           , "  <flowProperties>"
           , "    <flowProperty dataSetInternalID=\"0\">"
           , attrOnly "referenceToFlowPropertyDataSet" [("refObjectId", uuidText unitRef), ("type", "flow property data set")]
           , elem' "meanValue" "1"
           , "    </flowProperty>"
           , "  </flowProperties>"
           , "</flowDataSet>"
           ]

-- | CAS accessor across flow kinds (parser reads it for biosphere flows).
flowKindCAS :: FlowKind -> Maybe Text
flowKindCAS (TechKind f) = tfCAS f
flowKindCAS (BioKind f) = bfCAS f
flowKindCAS (WasteKind f) = wfCAS f

-- | @typeOfDataSet@ string mirroring 'ILCD.Parser.classifyFlowType'.
flowTypeText :: FlowKind -> Text
flowTypeText (TechKind _) = "Product flow"
flowTypeText (BioKind _) = "Elementary flow"
flowTypeText (WasteKind _) = "Waste flow"

casBlock :: Maybe Text -> [Text]
casBlock Nothing = []
casBlock (Just cas)
    | T.null cas = []
    | otherwise = [elem' "CASNumber" cas]

{- | Emit the biosphere @elementaryFlowCategorization@. We reverse the parser's
'parseCompartment': level 0 is "Emissions"/"Resources", level 1 names the
medium, level 2 (when a sub-compartment exists) is "<medium-phrase>, <sub>".
-}
compartmentBlock :: Maybe Compartment -> [Text]
compartmentBlock Nothing = []
compartmentBlock (Just (Compartment medium sub)) =
    [ "      <classificationInformation>"
    , "        <common:elementaryFlowCategorization>"
    , attrElem "common:category" [("level", "0")] level0
    , attrElem "common:category" [("level", "1")] level1
    ]
        ++ level2
        ++ [ "        </common:elementaryFlowCategorization>"
           , "      </classificationInformation>"
           ]
  where
    isResource = medium == "natural resource"
    level0 = if isResource then "Resources" else "Emissions"
    level1
        | isResource = "Resources"
        | otherwise = "Emissions to " <> medium
    level2 = case sub of
        Nothing -> []
        Just s
            | T.null s -> []
            | isResource -> [attrElem "common:category" [("level", "2")] ("Resources " <> s)]
            | otherwise -> [attrElem "common:category" [("level", "2")] ("Emissions to " <> medium <> ", " <> s)]

--------------------------------------------------------------------------------
-- FlowProperty XML  (one per unit; shares the unit's UUID)
--------------------------------------------------------------------------------

flowPropertyXML :: Unit -> [Text]
flowPropertyXML u =
    [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    , "<flowPropertyDataSet xmlns=\"http://lca.jrc.it/ILCD/FlowProperty\" xmlns:common=\"http://lca.jrc.it/ILCD/Common\">"
    , "  <flowPropertiesInformation>"
    , "    <dataSetInformation>"
    , elem' "common:UUID" (uuidText (unitId u))
    , attrElem "common:name" [("xml:lang", "en")] (unitName u)
    , "    </dataSetInformation>"
    , "    <quantitativeReference>"
    , attrOnly "referenceToReferenceUnitGroup" [("refObjectId", uuidText (unitId u)), ("type", "unit group data set")]
    , "    </quantitativeReference>"
    , "  </flowPropertiesInformation>"
    , "</flowPropertyDataSet>"
    ]

--------------------------------------------------------------------------------
-- UnitGroup XML  (one per unit; shares the unit's UUID; single reference unit)
--------------------------------------------------------------------------------

unitGroupXML :: Unit -> [Text]
unitGroupXML u =
    [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    , "<unitGroupDataSet xmlns=\"http://lca.jrc.it/ILCD/UnitGroup\" xmlns:common=\"http://lca.jrc.it/ILCD/Common\">"
    , "  <unitGroupInformation>"
    , "    <dataSetInformation>"
    , elem' "common:UUID" (uuidText (unitId u))
    , attrElem "common:name" [("xml:lang", "en")] (unitName u)
    , "    </dataSetInformation>"
    , "    <quantitativeReference>"
    , elem' "referenceToReferenceUnit" "0"
    , "    </quantitativeReference>"
    , "  </unitGroupInformation>"
    , "  <units>"
    , "    <unit dataSetInternalID=\"0\">"
    , elem' "name" (unitName u)
    , elem' "meanValue" "1"
    , "    </unit>"
    , "  </units>"
    , "</unitGroupDataSet>"
    ]

--------------------------------------------------------------------------------
-- XML primitives
--------------------------------------------------------------------------------

-- | Join the rendered lines with newlines and a trailing newline.
renderLines :: [Text] -> Text
renderLines = (<> "\n") . T.intercalate "\n"
{-# INLINE renderLines #-}

-- | @<tag>escaped-text</tag>@ on one indented line.
elem' :: Text -> Text -> Text
elem' tag txt = "      <" <> tag <> ">" <> escapeXml txt <> "</" <> tag <> ">"

-- | @<tag a=\"v\" ...>escaped</tag>@ on one indented line.
attrElem :: Text -> [(Text, Text)] -> Text -> Text
attrElem tag attrs txt =
    "      <" <> tag <> attrsText attrs <> ">" <> escapeXml txt <> "</" <> tag <> ">"

-- | Self-closing @<tag a=\"v\" .../>@.
attrOnly :: Text -> [(Text, Text)] -> Text
attrOnly tag attrs = "      <" <> tag <> attrsText attrs <> "/>"

-- | Open-only @<tag a=\"v\" ...>@ (caller emits the close).
attrOnlyOpen :: Text -> [(Text, Text)] -> Text
attrOnlyOpen tag attrs = "        <" <> tag <> attrsText attrs <> ">"

attrsText :: [(Text, Text)] -> Text
attrsText = T.concat . map (\(k, v) -> " " <> k <> "=\"" <> escapeXmlAttr v <> "\"")

{- | XML text-node escaping. Covers the three entities that matter in element
content (@&@, @<@, @>@). The parser (Xeno SAX) un-escapes these, so escaping
here keeps the round-trip faithful for names/comments containing @&@, @<@, etc.
-}
escapeXml :: Text -> Text
escapeXml =
    T.replace ">" "&gt;"
        . T.replace "<" "&lt;"
        . T.replace "&" "&amp;"

{- | Attribute-value escaping: the text entities, the quote characters, and the
newline/carriage-return control characters. A raw @\\n@/@\\r@ inside an attribute
value is normalised to a space by XML parsers, so encode it as a numeric
character reference (matching the EcoSpold2 writer) to keep it verbatim across a
round trip.
-}
escapeXmlAttr :: Text -> Text
escapeXmlAttr =
    T.replace "\r" "&#13;"
        . T.replace "\n" "&#10;"
        . T.replace "'" "&apos;"
        . T.replace "\"" "&quot;"
        . escapeXml

{- | Canonical 'Double' formatting via the shared 'showFFloatTrim' (fixed-point,
never scientific), so the value round-trips through the parser's
'Data.Text.Read.double' for the magnitudes real LCA amounts occupy — unlike
@show@, which emits scientific notation that re-reads lossily (e.g. @show 3.3e-20@
→ @3.2999999999999994e-20@). The near-underflow subnormal tail (≈@5e-324@) is the
exception (fixed-point can't carry enough digits, so it re-parses to @0@);
'checkILCDExportable' rejects any amount that does not re-parse, so the guarded
path never emits one. A non-finite value renders as its (non-parseable)
@"NaN"@/@"Infinity"@ form so a bad re-import fails loudly rather than silently
reading @0@; the guard rejects non-finite amounts first. Negative zero is
normalised to @0.0@.
-}
formatDouble :: Double -> Text
formatDouble x
    | isNaN x || isInfinite x = T.pack (show x)
    | x == 0 = "0.0"
    | otherwise = T.pack (showFFloatTrim x)

--------------------------------------------------------------------------------
-- UUID rendering
--------------------------------------------------------------------------------

uuidText :: UUID -> Text
uuidText = UUID.toText

uuidStr :: UUID -> FilePath
uuidStr = UUID.toString
