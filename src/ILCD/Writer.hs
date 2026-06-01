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
every exchange (flow ref, direction, amount, per-exchange comment), and the
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
import qualified Data.UUID as UUID
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

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

{- | Guard an ILCD export against multi-output activities. ILCD identifies a
process by a single dataset UUID with one process per file, keyed here on the
activity UUID alone. A multi-output activity — several @(actUUID, prodUUID)@
entries sharing one @actUUID@ — would therefore write two processes to the same
filename and the same @common:UUID@, and re-import could only keep one. Rather
than silently dropping all but one product's process dataset, report the first
offending activity so the caller can fail loudly. Single-output databases (every
@actUUID@ unique) pass unchanged.
-}
checkILCDExportable :: SimpleDatabase -> Either Text ()
checkILCDExportable db =
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
  where
    counts = M.fromListWith (+) [(actUUID, 1 :: Int) | (actUUID, _prodUUID) <- M.keys (sdbActivities db)]
    collisions = M.filter (> 1) counts
    nameOf actUUID =
        case [activityName act | ((a, _), act) <- M.toList (sdbActivities db), a == actUUID] of
            (nm : _) -> nm
            [] -> "?"

-- | Write the ILCD package as a directory tree rooted at @dir@.
writeILCDDatabase :: WriteOptions -> FilePath -> SimpleDatabase -> IO ()
writeILCDDatabase opts dir db = do
    createDirectoryIfMissing True (dir </> "processes")
    createDirectoryIfMissing True (dir </> "flows")
    createDirectoryIfMissing True (dir </> "flowproperties")
    createDirectoryIfMissing True (dir </> "unitgroups")
    mapM_ (\(rel, bytes) -> BS.writeFile (dir </> rel) bytes) (ilcdFiles opts db)

{- | Build a deterministic zip 'Archive' of the ILCD package and return its
serialized bytes. Entry modification times are pinned to epoch 0 so the
archive bytes are reproducible.
-}
writeILCDArchive :: WriteOptions -> SimpleDatabase -> BL.ByteString
writeILCDArchive opts db = fromArchive (buildArchive (ilcdFiles opts db))
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
    -- the (first) reference exchange, defaulting to 0 when none is marked.
    indexedExchanges = zip [0 ..] (exchanges act)
    refIdx :: Int
    refIdx = case [i | (i, ex) <- indexedExchanges, exchangeIsReference ex] of
        (i : _) -> i
        [] -> 0

-- | One @<exchange>@ block. @i@ is the @dataSetInternalID@.
exchangeXML :: Int -> Exchange -> [Text]
exchangeXML i ex =
    [ "    <exchange dataSetInternalID=\"" <> T.pack (show i) <> "\">"
    , attrOnly "referenceToFlowDataSet" [("refObjectId", uuidText (exchangeFlowId ex)), ("type", "flow data set")]
    , elem' "exchangeDirection" direction
    , elem' "resultingAmount" (formatDouble (exchangeAmount ex))
    ]
        ++ commentBlock (exchangeComment ex)
        ++ ["    </exchange>"]
  where
    direction = if exchangeIsInput ex then "Input" else "Output"

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

{- | ILCD @<processType>@, nested where the parser expects it. Omitted unless
the activity's native type is an ILCD process type.
-}
processTypeBlock :: Maybe NativeActivityType -> [Text]
processTypeBlock nt = case nt of
    Just (ILCDProcessType label)
        | not (T.null label) ->
            [ "  <modellingAndValidation>"
            , "    <LCIMethodAndAllocation>"
            , elem' "processType" label
            , "    </LCIMethodAndAllocation>"
            , "  </modellingAndValidation>"
            ]
    Just (ILCDProcessType _) -> []
    Just (EcoSpoldActivityType{}) -> []
    Just (SimaProProcessType{}) -> []
    Nothing -> []

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
    mediumWord = case medium of
        "natural resource" -> "natural resource"
        other -> other
    level1
        | isResource = "Resources"
        | otherwise = "Emissions to " <> mediumWord
    level2 = case sub of
        Nothing -> []
        Just s
            | T.null s -> []
            | isResource -> [attrElem "common:category" [("level", "2")] ("Resources " <> s)]
            | otherwise -> [attrElem "common:category" [("level", "2")] ("Emissions to " <> mediumWord <> ", " <> s)]

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

{- | XML text-node escaping. Covers the five predefined entities. The parser
(Xeno SAX) un-escapes these, so escaping here keeps the round-trip faithful
for names/comments containing @&@, @<@, etc.
-}
escapeXml :: Text -> Text
escapeXml =
    T.replace ">" "&gt;"
        . T.replace "<" "&lt;"
        . T.replace "&" "&amp;"

-- | Attribute-value escaping: text entities plus the quote characters.
escapeXmlAttr :: Text -> Text
escapeXmlAttr =
    T.replace "'" "&apos;"
        . T.replace "\"" "&quot;"
        . escapeXml

{- | Canonical 'Double' formatting. Integral values print without a trailing
@.0@-noise beyond a single @.0@ is avoided by emitting whole numbers as
integers (matching how the fixtures write @1@ for unit mean values), and
fractional values use 'show', which is round-trippable through
'Data.Text.Read.double' (the parser's reader). The two together make
write→parse→write stable.
-}
formatDouble :: Double -> Text
formatDouble x
    | isNaN x = "0"
    | isInfinite x = "0"
    | x == fromIntegral (round x :: Integer) = T.pack (show (round x :: Integer))
    | otherwise = T.pack (show x)

--------------------------------------------------------------------------------
-- UUID rendering
--------------------------------------------------------------------------------

uuidText :: UUID -> Text
uuidText = UUID.toText

uuidStr :: UUID -> FilePath
uuidStr = UUID.toString
