{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

{- |
Module      : Database.Loader
Description : High-performance EcoSpold XML loading with matrix caching

This module provides optimized loading of EcoSpold XML files together with a
single cache storing the fully indexed database and pre-computed sparse
matrices. When the cache is absent or invalidated, the loader reparses all
EcoSpold datasets, builds the in-memory structures, and writes the matrix cache
for subsequent runs.

Key performance features:
- Parallel parsing with controlled concurrency (prevents resource exhaustion)
- Automatic cache invalidation when the schema or the build inputs change
- Memory-efficient chunked processing for large databases
- Hash-based cache filenames for multi-dataset support

Cache performance (Ecoinvent 3.8 with 18K activities):
- Cold start (XML parsing + matrix build): ~45s
- Matrix cache hit: ~0.5s

The cache keeps day-to-day execution fast while preserving reproducibility.
-}
module Database.Loader (
    -- * Main Loading Functions
    loadDatabase,
    LoadOptions (..),
    defaultLoadOptions,
    loadDatabaseWithLocationAliases,
    reportKeyRefusals,
    loadSimaProCSV,
    loadDatabaseWithCrossDBLinking,
    findFilesByExtRecursive,

    -- * Cache Operations
    loadCachedDatabaseWithMatrices,
    saveCachedDatabaseWithMatrices,
    generateMatrixCacheFilename,

    -- * Cross-Database Linking
    fixActivityLinksWithCrossDB,
    relinkSimpleDatabase,
    findAllCrossDBLinks,
    CrossDBLinkingStats (..),
    crossDBLinksCount,
    unresolvedCount,
    crossDBBySource,
    collectUnlinkedProductNames,

    -- * Database Analysis
    countTotalTechInputs,
    countUnlinkedExchanges,
    collectDanglingProductNames,
    collectStagedDanglingProductNames,

    -- * Supplier-gap report
    GapReason (..),
    GapEdge (..),
    GapConsumer (..),
    GapEntry (..),
    GapReport (..),
    gapReportForLoaded,
    gapReportForStaged,

    -- * Internal Linking
    fixSimaProActivityLinks,
    fixEcoSpold1ActivityLinks,

    -- * Reporting
    reportCrossDBLinkingStats,

    -- * Internal (exposed for testing)
    normalizeText,
    mergeTechFlows,
    mergeBioFlows,
    generateActivityUUIDFromActivity,
    datasetUUIDFromPath,
    getReferenceProductUUID,
    indexActivities,
    UnlinkedSummary (..),
    LocationOverride (..),
    ecoSpold1LinkContext,
    fixAllActivities,
    buildSupplierIndex,
    buildSupplierIndexByName,
    fixExchangeLinkByName,
) where

import qualified BrightwayExcel.Parser as BrightwayExcel
import qualified Codec.Compression.Zstd as Zstd
import Control.Applicative ((<|>))
import Control.Concurrent.Async
import Control.DeepSeq (force)
import Control.Exception (SomeException, catch, evaluate)
import Control.Monad
import Data.Bits (xor)
import qualified Data.ByteString as BS
import Data.Char (toLower)
import Data.Either (lefts, partitionEithers, rights)
import Data.List (intercalate, sort, sortBy, sortOn)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M

-- The flow tables are merged through the strict API: one substance now appears
-- in many datasets, so every merge that used to be a no-op (keys were unique
-- per dataset) is real, and the lazy API would stack one unforced merge per
-- occurrence, holding every superseded record until something forced the chain.
import qualified Data.Map.Strict as MS
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import Data.Ord (Down (..))
import Data.Proxy (Proxy (..))
import qualified Data.Set as S
import Data.Store (decodeEx, encode)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Data.Typeable (typeRep, typeRepFingerprint)
import qualified Data.UUID as UUID
import qualified Data.UUID.V5 as UUID5
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word64)
import Database.Allocation (
    Allocating (..),
    PropertyRefusal,
    allocate,
    allocateAll,
    describePropertyRefusal,
    propertyKeyRefusals,
 )
import Database.CrossLinking (
    AliasMap,
    CrossDBLinkResult (..),
    IndexedDatabase (..),
    LinkWarning (..),
    LinkingContext (..),
    SupplierEntry (..),
    WasteTreatmentMatch (..),
    defaultLinkingThreshold,
    emptyAliasMap,
    extractBracketedLocation,
    findSupplierAcrossDatabases,
    findSupplierByActivityProduct,
    findWasteTreatmentAcrossDatabases,
    findWasteTreatmentByActivity,
    locationHierarchy,
    normalizeUnicode,
 )
import Database.MatrixBuild (findProducer)
import Database.Upload (listDirectoryRecursive)
import EcoSpold.Common (distributeFiles)
import EcoSpold.Parser1 (streamParseActivityAndFlowsFromFile1, streamParseAllDatasetsFromFile1)
import EcoSpold.Parser2 (streamParseActivityAndFlowsFromFile)
import GHC.Conc (getNumCapabilities)
import GHC.Fingerprint (Fingerprint (..))
import qualified ILCD.Parser as ILCD
import Method.Types (Location)
import Progress
import qualified SimaPro.Parser as SimaPro
import SynonymDB (SynonymDB)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeBaseName, takeDirectory, takeExtension, (</>))
import Text.Printf (printf)
import Types
import qualified UnitConversion as UC

-- | Magic bytes to identify VoLCA cache files
cacheMagic :: BS.ByteString
cacheMagic = "VOLCACHE"

{- | Merge two technosphere flows with the same UUID, combining their synonyms
and keeping whichever of the two declares a CAS number.
When multiple .spold files reference the same flow each may carry different
synonyms; M.fromListWith mergeTechFlows ensures no synonym is lost.

The CAS is kept the same way because a file that declares it and a file that
omits it describe the same substance, and which one lands first is an accident
of how the loader distributed the files over its workers. Of the export
measured here, 430 flows are declared with a CAS in some datasets and without
in others; losing it there would silently disable the CAS rung of the
characterization cascade for the whole database.
-}
mergeTechFlows :: TechnosphereFlow -> TechnosphereFlow -> TechnosphereFlow
mergeTechFlows a b =
    a
        { tfSynonyms = M.unionWith S.union (tfSynonyms a) (tfSynonyms b)
        , tfCAS = tfCAS a <|> tfCAS b
        }

-- | Biosphere counterpart of 'mergeTechFlows'.
mergeBioFlows :: BiosphereFlow -> BiosphereFlow -> BiosphereFlow
mergeBioFlows a b =
    a
        { bfSynonyms = M.unionWith S.union (bfSynonyms a) (bfSynonyms b)
        , bfCAS = bfCAS a <|> bfCAS b
        }

-- | 9-element unzip helper (Data.List ships 7-tuple as max).
unzip9 :: [(a, b, c, d, e, f, g, h, i)] -> ([a], [b], [c], [d], [e], [f], [g], [h], [i])
unzip9 = foldr step ([], [], [], [], [], [], [], [], [])
  where
    step (a, b, c, d, e, f, g, h, i) (as, bs, cs, ds, es, fs, gs, hs, is) =
        (a : as, b : bs, c : cs, d : ds, e : es, f : fs, g : gs, h : hs, i : is)

{- |
Schema signature of the cache payload.

The 'Typeable' fingerprint names the 'Database' type (package, module, name)
and nothing more: it does not move when a field is added or a nested type
changes. Every change to what the cache holds, layout or meaning, therefore
needs a bump of the trailing constant, so existing caches are rebuilt on the
next load instead of being misread or silently returning stale numbers.

History of manual bumps:
- 5: reference-product amounts normalized to canonical base unit at ingest
     (SimaPro CSV parser); matrices built before this bump divided by the
     raw amount regardless of unit, so e.g. a 1-ton reference yielded
     impacts 1000× too large.
- 6: SimaPro CSV parser now extracts location from the older "name//[XX]"
     pattern (ecoinvent 3.9.1 export). Caches built before this bump have
     empty activityLocation for every activity in such databases, which
     breaks geography-aware supplier lookups.
- 7: SimaPro multi-product processes now share one activityUUID across
     coproducts (activityName derived from "Process name" field, not from
     the product name). Activity record gained activityAllocationPercent
     and activityAllocationFormula. Old caches have stale per-product
     UUIDs and miss the allocation fields entirely.
- 8: EcoSpold1 biosphere-flow UUID now includes the subCategory, so an
     emission to two subcompartments (e.g. river + groundwater, long-term)
     no longer collapses to one row scored at a single arbitrary
     subcompartment's CF. Old caches merged those amounts under one flow.
- 9: LinkBlocker gained AliasTargetMissing (geo-aware relink mapping), which
     changes the Store layout of the linking stats embedded in the cache; a
     downgrade reading a newer cache would fail mid-decode, so both directions
     rebuild once instead.
- 10: Activity record gained activityFormulaCheck (mathematicalRelation
     consistency outcome, surfaced by the database quality report). Old
     caches miss the field and would fail mid-decode.
- 11: SimaPro flow CAS now backfilled from the export's own substance
     registry at parse time — a value change with no type change, so the
     fingerprint alone would accept old caches. Caches built before the
     backfill keep every SimaPro biosphere flow CAS-less, and the method
     CAS bridge silently never fires on them.
- 12: EcoSpold1 activity UUID now taken from the dataset file's own name
     when that name carries one, instead of always being minted from
     name and location. Old caches key the same dataset under the minted
     UUID, so a mixed pair would compare as two disjoint databases.
- 13: Activity record gained activityLocationSource (declared, read off the
     dataset name, or neither). Old caches miss the field; the Store layout
     is positional, so decoding them would misread every field after it.
- 14: EcoSpold1 flow UUID no longer carries the dataset a flow was read from,
     so one substance is one flow across the export. Old caches hold one flow
     per (dataset, substance) pair — a value change with no type change, which
     the fingerprint alone would accept.
- 15: Activity record gained activityDocumentation (the provenance a dataset
     states about itself: published source, technology, review). Old caches
     miss the field; the Store layout is positional, so decoding them would
     misread every field after it.
- 16: a dimension's reference unit is now its shortest spelling at factor 1.0,
     and the energy column of the unit table is scaled so that MJ carries it.
     A reference product ingested from SimaPro or Brightway Excel is therefore
     recorded as 3.6 mj where it used to be 3.6e6 j, which divides the
     activity's normalization factor by a million, and a volume or count
     reference is recorded under m3 / p rather than cubic meter /
     dimensionless, which changes the product flow's UUID (it is derived from
     the unit name) and so the activity's process id. Value changes with no
     type change, which the fingerprint alone would accept.
- 17: the flow index now lists the rows that use a flow, not their activity
     UUIDs, and the product and activity indexes list every row a flow or an
     activity was written as instead of one. The fingerprint hashes the
     identity of Database, never the types inside it, so an old cache would
     pass the check and be decoded reading 16-byte UUIDs as 4-byte row
     numbers, or one row number as a list of them.
- 18: a dataset read from SimaPro or Brightway Excel is identified by the
     identifier its file publishes, a flow by its name folded in case and its
     compartment with no unit, and every row is recorded in the reference unit
     of its dimension. Process ids, flow ids and amounts all move, and none of
     it changes a type, so an old cache would pass the fingerprint and answer
     with identifiers no request would name again.
- 19: an avoided product has its own role, a product row carries its declared
     share and its category, and every activity passes the allocation gate
     before the matrix. The cached exchanges of an allocated database say
     less than the loader now reads, so they are read again.
- 20: the payload records the unit table and location aliases the database
     was built with ('dbBuiltWith'), compared before a cache is trusted. Old
     caches end before the field.
- 21: a technosphere line carries the physical properties its source states
     ('techProperties'), the material an allocation key other than the declared
     one is computed from. Cached exchanges end before the field.
- 22: the payload records the allocation key the database was divided under
     ('dbBuiltWith'), so a cache of one key is not served to a load asking for
     another. Old caches end before the field.
- 23: the index of a block's products is gone, the activity UUID index being
     the same map: the payload has one field fewer, and every field after it
     would be read at the wrong offset.
- 24: an EcoSpold 1 activity now carries the number its dataset is published
     under, in a field that already existed and was already stored. Nothing
     changes type, so a cache written just before this would pass the
     fingerprint and go on reporting that those datasets have no identifier.
- 25: an EcoSpold 2 elementary exchange in the "inventory indicator"
     compartment is a biosphere flow again, not a waste one. Nothing changes
     type, so a cache written just before this would pass the fingerprint and
     go on holding those flows on the waste axis - invisible to every method,
     and demanding a supplier no activity can be.
- 26: a SimaPro "Final waste flows" row is an elementary flow of medium
     "waste", not a waste exchange. Nothing changes type, so a cache written
     just before this would pass the fingerprint and keep those flows on an
     axis no method reads.
- 27: the same row read from an EcoSpold 1 file is an elementary flow too, and
     an export writes it as one. Nothing changes type, so a cache written just
     before this would pass the fingerprint and keep those flows on the waste
     axis, still counted as demands the gap report can never close.

The signature is stored inside the cache file and checked on load.
If it doesn't match, the cache is automatically invalidated and rebuilt.
-}
schemaSignature :: Word64
schemaSignature =
    let Fingerprint hi lo = typeRepFingerprint (typeRep (Proxy :: Proxy Database))
     in hi `xor` lo `xor` 27

{- |
Helper function to parse UUID from Text with deterministic UUID generation fallback.
Uses the same namespace as Parser.hs to ensure consistency.
-}
testDataNamespace :: UUID.UUID
testDataNamespace = UUID5.generateNamed UUID5.namespaceURL (BS.unpack $ T.encodeUtf8 "acvengine.test")

parseUUID :: T.Text -> UUID.UUID
parseUUID txt = fromMaybe (UUID5.generateNamed testDataNamespace (BS.unpack $ T.encodeUtf8 txt)) (UUID.fromText txt)

-- | Namespace for EcoSpold1 UUID generation
ecospold1Namespace :: UUID.UUID
ecospold1Namespace = UUID5.generateNamed UUID5.namespaceURL (BS.unpack $ T.encodeUtf8 "ecospold1.ecoinvent.org")

-- | Generate activity UUID from activity name and location (for EcoSpold1)
generateActivityUUIDFromActivity :: Activity -> UUID.UUID
generateActivityUUIDFromActivity act =
    let key = activityName act <> ":" <> activityLocation act
     in UUID5.generateNamed ecospold1Namespace (BS.unpack $ T.encodeUtf8 key)

{- | The identifier an EcoSpold1 dataset publishes in its own file name,
@process_<uuid>.xml@ or plain @<uuid>.xml@.

Publishers that keep this identifier stable across releases (it survives a
rename, which a name-derived UUID does not) let two versions of a database be
compared dataset by dataset. Files named any other way — ecoinvent's EcoSpold1
exports are numbered, not identified — yield 'Nothing' and keep the minted
UUID.
-}
datasetUUIDFromPath :: FilePath -> Maybe UUID.UUID
datasetUUIDFromPath path =
    let base = T.pack (takeBaseName path)
     in UUID.fromText (fromMaybe base (T.stripPrefix "process_" base))

-- | Get reference product UUID from activity exchanges
getReferenceProductUUID :: Activity -> UUID.UUID
getReferenceProductUUID act =
    case filter exchangeIsReference (exchanges act) of
        (ref : _) -> exchangeFlowId ref
        [] -> UUID.nil -- No reference product found

{- | Key the activities of a name-linked file by @(activityUUID, productUUID)@,
naming every key two blocks claimed.

A block with no published identifier is named by its name folded in case and
its location, so two blocks whose names differ only in case claim one key, and
a map keeps one activity: the last read, as 'M.fromList' always did. Keeping it
in silence is what is not acceptable, since the other block's inventory goes
with it, so each contested key is described for the caller to report.
-}
indexActivities :: [Activity] -> (ActivityMap, [T.Text])
indexActivities activities =
    (M.map NE.head grouped, mapMaybe contested (M.elems grouped))
  where
    grouped :: M.Map (UUID.UUID, UUID.UUID) (NE.NonEmpty Activity)
    grouped =
        M.fromListWith
            (<>)
            [ ((SimaPro.generateActivityUUID act, getReferenceProductUUID act), pure act)
            | act <- activities
            ]
    contested :: NE.NonEmpty Activity -> Maybe T.Text
    contested group = describe (NE.head group) . NE.toList <$> snd (NE.uncons group)
    describe :: Activity -> [Activity] -> T.Text
    describe kept dropped =
        "'"
            <> activityName kept
            <> "' and "
            <> T.intercalate ", " ["'" <> activityName act <> "'" | act <- dropped]
            <> " are one activity at '"
            <> activityLocation kept
            <> "': they differ only in case, no process identifier tells them apart, and only the last read keeps its inventory"

-- | Type alias for supplier lookup index (with location)
type SupplierIndex = M.Map (T.Text, T.Text) (UUID.UUID, UUID.UUID)

{- | Type alias for name-only supplier lookup (for SimaPro)
Maps normalizedProductName → (activityUUID, productUUID, referenceProductUnit).
The reference-product unit lets the linker reject a candidate whose unit is
dimensionally incompatible with the consumer exchange (which the matrix builder
could not convert), instead of forming a link that aborts the whole load.
-}
type NameOnlyIndex = M.Map T.Text (UUID.UUID, UUID.UUID, T.Text)

{- | Name-only supplier lookup for EcoSpold1, mapping a normalized product name
to every dataset producing it as @(activityUUID, productUUID, location)@.

Several is the ordinary shape here, not the exception: an EcoSpold1 product name
carries no location, so one name covers every geography the product is made in.
That is why the value is a 'NE.NonEmpty' and why both readers refuse a name that
covers more than one dataset instead of taking whichever it finds.
-}
type SupplierByNameWithLocation = M.Map T.Text (NE.NonEmpty (UUID.UUID, UUID.UUID, T.Text))

-- | Dataset number → (activityUUID, productUUID) for EcoSpold1 Tier 1 linking
type DatasetNumberIndex = M.Map Int (UUID.UUID, UUID.UUID)

-- | Information about an unlinked technosphere exchange
data UnlinkedExchange = UnlinkedExchange
    { ueFlowName :: !T.Text
    , ueLocation :: !T.Text
    }
    deriving (Eq, Ord, Show)

{- | An EcoSpold1 input whose dataset number and declared location name two
different datasets of the product. The number is what the file links, so it
wins; this is the trace that the file contradicted itself.
-}
data LocationOverride = LocationOverride
    { loConsumer :: !T.Text
    , loConsumerLocation :: !T.Text
    , loFlowName :: !T.Text
    , loDeclared :: !T.Text -- the location the exchange declares
    , loLinked :: !T.Text -- the location of the dataset its number names
    , loDatasetNumber :: !Int
    }
    deriving (Eq, Ord, Show)

{- | Summary of unlinked exchanges grouped by consumer activity, and of the
linked ones worth a word.
'Monoid' is hand-written: bare 'Int' has no canonical instance, and using
'Sum Int' would force every reader to unwrap.
-}
data UnlinkedSummary = UnlinkedSummary
    { usActivities :: !(M.Map T.Text [UnlinkedExchange]) -- consumer name → list of unlinked exchanges
    , usTotalLinks :: !Int
    , usFoundLinks :: !Int
    , usMissingLinks :: !Int
    , usLocationOverrides :: ![LocationOverride]
    }
    deriving (Show)

instance Semigroup UnlinkedSummary where
    UnlinkedSummary a1 t1 f1 m1 o1 <> UnlinkedSummary a2 t2 f2 m2 o2 =
        UnlinkedSummary (M.unionWith (++) a1 a2) (t1 + t2) (f1 + f2) (m1 + m2) (o1 ++ o2)

instance Monoid UnlinkedSummary where
    mempty = UnlinkedSummary M.empty 0 0 0 []

-- | Report grouped summary of unlinked exchanges
reportUnlinkedSummary :: UnlinkedSummary -> IO ()
reportUnlinkedSummary summary
    | M.null (usActivities summary) = return () -- Nothing to report
    | otherwise = do
        let activities = usActivities summary
            activityCount = M.size activities
            -- Sort activities by number of unlinked exchanges (descending)
            sortedActivities = take 10 $ reverse $ sortOn' (length . snd) $ M.toList activities
            remainingCount = activityCount - length sortedActivities

        reportProgress Warning $
            printf "Unlinked activities: %d activities affected" activityCount

        -- Report top activities with their missing suppliers
        forM_ sortedActivities $ \(actName, unlinkedExchanges) -> do
            let uniqueExchanges = nub unlinkedExchanges -- Remove duplicates
                flowCount = length uniqueExchanges
                topFlows = take 3 uniqueExchanges
                remainingFlows = flowCount - length topFlows
            reportProgress Warning $
                printf "  - %s: %d missing suppliers" (T.unpack actName) flowCount
            forM_ topFlows $ \ue ->
                if T.null (ueLocation ue)
                    then reportProgress Warning $ printf "      * %s" (T.unpack (ueFlowName ue))
                    else reportProgress Warning $ printf "      * %s [%s]" (T.unpack (ueFlowName ue)) (T.unpack (ueLocation ue))
            when (remainingFlows > 0) $
                reportProgress Warning $
                    printf "      ... and %d more" remainingFlows

        when (remainingCount > 0) $
            reportProgress Warning $
                printf "  ... and %d more activities" remainingCount
  where
    sortOn' f = sortBy (\a b -> compare (f a) (f b))
    nub = map NE.head . NE.group . sort

-- | Normalize text for matching: lowercase, strip whitespace, normalize Unicode
normalizeText :: T.Text -> T.Text
normalizeText = T.toLower . T.strip . normalizeUnicode

{- | Build supplier index: (normalizedProductName, location) → (activityUUID, productUUID)
For each activity, we index it by its reference product name + activity location
-}
buildSupplierIndex :: ActivityMap -> TechFlowDB -> SupplierIndex
buildSupplierIndex activities techFlowDb =
    M.fromList
        [ ((normalizeText (tfName flow), activityLocation act), (actUUID, prodUUID))
        | ((actUUID, prodUUID), act) <- M.toList activities
        , ex <- exchanges act
        , exchangeIsReference ex
        , Just flow <- [M.lookup (exchangeFlowId ex) techFlowDb]
        ]

{- | Build name-only supplier index for SimaPro linking, on the reference
product name and nothing else.

When several activities produce one product name they are duplicates of each
other, a block exported twice, most often because one of the two has been
retired. The file says which: a retired block is filed under an obsolete
category, and 'activityIsObsolete' reads it, so the block still in service
supplies and the retired one supplies nothing. On the Agribalyse 4.0 export of
13 May 2026 that settles all ten of its duplicated products.

Two blocks the file gives no way to tell apart are ordered by activity name
then by location, never by identifier: a change in how identity is minted must
not move a supply chain. The identifier breaks the last tie only.

The duplication is a defect in its own right, and 'Database.Quality' reports it
as one, along with an input a retired block supplies.
-}
buildSupplierIndexByName :: UnitDB -> ActivityMap -> TechFlowDB -> NameOnlyIndex
buildSupplierIndexByName unitDB activities techFlowDb =
    M.map (\candidates -> let (_, _, _, _, entry) = minimum candidates in entry) $
        M.fromListWith
            (<>)
            [ ( normalizeText (tfName flow)
              , [(activityIsObsolete act, activityName act, activityLocation act, actUUID, (actUUID, prodUUID, getUnitNameForExchange unitDB ex))]
              )
            | ((actUUID, prodUUID), act) <- M.toList activities
            , ex <- exchanges act
            , exchangeIsReference ex
            , Just flow <- [M.lookup (exchangeFlowId ex) techFlowDb]
            ]

{- | Build the name-only supplier index for EcoSpold1 linking, keeping every
dataset a name covers rather than the last one seen.
-}
buildSupplierIndexByNameWithLocation :: ActivityMap -> TechFlowDB -> SupplierByNameWithLocation
buildSupplierIndexByNameWithLocation activities techFlowDb =
    M.fromListWith
        (flip (<>))
        [ (normalizeText (tfName flow), (actUUID, prodUUID, activityLocation act) NE.:| [])
        | ((actUUID, prodUUID), act) <- M.toList activities
        , ex <- exchanges act
        , exchangeIsReference ex
        , Just flow <- [M.lookup (exchangeFlowId ex) techFlowDb]
        ]

{- | Fix EcoSpold1 activity links by resolving supplier references.
An input's dataset number names its supplier first, checked against the
product name; (flowName, location) is the fallback when the number resolves
to nothing, and the name alone when it covers a single dataset. A number that
contradicts the declared location is reported, not overruled.
Unlinked exchanges stay unlinked so that cross-DB linking can resolve them.
Location aliases map wrongLocation → correctLocation (e.g., "ENTSO" → "ENTSO-E")
-}
fixEcoSpold1ActivityLinks :: M.Map T.Text T.Text -> DatasetNumberIndex -> M.Map UUID.UUID Int -> SimpleDatabase -> IO SimpleDatabase
fixEcoSpold1ActivityLinks locationAliases dsIndex supplierLinks db = do
    let ctx = ecoSpold1LinkContext locationAliases dsIndex supplierLinks db
        (fixedActivities, summary) = fixAllActivities ctx (sdbActivities db)
    reportProgress Info $
        printf
            "Built supplier index with %d entries for activity linking (%d location aliases, %d name-only entries, %d dataset-number entries)"
            (M.size (elcSupplierIndex ctx))
            (M.size locationAliases)
            (M.size (elcNameIndex ctx))
            (M.size dsIndex)

    reportProgress Info $
        printf
            "Activity linking: %d/%d resolved (%.1f%%), %d unresolved"
            (usFoundLinks summary)
            (usTotalLinks summary)
            (if usTotalLinks summary > 0 then 100.0 * fromIntegral (usFoundLinks summary) / fromIntegral (usTotalLinks summary) else 0.0 :: Double)
            (usMissingLinks summary)

    -- Report grouped summary of unlinked exchanges
    reportUnlinkedSummary summary
    reportLocationOverrides (usLocationOverrides summary)

    return $ db{sdbActivities = fixedActivities}

{- | One line per input whose dataset number and declared location named two
different datasets, sorted so one consumer's lines sit together.
-}
reportLocationOverrides :: [LocationOverride] -> IO ()
reportLocationOverrides [] = pure ()
reportLocationOverrides overrides = do
    reportProgress Warning $
        printf
            "Dataset number overrides the declared location on %d inputs (the number is the supplier the file links; the location is a label)"
            (length overrides)
    forM_ shown $ \o ->
        reportProgress Warning $
            printf
                "  - %s [%s]: %s declares %s, dataset %d is %s"
                (T.unpack (loConsumer o))
                (T.unpack (loConsumerLocation o))
                (T.unpack (loFlowName o))
                (T.unpack (loDeclared o))
                (loDatasetNumber o)
                (T.unpack (loLinked o))
    when (length overrides > length shown) $
        reportProgress Warning $
            printf "  ... and %d more" (length overrides - length shown)
  where
    shown :: [LocationOverride]
    shown = take 20 (sort overrides)

{- | Bundle of lookup tables threaded through EcoSpold1 activity-link resolution.
Previously these fields were passed as positional parameters through
'fixAllActivities' -> 'fixActivityExchanges' -> 'fixExchangeLink', each call
re-forwarding the same values. The record collapses the cascade to a single
argument and makes the dependencies explicit.
-}
data ExchangeLinkContext = ExchangeLinkContext
    { elcLocationAliases :: !(M.Map T.Text T.Text)
    , elcSupplierIndex :: !SupplierIndex
    , elcNameIndex :: !SupplierByNameWithLocation
    , elcDatasetIndex :: !DatasetNumberIndex
    , elcSupplierLinks :: !(M.Map UUID.UUID Int)
    , elcFlowDB :: !TechFlowDB
    , elcActivities :: !ActivityMap
    }

-- | The lookup tables 'fixAllActivities' links an EcoSpold1 database with.
ecoSpold1LinkContext :: M.Map T.Text T.Text -> DatasetNumberIndex -> M.Map UUID.UUID Int -> SimpleDatabase -> ExchangeLinkContext
ecoSpold1LinkContext locationAliases dsIndex supplierLinks db =
    ExchangeLinkContext
        { elcLocationAliases = locationAliases
        , elcSupplierIndex = buildSupplierIndex (sdbActivities db) (sdbTechFlows db)
        , -- Name-only index, with location, for exchanges missing the location attribute
          elcNameIndex = buildSupplierIndexByNameWithLocation (sdbActivities db) (sdbTechFlows db)
        , elcDatasetIndex = dsIndex
        , elcSupplierLinks = supplierLinks
        , elcFlowDB = sdbTechFlows db
        , elcActivities = sdbActivities db
        }

-- | Fix all activities and return statistics with unlinked summary
fixAllActivities :: ExchangeLinkContext -> ActivityMap -> (ActivityMap, UnlinkedSummary)
fixAllActivities ctx activities =
    let results = M.map (fixActivityExchanges ctx) activities
        summaries = map snd $ M.elems results
        combinedSummary = mconcat summaries
        fixedActivities = M.map fst results
     in (fixedActivities, combinedSummary)

-- | Fix activity exchanges and return (fixed activity, UnlinkedSummary)
fixActivityExchanges :: ExchangeLinkContext -> Activity -> (Activity, UnlinkedSummary)
fixActivityExchanges ctx act =
    let (fixedExchanges, summaries) = unzip $ map (fixExchangeLink ctx act) (exchanges act)
        combinedSummary = mconcat summaries
     in (act{exchanges = fixedExchanges}, combinedSummary)

{- | Fix a single exchange's activity link by (flowName, location) match.

Unlinked exchanges stay unlinked for cross-DB resolution.
Returns (fixed exchange, UnlinkedSummary)
-}
fixExchangeLink :: ExchangeLinkContext -> Activity -> Exchange -> (Exchange, UnlinkedSummary)
fixExchangeLink ExchangeLinkContext{..} consumer ex@TechnosphereExchange{techFlowId = fid, techRole = role, techLocation = loc}
    | role == Input || role == ReferenceInput =
        let linked overrides actUUID prodUUID = (ex{techFlowId = prodUUID, techActivityLinkId = actUUID}, UnlinkedSummary M.empty 1 1 0 overrides)
            unlinked flow lookupLoc =
                let ue = UnlinkedExchange (tfName flow) lookupLoc
                 in (ex, UnlinkedSummary (M.singleton (activityName consumer) [ue]) 1 0 1 [])
         in case M.lookup fid elcFlowDB of
                Just flow ->
                    -- Tier 1: dataset-number lookup with name validation
                    case M.lookup fid elcSupplierLinks >>= \dsNum -> (,) dsNum <$> M.lookup dsNum elcDatasetIndex of
                        Just (dsNum, (actUUID, prodUUID))
                            | Just supplierFlow <- M.lookup prodUUID elcFlowDB
                            , normalizeText (tfName supplierFlow) == normalizeText (tfName flow) ->
                                linked (locationOverride flow dsNum (actUUID, prodUUID)) actUUID prodUUID
                        _ ->
                            -- Tier 2: name + location lookup
                            let soleSupplier = M.lookup (normalizeText (tfName flow)) elcNameIndex >>= sole
                                lookupLoc
                                    | T.null declaredLoc = maybe declaredLoc (\(_, _, actLoc) -> actLoc) soleSupplier
                                    | otherwise = declaredLoc
                                key = (normalizeText (tfName flow), lookupLoc)
                             in case M.lookup key elcSupplierIndex of
                                    Just (actUUID, prodUUID) -> linked [] actUUID prodUUID
                                    Nothing ->
                                        -- Tier 3: the name alone, and only when it
                                        -- covers a single dataset. A name shared by
                                        -- several geographies names none of them.
                                        case soleSupplier of
                                            Just (actUUID, prodUUID, _) -> linked [] actUUID prodUUID
                                            Nothing -> unlinked flow lookupLoc
                Nothing ->
                    (ex, UnlinkedSummary M.empty 1 0 1 [])
    | otherwise = (ex, mempty)
  where
    declaredLoc :: T.Text
    declaredLoc = fromMaybe loc (M.lookup loc elcLocationAliases)

    -- The number named a dataset. When the declared location names another
    -- dataset of the same product, the number still wins: it is what the file
    -- links, and the results a database publishes follow it. The override is
    -- recorded so a reader can see the file contradict itself.
    locationOverride :: TechnosphereFlow -> Int -> (UUID.UUID, UUID.UUID) -> [LocationOverride]
    locationOverride flow dsNum supplierKey =
        [ LocationOverride
            { loConsumer = activityName consumer
            , loConsumerLocation = activityLocation consumer
            , loFlowName = tfName flow
            , loDeclared = loc
            , loLinked = activityLocation supplier
            , loDatasetNumber = dsNum
            }
        | not (T.null declaredLoc)
        , Just supplier <- [M.lookup supplierKey elcActivities]
        , activityLocation supplier /= declaredLoc
        , M.member (normalizeText (tfName flow), declaredLoc) elcSupplierIndex
        ]
fixExchangeLink _ _ ex@BiosphereExchange{} = (ex, mempty)
-- A WasteExchange in input direction (consumed by treatment) would benefit
-- from the same supplier-lookup logic as a technosphere Input, but at this
-- stage we leave waste links to the cross-DB linker (see CrossLinking) and
-- the downstream parsers. Pure pass-through here.
fixExchangeLink _ _ ex@WasteExchange{} = (ex, mempty)

{- |
Load all EcoSpold files with optimized parallel processing and deduplication.

This function implements a high-performance loading strategy:
1. **Chunked Processing**: Split files into optimal chunks (500 files/chunk)
2. **Controlled Parallelism**: Limit concurrent file handles (4 max)
3. **Memory Management**: Process chunks sequentially to control memory usage
4. **Deduplication**: Automatic flow and unit deduplication across files

Performance characteristics:
- Memory usage: ~2-4GB peak for Ecoinvent 3.8
- Processing time: ~45s for 18K activities (cold start)
- Parallelism: 4x concurrent file parsing within chunks
- Chunk size: 500 files (optimal for memory vs parallelism trade-off)

Used when no cache exists or caching is disabled.
-}
loadDatabase :: UC.UnitConfig -> FilePath -> IO (Either T.Text SimpleDatabase)
loadDatabase unitConfig = loadDatabaseWithLocationAliases (defaultLoadOptions unitConfig)

{- | Load all EcoSpold files with location aliases
Location aliases map wrongLocation → correctLocation (e.g., "ENTSO" → "ENTSO-E")

The 'UnitConfig' is passed down to parsers so reference-product amounts can be
normalized to the canonical base unit of their dimension at ingest time.
-}
loadDatabaseWithLocationAliases :: LoadOptions -> FilePath -> IO (Either T.Text SimpleDatabase)
loadDatabaseWithLocationAliases opts path = do
    loaded <- readSourceUnder opts path
    either (pure . Left) (\db -> Right db <$ reportKeyRefusals opts db) loaded

-- | The load itself, before anything is said about what the key refused.
readSourceUnder :: LoadOptions -> FilePath -> IO (Either T.Text SimpleDatabase)
readSourceUnder opts path = do
    -- Check if path is a file (SimaPro CSV) or directory (EcoSpold)
    isFile <- doesFileExist path
    isDir <- doesDirectoryExist path

    if isFile
        then case map toLower (takeExtension path) of
            ".csv" -> loadSimaProCSV opts path
            ".xml" -> loadSingleEcoSpold1File opts path
            ".xlsx" -> loadBrightwayExcel opts path
            _ -> return $ Left $ T.pack $ "Unsupported file type: " ++ path
        else
            if isDir
                then do
                    hasProcesses <- doesDirectoryExist (path </> "processes")
                    if hasProcesses
                        then ILCD.parseILCDDirectory (loUnitConfig opts) (loAllocation opts) path
                        else loadEcoSpoldDirectory opts path
                else return $ Left $ T.pack $ "Path does not exist: " ++ path

{- | What a load reads besides the files themselves.

Gathered rather than passed one by one: every format loader needs all three,
and three positional arguments of which two are maps invite a caller to swap
them.
-}
data LoadOptions = LoadOptions
    { loUnitConfig :: !UC.UnitConfig -- The merged unit table amounts are converted through
    , loLocationAliases :: !(M.Map T.Text T.Text) -- Wrong location -> correct location
    , loAllocation :: !AllocationKey -- How a multi-output block is divided
    }

{- | Say what the key this load asked for could not divide.

Loud rather than silent: such a block keeps every share its source declared
and still loses its column, and nothing else in the run says which key refused
it or why. The first ten are named, and a count says how many there were.
-}
reportKeyRefusals :: LoadOptions -> SimpleDatabase -> IO ()
reportKeyRefusals opts db = case loAllocation opts of
    Declared -> pure ()
    ByProperty prop -> unless (null refusals) $ do
        reportProgress Warning $
            show (length refusals) <> " blocks were not divided: the key names a property they do not carry"
        mapM_ (warn prop) (take 10 refusals)
  where
    refusals :: [(T.Text, PropertyRefusal)]
    refusals = propertyKeyRefusals (allocating opts (sdbUnits db)) (M.elems (sdbActivities db))

    warn :: AllocationProperty -> (T.Text, PropertyRefusal) -> IO ()
    warn prop (name, reason) =
        reportProgress Warning . T.unpack $
            name <> " has no column -- " <> describePropertyRefusal prop reason

-- | What 'allocate' reads, for a load of these options over these units.
allocating :: LoadOptions -> UnitDB -> Allocating
allocating opts unitDB =
    Allocating
        { alKey = loAllocation opts
        , alUnitConfig = loUnitConfig opts
        , alUnitDB = unitDB
        }

-- | The options a load with nothing configured runs under.
defaultLoadOptions :: UC.UnitConfig -> LoadOptions
defaultLoadOptions unitConfig =
    LoadOptions
        { loUnitConfig = unitConfig
        , loLocationAliases = M.empty
        , loAllocation = Declared
        }

-- | Everything one EcoSpold dataset parses to, before it is keyed.
type ParsedDataset = (Activity, [TechnosphereFlow], [BiosphereFlow], [WasteFlow], [Unit], Int, M.Map UUID.UUID Int)

{- | Say that an EcoSpold 2 dataset divided into several processes will come
back as one.

'buildProcEntry' reads a process's identity off the file name, which carries
one @activityUUID_productUUID@ pair whatever the file holds, so the coproducts
a key just separated all claim it and the registry keeps the last. Loud rather
than silent: the alternative is a database quietly missing the very products
the key was named to produce. Keying an EcoSpold 2 process on its own product
rather than on its file name is what would fix it.
-}
warnSplitKeyedOnFileName :: FilePath -> IO ()
warnSplitKeyedOnFileName file =
    reportProgress Warning $
        "only one product will be kept of "
            <> file
            <> ": an EcoSpold 2 process is identified by its file name, which names a single product"

{- | Allocate a parsed dataset before it is keyed: one entry per process
'allocate' splits it into, each carrying the file's flows and units, so the
entry is then keyed on its own reference product.
-}
allocateParsed :: LoadOptions -> ParsedDataset -> [ParsedDataset]
allocateParsed opts (act, techs, bios, wastes, units, dsNum, links) =
    [ (act', techs, bios, wastes, units, dsNum, links)
    | act' <- NE.toList (allocate (allocating opts (M.fromList [(unitId u, u) | u <- units])) act)
    ]

-- | Load SimaPro CSV file
loadSimaProCSV :: LoadOptions -> FilePath -> IO (Either T.Text SimpleDatabase)
loadSimaProCSV opts csvPath = do
    let unitConfig = loUnitConfig opts
    parsed <- SimaPro.parseSimaProCSV unitConfig csvPath
    case parsed of
        Left err -> return (Left err)
        Right (activities, techFlowDB, bioFlowDB, wasteFlowDB, unitDB) ->
            if null activities
                then return $ Left "No activities found in SimaPro CSV file."
                else do
                    -- Build ActivityMap with generated ProcessIds
                    -- For SimaPro: use the same UUID for both activity and product (like EcoSpold1)
                    let (procMap, collisions) = indexActivities (allocateAll (allocating opts unitDB) activities)
                    forM_ collisions $ reportProgress Warning . T.unpack

                    -- Build initial database
                    let simpleDb = SimpleDatabase procMap techFlowDB bioFlowDB wasteFlowDB unitDB

                    -- Fix activity links using supplier lookup (same as EcoSpold1)
                    Right <$> fixSimaProActivityLinks unitConfig simpleDb

{- | Load a Brightway Excel (.xlsx) inventory.

Mirrors 'loadSimaProCSV': the parser returns the same 5-tuple, activities are
keyed @(activityUUID, referenceProductUUID)@, and within-file supplier
references are resolved by the shared name-based pass. Cross-database links to a
background database (e.g. ecoinvent) are resolved later by
'fixActivityLinksWithCrossDB', exactly as for SimaPro and EcoSpold.
-}
loadBrightwayExcel :: LoadOptions -> FilePath -> IO (Either T.Text SimpleDatabase)
loadBrightwayExcel opts xlsxPath = do
    let unitConfig = loUnitConfig opts
    parsed <- BrightwayExcel.parseBrightwayExcel unitConfig xlsxPath
    case parsed of
        Left err -> return $ Left err
        Right (activities, techFlowDB, bioFlowDB, wasteFlowDB, unitDB)
            | null activities -> return $ Left "No activities found in Brightway Excel file."
            | otherwise -> do
                let (procMap, collisions) = indexActivities (allocateAll (allocating opts unitDB) activities)
                    simpleDb = SimpleDatabase procMap techFlowDB bioFlowDB wasteFlowDB unitDB
                forM_ collisions $ reportProgress Warning . T.unpack
                Right <$> fixSimaProActivityLinks unitConfig simpleDb

{- | Fix SimaPro activity links by resolving supplier references
Uses name-only matching (no location required) for SimaPro technosphere inputs
-}
fixSimaProActivityLinks :: UC.UnitConfig -> SimpleDatabase -> IO SimpleDatabase
fixSimaProActivityLinks unitConfig db = do
    let nameIndex = buildSupplierIndexByName (sdbUnits db) (sdbActivities db) (sdbTechFlows db)
    reportProgress Info $ printf "Built name-only supplier index with %d entries for SimaPro linking" (M.size nameIndex)

    -- Count and report statistics
    let (fixedActivities, summary) = fixAllActivitiesByName unitConfig (sdbUnits db) nameIndex (sdbTechFlows db) (sdbActivities db)

    reportProgress Info $
        printf
            "SimaPro activity linking: %d/%d resolved (%.1f%%), %d unresolved"
            (usFoundLinks summary)
            (usTotalLinks summary)
            (if usTotalLinks summary > 0 then 100.0 * fromIntegral (usFoundLinks summary) / fromIntegral (usTotalLinks summary) else 0.0 :: Double)
            (usMissingLinks summary)

    -- Report grouped summary of unlinked exchanges
    reportUnlinkedSummary summary

    return $ db{sdbActivities = fixedActivities}

-- | Fix all activities using name-only matching
fixAllActivitiesByName :: UC.UnitConfig -> UnitDB -> NameOnlyIndex -> TechFlowDB -> ActivityMap -> (ActivityMap, UnlinkedSummary)
fixAllActivitiesByName unitConfig unitDB idx techFlowDb activities =
    let results = M.map (fixActivityExchangesByName unitConfig unitDB idx techFlowDb) activities
        summaries = map snd $ M.elems results
        combinedSummary = mconcat summaries
        fixedActivities = M.map fst results
     in (fixedActivities, combinedSummary)

-- | Fix activity exchanges using name-only matching
fixActivityExchangesByName :: UC.UnitConfig -> UnitDB -> NameOnlyIndex -> TechFlowDB -> Activity -> (Activity, UnlinkedSummary)
fixActivityExchangesByName unitConfig unitDB idx techFlowDb act =
    let (fixedExchanges, summaries) = unzip $ map (fixExchangeLinkByName unitConfig unitDB idx techFlowDb (activityName act)) (exchanges act)
        combinedSummary = mconcat summaries
     in (act{exchanges = fixedExchanges}, combinedSummary)

{- | A name-based supplier link is admissible only when the matrix builder could
later convert the consumer's exchange unit to the supplier's reference-product
unit. This mirrors the builder's own rule exactly (see 'Database.MatrixBuild'):
a conversion is needed only when the two units differ and both are non-empty,
and it must then succeed. So a link is safe when the units are identical, when
either side is empty, or when they are dimensionally compatible. Forming any
other link would abort the whole load — better to leave the input unlinked.
-}
linkUnitsCompatible :: UC.UnitConfig -> T.Text -> T.Text -> Bool
linkUnitsCompatible unitConfig consumerUnit supplierUnit =
    let cu = T.toLower (T.strip consumerUnit)
        su = T.toLower (T.strip supplierUnit)
     in cu == su
            || T.null cu
            || T.null su
            || UC.unitsCompatible unitConfig consumerUnit supplierUnit

{- | Fix a single exchange's activity link using name-only matching.
Inputs and non-reference outputs (coproducts / avoided-production credits)
are eligible for relinking. A candidate is accepted only if its
reference-product unit is dimensionally compatible with the consumer exchange
('linkUnitsCompatible'); an incompatible candidate is skipped rather than
forming a link the matrix builder cannot convert — which would otherwise abort
the whole load.

There is no second guess. An input naming a product no activity of this
database produces stays unlinked, and the cross-database linker gets its turn
on it. Returns (fixed exchange, UnlinkedSummary).
-}
fixExchangeLinkByName :: UC.UnitConfig -> UnitDB -> NameOnlyIndex -> TechFlowDB -> T.Text -> Exchange -> (Exchange, UnlinkedSummary)
fixExchangeLinkByName unitConfig unitDB idx techFlowDb consumerName ex@TechnosphereExchange{techFlowId = fid, techRole = role, techLocation = loc}
    | role == Input || role == ReferenceInput || role == AvoidedProduct =
        case M.lookup fid techFlowDb of
            Just flow ->
                let key = normalizeText (tfName flow)
                    consumerUnit = getUnitNameForExchange unitDB ex
                    relink actUUID prodUUID = ex{techFlowId = prodUUID, techActivityLinkId = actUUID}
                    -- Accept a candidate only when its reference unit can convert.
                    accept (actUUID, prodUUID, supplierUnit)
                        | linkUnitsCompatible unitConfig consumerUnit supplierUnit = Just (actUUID, prodUUID)
                        | otherwise = Nothing
                 in case M.lookup key idx >>= accept of
                        Just (actUUID, prodUUID) ->
                            (relink actUUID prodUUID, UnlinkedSummary M.empty 1 1 0 [])
                        Nothing ->
                            let unlinked = UnlinkedExchange (tfName flow) loc
                                unlinkedMap = M.singleton consumerName [unlinked]
                             in (ex, UnlinkedSummary unlinkedMap 1 0 1 [])
            Nothing ->
                -- Flow not in technosphere map — shouldn't happen but be safe
                (ex, UnlinkedSummary M.empty 1 0 1 [])
    | otherwise = (ex, mempty) -- Reference products: nothing to relink
fixExchangeLinkByName _ _ _ _ _ ex@BiosphereExchange{} = (ex, mempty)
-- Waste link resolution is deferred to the cross-DB linker path.
fixExchangeLinkByName _ _ _ _ _ ex@WasteExchange{} = (ex, mempty)

{- | Recursively collect files under @dir@ whose lowercased extension matches
@ext@. Lets an EcoSpold package load from its root even when the .spold datasets
sit in a subdirectory (e.g. ecoinvent's datasets/).
-}
findFilesByExtRecursive :: String -> FilePath -> IO [FilePath]
findFilesByExtRecursive ext =
    fmap (filter ((== ext) . map toLower . takeExtension)) . listDirectoryRecursive

-- | Load EcoSpold files from directory
loadEcoSpoldDirectory :: LoadOptions -> FilePath -> IO (Either T.Text SimpleDatabase)
loadEcoSpoldDirectory opts dir = do
    reportProgress Info "Scanning directory for EcoSpold files"
    files <- listDirectory dir
    -- .spold datasets may live in a subdirectory (e.g. ecoinvent's datasets/),
    -- so find them recursively. .xml (EcoSpold1) stays top-level to avoid
    -- sweeping up MasterData/metadata XML that sits beside the datasets.
    spold2Files <- findFilesByExtRecursive ".spold" dir
    let spold1Files = [dir </> f | f <- files, map toLower (takeExtension f) == ".xml"]

    -- Determine which format to use based on what's found
    case (spold2Files, spold1Files) of
        ([], []) -> return $ Left $ T.pack $ "No EcoSpold files found in directory: " ++ dir
        ([], [singleXml]) -> do
            -- Single XML file: likely a multi-dataset EcoSpold1 file
            reportProgress Info $ "Found single EcoSpold1 file: " ++ singleXml
            loadSingleEcoSpold1File opts singleXml
        ([], xs) -> do
            reportProgress Info $ "Found " ++ show (length xs) ++ " EcoSpold1 (.XML) files for processing"
            loadWithWorkerParallelism xs True
        (xs, []) -> do
            reportProgress Info $ "Found " ++ show (length xs) ++ " EcoSpold2 (.spold) files for processing"
            loadWithWorkerParallelism xs False
        (xs, _) -> do
            reportProgress Info $ "Found " ++ show (length xs) ++ " EcoSpold2 (.spold) files for processing"
            loadWithWorkerParallelism xs False -- Prefer EcoSpold2 if both present
  where
    locationAliases :: MS.Map T.Text T.Text
    locationAliases = loLocationAliases opts
    -- Worker-based parallelism: divide files among N workers, all process in parallel
    loadWithWorkerParallelism :: [FilePath] -> Bool -> IO (Either T.Text SimpleDatabase)
    loadWithWorkerParallelism allFiles isEcoSpold1 = do
        -- Get actual number of CPU capabilities (respects +RTS -N)
        numWorkers <- getNumCapabilities
        let workers = distributeFiles numWorkers allFiles
        reportProgress Info $
            printf
                "Processing %d files with %d parallel workers (%d files per worker)"
                (length allFiles)
                numWorkers
                (length allFiles `div` numWorkers)

        -- Process all workers in parallel
        startTime <- getCurrentTime
        scoped <- inheritLogScope
        results <- mapConcurrently (scoped . processWorker startTime isEcoSpold1) (zip [1 ..] workers)

        -- Check for errors from any worker
        let errors = lefts results
        case errors of
            (firstErr : _) -> return $ Left firstErr
            [] -> do
                let successResults = rights results
                let (procMaps, techFlowMaps, bioFlowMaps, wasteFlowMaps, unitMaps, rawFlowCounts, rawUnitCounts, dsIndexes, supplierLinksLists) = unzip9 successResults
                let !finalProcMap = M.unions procMaps
                let !finalTechFlowMap = MS.unionsWith mergeTechFlows techFlowMaps
                let !finalBioFlowMap = MS.unionsWith mergeBioFlows bioFlowMaps
                let !finalWasteFlowMap = M.unions wasteFlowMaps
                let !finalUnitMap = M.unions unitMaps
                let !finalDsIndex = M.unions dsIndexes
                let !finalSupplierLinks = M.unions supplierLinksLists

                endTime <- getCurrentTime
                let totalDuration = realToFrac $ diffUTCTime endTime startTime
                let totalFiles = length allFiles
                let avgFilesPerSec = fromIntegral totalFiles / totalDuration
                let totalRawFlows = sum rawFlowCounts
                let totalRawUnits = sum rawUnitCounts
                let totalFlows = M.size finalTechFlowMap + M.size finalBioFlowMap
                let flowDeduplication = if totalRawFlows > 0 then 100.0 * (1.0 - fromIntegral totalFlows / fromIntegral totalRawFlows) else 0.0 :: Double
                let unitDeduplication = if totalRawUnits > 0 then 100.0 * (1.0 - fromIntegral (M.size finalUnitMap) / fromIntegral totalRawUnits) else 0.0 :: Double

                reportProgress Info $ printf "Parsing completed (%s, %.1f files/sec):" (formatDuration totalDuration) avgFilesPerSec
                reportProgress Info $ printf "  Activities: %d processes" (M.size finalProcMap)
                reportProgress Info $
                    printf
                        "  Flows: %d tech + %d bio (%.1f%% deduplication from %d raw)"
                        (M.size finalTechFlowMap)
                        (M.size finalBioFlowMap)
                        flowDeduplication
                        totalRawFlows
                reportProgress Info $
                    printf
                        "  Units: %d unique (%.1f%% deduplication from %d raw)"
                        (M.size finalUnitMap)
                        unitDeduplication
                        totalRawUnits
                reportMemoryUsage "Final parsing memory usage"

                -- For EcoSpold1: fix activity links using supplier lookup table
                let simpleDb = SimpleDatabase finalProcMap finalTechFlowMap finalBioFlowMap finalWasteFlowMap finalUnitMap
                if isEcoSpold1
                    then Right <$> fixEcoSpold1ActivityLinks locationAliases finalDsIndex finalSupplierLinks simpleDb
                    else return $ Right simpleDb

    -- Process one worker's share of files
    processWorker :: UTCTime -> Bool -> (Int, [FilePath]) -> IO (Either T.Text (ActivityMap, TechFlowDB, BioFlowDB, WasteFlowDB, UnitDB, Int, Int, DatasetNumberIndex, M.Map UUID.UUID Int))
    processWorker _startTime isEcoSpold1 (workerNum, workerFiles) = do
        workerStartTime <- getCurrentTime
        reportProgress Info $ printf "Worker %d started: processing %d files" workerNum (length workerFiles)

        -- Parse all files for this worker using appropriate parser.
        -- Both paths return (Activity, [TechnosphereFlow], [BiosphereFlow], [WasteFlow], [Unit], Int, M.Map UUID Int).
        -- For EcoSpold2: dataset number = 0, supplier links = empty. WasteFlows now flow
        -- through (intermediateExchange classified By-product:Waste).
        let parseFile =
                if isEcoSpold1
                    then streamParseActivityAndFlowsFromFile1
                    else fmap (fmap (\(a, ts, bs, ws, us) -> (a, ts, bs, ws, us, 0, M.empty))) . streamParseActivityAndFlowsFromFile
        workerResults <- mapM parseFile workerFiles
        let paired = zipWith (\f r -> fmap (f,) r) workerFiles workerResults
        let (errs, oks) = partitionEithers paired
        forM_ errs $ \e ->
            reportProgress Warning e
        let split = [(f, allocateParsed opts r) | (f, r) <- oks]
            (okFiles, okResults) = unzip [(f, r') | (f, rs) <- split, r' <- rs]
        unless isEcoSpold1 $
            mapM_ (warnSplitKeyedOnFileName . fst) (filter ((> 1) . length . snd) split)
        let procs = [a | (a, _, _, _, _, _, _) <- okResults]
            techLists = [ts | (_, ts, _, _, _, _, _) <- okResults]
            bioLists = [bs | (_, _, bs, _, _, _, _) <- okResults]
            wasteLists = [ws | (_, _, _, ws, _, _, _) <- okResults]
            unitLists = [us | (_, _, _, _, us, _, _) <- okResults]
            dsNums = [n | (_, _, _, _, _, n, _) <- okResults]
            supplierLinksList = [sl | (_, _, _, _, _, _, sl) <- okResults]
        let !allTechs = concat techLists
        let !allBios = concat bioLists
        let !allWastes = concat wasteLists
        let !allUnits = concat unitLists

        let procEntries = zipWith (buildProcEntry isEcoSpold1) okFiles procs

        case lefts procEntries of
            (firstErr : _) -> return $ Left firstErr
            [] -> do
                let !procMap = M.fromList (rights procEntries)
                let !techFlowMap = MS.fromListWith mergeTechFlows [(tfId f, f) | f <- allTechs]
                let !bioFlowMap = MS.fromListWith mergeBioFlows [(bfId f, f) | f <- allBios]
                let !wasteFlowMap = M.fromList [(wfId f, f) | f <- allWastes]
                let !unitMap = M.fromList [(unitId u, u) | u <- allUnits]
                let !dsIndex =
                        M.fromList
                            [(n, key) | (n, Right (key, _)) <- zip dsNums procEntries, n /= 0]
                let !allSupplierLinks = M.unions supplierLinksList

                workerEndTime <- getCurrentTime
                let workerDuration = realToFrac $ diffUTCTime workerEndTime workerStartTime
                let filesPerSec = fromIntegral (length workerFiles) / workerDuration
                let rawFlowCount = length allTechs + length allBios + length allWastes
                let rawUnitCount = length allUnits
                reportProgress Info $
                    printf
                        "Worker %d completed: %d activities, %d tech + %d bio + %d waste flows (%s, %.1f files/sec)"
                        workerNum
                        (M.size procMap)
                        (M.size techFlowMap)
                        (M.size bioFlowMap)
                        (M.size wasteFlowMap)
                        (formatDuration workerDuration)
                        filesPerSec

                return $ Right (procMap, techFlowMap, bioFlowMap, wasteFlowMap, unitMap, rawFlowCount, rawUnitCount, dsIndex, allSupplierLinks)

    -- Build a single process entry, returning Either for error handling
    buildProcEntry :: Bool -> FilePath -> Activity -> Either T.Text ((UUID, UUID), Activity)
    buildProcEntry True filepath activity =
        -- EcoSpold1: prefer the identifier the file itself carries, so a
        -- dataset keeps its identity across releases; mint from name and
        -- location only when the file name carries none.
        let actUUID = fromMaybe (generateActivityUUIDFromActivity activity) (datasetUUIDFromPath filepath)
            prodUUID = getReferenceProductUUID activity
         in Right ((actUUID, prodUUID), activity)
    buildProcEntry False filepath activity =
        -- EcoSpold2: Parse UUIDs from filename
        let filename = T.pack $ takeBaseName filepath
         in case T.splitOn "_" filename of
                [actUUIDText, prodUUIDText] ->
                    let actUUID = parseUUID actUUIDText
                        prodUUID = parseUUID prodUUIDText
                     in Right ((actUUID, prodUUID), activity)
                _ -> Left $ T.pack $ "Invalid filename format (expected activityUUID_productUUID.spold): " ++ filepath

{- | Load a single EcoSpold1 file containing multiple datasets
This handles files where <ecoSpold> contains multiple <dataset> elements.

A file holding exactly one dataset is keyed like the per-file directory
path: the identifier its file name carries wins over the minted UUID.
Several datasets share one file name, so none of them can claim it.
-}
loadSingleEcoSpold1File :: LoadOptions -> FilePath -> IO (Either T.Text SimpleDatabase)
loadSingleEcoSpold1File opts filepath = do
    let locationAliases = loLocationAliases opts
    reportProgress Info "Parsing multi-dataset EcoSpold1 file..."
    parsed <- streamParseAllDatasetsFromFile1 filepath
    reportProgress Info $ "Parsed " ++ show (length parsed) ++ " datasets from file"
    let results = concatMap (allocateParsed opts) parsed

    -- Build activity map from all parsed activities
    let fileUUID = case results of
            [_] -> datasetUUIDFromPath filepath
            _ -> Nothing
        expanded = map (buildProcEntryFromResult fileUUID) results
        !procMap = M.fromList expanded
        !techFlowMap = MS.fromListWith mergeTechFlows [(tfId f, f) | (_, techs, _, _, _, _, _) <- results, f <- techs]
        !bioFlowMap = MS.fromListWith mergeBioFlows [(bfId f, f) | (_, _, bios, _, _, _, _) <- results, f <- bios]
        !wasteFlowMap = M.fromList [(wfId f, f) | (_, _, _, wastes, _, _, _) <- results, f <- wastes]
        !unitMap = M.fromList [(unitId u, u) | (_, _, _, _, units, _, _) <- results, u <- units]
        !dsIndex =
            M.fromList
                [(dsNum, key) | ((_, _, _, _, _, dsNum, _), (key, _)) <- zip results expanded, dsNum /= 0]
        !supplierLinks = M.unions [sl | (_, _, _, _, _, _, sl) <- results]
        simpleDb = SimpleDatabase procMap techFlowMap bioFlowMap wasteFlowMap unitMap

    let totalTechs = sum [length techs | (_, techs, _, _, _, _, _) <- results]
    let totalBios = sum [length bios | (_, _, bios, _, _, _, _) <- results]
    let totalWastes = sum [length wastes | (_, _, _, wastes, _, _, _) <- results]
    let totalUnits = sum [length units | (_, _, _, _, units, _, _) <- results]
    reportProgress Info $ printf "  Activities: %d processes" (M.size procMap)
    reportProgress Info $ printf "  Flows: %d tech + %d bio + %d waste (from %d raw)" (M.size techFlowMap) (M.size bioFlowMap) (M.size wasteFlowMap) (totalTechs + totalBios + totalWastes)
    reportProgress Info $ printf "  Units: %d unique (from %d raw)" (M.size unitMap) totalUnits

    Right <$> fixEcoSpold1ActivityLinks locationAliases dsIndex supplierLinks simpleDb
  where
    buildProcEntryFromResult :: Maybe UUID.UUID -> (Activity, [TechnosphereFlow], [BiosphereFlow], [WasteFlow], [Unit], Int, M.Map UUID.UUID Int) -> ((UUID.UUID, UUID.UUID), Activity)
    buildProcEntryFromResult fileUUID (activity, _, _, _, _, _, _) =
        let actUUID = fromMaybe (generateActivityUUIDFromActivity activity) fileUUID
            prodUUID = getReferenceProductUUID activity
         in ((actUUID, prodUUID), activity)

{- |
Generate filename for matrix cache.

Matrix caches store pre-computed sparse matrices (technosphere A,
biosphere B) enabling direct LCA solving without matrix construction.

The cache lives next to the configured source path
(@takeDirectory sourcePath@). For uploaded databases this is the
upload directory; for preloaded/host-mounted databases it is the
mount directory. Either way the cache persists across restarts as
long as the source location does.

Cache invalidation is handled by a schema signature stored inside
the cache file, not by the filename.
-}
generateMatrixCacheFilename :: T.Text -> FilePath -> IO FilePath
generateMatrixCacheFilename dbName sourcePath = do
    let cacheFilename = "volca.cache." ++ T.unpack dbName ++ ".bin"
        cacheDir = takeDirectory sourcePath
    createDirectoryIfMissing True cacheDir
    return $ cacheDir </> cacheFilename

{- |
Load Database with pre-computed matrices from cache (second-tier).

This is the fastest loading method (~0.5s) as it bypasses both
XML parsing and matrix construction. The Database includes:
- All activities, flows, units (from SimpleDatabase)
- Pre-built indexes for fast querying
- Pre-computed sparse matrices (technosphere A, biosphere B)
- Activity and flow UUID mappings for matrix operations

Returns Nothing if no matrix cache exists, or if the one found was built with
other inputs than the ones in force: what it holds would not be what a fresh
read produces.
-}
loadCachedDatabaseWithMatrices :: T.Text -> FilePath -> BuildInputs -> IO (Maybe Database)
loadCachedDatabaseWithMatrices dbName dataDir inputs = do
    cacheFile <- generateMatrixCacheFilename dbName dataDir
    let zstdFile = cacheFile ++ ".zst"
    zstdExists <- doesFileExist zstdFile
    if not zstdExists
        then do
            reportCacheOperation "No matrix cache found"
            return Nothing
        else do
            -- Delegate to the shared reader; a Nothing here means the cache
            -- is corrupted or was written by another schema, and the database
            -- is rebuilt from source, as it is when the cache was built under
            -- other inputs. The file is left alone: a rebuild overwrites it
            -- anyway, and a host that ships only the cache (see
            -- 'Manager.loadDatabaseRawWithCrossDB') has no source to rebuild
            -- from, so deleting it there destroyed the only copy of the data.
            result <- loadCompressedCacheFile zstdFile
            case result of
                Just db | dbBuiltWith db == inputs -> return (Just db)
                Just db -> do
                    reportCacheOperation $ "Cache was built with another " ++ builtWithDifference (dbBuiltWith db) inputs
                    rebuild
                Nothing -> rebuild
  where
    rebuild :: IO (Maybe Database)
    rebuild = Nothing <$ reportCacheOperation "Will rebuild database from source files"

-- | Which of the build inputs a cache disagrees with, for the log line.
builtWithDifference :: BuildInputs -> BuildInputs -> String
builtWithDifference cached current =
    intercalate " and " $
        ["unit table" | biUnitConfig cached /= biUnitConfig current]
            ++ ["location aliases" | biLocationAliases cached /= biLocationAliases current]
            ++ ["allocation key" | biAllocation cached /= biAllocation current]

-- | Load compressed (.bin.zst) cache file with header validation
loadCompressedCacheFile :: FilePath -> IO (Maybe Database)
loadCompressedCacheFile zstdFile = do
    reportCacheInfo zstdFile
    catch
        ( withProgressTiming Cache "Matrix cache load with zstd decompression" $ do
            contents <- BS.readFile zstdFile
            -- Check minimum size for header (16 bytes)
            if BS.length contents < 16
                then do
                    reportCacheOperation "Cache file too small (missing header)"
                    return Nothing
                else do
                    let (header, compressed) = BS.splitAt 16 contents
                        (magic, sigBytes) = BS.splitAt 8 header
                    -- Check magic bytes
                    if magic /= cacheMagic
                        then do
                            reportCacheOperation "Invalid cache file (wrong magic bytes)"
                            return Nothing
                        else do
                            -- Check schema signature
                            let storedSig = decodeEx sigBytes :: Word64
                            if storedSig /= schemaSignature
                                then do
                                    reportCacheOperation $ "Schema mismatch: cache=" ++ show storedSig ++ " current=" ++ show schemaSignature
                                    reportCacheOperation "Cache will be rebuilt with new schema"
                                    return Nothing
                                else do
                                    -- Decompress and decode the payload
                                    case Zstd.decompress compressed of
                                        Zstd.Skip -> do
                                            reportError "Zstd decompression failed: Skip"
                                            return Nothing
                                        Zstd.Error err -> do
                                            reportError $ "Zstd decompression failed: " ++ show err
                                            return Nothing
                                        Zstd.Decompress decompressed -> do
                                            let !db = decodeEx decompressed
                                            -- Force full evaluation to prevent lazy thunk buildup
                                            db' <- evaluate (force db)
                                            reportCacheOperation $
                                                "Matrix cache loaded: "
                                                    ++ show (dbActivityCount db')
                                                    ++ " activities, "
                                                    ++ show (VU.length $ dbTechnosphereTriples db')
                                                    ++ " tech entries, "
                                                    ++ show (VU.length $ dbBiosphereTriples db')
                                                    ++ " bio entries (decompressed)"
                                            return (Just db')
        )
        ( \(e :: SomeException) -> do
            reportError $ "Compressed cache load failed: " ++ show e
            reportCacheOperation "The compressed cache file is corrupted or incompatible"
            return Nothing
        )

{- |
Save Database with pre-computed matrices to cache.

Serializes the complete Database including sparse matrices to enable
ultra-fast startup (~0.5s load time). The cache file includes:
- 8 bytes magic ("VOLCACHE")
- 8 bytes schema signature (auto-generated from type structure)
- Zstd compressed Database binary

Should be called after matrix construction is complete.
-}
saveCachedDatabaseWithMatrices :: T.Text -> FilePath -> Database -> IO ()
saveCachedDatabaseWithMatrices dbName dataDir db = do
    cacheFile <- generateMatrixCacheFilename dbName dataDir
    let zstdFile = cacheFile ++ ".zst"
    reportCacheOperation $ "Saving Database with matrices to compressed cache: " ++ zstdFile
    withProgressTiming Cache "Matrix cache save with zstd compression" $ do
        -- Serialize to ByteString (store returns strict ByteString)
        let serialized = encode db
        -- Compress with zstd (level 1 = fast compression, ~5% larger than level 3)
        let compressed = Zstd.compress 1 serialized
        -- Build header: magic (8 bytes) + schema signature (8 bytes)
        let signatureBytes = encode schemaSignature
        let header = cacheMagic <> signatureBytes
        -- Write header + compressed data
        BS.writeFile zstdFile (header <> compressed)
        reportCacheOperation $
            "Matrix cache saved ("
                ++ show (dbActivityCount db)
                ++ " activities, "
                ++ show (VU.length $ dbTechnosphereTriples db)
                ++ " tech entries, "
                ++ show (VU.length $ dbBiosphereTriples db)
                ++ " bio entries, compressed)"

--------------------------------------------------------------------------------
-- Cross-Database Linking
--------------------------------------------------------------------------------

{- | CrossDBLinkingStats, mempty, (<>),
  crossDBLinksCount, unresolvedCount, crossDBBySource
  are now defined in Types and re-exported from this module.
-}

{- | Load EcoSpold files with cross-database linking support.

This function loads EcoSpold files and then attempts to resolve unlinked
technosphere exchanges by searching across other already-loaded databases.

The loading sequence:
1. Parse XML files into SimpleDatabase
2. Build supplier index for THIS database
3. Attempt linking within THIS database (standard behavior)
4. For remaining unlinked exchanges, search OTHER databases
5. Report linking summary with cross-DB statistics
-}
loadDatabaseWithCrossDBLinking ::
    -- | What this database is read under: units, aliases, allocation key
    LoadOptions ->
    -- | Pre-built indexes from other databases
    [IndexedDatabase] ->
    -- | Synonym database for name matching
    SynonymDB ->
    -- | Location hierarchy (empty = use built-in)
    M.Map Location [Location] ->
    -- | Geography policy for this database
    GeographyPolicy ->
    -- | Path to load from
    FilePath ->
    IO (Either T.Text (SimpleDatabase, CrossDBLinkingStats))
loadDatabaseWithCrossDBLinking opts otherIndexes synonymDB locationHier policy path = do
    let unitConfig = loUnitConfig opts
    result <- loadDatabaseWithLocationAliases opts path
    case result of
        Left err -> return $ Left err
        Right simpleDb -> do
            -- Detect unknown units from the database's unit definitions
            let !unknownUnits =
                    S.fromList
                        [ unitName u
                        | u <- M.elems (sdbUnits simpleDb)
                        , not (UC.isKnownUnit unitConfig (unitName u))
                        , not (T.null (unitName u))
                        ]
            unless (S.null unknownUnits) $
                reportProgress Warning $
                    printf
                        "%d unknown unit(s): %s — add to the [[units]] CSV file"
                        (S.size unknownUnits)
                        (T.unpack $ T.intercalate ", " $ map (\u -> "\"" <> u <> "\"") $ S.toList unknownUnits)

            -- If there are other databases to search, perform cross-DB linking
            let !totalInputs = countTotalTechInputs simpleDb
            if null otherIndexes
                then do
                    -- No cross-DB linking needed
                    let !stats = mempty{cdlUnknownUnits = unknownUnits, cdlTotalInputs = totalInputs}
                    reportCrossDBLinkingStats (M.size (sdbActivities simpleDb)) stats
                    return $ Right (simpleDb, stats)
                else do
                    -- Perform cross-database linking using pre-built indexes
                    (linkedDb, stats) <-
                        fixActivityLinksWithCrossDB
                            otherIndexes
                            synonymDB
                            unitConfig
                            locationHier
                            policy
                            simpleDb
                    return $ Right (linkedDb, stats{cdlUnknownUnits = unknownUnits})

{- | Fix activity links using cross-database lookup.

For each unlinked technosphere input (where activityLinkId is nil),
search across other loaded databases to find a matching supplier.

Matching criteria:
- Product name must match (exact, synonym, or fuzzy)
- Units must be compatible
- Location scoring with hierarchy fallback

Cross-database links are stored in CrossDBLinkingStats.cdlLinks for use
in chained inventory solving. The exchanges are NOT modified - they
remain "unlinked" from the perspective of the internal matrix, but the
CrossDBLinks provide the information needed to resolve them at solve time.
-}
fixActivityLinksWithCrossDB ::
    -- | Pre-built indexes from other databases
    [IndexedDatabase] ->
    -- | Synonym database
    SynonymDB ->
    -- | Unit configuration
    UC.UnitConfig ->
    -- | Location hierarchy (code → parent codes)
    M.Map Location [Location] ->
    -- | Geography policy for this database
    GeographyPolicy ->
    -- | Database to fix
    SimpleDatabase ->
    IO (SimpleDatabase, CrossDBLinkingStats)
fixActivityLinksWithCrossDB indexedDbs synonymDB unitConfig locationHier policy db = do
    -- Count unlinked exchanges before
    let unlinkedBefore = countUnlinkedExchanges db
        !totalInputs = countTotalTechInputs db

    -- If no unlinked exchanges, skip
    if unlinkedBefore == 0
        then do
            reportProgress Info "No unlinked exchanges to resolve via cross-DB linking"
            return (db, mempty{cdlTotalInputs = totalInputs})
        else do
            reportProgress Info $
                printf
                    "Cross-database linking: %d unlinked exchanges, searching %d database(s)..."
                    unlinkedBefore
                    (length indexedDbs)

            -- Report index stats
            forM_ indexedDbs $ \idb ->
                reportProgress Info $
                    printf
                        "  - %s: %d products indexed"
                        (T.unpack (idbName idb))
                        (M.size (idbByProductName idb))

            -- Build the linking context with pre-built indexes
            let linkingCtx =
                    LinkingContext
                        { lcIndexedDatabases = indexedDbs
                        , lcSynonymDB = synonymDB
                        , lcUnitConfig = unitConfig
                        , lcThreshold = defaultLinkingThreshold
                        , lcLocationHierarchy = if M.null locationHier then locationHierarchy else locationHier
                        , lcGeographyPolicy = policy
                        , lcSupplierAliases = emptyAliasMap
                        }

            -- Process all activities to find cross-DB links
            reportProgress Info "Finding cross-database suppliers..."
            let stats =
                    findAllCrossDBLinks
                        linkingCtx
                        (sdbTechFlows db)
                        (sdbWasteFlows db)
                        (sdbUnits db)
                        (sdbActivities db)

            -- Report statistics
            let !stats' = stats{cdlTotalInputs = totalInputs}
            reportCrossDBLinkingStats (M.size (sdbActivities db)) stats'

            -- Return the original database unchanged, along with the cross-DB links
            -- The links will be stored in the Database.dbCrossDBLinks field later
            return (db, stats')

{- | Inputs that demand a supplier — the exact set the matrix builder tries to
resolve in 'Database.MatrixBuild.techTriple'.

Biosphere flows need no supplier. Reference exchanges sit on the diagonal of
@(I-A)@ and are skipped by the matrix builder, so a treatment process's
'ReferenceInput' is a self-edge, not a supplier demand — counting it would drag
completeness below 100% for a perfectly solvable database. Waste *outputs* are
generated, not demanded; only waste/technosphere *inputs* remain.

An input of zero demands nothing either: the matrix builder emits no entry for
it, so no producer is ever looked up, and counting it as an unmet demand would
report a gap that no solve can encounter.
-}
isSupplierDemand :: Exchange -> Bool
isSupplierDemand ex =
    exchangeAmount ex /= 0
        && not (isBiosphereExchange ex)
        && exchangeIsInput ex
        && not (exchangeIsReference ex)

{- | True when a staged input resolves to a producer activity present in the
same database — the @(activityLinkId, flowId)@ branch of
'Database.MatrixBuild.findProducer'.

The process-link branch is deliberately omitted: a 'ProcessId' is an interned
index assigned only when matrices are built, so it never exists on a
'SimpleDatabase' (it is always 'Nothing' here). The loaded-database counterpart
'collectDanglingProductNames' has the real lookup and calls 'findProducer'
directly, honouring both branches.

A nil @activityLinkId@ (SimaPro inputs awaiting cross-DB linking, or a genuine
orphan) is never an internal producer. A *non-nil* link to an activity absent
from this database — e.g. a partial EcoSpold2 import that references ecoinvent
background activities it doesn't ship — is unresolved too: the matrix builder
silently drops such an exchange, so it must count as unlinked rather than
masquerade as a resolved internal link.
-}
hasInternalProducer :: SimpleDatabase -> Exchange -> Bool
hasInternalProducer db ex =
    case exchangeActivityLinkId ex of
        Nothing -> False
        Just actUUID -> M.member (actUUID, exchangeFlowId ex) (sdbActivities db)

{- | Re-resolve the cross-DB links of a 'SimpleDatabase' against the given
dependencies, optionally aliasing supplier names. Unlike
'fixActivityLinksWithCrossDB' this always recomputes — a relink must re-resolve
already-linked exchanges, e.g. to apply a new alias map — and threads @aliases@
into 'lcSupplierAliases', so a staged relink behaves exactly like the loaded one.
-}
relinkSimpleDatabase ::
    [IndexedDatabase] ->
    SynonymDB ->
    UC.UnitConfig ->
    M.Map Location [Location] ->
    GeographyPolicy ->
    AliasMap ->
    SimpleDatabase ->
    CrossDBLinkingStats
relinkSimpleDatabase indexedDbs synonymDB unitConfig locationHier policy aliases db =
    let ctx =
            LinkingContext
                { lcIndexedDatabases = indexedDbs
                , lcSynonymDB = synonymDB
                , lcUnitConfig = unitConfig
                , lcThreshold = defaultLinkingThreshold
                , lcLocationHierarchy = if M.null locationHier then locationHierarchy else locationHier
                , lcGeographyPolicy = policy
                , lcSupplierAliases = aliases
                }
        stats = findAllCrossDBLinks ctx (sdbTechFlows db) (sdbWasteFlows db) (sdbUnits db) (sdbActivities db)
     in stats{cdlTotalInputs = countTotalTechInputs db}

{- | Product names of technosphere demands with no resolved internal producer —
the supplier gaps surfaced on the setup page. Covers both nil-link inputs and
non-nil links whose target activity is absent (partial EcoSpold2 imports).
-}
collectUnlinkedProductNames :: SimpleDatabase -> M.Map T.Text Int
collectUnlinkedProductNames db =
    M.fromListWith
        (+)
        [ (tfName flow, 1)
        | act <- M.elems (sdbActivities db)
        , ex@TechnosphereExchange{} <- exchanges act
        , isSupplierDemand ex
        , not (hasInternalProducer db ex)
        , Just flow <- [M.lookup (exchangeFlowId ex) (sdbTechFlows db)]
        ]

-- | Count supplier demands with no resolved internal producer.
countUnlinkedExchanges :: SimpleDatabase -> Int
countUnlinkedExchanges db =
    length
        [ ()
        | act <- M.elems (sdbActivities db)
        , ex <- exchanges act
        , isSupplierDemand ex
        , not (hasInternalProducer db ex)
        ]

-- | Count total supplier demands — the completeness denominator.
countTotalTechInputs :: SimpleDatabase -> Int
countTotalTechInputs db =
    length
        [ ()
        | act <- M.elems (sdbActivities db)
        , ex <- exchanges act
        , isSupplierDemand ex
        ]

{- | How many cross-DB links resolve each consumer @(activityUUID, productUUID,
flowId)@ triple. The engine resolves a demand by @(activityLinkId, flowId)@, so
one activity can consume the same product flow from several suppliers; counting
coverage (rather than testing set membership) lets the dangling scan drop exactly
the covered occurrences and still name a genuinely unresolved sibling.
-}
crossDBCoveredCounts :: [CrossDBLink] -> M.Map (UUID.UUID, UUID.UUID, UUID.UUID) Int
crossDBCoveredCounts links =
    M.fromListWith (+) [((cdlConsumerActUUID l, cdlConsumerProdUUID l, cdlConsumerFlowId l), 1) | l <- links]

{- | Tally dangling product names from @(consumer-triple, productName)@ pairs,
dropping per triple as many occurrences as 'crossDBCoveredCounts' already
covers. Inputs sharing a triple share a product name, so the surplus over the
covered count is the real gap.
-}
tallyDangling ::
    M.Map (UUID.UUID, UUID.UUID, UUID.UUID) Int ->
    [((UUID.UUID, UUID.UUID, UUID.UUID), T.Text)] ->
    M.Map T.Text Int
tallyDangling covered inputs =
    M.fromListWith
        (+)
        [ (name, surplus)
        | (triple, (name, n)) <- M.toList byTriple
        , let surplus = n - M.findWithDefault 0 triple covered
        , surplus > 0
        ]
  where
    byTriple =
        M.fromListWith
            (\(nm, a) (_, b) -> (nm, a + b))
            [(triple, (name, 1 :: Int)) | (triple, name) <- inputs]

{- | Product names of a *loaded* database's dangling internal links: non-nil
@activityLinkId@ inputs that 'findProducer' cannot resolve against the
database's own process lookup (the matrix builder silently drops them) *and*
that no cross-DB link supplies. The loaded-path counterpart that names the
supplier gaps a partial EcoSpold2 import leaves behind — distinct from nil-link
inputs, the cross-DB candidates already tracked in the linking stats.

Sharing 'findProducer' keeps this honest with the matrix even once
@techProcessLinkId@ is populated: an input whose process link resolves is not
dangling, however its activity link looks. Subtracting per-triple cross-DB
coverage ('crossDBCoveredCounts') keeps it honest the other way: once the
matching background is loaded, a UUID- or attribute-resolved input is supplied,
not missing.
-}
collectDanglingProductNames :: Database -> M.Map T.Text Int
collectDanglingProductNames db =
    tallyDangling
        (crossDBCoveredCounts (dbCrossDBLinks db))
        [ ((actUUID, prodUUID, exchangeFlowId ex), tfName flow)
        | ((actUUID, prodUUID), act) <- zip (V.toList (dbProcessIdTable db)) (V.toList (dbActivities db))
        , ex@TechnosphereExchange{} <- exchanges act
        , isSupplierDemand ex
        , isNothing (findProducer (dbProcessIdLookup db) ex)
        , Just _ <- [exchangeActivityLinkId ex]
        , Just flow <- [M.lookup (exchangeFlowId ex) (dbTechFlows db)]
        ]

{- | Staged-path counterpart of 'collectDanglingProductNames' on a
'SimpleDatabase' plus its just-computed links. A 'SimpleDatabase' has no process
lookup yet, so internal resolution is checked with 'hasInternalProducer' and
coverage against the supplied links rather than 'dbCrossDBLinks'.
-}
collectStagedDanglingProductNames :: SimpleDatabase -> [CrossDBLink] -> M.Map T.Text Int
collectStagedDanglingProductNames db links =
    tallyDangling
        (crossDBCoveredCounts links)
        [ ((actUUID, prodUUID, exchangeFlowId ex), tfName flow)
        | ((actUUID, prodUUID), act) <- M.toList (sdbActivities db)
        , ex@TechnosphereExchange{} <- exchanges act
        , isSupplierDemand ex
        , not (hasInternalProducer db ex)
        , Just _ <- [exchangeActivityLinkId ex]
        , Just flow <- [M.lookup (exchangeFlowId ex) (sdbTechFlows db)]
        ]

-- ---------------------------------------------------------------------------
-- Supplier-gap report
-- ---------------------------------------------------------------------------

{- | Why one supplier demand is left unsupplied after internal resolution and
cross-DB linking.
-}
data GapReason
    = -- | Nil-link input the attribute matcher could not place, with its blocker.
      GapBlocked !LinkBlocker
    | {- | Non-nil source identity no dependency ships, and no attribute match
      rescued it — a partial import referencing activities it doesn't carry.
      -}
      GapDanglingIdentity
    | {- | Waste input (treatment side): never a cross-DB demand, so an
      internally unlinked one is a genuine gap.
      -}
      GapWasteInput
    deriving (Show, Eq)

-- | One consumer edge left unsupplied — the unit of the supplier-gap report.
data GapEdge = GapEdge
    { gapFlowName :: !T.Text
    , gapLocation :: !T.Text
    -- ^ Effective requested location ("" when the demand names none)
    , gapUnit :: !T.Text
    , gapAmount :: !Double
    , gapConsumerAct :: !UUID.UUID
    , gapConsumerProd :: !UUID.UUID
    , gapReason :: !GapReason
    }
    deriving (Show, Eq)

-- | One consuming process of a gap entry, with how many of its edges hit it.
data GapConsumer = GapConsumer
    { gcActUUID :: !UUID.UUID
    , gcProdUUID :: !UUID.UUID
    , gcActivityName :: !T.Text
    , gcProductName :: !T.Text
    , gcLocation :: !T.Text
    , gcEdges :: !Int
    }
    deriving (Show, Eq)

{- | Aggregate over one (flow name, location, unit) key of the gap report.
Unit is part of the key so 'geDemandSum' never mixes units.
-}
data GapEntry = GapEntry
    { geFlowName :: !T.Text
    , geLocation :: !T.Text
    , geUnit :: !T.Text
    , geReason :: !GapReason
    , geEdges :: !Int
    , geConsumers :: !Int
    , geDemandSum :: !Double
    , geTopConsumers :: ![GapConsumer]
    }
    deriving (Show, Eq)

-- | Supplier-gap report: header arithmetic plus the aggregated gap entries.
data GapReport = GapReport
    { grDbName :: !T.Text
    , grTotalInputs :: !Int
    , grInternalLinks :: !Int
    , grCrossDBLinks :: !Int
    , grUnresolvedEdges :: !Int
    , grUnresolvedProducts :: !Int
    , grCompleteness :: !Double
    , grGaps :: ![GapEntry]
    }
    deriving (Show, Eq)

{- | Shared gap-edge scan: every supplier demand with no internal producer,
minus per-triple cross-DB coverage. The per-triple accounting mirrors
'tallyDangling' (count-based: a partially covered triple drops its covered
occurrences in scan order), so edge counts stay consistent with the dangling
scans.
-}
gapEdgesWith ::
    (Exchange -> Bool) ->
    SimpleDatabase ->
    [CrossDBLink] ->
    CrossDBLinkingStats ->
    [GapEdge]
gapEdgesWith hasProducer db links stats =
    concatMap surplus (M.toList byTriple)
  where
    covered = crossDBCoveredCounts links
    surplus (triple, es) = drop (M.findWithDefault 0 triple covered) es
    byTriple =
        M.fromListWith
            (flip (<>))
            [ ((actUUID, prodUUID, exchangeFlowId ex), [edge])
            | ((actUUID, prodUUID), act) <- M.toList (sdbActivities db)
            , ex <- exchanges act
            , isSupplierDemand ex
            , not (hasProducer ex)
            , Just edge <- [mkGapEdge db stats actUUID prodUUID ex]
            ]

{- | Describe one unsupplied demand. Biosphere exchanges are never demands
('isSupplierDemand'), hence 'Nothing'. A missing flow entry doesn't hide the
edge: the flow UUID stands in for the name so the report stays countable.

'cdlUnresolvedProducts' records one blocker per flow /name/, while the report
keys entries by (name, location, unit) — two same-named entries at different
locations therefore share that blocker even when the underlying causes differ.
-}
mkGapEdge ::
    SimpleDatabase ->
    CrossDBLinkingStats ->
    UUID.UUID ->
    UUID.UUID ->
    Exchange ->
    Maybe GapEdge
mkGapEdge db stats actUUID prodUUID ex = case ex of
    TechnosphereExchange{} ->
        let name = flowNameOr tfName (sdbTechFlows db)
            reason = case exchangeActivityLinkId ex of
                Nothing -> GapBlocked (maybe NoNameMatch upBlocker (M.lookup name (cdlUnresolvedProducts stats)))
                Just _ -> GapDanglingIdentity
         in Just (edge name reason)
    WasteExchange{} -> Just (edge (flowNameOr wfName (sdbWasteFlows db)) GapWasteInput)
    BiosphereExchange{} -> Nothing
  where
    flowNameOr nameOf flows =
        maybe (UUID.toText (exchangeFlowId ex)) nameOf (M.lookup (exchangeFlowId ex) flows)
    edge name reason =
        GapEdge
            { gapFlowName = name
            , gapLocation =
                let loc = exchangeLocation ex
                 in if T.null loc then extractBracketedLocation name else loc
            , gapUnit = getUnitNameForExchange (sdbUnits db) ex
            , gapAmount = exchangeAmount ex
            , gapConsumerAct = actUUID
            , gapConsumerProd = prodUUID
            , gapReason = reason
            }

-- | Consumers shown per gap entry — the tail is countable via 'geConsumers'.
topConsumerCap :: Int
topConsumerCap = 20

{- | Group gap edges by (flow name, location, unit), sorted by edge count
descending. Within a group the reason with the richest diagnostic wins
('GapBlocked' over 'GapDanglingIdentity' over 'GapWasteInput'). Consumer
processes are named from the database, most-demanding first.
-}
gapEntries :: SimpleDatabase -> [GapEdge] -> [GapEntry]
gapEntries db edges =
    sortOn (Down . geEdges) (map entry (M.toList byKey))
  where
    byKey =
        M.fromListWith
            (flip (<>))
            [((gapFlowName e, gapLocation e, gapUnit e), [e]) | e <- edges]
    reasonRank r = case r of
        GapBlocked _ -> 0 :: Int
        GapDanglingIdentity -> 1
        GapWasteInput -> 2
    strongerReason a b = if reasonRank a <= reasonRank b then a else b
    entry ((name, loc, unit), es) =
        let consumers =
                M.fromListWith (+) [((gapConsumerAct e, gapConsumerProd e), 1 :: Int) | e <- es]
         in GapEntry
                { geFlowName = name
                , geLocation = loc
                , geUnit = unit
                , geReason = foldr (strongerReason . gapReason) GapWasteInput es
                , geEdges = length es
                , geConsumers = M.size consumers
                , geDemandSum = sum (map gapAmount es)
                , geTopConsumers =
                    [ consumerOf a p n
                    | ((a, p), n) <- take topConsumerCap (sortOn (Down . snd) (M.toList consumers))
                    ]
                }
    consumerOf a p n =
        GapConsumer
            { gcActUUID = a
            , gcProdUUID = p
            , gcActivityName = maybe (UUID.toText a) activityName (M.lookup (a, p) (sdbActivities db))
            , gcProductName = maybe (UUID.toText p) tfName (M.lookup p (sdbTechFlows db))
            , gcLocation = maybe "" activityLocation (M.lookup (a, p) (sdbActivities db))
            , gcEdges = n
            }

{- | Assemble the report. Header counts reuse the setup-page predicates
('countTotalTechInputs' / 'countUnlinkedExchanges'); 'grUnresolvedEdges' is the
edge-accurate count (per-triple coverage), so it can sit below the setup page's
coarse @unlinked - crossDBLinks@ difference when waste-output links exist.
-}
buildGapReport :: T.Text -> SimpleDatabase -> Int -> [GapEdge] -> GapReport
buildGapReport dbName db nLinks edges =
    let total = countTotalTechInputs db
        unlinked = countUnlinkedExchanges db
        entries = gapEntries db edges
     in GapReport
            { grDbName = dbName
            , grTotalInputs = total
            , grInternalLinks = max 0 (total - unlinked)
            , grCrossDBLinks = nLinks
            , grUnresolvedEdges = length edges
            , grUnresolvedProducts = length entries
            , grCompleteness =
                if total > 0
                    then 100 * fromIntegral (total - length edges) / fromIntegral total
                    else 100
            , grGaps = entries
            }

-- | Supplier-gap report of a loaded database ('findProducer' honours process links).
gapReportForLoaded :: T.Text -> Database -> GapReport
gapReportForLoaded dbName db =
    let sdb = toSimpleDatabase db
        edges =
            gapEdgesWith
                (isJust . findProducer (dbProcessIdLookup db))
                sdb
                (dbCrossDBLinks db)
                (dbLinkingStats db)
     in buildGapReport dbName sdb (length (dbCrossDBLinks db)) edges

{- | Staged-path counterpart of 'gapReportForLoaded', against the staged
database's just-computed links and stats.
-}
gapReportForStaged :: T.Text -> SimpleDatabase -> CrossDBLinkingStats -> GapReport
gapReportForStaged dbName sdb stats =
    buildGapReport dbName sdb (crossDBLinksCount stats) (gapEdgesWith (hasInternalProducer sdb) sdb (cdlLinks stats) stats)

{- | Per-run linking environment: the cross-DB context, the consumer database's
own activity-key set (for the internal-resolution gate), and its flow tables.
Bundled so the per-activity / per-exchange matchers keep short signatures.

@lsOwnKeys@ lets the per-exchange matcher tell a non-nil link that resolves
*internally* (the matrix builder handles it) from a dangling one that needs a
cross-DB supplier — so we never emit a redundant cross-DB link for an input
already satisfied in place.
-}
data LinkScan = LinkScan
    { lsCtx :: !LinkingContext
    , lsOwnKeys :: !(S.Set (UUID.UUID, UUID.UUID))
    , lsTechFlows :: !TechFlowDB
    , lsWasteFlows :: !WasteFlowDB
    , lsUnits :: !UnitDB
    }

{- | Find all cross-database links without modifying activities
Returns statistics including the CrossDBLinks for chained solving
-}
findAllCrossDBLinks ::
    LinkingContext ->
    TechFlowDB ->
    WasteFlowDB ->
    UnitDB ->
    ActivityMap ->
    CrossDBLinkingStats
findAllCrossDBLinks ctx techFlowDb wasteFlowDb unitDb activities =
    let !scan = LinkScan ctx (M.keysSet activities) techFlowDb wasteFlowDb unitDb
        results = M.mapWithKey (findActivityCrossDBLinks scan) activities
     in mconcat (M.elems results)

-- | Find cross-database links for one activity's exchanges
findActivityCrossDBLinks ::
    LinkScan ->
    -- | Consumer activity key (actUUID, prodUUID)
    (UUID.UUID, UUID.UUID) ->
    Activity ->
    CrossDBLinkingStats
findActivityCrossDBLinks scan (consumerActUUID, consumerProdUUID) act =
    mconcat (map (findExchangeCrossDBLink scan consumerActUUID consumerProdUUID) (exchanges act))

{- | Find cross-database link for a single exchange.

Technosphere inputs that need a supplier (nil-link, or a non-nil
'activityLinkId' to an activity this database does not ship) resolve via a
cascade:

1. __Exact source identity__ — @(activityLinkId, flowId)@ matched verbatim in a
   dependency ('findSupplierByActivityProduct'). The same-release case; the
   dataset author's own disambiguation, no guessing. Nil-link inputs skip this
   tier (they carry no identity).
2. __Attribute matching__ — name / location / unit scoring
   ('findSupplierAcrossDatabases'), the matcher every other cross-link uses.
   When a *non-nil* input falls through to here its source activity was absent
   from every dependency, so the match is a likely cross-version stitch,
   recorded in 'cdlAttributeFallbacks' for the consumer to verify.

A link whose target resolves in the internal matrix would be double-counted by
a cross-DB link too, so 'resolvesInternally' gates it out — mirroring
'Database.MatrixBuild.findProducer': a populated process link, or a non-nil
@activityLinkId@ whose @(linkId, flowId)@ key is one of this database's own
('lsOwnKeys'). Waste outputs take the same gate, then a strict matcher chosen by
their link — 'findWasteTreatmentByActivity' when they name a treatment,
'findWasteTreatmentAcrossDatabases' when they name none — with no synonym and
no widening in either.
-}
findExchangeCrossDBLink ::
    LinkScan ->
    UUID.UUID ->
    UUID.UUID ->
    Exchange ->
    CrossDBLinkingStats
findExchangeCrossDBLink LinkScan{lsCtx = ctx, lsOwnKeys = ownKeys, lsTechFlows = techFlowDb, lsUnits = unitDb} consumerActUUID consumerProdUUID ex@TechnosphereExchange{techFlowId = fid, techAmount = amt, techActivityLinkId = linkId, techLocation = loc}
    | isSupplierDemand ex && not resolvesInternally =
        maybe mempty resolveTechInput (M.lookup fid techFlowDb)
    | otherwise = mempty
  where
    resolvesInternally =
        isJust (exchangeProcessLinkId ex)
            || (linkId /= UUID.nil && S.member (linkId, fid) ownKeys)
    mkTechLink supAct supProd supName supLoc srcDb tied flowUnitName =
        CrossDBLink
            { cdlConsumerActUUID = consumerActUUID
            , cdlConsumerProdUUID = consumerProdUUID
            , cdlConsumerFlowId = fid
            , cdlSupplierActUUID = supAct
            , cdlSupplierProdUUID = supProd
            , cdlCoefficient = amt
            , cdlExchangeUnit = flowUnitName
            , cdlFlowName = supName
            , cdlLocation = supLoc
            , cdlSourceDatabase = srcDb
            , cdlTiedAlternatives = tied
            }
    resolveTechInput flow =
        let flowUnitName = maybe "" unitName (M.lookup (tfUnitId flow) unitDb)
            identityMatches =
                if linkId == UUID.nil
                    then []
                    else findSupplierByActivityProduct (lcIndexedDatabases ctx) linkId fid
         in case identityMatches of
                ((entry, srcDb) : rest) ->
                    let !crossLink =
                            mkTechLink
                                (seActivityUUID entry)
                                (seProductUUID entry)
                                (seProductName entry)
                                (seLocation entry)
                                srcDb
                                (sort (map snd rest))
                                flowUnitName
                     in mempty{cdlLinks = [crossLink]}
                [] -> attributeMatch flow flowUnitName
    attributeMatch flow flowUnitName =
        case findSupplierAcrossDatabases ctx (tfName flow) loc flowUnitName of
            result@CrossDBLinked{} ->
                let !crossLink =
                        mkTechLink
                            (cdlrActivityUUID result)
                            (cdlrProductUUID result)
                            (cdlrProductName result)
                            (cdlrLocation result)
                            (cdlrDatabaseName result)
                            (cdlrTiedDatabases result)
                            flowUnitName
                    locFallbacks =
                        [ LocationFallback
                            { lfProduct = cdlrProductName result
                            , lfRequested = req
                            , lfActual = actLoc
                            , lfKind = kind
                            }
                        | UpperLocationUsed req actLoc kind <- cdlrWarnings result
                        ]
                    -- Non-nil input matched only by attributes: its named source
                    -- activity was in no dependency — flag the cross-version risk.
                    attrFallbacks =
                        [ AttributeFallback
                            { afProduct = tfName flow
                            , afRequested = loc
                            , afMatched = cdlrLocation result
                            , afSourceDatabase = cdlrDatabaseName result
                            }
                        | linkId /= UUID.nil
                        ]
                 in mempty
                        { cdlLinks = [crossLink]
                        , cdlLocationFallbacks = locFallbacks
                        , cdlAttributeFallbacks = attrFallbacks
                        }
            CrossDBNotLinked blocker
                -- Nil-link inputs report a rich blocker; a non-nil dangling input
                -- (no identity, no attribute match) is left for the dangling scan.
                | linkId == UUID.nil -> unresolvedStats flow blocker
                | otherwise -> mempty
    unresolvedStats flow blocker =
        let unresolved = case blocker of
                LocationRejectedByPolicy{lrRequested = req, lrBestCandidate = actLoc, lrBestKind = kind} ->
                    [ LocationUnresolved
                        { luProduct = tfName flow
                        , luRequested = req
                        , luReason = "policy rejected " <> locationKindCode kind <> " candidate " <> actLoc
                        }
                    ]
                LocationUnavailable req ->
                    [ LocationUnresolved
                        { luProduct = tfName flow
                        , luRequested = req
                        , luReason = "no candidate above link threshold"
                        }
                    ]
                NoNameMatch -> []
                UnitIncompatible{} -> []
                AliasTargetMissing{} -> []
         in mempty
                { cdlUnresolvedProducts = M.singleton (tfName flow) UnresolvedProduct{upDemands = 1, upBlocker = blocker}
                , cdlLocationUnresolved = unresolved
                }
findExchangeCrossDBLink _ _ _ BiosphereExchange{} = mempty
-- Cross-DB linking for waste OUTPUTS the internal matrix does not route:
-- strict match only. No synonym, no fuzzy name match, no location widening.
-- Multi-DB matches stay orphan as 'cdlWasteAmbiguous'. Which matcher applies
-- follows the link: an output that names its treatment is matched on that
-- identity, one that names none on the flow itself. Neither falls back on the
-- other — substituting a treatment found by name for the one the author named
-- would link the waste to an activity nobody asked for.
-- Waste inputs (treatment side) are left alone: they have no clean LCA
-- semantic as a cross-DB demand.
findExchangeCrossDBLink LinkScan{lsCtx = ctx, lsOwnKeys = ownKeys, lsWasteFlows = wasteFlowDb} consumerActUUID consumerProdUUID ex@WasteExchange{waFlowId = fid, waAmount = amt, waActivityLinkId = lid, waIsInput = isInp}
    | not isInp && not resolvesInternally =
        case treatmentMatch of
            WasteMatched entry dbN ->
                let
                    -- The dep-demand solve drives the matched treatment in
                    -- its OWN reference convention: an EcoSpold2 treatment
                    -- has a negative-output reference ('seRefSign' = -1),
                    -- an ILCD one a positive 'ReferenceInput' (+1). The
                    -- consumer's waste-output amount is positive, so we
                    -- carry the treatment's sign into the coefficient —
                    -- without it a negative-reference background treatment
                    -- scores the treated waste's burden with a flipped sign.
                    !crossLink =
                        CrossDBLink
                            { cdlConsumerActUUID = consumerActUUID
                            , cdlConsumerProdUUID = consumerProdUUID
                            , cdlConsumerFlowId = fid
                            , cdlSupplierActUUID = seActivityUUID entry
                            , cdlSupplierProdUUID = seProductUUID entry
                            , cdlCoefficient = amt * seRefSign entry
                            , cdlExchangeUnit = seUnit entry
                            , cdlFlowName = seProductName entry
                            , cdlLocation = seLocation entry
                            , cdlSourceDatabase = dbN
                            , cdlTiedAlternatives = []
                            }
                 in
                    mempty{cdlLinks = [crossLink], cdlWasteExactLinks = 1}
            WasteAmbiguous _ -> mempty{cdlWasteAmbiguous = 1}
            WasteNoMatch -> mempty{cdlCutoffWasteCount = 1}
    | otherwise = mempty
  where
    -- Same gate as the technosphere arm: a link the matrix already routes in
    -- place would be counted twice if a cross-DB link were emitted for it too.
    resolvesInternally =
        isJust (exchangeProcessLinkId ex)
            || (lid /= UUID.nil && S.member (lid, fid) ownKeys)
    treatmentMatch
        | lid /= UUID.nil = findWasteTreatmentByActivity ctx lid fid
        | otherwise = findWasteTreatmentAcrossDatabases ctx fid flowName
    flowName = maybe "" wfName (M.lookup fid wasteFlowDb)

-- | Report cross-database linking statistics
reportCrossDBLinkingStats :: Int -> CrossDBLinkingStats -> IO ()
reportCrossDBLinkingStats nActivities stats = do
    let !nInputs = cdlTotalInputs stats
        !nCrossDB = crossDBLinksCount stats
        !nUnresolved = unresolvedCount stats
        !nInternal = max 0 (nInputs - nCrossDB - nUnresolved)
        !nResolved = nInternal + nCrossDB

    -- Summary line (skip "0/0" for databases without technosphere input tracking)
    if nInputs > 0
        then do
            let !completeness = 100.0 * fromIntegral nResolved / fromIntegral nInputs :: Double
            reportProgress Info $
                printf
                    "Supply chain: %.1f%% complete (%d/%d inputs resolved), %d activities"
                    completeness
                    nResolved
                    nInputs
                    nActivities
            reportProgress Info $
                printf "  Internal: %d, Cross-DB: %d, Unresolved: %d" nInternal nCrossDB nUnresolved
        else
            reportProgress Info $
                printf "Supply chain: %d activities (no technosphere inputs)" nActivities

    -- Per-database breakdown
    forM_ (M.toList (crossDBBySource stats)) $ \(srcDb, count) ->
        reportProgress Info $
            printf "  - %s: %d links" (T.unpack srcDb) count

    -- Waste exchange resolution (only printed when this DB has any waste activity)
    let !wExact = cdlWasteExactLinks stats
        !wAmbig = cdlWasteAmbiguous stats
        !wCutoff = cdlCutoffWasteCount stats
    when (wExact + wAmbig + wCutoff > 0) $
        reportProgress Info $
            printf
                "Waste: %d linked (exact), %d ambiguous, %d cut-off (treatment not modelled)"
                wExact
                wAmbig
                wCutoff

    -- Missing suppliers
    let !missing = sortOn (Down . upDemands . snd) $ M.toList (cdlUnresolvedProducts stats)
    unless (null missing) $ do
        reportProgress Warning $
            printf "Missing suppliers: %d products unresolved" (length missing)
        forM_ (take 20 missing) $ \(name, unresolved) ->
            reportProgress Warning $
                printf
                    "  - %s (%d activities) — %s"
                    (T.unpack name)
                    (upDemands unresolved)
                    (showBlocker (upBlocker unresolved))
        when (length missing > 20) $
            reportProgress Warning $
                printf "  ... and %d more" (length missing - 20)

    -- Unknown units
    let !unknowns = S.toList (cdlUnknownUnits stats)
    unless (null unknowns) $
        reportProgress Warning $
            printf "Unknown units: %s" (T.unpack $ T.intercalate ", " unknowns)

    -- Location fallbacks (deduplicated)
    let !uniqueFallbacks = deduplicateFallbacks (cdlLocationFallbacks stats)
        !nFallbacks = length uniqueFallbacks
    when (nFallbacks > 0) $ do
        reportProgress Info $
            printf "Location fallbacks: %d unique products matched with different location" nFallbacks
        forM_ uniqueFallbacks $ \LocationFallback{lfProduct, lfRequested, lfActual, lfKind} ->
            reportProgress Info $
                printf
                    "  - %s: %s → %s (%s)"
                    (T.unpack lfProduct)
                    (T.unpack lfRequested)
                    (T.unpack lfActual)
                    (T.unpack (locationKindCode lfKind))

    -- Inputs rejected by geography_policy (deduplicated)
    let !uniqueUnresolved = deduplicateUnresolved (cdlLocationUnresolved stats)
        !nUnresolved' = length uniqueUnresolved
    when (nUnresolved' > 0) $ do
        reportProgress Warning $
            printf "Location unresolved: %d unique products with no acceptable supplier" nUnresolved'
        forM_ uniqueUnresolved $ \LocationUnresolved{luProduct, luRequested, luReason} ->
            reportProgress Warning $
                printf
                    "  - %s [%s] — %s"
                    (T.unpack luProduct)
                    (T.unpack luRequested)
                    (T.unpack luReason)

    -- Attribute fallbacks: source-identity inputs matched by attributes because
    -- no dependency shipped the exact activity — a likely cross-version stitch.
    let !uniqueAttrFallbacks = deduplicateAttributeFallbacks (cdlAttributeFallbacks stats)
        !nAttrFallbacks = length uniqueAttrFallbacks
    when (nAttrFallbacks > 0) $ do
        reportProgress Warning $
            printf
                "%d background link(s) matched by attributes, not source identity — verify the dependency is the same source release"
                nAttrFallbacks
        forM_ uniqueAttrFallbacks $ \AttributeFallback{afProduct, afRequested, afMatched, afSourceDatabase} ->
            reportProgress Warning $
                printf
                    "  - %s [%s] → %s in %s"
                    (T.unpack afProduct)
                    (T.unpack afRequested)
                    (T.unpack afMatched)
                    (T.unpack afSourceDatabase)

showBlocker :: LinkBlocker -> String
showBlocker NoNameMatch = "Not found"
showBlocker UnitIncompatible{uiQueryUnit = q, uiSupplierUnit = s} = printf "Unit: %s vs %s" (T.unpack q) (T.unpack s)
showBlocker (LocationUnavailable loc) = printf "Location: %s" (T.unpack loc)
showBlocker LocationRejectedByPolicy{lrRequested = req, lrBestCandidate = act, lrBestKind = kind} =
    printf "Rejected by policy: %s → %s (%s)" (T.unpack req) (T.unpack act) (T.unpack (locationKindCode kind))
showBlocker (AliasTargetMissing name mLoc) =
    printf "Mapping target not found: %s%s" (T.unpack name) (maybe "" ((" @ " <>) . T.unpack) mLoc)
