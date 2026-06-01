{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
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
- Automatic cache invalidation based on source file changes
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
    loadDatabaseWithLocationAliases,
    loadDatabaseWithCrossDBLinking,

    -- * Cache Operations
    loadCachedDatabaseWithMatrices,
    saveCachedDatabaseWithMatrices,
    loadDatabaseFromCacheFile,
    generateMatrixCacheFilename,

    -- * Cross-Database Linking
    fixActivityLinksWithCrossDB,
    findAllCrossDBLinks,
    CrossDBLinkingStats (..),
    crossDBLinksCount,
    unresolvedCount,
    crossDBBySource,
    collectUnlinkedProductNames,

    -- * Database Analysis
    countTotalTechInputs,
    countUnlinkedExchanges,

    -- * Internal Linking
    fixSimaProActivityLinks,

    -- * Reporting
    reportCrossDBLinkingStats,

    -- * Internal (exposed for testing)
    normalizeText,
    mergeTechFlows,
    mergeBioFlows,
    generateActivityUUIDFromActivity,
    getReferenceProductUUID,
    UnlinkedSummary (..),
    buildSupplierIndex,
    buildSupplierIndexByName,
    fixExchangeLinkByName,
) where

import qualified BrightwayExcel.Parser as BrightwayExcel
import qualified Codec.Compression.Zstd as Zstd
import Control.Concurrent.Async
import Control.DeepSeq (force)
import Control.Exception (SomeException, catch, evaluate)
import Control.Monad
import Data.Bits (xor)
import qualified Data.ByteString as BS
import Data.Char (toLower)
import Data.Either (partitionEithers)
import Data.List (sort, sortBy, sortOn)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import qualified Data.Set as S
import Data.Store (decodeEx, encode)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Data.Typeable (typeOf, typeRepFingerprint)
import qualified Data.UUID as UUID
import qualified Data.UUID.V5 as UUID5
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word64)
import Database.CrossLinking (
    CrossDBLinkResult (..),
    IndexedDatabase (..),
    LinkWarning (..),
    LinkingContext (..),
    SupplierEntry (..),
    WasteTreatmentMatch (..),
    defaultLinkingThreshold,
    extractProductPrefixes,
    findSupplierAcrossDatabases,
    findWasteTreatmentAcrossDatabases,
    locationHierarchy,
    normalizeUnicode,
 )
import EcoSpold.Common (distributeFiles)
import EcoSpold.Parser1 (streamParseActivityAndFlowsFromFile1, streamParseAllDatasetsFromFile1)
import EcoSpold.Parser2 (streamParseActivityAndFlowsFromFile)
import GHC.Conc (getNumCapabilities)
import GHC.Fingerprint (Fingerprint (..))
import qualified ILCD.Parser as ILCD
import Progress
import qualified SimaPro.Parser as SimaPro
import SynonymDB (SynonymDB)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getFileSize, listDirectory, removeFile)
import System.FilePath (takeBaseName, takeDirectory, takeExtension, (</>))
import Text.Printf (printf)
import Types
import qualified UnitConversion as UC

-- | Magic bytes to identify VoLCA cache files
cacheMagic :: BS.ByteString
cacheMagic = "VOLCACHE"

{- | Merge two technosphere flows with the same UUID, combining their synonyms.
When multiple .spold files reference the same flow each may carry different
synonyms; M.fromListWith mergeTechFlows ensures no synonym is lost.
-}
mergeTechFlows :: TechnosphereFlow -> TechnosphereFlow -> TechnosphereFlow
mergeTechFlows a b = a{tfSynonyms = M.unionWith S.union (tfSynonyms a) (tfSynonyms b)}

-- | Biosphere counterpart of 'mergeTechFlows'.
mergeBioFlows :: BiosphereFlow -> BiosphereFlow -> BiosphereFlow
mergeBioFlows a b = a{bfSynonyms = M.unionWith S.union (bfSynonyms a) (bfSynonyms b)}

-- | 9-element unzip helper (Data.List ships 7-tuple as max).
unzip9 :: [(a, b, c, d, e, f, g, h, i)] -> ([a], [b], [c], [d], [e], [f], [g], [h], [i])
unzip9 = foldr step ([], [], [], [], [], [], [], [], [])
  where
    step (a, b, c, d, e, f, g, h, i) (as, bs, cs, ds, es, fs, gs, hs, is) =
        (a : as, b : bs, c : cs, d : ds, e : es, f : fs, g : gs, h : hs, i : is)

{- |
Schema signature automatically derived from the Database type structure.

Automatically changes when:
- Fields are added/removed from Database or nested types
- Type names change
- Type structure changes

The trailing 'xor' constant is a manual cache-busting salt — bump it (e.g.
4 → 5) when the semantics of the cached matrices change without any type
change, so existing caches are treated as incompatible and rebuilt on the
next load instead of silently returning stale numbers.

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

The signature is stored inside the cache file and checked on load.
If it doesn't match, the cache is automatically invalidated and rebuilt.
-}
schemaSignature :: Word64
schemaSignature =
    let Fingerprint hi lo = typeRepFingerprint (typeOf (undefined :: Database))
     in hi `xor` lo `xor` 7

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

-- | Get reference product UUID from activity exchanges
getReferenceProductUUID :: Activity -> UUID.UUID
getReferenceProductUUID act =
    case filter exchangeIsReference (exchanges act) of
        (ref : _) -> exchangeFlowId ref
        [] -> UUID.nil -- No reference product found

-- | Type alias for supplier lookup index (with location)
type SupplierIndex = M.Map (T.Text, T.Text) (UUID.UUID, UUID.UUID)

{- | Type alias for name-only supplier lookup (for SimaPro)
Maps normalizedProductName → (activityUUID, productUUID)
-}
type NameOnlyIndex = M.Map T.Text (UUID.UUID, UUID.UUID)

{- | Type alias for name-only supplier lookup with location (for EcoSpold1)
Maps normalizedProductName → (activityUUID, productUUID, location)
Used when exchange has no location attribute to find the activity's actual location
-}
type SupplierByNameWithLocation = M.Map T.Text (UUID.UUID, UUID.UUID, T.Text)

-- | Dataset number → (activityUUID, productUUID) for EcoSpold1 Tier 1 linking
type DatasetNumberIndex = M.Map Int (UUID.UUID, UUID.UUID)

-- | Information about an unlinked technosphere exchange
data UnlinkedExchange = UnlinkedExchange
    { ueFlowName :: !T.Text
    , ueLocation :: !T.Text
    }
    deriving (Eq, Ord, Show)

{- | Summary of unlinked exchanges grouped by consumer activity.
'Monoid' is hand-written: bare 'Int' has no canonical instance, and using
'Sum Int' would force every reader to unwrap.
-}
data UnlinkedSummary = UnlinkedSummary
    { usActivities :: !(M.Map T.Text [UnlinkedExchange]) -- consumer name → list of unlinked exchanges
    , usTotalLinks :: !Int
    , usFoundLinks :: !Int
    , usMissingLinks :: !Int
    }
    deriving (Show)

instance Semigroup UnlinkedSummary where
    UnlinkedSummary a1 t1 f1 m1 <> UnlinkedSummary a2 t2 f2 m2 =
        UnlinkedSummary (M.unionWith (++) a1 a2) (t1 + t2) (f1 + f2) (m1 + m2)

instance Monoid UnlinkedSummary where
    mempty = UnlinkedSummary M.empty 0 0 0

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

{- | Build name-only supplier index for SimaPro linking
Uses the normalized product name + extracted prefixes (no location required).
Exact names take priority via M.union.
-}
buildSupplierIndexByName :: ActivityMap -> TechFlowDB -> NameOnlyIndex
buildSupplierIndexByName activities techFlowDb =
    let entries =
            [ (tfName flow, (actUUID, prodUUID))
            | ((actUUID, prodUUID), act) <- M.toList activities
            , ex <- exchanges act
            , exchangeIsReference ex
            , Just flow <- [M.lookup (exchangeFlowId ex) techFlowDb]
            ]
        exactIndex = M.fromList [(normalizeText name, val) | (name, val) <- entries]
        prefixIndex =
            M.fromList
                [ (normalizeText p, val)
                | (name, val) <- entries
                , p <- extractProductPrefixes name
                , normalizeText p /= normalizeText name
                ]
     in M.union exactIndex prefixIndex

{- | Build name-only supplier index with location for EcoSpold1 linking
Used when exchange has no location attribute to find the activity's actual location
Maps normalizedProductName → (activityUUID, productUUID, activityLocation)
-}
buildSupplierIndexByNameWithLocation :: ActivityMap -> TechFlowDB -> SupplierByNameWithLocation
buildSupplierIndexByNameWithLocation activities techFlowDb =
    M.fromList
        [ (normalizeText (tfName flow), (actUUID, prodUUID, activityLocation act))
        | ((actUUID, prodUUID), act) <- M.toList activities
        , ex <- exchanges act
        , exchangeIsReference ex
        , Just flow <- [M.lookup (exchangeFlowId ex) techFlowDb]
        ]

{- | Fix EcoSpold1 activity links by resolving supplier references.
Matches input exchanges to suppliers by (flowName, location).
Unlinked exchanges stay unlinked so that cross-DB linking can resolve them.
Location aliases map wrongLocation → correctLocation (e.g., "ENTSO" → "ENTSO-E")
-}
fixEcoSpold1ActivityLinks :: M.Map T.Text T.Text -> DatasetNumberIndex -> M.Map UUID.UUID Int -> SimpleDatabase -> IO SimpleDatabase
fixEcoSpold1ActivityLinks locationAliases dsIndex supplierLinks db = do
    -- Build supplier index
    let supplierIndex = buildSupplierIndex (sdbActivities db) (sdbTechFlows db)
    -- Build name-only index with location for exchanges missing location attribute
    let nameIndex = buildSupplierIndexByNameWithLocation (sdbActivities db) (sdbTechFlows db)
    reportProgress Info $
        printf
            "Built supplier index with %d entries for activity linking (%d location aliases, %d name-only entries, %d dataset-number entries)"
            (M.size supplierIndex)
            (M.size locationAliases)
            (M.size nameIndex)
            (M.size dsIndex)

    -- Count and report statistics
    let ctx =
            ExchangeLinkContext
                { elcLocationAliases = locationAliases
                , elcSupplierIndex = supplierIndex
                , elcNameIndex = nameIndex
                , elcDatasetIndex = dsIndex
                , elcSupplierLinks = supplierLinks
                , elcFlowDB = sdbTechFlows db
                }
        (fixedActivities, summary) = fixAllActivities ctx (sdbActivities db)

    reportProgress Info $
        printf
            "Activity linking: %d/%d resolved (%.1f%%), %d unresolved"
            (usFoundLinks summary)
            (usTotalLinks summary)
            (if usTotalLinks summary > 0 then 100.0 * fromIntegral (usFoundLinks summary) / fromIntegral (usTotalLinks summary) else 0.0 :: Double)
            (usMissingLinks summary)

    -- Report grouped summary of unlinked exchanges
    reportUnlinkedSummary summary

    return $ db{sdbActivities = fixedActivities}

{- | Bundle of lookup tables threaded through EcoSpold1 activity-link resolution.
Previously these six fields were passed as positional parameters through
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
    let (fixedExchanges, summaries) = unzip $ map (fixExchangeLink ctx (activityName act)) (exchanges act)
        combinedSummary = mconcat summaries
     in (act{exchanges = fixedExchanges}, combinedSummary)

{- | Fix a single exchange's activity link by (flowName, location) match.

Unlinked exchanges stay unlinked for cross-DB resolution.
Returns (fixed exchange, UnlinkedSummary)
-}
fixExchangeLink :: ExchangeLinkContext -> T.Text -> Exchange -> (Exchange, UnlinkedSummary)
fixExchangeLink ExchangeLinkContext{..} consumerName ex@TechnosphereExchange{techFlowId = fid, techRole = role, techLocation = loc}
    | role == Input || role == ReferenceInput =
        let linked actUUID prodUUID = (ex{techFlowId = prodUUID, techActivityLinkId = actUUID}, UnlinkedSummary M.empty 1 1 0)
            unlinked flow lookupLoc =
                let ue = UnlinkedExchange (tfName flow) lookupLoc
                 in (ex, UnlinkedSummary (M.singleton consumerName [ue]) 1 0 1)
         in case M.lookup fid elcFlowDB of
                Just flow ->
                    -- Tier 1: dataset-number lookup with name validation
                    case M.lookup fid elcSupplierLinks >>= \dsNum -> M.lookup dsNum elcDatasetIndex of
                        Just (actUUID, prodUUID)
                            | Just supplierFlow <- M.lookup prodUUID elcFlowDB
                            , normalizeText (tfName supplierFlow) == normalizeText (tfName flow) ->
                                linked actUUID prodUUID
                        _ ->
                            -- Tier 2: name + location lookup
                            let normalizedLoc = fromMaybe loc (M.lookup loc elcLocationAliases)
                                lookupLoc
                                    | T.null normalizedLoc =
                                        case M.lookup (normalizeText (tfName flow)) elcNameIndex of
                                            Just (_, _, actLoc) -> actLoc
                                            Nothing -> normalizedLoc
                                    | otherwise = normalizedLoc
                                key = (normalizeText (tfName flow), lookupLoc)
                             in case M.lookup key elcSupplierIndex of
                                    Just (actUUID, prodUUID) -> linked actUUID prodUUID
                                    Nothing ->
                                        -- Tier 3: name-only fallback (safe for EcoSpold1 where names include {LOCATION})
                                        case M.lookup (normalizeText (tfName flow)) elcNameIndex of
                                            Just (actUUID, prodUUID, _) -> linked actUUID prodUUID
                                            Nothing -> unlinked flow lookupLoc
                Nothing ->
                    (ex, UnlinkedSummary M.empty 1 0 1)
    | otherwise = (ex, mempty)
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
loadDatabase unitConfig = loadDatabaseWithLocationAliases unitConfig M.empty

{- | Load all EcoSpold files with location aliases
Location aliases map wrongLocation → correctLocation (e.g., "ENTSO" → "ENTSO-E")

The 'UnitConfig' is passed down to parsers so reference-product amounts can be
normalized to the canonical base unit of their dimension at ingest time.
-}
loadDatabaseWithLocationAliases :: UC.UnitConfig -> M.Map T.Text T.Text -> FilePath -> IO (Either T.Text SimpleDatabase)
loadDatabaseWithLocationAliases unitConfig locationAliases path = do
    -- Check if path is a file (SimaPro CSV) or directory (EcoSpold)
    isFile <- doesFileExist path
    isDir <- doesDirectoryExist path

    if isFile
        then case map toLower (takeExtension path) of
            ".csv" -> loadSimaProCSV unitConfig path
            ".xml" -> loadSingleEcoSpold1File locationAliases path
            ".xlsx" -> loadBrightwayExcel unitConfig path
            _ -> return $ Left $ T.pack $ "Unsupported file type: " ++ path
        else
            if isDir
                then do
                    hasProcesses <- doesDirectoryExist (path </> "processes")
                    if hasProcesses
                        then ILCD.parseILCDDirectory path
                        else loadEcoSpoldDirectory locationAliases path
                else return $ Left $ T.pack $ "Path does not exist: " ++ path

-- | Load SimaPro CSV file
loadSimaProCSV :: UC.UnitConfig -> FilePath -> IO (Either T.Text SimpleDatabase)
loadSimaProCSV unitConfig csvPath = do
    (activities, techFlowDB, bioFlowDB, wasteFlowDB, unitDB) <- SimaPro.parseSimaProCSV unitConfig csvPath

    if null activities
        then return $ Left "No activities found in SimaPro CSV file."
        else do
            -- Build ActivityMap with generated ProcessIds
            -- For SimaPro: use the same UUID for both activity and product (like EcoSpold1)
            let procMap =
                    M.fromList
                        [ ((SimaPro.generateActivityUUID act, getReferenceProductUUID act), act)
                        | act <- activities
                        ]

            -- Build initial database
            let simpleDb = SimpleDatabase procMap techFlowDB bioFlowDB wasteFlowDB unitDB

            -- Fix activity links using supplier lookup (same as EcoSpold1)
            Right <$> fixSimaProActivityLinks simpleDb

{- | Load a Brightway Excel (.xlsx) inventory.

Mirrors 'loadSimaProCSV': the parser returns the same 5-tuple, activities are
keyed @(activityUUID, referenceProductUUID)@, and within-file supplier
references are resolved by the shared name-based pass. Cross-database links to a
background database (e.g. ecoinvent) are resolved later by
'fixActivityLinksWithCrossDB', exactly as for SimaPro and EcoSpold.
-}
loadBrightwayExcel :: UC.UnitConfig -> FilePath -> IO (Either T.Text SimpleDatabase)
loadBrightwayExcel unitConfig xlsxPath = do
    parsed <- BrightwayExcel.parseBrightwayExcel unitConfig xlsxPath
    case parsed of
        Left err -> return $ Left err
        Right (activities, techFlowDB, bioFlowDB, wasteFlowDB, unitDB)
            | null activities -> return $ Left "No activities found in Brightway Excel file."
            | otherwise -> do
                let procMap =
                        M.fromList
                            [ ((SimaPro.generateActivityUUID act, getReferenceProductUUID act), act)
                            | act <- activities
                            ]
                    simpleDb = SimpleDatabase procMap techFlowDB bioFlowDB wasteFlowDB unitDB
                Right <$> fixSimaProActivityLinks simpleDb

{- | Fix SimaPro activity links by resolving supplier references
Uses name-only matching (no location required) for SimaPro technosphere inputs
-}
fixSimaProActivityLinks :: SimpleDatabase -> IO SimpleDatabase
fixSimaProActivityLinks db = do
    let nameIndex = buildSupplierIndexByName (sdbActivities db) (sdbTechFlows db)
    reportProgress Info $ printf "Built name-only supplier index with %d entries for SimaPro linking" (M.size nameIndex)

    -- Count and report statistics
    let (fixedActivities, summary) = fixAllActivitiesByName nameIndex (sdbTechFlows db) (sdbActivities db)

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
fixAllActivitiesByName :: NameOnlyIndex -> TechFlowDB -> ActivityMap -> (ActivityMap, UnlinkedSummary)
fixAllActivitiesByName idx techFlowDb activities =
    let results = M.map (fixActivityExchangesByName idx techFlowDb) activities
        summaries = map snd $ M.elems results
        combinedSummary = mconcat summaries
        fixedActivities = M.map fst results
     in (fixedActivities, combinedSummary)

-- | Fix activity exchanges using name-only matching
fixActivityExchangesByName :: NameOnlyIndex -> TechFlowDB -> Activity -> (Activity, UnlinkedSummary)
fixActivityExchangesByName idx techFlowDb act =
    let (fixedExchanges, summaries) = unzip $ map (fixExchangeLinkByName idx techFlowDb (activityName act)) (exchanges act)
        combinedSummary = mconcat summaries
     in (act{exchanges = fixedExchanges}, combinedSummary)

{- | Fix a single exchange's activity link using name-only matching.
Inputs and non-reference outputs (coproducts / avoided-production credits)
are eligible for relinking. Returns (fixed exchange, UnlinkedSummary).
-}
fixExchangeLinkByName :: NameOnlyIndex -> TechFlowDB -> T.Text -> Exchange -> (Exchange, UnlinkedSummary)
fixExchangeLinkByName idx techFlowDb consumerName ex@TechnosphereExchange{techFlowId = fid, techRole = role, techLocation = loc}
    | role == Input || role == ReferenceInput || role == Coproduct =
        case M.lookup fid techFlowDb of
            Just flow ->
                let key = normalizeText (tfName flow)
                    relink actUUID prodUUID = ex{techFlowId = prodUUID, techActivityLinkId = actUUID}
                 in case M.lookup key idx of
                        Just (actUUID, prodUUID) ->
                            (relink actUUID prodUUID, UnlinkedSummary M.empty 1 1 0)
                        Nothing ->
                            let prefixes = extractProductPrefixes (tfName flow)
                                tryPrefix [] = Nothing
                                tryPrefix (p : ps) = case M.lookup (normalizeText p) idx of
                                    Just result -> Just result
                                    Nothing -> tryPrefix ps
                             in case tryPrefix prefixes of
                                    Just (actUUID, prodUUID) ->
                                        (relink actUUID prodUUID, UnlinkedSummary M.empty 1 1 0)
                                    Nothing ->
                                        let unlinked = UnlinkedExchange (tfName flow) loc
                                            unlinkedMap = M.singleton consumerName [unlinked]
                                         in (ex, UnlinkedSummary unlinkedMap 1 0 1)
            Nothing ->
                -- Flow not in technosphere map — shouldn't happen but be safe
                (ex, UnlinkedSummary M.empty 1 0 1)
    | otherwise = (ex, mempty) -- Reference products: nothing to relink
fixExchangeLinkByName _ _ _ ex@BiosphereExchange{} = (ex, mempty)
-- Waste link resolution is deferred to the cross-DB linker path.
fixExchangeLinkByName _ _ _ ex@WasteExchange{} = (ex, mempty)

-- | Load EcoSpold files from directory
loadEcoSpoldDirectory :: M.Map T.Text T.Text -> FilePath -> IO (Either T.Text SimpleDatabase)
loadEcoSpoldDirectory locationAliases dir = do
    reportProgress Info "Scanning directory for EcoSpold files"
    files <- listDirectory dir
    -- Support both EcoSpold2 (.spold) and EcoSpold1 (.XML/.xml) files
    let spold2Files = [dir </> f | f <- files, takeExtension f == ".spold"]
    let spold1Files = [dir </> f | f <- files, map toLower (takeExtension f) == ".xml"]

    -- Determine which format to use based on what's found
    case (spold2Files, spold1Files) of
        ([], []) -> return $ Left $ T.pack $ "No EcoSpold files found in directory: " ++ dir
        ([], [singleXml]) -> do
            -- Single XML file: likely a multi-dataset EcoSpold1 file
            reportProgress Info $ "Found single EcoSpold1 file: " ++ singleXml
            loadSingleEcoSpold1File locationAliases singleXml
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
        results <- mapConcurrently (processWorker startTime isEcoSpold1) (zip [1 ..] workers)

        -- Check for errors from any worker
        let errors = [e | Left e <- results]
        case errors of
            (firstErr : _) -> return $ Left firstErr
            [] -> do
                let successResults = [r | Right r <- results]
                let (procMaps, techFlowMaps, bioFlowMaps, wasteFlowMaps, unitMaps, rawFlowCounts, rawUnitCounts, dsIndexes, supplierLinksLists) = unzip9 successResults
                let !finalProcMap = M.unions procMaps
                let !finalTechFlowMap = M.unionsWith mergeTechFlows techFlowMaps
                let !finalBioFlowMap = M.unionsWith mergeBioFlows bioFlowMaps
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
        -- through (Pattern A: elementaryExchange compartment=inventory indicator/waste;
        -- Pattern B: intermediateExchange classification=By-product:Waste).
        let parseFile =
                if isEcoSpold1
                    then streamParseActivityAndFlowsFromFile1
                    else fmap (fmap (\(a, ts, bs, ws, us) -> (a, ts, bs, ws, us, 0, M.empty))) . streamParseActivityAndFlowsFromFile
        workerResults <- mapM parseFile workerFiles
        let paired = zipWith (\f r -> fmap (f,) r) workerFiles workerResults
        let (errs, oks) = partitionEithers paired
        forM_ errs $ \e ->
            reportProgress Warning e
        let (okFiles, okResults) = unzip oks
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

        case [e | Left e <- procEntries] of
            (firstErr : _) -> return $ Left firstErr
            [] -> do
                let !procMap = M.fromList [e | Right e <- procEntries]
                let !techFlowMap = M.fromListWith mergeTechFlows [(tfId f, f) | f <- allTechs]
                let !bioFlowMap = M.fromListWith mergeBioFlows [(bfId f, f) | f <- allBios]
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
    buildProcEntry True _filepath activity =
        -- EcoSpold1: Generate activity UUID from name and location
        let actUUID = generateActivityUUIDFromActivity activity
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
This handles files where <ecoSpold> contains multiple <dataset> elements
-}
loadSingleEcoSpold1File :: M.Map T.Text T.Text -> FilePath -> IO (Either T.Text SimpleDatabase)
loadSingleEcoSpold1File locationAliases filepath = do
    reportProgress Info "Parsing multi-dataset EcoSpold1 file..."
    results <- streamParseAllDatasetsFromFile1 filepath
    reportProgress Info $ "Parsed " ++ show (length results) ++ " datasets from file"

    -- Build activity map from all parsed activities
    let expanded = map buildProcEntryFromResult results
        !procMap = M.fromList expanded
        !techFlowMap = M.fromListWith mergeTechFlows [(tfId f, f) | (_, techs, _, _, _, _, _) <- results, f <- techs]
        !bioFlowMap = M.fromListWith mergeBioFlows [(bfId f, f) | (_, _, bios, _, _, _, _) <- results, f <- bios]
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
    buildProcEntryFromResult :: (Activity, [TechnosphereFlow], [BiosphereFlow], [WasteFlow], [Unit], Int, M.Map UUID.UUID Int) -> ((UUID.UUID, UUID.UUID), Activity)
    buildProcEntryFromResult (activity, _, _, _, _, _, _) =
        let actUUID = generateActivityUUIDFromActivity activity
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
Validate cache file integrity before attempting to decode.

Checks:
- File size is reasonable (> 1KB to avoid empty/corrupted files)
- File exists and is readable

Returns True if cache file appears valid, False otherwise.
-}
validateCacheFile :: FilePath -> IO Bool
validateCacheFile cacheFile = do
    exists <- doesFileExist cacheFile
    if not exists
        then return False
        else do
            fileSize <- getFileSize cacheFile
            -- Cache file should be at least 1KB for a valid database
            -- Typical size is 100MB-600MB
            if fileSize < 1024
                then do
                    reportCacheOperation $ "Cache file is too small (" ++ show fileSize ++ " bytes), likely corrupted"
                    return False
                else return True

{- |
Load Database with pre-computed matrices from cache (second-tier).

This is the fastest loading method (~0.5s) as it bypasses both
XML parsing and matrix construction. The Database includes:
- All activities, flows, units (from SimpleDatabase)
- Pre-built indexes for fast querying
- Pre-computed sparse matrices (technosphere A, biosphere B)
- Activity and flow UUID mappings for matrix operations

Returns Nothing if no matrix cache exists.
-}
loadCachedDatabaseWithMatrices :: T.Text -> FilePath -> IO (Maybe Database)
loadCachedDatabaseWithMatrices dbName dataDir = do
    cacheFile <- generateMatrixCacheFilename dbName dataDir
    let zstdFile = cacheFile ++ ".zst"
    zstdExists <- doesFileExist zstdFile
    if not zstdExists
        then do
            reportCacheOperation "No matrix cache found"
            return Nothing
        else do
            -- Delegate to the shared reader; a Nothing here means the cache
            -- is corrupted/incompatible and should be rebuilt from source.
            result <- loadCompressedCacheFile zstdFile
            case result of
                Just _ -> return result
                Nothing -> do
                    reportCacheOperation $ "Deleting corrupted cache file: " ++ zstdFile
                    removeFile zstdFile
                    reportCacheOperation "Will rebuild database from source files"
                    return Nothing

{- |
Load Database directly from a specified cache file.

Similar to loadCachedDatabaseWithMatrices but takes an explicit cache file path
instead of generating it from a data directory. Supports both compressed (.bin.zst)
and uncompressed (.bin) formats.

This is useful for deploying just the cache file without the original .spold files.

Returns Nothing if the file cannot be loaded.
-}
loadDatabaseFromCacheFile :: FilePath -> IO (Maybe Database)
loadDatabaseFromCacheFile cacheFile = do
    let ext = takeExtension cacheFile
    let isCompressed = ext == ".zst"

    -- Validate file exists
    fileExists <- doesFileExist cacheFile
    if not fileExists
        then do
            reportError $ "Cache file not found: " ++ cacheFile
            return Nothing
        else do
            if isCompressed
                then loadCompressedCacheFile cacheFile
                else loadUncompressedCacheFile cacheFile

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

-- | Load uncompressed (.bin) cache file
loadUncompressedCacheFile :: FilePath -> IO (Maybe Database)
loadUncompressedCacheFile cacheFile = do
    -- Validate cache file before attempting to decode
    isValid <- validateCacheFile cacheFile
    if not isValid
        then do
            reportCacheOperation "Cache file validation failed"
            return Nothing
        else do
            reportCacheInfo cacheFile
            catch
                ( withProgressTiming Cache "Matrix cache load" $ do
                    !db <- BS.readFile cacheFile >>= \bs -> evaluate (force (decodeEx bs))
                    reportCacheOperation $
                        "Matrix cache loaded: "
                            ++ show (dbActivityCount db)
                            ++ " activities, "
                            ++ show (VU.length $ dbTechnosphereTriples db)
                            ++ " tech entries, "
                            ++ show (VU.length $ dbBiosphereTriples db)
                            ++ " bio entries"
                    return (Just db)
                )
                ( \(e :: SomeException) -> do
                    reportError $ "Cache load failed: " ++ show e
                    reportCacheOperation "The cache file is corrupted or incompatible with the current version"
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
    -- | Location aliases (wrongLocation → correctLocation)
    M.Map T.Text T.Text ->
    -- | Pre-built indexes from other databases
    [IndexedDatabase] ->
    -- | Synonym database for name matching
    SynonymDB ->
    -- | Unit configuration for compatibility checking
    UC.UnitConfig ->
    -- | Location hierarchy (empty = use built-in)
    M.Map T.Text [T.Text] ->
    -- | Geography policy for this database
    GeographyPolicy ->
    -- | Path to load from
    FilePath ->
    IO (Either T.Text (SimpleDatabase, CrossDBLinkingStats))
loadDatabaseWithCrossDBLinking locationAliases otherIndexes synonymDB unitConfig locationHier policy path = do
    result <- loadDatabaseWithLocationAliases unitConfig locationAliases path
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
    M.Map T.Text [T.Text] ->
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
                        , lcSupplierAliases = Nothing
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

-- | Collect unlinked product names from a database (for databases without cross-DB linking)
collectUnlinkedProductNames :: SimpleDatabase -> M.Map T.Text Int
collectUnlinkedProductNames db =
    M.fromListWith
        (+)
        [ (tfName flow, 1)
        | act <- M.elems (sdbActivities db)
        , ex@TechnosphereExchange{techFlowId = fid, techActivityLinkId = linkId} <- exchanges act
        , exchangeIsInput ex
        , linkId == UUID.nil
        , Just flow <- [M.lookup fid (sdbTechFlows db)]
        ]

-- | Count unlinked technosphere exchanges in a database
countUnlinkedExchanges :: SimpleDatabase -> Int
countUnlinkedExchanges db =
    sum
        [ 1
        | act <- M.elems (sdbActivities db)
        , ex <- exchanges act
        , isUnlinkedTechInput ex
        ]
  where
    isUnlinkedTechInput :: Exchange -> Bool
    isUnlinkedTechInput ex@TechnosphereExchange{techActivityLinkId = linkId} =
        exchangeIsInput ex && linkId == UUID.nil
    isUnlinkedTechInput BiosphereExchange{} = False
    -- A waste exchange counts as an unlinked "tech input" only when it is
    -- consumed (treatment side) and has no supplier yet. Waste *outputs*
    -- (the typical SimaPro 'Final waste flows' case) are end-of-life
    -- markers, not demands — they shouldn't inflate the missing-supplier
    -- tally.
    isUnlinkedTechInput ex@WasteExchange{waActivityLinkId = linkId} =
        exchangeIsInput ex && linkId == UUID.nil

-- | Count total technosphere input exchanges in a database
countTotalTechInputs :: SimpleDatabase -> Int
countTotalTechInputs db =
    sum
        [ 1
        | act <- M.elems (sdbActivities db)
        , ex <- exchanges act
        , isTechInput ex
        ]
  where
    isTechInput :: Exchange -> Bool
    isTechInput ex@TechnosphereExchange{} = exchangeIsInput ex
    isTechInput BiosphereExchange{} = False
    -- Mirror isUnlinkedTechInput: only waste *inputs* (treatment side) count.
    isTechInput ex@WasteExchange{} = exchangeIsInput ex

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
    let results = M.mapWithKey (findActivityCrossDBLinks ctx techFlowDb wasteFlowDb unitDb) activities
     in mconcat (M.elems results)

-- | Find cross-database links for one activity's exchanges
findActivityCrossDBLinks ::
    LinkingContext ->
    TechFlowDB ->
    WasteFlowDB ->
    UnitDB ->
    -- | Consumer activity key (actUUID, prodUUID)
    (UUID.UUID, UUID.UUID) ->
    Activity ->
    CrossDBLinkingStats
findActivityCrossDBLinks ctx techFlowDb wasteFlowDb unitDb (consumerActUUID, consumerProdUUID) act =
    let stats = map (findExchangeCrossDBLink ctx techFlowDb wasteFlowDb unitDb consumerActUUID consumerProdUUID) (exchanges act)
     in mconcat stats

{- | Find cross-database link for a single exchange.

Two paths:
* Technosphere inputs whose linkId is nil: use the existing scored matcher
  ('findSupplierAcrossDatabases').
* Orphan waste outputs (waIsInput=False, linkId=nil): strict-match only via
  'findWasteTreatmentAcrossDatabases' — no synonym, no widening; one DB
  wins or stays orphan, multi-DB matches count as ambiguous.
-}
findExchangeCrossDBLink ::
    LinkingContext ->
    TechFlowDB ->
    WasteFlowDB ->
    UnitDB ->
    UUID.UUID ->
    UUID.UUID ->
    Exchange ->
    CrossDBLinkingStats
findExchangeCrossDBLink ctx techFlowDb _wasteFlowDb unitDb consumerActUUID consumerProdUUID ex@TechnosphereExchange{techFlowId = fid, techAmount = amt, techActivityLinkId = linkId, techLocation = loc}
    | exchangeIsInput ex && linkId == UUID.nil =
        case M.lookup fid techFlowDb of
            Nothing -> mempty
            Just flow ->
                let flowUnitName = maybe "" unitName (M.lookup (tfUnitId flow) unitDb)
                 in case findSupplierAcrossDatabases ctx (tfName flow) loc flowUnitName of
                        result@CrossDBLinked{} ->
                            let !crossLink =
                                    CrossDBLink
                                        { cdlConsumerActUUID = consumerActUUID
                                        , cdlConsumerProdUUID = consumerProdUUID
                                        , cdlConsumerFlowId = fid
                                        , cdlSupplierActUUID = cdlrActivityUUID result
                                        , cdlSupplierProdUUID = cdlrProductUUID result
                                        , cdlCoefficient = amt
                                        , cdlExchangeUnit = flowUnitName
                                        , cdlFlowName = cdlrProductName result
                                        , cdlLocation = cdlrLocation result
                                        , cdlSourceDatabase = cdlrDatabaseName result
                                        , cdlTiedAlternatives = cdlrTiedDatabases result
                                        }
                                fallbacks =
                                    [ LocationFallback (cdlrProductName result) req actLoc kind
                                    | UpperLocationUsed req actLoc kind <- cdlrWarnings result
                                    ]
                             in mempty{cdlLinks = [crossLink], cdlLocationFallbacks = fallbacks}
                        CrossDBNotLinked blocker ->
                            let unresolved = case blocker of
                                    LocationRejectedByPolicy req actLoc kind ->
                                        [ LocationUnresolved
                                            (tfName flow)
                                            req
                                            ( "policy rejected "
                                                <> locationKindCode kind
                                                <> " candidate "
                                                <> actLoc
                                            )
                                        ]
                                    LocationUnavailable req ->
                                        [LocationUnresolved (tfName flow) req "no candidate above link threshold"]
                                    NoNameMatch -> []
                                    UnitIncompatible _ _ -> []
                             in mempty
                                    { cdlUnresolvedProducts = M.singleton (tfName flow) (1, blocker)
                                    , cdlLocationUnresolved = unresolved
                                    }
    | otherwise = mempty
findExchangeCrossDBLink _ _ _ _ _ _ BiosphereExchange{} = mempty
-- Cross-DB linking for orphan waste OUTPUTS: strict match only — see
-- 'findWasteTreatmentAcrossDatabases'. No synonym, no fuzzy name match, no
-- location widening. Multi-DB matches stay orphan as 'cdlWasteAmbiguous'.
-- Waste inputs (treatment side) are left alone: they have no clean LCA
-- semantic as a cross-DB demand.
findExchangeCrossDBLink ctx _ wasteFlowDb _ consumerActUUID consumerProdUUID WasteExchange{waFlowId = fid, waAmount = amt, waActivityLinkId = lid, waIsInput = isInp}
    | not isInp && lid == UUID.nil =
        let flowName = maybe "" wfName (M.lookup fid wasteFlowDb)
         in case findWasteTreatmentAcrossDatabases ctx fid flowName of
                WasteMatched entry dbN ->
                    let !crossLink =
                            CrossDBLink
                                { cdlConsumerActUUID = consumerActUUID
                                , cdlConsumerProdUUID = consumerProdUUID
                                , cdlConsumerFlowId = fid
                                , cdlSupplierActUUID = seActivityUUID entry
                                , cdlSupplierProdUUID = seProductUUID entry
                                , cdlCoefficient = amt
                                , cdlExchangeUnit = seUnit entry
                                , cdlFlowName = seProductName entry
                                , cdlLocation = seLocation entry
                                , cdlSourceDatabase = dbN
                                , cdlTiedAlternatives = []
                                }
                     in mempty{cdlLinks = [crossLink], cdlWasteExactLinks = 1}
                WasteAmbiguous _ -> mempty{cdlWasteAmbiguous = 1}
                WasteNoMatch -> mempty{cdlCutoffWasteCount = 1}
    | otherwise = mempty

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
    let !missing = sortOn (\(_, (cnt, _)) -> Down cnt) $ M.toList (cdlUnresolvedProducts stats)
    when (not (null missing)) $ do
        reportProgress Warning $
            printf "Missing suppliers: %d products unresolved" (length missing)
        forM_ (take 20 missing) $ \(name, (cnt, blocker)) ->
            reportProgress Warning $
                printf "  - %s (%d activities) — %s" (T.unpack name) cnt (showBlocker blocker)
        when (length missing > 20) $
            reportProgress Warning $
                printf "  ... and %d more" (length missing - 20)

    -- Unknown units
    let !unknowns = S.toList (cdlUnknownUnits stats)
    when (not (null unknowns)) $
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

showBlocker :: LinkBlocker -> String
showBlocker NoNameMatch = "Not found"
showBlocker (UnitIncompatible q s) = printf "Unit: %s vs %s" (T.unpack q) (T.unpack s)
showBlocker (LocationUnavailable loc) = printf "Location: %s" (T.unpack loc)
showBlocker (LocationRejectedByPolicy req act kind) =
    printf "Rejected by policy: %s → %s (%s)" (T.unpack req) (T.unpack act) (T.unpack (locationKindCode kind))
