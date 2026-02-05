{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : EcoSpold.Loader
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
module EcoSpold.Loader (loadAllSpoldsWithFlows, loadAllSpoldsWithLocationAliases, loadCachedDatabaseWithMatrices, saveCachedDatabaseWithMatrices, loadDatabaseFromCacheFile) where

import qualified Codec.Compression.Zstd as Zstd
import Control.Concurrent.Async
import Control.DeepSeq (force)
import Control.Exception (SomeException, catch, evaluate)
import Control.Monad
import Data.Binary (decode, encode)
import Data.Bits (xor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Char (toLower)
import Data.Hashable (hash)
import Data.List (sortBy, group, sort)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Data.Typeable (typeOf, typeRepFingerprint)
import qualified Data.UUID as UUID
import qualified Data.UUID.V5 as UUID5
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word64)
import EcoSpold.Parser (streamParseActivityAndFlowsFromFile)
import EcoSpold.Parser1 (streamParseActivityAndFlowsFromFile1, streamParseAllDatasetsFromFile1)
import GHC.Conc (getNumCapabilities)
import GHC.Fingerprint (Fingerprint (..))
import LCA.Progress
import LCA.Types
import qualified SimaPro.Parser as SimaPro
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getFileSize, listDirectory, removeFile)
import LCA.UploadedDatabase (getDataDir, isUploadedPath)
import System.FilePath (takeBaseName, takeExtension, splitDirectories, (</>))
import Text.Printf (printf)

-- | Magic bytes to identify fpLCA cache files
cacheMagic :: BS.ByteString
cacheMagic = "FPLCACHE"

{- |
Schema signature automatically derived from the Database type structure.

This signature changes when:
- Fields are added/removed from Database or nested types
- Type names change
- Type structure changes

The signature is stored inside the cache file and checked on load.
If it doesn't match, the cache is automatically invalidated and rebuilt.
-}
schemaSignature :: Word64
schemaSignature =
    let Fingerprint hi lo = typeRepFingerprint (typeOf (undefined :: Database))
     in hi `xor` lo

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

-- | Information about an unlinked technosphere exchange
data UnlinkedExchange = UnlinkedExchange
    { ueFlowName :: !T.Text
    , ueLocation :: !T.Text
    } deriving (Eq, Ord, Show)

-- | Summary of unlinked exchanges grouped by consumer activity
data UnlinkedSummary = UnlinkedSummary
    { usActivities :: !(M.Map T.Text [UnlinkedExchange])  -- consumer name → list of unlinked exchanges
    , usTotalLinks :: !Int
    , usFoundLinks :: !Int
    , usMissingLinks :: !Int
    } deriving (Show)

-- | Empty unlinked summary
emptyUnlinkedSummary :: UnlinkedSummary
emptyUnlinkedSummary = UnlinkedSummary M.empty 0 0 0

-- | Merge two unlinked summaries
mergeUnlinkedSummaries :: UnlinkedSummary -> UnlinkedSummary -> UnlinkedSummary
mergeUnlinkedSummaries s1 s2 = UnlinkedSummary
    { usActivities = M.unionWith (++) (usActivities s1) (usActivities s2)
    , usTotalLinks = usTotalLinks s1 + usTotalLinks s2
    , usFoundLinks = usFoundLinks s1 + usFoundLinks s2
    , usMissingLinks = usMissingLinks s1 + usMissingLinks s2
    }

-- | Report grouped summary of unlinked exchanges
reportUnlinkedSummary :: UnlinkedSummary -> IO ()
reportUnlinkedSummary summary
    | M.null (usActivities summary) = return ()  -- Nothing to report
    | otherwise = do
        let activities = usActivities summary
            activityCount = M.size activities
            -- Sort activities by number of unlinked exchanges (descending)
            sortedActivities = take 10 $ reverse $ sortOn (length . snd) $ M.toList activities
            remainingCount = activityCount - length sortedActivities

        reportProgress Warning $
            printf "Unlinked activities: %d activities affected" activityCount

        -- Report top activities with their missing suppliers
        forM_ sortedActivities $ \(actName, unlinkedExchanges) -> do
            let uniqueExchanges = nub unlinkedExchanges  -- Remove duplicates
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
                reportProgress Warning $ printf "      ... and %d more" remainingFlows

        when (remainingCount > 0) $
            reportProgress Warning $ printf "  ... and %d more activities" remainingCount
  where
    sortOn f = sortBy (\a b -> compare (f a) (f b))
    nub = map head . group . sort

-- | Normalize text for matching: lowercase and strip whitespace
normalizeText :: T.Text -> T.Text
normalizeText = T.toLower . T.strip

{- | Build supplier index: (normalizedProductName, location) → (activityUUID, productUUID)
For each activity, we index it by its reference product name + activity location
-}
buildSupplierIndex :: ActivityMap -> FlowDB -> SupplierIndex
buildSupplierIndex activities flowDb =
    M.fromList
        [ ((normalizeText (flowName flow), activityLocation act), (actUUID, prodUUID))
        | ((actUUID, prodUUID), act) <- M.toList activities
        , ex <- exchanges act
        , exchangeIsReference ex
        , Just flow <- [M.lookup (exchangeFlowId ex) flowDb]
        ]

{- | Build name-only supplier index for SimaPro linking
Uses only the normalized product name (no location required)
-}
buildSupplierIndexByName :: ActivityMap -> FlowDB -> NameOnlyIndex
buildSupplierIndexByName activities flowDb =
    M.fromList
        [ (normalizeText (flowName flow), (actUUID, prodUUID))
        | ((actUUID, prodUUID), act) <- M.toList activities
        , ex <- exchanges act
        , exchangeIsReference ex
        , Just flow <- [M.lookup (exchangeFlowId ex) flowDb]
        ]

{- | Build name-only supplier index with location for EcoSpold1 linking
Used when exchange has no location attribute to find the activity's actual location
Maps normalizedProductName → (activityUUID, productUUID, activityLocation)
-}
buildSupplierIndexByNameWithLocation :: ActivityMap -> FlowDB -> SupplierByNameWithLocation
buildSupplierIndexByNameWithLocation activities flowDb =
    M.fromList
        [ (normalizeText (flowName flow), (actUUID, prodUUID, activityLocation act))
        | ((actUUID, prodUUID), act) <- M.toList activities
        , ex <- exchanges act
        , exchangeIsReference ex
        , Just flow <- [M.lookup (exchangeFlowId ex) flowDb]
        ]

{- | Fix EcoSpold1 activity links by resolving supplier references
Uses (flowName, flowLocation) to look up the correct supplier activity
Location aliases map wrongLocation → correctLocation (e.g., "ENTSO" → "ENTSO-E")
When exchange has no location, uses name-only lookup to find the activity's location
-}
fixEcoSpold1ActivityLinks :: M.Map T.Text T.Text -> SimpleDatabase -> IO SimpleDatabase
fixEcoSpold1ActivityLinks locationAliases db = do
    -- Build supplier index
    let supplierIndex = buildSupplierIndex (sdbActivities db) (sdbFlows db)
    -- Build name-only index with location for exchanges missing location attribute
    let nameIndex = buildSupplierIndexByNameWithLocation (sdbActivities db) (sdbFlows db)
    reportProgress Info $
        printf
            "Built supplier index with %d entries for activity linking (%d location aliases, %d name-only entries)"
            (M.size supplierIndex)
            (M.size locationAliases)
            (M.size nameIndex)

    -- Count and report statistics
    let (fixedActivities, summary) = fixAllActivities locationAliases supplierIndex nameIndex (sdbFlows db) (sdbActivities db)

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

-- | Fix all activities and return statistics with unlinked summary
fixAllActivities :: M.Map T.Text T.Text -> SupplierIndex -> SupplierByNameWithLocation -> FlowDB -> ActivityMap -> (ActivityMap, UnlinkedSummary)
fixAllActivities locationAliases idx nameIdx flowDb activities =
    let results = M.map (\act -> fixActivityExchanges locationAliases idx nameIdx flowDb act) activities
        summaries = map snd $ M.elems results
        combinedSummary = foldr mergeUnlinkedSummaries emptyUnlinkedSummary summaries
        fixedActivities = M.map fst results
     in (fixedActivities, combinedSummary)

-- | Fix activity exchanges and return (fixed activity, UnlinkedSummary)
fixActivityExchanges :: M.Map T.Text T.Text -> SupplierIndex -> SupplierByNameWithLocation -> FlowDB -> Activity -> (Activity, UnlinkedSummary)
fixActivityExchanges locationAliases idx nameIdx flowDb act =
    let (fixedExchanges, summaries) = unzip $ map (fixExchangeLink locationAliases idx nameIdx flowDb (activityName act)) (exchanges act)
        combinedSummary = foldr mergeUnlinkedSummaries emptyUnlinkedSummary summaries
     in (act{exchanges = fixedExchanges}, combinedSummary)

{- | Fix a single exchange's activity link
Returns (fixed exchange, UnlinkedSummary)
Location aliases map wrongLocation → correctLocation (e.g., "ENTSO" → "ENTSO-E")
When exchange has no location, uses name-only lookup to find the activity's location
-}
fixExchangeLink :: M.Map T.Text T.Text -> SupplierIndex -> SupplierByNameWithLocation -> FlowDB -> T.Text -> Exchange -> (Exchange, UnlinkedSummary)
fixExchangeLink locationAliases idx nameIdx flowDb consumerName ex@(TechnosphereExchange fid amt uid isInp isRef _ procLink loc)
    | isInp -- Fix all technosphere inputs (with or without location)
        =
        case M.lookup fid flowDb of
            Just flow ->
                -- Normalize location using aliases (e.g., "ENTSO" → "ENTSO-E")
                let normalizedLoc = fromMaybe loc (M.lookup loc locationAliases)
                    -- Determine final lookup location:
                    -- If no location on exchange, try name-only lookup to get activity's location
                    lookupLoc
                        | T.null normalizedLoc =
                            case M.lookup (normalizeText (flowName flow)) nameIdx of
                                Just (_, _, actLoc) -> actLoc  -- Use activity's location
                                Nothing -> normalizedLoc       -- Fall back (empty)
                        | otherwise = normalizedLoc            -- Has location, use it
                    key = (normalizeText (flowName flow), lookupLoc)
                 in case M.lookup key idx of
                        Just (actUUID, prodUUID) ->
                            -- Found supplier: update both activityLinkId AND flowId to match supplier's reference product
                            -- This is critical because the matrix lookup uses (activityLinkId, flowId) as the key
                            (TechnosphereExchange prodUUID amt uid isInp isRef actUUID procLink loc, UnlinkedSummary M.empty 1 1 0)
                        Nothing ->
                            -- Supplier not found - collect unlinked exchange info
                            let unlinked = UnlinkedExchange (flowName flow) lookupLoc
                                unlinkedMap = M.singleton consumerName [unlinked]
                             in (ex, UnlinkedSummary unlinkedMap 1 0 1)
            Nothing ->
                -- Flow not in database - shouldn't happen but be safe
                (ex, UnlinkedSummary M.empty 1 0 1)
    | otherwise = (ex, emptyUnlinkedSummary) -- Not a linkable exchange (outputs/references)
fixExchangeLink _ _ _ _ _ ex = (ex, emptyUnlinkedSummary) -- BiosphereExchange - no linking needed

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
loadAllSpoldsWithFlows :: FilePath -> IO (Either T.Text SimpleDatabase)
loadAllSpoldsWithFlows = loadAllSpoldsWithLocationAliases M.empty

{- | Load all EcoSpold files with location aliases
Location aliases map wrongLocation → correctLocation (e.g., "ENTSO" → "ENTSO-E")
-}
loadAllSpoldsWithLocationAliases :: M.Map T.Text T.Text -> FilePath -> IO (Either T.Text SimpleDatabase)
loadAllSpoldsWithLocationAliases locationAliases path = do
    -- Check if path is a file (SimaPro CSV) or directory (EcoSpold)
    isFile <- doesFileExist path
    isDir <- doesDirectoryExist path

    if isFile && map toLower (takeExtension path) == ".csv"
        then loadSimaProCSV path
        else
            if isDir
                then loadEcoSpoldDirectory locationAliases path
                else return $ Left $ T.pack $ "Path is neither a CSV file nor a directory: " ++ path

-- | Load SimaPro CSV file
loadSimaProCSV :: FilePath -> IO (Either T.Text SimpleDatabase)
loadSimaProCSV csvPath = do
    (activities, flowDB, unitDB) <- SimaPro.parseSimaProCSV csvPath

    -- Build ActivityMap with generated ProcessIds
    -- For SimaPro: use the same UUID for both activity and product (like EcoSpold1)
    let activityList = zip [0 ..] activities
        procMap =
            M.fromList
                [ ((SimaPro.generateActivityUUID (activityName act), getReferenceProductUUID act), act)
                | (_, act) <- activityList
                ]

    -- Build initial database
    let simpleDb = SimpleDatabase procMap flowDB unitDB

    -- Fix activity links using supplier lookup (same as EcoSpold1)
    Right <$> fixSimaProActivityLinks simpleDb

{- | Fix SimaPro activity links by resolving supplier references
Uses name-only matching (no location required) for SimaPro technosphere inputs
-}
fixSimaProActivityLinks :: SimpleDatabase -> IO SimpleDatabase
fixSimaProActivityLinks db = do
    let nameIndex = buildSupplierIndexByName (sdbActivities db) (sdbFlows db)
    reportProgress Info $ printf "Built name-only supplier index with %d entries for SimaPro linking" (M.size nameIndex)

    -- Count and report statistics
    let (fixedActivities, summary) = fixAllActivitiesByName nameIndex (sdbFlows db) (sdbActivities db)

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
fixAllActivitiesByName :: NameOnlyIndex -> FlowDB -> ActivityMap -> (ActivityMap, UnlinkedSummary)
fixAllActivitiesByName idx flowDb activities =
    let results = M.map (\act -> fixActivityExchangesByName idx flowDb act) activities
        summaries = map snd $ M.elems results
        combinedSummary = foldr mergeUnlinkedSummaries emptyUnlinkedSummary summaries
        fixedActivities = M.map fst results
     in (fixedActivities, combinedSummary)

-- | Fix activity exchanges using name-only matching
fixActivityExchangesByName :: NameOnlyIndex -> FlowDB -> Activity -> (Activity, UnlinkedSummary)
fixActivityExchangesByName idx flowDb act =
    let (fixedExchanges, summaries) = unzip $ map (fixExchangeLinkByName idx flowDb (activityName act)) (exchanges act)
        combinedSummary = foldr mergeUnlinkedSummaries emptyUnlinkedSummary summaries
     in (act{exchanges = fixedExchanges}, combinedSummary)

{- | Fix a single exchange's activity link using name-only matching
Returns (fixed exchange, UnlinkedSummary)
-}
fixExchangeLinkByName :: NameOnlyIndex -> FlowDB -> T.Text -> Exchange -> (Exchange, UnlinkedSummary)
fixExchangeLinkByName idx flowDb consumerName ex@(TechnosphereExchange fid amt uid isInp isRef _ procLink loc)
    | isInp -- All technosphere inputs (no location check!)
        =
        case M.lookup fid flowDb of
            Just flow ->
                let key = normalizeText (flowName flow)
                 in case M.lookup key idx of
                        Just (actUUID, prodUUID) ->
                            -- Found supplier: update both activityLinkId AND flowId to match supplier's reference product
                            (TechnosphereExchange prodUUID amt uid isInp isRef actUUID procLink loc, UnlinkedSummary M.empty 1 1 0)
                        Nothing ->
                            -- Supplier not found - collect unlinked exchange info
                            let unlinked = UnlinkedExchange (flowName flow) loc
                                unlinkedMap = M.singleton consumerName [unlinked]
                             in (ex, UnlinkedSummary unlinkedMap 1 0 1)
            Nothing ->
                -- Flow not in database - shouldn't happen but be safe
                (ex, UnlinkedSummary M.empty 1 0 1)
    | otherwise = (ex, emptyUnlinkedSummary) -- Not a linkable exchange (outputs/references)
fixExchangeLinkByName _ _ _ ex = (ex, emptyUnlinkedSummary) -- BiosphereExchange - no linking needed

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
            loadWithWorkerParallelism xs False  -- Prefer EcoSpold2 if both present
  where
    -- Helper function for unzipping 5-tuples
    unzip5 :: [(a, b, c, d, e)] -> ([a], [b], [c], [d], [e])
    unzip5 = foldr (\(a, b, c, d, e) (as, bs, cs, ds, es) -> (a : as, b : bs, c : cs, d : ds, e : es)) ([], [], [], [], [])

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
            (firstErr:_) -> return $ Left firstErr
            [] -> do
                let successResults = [r | Right r <- results]
                let (procMaps, flowMaps, unitMaps, rawFlowCounts, rawUnitCounts) = unzip5 successResults
                let !finalProcMap = M.unions procMaps
                let !finalFlowMap = M.unions flowMaps
                let !finalUnitMap = M.unions unitMaps

                endTime <- getCurrentTime
                let totalDuration = realToFrac $ diffUTCTime endTime startTime
                let totalFiles = length allFiles
                let avgFilesPerSec = fromIntegral totalFiles / totalDuration
                let totalRawFlows = sum rawFlowCounts
                let totalRawUnits = sum rawUnitCounts
                let flowDeduplication = if totalRawFlows > 0 then 100.0 * (1.0 - fromIntegral (M.size finalFlowMap) / fromIntegral totalRawFlows) else 0.0 :: Double
                let unitDeduplication = if totalRawUnits > 0 then 100.0 * (1.0 - fromIntegral (M.size finalUnitMap) / fromIntegral totalRawUnits) else 0.0 :: Double

                reportProgress Info $ printf "Parsing completed (%s, %.1f files/sec):" (formatDuration totalDuration) avgFilesPerSec
                reportProgress Info $ printf "  Activities: %d processes" (M.size finalProcMap)
                reportProgress Info $
                    printf
                        "  Flows: %d unique (%.1f%% deduplication from %d raw)"
                        (M.size finalFlowMap)
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
                let simpleDb = SimpleDatabase finalProcMap finalFlowMap finalUnitMap
                if isEcoSpold1
                    then Right <$> fixEcoSpold1ActivityLinks locationAliases simpleDb
                    else return $ Right simpleDb

    -- Distribute files evenly among N workers
    distributeFiles :: Int -> [a] -> [[a]]
    distributeFiles n xs =
        let len = length xs
            baseSize = len `div` n
            remainder = len `mod` n
            -- First 'remainder' workers get baseSize+1 files, rest get baseSize
            sizes = replicate remainder (baseSize + 1) ++ replicate (n - remainder) baseSize
         in distribute sizes xs
      where
        distribute [] _ = []
        distribute _ [] = []
        distribute (s : ss) items = take s items : distribute ss (drop s items)

    -- Process one worker's share of files
    processWorker :: UTCTime -> Bool -> (Int, [FilePath]) -> IO (Either T.Text (ActivityMap, FlowDB, UnitDB, Int, Int))
    processWorker startTime isEcoSpold1 (workerNum, workerFiles) = do
        workerStartTime <- getCurrentTime
        reportProgress Info $ printf "Worker %d started: processing %d files" workerNum (length workerFiles)

        -- Parse all files for this worker using appropriate parser
        let parseFile =
                if isEcoSpold1
                    then streamParseActivityAndFlowsFromFile1
                    else streamParseActivityAndFlowsFromFile
        workerResults <- mapM parseFile workerFiles
        let (!procs, flowLists, unitLists) = unzip3 workerResults
        let !allFlows = concat flowLists
        let !allUnits = concat unitLists

        -- Build maps for this worker - extract UUID pairs from filenames
        -- For EcoSpold1: generate UUIDs from numeric dataset number
        -- For EcoSpold2: parse UUIDs from filename (activityUUID_productUUID.spold)
        let procEntries = zipWith (buildProcEntry isEcoSpold1) workerFiles procs

        -- Check for any filename parsing errors
        case [e | Left e <- procEntries] of
            (firstErr:_) -> return $ Left firstErr
            [] -> do
                let !procMap = M.fromList [e | Right e <- procEntries]
                let !flowMap = M.fromList [(flowId f, f) | f <- allFlows]
                let !unitMap = M.fromList [(unitId u, u) | u <- allUnits]

                workerEndTime <- getCurrentTime
                let workerDuration = realToFrac $ diffUTCTime workerEndTime workerStartTime
                let filesPerSec = fromIntegral (length workerFiles) / workerDuration
                let rawFlowCount = length allFlows
                let rawUnitCount = length allUnits
                reportProgress Info $
                    printf
                        "Worker %d completed: %d activities, %d flows (%s, %.1f files/sec)"
                        workerNum
                        (M.size procMap)
                        (M.size flowMap)
                        (formatDuration workerDuration)
                        filesPerSec

                return $ Right (procMap, flowMap, unitMap, rawFlowCount, rawUnitCount)

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

-- | Load a single EcoSpold1 file containing multiple datasets
-- This handles files where <ecoSpold> contains multiple <dataset> elements
loadSingleEcoSpold1File :: M.Map T.Text T.Text -> FilePath -> IO (Either T.Text SimpleDatabase)
loadSingleEcoSpold1File locationAliases filepath = do
    reportProgress Info "Parsing multi-dataset EcoSpold1 file..."
    results <- streamParseAllDatasetsFromFile1 filepath
    reportProgress Info $ "Parsed " ++ show (length results) ++ " datasets from file"

    -- Build activity map from all parsed activities
    let procEntries = map buildProcEntryFromResult results
        !procMap = M.fromList procEntries
        !flowMap = M.fromList [(flowId f, f) | (_, flows, _) <- results, f <- flows]
        !unitMap = M.fromList [(unitId u, u) | (_, _, units) <- results, u <- units]
        simpleDb = SimpleDatabase procMap flowMap unitMap

    -- Report statistics
    let totalFlows = sum [length flows | (_, flows, _) <- results]
    let totalUnits = sum [length units | (_, _, units) <- results]
    reportProgress Info $ printf "  Activities: %d processes" (M.size procMap)
    reportProgress Info $ printf "  Flows: %d unique (from %d raw)" (M.size flowMap) totalFlows
    reportProgress Info $ printf "  Units: %d unique (from %d raw)" (M.size unitMap) totalUnits

    -- Fix activity links using supplier lookup
    Right <$> fixEcoSpold1ActivityLinks locationAliases simpleDb
  where
    buildProcEntryFromResult :: (Activity, [Flow], [Unit]) -> ((UUID.UUID, UUID.UUID), Activity)
    buildProcEntryFromResult (activity, _, _) =
        let actUUID = generateActivityUUIDFromActivity activity
            prodUUID = getReferenceProductUUID activity
        in ((actUUID, prodUUID), activity)

{- |
Generate filename for matrix cache.

Matrix caches store pre-computed sparse matrices (technosphere A,
biosphere B) enabling direct LCA solving without matrix construction.

Cache location depends on database source:
- Uploaded databases (path starts with "uploads/"): cache in the upload directory
- Configured databases: cache in "cache/" subdirectory

Cache invalidation is handled by a schema signature stored inside
the cache file, not by the filename.
-}
generateMatrixCacheFilename :: T.Text -> FilePath -> IO FilePath
generateMatrixCacheFilename dbName dataPath = do
    let cacheFilename = "fplca.cache." ++ T.unpack dbName ++ ".bin"
    -- Check if this is an uploaded database
    if isUploadedPath dataPath
        then do
            -- For uploads, extract the upload directory (first two components: .../uploads/<slug>)
            let uploadDir = getUploadDir dataPath
            return $ uploadDir </> cacheFilename
        else do
            -- For configured databases, use cache/ subdirectory under data dir
            base <- getDataDir
            let cacheDir = base </> "cache"
            createDirectoryIfMissing True cacheDir
            return $ cacheDir </> cacheFilename
  where
    -- Extract upload directory from data path
    -- e.g., ".../uploads/ecoinvent-3-11/datasets" -> take up to and including the slug after "uploads"
    getUploadDir :: FilePath -> FilePath
    getUploadDir p =
        let parts = splitDirectories p
            takeUntilUploadsSlug [] = []
            takeUntilUploadsSlug ("uploads" : slug : _) = ["uploads", slug]
            takeUntilUploadsSlug (x : xs) = x : takeUntilUploadsSlug xs
        in foldl (</>) "" (takeUntilUploadsSlug parts)

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
            reportCacheInfo zstdFile
            -- Wrap cache loading in exception handler to prevent crashes
            catch
                ( withProgressTiming Cache "Matrix cache load with zstd decompression" $ do
                    contents <- BS.readFile zstdFile
                    -- Check header: magic (8 bytes) + signature (8 bytes)
                    let headerSize = 16
                    if BS.length contents < headerSize
                        then do
                            reportCacheOperation "Cache file too small, rebuilding"
                            removeFile zstdFile
                            return Nothing
                        else do
                            let (header, payload) = BS.splitAt headerSize contents
                            let (magic, sigBytes) = BS.splitAt 8 header
                            -- Check magic bytes
                            if magic /= cacheMagic
                                then do
                                    reportCacheOperation "Cache file has invalid magic (old format?), rebuilding"
                                    removeFile zstdFile
                                    return Nothing
                                else do
                                    -- Check schema signature
                                    let storedSig = decode (BSL.fromStrict sigBytes) :: Word64
                                    if storedSig /= schemaSignature
                                        then do
                                            reportCacheOperation $ "Schema signature mismatch (stored: " ++ show storedSig ++ ", current: " ++ show schemaSignature ++ "), rebuilding"
                                            removeFile zstdFile
                                            return Nothing
                                        else do
                                            -- Decompress and decode
                                            case Zstd.decompress payload of
                                                Zstd.Skip -> do
                                                    reportError "Zstd decompression failed: Skip"
                                                    return Nothing
                                                Zstd.Error err -> do
                                                    reportError $ "Zstd decompression failed: " ++ show err
                                                    return Nothing
                                                Zstd.Decompress decompressed -> do
                                                    let !db = decode (BSL.fromStrict decompressed)
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
                    reportError $ "Cache load failed: " ++ show e
                    reportCacheOperation "The cache file is corrupted or incompatible"
                    reportCacheOperation $ "Deleting corrupted cache file: " ++ zstdFile
                    removeFile zstdFile
                    reportCacheOperation "Will rebuild database from source files"
                    return Nothing
                )

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
                            let storedSig = decode (BSL.fromStrict sigBytes) :: Word64
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
                                            let !db = decode (BSL.fromStrict decompressed)
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
                    !db <- BSL.readFile cacheFile >>= \bs -> evaluate (force (decode bs))
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
- 8 bytes magic ("FPLCACHE")
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
        -- Serialize to ByteString
        let serialized = encode db
        -- Compress with zstd (level 3 = good balance of compression and speed)
        let compressed = Zstd.compress 3 (BSL.toStrict serialized)
        -- Build header: magic (8 bytes) + schema signature (8 bytes)
        let signatureBytes = BSL.toStrict $ encode schemaSignature
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
