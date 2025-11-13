{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module LCA.Query where

import LCA.Progress
import LCA.Types
import Control.Parallel.Strategies
import Data.Int (Int32)
import Data.List (elemIndex, find, partition, sort, sortOn)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import GHC.Generics (Generic)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)

{- | Build complete database with pre-computed sparse matrices

SIGN CONVENTION:
- Technosphere triplets are stored as POSITIVE values (input coefficients per unit output)
- Matrix.hs negates these when constructing (I-A) system matrix for solving
- The biosphere matrix stores ALL flows as POSITIVE (emissions AND resource extractions)
- Resource extractions represent "outputs" from nature into the technosphere (positive like emissions)
- This follows Ecoinvent convention where B matrix contains positive values for all environmental flows

Matrix Construction:
- Accepts a Map with (UUID, UUID) keys and converts to Vector internally
- Builds sparse triplets for technosphere (A) and biosphere (B) matrices
- Normalizes exchanges by NET reference product amounts (gross output - internal consumption)
- SELF-LOOPS (internal consumption) are EXCLUDED from matrix triplets but affect normalization
- Example: Electricity market with 1.0 kWh output and 0.012 kWh internal loss
  * Normalization factor: 1.0 - 0.012 = 0.988 kWh (net output)
  * All inputs divided by 0.988, giving ~1.2% increase in coefficients
  * Self-loop NOT exported as matrix entry (matches Ecoinvent convention)
- Solver constructs (I-A) by adding identity and negating technosphere triplets
-}
buildDatabaseWithMatrices :: M.Map (UUID, UUID) Activity -> FlowDB -> UnitDB -> IO Database
buildDatabaseWithMatrices activityMap flowDB unitDB = do
    reportMatrixOperation "Building database with pre-computed sparse matrices"

    -- Step 1: Build UUID interning tables from Map keys
    let activityKeys = M.keys activityMap
        sortedKeys = sort activityKeys -- Ensure deterministic ordering

        -- Build forward lookup: ProcessId (Int16) -> (UUID, UUID)
        dbProcessIdTable = V.fromList sortedKeys

        -- Build reverse lookup: (UUID, UUID) -> ProcessId (Int16)
        dbProcessIdLookup = M.fromList $ zip sortedKeys [0 ..]

        -- Build activity-product lookup for correct multi-output handling
        -- Maps (activityUUID, productFlowUUID) -> ProcessId
        -- This ensures exchanges link to the correct product in multi-output activities
        activityProductLookup = M.fromList [((actUUID, prodUUID), pid) | (pid, (actUUID, prodUUID)) <- zip [0 ..] sortedKeys]

        -- Convert Map to Vector indexed by ProcessId
        dbActivities = V.fromList [activityMap M.! key | key <- sortedKeys]

        -- Build indexes (now using Vector)
        indexes = buildIndexesWithProcessIds dbActivities dbProcessIdTable flowDB

    -- Build activity index for matrix construction
    reportMatrixOperation "Building activity indexes"
    let activityCount = fromIntegral (V.length dbActivities) :: Int32

    -- Note: ProcessId is already the matrix index (identity mapping removed for performance)
    reportMatrixOperation ("Activity index built: " ++ show activityCount ++ " activities")

    -- Build technosphere sparse triplets
    reportMatrixOperation "Building technosphere matrix triplets"
    let !techTriples =
            let buildTechTriple normalizationFactor j consumerActivity consumerPid ex
                    | not (isTechnosphereExchange ex) = []
                    | not (exchangeIsInput ex) = []
                    | exchangeIsReference ex && not (exchangeIsInput ex) = []
                    | otherwise =
                        let producerPid = case exchangeProcessLinkId ex of
                                Just pid -> Just pid
                                Nothing -> case exchangeActivityLinkId ex of
                                    Just actUUID ->
                                        case M.lookup (actUUID, exchangeFlowId ex) activityProductLookup of
                                            Just pid -> Just pid
                                            Nothing ->
                                                let !_ = unsafePerformIO $ hPutStrLn stderr $
                                                        "[WARNING] Missing activity-product pair referenced by exchange:\n"
                                                        ++ "  Activity UUID: " ++ T.unpack (UUID.toText actUUID) ++ "\n"
                                                        ++ "  Product UUID: " ++ T.unpack (UUID.toText (exchangeFlowId ex)) ++ "\n"
                                                        ++ "  Consumer: " ++ T.unpack (activityName consumerActivity) ++ "\n"
                                                        ++ "  Expected file: " ++ T.unpack (UUID.toText actUUID) ++ "_" ++ T.unpack (UUID.toText (exchangeFlowId ex)) ++ ".spold\n"
                                                        ++ "  This exchange will be skipped."
                                                in Nothing
                                    Nothing -> Nothing
                            -- ProcessId is already the matrix index (no identity mapping needed)
                            producerIdx =
                                producerPid >>= \pid ->
                                    if pid >= 0 && fromIntegral pid < activityCount
                                        then Just $ fromIntegral pid
                                        else Nothing
                         in case producerIdx of
                                Just idx ->
                                    let rawValue = exchangeAmount ex
                                        denom = if normalizationFactor > 1e-15
                                                then normalizationFactor
                                                else error $ "Zero normalization factor for activity at index "
                                                          ++ show j ++ " (activity: "
                                                          ++ T.unpack (activityName consumerActivity) ++ ")"
                                        value = rawValue / denom
                                        -- Exclude self-loops (internal consumption): idx == j
                                        -- Self-loops are accounted for in normalization factor but not exported as matrix entries
                                        -- This matches Ecoinvent's convention where internal losses affect normalization only
                                     in [SparseTriple idx j value | abs value > 1e-15, idx /= j]
                                Nothing -> []

                buildActivityTriplets (j, consumerPid) =
                    let consumerActivity = dbActivities V.! fromIntegral consumerPid
                        -- Get activity UUID from ProcessId table
                        (activityUUID, _) = dbProcessIdTable V.! fromIntegral consumerPid

                        -- For normalization, only use reference OUTPUTS (not treatment inputs)
                        -- Treatment inputs have negative amounts and would incorrectly inflate the normalization factor
                        refOutputs = [ exchangeAmount ex | ex <- exchanges consumerActivity, exchangeIsReference ex, not (exchangeIsInput ex) ]
                        -- If no outputs (pure treatment), use abs of reference input
                        refInputs = [ abs (exchangeAmount ex) | ex <- exchanges consumerActivity, exchangeIsReference ex, exchangeIsInput ex ]

                        -- Calculate internal consumption (self-loops): technosphere inputs that link back to same activity
                        -- These represent internal losses (e.g., electricity market losses, heat for process)
                        internalConsumption = sum [ exchangeAmount ex
                                                  | ex <- exchanges consumerActivity
                                                  , isTechnosphereExchange ex
                                                  , exchangeIsInput ex
                                                  , not (exchangeIsReference ex)  -- Don't count reference products
                                                  , case exchangeActivityLinkId ex of
                                                        Just linkUUID -> linkUUID == activityUUID
                                                        Nothing -> False
                                                  ]

                        normalizationFactor =
                            let grossOutput = sum refOutputs
                                grossInput = sum refInputs
                                -- Net output = gross output - internal consumption
                                -- This matches Ecoinvent's normalization convention for market activities
                                netOutput = if grossOutput > 1e-15
                                           then grossOutput - internalConsumption
                                           else 0.0
                            in if netOutput > 1e-15 then netOutput
                               else if grossInput > 1e-15 then grossInput
                               else 1.0  -- Fallback for activities with no reference products (shouldn't happen)
                        buildNormalizedTechTriple = buildTechTriple normalizationFactor j consumerActivity consumerPid
                     in concatMap buildNormalizedTechTriple (exchanges consumerActivity)

                !result = VU.fromList $ concatMap buildActivityTriplets [(fromIntegral j, j) | j <- [0 .. fromIntegral activityCount - 1]]
             in result

    reportMatrixOperation ("Technosphere matrix: " ++ show (VU.length techTriples) ++ " non-zero entries")

    -- Build biosphere sparse triplets
    reportMatrixOperation "Building biosphere matrix triplets"
    let bioFlowUUIDs =
            V.fromList $ sort $
                S.toList $
                    S.fromList
                        [ exchangeFlowId ex | pid <- [0 .. fromIntegral activityCount - 1], let activity = dbActivities V.! fromIntegral pid, ex <- exchanges activity, isBiosphereExchange ex
                        ]
        bioFlowCount = fromIntegral $ V.length bioFlowUUIDs :: Int32
        bioFlowIndex = M.fromList $ zip (V.toList bioFlowUUIDs) [0 ..]

        !bioTriples =
            let buildBioTriple normalizationFactor j activity ex
                    | not (isBiosphereExchange ex) = []
                    | otherwise =
                        case M.lookup (exchangeFlowId ex) bioFlowIndex of
                            Just i ->
                                let rawValue = exchangeAmount ex
                                    denom = if normalizationFactor > 1e-15
                                            then normalizationFactor
                                            else error $ "Zero normalization factor for biosphere at activity index "
                                                      ++ show j ++ " (activity: "
                                                      ++ T.unpack (activityName activity) ++ ")"
                                    -- Ecoinvent convention: ALL biosphere flows are positive (both emissions AND resource extractions)
                                    -- Resource extractions represent "outputs" from nature into the technosphere
                                    -- NO sign inversion needed - store as positive regardless of input/output status
                                    value = rawValue / denom
                                 in [SparseTriple i j value | abs value > 1e-15]
                            Nothing -> []

                buildActivityBioTriplets (j, pid) =
                    let activity = dbActivities V.! fromIntegral pid
                        -- Get activity UUID from ProcessId table
                        (activityUUID, _) = dbProcessIdTable V.! fromIntegral pid

                        refOutputs = [ exchangeAmount ex | ex <- exchanges activity, exchangeIsReference ex, not (exchangeIsInput ex) ]
                        refInputs = [ abs (exchangeAmount ex) | ex <- exchanges activity, exchangeIsReference ex, exchangeIsInput ex ]

                        -- Calculate internal consumption (self-loops) same as for technosphere matrix
                        internalConsumption = sum [ exchangeAmount ex
                                                  | ex <- exchanges activity
                                                  , isTechnosphereExchange ex
                                                  , exchangeIsInput ex
                                                  , not (exchangeIsReference ex)
                                                  , case exchangeActivityLinkId ex of
                                                        Just linkUUID -> linkUUID == activityUUID
                                                        Nothing -> False
                                                  ]

                        normalizationFactor =
                            let grossOutput = sum refOutputs
                                grossInput = sum refInputs
                                -- Net output = gross output - internal consumption
                                netOutput = if grossOutput > 1e-15
                                           then grossOutput - internalConsumption
                                           else 0.0
                            in if netOutput > 1e-15 then netOutput
                               else if grossInput > 1e-15 then grossInput
                               else 1.0
                        buildNormalizedBioTriple = buildBioTriple normalizationFactor j activity
                     in concatMap buildNormalizedBioTriple (exchanges activity)

                !result = VU.fromList $ concatMap buildActivityBioTriplets [(fromIntegral j, j) | j <- [0 .. fromIntegral activityCount - 1]]
             in result

    reportMatrixOperation ("Biosphere matrix: " ++ show (VU.length bioTriples) ++ " non-zero entries")

    reportMatrixOperation "Database with matrices built successfully"
    reportMatrixOperation ("Final matrix stats: " ++ show (VU.length techTriples) ++ " tech entries, " ++ show (VU.length bioTriples) ++ " bio entries")

    return Database
            { dbProcessIdTable = dbProcessIdTable
            , dbProcessIdLookup = dbProcessIdLookup
            , dbActivities = dbActivities
            , dbFlows = flowDB
            , dbUnits = unitDB
            , dbIndexes = indexes
            , dbTechnosphereTriples = techTriples
            , dbBiosphereTriples = bioTriples
            , dbActivityIndex = V.generate (fromIntegral activityCount) fromIntegral -- Identity mapping for compatibility
            , dbBiosphereFlows = bioFlowUUIDs
            , dbActivityCount = activityCount
            , dbBiosphereCount = bioFlowCount
            , dbCachedFactorization = Nothing
            }

-- | Build indexes with ProcessIds
buildIndexesWithProcessIds :: V.Vector Activity -> V.Vector (UUID, UUID) -> FlowDB -> Indexes
buildIndexesWithProcessIds activityVec processIdTable flowDB =
    let
        -- Convert Vector to temporary Map for index building
        -- We use the ProcessId-to-UUID mapping for lookups
        activityUUIDs = [actUUID | (actUUID, _) <- V.toList processIdTable]
        activities = V.toList activityVec
        activityPairs = zip activityUUIDs activities

        -- Build indexes using activity UUIDs
        nameIdx =
            M.fromListWith
                (++)
                [(T.toLower (activityName activity), [uuid]) | (uuid, activity) <- activityPairs]

        locationIdx =
            M.fromListWith
                (++)
                [(activityLocation activity, [uuid]) | (uuid, activity) <- activityPairs]

        flowIdx =
            M.fromListWith
                (++)
                [ (exchangeFlowId ex, [uuid]) | (uuid, activity) <- activityPairs, ex <- exchanges activity
                ]

        unitIdx =
            M.fromListWith
                (++)
                [(activityUnit activity, [uuid]) | (uuid, activity) <- activityPairs]

        flowCatIdx =
            M.fromListWith
                (++)
                [(flowCategory flow, [flowId]) | (flowId, flow) <- M.toList flowDB]

        flowTypeIdx =
            M.fromListWith
                (++)
                [(flowType flow, [flowId]) | (flowId, flow) <- M.toList flowDB]

        -- Memory optimization: Removed exchange indexes (exchangeIdx, procExchangeIdx, refProdIdx,
        -- inputIdx, outputIdx) that duplicated 600K Exchange records across 5 maps.
        -- Exchanges can be accessed directly from Activity.exchanges when needed.
        -- This saves ~3-4GB of RAM on Ecoinvent 3.12.
     in
        Indexes
            { idxByName = nameIdx
            , idxByLocation = locationIdx
            , idxByFlow = flowIdx
            , idxByUnit = unitIdx
            , idxFlowByCategory = flowCatIdx
            , idxFlowByType = flowTypeIdx
            }

-- | Search activities by multiple fields (name, geography, product)
findActivitiesByFields :: Database -> Maybe Text -> Maybe Text -> Maybe Text -> [Activity]
findActivitiesByFields db nameParam geoParam productParam =
    let activities = V.toList (dbActivities db)
        indexes = dbIndexes db

        -- Filter by name if provided (substring match)
        nameFiltered = case nameParam of
            Nothing -> activities
            Just name ->
                let nameLower = T.toLower name
                 in [a | a <- activities, T.isInfixOf nameLower (T.toLower (activityName a))]

        -- Filter by geography if provided (substring match)
        geoFiltered = case geoParam of
            Nothing -> nameFiltered
            Just geo ->
                let geoLower = T.toLower geo
                 in [a | a <- nameFiltered, T.isInfixOf geoLower (T.toLower (activityLocation a))]

        -- Filter by product if provided (substring match)
        productFiltered = case productParam of
            Nothing -> geoFiltered
            Just product ->
                let productLower = T.toLower product
                 in [ a | a <- geoFiltered, any
                                                ( \ex ->
                                                    exchangeIsReference ex
                                                        && not (exchangeIsInput ex)
                                                        && case M.lookup (exchangeFlowId ex) (dbFlows db) of
                                                            Just flow -> T.isInfixOf productLower (T.toLower (flowName flow))
                                                            Nothing -> False
                                                )
                                                (exchanges a)
                    ]
     in productFiltered

-- | Search flows by synonym
findFlowsBySynonym :: Database -> Text -> [Flow]
findFlowsBySynonym db query =
    let queryLower = T.toLower query
        flows = M.elems (dbFlows db)
     in [ f | f <- flows, T.isInfixOf queryLower (T.toLower (flowName f))
                            || any
                                (\synonyms -> any (T.isInfixOf queryLower . T.toLower) (S.toList synonyms))
                                (M.elems (flowSynonyms f))
        ]
