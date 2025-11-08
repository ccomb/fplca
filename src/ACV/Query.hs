{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ACV.Query where

import ACV.Progress
import ACV.Types
import Control.Parallel.Strategies
import Data.Int (Int32)
import Data.List (elemIndex, find, partition, sort, sortOn)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
import System.IO.Unsafe (unsafePerformIO)

{- | Build complete database with pre-computed sparse matrices

SIGN CONVENTION:
- Technosphere triplets are stored as POSITIVE values (input coefficients per unit output)
- Matrix.hs negates these when constructing (I-A) system matrix for solving
- The biosphere matrix stores emissions as positive, resource extraction as negative
- This follows standard LCA convention where A contains positive input coefficients

Matrix Construction:
- Accepts a Map with (UUID, UUID) keys and converts to Vector internally
- Builds sparse triplets for technosphere (A) and biosphere (B) matrices
- Normalizes exchanges by reference product amounts
- Solver constructs (I-A) by adding identity and negating technosphere triplets
-}
buildDatabaseWithMatrices :: M.Map (UUID, UUID) Activity -> FlowDB -> UnitDB -> Database
buildDatabaseWithMatrices activityMap flowDB unitDB =
    let _ = unsafePerformIO $ reportMatrixOperation "Building database with pre-computed sparse matrices"

        -- Step 1: Build UUID interning tables from Map keys
        activityKeys = M.keys activityMap
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

        referenceProducts = idxReferenceProducts indexes

        -- Build activity index for matrix construction
        _ = unsafePerformIO $ reportMatrixOperation "Building activity indexes"
        activityCount = fromIntegral (V.length dbActivities) :: Int32

        -- Note: ProcessId is already the matrix index (identity mapping removed for performance)
        _ = unsafePerformIO $ reportMatrixOperation ("Activity index built: " ++ show activityCount ++ " activities")

        -- Build technosphere sparse triplets
        _ = unsafePerformIO $ reportMatrixOperation "Building technosphere matrix triplets"
        !techTriples =
            let buildTechTriple normalizationFactor j consumerActivity consumerPid ex
                    | not (isTechnosphereExchange ex) = []
                    | not (exchangeIsInput ex) = []
                    | exchangeIsReference ex && not (exchangeIsInput ex) = []
                    | otherwise =
                        let producerPid = case exchangeProcessLinkId ex of
                                Just pid -> Just pid
                                Nothing -> case exchangeActivityLinkId ex of
                                    Just actUUID ->
                                        M.lookup (actUUID, exchangeFlowId ex) activityProductLookup
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
                                     in [(idx, j, value) | abs value > 1e-15]
                                Nothing -> []

                buildActivityTriplets (j, consumerPid) =
                    let consumerActivity = dbActivities V.! fromIntegral consumerPid
                        refProductAmounts =
                            [ abs (exchangeAmount ex) | ex <- exchanges consumerActivity, exchangeIsReference ex
                            ]
                        normalizationFactor = sum refProductAmounts
                        buildNormalizedTechTriple = buildTechTriple normalizationFactor j consumerActivity consumerPid
                     in concatMap buildNormalizedTechTriple (exchanges consumerActivity)

                !result = V.fromList $ concatMap buildActivityTriplets [(fromIntegral j, j) | j <- [0 .. fromIntegral activityCount - 1]]
                _ = unsafePerformIO $ reportMatrixOperation ("Technosphere matrix: " ++ show (V.length result) ++ " non-zero entries")
             in result

        -- Build biosphere sparse triplets
        _ = unsafePerformIO $ reportMatrixOperation "Building biosphere matrix triplets"
        bioFlowUUIDs =
            sort $
                S.toList $
                    S.fromList
                        [ exchangeFlowId ex | pid <- [0 .. fromIntegral activityCount - 1], let activity = dbActivities V.! fromIntegral pid, ex <- exchanges activity, isBiosphereExchange ex
                        ]
        bioFlowCount = fromIntegral $ length bioFlowUUIDs :: Int32
        bioFlowIndex = M.fromList $ zip bioFlowUUIDs [0 ..]

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
                                    value =
                                        if exchangeIsInput ex
                                            then -(rawValue / denom) -- Resource extraction
                                            else rawValue / denom -- Emissions
                                 in [(i, j, value) | abs value > 1e-15]
                            Nothing -> []

                buildActivityBioTriplets (j, pid) =
                    let activity = dbActivities V.! fromIntegral pid
                        refProductAmounts =
                            [ abs (exchangeAmount ex) | ex <- exchanges activity, exchangeIsReference ex
                            ]
                        normalizationFactor = sum refProductAmounts
                        buildNormalizedBioTriple = buildBioTriple normalizationFactor j activity
                     in concatMap buildNormalizedBioTriple (exchanges activity)

                !result = V.fromList $ concatMap buildActivityBioTriplets [(fromIntegral j, j) | j <- [0 .. fromIntegral activityCount - 1]]
                _ = unsafePerformIO $ reportMatrixOperation ("Biosphere matrix: " ++ show (V.length result) ++ " non-zero entries")
             in result

        _ = techTriples `seq` bioTriples `seq` unsafePerformIO (reportMatrixOperation "Database with matrices built successfully")
        _ = unsafePerformIO $ reportMatrixOperation ("Final matrix stats: " ++ show (V.length techTriples) ++ " tech entries, " ++ show (V.length bioTriples) ++ " bio entries")
     in Database
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

        exchangeIdx =
            M.fromListWith
                (++)
                [ (exchangeFlowId ex, [(uuid, ex)]) | (uuid, activity) <- activityPairs, ex <- exchanges activity
                ]

        procExchangeIdx =
            M.fromListWith
                (++)
                [(uuid, exchanges activity) | (uuid, activity) <- activityPairs]

        refProdIdx =
            M.fromListWith
                (++)
                [ (exchangeFlowId ex, [(uuid, ex)]) | (uuid, activity) <- activityPairs, ex <- exchanges activity, exchangeIsReference ex
                ]

        inputIdx =
            M.fromListWith
                (++)
                [ (uuid, [ex]) | (uuid, activity) <- activityPairs, ex <- exchanges activity, exchangeIsInput ex
                ]

        outputIdx =
            M.fromListWith
                (++)
                [ (uuid, [ex]) | (uuid, activity) <- activityPairs, ex <- exchanges activity, not (exchangeIsInput ex)
                ]
     in
        Indexes
            { idxByName = nameIdx
            , idxByLocation = locationIdx
            , idxByFlow = flowIdx
            , idxByUnit = unitIdx
            , idxFlowByCategory = flowCatIdx
            , idxFlowByType = flowTypeIdx
            , idxExchangeByFlow = exchangeIdx
            , idxExchangeByActivity = procExchangeIdx
            , idxReferenceProducts = refProdIdx
            , idxInputsByActivity = inputIdx
            , idxOutputsByActivity = outputIdx
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
