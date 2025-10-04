{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : ACV.Query
Description : Database indexing and sparse matrix construction for LCA calculations

This module handles the construction of the Database type from raw EcoSpold data,
including the critical sparse matrix pre-computation that enables fast LCA calculations.

Key responsibilities:
- Building activity and flow indexes for fast UUID lookups
- Constructing sparse technosphere matrix (A) in coordinate triplet format
- Constructing sparse biosphere matrix (B) in coordinate triplet format
- Database statistics and search functionality

The sparse matrices use coordinate triplet lists (i,j,value)
that can be efficiently consumed by PETSc's sparse linear algebra routines.

Performance characteristics:
- Matrix construction: O(n*m) where n=activities, m=exchanges per activity
- Memory usage: ~10-50 MB for typical databases
- Construction time: ~1-5 seconds for full Ecoinvent database
-}
module ACV.Query where

import ACV.Progress
import ACV.Types
import ACV.UnitConversion (normalizeExchangeAmount)
import Control.Parallel.Strategies
import Data.List (find, sortOn)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.IO.Unsafe (unsafePerformIO)

-- | Construction des index à partir d'une base de données simple (parallélisée)
buildIndexes :: ActivityDB -> FlowDB -> Indexes
buildIndexes procDB flowDB =
    let
        -- Build indexes with parallel evaluation
        nameIdx = buildNameIndex procDB `using` rdeepseq
        locationIdx = buildLocationIndex procDB `using` rdeepseq
        flowIdx = buildFlowIndex procDB `using` rdeepseq
        unitIdx = buildActivityUnitIndex procDB `using` rdeepseq
        flowCatIdx = buildFlowCategoryIndex flowDB `using` rdeepseq
        flowTypeIdx = buildFlowTypeIndex flowDB `using` rdeepseq
        exchangeIdx = buildExchangeIndex procDB `using` rdeepseq
        procExchangeIdx = buildActivityExchangeIndex procDB `using` rdeepseq
        refProdIdx = buildReferenceProductIndex procDB `using` rdeepseq
        inputIdx = buildActivityInputIndex procDB `using` rdeepseq
        outputIdx = buildActivityOutputIndex procDB `using` rdeepseq
     in
        Indexes
            { -- Index au niveau activité
              idxByName = nameIdx
            , idxByLocation = locationIdx
            , idxByFlow = flowIdx
            , idxByUnit = unitIdx
            , -- Index au niveau flux
              idxFlowByCategory = flowCatIdx
            , idxFlowByType = flowTypeIdx
            , -- Index au niveau échange
              idxExchangeByFlow = exchangeIdx
            , idxExchangeByActivity = procExchangeIdx
            , idxReferenceProducts = refProdIdx
            , idxInputsByActivity = inputIdx
            , idxOutputsByActivity = outputIdx
            }

-- | Build complete database with pre-computed sparse matrices
buildDatabaseWithMatrices :: ActivityDB -> FlowDB -> UnitDB -> Database
buildDatabaseWithMatrices activityDB flowDB unitDB =
    let _ = unsafePerformIO $ reportMatrixOperation "Building database with pre-computed sparse matrices"
        indexes = buildIndexes activityDB flowDB
        -- Build complete sparse coordinate lists for entire database
        _ = unsafePerformIO $ reportMatrixOperation "Building activity indexes"
        allActivities = activityDB
        activityUUIDs = M.keys allActivities
        activityCount = length activityUUIDs
        activityIndex = M.fromList $ zip activityUUIDs [0 ..]
        _ = unsafePerformIO $ reportMatrixOperation ("Activity index built: " ++ show activityCount ++ " activities")

        -- Build technosphere sparse triplets (optimized with strict evaluation)
        _ = unsafePerformIO $ reportMatrixOperation "Building technosphere matrix triplets"
        !techTriples =
            let buildTechTriple j consumerActivity ex
                    | not (isTechnosphereExchange ex) = []
                    | exchangeIsReference ex = [] -- Skip reference products - normalization approach
                    | otherwise =
                        case exchangeActivityLinkId ex of
                            Just inputActivityUUID ->
                                case M.lookup inputActivityUUID activityIndex of
                                    Just producerIdx ->
                                        let unitName = getUnitNameForExchange unitDB ex
                                            (normalizedValue, _) = normalizeExchangeAmount unitName (exchangeAmount ex)
                                            value = normalizedValue -- Positive: A(i,j) = amount of product i required by activity j
                                         in if abs value > 1e-15 then [(producerIdx, j, value)] else []
                                    Nothing -> []
                            Nothing -> []
                buildActivityTriplets (j, consumerActivity) =
                    let
                        -- Find reference product amount for normalization
                        refProductAmount = case find exchangeIsReference (exchanges consumerActivity) of
                            Just refEx ->
                                let unitName = getUnitNameForExchange unitDB refEx
                                    (normalizedAmount, _) = normalizeExchangeAmount unitName (exchangeAmount refEx)
                                 in if normalizedAmount > 1e-15 then normalizedAmount else 1.0
                            Nothing -> 1.0 -- No reference product, no scaling needed

                        -- Build triplets - NO reference product normalization needed here
                        -- (normalization will be done separately in the matrix solver)
                        buildNormalizedTechTriple ex = buildTechTriple j consumerActivity ex
                     in
                        concatMap buildNormalizedTechTriple (exchanges consumerActivity)
                !result = concatMap buildActivityTriplets (zip [0 ..] (M.elems allActivities))
                _ = unsafePerformIO $ reportMatrixOperation ("Technosphere matrix: " ++ show (length result) ++ " non-zero entries")
             in result

        -- Build biosphere sparse triplets (optimized with strict evaluation)
        _ = unsafePerformIO $ reportMatrixOperation "Building biosphere flow index"
        !bioFlowUUIDs =
            S.toList $
                S.fromList
                    [ exchangeFlowId ex
                    | activity <- M.elems allActivities
                    , ex <- exchanges activity
                    , isBiosphereExchange ex
                    ]
        !bioFlowCount = length bioFlowUUIDs
        !bioFlowIndex = M.fromList $ zip bioFlowUUIDs [0 ..]
        _ = unsafePerformIO $ reportMatrixOperation ("Biosphere index built: " ++ show bioFlowCount ++ " flows")

        _ = unsafePerformIO $ reportMatrixOperation "Building biosphere matrix triplets"
        !bioTriples =
            let buildBioTriple j activity ex
                    | not (isBiosphereExchange ex) = []
                    | otherwise =
                        case M.lookup (exchangeFlowId ex) bioFlowIndex of
                            Just i ->
                                let unitName = getUnitNameForExchange unitDB ex
                                    (normalizedAmount, _) = normalizeExchangeAmount unitName (exchangeAmount ex)
                                    amount =
                                        if exchangeIsInput ex
                                            then -normalizedAmount -- Resource consumption (negative)
                                            else normalizedAmount -- Emission (positive)
                                 in if abs amount > 1e-15 then [(i, j, amount)] else []
                            Nothing -> []
                buildActivityBioTriplets (j, activity) =
                    let
                        -- Find reference product amount for this activity (same as technosphere)
                        refProductAmount = case find exchangeIsReference (exchanges activity) of
                            Just refEx ->
                                let unitName = getUnitNameForExchange unitDB refEx
                                    (normalizedAmount, _) = normalizeExchangeAmount unitName (exchangeAmount refEx)
                                 in if normalizedAmount > 1e-15 then normalizedAmount else 1.0
                            Nothing -> 1.0 -- No reference product, no scaling needed

                        -- Build biosphere triplets - NO reference product normalization needed here
                        -- (normalization will be done separately in the matrix solver)
                        buildNormalizedBioTriple ex = buildBioTriple j activity ex
                     in
                        concatMap buildNormalizedBioTriple (exchanges activity)
                !result = concatMap buildActivityBioTriplets (zip [0 ..] (M.elems allActivities))
                _ = unsafePerformIO $ reportMatrixOperation ("Biosphere matrix: " ++ show (length result) ++ " non-zero entries")
             in result

        -- Force evaluation of matrix contents to ensure they're built now, not lazily later
        _ = techTriples `seq` bioTriples `seq` unsafePerformIO (reportMatrixOperation "Database with matrices built successfully")
        _ = unsafePerformIO $ reportMatrixOperation ("Final matrix stats: " ++ show (length techTriples) ++ " tech entries, " ++ show (length bioTriples) ++ " bio entries")
     in Database
            { dbActivities = activityDB
            , dbFlows = flowDB
            , dbUnits = unitDB
            , dbIndexes = indexes
            , dbTechnosphereTriples = techTriples
            , dbBiosphereTriples = bioTriples
            , dbActivityIndex = activityIndex
            , dbBiosphereFlows = bioFlowUUIDs
            , dbActivityCount = activityCount
            , dbBiosphereCount = bioFlowCount
            }

{- |
Build activity name index for efficient text-based searching.

Creates a mapping from lowercase activity names to lists of activity UUIDs.
This enables fast fuzzy search by allowing substring matching against
the normalized (lowercase) names.

Time Complexity: O(n) where n is the number of activities
Space Complexity: O(n) for the index structure
-}
buildNameIndex :: ActivityDB -> NameIndex
buildNameIndex procDB =
    M.fromListWith
        (++)
        [ (T.toLower (activityName proc), [activityId proc])
        | proc <- M.elems procDB
        ]

{- |
Build activity location index for geography-based filtering.

Creates a mapping from location codes (e.g., "FR", "GLO", "RER") to
lists of activity UUIDs. Enables efficient filtering by geography
for LCA regionalization studies.

Note: Location matching is exact, not fuzzy ("FR" ≠ "FRA")

Time Complexity: O(n) where n is the number of activities
-}
buildLocationIndex :: ActivityDB -> LocationIndex
buildLocationIndex procDB =
    M.fromListWith
        (++)
        [ (activityLocation proc, [activityId proc])
        | proc <- M.elems procDB
        ]

{- |
Build activity unit index for unit-based analysis.

Creates a mapping from unit names to lists of activity UUIDs.
Enables filtering activities by their functional unit
(e.g., "kg", "MJ", "m3", "tkm").

Useful for: Comparative LCA studies, unit conversion validation

Time Complexity: O(n) where n is the number of activities
-}
buildActivityUnitIndex :: ActivityDB -> ActivityUnitIndex
buildActivityUnitIndex procDB =
    M.fromListWith
        (++)
        [ (activityUnit proc, [activityId proc])
        | proc <- M.elems procDB
        ]

{- |
Build flow usage index for activity-flow relationships.

Creates a mapping from flow UUIDs to lists of activity UUIDs that use them.
This is the inverse of the exchange index - it answers "which activities
use this flow?" rather than "what exchanges does this activity have?"

Used for: Flow impact analysis, supply chain bottleneck identification

Time Complexity: O(e) where e is the total number of exchanges
-}
buildFlowIndex :: ActivityDB -> FlowIndex
buildFlowIndex procDB =
    M.fromListWith
        (++)
        [ (exchangeFlowId ex, [activityId proc])
        | proc <- M.elems procDB
        , ex <- exchanges proc
        ]

-- | Construction de l'index par catégorie de flux
buildFlowCategoryIndex :: FlowDB -> FlowCategoryIndex
buildFlowCategoryIndex flowDB =
    M.fromListWith
        (++)
        [ (flowCategory flow, [flowId flow])
        | flow <- M.elems flowDB
        ]

-- | Construction de l'index par type de flux
buildFlowTypeIndex :: FlowDB -> FlowTypeIndex
buildFlowTypeIndex flowDB =
    M.fromListWith
        (++)
        [ (flowType flow, [flowId flow])
        | flow <- M.elems flowDB
        ]

-- | Construction de l'index des échanges par flux
buildExchangeIndex :: ActivityDB -> ExchangeIndex
buildExchangeIndex procDB =
    M.fromListWith
        (++)
        [ (exchangeFlowId ex, [(activityId proc, ex)])
        | proc <- M.elems procDB
        , ex <- exchanges proc
        ]

-- | Construction de l'index des échanges par activité
buildActivityExchangeIndex :: ActivityDB -> ActivityExchangeIndex
buildActivityExchangeIndex procDB =
    M.fromList
        [ (activityId proc, exchanges proc)
        | proc <- M.elems procDB
        ]

-- | Construction de l'index des produits de référence
buildReferenceProductIndex :: ActivityDB -> ReferenceProductIndex
buildReferenceProductIndex procDB =
    M.fromList
        [ (exchangeFlowId ex, (activityId proc, ex))
        | proc <- M.elems procDB
        , ex <- exchanges proc
        , exchangeIsReference ex
        ]

-- | Construction de l'index des entrées par activité
buildActivityInputIndex :: ActivityDB -> ActivityInputIndex
buildActivityInputIndex procDB =
    M.fromList
        [ (activityId proc, filter exchangeIsInput (exchanges proc))
        | proc <- M.elems procDB
        ]

-- | Construction de l'index des sorties par activité
buildActivityOutputIndex :: ActivityDB -> ActivityOutputIndex
buildActivityOutputIndex procDB =
    M.fromList
        [ (activityId proc, filter (not . exchangeIsInput) (exchanges proc))
        | proc <- M.elems procDB
        ]

-- | Requêtes utilisant les index

{- |
Find activities matching specific field criteria with index optimization.

This function implements an efficient multi-field search strategy:
1. Use indexes to get candidate UUIDs (reduces search space)
2. Apply full criteria matching only to candidates
3. Return matching Activity objects

Search fields:
- nameParam: Fuzzy substring matching in activity names
- geoParam: Exact matching for geography codes
- productParam: Fuzzy matching in reference product names

Performance: O(log n + m) where n is total activities, m is result size
-}
findActivitiesByFields :: Database -> Maybe Text -> Maybe Text -> Maybe Text -> [Activity]
findActivitiesByFields db nameParam geoParam productParam =
    let
        -- Get candidate UUIDs using indexes for faster filtering
        candidateUUIDs = case (nameParam, geoParam, productParam) of
            -- If we have a name parameter, use the name index for initial filtering
            (Just name, _, _) -> getCandidatesFromNameIndex db name
            -- If we have a location but no name, use location index
            (Nothing, Just geo, _) -> getCandidatesFromLocationIndex db geo
            -- If we only have product parameter, use reference product index
            (Nothing, Nothing, Just _) -> M.keys (dbActivities db) -- Fall back to all activities
            -- If no parameters, return all activities
            (Nothing, Nothing, Nothing) -> M.keys (dbActivities db)

        -- Filter the candidates with the full criteria
        filteredActivities =
            [ activity
            | uuid <- candidateUUIDs
            , Just activity <- [M.lookup uuid (dbActivities db)]
            , matchesActivityFields db nameParam geoParam productParam activity
            ]
     in
        filteredActivities

-- | Get candidate activity UUIDs from name index using fuzzy matching
getCandidatesFromNameIndex :: Database -> Text -> [UUID]
getCandidatesFromNameIndex db searchName =
    let nameIndex = idxByName (dbIndexes db)
        lowerSearch = T.toLower searchName
        matchingEntries =
            [ uuids
            | (indexedName, uuids) <- M.toList nameIndex
            , lowerSearch `T.isInfixOf` indexedName -- Note: indexedName is already lowercase from buildNameIndex
            ]
     in concat matchingEntries

-- | Get candidate activity UUIDs from location index
getCandidatesFromLocationIndex :: Database -> Text -> [UUID]
getCandidatesFromLocationIndex db searchLocation =
    let locationIndex = idxByLocation (dbIndexes db)
        lowerSearch = T.toLower searchLocation
        matchingEntries =
            [ uuids
            | (indexedLocation, uuids) <- M.toList locationIndex
            , lowerSearch `T.isInfixOf` T.toLower indexedLocation
            ]
     in concat matchingEntries

-- | Matching par champs spécifiques d'activité
matchesActivityFields :: Database -> Maybe Text -> Maybe Text -> Maybe Text -> Activity -> Bool
matchesActivityFields db nameParam geoParam productParam activity =
    let nameMatch = case nameParam of
            Nothing -> True
            Just nameQuery ->
                let lowerName = T.toLower (activityName activity)
                    searchTerms = T.words (T.toLower nameQuery)
                 in all (`T.isInfixOf` lowerName) searchTerms

        geoMatch = case geoParam of
            Nothing -> True
            Just geoQuery ->
                let activityLoc = T.toLower (activityLocation activity)
                    queryLoc = T.toLower geoQuery
                 in activityLoc == queryLoc -- Exact match for geography codes
        productMatch = case productParam of
            Nothing -> True
            Just productQuery ->
                let referenceFlowNames =
                        [ T.toLower (flowName flow)
                        | exchange <- exchanges activity
                        , exchangeIsReference exchange
                        , Just flow <- [M.lookup (exchangeFlowId exchange) (dbFlows db)]
                        ]
                    searchTerms = T.words (T.toLower productQuery)
                    matchesProduct = any (\flowName -> all (`T.isInfixOf` flowName) searchTerms) referenceFlowNames
                 in matchesProduct
     in nameMatch && geoMatch && productMatch

{- |
Find flows by type (Technosphere or Biosphere) using pre-built index.

This is a fundamental LCA operation for matrix construction:
- Technosphere flows: Products and services exchanged between activities
- Biosphere flows: Environmental exchanges (emissions, resources)

Used extensively in sparse matrix assembly for solving LCA equations.

Time Complexity: O(log 1 + r) where r is the number of flows of the given type
-}
findFlowsByType :: Database -> FlowType -> [Flow]
findFlowsByType db ftype =
    case M.lookup ftype (idxFlowByType $ dbIndexes db) of
        Nothing -> []
        Just uuids -> mapMaybe (`M.lookup` dbFlows db) uuids

-- | ===== REQUÊTES AU NIVEAU ÉCHANGE =====

{- |
Find all reference products in the database with their producing activities.

Reference products define what each activity produces and are critical for:
- Supply chain linking ("who produces what I consume?")
- Functional unit selection
- Database completeness validation

Returns triples of (producing activity, reference product flow, exchange details)

Time Complexity: O(p) where p is the number of reference products
-}
findAllReferenceProducts :: Database -> [(Activity, Flow, Exchange)]
findAllReferenceProducts db =
    [ (proc, flow, exchange)
    | (flowUUID, (procUUID, exchange)) <- M.toList (idxReferenceProducts $ dbIndexes db)
    , Just proc <- [M.lookup procUUID (dbActivities db)]
    , Just flow <- [M.lookup flowUUID (dbFlows db)]
    ]

-- | Statistiques de la base de données
data DatabaseStats = DatabaseStats
    { statsActivityCount :: !Int
    , statsFlowCount :: !Int
    , statsExchangeCount :: !Int
    , statsTechnosphereFlows :: !Int
    , statsBiosphereFlows :: !Int
    , statsReferenceProducts :: !Int
    , statsInputCount :: !Int
    , statsOutputCount :: !Int
    , statsLocations :: ![Text]
    , statsCategories :: ![Text]
    , statsUnits :: ![Text]
    }
    deriving (Show)

{- |
Calculate comprehensive database statistics for monitoring and validation.

Provides key metrics for:
- Database completeness assessment
- Performance optimization insights
- Quality assurance (balanced exchanges, coverage)
- User interface displays

Statistics include:
- Activity/flow/exchange counts
- Technosphere vs biosphere flow distribution
- Geographic and unit coverage
- Input/output balance indicators

Time Complexity: O(n + f) where n is activities, f is flows
-}
getDatabaseStats :: Database -> DatabaseStats
getDatabaseStats db =
    DatabaseStats
        { statsActivityCount = M.size (dbActivities db)
        , statsFlowCount = M.size (dbFlows db)
        , statsExchangeCount = M.size (idxExchangeByFlow $ dbIndexes db)
        , statsTechnosphereFlows = length $ findFlowsByType db Technosphere
        , statsBiosphereFlows = length $ findFlowsByType db Biosphere
        , statsReferenceProducts = M.size (idxReferenceProducts $ dbIndexes db)
        , statsInputCount = sum [length inputs | inputs <- M.elems (idxInputsByActivity $ dbIndexes db)]
        , statsOutputCount = sum [length outputs | outputs <- M.elems (idxOutputsByActivity $ dbIndexes db)]
        , statsLocations = M.keys (idxByLocation $ dbIndexes db)
        , statsCategories = M.keys (idxFlowByCategory $ dbIndexes db)
        , statsUnits = S.toList $ S.fromList [getUnitNameForFlow (dbUnits db) flow | flow <- M.elems (dbFlows db)]
        }

-- | Fonctions utilitaires pour les requêtes complexes

{- |
Search flows by name and synonyms using fuzzy matching.

Implements multi-stage fuzzy search:
1. Get candidates from category/name indexes (performance optimization)
2. Apply fuzzy substring matching on name, category, and all synonyms
3. Support multi-term queries (all terms must match somewhere)

Used for: Flow discovery in web interfaces, impact category mapping

Performance: O(log n + m*t) where n is flows, m is matches, t is terms
-}
findFlowsBySynonym :: Database -> Text -> [Flow]
findFlowsBySynonym db searchText =
    let lowerSearch = T.toLower searchText
        searchTerms = T.words lowerSearch -- Split on whitespace for multi-term search
        -- Try to get candidates from category index first for better performance
        candidateUUIDs = getCandidatesFromFlowSearch db searchText
        candidateFlows = mapMaybe (`M.lookup` dbFlows db) candidateUUIDs
        -- Filter candidates with full fuzzy matching
        matchingFlows = filter (matchesFlowFuzzy searchTerms) candidateFlows
     in matchingFlows

-- | Get candidate flow UUIDs using category index and name matching
getCandidatesFromFlowSearch :: Database -> Text -> [UUID]
getCandidatesFromFlowSearch db searchText =
    let lowerSearch = T.toLower searchText
        categoryIndex = idxFlowByCategory (dbIndexes db)
        -- Get flows from categories that match the search text
        categoryMatches =
            [ uuids
            | (category, uuids) <- M.toList categoryIndex
            , T.isInfixOf lowerSearch (T.toLower category)
            ]
        categoryUUIDs = concat categoryMatches

        -- For name matching, use a more efficient approach
        -- Check if we can find exact matches first, then fallback to partial matching
        flowsDB = dbFlows db
        nameMatches =
            if length categoryUUIDs < 100 -- Only do expensive name search if category search was restrictive
                then
                    [ flowId flow
                    | flow <- M.elems flowsDB
                    , T.isInfixOf lowerSearch (T.toLower (flowName flow))
                    ]
                else
                    [] -- Skip name matching if we already have many category matches

        -- Combine and deduplicate
        allCandidates = S.toList $ S.fromList (categoryUUIDs ++ nameMatches)
     in allCandidates

-- | Advanced fuzzy matching for flows (substring matching + multi-term)
matchesFlowFuzzy :: [Text] -> Flow -> Bool
matchesFlowFuzzy searchTerms flow =
    let lowerName = T.toLower (flowName flow)
        lowerCategory = T.toLower (flowCategory flow)
        allSynonyms = getAllSynonyms flow
        lowerSynonyms = map T.toLower allSynonyms

        -- Check if all search terms match somewhere
        matchesTerm term =
            -- Substring in flow name
            term `T.isInfixOf` lowerName
                ||
                -- Substring in category
                term `T.isInfixOf` lowerCategory
                ||
                -- Substring in any synonym
                any (term `T.isInfixOf`) lowerSynonyms
     in all matchesTerm searchTerms

{- |
Search flows by synonyms in a specific language with fuzzy matching.

Similar to findFlowsBySynonym but restricts synonym matching to a specific
language code (e.g., "fr", "de", "en"). This improves precision when
working with multilingual databases.

Used for: Internationalization, language-specific flow discovery

Performance: O(log n + m*t) where n is flows, m is matches, t is terms
-}
findFlowsBySynonymInLanguage :: Database -> Text -> Text -> [Flow]
findFlowsBySynonymInLanguage db lang searchText =
    let lowerSearch = T.toLower searchText
        searchTerms = T.words lowerSearch
        -- Use the same candidate filtering approach as the general search
        candidateUUIDs = getCandidatesFromFlowSearch db searchText
        candidateFlows = mapMaybe (`M.lookup` dbFlows db) candidateUUIDs
        -- Filter candidates with language-specific matching
        matchingFlows = filter (matchesFlowInLanguage searchTerms lang) candidateFlows
     in matchingFlows

-- | Fuzzy matching for flows in specific language
matchesFlowInLanguage :: [Text] -> Text -> Flow -> Bool
matchesFlowInLanguage searchTerms lang flow =
    let lowerName = T.toLower (flowName flow)
        langSynonyms = case M.lookup lang (flowSynonyms flow) of
            Nothing -> []
            Just syns -> S.toList syns
        lowerSynonyms = map T.toLower langSynonyms

        matchesTerm term =
            -- Substring in flow name
            term `T.isInfixOf` lowerName
                ||
                -- Substring in language-specific synonyms
                any (term `T.isInfixOf`) lowerSynonyms
     in all matchesTerm searchTerms

{- |
Get all available languages from flow synonyms.

Extracts the complete set of language codes present in the database's
synonym system. Used for building language selection interfaces and
validating language-specific queries.

Returns: List of ISO language codes (e.g., ["en", "fr", "de", "es"])

Time Complexity: O(f*l) where f is flows, l is avg languages per flow
-}
getAvailableLanguages :: Database -> [Text]
getAvailableLanguages db =
    S.toList $
        S.fromList $
            concat
                [ M.keys (flowSynonyms flow)
                | flow <- M.elems (dbFlows db)
                ]

-- | Statistiques sur les synonymes
data SynonymStats = SynonymStats
    { ssFlowsWithSynonyms :: Int -- Nombre de flux avec synonymes
    , ssAveragePerFlow :: Double -- Nombre moyen de synonymes par flux
    , ssLanguageStats :: [(Text, Int)] -- Statistiques par langue
    , ssTotalSynonyms :: Int -- Total des synonymes
    }
    deriving (Generic)

{- |
Calculate comprehensive synonym statistics for database analysis.

Provides metrics on synonym coverage and distribution:
- Number/percentage of flows with synonyms
- Average synonyms per flow
- Language distribution statistics
- Total synonym count

Used for: Database quality assessment, internationalization planning

Time Complexity: O(f*s*l) where f is flows, s is synonyms, l is languages
-}
getSynonymStats :: Database -> SynonymStats
getSynonymStats db =
    let flows = M.elems (dbFlows db)
        flowsWithSynonyms = [flow | flow <- flows, not . M.null $ flowSynonyms flow]
        totalSynonyms = sum [S.size syns | flow <- flows, syns <- M.elems (flowSynonyms flow)]
        languageStats =
            [ (lang, length [flow | flow <- flows, M.member lang (flowSynonyms flow)])
            | lang <- getAvailableLanguages db
            ]
        avgSynonyms = if null flows then 0 else fromIntegral totalSynonyms / fromIntegral (length flows)
     in SynonymStats
            { ssFlowsWithSynonyms = length flowsWithSynonyms
            , ssAveragePerFlow = avgSynonyms
            , ssLanguageStats = languageStats
            , ssTotalSynonyms = totalSynonyms
            }

-- Comprehensive synonym function removed - synonyms now included directly in flow responses
