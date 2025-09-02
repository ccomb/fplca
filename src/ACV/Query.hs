{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ACV.Query where

import ACV.Types
import Control.Parallel.Strategies
import Data.List (sortOn)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace (trace)
import GHC.Generics (Generic)

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

-- | Build complete database with pre-computed sparse matrices (Brightway approach)
buildDatabaseWithMatrices :: ActivityDB -> FlowDB -> UnitDB -> Database
buildDatabaseWithMatrices activityDB flowDB unitDB =
    let _ = trace ("=== BUILDING DATABASE WITH MATRICES ===") ()
        indexes = buildIndexes activityDB flowDB
        -- Build complete sparse coordinate lists for entire database
        _ = trace ("Building activity indexes...") ()
        allActivities = activityDB
        activityUUIDs = M.keys allActivities
        activityCount = length activityUUIDs
        activityIndex = M.fromList $ zip activityUUIDs [0 ..]
        _ = trace ("Activity index built: " ++ show activityCount ++ " activities") ()

        -- Build technosphere sparse triplets (optimized with strict evaluation)
        _ = trace ("Building technosphere matrix triplets...") ()
        !techTriples =
            let buildTechTriple j consumerActivity ex
                    | not (isTechnosphereExchange ex) = []
                    | exchangeIsReference ex = [] -- Skip reference products - do not add to A matrix
                    | otherwise =
                        case exchangeActivityLinkId ex of
                            Just inputActivityUUID ->
                                case M.lookup inputActivityUUID activityIndex of
                                    Just producerIdx ->
                                        let value = exchangeAmount ex -- Positive: A(i,j) = amount of product i required by activity j
                                         in if abs value > 1e-15 then [(producerIdx, j, value)] else []
                                    Nothing -> []
                            Nothing -> []
                buildActivityTriplets (j, consumerActivity) =
                    concatMap (buildTechTriple j consumerActivity) (exchanges consumerActivity)
                !result = concatMap buildActivityTriplets (zip [0 ..] (M.elems allActivities))
                _ = trace ("Technosphere matrix: " ++ show (length result) ++ " non-zero entries") ()
             in result

        -- Build biosphere sparse triplets (optimized with strict evaluation)
        _ = trace ("Building biosphere flow index...") ()
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
        _ = trace ("Biosphere index built: " ++ show bioFlowCount ++ " flows") ()

        _ = trace ("Building biosphere matrix triplets...") ()
        !bioTriples =
            let buildBioTriple j activity ex
                    | not (isBiosphereExchange ex) = []
                    | otherwise =
                        case M.lookup (exchangeFlowId ex) bioFlowIndex of
                            Just i ->
                                let amount =
                                        if exchangeIsInput ex
                                            then -(exchangeAmount ex) -- Resource consumption (negative)
                                            else exchangeAmount ex -- Emission (positive)
                                 in if abs amount > 1e-15 then [(i, j, amount)] else []
                            Nothing -> []
                buildActivityBioTriplets (j, activity) =
                    concatMap (buildBioTriple j activity) (exchanges activity)
                !result = concatMap buildActivityBioTriplets (zip [0 ..] (M.elems allActivities))
                _ = trace ("Biosphere matrix: " ++ show (length result) ++ " non-zero entries") ()
             in result

        -- Force evaluation of matrix contents to ensure they're built now, not lazily later
        _ = techTriples `seq` bioTriples `seq` trace ("=== DATABASE WITH MATRICES BUILT SUCCESSFULLY ===") ()
        _ = trace ("Final matrix stats: " ++ show (length techTriples) ++ " tech entries, " ++ show (length bioTriples) ++ " bio entries") ()
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

-- | Construction de l'index par nom (insensible à la casse)
buildNameIndex :: ActivityDB -> NameIndex
buildNameIndex procDB =
    M.fromListWith
        (++)
        [ (T.toLower (activityName proc), [activityId proc])
        | proc <- M.elems procDB
        ]

-- | Construction de l'index par localisation
buildLocationIndex :: ActivityDB -> LocationIndex
buildLocationIndex procDB =
    M.fromListWith
        (++)
        [ (activityLocation proc, [activityId proc])
        | proc <- M.elems procDB
        ]

-- | Construction de l'index par unité de référence
buildActivityUnitIndex :: ActivityDB -> ActivityUnitIndex
buildActivityUnitIndex procDB =
    M.fromListWith
        (++)
        [ (activityUnit proc, [activityId proc])
        | proc <- M.elems procDB
        ]

-- | Construction de l'index par flux (quels activités utilisent un flux)
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

-- | Recherche d'activités par champs spécifiques
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

-- | Recherche de flux par type (Technosphere ou Biosphere)
findFlowsByType :: Database -> FlowType -> [Flow]
findFlowsByType db ftype =
    case M.lookup ftype (idxFlowByType $ dbIndexes db) of
        Nothing -> []
        Just uuids -> mapMaybe (`M.lookup` dbFlows db) uuids

-- | ===== REQUÊTES AU NIVEAU ÉCHANGE =====

-- | Trouve tous les produits de référence de la base de données
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

-- | Calcule les statistiques de la base de données
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

{- | Recherche de flux par synonymes et nom (fuzzy search)
| Trouve tous les flux qui correspondent au texte de recherche (substring dans le nom ou synonymes)
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

-- | Recherche de flux par synonyme dans une langue spécifique (fuzzy search)
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

-- | Obtient toutes les langues disponibles dans les synonymes
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

-- | Calcule les statistiques des synonymes
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
