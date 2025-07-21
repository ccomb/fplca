{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module ACV.Query where

import ACV.Types
import Control.Parallel.Strategies
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (mapMaybe)
import Data.List (sortOn)
import GHC.Generics (Generic)

-- | Construction d'index vides
emptyIndexes :: Indexes
emptyIndexes = Indexes M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty

-- | Construction des index à partir d'une base de données simple (parallélisée)
buildIndexes :: ActivityDB -> FlowDB -> Indexes
buildIndexes procDB flowDB = 
    let -- Build indexes with parallel evaluation
        nameIdx = buildNameIndex procDB `using` rdeepseq
        locationIdx = buildLocationIndex procDB `using` rdeepseq
        flowIdx = buildFlowIndex procDB `using` rdeepseq
        flowCatIdx = buildFlowCategoryIndex flowDB `using` rdeepseq
        flowTypeIdx = buildFlowTypeIndex flowDB `using` rdeepseq
        exchangeIdx = buildExchangeIndex procDB `using` rdeepseq
        procExchangeIdx = buildActivityExchangeIndex procDB `using` rdeepseq
        refProdIdx = buildReferenceProductIndex procDB `using` rdeepseq
        inputIdx = buildActivityInputIndex procDB `using` rdeepseq
        outputIdx = buildActivityOutputIndex procDB `using` rdeepseq
    in Indexes
    { -- Index au niveau activité
      idxByName = nameIdx
    , idxByLocation = locationIdx
    , idxByFlow = flowIdx
      -- Index au niveau flux
    , idxFlowByCategory = flowCatIdx
    , idxFlowByType = flowTypeIdx
      -- Index au niveau échange
    , idxExchangeByFlow = exchangeIdx
    , idxExchangeByActivity = procExchangeIdx
    , idxReferenceProducts = refProdIdx
    , idxInputsByActivity = inputIdx
    , idxOutputsByActivity = outputIdx
    }

-- | Construction de l'index par nom (insensible à la casse)
buildNameIndex :: ActivityDB -> NameIndex
buildNameIndex procDB = 
    M.fromListWith (++) 
    [ (T.toLower (activityName proc), [activityId proc])
    | proc <- M.elems procDB
    ]

-- | Construction de l'index par localisation
buildLocationIndex :: ActivityDB -> LocationIndex
buildLocationIndex procDB = 
    M.fromListWith (++)
    [ (activityLocation proc, [activityId proc])
    | proc <- M.elems procDB
    ]

-- | Construction de l'index par flux (quels activityus utilisent un flux)
buildFlowIndex :: ActivityDB -> FlowIndex
buildFlowIndex procDB = 
    M.fromListWith (++)
    [ (exchangeFlowId ex, [activityId proc])
    | proc <- M.elems procDB
    , ex <- exchanges proc
    ]

-- | Construction de l'index par catégorie de flux
buildFlowCategoryIndex :: FlowDB -> FlowCategoryIndex
buildFlowCategoryIndex flowDB = 
    M.fromListWith (++)
    [ (flowCategory flow, [flowId flow])
    | flow <- M.elems flowDB
    ]

-- | Construction de l'index par type de flux
buildFlowTypeIndex :: FlowDB -> FlowTypeIndex
buildFlowTypeIndex flowDB = 
    M.fromListWith (++)
    [ (flowType flow, [flowId flow])
    | flow <- M.elems flowDB
    ]

-- | Construction de l'index des échanges par flux
buildExchangeIndex :: ActivityDB -> ExchangeIndex
buildExchangeIndex procDB = 
    M.fromListWith (++)
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

-- | Recherche de activityus par nom (recherche partielle, insensible à la casse)
findActivitiesByName :: Database -> Text -> [Activity]
findActivitiesByName db searchTerm = 
    let searchLower = T.toLower searchTerm
        matchingUUIDs = concat 
            [ uuids 
            | (name, uuids) <- M.toList (idxByName $ dbIndexes db)
            , searchLower `T.isInfixOf` name
            ]
        uniqueUUIDs = S.toList $ S.fromList matchingUUIDs
    in mapMaybe (`M.lookup` dbActivities db) uniqueUUIDs

-- | Recherche de activityus par localisation exacte
findActivitiesByLocation :: Database -> Text -> [Activity]
findActivitiesByLocation db location = 
    case M.lookup location (idxByLocation $ dbIndexes db) of
        Nothing -> []
        Just uuids -> mapMaybe (`M.lookup` dbActivities db) uuids

-- | Recherche de activityus utilisant un flux donné
findActivitiesUsingFlow :: Database -> UUID -> [Activity]
findActivitiesUsingFlow db flowUUID = 
    case M.lookup flowUUID (idxByFlow $ dbIndexes db) of
        Nothing -> []
        Just uuids -> mapMaybe (`M.lookup` dbActivities db) uuids

-- | Recherche de flux par catégorie
findFlowsByCategory :: Database -> Text -> [Flow]
findFlowsByCategory db category = 
    case M.lookup category (idxFlowByCategory $ dbIndexes db) of
        Nothing -> []
        Just uuids -> mapMaybe (`M.lookup` dbFlows db) uuids

-- | Recherche de flux par type (Technosphere ou Biosphere)
findFlowsByType :: Database -> FlowType -> [Flow]
findFlowsByType db ftype = 
    case M.lookup ftype (idxFlowByType $ dbIndexes db) of
        Nothing -> []
        Just uuids -> mapMaybe (`M.lookup` dbFlows db) uuids

-- | ===== REQUÊTES AU NIVEAU ÉCHANGE =====

-- | Trouve tous les échanges utilisant un flux donné
findExchangesByFlow :: Database -> UUID -> [(Activity, Exchange)]
findExchangesByFlow db flowUUID = 
    case M.lookup flowUUID (idxExchangeByFlow $ dbIndexes db) of
        Nothing -> []
        Just procExchanges -> 
            [ (proc, exchange)
            | (procUUID, exchange) <- procExchanges
            , Just proc <- [M.lookup procUUID (dbActivities db)]
            ]

-- | Trouve tous les échanges d'un activité
findExchangesByActivity :: Database -> UUID -> [Exchange]
findExchangesByActivity db procUUID = 
    case M.lookup procUUID (idxExchangeByActivity $ dbIndexes db) of
        Nothing -> []
        Just exchanges -> exchanges

-- | Trouve toutes les entrées d'un activité
findInputsForActivity :: Database -> UUID -> [Exchange]
findInputsForActivity db procUUID = 
    case M.lookup procUUID (idxInputsByActivity $ dbIndexes db) of
        Nothing -> []
        Just inputs -> inputs

-- | Trouve toutes les sorties d'un activité
findOutputsForActivity :: Database -> UUID -> [Exchange]
findOutputsForActivity db procUUID = 
    case M.lookup procUUID (idxOutputsByActivity $ dbIndexes db) of
        Nothing -> []
        Just outputs -> outputs

-- | Trouve le produit de référence d'un activité (s'il existe)
findReferenceProduct :: Database -> UUID -> Maybe (Flow, Exchange)
findReferenceProduct db procUUID = 
    case findOutputsForActivity db procUUID of
        [] -> Nothing
        outputs -> 
            case filter exchangeIsReference outputs of
                [] -> Nothing
                (refEx:_) -> do
                    flow <- M.lookup (exchangeFlowId refEx) (dbFlows db)
                    return (flow, refEx)

-- | Trouve tous les produits de référence de la base de données
findAllReferenceProducts :: Database -> [(Activity, Flow, Exchange)]
findAllReferenceProducts db = 
    [ (proc, flow, exchange)
    | (flowUUID, (procUUID, exchange)) <- M.toList (idxReferenceProducts $ dbIndexes db)
    , Just proc <- [M.lookup procUUID (dbActivities db)]
    , Just flow <- [M.lookup flowUUID (dbFlows db)]
    ]

-- | Trouve les échanges par unité
findExchangesByUnit :: Database -> Text -> [(Activity, Flow, Exchange)]
findExchangesByUnit db unit = 
    [ (proc, flow, exchange)
    | (procUUID, exchanges) <- M.toList (idxExchangeByActivity $ dbIndexes db)
    , exchange <- exchanges
    , Just flow <- [M.lookup (exchangeFlowId exchange) (dbFlows db)]
    , getUnitNameForFlow (dbUnits db) flow == unit
    , Just proc <- [M.lookup procUUID (dbActivities db)]
    ]

-- | Trouve les échanges dans une plage de quantité
findExchangesByAmountRange :: Database -> Double -> Double -> [(Activity, Flow, Exchange)]
findExchangesByAmountRange db minAmount maxAmount = 
    [ (proc, flow, exchange)
    | (procUUID, exchanges) <- M.toList (idxExchangeByActivity $ dbIndexes db)
    , exchange <- exchanges
    , let amount = exchangeAmount exchange
    , amount >= minAmount && amount <= maxAmount
    , Just flow <- [M.lookup (exchangeFlowId exchange) (dbFlows db)]
    , Just proc <- [M.lookup procUUID (dbActivities db)]
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
    } deriving (Show, Eq)

-- | Calcule les statistiques de la base de données
getDatabaseStats :: Database -> DatabaseStats
getDatabaseStats db = DatabaseStats
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

-- | Trouve les activityus les plus utilisés (qui apparaissent dans le plus d'échanges)
findMostUsedActivities :: Database -> Int -> [(Activity, Int)]
findMostUsedActivities db limit = 
    let usageCounts = 
            [ (proc, length uuids)
            | (flowUUID, procUUIDs) <- M.toList (idxByFlow $ dbIndexes db)
            , let uuids = procUUIDs
            , proc <- mapMaybe (`M.lookup` dbActivities db) procUUIDs
            ]
        sorted = sortOn (negate . snd) usageCounts
    in take limit sorted

-- | Trouve les flux les plus utilisés
findMostUsedFlows :: Database -> Int -> [(Flow, Int)]
findMostUsedFlows db limit = 
    let usageCounts = 
            [ (flow, length procUUIDs)
            | (flowUUID, procUUIDs) <- M.toList (idxByFlow $ dbIndexes db)
            , Just flow <- [M.lookup flowUUID (dbFlows db)]
            ]
        sorted = sortOn (negate . snd) usageCounts
    in take limit sorted

-- | Recherche de flux par synonymes et nom (fuzzy search)
-- | Trouve tous les flux qui correspondent au texte de recherche (substring dans le nom ou synonymes)
findFlowsBySynonym :: Database -> Text -> [Flow]
findFlowsBySynonym db searchText = 
    let lowerSearch = T.toLower searchText
        searchTerms = T.words lowerSearch  -- Split on whitespace for multi-term search
    in [ flow 
       | flow <- M.elems (dbFlows db)
       , matchesFlowFuzzy searchTerms flow
       ]

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
            term `T.isInfixOf` lowerName ||
            -- Substring in category
            term `T.isInfixOf` lowerCategory ||
            -- Substring in any synonym
            any (term `T.isInfixOf`) lowerSynonyms
            
    in all matchesTerm searchTerms

-- | Trouve tous les flux qui ont des synonymes dans une langue donnée
findFlowsWithSynonymsInLanguage :: Database -> Text -> [Flow]
findFlowsWithSynonymsInLanguage db lang = 
    [ flow 
    | flow <- M.elems (dbFlows db)
    , case M.lookup lang (flowSynonyms flow) of
        Nothing -> False
        Just synonyms -> not (S.null synonyms)
    ]

-- | Recherche de flux par synonyme dans une langue spécifique (fuzzy search)
findFlowsBySynonymInLanguage :: Database -> Text -> Text -> [Flow]
findFlowsBySynonymInLanguage db lang searchText = 
    let lowerSearch = T.toLower searchText
        searchTerms = T.words lowerSearch
    in [ flow 
       | flow <- M.elems (dbFlows db)
       , matchesFlowInLanguage searchTerms lang flow
       ]

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
            term `T.isInfixOf` lowerName ||
            -- Substring in language-specific synonyms
            any (term `T.isInfixOf`) lowerSynonyms
            
    in all matchesTerm searchTerms

-- | Obtient toutes les langues disponibles dans les synonymes
getAvailableLanguages :: Database -> [Text]
getAvailableLanguages db = 
    S.toList $ S.fromList $ concat
        [ M.keys (flowSynonyms flow)
        | flow <- M.elems (dbFlows db)
        ]

-- | Statistiques sur les synonymes
data SynonymStats = SynonymStats
    { ssFlowsWithSynonyms :: Int                -- Nombre de flux avec synonymes
    , ssAveragePerFlow :: Double                -- Nombre moyen de synonymes par flux
    , ssLanguageStats :: [(Text, Int)]          -- Statistiques par langue
    , ssTotalSynonyms :: Int                    -- Total des synonymes
    } deriving (Eq, Show, Generic)

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