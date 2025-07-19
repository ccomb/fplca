{-# LANGUAGE OverloadedStrings #-}

module ACV.Query where

import ACV.Types
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (mapMaybe)
import Data.List (sortOn)

-- | Construction d'index vides
emptyIndexes :: Indexes
emptyIndexes = Indexes M.empty M.empty M.empty M.empty M.empty

-- | Construction des index à partir d'une base de données simple
buildIndexes :: ProcessDB -> FlowDB -> Indexes
buildIndexes procDB flowDB = Indexes
    { idxByName = buildNameIndex procDB
    , idxByLocation = buildLocationIndex procDB
    , idxByFlow = buildFlowIndex procDB
    , idxFlowByCategory = buildFlowCategoryIndex flowDB
    , idxFlowByType = buildFlowTypeIndex flowDB
    }

-- | Construction de l'index par nom (insensible à la casse)
buildNameIndex :: ProcessDB -> NameIndex
buildNameIndex procDB = 
    M.fromListWith (++) 
    [ (T.toLower (processName proc), [processId proc])
    | proc <- M.elems procDB
    ]

-- | Construction de l'index par localisation
buildLocationIndex :: ProcessDB -> LocationIndex
buildLocationIndex procDB = 
    M.fromListWith (++)
    [ (processLocation proc, [processId proc])
    | proc <- M.elems procDB
    ]

-- | Construction de l'index par flux (quels processus utilisent un flux)
buildFlowIndex :: ProcessDB -> FlowIndex
buildFlowIndex procDB = 
    M.fromListWith (++)
    [ (exchangeFlowId ex, [processId proc])
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

-- | Requêtes utilisant les index

-- | Recherche de processus par nom (recherche partielle, insensible à la casse)
findProcessesByName :: Database -> Text -> [Process]
findProcessesByName db searchTerm = 
    let searchLower = T.toLower searchTerm
        matchingUUIDs = concat 
            [ uuids 
            | (name, uuids) <- M.toList (idxByName $ dbIndexes db)
            , searchLower `T.isInfixOf` name
            ]
        uniqueUUIDs = S.toList $ S.fromList matchingUUIDs
    in mapMaybe (`M.lookup` dbProcesses db) uniqueUUIDs

-- | Recherche de processus par localisation exacte
findProcessesByLocation :: Database -> Text -> [Process]
findProcessesByLocation db location = 
    case M.lookup location (idxByLocation $ dbIndexes db) of
        Nothing -> []
        Just uuids -> mapMaybe (`M.lookup` dbProcesses db) uuids

-- | Recherche de processus utilisant un flux donné
findProcessesUsingFlow :: Database -> UUID -> [Process]
findProcessesUsingFlow db flowUUID = 
    case M.lookup flowUUID (idxByFlow $ dbIndexes db) of
        Nothing -> []
        Just uuids -> mapMaybe (`M.lookup` dbProcesses db) uuids

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

-- | Statistiques de la base de données
data DatabaseStats = DatabaseStats
    { statsProcessCount :: !Int
    , statsFlowCount :: !Int
    , statsExchangeCount :: !Int
    , statsTechnosphereFlows :: !Int
    , statsBiosphereFlows :: !Int
    , statsLocations :: ![Text]
    , statsCategories :: ![Text]
    } deriving (Show, Eq)

-- | Calcule les statistiques de la base de données
getDatabaseStats :: Database -> DatabaseStats
getDatabaseStats db = DatabaseStats
    { statsProcessCount = M.size (dbProcesses db)
    , statsFlowCount = M.size (dbFlows db)
    , statsExchangeCount = sum [length (exchanges proc) | proc <- M.elems (dbProcesses db)]
    , statsTechnosphereFlows = length $ findFlowsByType db Technosphere
    , statsBiosphereFlows = length $ findFlowsByType db Biosphere
    , statsLocations = M.keys (idxByLocation $ dbIndexes db)
    , statsCategories = M.keys (idxFlowByCategory $ dbIndexes db)
    }

-- | Fonctions utilitaires pour les requêtes complexes

-- | Trouve les processus les plus utilisés (qui apparaissent dans le plus d'échanges)
findMostUsedProcesses :: Database -> Int -> [(Process, Int)]
findMostUsedProcesses db limit = 
    let usageCounts = 
            [ (proc, length uuids)
            | (flowUUID, procUUIDs) <- M.toList (idxByFlow $ dbIndexes db)
            , let uuids = procUUIDs
            , proc <- mapMaybe (`M.lookup` dbProcesses db) procUUIDs
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