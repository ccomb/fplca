{-# LANGUAGE OverloadedStrings #-}

module ACV.Query where

import ACV.Types
import Control.Parallel.Strategies
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (mapMaybe)
import Data.List (sortOn)

-- | Construction d'index vides
emptyIndexes :: Indexes
emptyIndexes = Indexes M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty

-- | Construction des index à partir d'une base de données simple (parallélisée)
buildIndexes :: ProcessDB -> FlowDB -> Indexes
buildIndexes procDB flowDB = 
    let -- Build indexes with parallel evaluation
        nameIdx = buildNameIndex procDB `using` rdeepseq
        locationIdx = buildLocationIndex procDB `using` rdeepseq
        flowIdx = buildFlowIndex procDB `using` rdeepseq
        flowCatIdx = buildFlowCategoryIndex flowDB `using` rdeepseq
        flowTypeIdx = buildFlowTypeIndex flowDB `using` rdeepseq
        exchangeIdx = buildExchangeIndex procDB `using` rdeepseq
        procExchangeIdx = buildProcessExchangeIndex procDB `using` rdeepseq
        refProdIdx = buildReferenceProductIndex procDB `using` rdeepseq
        inputIdx = buildProcessInputIndex procDB `using` rdeepseq
        outputIdx = buildProcessOutputIndex procDB `using` rdeepseq
    in Indexes
    { -- Index au niveau procédé
      idxByName = nameIdx
    , idxByLocation = locationIdx
    , idxByFlow = flowIdx
      -- Index au niveau flux
    , idxFlowByCategory = flowCatIdx
    , idxFlowByType = flowTypeIdx
      -- Index au niveau échange
    , idxExchangeByFlow = exchangeIdx
    , idxExchangeByProcess = procExchangeIdx
    , idxReferenceProducts = refProdIdx
    , idxInputsByProcess = inputIdx
    , idxOutputsByProcess = outputIdx
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

-- | Construction de l'index des échanges par flux
buildExchangeIndex :: ProcessDB -> ExchangeIndex
buildExchangeIndex procDB = 
    M.fromListWith (++)
    [ (exchangeFlowId ex, [(processId proc, ex)])
    | proc <- M.elems procDB
    , ex <- exchanges proc
    ]

-- | Construction de l'index des échanges par procédé
buildProcessExchangeIndex :: ProcessDB -> ProcessExchangeIndex
buildProcessExchangeIndex procDB = 
    M.fromList
    [ (processId proc, exchanges proc)
    | proc <- M.elems procDB
    ]

-- | Construction de l'index des produits de référence
buildReferenceProductIndex :: ProcessDB -> ReferenceProductIndex
buildReferenceProductIndex procDB = 
    M.fromList
    [ (exchangeFlowId ex, (processId proc, ex))
    | proc <- M.elems procDB
    , ex <- exchanges proc
    , exchangeIsReference ex
    ]

-- | Construction de l'index des entrées par procédé
buildProcessInputIndex :: ProcessDB -> ProcessInputIndex
buildProcessInputIndex procDB = 
    M.fromList
    [ (processId proc, filter exchangeIsInput (exchanges proc))
    | proc <- M.elems procDB
    ]

-- | Construction de l'index des sorties par procédé
buildProcessOutputIndex :: ProcessDB -> ProcessOutputIndex
buildProcessOutputIndex procDB = 
    M.fromList
    [ (processId proc, filter (not . exchangeIsInput) (exchanges proc))
    | proc <- M.elems procDB
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

-- | ===== REQUÊTES AU NIVEAU ÉCHANGE =====

-- | Trouve tous les échanges utilisant un flux donné
findExchangesByFlow :: Database -> UUID -> [(Process, Exchange)]
findExchangesByFlow db flowUUID = 
    case M.lookup flowUUID (idxExchangeByFlow $ dbIndexes db) of
        Nothing -> []
        Just procExchanges -> 
            [ (proc, exchange)
            | (procUUID, exchange) <- procExchanges
            , Just proc <- [M.lookup procUUID (dbProcesses db)]
            ]

-- | Trouve tous les échanges d'un procédé
findExchangesByProcess :: Database -> UUID -> [Exchange]
findExchangesByProcess db procUUID = 
    case M.lookup procUUID (idxExchangeByProcess $ dbIndexes db) of
        Nothing -> []
        Just exchanges -> exchanges

-- | Trouve toutes les entrées d'un procédé
findInputsForProcess :: Database -> UUID -> [Exchange]
findInputsForProcess db procUUID = 
    case M.lookup procUUID (idxInputsByProcess $ dbIndexes db) of
        Nothing -> []
        Just inputs -> inputs

-- | Trouve toutes les sorties d'un procédé
findOutputsForProcess :: Database -> UUID -> [Exchange]
findOutputsForProcess db procUUID = 
    case M.lookup procUUID (idxOutputsByProcess $ dbIndexes db) of
        Nothing -> []
        Just outputs -> outputs

-- | Trouve le produit de référence d'un procédé (s'il existe)
findReferenceProduct :: Database -> UUID -> Maybe (Flow, Exchange)
findReferenceProduct db procUUID = 
    case findOutputsForProcess db procUUID of
        [] -> Nothing
        outputs -> 
            case filter exchangeIsReference outputs of
                [] -> Nothing
                (refEx:_) -> do
                    flow <- M.lookup (exchangeFlowId refEx) (dbFlows db)
                    return (flow, refEx)

-- | Trouve tous les produits de référence de la base de données
findAllReferenceProducts :: Database -> [(Process, Flow, Exchange)]
findAllReferenceProducts db = 
    [ (proc, flow, exchange)
    | (flowUUID, (procUUID, exchange)) <- M.toList (idxReferenceProducts $ dbIndexes db)
    , Just proc <- [M.lookup procUUID (dbProcesses db)]
    , Just flow <- [M.lookup flowUUID (dbFlows db)]
    ]

-- | Trouve les échanges par unité
findExchangesByUnit :: Database -> Text -> [(Process, Flow, Exchange)]
findExchangesByUnit db unit = 
    [ (proc, flow, exchange)
    | (procUUID, exchanges) <- M.toList (idxExchangeByProcess $ dbIndexes db)
    , exchange <- exchanges
    , Just flow <- [M.lookup (exchangeFlowId exchange) (dbFlows db)]
    , flowUnit flow == unit
    , Just proc <- [M.lookup procUUID (dbProcesses db)]
    ]

-- | Trouve les échanges dans une plage de quantité
findExchangesByAmountRange :: Database -> Double -> Double -> [(Process, Flow, Exchange)]
findExchangesByAmountRange db minAmount maxAmount = 
    [ (proc, flow, exchange)
    | (procUUID, exchanges) <- M.toList (idxExchangeByProcess $ dbIndexes db)
    , exchange <- exchanges
    , let amount = exchangeAmount exchange
    , amount >= minAmount && amount <= maxAmount
    , Just flow <- [M.lookup (exchangeFlowId exchange) (dbFlows db)]
    , Just proc <- [M.lookup procUUID (dbProcesses db)]
    ]

-- | Statistiques de la base de données
data DatabaseStats = DatabaseStats
    { statsProcessCount :: !Int
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
    { statsProcessCount = M.size (dbProcesses db)
    , statsFlowCount = M.size (dbFlows db)
    , statsExchangeCount = M.size (idxExchangeByFlow $ dbIndexes db)
    , statsTechnosphereFlows = length $ findFlowsByType db Technosphere
    , statsBiosphereFlows = length $ findFlowsByType db Biosphere
    , statsReferenceProducts = M.size (idxReferenceProducts $ dbIndexes db)
    , statsInputCount = sum [length inputs | inputs <- M.elems (idxInputsByProcess $ dbIndexes db)]
    , statsOutputCount = sum [length outputs | outputs <- M.elems (idxOutputsByProcess $ dbIndexes db)]
    , statsLocations = M.keys (idxByLocation $ dbIndexes db)
    , statsCategories = M.keys (idxFlowByCategory $ dbIndexes db)
    , statsUnits = S.toList $ S.fromList [flowUnit flow | flow <- M.elems (dbFlows db)]
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