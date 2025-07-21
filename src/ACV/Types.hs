{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ACV.Types where

import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic, Generic1)
import Text.XML (Instruction (instructionData))

-- | Identifiant universel unique (généralement un UUID EcoSpold)
type UUID = Text

-- | Type de flux : Technosphère (échange entre activityus) ou Biosphère (échange avec l'environnement)
data FlowType = Technosphere | Biosphere
    deriving (Eq, Ord, Show, Generic, NFData, Binary)

-- | Représentation d'une unité (kg, MJ, m³, etc.)
data Unit = Unit
    { unitId :: !UUID -- Identifiant unique de l'unité
    , unitName :: !Text -- Nom de l'unité (e.g. "kg", "kilogram")
    , unitSymbol :: !Text -- Symbole (e.g. "kg", "MJ")
    , unitComment :: !Text -- Description/commentaire
    }
    deriving (Eq, Show, Generic, NFData, Binary)

-- | Représentation d'un flux (matière, énergie, émission, etc.)
data Flow = Flow
    { flowId :: !UUID -- Identifiant du flux
    , flowName :: !Text -- Nom lisible
    , flowCategory :: !Text -- Catégorie (e.g. air, eau, ressource)
    , flowUnitId :: !UUID -- Référence vers l'unité par défaut dans UnitDB
    , flowType :: !FlowType -- Type de flux
    , flowSynonyms :: !(M.Map Text (S.Set Text)) -- Synonymes par langue (e.g. "en" -> {"BaP", "benzo[a]pyrene"})
    }
    deriving (Eq, Show, Generic, NFData, Binary)

-- | Échange dans un activité - Mirrors EcoSpold intermediateExchange/elementaryExchange structure
data Exchange
    = TechnosphereExchange
        { techFlowId :: !UUID -- Flow being exchanged
        , techAmount :: !Double -- Quantity exchanged
        , techUnitId :: !UUID -- Unit of measurement
        , techIsInput :: !Bool -- True if input
        , techIsReference :: !Bool -- True if reference product (main output)
        , techActivityLinkId :: !UUID -- Target activity ID (always present for technosphere)
        }
    | BiosphereExchange
        { bioFlowId :: !UUID -- Flow being exchanged
        , bioAmount :: !Double -- Quantity exchanged
        , bioUnitId :: !UUID -- Unit of measurement
        , bioIsInput :: !Bool -- True for resource extraction, False for emissions
        }
    deriving (Eq, Show, Generic, NFData, Binary)

-- | Helper functions for Exchange variants
exchangeFlowId :: Exchange -> UUID
exchangeFlowId (TechnosphereExchange fid _ _ _ _ _) = fid
exchangeFlowId (BiosphereExchange fid _ _ _) = fid

exchangeAmount :: Exchange -> Double
exchangeAmount (TechnosphereExchange _ amt _ _ _ _) = amt
exchangeAmount (BiosphereExchange _ amt _ _) = amt

exchangeUnitId :: Exchange -> UUID
exchangeUnitId (TechnosphereExchange _ _ uid _ _ _) = uid
exchangeUnitId (BiosphereExchange _ _ uid _) = uid

exchangeIsInput :: Exchange -> Bool
exchangeIsInput (TechnosphereExchange _ _ _ isInput _ _) = isInput
exchangeIsInput (BiosphereExchange _ _ _ isInput) = isInput

exchangeIsReference :: Exchange -> Bool
exchangeIsReference (TechnosphereExchange _ _ _ _ isRef _) = isRef
exchangeIsReference (BiosphereExchange _ _ _ _) = False -- Biosphere exchanges are never reference products

exchangeActivityLinkId :: Exchange -> Maybe UUID
exchangeActivityLinkId (TechnosphereExchange _ _ _ _ _ linkId) = Just linkId
exchangeActivityLinkId (BiosphereExchange _ _ _ _) = Nothing

-- | Check if exchange is technosphere
isTechnosphereExchange :: Exchange -> Bool
isTechnosphereExchange (TechnosphereExchange _ _ _ _ _ _) = True
isTechnosphereExchange (BiosphereExchange _ _ _ _) = False

-- | Check if exchange is biosphere
isBiosphereExchange :: Exchange -> Bool
isBiosphereExchange = not . isTechnosphereExchange

-- | Get unit information for an exchange
getUnitForExchange :: UnitDB -> Exchange -> Maybe Unit
getUnitForExchange unitDB exchange = M.lookup (exchangeUnitId exchange) unitDB

-- | Get unit name for an exchange (fallback to "unknown" if not found)
getUnitNameForExchange :: UnitDB -> Exchange -> Text
getUnitNameForExchange unitDB exchange =
    case getUnitForExchange unitDB exchange of
        Just unit -> unitName unit
        Nothing -> "unknown"

-- | Get unit symbol for an exchange (fallback to "?" if not found)
getUnitSymbolForExchange :: UnitDB -> Exchange -> Text
getUnitSymbolForExchange unitDB exchange =
    case getUnitForExchange unitDB exchange of
        Just unit -> unitSymbol unit
        Nothing -> "?"

-- | Get unit information for a flow
getUnitForFlow :: UnitDB -> Flow -> Maybe Unit
getUnitForFlow unitDB flow = M.lookup (flowUnitId flow) unitDB

-- | Get unit name for a flow (fallback to "unknown" if not found)
getUnitNameForFlow :: UnitDB -> Flow -> Text
getUnitNameForFlow unitDB flow =
    case getUnitForFlow unitDB flow of
        Just unit -> unitName unit
        Nothing -> "unknown"

-- | Get unit symbol for a flow (fallback to "?" if not found)
getUnitSymbolForFlow :: UnitDB -> Flow -> Text
getUnitSymbolForFlow unitDB flow =
    case getUnitForFlow unitDB flow of
        Just unit -> unitSymbol unit
        Nothing -> "?"

-- | Get synonyms for a specific language
getSynonymsForLanguage :: Flow -> Text -> [Text]
getSynonymsForLanguage flow lang =
    case M.lookup lang (flowSynonyms flow) of
        Nothing -> []
        Just syns -> S.toList syns

-- | Get all synonyms across all languages
getAllSynonyms :: Flow -> [Text]
getAllSynonyms flow = concatMap S.toList $ M.elems (flowSynonyms flow)

-- | Add synonym to a flow for a specific language
addSynonym :: Text -> Text -> Flow -> Flow
addSynonym lang synonym flow =
    flow
        { flowSynonyms = M.insertWith S.union lang (S.singleton synonym) (flowSynonyms flow)
        }

-- | Check if text matches flow name or any synonym
matchesFlowOrSynonym :: Text -> Flow -> Bool
matchesFlowOrSynonym searchText flow =
    let lowerSearch = T.toLower searchText
        lowerName = T.toLower (flowName flow)
        lowerSynonyms = map T.toLower (getAllSynonyms flow)
     in lowerSearch == lowerName || lowerSearch `elem` lowerSynonyms

-- | Activité ACV de base (activité)
data Activity = Activity
    { activityId :: !UUID -- Identifiant unique du activité
    , activityName :: !Text -- Nom
    , activityDescription :: ![Text] -- Description générale (generalComment) par paragraphes
    , activitySynonyms :: !(M.Map Text (S.Set Text)) -- Synonymes par langue comme les flux
    , activityClassification :: !(M.Map Text Text) -- Classifications (ISIC, CPC, etc.)
    , activityLocation :: !Text -- Code de localisation (ex: FR, RER)
    , activityUnit :: !Text -- Unité de référence
    , exchanges :: ![Exchange] -- Liste des échanges
    }
    deriving (Eq, Show, Generic, NFData, Binary)

-- | Arbre de calcul ACV (représentation récursive)
data ActivityTree
    = Leaf !Activity
    | Node !Activity ![(Double, ActivityTree)] -- Activityus et sous-activityus pondérés
    deriving (Eq, Show, Generic, Binary)

-- | Base de données des flux (dédupliquée)
type FlowDB = M.Map UUID Flow

-- | Base de données des unités (dédupliquée)
type UnitDB = M.Map UUID Unit

-- | Base de données des activités
type ActivityDB = M.Map UUID Activity

-- | Index par nom de activité - permet la recherche par nom (insensible à la casse)
type NameIndex = M.Map Text [UUID] -- Nom -> Liste des UUIDs des activités

-- | Index par localisation - permet la recherche par zone géographique
type LocationIndex = M.Map Text [UUID] -- Location -> Liste des UUIDs des activités

-- | Index par flux - permet de trouver quels activités utilisent un flux donné
type FlowIndex = M.Map UUID [UUID] -- FlowID -> Liste des UUIDs des activités qui l'utilisent

-- | Index par catégorie de flux - permet la recherche par type de flux
type FlowCategoryIndex = M.Map Text [UUID] -- Category -> Liste des UUIDs des flux

-- | Index par type de flux - sépare Technosphere/Biosphere
type FlowTypeIndex = M.Map FlowType [UUID] -- FlowType -> Liste des UUIDs des flux

-- | Index des échanges par flux - permet de trouver tous les échanges utilisant un flux
type ExchangeIndex = M.Map UUID [(UUID, Exchange)] -- FlowID -> [(ActivityID, Exchange)]

-- | Index des échanges par activité - accès rapide aux échanges d'un activité
type ActivityExchangeIndex = M.Map UUID [Exchange] -- ActivityID -> [Exchange]

-- | Index des produits de référence - tous les outputs principaux
type ReferenceProductIndex = M.Map UUID (UUID, Exchange) -- FlowID -> (ActivityID, Exchange)

-- | Index des entrées par activité - sépare inputs/outputs pour recherches efficaces
type ActivityInputIndex = M.Map UUID [Exchange] -- ActivityID -> [Input Exchanges]

-- | Index des sorties par activité - sépare inputs/outputs pour recherches efficaces
type ActivityOutputIndex = M.Map UUID [Exchange] -- ActivityID -> [Output Exchanges]

-- | Structure d'index complète pour recherches efficaces
data Indexes = Indexes
    { -- Index au niveau activité
      idxByName :: !NameIndex -- Recherche activités par nom
    , idxByLocation :: !LocationIndex -- Recherche activités par localisation
    , idxByFlow :: !FlowIndex -- Recherche activités utilisant un flux
    -- Index au niveau flux
    , idxFlowByCategory :: !FlowCategoryIndex -- Recherche flux par catégorie
    , idxFlowByType :: !FlowTypeIndex -- Recherche flux par type
    -- Index au niveau échange
    , idxExchangeByFlow :: !ExchangeIndex -- Tous les échanges par flux
    , idxExchangeByActivity :: !ActivityExchangeIndex -- Tous les échanges par activité
    , idxReferenceProducts :: !ReferenceProductIndex -- Produits de référence
    , idxInputsByActivity :: !ActivityInputIndex -- Entrées par activité
    , idxOutputsByActivity :: !ActivityOutputIndex -- Sorties par activité
    }
    deriving (Eq, Show, Generic, Binary)

-- | Base de données complète avec index pour recherches efficaces
data Database = Database
    { dbActivities :: !ActivityDB
    , dbFlows :: !FlowDB
    , dbUnits :: !UnitDB
    , dbIndexes :: !Indexes
    }
    deriving (Eq, Show, Generic, Binary)

-- | Version simplifiée sans index (pour compatibilité)
data SimpleDatabase = SimpleDatabase
    { sdbActivities :: !ActivityDB
    , sdbFlows :: !FlowDB
    , sdbUnits :: !UnitDB
    }
    deriving (Eq, Show, Generic, Binary)

-- | Catégorie d'impact (e.g. Changement climatique)
data ImpactCategory = ImpactCategory
    { categoryId :: !Text
    , categoryName :: !Text
    }
    deriving (Eq, Ord, Show, Generic, Binary)

-- | Facteur de caractérisation (lié à une méthode LCIA)
data CF = CF
    { cfFlowId :: !UUID -- Flux biosphère concerné
    , cfCategory :: !ImpactCategory -- Catégorie d'impact
    , cfFactor :: !Double -- Facteur de caractérisation
    }
    deriving (Eq, Show, Generic, Binary)
