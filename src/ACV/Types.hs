{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ACV.Types where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic, Generic1)
import Text.XML (Instruction (instructionData))

-- | Identifiant universel unique (généralement un UUID EcoSpold)
type UUID = Text

-- | Type de flux : Technosphère (échange entre activités) ou Biosphère (échange avec l'environnement)
data FlowType = Technosphere | Biosphere
    deriving (Eq, Ord, Generic, NFData, Binary)

-- | Représentation d'une unité (kg, MJ, m³, etc.)
data Unit = Unit
    { unitId :: !UUID -- Identifiant unique de l'unité
    , unitName :: !Text -- Nom de l'unité (e.g. "kg", "kilogram")
    , unitSymbol :: !Text -- Symbole (e.g. "kg", "MJ")
    , unitComment :: !Text -- Description/commentaire
    }
    deriving (Generic, Binary)

-- | Représentation d'un flux (matière, énergie, émission, etc.)
data Flow = Flow
    { flowId :: !UUID -- Identifiant du flux
    , flowName :: !Text -- Nom lisible
    , flowCategory :: !Text -- Catégorie (e.g. air, eau, ressource)
    , flowUnitId :: !UUID -- Référence vers l'unité par défaut dans UnitDB
    , flowType :: !FlowType -- Type de flux
    , flowSynonyms :: !(M.Map Text (S.Set Text)) -- Synonymes par langue (e.g. "en" -> {"BaP", "benzo[a]pyrene"})
    }
    deriving (Generic, Binary)

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
    deriving (Generic, NFData, Binary)

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

-- | Get unit information for a flow
getUnitForFlow :: UnitDB -> Flow -> Maybe Unit
getUnitForFlow unitDB flow = M.lookup (flowUnitId flow) unitDB

-- | Get unit name for a flow (fallback to "unknown" if not found)
getUnitNameForFlow :: UnitDB -> Flow -> Text
getUnitNameForFlow unitDB flow =
    case getUnitForFlow unitDB flow of
        Just unit -> unitName unit
        Nothing -> "unknown"

-- | Get all synonyms across all languages
getAllSynonyms :: Flow -> [Text]
getAllSynonyms flow = concatMap S.toList $ M.elems (flowSynonyms flow)

-- | Activité ACV de base (activité)
data Activity = Activity
    { activityId :: !UUID -- Identifiant unique de l'activité
    , activityName :: !Text -- Nom
    , activityDescription :: ![Text] -- Description générale (generalComment) par paragraphes
    , activitySynonyms :: !(M.Map Text (S.Set Text)) -- Synonymes par langue comme les flux
    , activityClassification :: !(M.Map Text Text) -- Classifications (ISIC, CPC, etc.)
    , activityLocation :: !Text -- Code de localisation (ex: FR, RER)
    , activityUnit :: !Text -- Unité de référence
    , exchanges :: ![Exchange] -- Liste des échanges
    }
    deriving (Generic, Binary)

-- | Arbre de calcul ACV (représentation récursive)
data ActivityTree
    = Leaf !Activity
    | Node !Activity ![(Double, ActivityTree)] -- Activités et sous-activités pondérés

-- | Arbre avec détection de boucles pour export SVG
data LoopAwareTree
    = TreeLeaf !Activity
    | TreeNode !Activity ![(Double, Flow, LoopAwareTree)] -- Activity + (quantity, flow, subtree)
    | TreeLoop !UUID !Text !Int -- Loop reference: UUID + ActivityName + Depth

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

-- | Index par unité de référence - permet la recherche par unité
type ActivityUnitIndex = M.Map Text [UUID] -- Unit -> Liste des UUIDs des activités

-- | Structure d'index complète pour recherches efficaces
data Indexes = Indexes
    { -- Index au niveau activité
      idxByName :: !NameIndex -- Recherche activités par nom
    , idxByLocation :: !LocationIndex -- Recherche activités par localisation
    , idxByFlow :: !FlowIndex -- Recherche activités utilisant un flux
    , idxByUnit :: !ActivityUnitIndex -- Recherche activités par unité de référence
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

-- | Base de données complète avec index pour recherches efficaces
data Database = Database
    { dbActivities :: !ActivityDB
    , dbFlows :: !FlowDB
    , dbUnits :: !UnitDB
    , dbIndexes :: !Indexes
    }

-- | Version simplifiée sans index (pour compatibilité)
data SimpleDatabase = SimpleDatabase
    { sdbActivities :: !ActivityDB
    , sdbFlows :: !FlowDB
    , sdbUnits :: !UnitDB
    }
    deriving (Generic, Binary)

-- | Catégorie d'impact (e.g. Changement climatique)
data ImpactCategory = ImpactCategory
    { categoryId :: !Text
    , categoryName :: !Text
    }
    deriving (Eq, Ord, Show)

-- | Facteur de caractérisation (lié à une méthode LCIA)
data CF = CF
    { cfFlowId :: !UUID -- Flux biosphère concerné
    , cfCategory :: !ImpactCategory -- Catégorie d'impact
    , cfFactor :: !Double -- Facteur de caractérisation
    }

-- JSON instances for API compatibility
instance ToJSON Exchange
instance ToJSON FlowType

instance FromJSON Exchange
