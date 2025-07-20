{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module ACV.Types where

import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import Data.Text (Text)
import qualified Data.Map as M
import qualified Data.Set as S
import GHC.Generics (Generic, Generic1)
import Text.XML (Instruction (instructionData))

-- | Identifiant universel unique (généralement un UUID EcoSpold)
type UUID = Text

-- | Type de flux : Technosphère (échange entre processus) ou Biosphère (échange avec l'environnement)
data FlowType = Technosphere | Biosphere
    deriving (Eq, Ord, Show, Generic, NFData, Binary)

-- | Représentation d'un flux (matière, énergie, émission, etc.)
data Flow = Flow
    { flowId :: !UUID -- Identifiant du flux
    , flowName :: !Text -- Nom lisible
    , flowCategory :: !Text -- Catégorie (e.g. air, eau, ressource)
    , flowUnit :: !Text -- Unité (e.g. kg, MJ)
    , flowType :: !FlowType -- Type de flux
    }
    deriving (Eq, Show, Generic, NFData, Binary)

-- | Échange dans un procédé (entrée ou sortie) - Version optimisée avec référence au flux
data Exchange = Exchange
    { exchangeFlowId :: !UUID -- Référence vers le flux (dans FlowDB)
    , exchangeAmount :: !Double -- Quantité échangée
    , exchangeIsInput :: !Bool -- Vrai si c'est une entrée
    , exchangeIsReference :: !Bool -- Vrai si c'est le flux de référence (output principal)
    }
    deriving (Eq, Show, Generic, NFData, Binary)

-- | Procédé ACV de base (activité)
data Process = Process
    { processId :: !UUID -- Identifiant unique du procédé
    , processName :: !Text -- Nom
    , processLocation :: !Text -- Code de localisation (ex: FR, RER)
    , exchanges :: ![Exchange] -- Liste des échanges
    }
    deriving (Eq, Show, Generic, NFData, Binary)

-- | Arbre de calcul ACV (représentation récursive)
data ProcessTree
    = Leaf !Process
    | Node !Process ![(Double, ProcessTree)] -- Processus et sous-processus pondérés
    deriving (Eq, Show, Generic, Binary)

-- | Base de données des flux (dédupliquée)
type FlowDB = M.Map UUID Flow

-- | Base de données des procédés
type ProcessDB = M.Map UUID Process

-- | Index par nom de procédé - permet la recherche par nom (insensible à la casse)
type NameIndex = M.Map Text [UUID]  -- Nom -> Liste des UUIDs des procédés

-- | Index par localisation - permet la recherche par zone géographique
type LocationIndex = M.Map Text [UUID]  -- Location -> Liste des UUIDs des procédés

-- | Index par flux - permet de trouver quels procédés utilisent un flux donné
type FlowIndex = M.Map UUID [UUID]  -- FlowID -> Liste des UUIDs des procédés qui l'utilisent

-- | Index par catégorie de flux - permet la recherche par type de flux
type FlowCategoryIndex = M.Map Text [UUID]  -- Category -> Liste des UUIDs des flux

-- | Index par type de flux - sépare Technosphere/Biosphere
type FlowTypeIndex = M.Map FlowType [UUID]  -- FlowType -> Liste des UUIDs des flux

-- | Index des échanges par flux - permet de trouver tous les échanges utilisant un flux
type ExchangeIndex = M.Map UUID [(UUID, Exchange)]  -- FlowID -> [(ProcessID, Exchange)]

-- | Index des échanges par procédé - accès rapide aux échanges d'un procédé
type ProcessExchangeIndex = M.Map UUID [Exchange]  -- ProcessID -> [Exchange]

-- | Index des produits de référence - tous les outputs principaux
type ReferenceProductIndex = M.Map UUID (UUID, Exchange)  -- FlowID -> (ProcessID, Exchange)

-- | Index des entrées par procédé - sépare inputs/outputs pour recherches efficaces
type ProcessInputIndex = M.Map UUID [Exchange]  -- ProcessID -> [Input Exchanges]

-- | Index des sorties par procédé - sépare inputs/outputs pour recherches efficaces
type ProcessOutputIndex = M.Map UUID [Exchange]  -- ProcessID -> [Output Exchanges]

-- | Structure d'index complète pour recherches efficaces
data Indexes = Indexes
    { -- Index au niveau procédé
      idxByName :: !NameIndex           -- Recherche procédés par nom
    , idxByLocation :: !LocationIndex   -- Recherche procédés par localisation
    , idxByFlow :: !FlowIndex           -- Recherche procédés utilisant un flux
      -- Index au niveau flux
    , idxFlowByCategory :: !FlowCategoryIndex  -- Recherche flux par catégorie
    , idxFlowByType :: !FlowTypeIndex   -- Recherche flux par type
      -- Index au niveau échange
    , idxExchangeByFlow :: !ExchangeIndex       -- Tous les échanges par flux
    , idxExchangeByProcess :: !ProcessExchangeIndex  -- Tous les échanges par procédé
    , idxReferenceProducts :: !ReferenceProductIndex -- Produits de référence
    , idxInputsByProcess :: !ProcessInputIndex       -- Entrées par procédé
    , idxOutputsByProcess :: !ProcessOutputIndex     -- Sorties par procédé
    }
    deriving (Eq, Show, Generic, Binary)

-- | Base de données complète avec index pour recherches efficaces
data Database = Database
    { dbProcesses :: !ProcessDB
    , dbFlows :: !FlowDB
    , dbIndexes :: !Indexes
    }
    deriving (Eq, Show, Generic, Binary)

-- | Version simplifiée sans index (pour compatibilité)
data SimpleDatabase = SimpleDatabase
    { sdbProcesses :: !ProcessDB
    , sdbFlows :: !FlowDB
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
