{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module ACV.Types where

import Control.DeepSeq (NFData)
import Data.Text (Text)
import qualified Data.Map as M
import GHC.Generics (Generic, Generic1)
import Text.XML (Instruction (instructionData))

-- | Identifiant universel unique (généralement un UUID EcoSpold)
type UUID = Text

-- | Type de flux : Technosphère (échange entre processus) ou Biosphère (échange avec l'environnement)
data FlowType = Technosphere | Biosphere
    deriving (Eq, Show, Generic, NFData)

-- | Représentation d'un flux (matière, énergie, émission, etc.)
data Flow = Flow
    { flowId :: !UUID -- Identifiant du flux
    , flowName :: !Text -- Nom lisible
    , flowCategory :: !Text -- Catégorie (e.g. air, eau, ressource)
    , flowUnit :: !Text -- Unité (e.g. kg, MJ)
    , flowType :: !FlowType -- Type de flux
    }
    deriving (Eq, Show, Generic, NFData)

-- | Échange dans un procédé (entrée ou sortie) - Version optimisée avec référence au flux
data Exchange = Exchange
    { exchangeFlowId :: !UUID -- Référence vers le flux (dans FlowDB)
    , exchangeAmount :: !Double -- Quantité échangée
    , exchangeIsInput :: !Bool -- Vrai si c'est une entrée
    , exchangeIsReference :: !Bool -- Vrai si c'est le flux de référence (output principal)
    }
    deriving (Eq, Show, Generic, NFData)

-- | Procédé ACV de base (activité)
data Process = Process
    { processId :: !UUID -- Identifiant unique du procédé
    , processName :: !Text -- Nom
    , processLocation :: !Text -- Code de localisation (ex: FR, RER)
    , exchanges :: ![Exchange] -- Liste des échanges
    }
    deriving (Eq, Show, Generic, NFData)

-- | Arbre de calcul ACV (représentation récursive)
data ProcessTree
    = Leaf !Process
    | Node !Process ![(Double, ProcessTree)] -- Processus et sous-processus pondérés
    deriving (Eq, Show)

-- | Base de données des flux (dédupliquée)
type FlowDB = M.Map UUID Flow

-- | Base de données des procédés
type ProcessDB = M.Map UUID Process

-- | Base de données complète (procédés + flux)
data Database = Database
    { dbProcesses :: !ProcessDB
    , dbFlows :: !FlowDB
    }
    deriving (Eq, Show)

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
    deriving (Eq, Show)
