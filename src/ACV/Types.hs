{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ACV.Types where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Hashable (Hashable)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic, Generic1)
import Text.XML (Instruction (instructionData))

-- | Identifiant universel unique (généralement un UUID EcoSpold)
type UUID = Text

-- | Process identifier based on EcoSpold filename pattern {activity_uuid}_{product_uuid}.spold
-- This ensures each .spold file becomes a distinct process after cut-off allocation
data ProcessId = ProcessId
    { activityUUID :: !UUID  -- Activity UUID from filename
    , productUUID :: !UUID   -- Product UUID from filename
    }
    deriving (Eq, Ord, Show, Generic, NFData, Binary, Hashable)

-- | Helper functions for ProcessId
mkProcessId :: UUID -> UUID -> ProcessId
mkProcessId = ProcessId

-- | Convert ProcessId to Text representation for display
processIdToText :: ProcessId -> Text
processIdToText (ProcessId actUUID prodUUID) = actUUID <> "_" <> prodUUID

-- | Parse ProcessId from filename stem (without extension)
parseProcessId :: Text -> Maybe ProcessId
parseProcessId filename = case T.splitOn "_" filename of
    [actUUID, prodUUID] | not (T.null actUUID) && not (T.null prodUUID) ->
        Just $ ProcessId actUUID prodUUID
    _ -> Nothing

-- | Extract activity UUID from ProcessId for backward compatibility
getActivityUUID :: ProcessId -> UUID
getActivityUUID (ProcessId actUUID _) = actUUID

-- | Extract product UUID from ProcessId
getProductUUID :: ProcessId -> UUID
getProductUUID (ProcessId _ prodUUID) = prodUUID

-- | Create ProcessId from activity UUID (using same UUID for both parts)
-- Temporary function for backward compatibility
processIdFromActivityUUID :: UUID -> ProcessId
processIdFromActivityUUID uuid = ProcessId uuid uuid

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
        , techActivityLinkId :: !UUID -- Target activity ID (backward compatibility)
        , techProcessLinkId :: !(Maybe ProcessId) -- Target process ID (new field)
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
exchangeFlowId (TechnosphereExchange fid _ _ _ _ _ _) = fid
exchangeFlowId (BiosphereExchange fid _ _ _) = fid

exchangeAmount :: Exchange -> Double
exchangeAmount (TechnosphereExchange _ amt _ _ _ _ _) = amt
exchangeAmount (BiosphereExchange _ amt _ _) = amt

exchangeUnitId :: Exchange -> UUID
exchangeUnitId (TechnosphereExchange _ _ uid _ _ _ _) = uid
exchangeUnitId (BiosphereExchange _ _ uid _) = uid

exchangeIsInput :: Exchange -> Bool
exchangeIsInput (TechnosphereExchange _ _ _ isInput _ _ _) = isInput
exchangeIsInput (BiosphereExchange _ _ _ isInput) = isInput

exchangeIsReference :: Exchange -> Bool
exchangeIsReference (TechnosphereExchange _ _ _ _ isRef _ _) = isRef
exchangeIsReference (BiosphereExchange _ _ _ _) = False -- Biosphere exchanges are never reference products

-- | Get activity link ID (backward compatibility)
exchangeActivityLinkId :: Exchange -> Maybe UUID
exchangeActivityLinkId (TechnosphereExchange _ _ _ _ _ linkId _) = Just linkId
exchangeActivityLinkId (BiosphereExchange _ _ _ _) = Nothing

-- | Get process link ID (new field)
exchangeProcessLinkId :: Exchange -> Maybe ProcessId
exchangeProcessLinkId (TechnosphereExchange _ _ _ _ _ _ processLinkId) = processLinkId
exchangeProcessLinkId (BiosphereExchange _ _ _ _) = Nothing

-- | Check if exchange is technosphere
isTechnosphereExchange :: Exchange -> Bool
isTechnosphereExchange (TechnosphereExchange _ _ _ _ _ _ _) = True
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
    { activityId :: !UUID -- Identifiant unique de l'activité (backward compatibility)
    , activityProcessId :: !(Maybe ProcessId) -- New ProcessId field for filename-based identification
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

-- | Base de données des activités - keyed by ProcessId to support multi-product activities
-- Each spold file (activity_uuid_product_uuid.spold) becomes a separate entry
type ActivityDB = M.Map ProcessId Activity

-- | Find any ProcessId matching an activity UUID
-- Returns the first ProcessId found with the given activity UUID.
-- ESSENTIAL for EcoSpold data: exchange links only contain activity UUIDs (not full ProcessIds),
-- so we must translate from UUID → ProcessId to handle multi-product activities.
findProcessIdByActivityUUID :: ActivityDB -> UUID -> Maybe ProcessId
findProcessIdByActivityUUID activityDB searchUUID =
    case [pid | (pid, _) <- M.toList activityDB, activityUUID pid == searchUUID] of
        (pid:_) -> Just pid
        [] -> Nothing

-- | Find activity by activity UUID (returns first matching product)
-- ESSENTIAL for EcoSpold data: exchange links only contain activity UUIDs (not full ProcessIds).
-- When multiple products exist for one activity, this returns an arbitrary match.
findActivityByActivityUUID :: ActivityDB -> UUID -> Maybe Activity
findActivityByActivityUUID activityDB searchUUID =
    case [act | (pid, act) <- M.toList activityDB, activityUUID pid == searchUUID] of
        (act:_) -> Just act
        [] -> Nothing

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
type ReferenceProductIndex = M.Map UUID [(UUID, Exchange)] -- FlowID -> [(ActivityID, Exchange)]

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
    deriving (Generic, Binary)

-- | Sparse matrix coordinate triplet (row, col, value)
type SparseTriple = (Int, Int, Double)

-- | Pre-computed matrix factorization for fast inventory calculations
data MatrixFactorization = MatrixFactorization
    { mfSystemMatrix :: ![SparseTriple] -- Cached (I - A) system matrix
    , mfActivityCount :: !Int -- Matrix dimension
    } deriving (Generic, Binary)

-- | Base de données complète avec index pour recherches efficaces
data Database = Database
    { dbActivities :: !ActivityDB
    , dbFlows :: !FlowDB
    , dbUnits :: !UnitDB
    , dbIndexes :: !Indexes
    , -- Pre-computed sparse matrices for efficient LCA calculations
      dbTechnosphereTriples :: ![SparseTriple] -- A matrix: activities × activities (sparse)
    , dbBiosphereTriples :: ![SparseTriple] -- B matrix: biosphere flows × activities (sparse)
    , dbActivityIndex :: !(M.Map ProcessId Int) -- ProcessId → matrix index mapping
    , dbBiosphereFlows :: ![UUID] -- Ordered list of biosphere flow UUIDs (source of truth for indexing)
    , dbActivityCount :: !Int -- Number of activities (matrix dimension)
    , dbBiosphereCount :: !Int -- Number of biosphere flows (matrix dimension)
    -- Cached factorization for concurrent inventory calculations (runtime only)
    , dbCachedFactorization :: !(Maybe MatrixFactorization) -- Pre-computed (I - A) for fast solves
    }
    deriving (Generic, Binary)

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
instance ToJSON ProcessId
instance ToJSON Exchange
instance ToJSON FlowType

instance FromJSON ProcessId
instance FromJSON Exchange
