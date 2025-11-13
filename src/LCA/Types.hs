{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module LCA.Types (
    module LCA.Types,
    UUID
) where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.Binary (Binary)
import qualified Data.Binary as Binary
import Data.Vector.Binary () -- Orphan instances for Vector Binary
import Data.Hashable (Hashable)
import Data.Int (Int16, Int32)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed.Mutable as VUM
import GHC.Generics (Generic, Generic1)

-- Note: UUID is now Data.UUID.UUID (16 bytes) instead of Text (~80+ bytes)
-- This saves ~2-3GB of RAM by reducing memory footprint from ~100,000+ UUID instances
-- Binary and NFData instances are provided by the uuid package

-- | Process identifier - compact Int16 index for efficient matrix operations
-- Maps to (activityUUID, productUUID) via Database.dbProcessIdTable
-- Based on EcoSpold filename pattern {activity_uuid}_{product_uuid}.spold
type ProcessId = Int16

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
    deriving (Generic, NFData, Binary)

-- | Représentation d'un flux (matière, énergie, émission, etc.)
data Flow = Flow
    { flowId :: !UUID -- Identifiant du flux
    , flowName :: !Text -- Nom lisible
    , flowCategory :: !Text -- Catégorie (e.g. air, eau, ressource)
    , flowUnitId :: !UUID -- Référence vers l'unité par défaut dans UnitDB
    , flowType :: !FlowType -- Type de flux
    , flowSynonyms :: !(M.Map Text (S.Set Text)) -- Synonymes par langue (e.g. "en" -> {"BaP", "benzo[a]pyrene"})
    }
    deriving (Generic, NFData, Binary)

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
exchangeActivityLinkId (TechnosphereExchange _ _ _ _ _ linkId _) =
    if linkId == UUID.nil then Nothing else Just linkId
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

-- | Activité ACV de base (activité)
-- Note: ProcessId is the index in dbActivities vector, UUIDs stored in dbProcessIdTable
data Activity = Activity
    { activityName :: !Text -- Nom
    , activityDescription :: ![Text] -- Description générale (generalComment) par paragraphes
    , activitySynonyms :: !(M.Map Text (S.Set Text)) -- Synonymes par langue comme les flux
    , activityClassification :: !(M.Map Text Text) -- Classifications (ISIC, CPC, etc.)
    , activityLocation :: !Text -- Code de localisation (ex: FR, RER)
    , activityUnit :: !Text -- Unité de référence
    , exchanges :: ![Exchange] -- Liste des échanges
    }
    deriving (Generic, NFData, Binary)

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

-- | Temporary Map structure used during database loading phase
-- Maps from (activityUUID, productUUID) pairs extracted from .spold filenames
-- This is converted to ActivityDB Vector during database construction
type ActivityMap = M.Map (UUID, UUID) Activity

-- | Base de données des activités - Vector for direct indexing by ProcessId (Int16)
-- Each spold file (activity_uuid_product_uuid.spold) becomes a separate entry
type ActivityDB = V.Vector Activity

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
-- Memory optimization: Removed unused exchange indexes that were duplicating 600K Exchange records
-- across 5 maps (idxExchangeByFlow, idxExchangeByActivity, idxReferenceProducts,
-- idxInputsByActivity, idxOutputsByActivity) - saves ~3-4GB RAM
data Indexes = Indexes
    { -- Index au niveau activité
      idxByName :: !NameIndex -- Recherche activités par nom
    , idxByLocation :: !LocationIndex -- Recherche activités par localisation
    , idxByFlow :: !FlowIndex -- Recherche activités utilisant un flux
    , idxByUnit :: !ActivityUnitIndex -- Recherche activités par unité de référence
    -- Index au niveau flux
    , idxFlowByCategory :: !FlowCategoryIndex -- Recherche flux par catégorie
    , idxFlowByType :: !FlowTypeIndex -- Recherche flux par type
    -- Note: Exchange-level indexes removed - exchanges can be accessed directly from Activity.exchanges
    }
    deriving (Generic, NFData, Binary)

-- | Sparse matrix coordinate triplet (row, col, value)
-- Using Int32 for matrix indices to support large databases (up to 2 billion activities)
-- Unboxed to eliminate per-element boxing overhead (~48 bytes → 16 bytes per triple)
-- With ~800K triples, this saves ~25MB + significant GC pressure
data SparseTriple = SparseTriple {-# UNPACK #-} !Int32 {-# UNPACK #-} !Int32 {-# UNPACK #-} !Double
    deriving (Eq, Show, Generic)

-- Manual Unbox instance for SparseTriple to enable VU.Vector storage
newtype instance VU.MVector s SparseTriple = MV_SparseTriple (VU.MVector s (Int32, Int32, Double))
newtype instance VU.Vector SparseTriple = V_SparseTriple (VU.Vector (Int32, Int32, Double))

instance VGM.MVector VU.MVector SparseTriple where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicOverlaps #-}
    {-# INLINE basicUnsafeNew #-}
    {-# INLINE basicInitialize #-}
    {-# INLINE basicUnsafeRead #-}
    {-# INLINE basicUnsafeWrite #-}
    basicLength (MV_SparseTriple v) = VGM.basicLength v
    basicUnsafeSlice i n (MV_SparseTriple v) = MV_SparseTriple $ VGM.basicUnsafeSlice i n v
    basicOverlaps (MV_SparseTriple v1) (MV_SparseTriple v2) = VGM.basicOverlaps v1 v2
    basicUnsafeNew n = MV_SparseTriple <$> VGM.basicUnsafeNew n
    basicInitialize (MV_SparseTriple v) = VGM.basicInitialize v
    basicUnsafeRead (MV_SparseTriple v) i = do
        (r, c, val) <- VGM.basicUnsafeRead v i
        return $ SparseTriple r c val
    basicUnsafeWrite (MV_SparseTriple v) i (SparseTriple r c val) =
        VGM.basicUnsafeWrite v i (r, c, val)

instance VG.Vector VU.Vector SparseTriple where
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeFreeze (MV_SparseTriple v) = V_SparseTriple <$> VG.basicUnsafeFreeze v
    basicUnsafeThaw (V_SparseTriple v) = MV_SparseTriple <$> VG.basicUnsafeThaw v
    basicLength (V_SparseTriple v) = VG.basicLength v
    basicUnsafeSlice i n (V_SparseTriple v) = V_SparseTriple $ VG.basicUnsafeSlice i n v
    basicUnsafeIndexM (V_SparseTriple v) i = do
        (r, c, val) <- VG.basicUnsafeIndexM v i
        return $ SparseTriple r c val

instance VU.Unbox SparseTriple

-- Binary instance for cache serialization (VU.Vector has Binary instance via vector-binary-instances)
instance Binary SparseTriple where
    put (SparseTriple r c v) = Binary.put r >> Binary.put c >> Binary.put v
    get = SparseTriple <$> Binary.get <*> Binary.get <*> Binary.get

-- NFData derived via Generic
instance NFData SparseTriple

-- | Pre-computed matrix factorization for fast inventory calculations
data MatrixFactorization = MatrixFactorization
    { mfSystemMatrix :: !(VU.Vector SparseTriple) -- Cached (I - A) system matrix (unboxed)
    , mfActivityCount :: !Int32 -- Matrix dimension
    } deriving (Generic, NFData, Binary)

-- | Base de données complète avec index pour recherches efficaces
data Database = Database
    { -- UUID interning tables for ProcessId ↔ (UUID, UUID) conversion
      dbProcessIdTable :: !(V.Vector (UUID, UUID)) -- ProcessId (Int16) → (activityUUID, productUUID)
    , dbProcessIdLookup :: !(M.Map (UUID, UUID) ProcessId) -- reverse lookup
    , dbActivities :: !ActivityDB -- Vector of activities indexed by ProcessId
    , dbFlows :: !FlowDB
    , dbUnits :: !UnitDB
    , dbIndexes :: !Indexes
    , -- Pre-computed sparse matrices for efficient LCA calculations (unboxed for memory efficiency)
      dbTechnosphereTriples :: !(VU.Vector SparseTriple) -- A matrix: activities × activities (sparse, unboxed)
    , dbBiosphereTriples :: !(VU.Vector SparseTriple) -- B matrix: biosphere flows × activities (sparse, unboxed)
    , dbActivityIndex :: !(V.Vector Int32) -- ProcessId → matrix index mapping (direct vector indexing)
    , dbBiosphereFlows :: !(V.Vector UUID) -- Ordered vector of biosphere flow UUIDs (source of truth for indexing, strict for memory efficiency)
    , dbActivityCount :: !Int32 -- Number of activities (matrix dimension)
    , dbBiosphereCount :: !Int32 -- Number of biosphere flows (matrix dimension)
    -- Cached factorization for concurrent inventory calculations (runtime only)
    , dbCachedFactorization :: !(Maybe MatrixFactorization) -- Pre-computed (I - A) for fast solves
    }
    deriving (Generic, NFData, Binary)

-- | Helper functions for ProcessId and Database operations

-- | Get activity by ProcessId (direct vector indexing)
getActivity :: Database -> ProcessId -> Maybe Activity
getActivity db pid
    | pid >= 0 && fromIntegral pid < V.length (dbActivities db) =
        Just $ dbActivities db V.! fromIntegral pid
    | otherwise = Nothing

-- | Find ProcessId from UUID pair
findProcessId :: Database -> UUID -> UUID -> Maybe ProcessId
findProcessId db actUUID prodUUID =
    M.lookup (actUUID, prodUUID) (dbProcessIdLookup db)

-- | Find any ProcessId matching an activity UUID
-- Returns the first ProcessId found with the given activity UUID.
-- ESSENTIAL for EcoSpold data: exchange links only contain activity UUIDs (not full ProcessIds),
-- so we must translate from UUID → ProcessId to handle multi-product activities.
findProcessIdByActivityUUID :: Database -> UUID -> Maybe ProcessId
findProcessIdByActivityUUID db searchUUID =
    case [pid | (pid, (actUUID, _)) <- zip [0..] (V.toList $ dbProcessIdTable db), actUUID == searchUUID] of
        (pid:_) -> Just (fromIntegral pid :: ProcessId)
        [] -> Nothing

-- | Find activity by activity UUID (returns first matching product)
-- ESSENTIAL for EcoSpold data: exchange links only contain activity UUIDs (not full ProcessIds).
-- When multiple products exist for one activity, this returns an arbitrary match.
findActivityByActivityUUID :: Database -> UUID -> Maybe Activity
findActivityByActivityUUID db searchUUID = do
    pid <- findProcessIdByActivityUUID db searchUUID
    getActivity db pid

-- | Convert ProcessId to UUID pair
processIdToUUIDs :: Database -> ProcessId -> Maybe (UUID, UUID)
processIdToUUIDs db pid
    | pid >= 0 && fromIntegral pid < V.length (dbProcessIdTable db) =
        Just $ dbProcessIdTable db V.! fromIntegral pid
    | otherwise = Nothing

-- | Convert ProcessId to Text representation for display (activityUUID_productUUID)
processIdToText :: Database -> ProcessId -> Text
processIdToText db pid =
    case processIdToUUIDs db pid of
        Just (actUUID, prodUUID) -> UUID.toText actUUID <> "_" <> UUID.toText prodUUID
        Nothing -> "invalid-process-id-" <> T.pack (show pid)

-- | Parse ProcessId from filename stem (requires Database for lookup)
parseProcessId :: Database -> Text -> Maybe ProcessId
parseProcessId db filename = case T.splitOn "_" filename of
    [actUUIDText, prodUUIDText] | not (T.null actUUIDText) && not (T.null prodUUIDText) ->
        case (UUID.fromText actUUIDText, UUID.fromText prodUUIDText) of
            (Just actUUID, Just prodUUID) -> findProcessId db actUUID prodUUID
            _ -> Nothing
    _ -> Nothing

-- | Version simplifiée sans index (pour compatibilité)
-- Used during database loading, before conversion to final Vector structure
data SimpleDatabase = SimpleDatabase
    { sdbActivities :: !ActivityMap  -- Temporary Map structure
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
-- Note: ProcessId is Int16, which already has ToJSON/FromJSON instances
instance ToJSON Exchange
instance ToJSON FlowType

instance FromJSON Exchange
