{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Cross-Database Activity Linking
--
-- When loading databases that depend on other databases (e.g., Ginko 2025 depends on
-- Agribalyse 3.2), this module provides functions to resolve supplier references
-- by searching across all loaded databases.
--
-- The matching algorithm uses:
-- 1. Product name matching (exact → synonym)
-- 2. Location matching with hierarchy fallback
-- 3. Unit compatibility checking
--
-- A candidate must score above a threshold to be automatically linked.
--
-- Performance: Uses pre-built indexes for O(1) product name lookup instead of
-- O(n) linear scans.
module EcoSpold.CrossDatabaseLinking
    ( -- * Types
      LinkingContext(..)
    , CrossDBCandidate(..)
    , CrossDBLinkResult(..)
    , LinkWarning(..)
    , LinkBlocker(..)
    , IndexedDatabase(..)
    , SupplierEntry(..)
      -- * Configuration
    , defaultLinkingThreshold
      -- * Index Building
    , buildIndexedDatabase
    , buildIndexedDatabaseFromDB
      -- * Main Functions
    , findSupplierAcrossDatabases
    , findSupplierInIndexedDBs
      -- * Scoring Functions
    , matchProductName
    , matchLocation
      -- * Location Hierarchy
    , isSubregionOf
    , locationHierarchy
      -- * Compound Name Parsing
    , extractProductPrefixes
    , extractBracketedLocation
      -- * Re-exports
    , normalizeText
    ) where

import Data.List (maximumBy)
import Data.Maybe (mapMaybe, fromMaybe)
import qualified Data.Map.Strict as M
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)

import LCA.SynonymDB (SynonymDB, lookupSynonymGroup, normalizeName)
import qualified LCA.UnitConversion as UC
import LCA.Types (SimpleDatabase(..), Database(..), Activity(..), Exchange(..), Flow(..), UUID, getActivity)
import qualified Data.Vector as V

-- | Pre-indexed database for fast cross-DB supplier lookup
-- Built once when database is loaded, reused for all lookups
data IndexedDatabase = IndexedDatabase
    { idbName           :: !Text
    , idbByProductName  :: !(M.Map Text [SupplierEntry])  -- Normalized product name → suppliers
    , idbBySynonymGroup :: !(M.Map Int [SupplierEntry])   -- Synonym group ID → suppliers
    }

-- | Entry in the supplier index
-- Contains only the metadata needed for cross-DB linking (no Activity/Flow import)
data SupplierEntry = SupplierEntry
    { seActivityUUID :: !UUID
    , seProductUUID  :: !UUID
    , seLocation     :: !Text
    , seUnit         :: !Text
    , seProductName  :: !Text        -- Product name for display/debugging
    }

-- | Context for cross-database linking (with pre-built indexes)
data LinkingContext = LinkingContext
    { lcIndexedDatabases :: ![IndexedDatabase]  -- ^ Pre-indexed databases to search
    , lcSynonymDB        :: !SynonymDB          -- ^ For product name matching
    , lcUnitConfig       :: !UC.UnitConfig      -- ^ For unit compatibility
    , lcThreshold        :: !Int                -- ^ Minimum score to auto-link (default: 60)
    }

-- | A candidate supplier from another database
data CrossDBCandidate = CrossDBCandidate
    { cdbActivityUUID  :: !UUID        -- ^ Activity UUID in the other database
    , cdbProductUUID   :: !UUID        -- ^ Reference product UUID
    , cdbDatabaseName  :: !Text        -- ^ Name of the source database
    , cdbScore         :: !Int         -- ^ Match score (higher = better)
    , cdbLocation      :: !Text        -- ^ Location of the activity
    , cdbProductName   :: !Text        -- ^ Product name for display/debugging
    }

-- | Result of cross-database linking attempt
data CrossDBLinkResult
    = CrossDBLinked !UUID !UUID !Text !Int !Text !Text ![LinkWarning]
      -- ^ Success: actUUID, prodUUID, dbName, score, productName, location, warnings
    | CrossDBNotLinked !LinkBlocker
      -- ^ Failed: reason for failure

-- | Non-blocking warning: link succeeded but with caveats
data LinkWarning
    = UpperLocationUsed !Text !Text    -- ^ requestedLoc, actualLoc (e.g. FR → RER)
    deriving (Show, Eq)

-- | Blocking reason: link failed, here's why
data LinkBlocker
    = NoNameMatch                      -- ^ Product not found at all
    | UnitIncompatible !Text !Text     -- ^ queryUnit, supplierUnit
    | LocationUnavailable !Text        -- ^ requestedLoc (no fallback found above threshold)
    deriving (Show, Eq)

-- | Default threshold for automatic linking
-- Requires at minimum: product name match (45-50) + some location match (10+)
defaultLinkingThreshold :: Int
defaultLinkingThreshold = 55

-- | Normalize text for matching: lowercase and strip whitespace
normalizeText :: Text -> Text
normalizeText = T.toLower . T.strip

-- | Separators that may appear between product name and additional info
-- in SimaPro compound process names (e.g. "product//[GLO] activity name").
-- Ordered by priority.
compoundSeparators :: [Text]
compoundSeparators = ["//", " {", " [", " |"]

-- | Extract product name prefixes from a compound name.
-- Tries splitting at each separator and returns candidate prefixes (stripped).
-- Returns empty list if no separator is found (name is already clean).
extractProductPrefixes :: Text -> [Text]
extractProductPrefixes name =
    [ T.strip prefix
    | sep <- compoundSeparators
    , let (prefix, rest) = T.breakOn sep name
    , not (T.null rest)       -- separator was found
    , not (T.null prefix)     -- non-empty prefix
    ]

-- | Extract a location code from any bracket pattern in a name.
-- Looks for [XX] first, then {XX}. Returns the content of the first match,
-- or empty text if no brackets found.
extractBracketedLocation :: Text -> Text
extractBracketedLocation name =
    case extractFromBrackets '[' ']' name of
        Just loc | not (T.null loc) -> loc
        _ -> case extractFromBrackets '{' '}' name of
            Just loc -> loc
            Nothing  -> ""
  where
    extractFromBrackets :: Char -> Char -> Text -> Maybe Text
    extractFromBrackets open close txt =
        let (_, afterOpen) = T.breakOn (T.singleton open) txt
        in if T.null afterOpen
            then Nothing
            else let inside = T.drop 1 afterOpen  -- skip the open bracket
                     (content, afterClose) = T.breakOn (T.singleton close) inside
                 in if T.null afterClose
                    then Nothing
                    else Just (T.strip content)

-- | Build an indexed database for fast cross-DB lookups
-- This should be called once when a database is loaded
buildIndexedDatabase :: Text -> SynonymDB -> SimpleDatabase -> IndexedDatabase
buildIndexedDatabase dbName synDB db =
    let entries = buildSupplierEntries db
        -- Index by normalized product name
        byName = M.fromListWith (++)
            [ (normalizeText prodName, [entry])
            | (prodName, entry) <- entries
            ]
        -- Index by synonym group (for synonym matching)
        bySynonym = M.fromListWith (++)
            [ (groupId, [entry])
            | (prodName, entry) <- entries
            , Just groupId <- [lookupSynonymGroup synDB (normalizeName prodName)]
            ]
    in IndexedDatabase
        { idbName = dbName
        , idbByProductName = byName
        , idbBySynonymGroup = bySynonym
        }

-- | Build supplier entries from a SimpleDatabase
buildSupplierEntries :: SimpleDatabase -> [(Text, SupplierEntry)]
buildSupplierEntries db =
    [ (flowName flow, SupplierEntry actUUID prodUUID (activityLocation act) (activityUnit act) (flowName flow))
    | ((actUUID, prodUUID), act) <- M.toList (sdbActivities db)
    , ex <- exchanges act
    , isReferenceExchange ex
    , Just flow <- [M.lookup (getExchangeFlowId ex) (sdbFlows db)]
    ]
  where
    isReferenceExchange :: Exchange -> Bool
    isReferenceExchange (TechnosphereExchange _ _ _ _ isRef _ _ _) = isRef
    isReferenceExchange _ = False

    getExchangeFlowId :: Exchange -> UUID
    getExchangeFlowId (TechnosphereExchange fid _ _ _ _ _ _ _) = fid
    getExchangeFlowId (BiosphereExchange fid _ _ _ _) = fid

-- | Build an indexed database from a full Database (used when loading from cache)
-- This is the preferred method as it works with cached databases
buildIndexedDatabaseFromDB :: Text -> SynonymDB -> Database -> IndexedDatabase
buildIndexedDatabaseFromDB dbName synDB db =
    let entries = buildSupplierEntriesFromDB db
        -- Index by normalized product name
        byName = M.fromListWith (++)
            [ (normalizeText prodName, [entry])
            | (prodName, entry) <- entries
            ]
        -- Index by synonym group (for synonym matching)
        bySynonym = M.fromListWith (++)
            [ (groupId, [entry])
            | (prodName, entry) <- entries
            , Just groupId <- [lookupSynonymGroup synDB (normalizeName prodName)]
            ]
    in IndexedDatabase
        { idbName = dbName
        , idbByProductName = byName
        , idbBySynonymGroup = bySynonym
        }

-- | Build supplier entries from a full Database
buildSupplierEntriesFromDB :: Database -> [(Text, SupplierEntry)]
buildSupplierEntriesFromDB db =
    [ (flowName flow, SupplierEntry actUUID prodUUID (activityLocation act) (activityUnit act) (flowName flow))
    | (pid, (actUUID, prodUUID)) <- zip [0..] (V.toList (dbProcessIdTable db))
    , Just act <- [getActivity db (fromIntegral pid)]
    , ex <- exchanges act
    , isReferenceExchange ex
    , Just flow <- [M.lookup (getExchangeFlowId ex) (dbFlows db)]
    ]
  where
    isReferenceExchange :: Exchange -> Bool
    isReferenceExchange (TechnosphereExchange _ _ _ _ isRef _ _ _) = isRef
    isReferenceExchange _ = False

    getExchangeFlowId :: Exchange -> UUID
    getExchangeFlowId (TechnosphereExchange fid _ _ _ _ _ _ _) = fid
    getExchangeFlowId (BiosphereExchange fid _ _ _ _) = fid

-- | Find a supplier across all loaded databases (using pre-built indexes)
-- This is the fast O(1) lookup version
findSupplierInIndexedDBs
    :: LinkingContext
    -> Text           -- ^ Product name to find
    -> Text           -- ^ Location of the consumer
    -> Text           -- ^ Unit of the exchange
    -> CrossDBLinkResult
findSupplierInIndexedDBs LinkingContext{..} productName location unit =
    let normalizedName = normalizeText productName
        -- Try exact match first (O(1) lookup)
        exactCandidates = concatMap (lookupExact normalizedName) lcIndexedDatabases
        -- Try synonym match if no exact match
        synonymCandidates = if null exactCandidates
            then case lookupSynonymGroup lcSynonymDB (normalizeName productName) of
                Just groupId -> concatMap (lookupBySynonym groupId) lcIndexedDatabases
                Nothing -> []
            else []
        -- Fallback: try prefix-based splitting for compound names (e.g. SimaPro)
        prefixCandidates = if null exactCandidates && null synonymCandidates
            then tryPrefixes (extractProductPrefixes productName)
            else []
        allCandidates = exactCandidates ++ synonymCandidates ++ prefixCandidates
        -- Effective location: if raw location is empty, try extracting from compound name
        effectiveLocation = if T.null location
            then extractBracketedLocation productName
            else location
    in if null allCandidates
        then CrossDBNotLinked NoNameMatch
        else
            -- Check unit compatibility first
            let unitCompatible = filter (\(_, se) -> unitsAreCompatible lcUnitConfig unit (seUnit se)) allCandidates
            in if null unitCompatible
                then
                    -- All candidates failed unit check — report the first supplier's unit
                    let (_, firstSe) = head allCandidates
                    in CrossDBNotLinked (UnitIncompatible unit (seUnit firstSe))
                else
                    -- Score by effective location
                    let scoredCandidates = map (scoreEntry effectiveLocation) unitCompatible
                        !best = maximumBy (comparing cdbScore) scoredCandidates
                    in if cdbScore best >= lcThreshold
                        then
                            -- Build warnings
                            let warnings = buildWarnings effectiveLocation (cdbLocation best)
                            in CrossDBLinked (cdbActivityUUID best) (cdbProductUUID best)
                                            (cdbDatabaseName best) (cdbScore best)
                                            (cdbProductName best) (cdbLocation best) warnings
                        else CrossDBNotLinked (LocationUnavailable effectiveLocation)
  where
    lookupExact :: Text -> IndexedDatabase -> [(Text, SupplierEntry)]
    lookupExact name idb =
        [(idbName idb, entry) | entry <- fromMaybe [] (M.lookup name (idbByProductName idb))]

    lookupBySynonym :: Int -> IndexedDatabase -> [(Text, SupplierEntry)]
    lookupBySynonym groupId idb =
        [(idbName idb, entry) | entry <- fromMaybe [] (M.lookup groupId (idbBySynonymGroup idb))]

    -- Try each prefix from compound name splitting, return first match
    tryPrefixes :: [Text] -> [(Text, SupplierEntry)]
    tryPrefixes [] = []
    tryPrefixes (p:ps) =
        let normalized = normalizeText p
            candidates = concatMap (lookupExact normalized) lcIndexedDatabases
        in if null candidates
            then -- Also try synonym match for this prefix
                case lookupSynonymGroup lcSynonymDB (normalizeName p) of
                    Just groupId ->
                        let synCandidates = concatMap (lookupBySynonym groupId) lcIndexedDatabases
                        in if null synCandidates then tryPrefixes ps else synCandidates
                    Nothing -> tryPrefixes ps
            else candidates

    scoreEntry :: Text -> (Text, SupplierEntry) -> CrossDBCandidate
    scoreEntry queryLoc (dbName, SupplierEntry{..}) =
        let locScore = matchLocation queryLoc seLocation
            nameScore = 50
            !totalScore = nameScore + locScore
        in CrossDBCandidate
            { cdbActivityUUID = seActivityUUID
            , cdbProductUUID = seProductUUID
            , cdbDatabaseName = dbName
            , cdbScore = totalScore
            , cdbLocation = seLocation
            , cdbProductName = seProductName
            }

    buildWarnings :: Text -> Text -> [LinkWarning]
    buildWarnings queryLoc actualLoc
        | queryLoc == actualLoc = []
        | otherwise = [UpperLocationUsed queryLoc actualLoc]

-- | Legacy function for backward compatibility (slower, builds indexes on the fly)
findSupplierAcrossDatabases
    :: LinkingContext
    -> Text           -- ^ Product name to find
    -> Text           -- ^ Location of the consumer
    -> Text           -- ^ Unit of the exchange
    -> CrossDBLinkResult
findSupplierAcrossDatabases ctx productName location unit =
    -- Just delegate to the indexed version
    findSupplierInIndexedDBs ctx productName location unit

-- | Match product names (simplified - just for scoring display)
-- Actual matching is done via index lookup
matchProductName :: SynonymDB -> Text -> Text -> Int
matchProductName synDB query candidate
    | normalizeText query == normalizeText candidate = 50  -- Exact match
    | areSynonyms synDB query candidate              = 45  -- Synonym match
    | otherwise                                      = 0   -- No match

-- | Check if two names are synonyms using the SynonymDB
areSynonyms :: SynonymDB -> Text -> Text -> Bool
areSynonyms synDB name1 name2 =
    case (lookupSynonymGroup synDB (normalizeName name1),
          lookupSynonymGroup synDB (normalizeName name2)) of
        (Just g1, Just g2) -> g1 == g2
        _ -> False

-- | Match locations with hierarchy fallback
--
-- Returns:
--   30 = Exact match
--   20 = Subregion match (e.g., FR ⊂ Europe)
--   10 = Global fallback (GLO or RoW)
--    5 = Different but not blocking
matchLocation :: Text -> Text -> Int
matchLocation queryLoc candidateLoc
    | queryLoc == candidateLoc                        = 30  -- Exact
    | isSubregionOf queryLoc candidateLoc             = 20  -- FR ⊂ Europe
    | candidateLoc `elem` ["GLO", "RoW"]              = 10  -- Global fallback
    | otherwise                                       = 5   -- Different but not blocking

-- | Check if one location is a subregion of another
isSubregionOf :: Text -> Text -> Bool
isSubregionOf child parent =
    case M.lookup child locationHierarchy of
        Just parents -> parent `elem` parents
        Nothing      -> False

-- | Location hierarchy for common LCA regions
-- Maps a location code to its parent regions
locationHierarchy :: M.Map Text [Text]
locationHierarchy = M.fromList
    [ -- European countries → regional/continental groupings
      ("FR", ["Europe", "EU", "RER", "ENTSO-E", "GLO", "RoW"])
    , ("DE", ["Europe", "EU", "RER", "ENTSO-E", "GLO", "RoW"])
    , ("IT", ["Europe", "EU", "RER", "ENTSO-E", "GLO", "RoW"])
    , ("ES", ["Europe", "EU", "RER", "ENTSO-E", "GLO", "RoW"])
    , ("GB", ["Europe", "RER", "ENTSO-E", "GLO", "RoW"])
    , ("UK", ["Europe", "RER", "ENTSO-E", "GLO", "RoW"])
    , ("PL", ["Europe", "EU", "RER", "ENTSO-E", "GLO", "RoW"])
    , ("NL", ["Europe", "EU", "RER", "ENTSO-E", "GLO", "RoW"])
    , ("BE", ["Europe", "EU", "RER", "ENTSO-E", "GLO", "RoW"])
    , ("AT", ["Europe", "EU", "RER", "ENTSO-E", "GLO", "RoW"])
    , ("CH", ["Europe", "RER", "ENTSO-E", "GLO", "RoW"])
    , ("SE", ["Europe", "EU", "RER", "ENTSO-E", "NORDEL", "GLO", "RoW"])
    , ("NO", ["Europe", "RER", "ENTSO-E", "NORDEL", "GLO", "RoW"])
    , ("DK", ["Europe", "EU", "RER", "ENTSO-E", "NORDEL", "GLO", "RoW"])
    , ("FI", ["Europe", "EU", "RER", "ENTSO-E", "NORDEL", "GLO", "RoW"])
    , ("PT", ["Europe", "EU", "RER", "ENTSO-E", "GLO", "RoW"])
    , ("GR", ["Europe", "EU", "RER", "ENTSO-E", "GLO", "RoW"])
    , ("IE", ["Europe", "EU", "RER", "ENTSO-E", "GLO", "RoW"])
    , ("CZ", ["Europe", "EU", "RER", "ENTSO-E", "GLO", "RoW"])
    , ("RO", ["Europe", "EU", "RER", "ENTSO-E", "GLO", "RoW"])
    , ("HU", ["Europe", "EU", "RER", "ENTSO-E", "GLO", "RoW"])
    , ("SK", ["Europe", "EU", "RER", "ENTSO-E", "GLO", "RoW"])
    , ("BG", ["Europe", "EU", "RER", "ENTSO-E", "GLO", "RoW"])
    , ("HR", ["Europe", "EU", "RER", "ENTSO-E", "GLO", "RoW"])
    , ("SI", ["Europe", "EU", "RER", "ENTSO-E", "GLO", "RoW"])
    , ("LT", ["Europe", "EU", "RER", "ENTSO-E", "GLO", "RoW"])
    , ("LV", ["Europe", "EU", "RER", "ENTSO-E", "GLO", "RoW"])
    , ("EE", ["Europe", "EU", "RER", "ENTSO-E", "GLO", "RoW"])
    , ("LU", ["Europe", "EU", "RER", "ENTSO-E", "GLO", "RoW"])
    , ("MT", ["Europe", "EU", "RER", "GLO", "RoW"])
    , ("CY", ["Europe", "EU", "RER", "GLO", "RoW"])

    -- Regional groupings → larger regions
    , ("EU", ["Europe", "RER", "GLO", "RoW"])
    , ("EU-27", ["Europe", "EU", "RER", "GLO", "RoW"])
    , ("EU-28", ["Europe", "EU", "RER", "GLO", "RoW"])
    , ("RER", ["Europe", "GLO", "RoW"])
    , ("ENTSO-E", ["Europe", "RER", "GLO", "RoW"])
    , ("NORDEL", ["Europe", "ENTSO-E", "RER", "GLO", "RoW"])
    , ("UCTE", ["Europe", "ENTSO-E", "RER", "GLO", "RoW"])
    , ("Europe", ["GLO", "RoW"])

    -- North American countries
    , ("US", ["North America", "NAFTA", "GLO", "RoW"])
    , ("CA", ["North America", "NAFTA", "GLO", "RoW"])
    , ("MX", ["North America", "Latin America", "NAFTA", "GLO", "RoW"])
    , ("NAFTA", ["North America", "GLO", "RoW"])
    , ("North America", ["GLO", "RoW"])
    , ("RNA", ["North America", "GLO", "RoW"])

    -- Asian countries
    , ("CN", ["Asia", "GLO", "RoW"])
    , ("JP", ["Asia", "GLO", "RoW"])
    , ("KR", ["Asia", "GLO", "RoW"])
    , ("IN", ["Asia", "GLO", "RoW"])
    , ("TW", ["Asia", "GLO", "RoW"])
    , ("ID", ["Asia", "GLO", "RoW"])
    , ("TH", ["Asia", "GLO", "RoW"])
    , ("MY", ["Asia", "GLO", "RoW"])
    , ("VN", ["Asia", "GLO", "RoW"])
    , ("PH", ["Asia", "GLO", "RoW"])
    , ("SG", ["Asia", "GLO", "RoW"])
    , ("Asia", ["GLO", "RoW"])
    , ("RAS", ["Asia", "GLO", "RoW"])

    -- Latin American countries
    , ("BR", ["Latin America", "South America", "GLO", "RoW"])
    , ("AR", ["Latin America", "South America", "GLO", "RoW"])
    , ("CL", ["Latin America", "South America", "GLO", "RoW"])
    , ("CO", ["Latin America", "South America", "GLO", "RoW"])
    , ("PE", ["Latin America", "South America", "GLO", "RoW"])
    , ("Latin America", ["GLO", "RoW"])
    , ("South America", ["Latin America", "GLO", "RoW"])
    , ("RLA", ["Latin America", "GLO", "RoW"])

    -- African countries/regions
    , ("ZA", ["Africa", "GLO", "RoW"])
    , ("EG", ["Africa", "Middle East", "GLO", "RoW"])
    , ("NG", ["Africa", "GLO", "RoW"])
    , ("MA", ["Africa", "GLO", "RoW"])
    , ("Africa", ["GLO", "RoW"])
    , ("RAF", ["Africa", "GLO", "RoW"])

    -- Oceania
    , ("AU", ["Oceania", "GLO", "RoW"])
    , ("NZ", ["Oceania", "GLO", "RoW"])
    , ("Oceania", ["GLO", "RoW"])

    -- Middle East
    , ("SA", ["Middle East", "Asia", "GLO", "RoW"])
    , ("AE", ["Middle East", "Asia", "GLO", "RoW"])
    , ("IL", ["Middle East", "GLO", "RoW"])
    , ("TR", ["Middle East", "Europe", "GLO", "RoW"])
    , ("Middle East", ["GLO", "RoW"])
    , ("RME", ["Middle East", "GLO", "RoW"])

    -- Global and fallback
    , ("GLO", ["RoW"])
    , ("RoW", [])
    ]

-- | Check if two units are compatible for linking
unitsAreCompatible :: UC.UnitConfig -> Text -> Text -> Bool
unitsAreCompatible cfg unit1 unit2
    | normalizeText unit1 == normalizeText unit2 = True  -- Same unit (after normalization)
    | otherwise = UC.unitsCompatible cfg unit1 unit2     -- Dimensionally compatible
