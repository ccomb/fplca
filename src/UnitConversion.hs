{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Dimensional unit conversion system.
--
-- Units are defined by:
-- 1. A dimension vector (exponents for each base dimension)
-- 2. A conversion factor to SI base units
--
-- Example: tkm (tonne-kilometer) has dimension mass*length = [1,1,0,0,0,0,0,0]
-- and factor 1e6 (1 tkm = 1e6 kg*m)
--
-- Unit definitions are loaded from [units] section in fplca.toml.
module UnitConversion
    ( -- * Types
      Dimension
    , UnitDef(..)
    , UnitConfig(..)
      -- * Loading
    , defaultUnitConfig
    , buildUnitConfigFromToml
      -- * Operations
    , normalizeUnit
    , isKnownUnit
    , unitsCompatible
    , convertUnit
    , lookupUnitDef
      -- * Backward compatibility
    , convertExchangeAmount
      -- * Dimension parsing
    , parseDimension
      -- * Warnings
    , UnknownUnitTracker
    , newUnknownUnitTracker
    , warnIfUnknownUnit
    , getUnknownUnits
    ) where

import Control.DeepSeq (NFData)
import Control.Monad (unless)
import Data.Binary (Binary)
import qualified Data.Binary as Binary
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (elemIndex, foldl')
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Progress (ProgressLevel(Info), reportProgress)

-- | Dimension as exponent vector.
-- Order: [mass, length, time, energy, area, volume, count, currency]
-- Example: mass*length = [1, 1, 0, 0, 0, 0, 0, 0]
-- Example: length/time = [0, 1, -1, 0, 0, 0, 0, 0]
type Dimension = [Int]

-- | Unit definition: dimension + factor to convert to SI base units.
data UnitDef = UnitDef
    { udDimension :: !Dimension
    , udFactor    :: !Double
    } deriving (Eq, Show, Generic)

instance NFData UnitDef
instance Binary UnitDef

-- | Unit configuration loaded from fplca.toml.
data UnitConfig = UnitConfig
    { ucDimensionOrder :: ![Text]              -- ["mass", "length", "time", ...]
    , ucUnits          :: !(M.Map Text UnitDef) -- normalized (lowercase, trimmed) keys
    , ucOriginalKeys   :: !(M.Map Text Text)    -- normalized → original (for error messages)
    } deriving (Show, Generic)

instance NFData UnitConfig
instance Binary UnitConfig

-- | Default dimension order (must match the order in fplca.toml).
defaultDimensionOrder :: [Text]
defaultDimensionOrder = ["mass", "length", "time", "energy", "area", "volume", "count", "currency"]

-- | Normalize unit string for lookup (lowercase, trim whitespace).
normalizeUnit :: Text -> Text
normalizeUnit = T.toLower . T.strip

-- | Check if a unit is known in the config.
isKnownUnit :: UnitConfig -> Text -> Bool
isKnownUnit cfg u = M.member (normalizeUnit u) (ucUnits cfg)

-- | Look up a unit definition.
lookupUnitDef :: UnitConfig -> Text -> Maybe UnitDef
lookupUnitDef cfg u = M.lookup (normalizeUnit u) (ucUnits cfg)

-- | Check if two units are dimensionally compatible.
unitsCompatible :: UnitConfig -> Text -> Text -> Bool
unitsCompatible cfg u1 u2 =
    case (lookupUnitDef cfg u1, lookupUnitDef cfg u2) of
        (Just d1, Just d2) -> udDimension d1 == udDimension d2
        _ -> False

-- | Convert amount from one unit to another.
-- Returns Nothing if units are incompatible or unknown.
convertUnit :: UnitConfig -> Text -> Text -> Double -> Maybe Double
convertUnit cfg fromUnit toUnit amount = do
    UnitDef dimFrom factorFrom <- lookupUnitDef cfg fromUnit
    UnitDef dimTo factorTo     <- lookupUnitDef cfg toUnit
    if dimFrom == dimTo && factorTo /= 0
        then Just (amount * factorFrom / factorTo)
        else Nothing

-- | Parse a dimension expression like "mass*length/time" into an exponent vector.
-- Returns Left with error message on failure.
--
-- Examples:
--   "mass"              -> [1, 0, 0, 0, 0, 0, 0, 0]
--   "mass*length"       -> [1, 1, 0, 0, 0, 0, 0, 0]
--   "length/time"       -> [0, 1, -1, 0, 0, 0, 0, 0]
--   "mass*length/time/time" -> [1, 1, -2, 0, 0, 0, 0, 0]
parseDimension :: [Text] -> Text -> Either Text Dimension
parseDimension dimOrder expr
    | T.null (T.strip expr) = Left "Empty dimension expression"
    | otherwise = do
        let baseVec = replicate (length dimOrder) 0
            -- Split on first '/' to get numerator and denominator
            (numExpr, denExpr) = case T.breakOn "/" expr of
                (num, rest) | T.null rest -> (num, "")
                            | otherwise   -> (num, T.drop 1 rest)  -- drop the '/'
            numParts = filter (not . T.null) $ map T.strip $ T.splitOn "*" numExpr
            -- Denominator can have multiple divisions: length/time/time
            denParts = if T.null denExpr
                       then []
                       else concatMap (filter (not . T.null) . map T.strip . T.splitOn "*")
                                      (T.splitOn "/" denExpr)
        -- Add +1 for each numerator dimension
        vec1 <- foldlM (addExp dimOrder 1) baseVec numParts
        -- Add -1 for each denominator dimension
        foldlM (addExp dimOrder (-1)) vec1 denParts
  where
    addExp :: [Text] -> Int -> Dimension -> Text -> Either Text Dimension
    addExp order delta vec dimName =
        case elemIndex dimName order of
            Just idx -> Right $ modifyAt idx (+ delta) vec
            Nothing  -> Left $ "Unknown dimension: " <> dimName
                            <> " (valid: " <> T.intercalate ", " order <> ")"

    modifyAt :: Int -> (Int -> Int) -> [Int] -> [Int]
    modifyAt idx f xs = zipWith (\i x -> if i == idx then f x else x) [0..] xs

    -- foldlM for Either (not in base)
    foldlM :: (b -> a -> Either e b) -> b -> [a] -> Either e b
    foldlM _ acc []     = Right acc
    foldlM f acc (x:xs) = case f acc x of
        Left err   -> Left err
        Right acc' -> foldlM f acc' xs

-- | Tracker for unknown units encountered during parsing.
data UnknownUnitTracker = UnknownUnitTracker
    { uutSeen   :: !(IORef (S.Set Text))
    , uutConfig :: !UnitConfig
    }

-- | Create a new tracker.
newUnknownUnitTracker :: UnitConfig -> IO UnknownUnitTracker
newUnknownUnitTracker cfg = do
    seen <- newIORef S.empty
    return UnknownUnitTracker { uutSeen = seen, uutConfig = cfg }

-- | Warn if a unit is unknown (deduplicated).
warnIfUnknownUnit :: UnknownUnitTracker -> Text -> IO ()
warnIfUnknownUnit tracker unit = do
    let normalized = normalizeUnit unit
    unless (isKnownUnit (uutConfig tracker) unit || T.null normalized) $ do
        seen <- readIORef (uutSeen tracker)
        unless (S.member normalized seen) $ do
            modifyIORef' (uutSeen tracker) (S.insert normalized)
            reportProgress Info $ "[WARNING] Unknown unit: \"" <> T.unpack unit
                               <> "\" - add to [units.aliases] in fplca.toml"

-- | Get all unknown units encountered so far.
getUnknownUnits :: UnknownUnitTracker -> IO (S.Set Text)
getUnknownUnits tracker = readIORef (uutSeen tracker)

--------------------------------------------------------------------------------
-- Default unit configuration (built-in, used if no [units] section in config)
--------------------------------------------------------------------------------

-- | Default unit configuration with common LCA units.
defaultUnitConfig :: UnitConfig
defaultUnitConfig = UnitConfig
    { ucDimensionOrder = defaultDimensionOrder
    , ucUnits = defaultUnits
    , ucOriginalKeys = M.mapWithKey (\k _ -> k) defaultUnits
    }

-- | Build default units map.
defaultUnits :: M.Map Text UnitDef
defaultUnits = M.fromList $ concat
    [ -- Mass (SI: kg)
      mkAliases [1,0,0,0,0,0,0,0] 1.0      ["kg", "kilogram"]
    , mkAliases [1,0,0,0,0,0,0,0] 0.001    ["g", "gram"]
    , mkAliases [1,0,0,0,0,0,0,0] 1e-6     ["mg", "milligram"]
    , mkAliases [1,0,0,0,0,0,0,0] 1000.0   ["t", "ton", "tonne", "metric ton"]
    , mkAliases [1,0,0,0,0,0,0,0] 0.45359237 ["pound", "lb"]
    , mkAliases [1,0,0,0,0,0,0,0] 0.0283495231 ["ounce avoirdupois", "oz"]

      -- Length (SI: m)
    , mkAliases [0,1,0,0,0,0,0,0] 1.0      ["m", "meter", "metre"]
    , mkAliases [0,1,0,0,0,0,0,0] 1000.0   ["km", "kilometer", "kilometre"]
    , mkAliases [0,1,0,0,0,0,0,0] 0.01     ["cm", "centimeter", "centimetre"]
    , mkAliases [0,1,0,0,0,0,0,0] 0.001    ["mm", "millimeter", "millimetre"]
    , mkAliases [0,1,0,0,0,0,0,0] 1609.344 ["mile"]
    , mkAliases [0,1,0,0,0,0,0,0] 0.3048   ["ft", "foot"]
    , mkAliases [0,1,0,0,0,0,0,0] 0.0254   ["inch", "in"]

      -- Time (SI: s)
    , mkAliases [0,0,1,0,0,0,0,0] 1.0      ["s", "second", "sec"]
    , mkAliases [0,0,1,0,0,0,0,0] 60.0     ["min", "minute"]
    , mkAliases [0,0,1,0,0,0,0,0] 3600.0   ["h", "hour", "hr"]
    , mkAliases [0,0,1,0,0,0,0,0] 86400.0  ["d", "day"]
    , mkAliases [0,0,1,0,0,0,0,0] 31536000.0 ["year", "a", "yr"]
    , mkAliases [0,0,1,0,0,0,0,0] 2592000.0 ["month"]

      -- Energy (SI: J)
    , mkAliases [0,0,0,1,0,0,0,0] 1.0      ["j", "joule"]
    , mkAliases [0,0,0,1,0,0,0,0] 1000.0   ["kj", "kilojoule"]
    , mkAliases [0,0,0,1,0,0,0,0] 1e6      ["mj", "megajoule"]
    , mkAliases [0,0,0,1,0,0,0,0] 1e9      ["gj", "gigajoule"]
    , mkAliases [0,0,0,1,0,0,0,0] 1e12     ["tj", "terajoule"]
    , mkAliases [0,0,0,1,0,0,0,0] 3.6e6    ["kwh", "kilowatt hour", "kilowatt-hour"]
    , mkAliases [0,0,0,1,0,0,0,0] 3.6e9    ["mwh", "megawatt hour", "megawatt-hour"]
    , mkAliases [0,0,0,1,0,0,0,0] 3600.0   ["wh", "watt hour", "watt-hour"]
    , mkAliases [0,0,0,1,0,0,0,0] 4184.0   ["kcal", "kilocalorie"]
    , mkAliases [0,0,0,1,0,0,0,0] 4.184    ["cal", "calorie"]
    , mkAliases [0,0,0,1,0,0,0,0] 1055.06  ["btu", "british thermal unit"]

      -- Area (SI: m²)
    , mkAliases [0,0,0,0,1,0,0,0] 1.0      ["m2", "m²", "square meter", "square metre"]
    , mkAliases [0,0,0,0,1,0,0,0] 10000.0  ["ha", "hectare"]
    , mkAliases [0,0,0,0,1,0,0,0] 1e6      ["km2", "km²"]
    , mkAliases [0,0,0,0,1,0,0,0] 1e-4     ["cm2", "cm²"]
    , mkAliases [0,0,0,0,1,0,0,0] 4046.8564224 ["acre"]

      -- Volume (SI: m³)
    , mkAliases [0,0,0,0,0,1,0,0] 1.0      ["m3", "m³", "cubic meter", "cubic metre", "nm3", "sm3"]
    , mkAliases [0,0,0,0,0,1,0,0] 0.001    ["l", "liter", "litre", "dm3"]
    , mkAliases [0,0,0,0,0,1,0,0] 1e-6     ["ml", "cm3", "cm³"]
    , mkAliases [0,0,0,0,0,1,0,0] 0.003785411784 ["gallon", "gal"]

      -- Count (dimensionless)
    , mkAliases [0,0,0,0,0,0,1,0] 1.0      ["unit", "units", "p", "pcs", "piece", "pieces", "item", "items"]
    , mkAliases [0,0,0,0,0,0,1,0] 1.0      ["dimensionless"]
    , mkAliases [0,0,0,0,0,0,1,0] 1.0      ["kgy", "kilo-gray"]

      -- Currency
    , mkAliases [0,0,0,0,0,0,0,1] 1.0      ["eur", "eur2005", "euro"]
    , mkAliases [0,0,0,0,0,0,0,1] 1.0      ["usd", "usd2011", "usd(2011)", "dollar"]

      -- Mass×Length (transport: mass*distance)
      -- SI: kg*m, but we use kg*km internally for LCA convenience
    , mkAliases [1,1,0,0,0,0,0,0] 1e6      ["tkm", "t*km", "t.km", "metric ton*km", "tonne*km", "tonne-km", "tonne-kilometer", "tonne-kilometre"]
    , mkAliases [1,1,0,0,0,0,0,0] 1000.0   ["kgkm", "kg*km", "kg.km", "kilogram*km", "kilogram-km", "kilogram-kilometer", "kilogram-kilometre"]
    , mkAliases [1,1,0,0,0,0,0,0] 1.0      ["kg*m", "kgm"]
    , mkAliases [1,1,0,0,0,0,0,0] 1609.344e3 ["t*mile", "ton*mile", "ton-mile"]

      -- Person×Length (passenger transport)
    , mkAliases [0,1,0,0,0,0,1,0] 1000.0   ["pkm", "person*km", "passenger*km", "passenger-km", "passenger-kilometer"]
    , mkAliases [0,1,0,0,0,0,1,0] 1609.344 ["person*mile", "passenger*mile", "passenger-mile"]

      -- Velocity (length/time)
    , mkAliases [0,1,-1,0,0,0,0,0] 1.0     ["m/s"]
    , mkAliases [0,1,-1,0,0,0,0,0] 0.27778 ["km/h", "kph"]
    , mkAliases [0,1,-1,0,0,0,0,0] 0.44704 ["mph", "mile/h"]

      -- Activity (radioactivity)
    , mkAliases [0,0,-1,0,0,0,0,0] 1000.0  ["kbq", "kilobecquerel"]
    , mkAliases [0,0,-1,0,0,0,0,0] 1.0     ["bq", "becquerel"]

      -- Area×Time (land use)
    , mkAliases [0,0,1,0,1,0,0,0] 1.0      ["m2*year", "m²*year", "m2.year", "m2*a", "m2a"]
    , mkAliases [0,0,1,0,1,0,0,0] 315360000000.0 ["ha*year", "hectare*year", "ha.year", "ha*a", "ha a"]

      -- Volume×Time
    , mkAliases [0,0,1,0,0,1,0,0] 31536000.0 ["m3*year", "m³*year", "m3.year", "m3*a", "m3y"]
    , mkAliases [0,0,1,0,0,1,0,0] 86.4       ["l*day", "l.day", "l*d", "liter*day", "litre*day"]
    , mkAliases [0,0,1,0,0,1,0,0] 31536.0    ["l*year", "l.year", "l*a", "liter*year", "litre*year"]

      -- Mass×Time (some specific processes)
    , mkAliases [1,0,1,0,0,0,0,0] 31536000.0 ["kg*year", "kg.year", "kg*a"]
    , mkAliases [1,0,1,0,0,0,0,0] 86400.0  ["kg*day", "kg.day", "kg*d"]

      -- Length×Time
    , mkAliases [0,1,1,0,0,0,0,0] 31536000.0 ["m*year", "m.year", "m*a"]
    , mkAliases [0,1,1,0,0,0,0,0] 31536000000.0 ["km*year", "km.year", "km*a"]
    , mkAliases [0,1,1,0,0,0,0,0] 50805446860.8 ["mile*year", "mile.year"]

      -- Special LCA units that appear in databases
    , mkAliases [0,0,0,0,0,0,1,0] 1.0      ["guest night", "teu"]
    , mkAliases [0,0,0,0,0,0,1,0] 1.0      ["unit/ha", "unit per ha", "unit/mj", "unit per mj"]
    , mkAliases [0,0,0,0,0,0,1,0] 1.0      ["units/m²", "units/m2"]
    , mkAliases [0,0,0,0,0,0,1,0] 1.0      ["lm/m2", "lm/ft2"]

      -- Rate units (often used as if dimensionless in LCA)
    , mkAliases [1,0,-1,0,0,0,0,0] 1.0     ["kg/s"]
    , mkAliases [1,0,-1,0,0,0,0,0] 3600.0  ["kg/h", "kg/hour"]
    , mkAliases [1,0,-1,0,0,0,0,0] 31536000.0 ["kg/year", "kg/a", "kg/yr"]
    , mkAliases [1,-2,0,0,0,0,0,0] 1.0     ["kg/m2"]
    , mkAliases [1,-3,0,0,0,0,0,0] 1.0     ["kg/m3"]
    , mkAliases [1,-3,0,0,0,0,0,0] 1000.0  ["kg/l"]

      -- Energy rate
    , mkAliases [0,0,0,1,-1,0,0,0] 1e6     ["mj/kg"]
    , mkAliases [0,0,0,1,-1,0,0,0] 3.6e6   ["kwh/km"]
    , mkAliases [0,0,0,1,-1,0,0,0] 3.6e9   ["kwh/year", "kwh/a"]

      -- Dimensionless ratios
    , mkAliases [0,0,0,0,0,0,1,0] 1.0      ["kg/kg"]

      -- Agricultural units (often used as mass per area)
    , mkAliases [1,-2,0,0,0,0,0,0] 1e-4    ["kg/ha", "kg n/ha", "kg p/ha", "kg no3-n/ha.year", "kg p2o5/ha", "kg po4/ha", "kg nh3-n"]
    ]
  where
    mkAliases :: Dimension -> Double -> [Text] -> [(Text, UnitDef)]
    mkAliases dim factor aliases =
        [ (normalizeUnit alias, UnitDef dim factor)
        | alias <- aliases
        ]

--------------------------------------------------------------------------------
-- Build UnitConfig from TOML configuration
--------------------------------------------------------------------------------

-- | Build a UnitConfig from TOML configuration, merging with defaults.
-- Custom aliases override defaults with the same (normalized) key.
-- Returns Left with error message if dimension parsing fails.
buildUnitConfigFromToml
    :: [Text]                    -- ^ Dimension names (order matters)
    -> M.Map Text (Text, Double) -- ^ Aliases: name -> (dim_expr, factor)
    -> Either Text UnitConfig
buildUnitConfigFromToml dimensions aliases = do
    -- Parse each alias and build UnitDef
    customUnits <- parseAliases dimensions (M.toList aliases)
    -- Merge: custom units override defaults
    let mergedUnits = M.union customUnits (ucUnits defaultUnitConfig)
        mergedOrigKeys = M.union
            (M.mapWithKey (\k _ -> k) customUnits)
            (ucOriginalKeys defaultUnitConfig)
    Right UnitConfig
        { ucDimensionOrder = if null dimensions then defaultDimensionOrder else dimensions
        , ucUnits = mergedUnits
        , ucOriginalKeys = mergedOrigKeys
        }
  where
    parseAliases :: [Text] -> [(Text, (Text, Double))] -> Either Text (M.Map Text UnitDef)
    parseAliases dimOrder aliasList = do
        pairs <- mapM (parseAlias dimOrder) aliasList
        Right $ M.fromList pairs

    parseAlias :: [Text] -> (Text, (Text, Double)) -> Either Text (Text, UnitDef)
    parseAlias dimOrder (name, (dimExpr, factor)) = do
        dim <- parseDimension dimOrder dimExpr
        Right (normalizeUnit name, UnitDef dim factor)

--------------------------------------------------------------------------------
-- Backward compatibility
--------------------------------------------------------------------------------

-- | Convert an amount from one unit to another using the default config.
-- Returns the original amount if conversion fails.
--
-- This function is provided for backward compatibility with the old API.
convertExchangeAmount :: Text -> Text -> Double -> Double
convertExchangeAmount fromUnit toUnit amount =
    case convertUnit defaultUnitConfig fromUnit toUnit amount of
        Just converted -> converted
        Nothing -> amount  -- Return original if incompatible or unknown
