{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module LCA.UnitConversion where

import LCA.Progress (ProgressLevel (Info), reportProgress)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Maybe (isJust)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import System.IO.Unsafe (unsafePerformIO)

-- | Physical dimension categories with reference units used internally.
data ExchangeUnitClass
    = Mass
    | Energy
    | Volume
    | Area
    | Length
    | Time
    | Activity
    | Count
    | Currency
    deriving (Eq, Ord, Show)

-- | Canonical representation of a unit within the engine.
data CanonicalUnit = CanonicalUnit
    { cuClass :: !ExchangeUnitClass
    , cuReferenceUnit :: !Text
    }
    deriving (Eq, Ord, Show)

-- | Definition for an incoming unit string.
data UnitDefinition = UnitDefinition
    { udCanonicalUnit :: !CanonicalUnit
    , udToReferenceFactor :: !Double
    }
    deriving (Eq, Show)

-- | Result returned by normalization.
data NormalizedAmount
    = NormalizedAmount
        { naAmount :: !Double
        , naUnit :: !CanonicalUnit
        }
    | UnknownUnitAmount
        { naOriginalUnit :: !Text
        , naAmount :: !Double
        }
    deriving (Eq, Show)

type UnitKey = Text

normalizeUnitKey :: Text -> UnitKey
normalizeUnitKey = T.toLower . T.strip

mkUnit :: ExchangeUnitClass -> Text -> Double -> UnitDefinition
mkUnit unitClass referenceName factor =
    UnitDefinition (CanonicalUnit unitClass referenceName) factor

alias :: UnitDefinition -> [Text] -> [(UnitKey, UnitDefinition)]
alias def names =
    [ (normalizeUnitKey name, def)
    | name <- names
    , let trimmed = T.strip name
    , not (T.null trimmed)
    ]

massDefinitions :: [(UnitKey, UnitDefinition)]
massDefinitions =
    alias (mkUnit Mass "kg" 1.0) ["kg", "kilogram"]
        ++ alias (mkUnit Mass "kg" 0.001) ["g", "gram"]
        ++ alias (mkUnit Mass "kg" 1000.0) ["t", "tonne", "metric ton"]
        ++ alias (mkUnit Mass "kg" 0.45359237) ["pound"]
        ++ alias (mkUnit Mass "kg" 0.0283495231) ["ounce avoirdupois"]
        ++ alias (mkUnit Mass "kg" 81.6466266) ["barrel lime small mass equivalent"]
        ++ alias (mkUnit Mass "kg" 1.0) ["kg/m2", "kg/year", "kg/hour", "kg/m3", "kg/l", "kg nh3-n", "kg*day"]
        ++ alias (mkUnit Mass "kg" 0.0001) ["kg/ha", "kg n/ha", "kg p/ha", "kg no3-n/ha.year", "kg p2o5/ha", "kg po4/ha"]
        ++ alias (mkUnit Count "unit" 1.0) ["kg/kg"]

energyDefinitions :: [(UnitKey, UnitDefinition)]
energyDefinitions =
    alias (mkUnit Energy "MJ" 1.0) ["mj", "megajoule", "mj/kg"]
        ++ alias (mkUnit Energy "MJ" 0.001) ["kj", "kilojoule"]
        ++ alias (mkUnit Energy "MJ" 3.6) ["kwh", "kilowatt hour", "kilowatt-hour", "kwh/year", "kwh/km"]
        ++ alias (mkUnit Energy "MJ" 0.0036) ["wh", "watt hour", "watt-hour"]
        ++ alias (mkUnit Energy "MJ" 0.004184) ["kcal", "kilocalorie"]
        ++ alias (mkUnit Energy "MJ" 4.184e-6) ["cal", "calorie"]
        ++ alias (mkUnit Energy "MJ" 0.00105435) ["british thermal unit thermochemical", "btu/kg", "btu"]
        ++ alias (mkUnit Energy "MJ" 1000.0) ["gj", "gigajoule"]
        ++ alias (mkUnit Energy "MJ" 1000000.0) ["tj", "terajoule"]

volumeDefinitions :: [(UnitKey, UnitDefinition)]
volumeDefinitions =
    alias (mkUnit Volume "m3" 1.0) ["m3", "sm3", "cubic meter", "cubic metre"]
        ++ alias (mkUnit Volume "m3" 0.001) ["l", "liter", "litre", "dm3"]
        ++ alias (mkUnit Volume "m3" 1.0e-6) ["cm3", "ml"]
        ++ alias (mkUnit Volume "m3" 0.003785411784) ["gallon fluid us"]

areaDefinitions :: [(UnitKey, UnitDefinition)]
areaDefinitions =
    alias (mkUnit Area "m2" 1.0) ["m2", "square meter", "square metre", "m2*year"]
        ++ alias (mkUnit Area "m2" 10000.0) ["ha", "hectare", "ha*year"]
        ++ alias (mkUnit Area "m2" 4046.8564224) ["acre", "acre*year"]
        ++ alias (mkUnit Area "m2" 1.0e-28) ["barn"]
        ++ alias (mkUnit Area "m2" 1.0e6) ["km2"]
        ++ alias (mkUnit Area "m2" 1.0e-4) ["cm2"]
        ++ alias (mkUnit Area "m2" 1.0e-6) ["mm2"]

lengthDefinitions :: [(UnitKey, UnitDefinition)]
lengthDefinitions =
    alias (mkUnit Length "m" 1.0) ["m", "meter", "metre", "m*year"]
        ++ alias (mkUnit Length "m" 1000.0) ["km", "kilometer", "kilometre", "km*year"]
        ++ alias (mkUnit Length "m" 0.01) ["cm", "centimeter", "centimetre"]
        ++ alias (mkUnit Length "m" 0.001) ["mm", "millimeter", "millimetre"]
        ++ alias (mkUnit Length "m" 1.0e-6) ["micron"]
        ++ alias (mkUnit Length "m" 1609.344) ["mile", "mile*year"]
        ++ alias (mkUnit Length "m" 0.0254) ["inch"]
        ++ alias (mkUnit Length "m" 0.3048) ["ft", "foot"]
        ++ alias (mkUnit Length "m" 0.9144) ["yard"]

timeDefinitions :: [(UnitKey, UnitDefinition)]
timeDefinitions =
    alias (mkUnit Time "h" 1.0) ["h", "hour"]
        ++ alias (mkUnit Time "h" (1 / 60.0)) ["min", "minute"]
        ++ alias (mkUnit Time "h" (1 / 3600.0)) ["s", "second"]
        ++ alias (mkUnit Time "h" 24.0) ["day", "d"]
        ++ alias (mkUnit Time "h" 8760.0) ["year", "a"]
        ++ alias (mkUnit Time "h" 730.0) ["month"]

activityDefinitions :: [(UnitKey, UnitDefinition)]
activityDefinitions = alias (mkUnit Activity "kBq" 1.0) ["kbq"]

countDefinitions :: [(UnitKey, UnitDefinition)]
countDefinitions =
    alias (mkUnit Count "unit" 1.0) ["unit", "units", "dimensionless", "guest night", "teu", "unit/ha", "unit per ha", "unit/mj", "unit per mj", "units/m²", "units/m2"]
        ++ alias (mkUnit Count "unit" 1.0) ["person*km", "person*mile", "pkm"]
        ++ alias (mkUnit Count "unit" 1.0) ["metric ton*km", "t*km", "tkm", "kg*km"]
        ++ alias (mkUnit Count "unit" 1.0) ["lm/m2", "lm/ft2"]

currencyDefinitions :: [(UnitKey, UnitDefinition)]
currencyDefinitions =
    alias (mkUnit Currency "EUR" 1.0) ["eur", "eur2005"]
        ++ alias (mkUnit Currency "USD(2011)" 1.0) ["usd(2011)"]

unitDefinitionEntries :: [(UnitKey, UnitDefinition)]
unitDefinitionEntries =
    massDefinitions
        ++ energyDefinitions
        ++ volumeDefinitions
        ++ areaDefinitions
        ++ lengthDefinitions
        ++ timeDefinitions
        ++ activityDefinitions
        ++ countDefinitions
        ++ currencyDefinitions

unitDefinitionMap :: M.Map UnitKey UnitDefinition
unitDefinitionMap = M.fromList unitDefinitionEntries

{-# NOINLINE unknownUnitsRef #-}
unknownUnitsRef :: IORef (S.Set UnitKey)
unknownUnitsRef = unsafePerformIO (newIORef S.empty)

warnUnknownUnit :: Text -> ()
warnUnknownUnit rawUnit =
    unsafePerformIO $ do
        let key = normalizeUnitKey rawUnit
            trimmed = T.strip rawUnit
        seen <- readIORef unknownUnitsRef
        if T.null key || S.member key seen
            then return ()
            else do
                modifyIORef' unknownUnitsRef (S.insert key)
                reportProgress Info $ "[WARNING] Unknown unit encountered: " ++ T.unpack trimmed
        return ()

classifyUnit :: Text -> Maybe UnitDefinition
classifyUnit unitName = M.lookup (normalizeUnitKey unitName) unitDefinitionMap

convertExchangeAmount :: Text -> Text -> Double -> Double
convertExchangeAmount fromUnit toUnit amount =
    case (classifyUnit fromUnit, classifyUnit toUnit) of
        (Just fromDef, Just toDef)
            | cuClass (udCanonicalUnit fromDef) == cuClass (udCanonicalUnit toDef) ->
                let referenceAmount = amount * udToReferenceFactor fromDef
                    targetFactor = udToReferenceFactor toDef
                in if targetFactor == 0
                    then referenceAmount
                    else referenceAmount / targetFactor
            | otherwise -> amount
        (Nothing, _) ->
            let !_ = warnUnknownUnit fromUnit
             in amount
        (_, Nothing) ->
            let !_ = warnUnknownUnit toUnit
             in amount
