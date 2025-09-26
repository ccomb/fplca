{-# LANGUAGE OverloadedStrings #-}

module ACV.UnitConversion where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)

{- | Unit conversion factor from source unit to target unit
Factor represents: 1 sourceUnit = factor * targetUnit
-}
data UnitConversion = UnitConversion
    { ucFromUnit :: !Text -- Source unit name
    , ucToUnit :: !Text -- Target unit name
    , ucFactor :: !Double -- Conversion factor
    , ucUnitType :: !Text -- Unit category (energy, mass, volume, etc.)
    }

-- | Conversion database: Map from (fromUnit, toUnit) to conversion factor
type ConversionDB = M.Map (Text, Text) Double

-- | Build conversion database from hardcoded EcoInvent unit conversions
buildConversionDB :: ConversionDB
buildConversionDB =
    M.fromList $
        concat
            [ -- Mass/Weight conversions to kg

                [ (("ounce avoirdupois", "kg"), 0.0283495231)
                , (("barrel lime small mass equivalent", "kg"), 81.6466266)
                , (("pound", "kg"), 0.45359237)
                , (("tonne", "kg"), 1000.0)
                , (("g", "kg"), 0.001)
                , (("t", "kg"), 1000.0)
                , (("kg", "g"), 1000.0)
                , (("kg", "t"), 0.001)
                , (("kg", "tonne"), 0.001)
                , (("kg", "pound"), 1 / 0.45359237)
                , (("kg", "ounce avoirdupois"), 1 / 0.0283495231)
                ]
            , -- Energy conversions to MJ

                [ (("Wh", "MJ"), 0.0036)
                , (("kWh", "MJ"), 3.6)
                , (("British thermal unit thermochemical", "MJ"), 0.00105435)
                , (("kJ", "MJ"), 0.001)
                , (("GJ", "MJ"), 1000.0)
                , (("TJ", "MJ"), 1000000.0)
                , (("cal", "MJ"), 4.184e-6)
                , (("kcal", "MJ"), 0.004184)
                , -- Inverse energy conversions
                  (("MJ", "Wh"), 1 / 0.0036)
                , (("MJ", "kWh"), 1 / 3.6)
                , (("MJ", "kJ"), 1000.0)
                , (("MJ", "GJ"), 0.001)
                , (("MJ", "TJ"), 1e-6)
                , (("MJ", "cal"), 1 / 4.184e-6)
                , (("MJ", "kcal"), 1 / 0.004184)
                ]
            , -- Volume conversions to l (liters)

                [ (("gallon fluid US", "l"), 3.785411784)
                , (("m3", "l"), 1000.0)
                , (("dm3", "l"), 1.0)
                , (("cm3", "l"), 0.001)
                , (("ml", "l"), 0.001)
                , -- Inverse volume conversions
                  (("l", "gallon fluid US"), 1 / 3.785411784)
                , (("l", "m3"), 0.001)
                , (("l", "dm3"), 1.0)
                , (("l", "cm3"), 1000.0)
                , (("l", "ml"), 1000.0)
                ]
            , -- Area conversions to m2

                [ (("ha", "m2"), 10000.0)
                , (("acre", "m2"), 4046.8564224)
                , (("barn", "m2"), 1e-28) -- Used in nuclear physics
                , (("km2", "m2"), 1000000.0)
                , (("cm2", "m2"), 0.0001)
                , (("mm2", "m2"), 0.000001)
                , -- Inverse area conversions
                  (("m2", "ha"), 1 / 10000.0)
                , (("m2", "acre"), 1 / 4046.8564224)
                , (("m2", "barn"), 1e28)
                , (("m2", "km2"), 1e-6)
                , (("m2", "cm2"), 10000.0)
                , (("m2", "mm2"), 1000000.0)
                ]
            , -- Length conversions to m

                [ (("km", "m"), 1000.0)
                , (("cm", "m"), 0.01)
                , (("mm", "m"), 0.001)
                , (("micron", "m"), 1e-6)
                , (("mile", "m"), 1609.344)
                , (("inch", "m"), 0.0254)
                , (("ft", "m"), 0.3048)
                , (("yard", "m"), 0.9144)
                , -- Inverse length conversions
                  (("m", "km"), 0.001)
                , (("m", "cm"), 100.0)
                , (("m", "mm"), 1000.0)
                , (("m", "micron"), 1e6)
                , (("m", "mile"), 1 / 1609.344)
                , (("m", "inch"), 1 / 0.0254)
                , (("m", "ft"), 1 / 0.3048)
                , (("m", "yard"), 1 / 0.9144)
                ]
            , -- Time conversions to s (seconds)

                [ (("min", "s"), 60.0)
                , (("h", "s"), 3600.0)
                , (("d", "s"), 86400.0)
                , (("year", "s"), 31557600.0) -- Julian year
                , (("a", "s"), 31557600.0) -- Alternative notation for year
                -- Inverse time conversions
                , (("s", "min"), 1 / 60.0)
                , (("s", "h"), 1 / 3600.0)
                , (("s", "d"), 1 / 86400.0)
                , (("s", "year"), 1 / 31557600.0)
                , (("s", "a"), 1 / 31557600.0)
                ]
            , -- Length-Time conversions to m*year

                [ (("mile*year", "m*year"), 1609.344)
                , (("km*year", "m*year"), 1000.0)
                , -- Inverse
                  (("m*year", "mile*year"), 1 / 1609.344)
                , (("m*year", "km*year"), 0.001)
                ]
            , -- Area-Time conversions to m2*year

                [ (("ha*year", "m2*year"), 10000.0)
                , (("acre*year", "m2*year"), 4046.8564224)
                , -- Inverse
                  (("m2*year", "ha*year"), 1 / 10000.0)
                , (("m2*year", "acre*year"), 1 / 4046.8564224)
                ]
            , -- Density conversions to kg/m3 (common base unit for density)

                [ (("kg/l", "kg/m3"), 1000.0)
                , (("g/cm3", "kg/m3"), 1000.0)
                , (("g/l", "kg/m3"), 1.0)
                , -- Inverse
                  (("kg/m3", "kg/l"), 0.001)
                , (("kg/m3", "g/cm3"), 0.001)
                , (("kg/m3", "g/l"), 1.0)
                ]
            , -- Freight transport work conversions to tkm (tonne-kilometer)

                [ (("kg*km", "tkm"), 0.001)
                , (("t*km", "tkm"), 1.0)
                , (("pound*mile", "tkm"), 0.45359237 * 1.609344 / 1000.0)
                , -- Inverse
                  (("tkm", "kg*km"), 1000.0)
                , (("tkm", "t*km"), 1.0)
                ]
            , -- Person transport work conversions to pkm (person-kilometer)

                [ (("person*km", "pkm"), 1.0)
                , (("person*mile", "pkm"), 1.609344)
                , -- Inverse
                  (("pkm", "person*km"), 1.0)
                , (("pkm", "person*mile"), 1 / 1.609344)
                ]
            , -- Illuminance conversions to lux

                [ (("lm/m2", "lux"), 1.0) -- lumen per square meter = lux
                , (("lm/ft2", "lux"), 10.764) -- lumen per square foot
                ]
            , -- Thermal resistance conversions to m2*K/W

                [ (("m2*°C/W", "m2*K/W"), 1.0) -- Celsius and Kelvin differences are the same
                , (("ft2*°F*h/BTU", "m2*K/W"), 0.176110) -- Imperial to metric thermal resistance
                ]
            ]

{- | Convert quantity from source unit to target unit
Returns Nothing if no conversion path exists
-}
convertUnit :: Text -> Text -> Double -> Maybe Double
convertUnit fromUnit toUnit quantity
    | fromUnit == toUnit = Just quantity -- Same unit, no conversion needed
    | otherwise = do
        factor <- M.lookup (fromUnit, toUnit) buildConversionDB
        return $ quantity * factor

{- | Normalize unit name by removing common variations
This helps match unit names that might have slight differences
-}
normalizeUnitName :: Text -> Text
normalizeUnitName unit =
    let normalized = T.toLower $ T.strip unit
     in case normalized of
            "kilogram" -> "kg"
            "gram" -> "g"
            "megajoule" -> "MJ"
            "kilojoule" -> "kJ"
            "gigajoule" -> "GJ"
            "terajoule" -> "TJ"
            "kilowatt hour" -> "kWh"
            "kilowatt-hour" -> "kWh"
            "watt hour" -> "Wh"
            "watt-hour" -> "Wh"
            "liter" -> "l"
            "litre" -> "l"
            "cubic meter" -> "m3"
            "cubic metre" -> "m3"
            "square meter" -> "m2"
            "square metre" -> "m2"
            "hectare" -> "ha"
            "kilometer" -> "km"
            "kilometre" -> "km"
            "meter" -> "m"
            "metre" -> "m"
            "centimeter" -> "cm"
            "centimetre" -> "cm"
            "millimeter" -> "mm"
            "millimetre" -> "mm"
            "tonne" -> "t"
            "metric ton" -> "t"
            "hour" -> "h"
            "minute" -> "min"
            "second" -> "s"
            "day" -> "d"
            other -> other

-- | Convert with unit name normalization
convertUnitNormalized :: Text -> Text -> Double -> Maybe Double
convertUnitNormalized fromUnit toUnit quantity =
    convertUnit (normalizeUnitName fromUnit) (normalizeUnitName toUnit) quantity

{- | Apply unit conversion to exchange amount during tree traversal
This is the key function for LCA calculations to ensure all quantities are in compatible units
-}
{-|
Physical dimensions for LCA exchanges with reference units.

This defines the standard reference units used in LCA calculations:
- Mass: kg (kilogram) - for all material flows
- Energy: MJ (megajoule) - for all energy flows
- Volume: m³ (cubic meter) - for volumetric flows
- Area: m² (square meter) - for land use
- Length: m (meter) - for distances
- Time: h (hour) - common LCA time unit
- Activity: kBq (kilobecquerel) - for radioactive substances
- Count: unit (dimensionless) - for countable items
- Currency: EUR (euro) - for economic flows (no conversion)
-}
data ExchangeUnitClass
    = Mass       -- Reference: kg
    | Energy     -- Reference: MJ
    | Volume     -- Reference: m³
    | Area       -- Reference: m²
    | Length     -- Reference: m
    | Time       -- Reference: h
    | Activity   -- Reference: kBq
    | Count      -- Reference: unit
    | Currency   -- Reference: EUR
    deriving (Eq, Show, Ord)

-- | Get reference unit for each physical dimension
getReferenceUnit :: ExchangeUnitClass -> Text
getReferenceUnit Mass = "kg"
getReferenceUnit Energy = "MJ"
getReferenceUnit Volume = "m3"
getReferenceUnit Area = "m2"
getReferenceUnit Length = "m"
getReferenceUnit Time = "h"
getReferenceUnit Activity = "kBq"
getReferenceUnit Count = "unit"
getReferenceUnit Currency = "EUR"

{-|
Comprehensive mapping of EcoSpold exchange units to reference units.

This map covers all units commonly found in EcoSpold files and provides
conversion factors to normalize them to reference units for consistent
matrix calculations.

Each entry maps: unitName -> (unitClass, conversionFactor)
where conversionFactor converts the unit to the reference unit.
-}
exchangeUnitMap :: M.Map Text (ExchangeUnitClass, Double)
exchangeUnitMap = M.fromList
    [ -- Mass units (reference: kg)
      ("kg", (Mass, 1.0))
    , ("g", (Mass, 0.001))
    , ("metric ton", (Mass, 1000.0))
    , ("t", (Mass, 1000.0))
    , ("tonne", (Mass, 1000.0))

    -- Energy units (reference: MJ)
    , ("MJ", (Energy, 1.0))
    , ("kJ", (Energy, 0.001))
    , ("kWh", (Energy, 3.6))
    , ("kcal", (Energy, 0.004184))
    , ("BTU/kg", (Energy, 0.001055))  -- BTU to MJ

    -- Volume units (reference: m³)
    , ("m3", (Volume, 1.0))
    , ("l", (Volume, 0.001))
    , ("Sm3", (Volume, 1.0))  -- Standard conditions - treat as m³

    -- Area units (reference: m²)
    , ("m2", (Area, 1.0))
    , ("ha", (Area, 10000.0))

    -- Length units (reference: m)
    , ("m", (Length, 1.0))
    , ("km", (Length, 1000.0))
    , ("mm", (Length, 0.001))

    -- Time units (reference: h)
    , ("h", (Time, 1.0))
    , ("hour", (Time, 1.0))
    , ("day", (Time, 24.0))
    , ("year", (Time, 8760.0))  -- 365*24
    , ("month", (Time, 730.0))  -- 30.4*24 average

    -- Activity units (reference: kBq)
    , ("kBq", (Activity, 1.0))

    -- Count/dimensionless units (reference: unit)
    , ("unit", (Count, 1.0))
    , ("dimensionless", (Count, 1.0))
    , ("guest night", (Count, 1.0))
    , ("person*km", (Count, 1.0))
    , ("metric ton*km", (Count, 1.0))
    , ("TEU", (Count, 1.0))
    , ("units/m²", (Count, 1.0))
    , ("unit/ha", (Count, 1.0))
    , ("unit/MJ", (Count, 1.0))

    -- Currency units (reference: EUR, no conversion)
    , ("EUR2005", (Currency, 1.0))
    , ("USD(2011)", (Currency, 1.0))

    -- Compound units - normalize the primary component
    , ("kg/m2", (Mass, 1.0))        -- Mass density
    , ("kg/ha", (Mass, 0.0001))     -- kg per 10000 m² -> kg/m²
    , ("kg/year", (Mass, 1.0))      -- Mass rate - normalize mass part
    , ("kg/hour", (Mass, 1.0))      -- Mass rate
    , ("kg/m3", (Mass, 1.0))        -- Mass concentration
    , ("kg/l", (Mass, 1.0))         -- Mass concentration
    , ("kg/kg", (Count, 1.0))       -- Mass ratio - dimensionless
    , ("kg*day", (Mass, 1.0))       -- Mass × time
    , ("kWh/km", (Energy, 3.6))     -- Energy per distance
    , ("kWh/year", (Energy, 3.6))   -- Energy rate
    , ("MJ/kg", (Energy, 1.0))      -- Specific energy
    , ("m2*year", (Area, 1.0))      -- Area × time
    , ("m*year", (Length, 1.0))     -- Length × time
    , ("km*year", (Length, 1000.0)) -- Distance × time

    -- Specialized LCA units
    , ("kg N/ha", (Mass, 0.0001))   -- Nitrogen per area
    , ("kg P/ha", (Mass, 0.0001))   -- Phosphorus per area
    , ("kg NO3-N/ha.year", (Mass, 0.0001))  -- Nitrate-N per area per time
    , ("kg NH3-N", (Mass, 1.0))     -- Ammonia-N mass
    , ("kg P2O5/ha", (Mass, 0.0001)) -- P2O5 per area
    , ("kg PO4/ha", (Mass, 0.0001))  -- Phosphate per area
    ]

{-|
Normalize exchange amount to reference unit.

Converts any exchange amount from its original unit to the corresponding
reference unit (kg, MJ, m³, etc.). This ensures all matrix calculations
use consistent units.

Returns: (normalizedAmount, referenceUnit)
-}
normalizeExchangeAmount :: Text -> Double -> (Double, Text)
normalizeExchangeAmount unitName amount =
    case M.lookup (normalizeUnitName unitName) exchangeUnitMap of
        Just (unitClass, factor) ->
            (amount * factor, getReferenceUnit unitClass)
        Nothing ->
            (amount, unitName)  -- Keep original if unknown

{-|
Get unit class and conversion factor for a unit.

Useful for validation and reporting.
-}
getUnitInfo :: Text -> Maybe (ExchangeUnitClass, Double)
getUnitInfo unitName = M.lookup (normalizeUnitName unitName) exchangeUnitMap

{-|
Check if a unit is supported for conversion.
-}
isSupportedUnit :: Text -> Bool
isSupportedUnit unitName = M.member (normalizeUnitName unitName) exchangeUnitMap

convertExchangeAmount :: Text -> Text -> Double -> Double
convertExchangeAmount fromUnit toUnit amount =
    case convertUnitNormalized fromUnit toUnit amount of
        Just convertedAmount -> convertedAmount
        Nothing -> amount -- No conversion available, keep original amount

{-|
Statistics tracking for unit conversions during matrix construction.
-}
data UnitConversionStats = UnitConversionStats
    { totalExchanges :: !Int
    , convertedExchanges :: !Int
    , unknownUnits :: !(S.Set Text)
    , unitClassCounts :: !(M.Map ExchangeUnitClass Int)
    } deriving (Show)

{-|
Initialize empty conversion statistics.
-}
emptyStats :: UnitConversionStats
emptyStats = UnitConversionStats 0 0 S.empty M.empty

{-|
Update conversion statistics for a single exchange.
-}
updateConversionStats :: UnitConversionStats -> Text -> UnitConversionStats
updateConversionStats stats unitName =
    let normalizedUnit = normalizeUnitName unitName
        isConverted = M.member normalizedUnit exchangeUnitMap
        newUnknownUnits = if isConverted
                         then unknownUnits stats
                         else S.insert unitName (unknownUnits stats)
        newUnitClassCounts = case M.lookup normalizedUnit exchangeUnitMap of
            Just (unitClass, _) -> M.insertWith (+) unitClass 1 (unitClassCounts stats)
            Nothing -> unitClassCounts stats
    in stats
        { totalExchanges = totalExchanges stats + 1
        , convertedExchanges = if isConverted then convertedExchanges stats + 1 else convertedExchanges stats
        , unknownUnits = newUnknownUnits
        , unitClassCounts = newUnitClassCounts
        }

{-|
Format unit conversion statistics for reporting.
-}
formatConversionStats :: UnitConversionStats -> [Text]
formatConversionStats stats =
    [ "Unit Conversion Statistics:"
    , "  Total exchanges: " <> T.pack (show $ totalExchanges stats)
    , "  Converted exchanges: " <> T.pack (show $ convertedExchanges stats) <> " ("
      <> T.pack (show $ round (100.0 * fromIntegral (convertedExchanges stats) / fromIntegral (totalExchanges stats) :: Double)) <> "%)"
    , "  Unit class distribution:"
    ] ++
    [ "    " <> T.pack (show unitClass) <> ": " <> T.pack (show count)
    | (unitClass, count) <- M.toList (unitClassCounts stats)
    ] ++
    (if S.null (unknownUnits stats)
     then ["  All units recognized ✓"]
     else [ "  Unknown units (" <> T.pack (show $ S.size $ unknownUnits stats) <> "):"
          ] ++
          [ "    " <> unitName | unitName <- take 10 $ S.toList (unknownUnits stats) ] ++
          (if S.size (unknownUnits stats) > 10
           then ["    ... and " <> T.pack (show $ S.size (unknownUnits stats) - 10) <> " more"]
           else [])
    )
