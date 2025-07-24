{-# LANGUAGE OverloadedStrings #-}

module ACV.UnitConversion where

import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T

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
convertExchangeAmount :: Text -> Text -> Double -> Double
convertExchangeAmount fromUnit toUnit amount =
    case convertUnitNormalized fromUnit toUnit amount of
        Just convertedAmount -> convertedAmount
        Nothing -> amount -- No conversion available, keep original amount
