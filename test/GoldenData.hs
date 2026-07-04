{-# LANGUAGE OverloadedStrings #-}

module GoldenData (
    -- SAMPLE.min3 expected values
    sampleMin3ActivityX,
    sampleMin3ActivityZ,
    sampleMin3ExpectedSupply,
    sampleMin3ExpectedCO2,
    sampleMin3ExpectedZinc,
    -- Test tolerance
    defaultTolerance,
) where

-- | Default tolerance for floating point comparisons
defaultTolerance :: Double
defaultTolerance = 1.0e-10

-- SAMPLE.min3 Activity UUIDs
sampleMin3ActivityX :: String
sampleMin3ActivityX = "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa"

sampleMin3ActivityZ :: String
sampleMin3ActivityZ = "dddddddd-dddd-dddd-dddd-dddddddddddd"

-- SAMPLE.min3 Expected Values (for 1 kg Product X)
-- Supply chain: Z (4.0 kg CO2, 0.003 kg Zinc) → Y (needs 0.4 kg Z) → X (needs 0.6 kg Y)

{- | Expected supply vector for SAMPLE.min3 with demand [1, 0, 0]
[Product X, Product Y, Product Z] = [1.0, 0.6, 0.24]
-}
sampleMin3ExpectedSupply :: [Double]
sampleMin3ExpectedSupply = [1.0, 0.6, 0.24]

{- | Expected CO2 emissions for 1 kg Product X
0.24 kg Z × 4.0 kg CO2/kg Z = 0.96 kg CO2
-}
sampleMin3ExpectedCO2 :: Double
sampleMin3ExpectedCO2 = 0.96

{- | Expected Zinc emissions for 1 kg Product X
0.24 kg Z × 0.003 kg Zinc/kg Z = 0.00072 kg Zinc
-}
sampleMin3ExpectedZinc :: Double
sampleMin3ExpectedZinc = 0.00072
