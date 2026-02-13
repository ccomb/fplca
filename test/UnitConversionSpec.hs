{-# LANGUAGE OverloadedStrings #-}

module UnitConversionSpec (spec) where

import Test.Hspec
import LCA.UnitConversion
import qualified Data.Map.Strict as M

-- Helper for testing Left results
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

-- Helper for testing Right results
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

spec :: Spec
spec = do
    describe "Dimension Parsing" $ do
        let dimOrder = ["mass", "length", "time", "energy", "area", "volume", "count", "currency"]

        it "parses single dimension" $ do
            parseDimension dimOrder "mass" `shouldBe` Right [1, 0, 0, 0, 0, 0, 0, 0]

        it "parses product of dimensions (mass*length)" $ do
            parseDimension dimOrder "mass*length" `shouldBe` Right [1, 1, 0, 0, 0, 0, 0, 0]

        it "parses division (length/time)" $ do
            parseDimension dimOrder "length/time" `shouldBe` Right [0, 1, -1, 0, 0, 0, 0, 0]

        it "parses repeated division (length/time/time)" $ do
            parseDimension dimOrder "length/time/time" `shouldBe` Right [0, 1, -2, 0, 0, 0, 0, 0]

        it "parses complex expression (mass*length/time)" $ do
            parseDimension dimOrder "mass*length/time" `shouldBe` Right [1, 1, -1, 0, 0, 0, 0, 0]

        it "rejects unknown dimension" $ do
            parseDimension dimOrder "velocity" `shouldSatisfy` isLeft

    describe "Unit Compatibility" $ do
        let cfg = defaultUnitConfig

        it "tkm and kgkm are compatible (both mass*length)" $ do
            unitsCompatible cfg "tkm" "kgkm" `shouldBe` True

        it "tkm and kg are NOT compatible (mass*length vs mass)" $ do
            unitsCompatible cfg "tkm" "kg" `shouldBe` False

        it "kg and t are compatible (both mass)" $ do
            unitsCompatible cfg "kg" "t" `shouldBe` True

        it "kg and g are compatible (both mass)" $ do
            unitsCompatible cfg "kg" "g" `shouldBe` True

        it "MJ and kWh are compatible (both energy)" $ do
            unitsCompatible cfg "MJ" "kWh" `shouldBe` True

        it "m/s and km/h are compatible (both velocity)" $ do
            unitsCompatible cfg "m/s" "km/h" `shouldBe` True

        it "pkm and person*km are compatible (passenger transport)" $ do
            unitsCompatible cfg "pkm" "person*km" `shouldBe` True

        it "unknown units are not compatible" $ do
            unitsCompatible cfg "unknown_unit" "kg" `shouldBe` False

        it "l*day is a known unit" $ do
            isKnownUnit cfg "l*day" `shouldBe` True

        it "l*day and m3*year are compatible (both volume×time)" $ do
            unitsCompatible cfg "l*day" "m3*year" `shouldBe` True

    describe "Unit Conversion" $ do
        let cfg = defaultUnitConfig

        it "converts 1 tkm to 1000 kgkm" $ do
            convertUnit cfg "tkm" "kgkm" 1.0 `shouldBe` Just 1000.0

        it "converts 1000 kgkm to 1 tkm" $ do
            convertUnit cfg "kgkm" "tkm" 1000.0 `shouldBe` Just 1.0

        it "converts 1 t to 1000 kg" $ do
            convertUnit cfg "t" "kg" 1.0 `shouldBe` Just 1000.0

        it "converts 1 kg to 1000 g" $ do
            convertUnit cfg "kg" "g" 1.0 `shouldBe` Just 1000.0

        it "converts 1 kWh to 3.6 MJ" $ do
            case convertUnit cfg "kWh" "MJ" 1.0 of
                Just v -> v `shouldSatisfy` (\x -> abs (x - 3.6) < 0.001)
                Nothing -> expectationFailure "conversion failed"

        it "returns Nothing for incompatible units" $ do
            convertUnit cfg "kg" "m" 1.0 `shouldBe` Nothing

        it "returns Nothing for unknown units" $ do
            convertUnit cfg "unknown" "kg" 1.0 `shouldBe` Nothing

        it "converts 1 m3*year to 365000 l*day" $ do
            case convertUnit cfg "m3*year" "l*day" 1.0 of
                Just v -> v `shouldSatisfy` (\x -> abs (x - 365000.0) < 1.0)
                Nothing -> expectationFailure "conversion failed"

    describe "Backward Compatibility" $ do
        it "convertExchangeAmount converts tkm to kgkm" $ do
            convertExchangeAmount "tkm" "kgkm" 1.0 `shouldBe` 1000.0

        it "convertExchangeAmount returns original for incompatible units" $ do
            convertExchangeAmount "kg" "m" 5.0 `shouldBe` 5.0

    describe "Unit Normalization" $ do
        it "normalizes to lowercase" $ do
            normalizeUnit "KG" `shouldBe` "kg"

        it "trims whitespace" $ do
            normalizeUnit "  kg  " `shouldBe` "kg"

        it "case-insensitive lookup works" $ do
            let cfg = defaultUnitConfig
            isKnownUnit cfg "KG" `shouldBe` True
            isKnownUnit cfg "Kg" `shouldBe` True
            isKnownUnit cfg "kG" `shouldBe` True

    describe "Config Building (buildUnitConfigFromToml)" $ do
        let defaultDims = ["mass", "length", "time", "energy", "area", "volume", "count", "currency"]

        it "builds config with empty aliases (uses defaults)" $ do
            let result = buildUnitConfigFromToml defaultDims M.empty
            result `shouldSatisfy` isRight
            case result of
                Right cfg -> do
                    isKnownUnit cfg "kg" `shouldBe` True
                    isKnownUnit cfg "tkm" `shouldBe` True
                Left _ -> expectationFailure "should succeed"

        it "adds new custom unit alias" $ do
            let aliases = M.fromList [("customunit", ("mass", 42.0))]
                result = buildUnitConfigFromToml defaultDims aliases
            result `shouldSatisfy` isRight
            case result of
                Right cfg -> do
                    isKnownUnit cfg "customunit" `shouldBe` True
                    -- Check factor
                    case lookupUnitDef cfg "customunit" of
                        Just def -> udFactor def `shouldBe` 42.0
                        Nothing -> expectationFailure "customunit should exist"
                Left _ -> expectationFailure "should succeed"

        it "custom unit overrides default with same name" $ do
            -- Override kg with a different factor
            let aliases = M.fromList [("kg", ("mass", 999.0))]
                result = buildUnitConfigFromToml defaultDims aliases
            result `shouldSatisfy` isRight
            case result of
                Right cfg -> do
                    case lookupUnitDef cfg "kg" of
                        Just def -> udFactor def `shouldBe` 999.0  -- Overridden
                        Nothing -> expectationFailure "kg should exist"
                Left _ -> expectationFailure "should succeed"

        it "preserves defaults when no override" $ do
            let aliases = M.fromList [("newunit", ("energy", 100.0))]
                result = buildUnitConfigFromToml defaultDims aliases
            result `shouldSatisfy` isRight
            case result of
                Right cfg -> do
                    -- Default kg should still work
                    case lookupUnitDef cfg "kg" of
                        Just def -> udFactor def `shouldBe` 1.0  -- Default
                        Nothing -> expectationFailure "kg should exist"
                    -- tkm should still be mass*length
                    unitsCompatible cfg "tkm" "kgkm" `shouldBe` True
                Left _ -> expectationFailure "should succeed"

        it "parses compound dimension expressions" $ do
            let aliases = M.fromList
                    [ ("myvelocity", ("length/time", 1.0))
                    , ("mytransport", ("mass*length", 500.0))
                    ]
                result = buildUnitConfigFromToml defaultDims aliases
            result `shouldSatisfy` isRight
            case result of
                Right cfg -> do
                    -- myvelocity should be compatible with m/s
                    unitsCompatible cfg "myvelocity" "m/s" `shouldBe` True
                    -- mytransport should be compatible with tkm
                    unitsCompatible cfg "mytransport" "tkm" `shouldBe` True
                Left _ -> expectationFailure "should succeed"

        it "fails on invalid dimension name" $ do
            let aliases = M.fromList [("badunit", ("invalid_dimension", 1.0))]
                result = buildUnitConfigFromToml defaultDims aliases
            result `shouldSatisfy` isLeft

        it "fails on invalid dimension expression" $ do
            let aliases = M.fromList [("badunit", ("mass*invalid", 1.0))]
                result = buildUnitConfigFromToml defaultDims aliases
            result `shouldSatisfy` isLeft

        it "uses custom dimension order when provided" $ do
            let customDims = ["length", "mass"]  -- Reversed order
                aliases = M.fromList [("myunit", ("length", 1.0))]
                result = buildUnitConfigFromToml customDims aliases
            result `shouldSatisfy` isRight
            case result of
                Right cfg -> do
                    ucDimensionOrder cfg `shouldBe` customDims
                Left _ -> expectationFailure "should succeed"

        it "uses default dimension order when empty list provided" $ do
            let result = buildUnitConfigFromToml [] M.empty
            result `shouldSatisfy` isRight
            case result of
                Right cfg -> do
                    ucDimensionOrder cfg `shouldBe` defaultDims
                Left _ -> expectationFailure "should succeed"
