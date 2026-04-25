{-# LANGUAGE OverloadedStrings #-}

module EcoSpold2Spec (spec) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import EcoSpold.Parser2 (streamParseActivityAndFlowsFromFile)
import Types

{- | The bundled fixture has a `<comment xml:lang="en">...</comment>` on each
of its four exchanges (1 input, 1 reference output, 2 emissions).

streamParseActivityAndFlowsFromFile derives a synthetic ProcessId from the
filename and rejects names that don't match `actUUID_prodUUID`, so we copy
the fixture into a temp path with that shape.
-}
withFixture :: ((Activity, [Flow], [Unit]) -> IO ()) -> IO ()
withFixture k = withSystemTempDirectory "es2-spec" $ \dir -> do
    bytes <- BS.readFile "test-data/electricity-production.spold"
    let path = dir </> "12345678-1234-5678-9abc-123456789001_12345678-1234-5678-9abc-123456789002.spold"
    BS.writeFile path bytes
    result <- streamParseActivityAndFlowsFromFile path
    case result of
        Left err -> expectationFailure $ "Parse failed: " ++ err
        Right res -> k res

spec :: Spec
spec = describe "per-exchange comments" $ do
    it "captures English <comment> on intermediateExchange and elementaryExchange" $
        withFixture $ \(act, _, _) ->
            map exchangeComment (exchanges act)
                `shouldMatchList` [ Just "Coal input for electricity generation"
                                  , Just "Electricity output (reference product)"
                                  , Just "CO2 emission from coal combustion"
                                  , Just "SO2 emission from coal combustion"
                                  ]

    it "preserves all four exchanges" $
        withFixture $
            \(act, _, _) -> length (exchanges act) `shouldBe` 4

    it "comments contain no &-entity artefacts" $
        withFixture $ \(act, _, _) ->
            let comments = [c | ex <- exchanges act, Just c <- [exchangeComment ex]]
             in all (not . T.isInfixOf "&") comments `shouldBe` True

    -- Critical correctness test: <property> children of an exchange may carry
    -- their own <comment> describing the property (e.g. "dry mass on a kg
    -- basis"). Those must NOT be attributed to the exchange itself.
    it "ignores <comment> nested inside <property> children of an exchange" $ do
        result <- streamParseActivityAndFlowsFromFile "test-data/sawnwood-properties_12345678-1234-5678-9abc-12345678aaaa.spold"
        case result of
            Left err -> expectationFailure $ "Parse failed: " ++ err
            Right (act, _, _) ->
                map exchangeComment (exchanges act)
                    `shouldMatchList` [ Nothing -- ex1 has no top-level comment, only property comments
                                      , Just "Adhesive applied during pressing" -- ex2's exchange-level comment, NOT the noisy property comment
                                      ]
