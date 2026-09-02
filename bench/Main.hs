{-# LANGUAGE OverloadedStrings #-}

{- | Bench orchestrator.

Collects 'BenchSpec's from every Bench.* module, runs each one with a
simple wall-clock harness (3 warmup + N timed iterations), and writes the
aggregate to a JSON file consumable by the perf page in another session.

The orchestrator is intentionally minimal: there is no statistical sample
cleaning, no CI bands, no automatic rerun on outliers. We trust LCA hot
paths to be deterministic enough that a small N (5–11) gives a stable
mean; the exact spread is reported as @stddev@ for inspection.
-}
module Main (main) where

import Control.Exception (SomeException, try)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import GHC.Conc (getNumProcessors)
import GHC.IO.Exception (ExitCode (ExitSuccess))
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure)
import System.IO (hFlush, stdout)
import System.Process (readProcessWithExitCode)
import Text.Printf (printf)

import qualified Criterion.Measurement as Meas
import qualified Criterion.Measurement.Types as MeasTypes
import System.Mem (performMajorGC)

import qualified Builtin as B
import qualified UnitConversion as UC

import qualified Bench.Json as J
import qualified Bench.Lcia as Lcia
import qualified Bench.Loader as Loader
import qualified Bench.Parsers as Parsers
import qualified Bench.Solve as Solve

main :: IO ()
main = do
    outPath <- resolveOutput
    putStrLn $ "[bench] output: " <> outPath

    unitCfg <- resolveUnitConfig

    putStrLn "[bench] collecting bench specs..."
    specs <-
        concat
            <$> sequence
                [ Parsers.register unitCfg
                , Loader.register unitCfg
                , Solve.register unitCfg
                , Lcia.register unitCfg
                ]
    putStrLn $ "[bench] " <> show (length specs) <> " benchs ready"

    putStrLn "[bench] reading hardware metadata..."
    meta <- buildMetadata

    putStrLn "[bench] running..."
    results <- mapM runOne specs

    let out =
            J.SchemaOutput
                { J.soSchemaVersion = J.schemaVersion
                , J.soMetadata = meta
                , J.soResults = results
                }
    J.writeBenchResults outPath out
    putStrLn $ "[bench] wrote " <> outPath <> " (" <> show (length results) <> " results)"

{- | The unit table every bench reads a real database with: the one the
engine carries in its binary, which is what an engine started with no
configuration runs on. 'UC.defaultUnitConfig' knows four units, so a real
database refuses to load against it: two rows written in kg and in g are
then two flows no conversion relates.
-}
resolveUnitConfig :: IO UC.UnitConfig
resolveUnitConfig =
    case UC.buildFromCSV (B.builtinContent B.BuiltinUnits) of
        Left err -> do
            putStrLn $ "[bench] cannot read the built-in unit table: " <> T.unpack err
            exitFailure
        Right cfg -> do
            putStrLn $ "[bench] unit table: " <> show (UC.unitCount cfg) <> " units"
            pure cfg

resolveOutput :: IO FilePath
resolveOutput = do
    args <- getArgs
    case args of
        ["--output", p] -> pure p
        ("--output" : p : _) -> pure p
        _ -> fromMaybe "bench-results.json" <$> lookupEnv "VOLCA_BENCH_OUTPUT"

-- ---------------------------------------------------------------------------
-- Measurement
-- ---------------------------------------------------------------------------

runOne :: J.BenchSpec -> IO J.BenchResult
runOne spec = do
    let cap = T.unpack (J.bsCapability spec)
    putStr ("  " <> cap <> " ... ")
    hFlush stdout

    -- Strategy: do a small number of full iterations with explicit GC
    -- between each. We avoid criterion's default sample explosion (which
    -- runs the action thousands of times for sub-second benches and
    -- accumulates heap on heavy ones). The trade-off is a coarser stddev
    -- — fine for the size of numbers we report (parser at seconds, scoring
    -- at milliseconds).
    let nWarmup = 1
        nSamples = chooseSampleCount spec
    -- Warmup primes caches and trims first-iteration noise.
    mapM_ (const (runIter (J.bsAction spec))) ([1 .. nWarmup] :: [Int])
    samples <- mapM (const (runIter (J.bsAction spec))) [1 .. nSamples]

    let !mean = sum samples / fromIntegral nSamples
        !var =
            if nSamples > 1
                then sum [(s - mean) ** 2 | s <- samples] / fromIntegral (nSamples - 1)
                else 0
        !stddev = sqrt var
        !nItems = J.uowN (J.bsUnitOfWork spec)
        !ips = if mean > 0 then fromIntegral nItems / mean else 0
    putStrLn (printf "%.3f s ± %.3f s (%d samples)" mean stddev nSamples :: String)
    pure
        J.BenchResult
            { J.brCapability = J.bsCapability spec
            , J.brLabel = J.bsLabel spec
            , J.brDescription = J.bsDescription spec
            , J.brUnitOfWork = J.bsUnitOfWork spec
            , J.brMetric = J.bsMetric spec
            , J.brFixture = J.bsFixture spec
            , J.brMean = mean
            , J.brStddev = stddev
            , J.brSamples = nSamples
            , J.brDerived = J.Derived{J.dItemsPerSecond = ips}
            }

{- | Time one full iteration of a 'Benchmarkable', and collect heap so the
next iteration starts from a known state.
-}
runIter :: MeasTypes.Benchmarkable -> IO Double
runIter act = do
    performMajorGC
    (m, _) <- Meas.measure act 1
    pure (MeasTypes.measTime m)

{- | Pick a sample count that balances accuracy with total wall time. Small
benches (<100 ms expected) get more samples; heavy ones (>1 s) get
fewer to keep the overall run reasonable.
-}
chooseSampleCount :: J.BenchSpec -> Int
chooseSampleCount spec = case J.bsMetric spec of
    "milliseconds" -> 11
    _ -> 5

-- ---------------------------------------------------------------------------
-- Metadata
-- ---------------------------------------------------------------------------

buildMetadata :: IO J.Metadata
buildMetadata = do
    git <- gitSha
    ghc <- ghcVersion
    now <- getCurrentTime
    hw <- readHardware
    pure
        J.Metadata
            { J.mGitSha = git
            , J.mGhc = ghc
            , J.mTimestampIso = T.pack (iso8601Show now)
            , J.mHardware = hw
            }

gitSha :: IO (Maybe Text)
gitSha = trimResult <$> safeRead "git" ["rev-parse", "HEAD"]

ghcVersion :: IO (Maybe Text)
ghcVersion = trimResult <$> safeRead "ghc" ["--numeric-version"]

readHardware :: IO J.Hardware
readHardware = do
    cpu <- readCpu
    cores <- Just <$> getNumProcessors
    ram <- readRamGb
    os <- trimResult <$> safeRead "uname" ["-sr"]
    pure
        J.Hardware
            { J.hwCpu = cpu
            , J.hwCores = cores
            , J.hwRamGb = ram
            , J.hwOs = os
            }

readCpu :: IO (Maybe Text)
readCpu = do
    contents <- safeReadFile "/proc/cpuinfo"
    pure $ case contents of
        Nothing -> Nothing
        Just c ->
            let modelLine = take 1 [l | l <- lines c, "model name" `prefixOf` l]
             in case modelLine of
                    [l] -> Just (T.strip (T.pack (drop 1 (dropWhile (/= ':') l))))
                    _ -> Nothing
  where
    prefixOf p s = take (length p) s == p

readRamGb :: IO (Maybe Double)
readRamGb = do
    contents <- safeReadFile "/proc/meminfo"
    pure $ case contents of
        Nothing -> Nothing
        Just c ->
            case [l | l <- lines c, "MemTotal:" `prefixOf` l] of
                (l : _) ->
                    let kbs = takeWhile (/= ' ') (dropWhile (== ' ') (drop (length ("MemTotal:" :: String)) l))
                     in case reads kbs :: [(Double, String)] of
                            [(kb, _)] -> Just (kb / 1024 / 1024)
                            _ -> Nothing
                _ -> Nothing
  where
    prefixOf p s = take (length p) s == p

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

{- | Run a sub-process and return its stdout only on @ExitSuccess@. Spawn
failures and non-zero exits both collapse to 'Nothing' so the
'Metadata' field stays absent rather than carrying garbage (e.g. an
empty string when @git rev-parse HEAD@ fails outside a checkout).
-}
safeRead :: FilePath -> [String] -> IO (Maybe Text)
safeRead cmd args = do
    r <- try (readProcessWithExitCode cmd args "") :: IO (Either SomeException (ExitCode, String, String))
    pure $ case r of
        Right (ExitSuccess, out, _) -> Just (T.pack out)
        _ -> Nothing

safeReadFile :: FilePath -> IO (Maybe String)
safeReadFile path = do
    r <- try (readFile path) :: IO (Either SomeException String)
    pure $ case r of
        Right s -> Just s
        Left _ -> Nothing

{- | Strip whitespace and demote @Just ""@ to 'Nothing' — handy when the
underlying command exits cleanly but prints nothing useful.
-}
trimResult :: Maybe Text -> Maybe Text
trimResult m = case T.strip <$> m of
    Just t | not (T.null t) -> Just t
    _ -> Nothing
