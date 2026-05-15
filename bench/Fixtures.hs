{- | Resolves environment variables to filesystem paths for real LCA fixtures.

The bench never ships fixtures itself: each capability declares which env var
it needs, and 'lookupFixture' returns 'Nothing' if the env var is unset or
the path doesn't exist. Callers omit the bench rather than failing — the
overall run reports only what could actually be measured.
-}
module Fixtures (
    FixtureSource (..),
    fixtureEnvVar,
    fixtureSourceLabel,
    lookupFixture,
) where

import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Environment (lookupEnv)

-- | The fixture reservoirs the bench knows how to consume.
data FixtureSource
    = Agribalyse
    | Ecoinvent
    | Bafu
    | Ilcd
    | MethodEFIlcd
    | MethodCsv
    | MethodSimaProCsv
    | MethodOlcaJson
    deriving (Eq, Show)

fixtureEnvVar :: FixtureSource -> String
fixtureEnvVar Agribalyse = "VOLCA_BENCH_AGRIBALYSE"
fixtureEnvVar Ecoinvent = "VOLCA_BENCH_ECOINVENT"
fixtureEnvVar Bafu = "VOLCA_BENCH_BAFU"
fixtureEnvVar Ilcd = "VOLCA_BENCH_ILCD"
fixtureEnvVar MethodEFIlcd = "VOLCA_BENCH_METHOD_EF_ILCD"
fixtureEnvVar MethodCsv = "VOLCA_BENCH_METHOD_CSV"
fixtureEnvVar MethodSimaProCsv = "VOLCA_BENCH_METHOD_SIMAPRO_CSV"
fixtureEnvVar MethodOlcaJson = "VOLCA_BENCH_METHOD_OLCA_JSON"

-- | Stable provenance string emitted into bench-results.json. Does not name a
-- specific version on purpose — the path on disk is what gives the version.
fixtureSourceLabel :: FixtureSource -> Text
fixtureSourceLabel Agribalyse = T.pack "agribalyse"
fixtureSourceLabel Ecoinvent = T.pack "ecoinvent"
fixtureSourceLabel Bafu = T.pack "bafu"
fixtureSourceLabel Ilcd = T.pack "ilcd"
fixtureSourceLabel MethodEFIlcd = T.pack "method-ef-ilcd"
fixtureSourceLabel MethodCsv = T.pack "method-csv"
fixtureSourceLabel MethodSimaProCsv = T.pack "method-simapro-csv"
fixtureSourceLabel MethodOlcaJson = T.pack "method-olca-json"

-- | Returns the path if the env var is set AND the path exists (file or
-- directory). Prints a single-line « [bench] skipping … » note when absent so
-- the run log makes it obvious which benchs were left out and why.
lookupFixture :: FixtureSource -> IO (Maybe FilePath)
lookupFixture src = do
    let var = fixtureEnvVar src
    mPath <- lookupEnv var
    case mPath of
        Nothing -> do
            putStrLn $ "[bench] " <> show src <> ": " <> var <> " not set, skipping"
            pure Nothing
        Just path -> do
            isFile <- doesFileExist path
            isDir <- doesDirectoryExist path
            if isFile || isDir
                then pure (Just path)
                else do
                    putStrLn $
                        "[bench] "
                            <> show src
                            <> ": "
                            <> var
                            <> "="
                            <> path
                            <> " does not exist, skipping"
                    pure Nothing
