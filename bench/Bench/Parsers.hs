{-# LANGUAGE OverloadedStrings #-}

{- | Parser benchmarks on real LCA fixtures.

We register one bench per (parser, fixture) pair. The fixture provider
('Fixtures.lookupFixture') decides whether the bench runs at all; if the
env var is unset, the bench list simply omits it.

For per-file parsers (EcoSpold 2/1) we slice the file list to a fixed N so
the four process-parsers can be compared at the same scale. For
single-file parsers (SimaPro CSV, ILCD directory) the whole fixture is
parsed and the actual N is reported — slicing those would require either
patching the parser or pre-building a derived fixture, neither of which
fits in the bench's read-only contract.
-}
module Bench.Parsers (
    register,
) where

import Control.Concurrent.Async (mapConcurrently)
import Control.DeepSeq (NFData, deepseq)
import Control.Exception (evaluate)
import Criterion.Main (Benchmarkable, nfIO)
import qualified Data.ByteString as BS
import Data.List (sort)
import qualified Data.Text as T
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>))

import qualified EcoSpold.Parser1 as ES1
import qualified EcoSpold.Parser2 as ES2
import qualified ILCD.Parser as ILCD
import qualified Method.Parser as MP
import qualified Method.Parser.OlcaSchema as MPO
import qualified Method.ParserCSV as MPC
import qualified Method.ParserSimaPro as MPS
import Method.Types (Method (..), MethodCollection (..))
import qualified SimaPro.Parser as SP
import Types (sdbActivities)
import qualified UnitConversion as UC

import Bench.Json (BenchSpec (..), UnitOfWork (..))
import qualified Bench.Json as J
import qualified Fixtures as F

-- ---------------------------------------------------------------------------
-- Group sizes (aligned for visual comparison)
-- ---------------------------------------------------------------------------

nProcessFiles :: Int
nProcessFiles = 1000

-- ---------------------------------------------------------------------------
-- Public registration
-- ---------------------------------------------------------------------------

register :: IO [BenchSpec]
register = do
    es2 <- registerEcoSpold2
    es1 <- registerEcoSpold1
    sp <- registerSimaPro
    ilcd <- registerIlcd
    methods <- registerMethodParsers
    pure (es2 ++ es1 ++ sp ++ ilcd ++ methods)

-- ---------------------------------------------------------------------------
-- EcoSpold 2 (per-file XML, parallel)
-- ---------------------------------------------------------------------------

registerEcoSpold2 :: IO [BenchSpec]
registerEcoSpold2 = do
    mDir <- F.lookupFixture F.Ecoinvent
    case mDir of
        Nothing -> pure []
        Just root -> do
            mDatasets <- locateDatasetsDir root
            case mDatasets of
                Nothing -> do
                    putStrLn $ "[bench] parser.ecospold2: no datasets/ directory under " <> root <> ", skipping"
                    pure []
                Just dir -> do
                    files <- listFilesByExt ".spold" dir
                    if length files < nProcessFiles
                        then do
                            putStrLn $
                                "[bench] parser.ecospold2: only "
                                    <> show (length files)
                                    <> " .spold files (need "
                                    <> show nProcessFiles
                                    <> "), skipping"
                            pure []
                        else
                            let !sliced = take nProcessFiles files
                             in pure
                                    [ BenchSpec
                                        { bsCapability = "parser.ecospold2"
                                        , bsLabel = T.pack ("Parse " <> show nProcessFiles <> " EcoSpold 2 process files (XML)")
                                        , bsDescription =
                                            "Reads and deserialises N EcoSpold 2 process files (compressed XML on disk) \
                                            \into Haskell structures ready to query. Each file is one activity dataset; \
                                            \parsing runs in parallel across CPU cores, matching what the production \
                                            \loader does at database load time."
                                        , bsUnitOfWork = UnitOfWork{uowKind = "ecospold2_files", uowN = nProcessFiles}
                                        , bsMetric = "seconds"
                                        , bsFixture = J.Fixture{J.fSource = F.fixtureSourceLabel F.Ecoinvent, J.fSlice = T.pack ("first " <> show nProcessFiles <> " by name")}
                                        , bsAction = parallelParseFiles ES2.streamParseActivityAndFlowsFromFile sliced
                                        }
                                    ]

{- | Ecoinvent's 7z extracts to ./datasets/ inside a sibling .d/ directory.
Accept either the dataset directory directly or its parent (so users can
point the env var at either).
-}
locateDatasetsDir :: FilePath -> IO (Maybe FilePath)
locateDatasetsDir path = do
    isDir <- doesDirectoryExist path
    if not isDir
        then pure Nothing
        else do
            let candidate = path </> "datasets"
            isCandidate <- doesDirectoryExist candidate
            if isCandidate
                then pure (Just candidate)
                else do
                    -- Maybe the user pointed straight at datasets/.
                    files <- listDirectory path
                    if any ((".spold" ==) . takeExtension) files
                        then pure (Just path)
                        else pure Nothing

-- ---------------------------------------------------------------------------
-- EcoSpold 1 (per-file XML, parallel)
-- ---------------------------------------------------------------------------

registerEcoSpold1 :: IO [BenchSpec]
registerEcoSpold1 = do
    mDir <- F.lookupFixture F.Bafu
    case mDir of
        Nothing -> pure []
        Just root -> do
            files <- listFilesByExt ".xml" root
            if length files < nProcessFiles
                then do
                    putStrLn $
                        "[bench] parser.ecospold1: only "
                            <> show (length files)
                            <> " .xml files (need "
                            <> show nProcessFiles
                            <> "), skipping"
                    pure []
                else
                    let !sliced = take nProcessFiles files
                     in pure
                            [ BenchSpec
                                { bsCapability = "parser.ecospold1"
                                , bsLabel = T.pack ("Parse " <> show nProcessFiles <> " EcoSpold 1 process files (XML)")
                                , bsDescription =
                                    "Reads and deserialises N EcoSpold 1 process files (XML on disk) into Haskell \
                                    \structures ready to query. EcoSpold 1 is the older multi-file format used by \
                                    \BAFU, KBOB and other reference inventories; this is the cost paid when loading \
                                    \those databases."
                                , bsUnitOfWork = UnitOfWork{uowKind = "ecospold1_files", uowN = nProcessFiles}
                                , bsMetric = "seconds"
                                , bsFixture = J.Fixture{J.fSource = F.fixtureSourceLabel F.Bafu, J.fSlice = T.pack ("first " <> show nProcessFiles <> " by name")}
                                , bsAction = parallelParseFiles ES1.streamParseActivityAndFlowsFromFile1 sliced
                                }
                            ]

-- ---------------------------------------------------------------------------
-- SimaPro CSV (single-file, parallel internally)
-- ---------------------------------------------------------------------------

registerSimaPro :: IO [BenchSpec]
registerSimaPro = do
    mPath <- F.lookupFixture F.Agribalyse
    case mPath of
        Nothing -> pure []
        Just path -> do
            -- Parse once to learn N_actual; the bench measurement re-parses fresh.
            parsed <- SP.parseSimaProCSV UC.defaultUnitConfig path
            case parsed of
                Left err -> do
                    putStrLn $ "[bench] parser.simapro: parse failed (" <> T.unpack err <> "), skipping"
                    pure []
                Right (acts, _, _, _, _) -> do
                    let !n = length acts
                    pure
                        [ BenchSpec
                            { bsCapability = "parser.simapro"
                            , bsLabel = T.pack ("Parse " <> show n <> " processes from a SimaPro CSV")
                            , bsDescription =
                                "Reads a SimaPro CSV export (Windows-1252 encoded) and parses every process block into \
                                \Haskell structures. Internally splits the file across worker threads at process \
                                \boundaries. SimaPro CSV is the dominant interchange format from food and agriculture LCA \
                                \databases; this benches the cold-start parsing cost."
                            , bsUnitOfWork = UnitOfWork{uowKind = "simapro_processes", uowN = n}
                            , bsMetric = "seconds"
                            , bsFixture = J.Fixture{J.fSource = F.fixtureSourceLabel F.Agribalyse, J.fSlice = "whole file"}
                            , bsAction = nfIO $ do
                                reparsed <- SP.parseSimaProCSV UC.defaultUnitConfig path
                                evaluate (fmap (\(acts', _, _, _, _) -> length acts') reparsed)
                            }
                        ]

-- ---------------------------------------------------------------------------
-- ILCD directory parser
-- ---------------------------------------------------------------------------

registerIlcd :: IO [BenchSpec]
registerIlcd = do
    mDir <- F.lookupFixture F.Ilcd
    case mDir of
        Nothing -> pure []
        Just dir -> do
            -- Probe parse to learn the actual process count.
            res <- ILCD.parseILCDDirectory dir
            case res of
                Left err -> do
                    putStrLn $ "[bench] parser.ilcd: probe parse failed (" <> T.unpack err <> "), skipping"
                    pure []
                Right sdb -> do
                    let !n = length (sdbActivities sdb)
                    pure
                        [ BenchSpec
                            { bsCapability = "parser.ilcd"
                            , bsLabel = T.pack ("Parse " <> show n <> " ILCD processes (XML, parallel)")
                            , bsDescription =
                                "Reads an ILCD directory (processes/, flows/, flowproperties/, unitgroups/) \
                                \and parses every process XML in parallel into Haskell structures. ILCD is \
                                \the EU JRC's reference format used by EF / PEF reference packs and many \
                                \methodology repositories."
                            , bsUnitOfWork = UnitOfWork{uowKind = "ilcd_processes", uowN = n}
                            , bsMetric = "seconds"
                            , bsFixture = J.Fixture{J.fSource = F.fixtureSourceLabel F.Ilcd, J.fSlice = "whole directory"}
                            , bsAction = nfIO $ do
                                r <- ILCD.parseILCDDirectory dir
                                case r of
                                    Left err -> evaluate (T.length err)
                                    Right sdb' -> evaluate (length (sdbActivities sdb'))
                            }
                        ]

-- ---------------------------------------------------------------------------
-- Method parsers (4 formats, all aimed at N=5000 CFs)
-- ---------------------------------------------------------------------------

registerMethodParsers :: IO [BenchSpec]
registerMethodParsers = do
    ilcdSpecs <- registerMethodIlcd
    csvSpecs <- registerMethodCsv
    simaSpecs <- registerMethodSimapro
    olcaSpecs <- registerMethodOlca
    pure (ilcdSpecs ++ csvSpecs ++ simaSpecs ++ olcaSpecs)

registerMethodIlcd :: IO [BenchSpec]
registerMethodIlcd = do
    mPath <- F.lookupFixture F.MethodEFIlcd
    case mPath of
        Nothing -> pure []
        Just path -> do
            res <- MP.parseMethodFile path
            case res of
                Left err -> do
                    putStrLn $ "[bench] parser.method_ilcd_xml: parse failed (" <> err <> "), skipping"
                    pure []
                Right method -> do
                    let !n = length (methodFactors method)
                    pure
                        [ BenchSpec
                            { bsCapability = "parser.method_ilcd_xml"
                            , bsLabel = T.pack ("Parse an LCIA method with " <> show n <> " CFs (ILCD XML)")
                            , bsDescription =
                                "Reads one ILCD method XML file and extracts every characterization factor for the \
                                \method. ILCD XML is the format published for PEF / EF reference packs. This benches \
                                \the cost of bringing a single method online."
                            , bsUnitOfWork = UnitOfWork{uowKind = "characterization_factors", uowN = n}
                            , bsMetric = "milliseconds"
                            , bsFixture = J.Fixture{J.fSource = F.fixtureSourceLabel F.MethodEFIlcd, J.fSlice = "whole file"}
                            , bsAction = nfIO $ do
                                r <- MP.parseMethodFile path
                                evaluate (case r of Right m -> length (methodFactors m); Left _ -> 0)
                            }
                        ]

registerMethodCsv :: IO [BenchSpec]
registerMethodCsv = do
    mPath <- F.lookupFixture F.MethodCsv
    case mPath of
        Nothing -> pure []
        Just path -> do
            bytes <- BS.readFile path
            case MPC.parseMethodCSVBytes bytes of
                Left err -> do
                    putStrLn $ "[bench] parser.method_csv: parse failed (" <> err <> "), skipping"
                    pure []
                Right ms -> do
                    let !n = sum (map (length . methodFactors) ms)
                    pure
                        [ BenchSpec
                            { bsCapability = "parser.method_csv"
                            , bsLabel = T.pack ("Parse an LCIA method with " <> show n <> " CFs (generic CSV)")
                            , bsDescription =
                                "Reads a generic CSV export of an LCIA method and parses every characterization \
                                \factor row. This is the path used when methods are shared as flat tables instead \
                                \of XML packs."
                            , bsUnitOfWork = UnitOfWork{uowKind = "characterization_factors", uowN = n}
                            , bsMetric = "milliseconds"
                            , bsFixture = J.Fixture{J.fSource = F.fixtureSourceLabel F.MethodCsv, J.fSlice = "whole file"}
                            , bsAction = parseBytesBench MPC.parseMethodCSVBytes path
                            }
                        ]

registerMethodSimapro :: IO [BenchSpec]
registerMethodSimapro = do
    mPath <- F.lookupFixture F.MethodSimaProCsv
    case mPath of
        Nothing -> pure []
        Just path -> do
            bytes <- BS.readFile path
            case MPS.parseSimaProMethodCSVBytes bytes of
                Left err -> do
                    putStrLn $ "[bench] parser.method_simapro_csv: parse failed (" <> err <> "), skipping"
                    pure []
                Right collection -> do
                    let !n = sum (map (length . methodFactors) (mcMethods collection))
                    pure
                        [ BenchSpec
                            { bsCapability = "parser.method_simapro_csv"
                            , bsLabel = T.pack ("Parse an LCIA method with " <> show n <> " CFs (SimaPro CSV)")
                            , bsDescription =
                                "Reads a SimaPro export of an LCIA method (one CSV containing the impact categories, \
                                \damage categories, normalisation and weighting). SimaPro is one of the most common \
                                \tools for publishing custom methods; this benches the cost of importing one of those \
                                \exports."
                            , bsUnitOfWork = UnitOfWork{uowKind = "characterization_factors", uowN = n}
                            , bsMetric = "milliseconds"
                            , bsFixture = J.Fixture{J.fSource = F.fixtureSourceLabel F.MethodSimaProCsv, J.fSlice = "whole file"}
                            , bsAction = parseBytesBench MPS.parseSimaProMethodCSVBytes path
                            }
                        ]

registerMethodOlca :: IO [BenchSpec]
registerMethodOlca = do
    mPath <- F.lookupFixture F.MethodOlcaJson
    case mPath of
        Nothing -> pure []
        Just path -> do
            bytes <- BS.readFile path
            case MPO.parseOlcaImpactCategoryBytes bytes of
                Left err -> do
                    putStrLn $ "[bench] parser.method_olca_json: parse failed (" <> err <> "), skipping"
                    pure []
                Right method -> do
                    let !n = length (methodFactors method)
                    pure
                        [ BenchSpec
                            { bsCapability = "parser.method_olca_json"
                            , bsLabel = T.pack ("Parse an LCIA method with " <> show n <> " CFs (openLCA JSON)")
                            , bsDescription =
                                "Reads one openLCA-JSON impact-category file and extracts every characterization \
                                \factor. openLCA's JSON-LD schema is the most common interchange format for methods \
                                \shipped through the LCIA Methods Database; this benches the cost of ingesting one."
                            , bsUnitOfWork = UnitOfWork{uowKind = "characterization_factors", uowN = n}
                            , bsMetric = "milliseconds"
                            , bsFixture = J.Fixture{J.fSource = F.fixtureSourceLabel F.MethodOlcaJson, J.fSlice = "whole file"}
                            , bsAction = parseBytesBench MPO.parseOlcaImpactCategoryBytes path
                            }
                        ]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

listFilesByExt :: String -> FilePath -> IO [FilePath]
listFilesByExt ext dir = do
    names <- listDirectory dir
    let matching = sort [n | n <- names, takeExtension n == ext]
    pure (map (dir </>) matching)

{- | Parse a list of files in parallel, fully evaluating every result so the
bench reflects end-to-end work (XML decode + structure build + force).
-}
parallelParseFiles ::
    (NFData a) =>
    (FilePath -> IO a) ->
    [FilePath] ->
    Benchmarkable
parallelParseFiles parser files = nfIO $ do
    results <- mapConcurrently parser files
    evaluate (results `deepseq` length results)

{- | Parse a file's bytes through a pure parser; the file is re-read each
iteration so the timing covers both I/O and parsing — what the
application does on a fresh request.
-}
parseBytesBench ::
    (NFData a) =>
    (BS.ByteString -> Either String a) ->
    FilePath ->
    Benchmarkable
parseBytesBench parser path = nfIO $ do
    bytes <- BS.readFile path
    let !r = parser bytes
    r `deepseq` pure ()
