{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- | Synonym Database Compiler
--
-- Converts the JSON synonym database (from Python build_synonyms_db.py) into
-- a compact binary format compressed with Zstd for embedding in the fplca binary.
--
-- Usage:
--   cabal run synonyms-compiler -- \
--       --input scripts/synonyms_db.json \
--       --output src/LCA/SynonymDB/synonyms.bin.zst
--
-- The output file can be embedded at compile time using file-embed.
module Main where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import qualified Codec.Compression.Zstd as Zstd
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Binary as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Vector as V
import Options.Applicative
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

import LCA.SynonymDB.Types (SynonymDB(..))

-- | Command line options
data Options = Options
    { optInput  :: !FilePath  -- ^ Input JSON file
    , optOutput :: !FilePath  -- ^ Output .bin.zst file
    , optLevel  :: !Int       -- ^ Zstd compression level (1-22)
    } deriving (Show)

optParser :: Parser Options
optParser = Options
    <$> strOption
        (  long "input"
        <> short 'i'
        <> metavar "FILE"
        <> help "Input JSON file (from build_synonyms_db.py)"
        )
    <*> strOption
        (  long "output"
        <> short 'o'
        <> metavar "FILE"
        <> help "Output binary file (e.g., src/LCA/SynonymDB/synonyms.bin.zst)"
        )
    <*> option auto
        (  long "level"
        <> short 'l'
        <> metavar "LEVEL"
        <> value 3
        <> help "Zstd compression level 1-22 (default: 3)"
        )

-- | Parse the JSON structure into SynonymDB
--
-- Expected JSON format:
-- {
--   "name_to_id": { "carbon dioxide": 123, ... },
--   "id_to_synonyms": { "123": ["carbon dioxide", "co2", ...], ... }
-- }
parseJSONToSynonymDB :: A.Value -> Either String SynonymDB
parseJSONToSynonymDB (A.Object obj) = do
    -- Parse name_to_id: Map Text Int
    nameToIdValue <- maybe (Left "Missing 'name_to_id' field") Right
        (KM.lookup "name_to_id" obj)
    nameToId <- parseNameToId nameToIdValue

    -- Parse id_to_synonyms: Map Int [Text]
    idToSynValue <- maybe (Left "Missing 'id_to_synonyms' field") Right
        (KM.lookup "id_to_synonyms" obj)
    idToNames <- parseIdToSynonyms idToSynValue

    Right $ SynonymDB
        { synNameToId = nameToId
        , synIdToNames = idToNames
        }
parseJSONToSynonymDB _ = Left "Expected JSON object at top level"

-- | Parse the name_to_id mapping
parseNameToId :: A.Value -> Either String (M.Map T.Text Int)
parseNameToId (A.Object obj) =
    let pairs = [(AK.toText k, v) | (k, v) <- KM.toList obj]
    in M.fromList <$> mapM parsePair pairs
  where
    parsePair :: (T.Text, A.Value) -> Either String (T.Text, Int)
    parsePair (name, A.Number n) = Right (name, round n)
    parsePair (name, _) = Left $ "Expected number for key: " ++ T.unpack name
parseNameToId _ = Left "Expected object for name_to_id"

-- | Parse the id_to_synonyms mapping
parseIdToSynonyms :: A.Value -> Either String (M.Map Int [T.Text])
parseIdToSynonyms (A.Object obj) =
    let pairs = [(AK.toText k, v) | (k, v) <- KM.toList obj]
    in M.fromList <$> mapM parsePair pairs
  where
    parsePair :: (T.Text, A.Value) -> Either String (Int, [T.Text])
    parsePair (idText, A.Array arr) = do
        let !gid = read (T.unpack idText) :: Int
        names <- mapM parseText (V.toList arr)
        Right (gid, names)
    parsePair (idText, _) = Left $ "Expected array for group: " ++ T.unpack idText

    parseText :: A.Value -> Either String T.Text
    parseText (A.String t) = Right t
    parseText _ = Left "Expected string in synonyms array"
parseIdToSynonyms _ = Left "Expected object for id_to_synonyms"

main :: IO ()
main = do
    opts <- execParser $ info (optParser <**> helper)
        ( fullDesc
        <> progDesc "Compile JSON synonym database to compressed binary"
        <> header "synonyms-compiler - convert synonyms DB for embedding"
        )

    hPutStrLn stderr $ "Reading JSON from: " ++ optInput opts

    -- Read and parse JSON
    jsonBytes <- BSL.readFile (optInput opts)
    jsonValue <- case A.decode jsonBytes of
        Nothing -> fail "Failed to parse JSON"
        Just v -> return v

    -- Convert to SynonymDB
    db <- case parseJSONToSynonymDB jsonValue of
        Left err -> fail $ "Failed to convert JSON: " ++ err
        Right d -> return d

    -- Force evaluation and report stats
    !db' <- evaluate (force db)
    let nameCount = M.size (synNameToId db')
        groupCount = M.size (synIdToNames db')
    hPutStrLn stderr $ printf "Loaded: %d names, %d synonym groups" nameCount groupCount

    -- Serialize to Binary
    let binary = Binary.encode db'
        binarySize = BSL.length binary
    hPutStrLn stderr $ printf "Binary size: %d bytes (%.2f MB)"
        binarySize (fromIntegral binarySize / (1024 * 1024) :: Double)

    -- Compress with Zstd
    let compressed = Zstd.compress (optLevel opts) (BSL.toStrict binary)
        compressedSize = BS.length compressed
        ratio = (fromIntegral compressedSize / fromIntegral binarySize) * 100 :: Double
    hPutStrLn stderr $ printf "Compressed size: %d bytes (%.2f MB, %.1f%% of original)"
        compressedSize (fromIntegral compressedSize / (1024 * 1024) :: Double) ratio

    -- Write output
    BS.writeFile (optOutput opts) compressed
    hPutStrLn stderr $ "Written to: " ++ optOutput opts
    hPutStrLn stderr "Done!"
