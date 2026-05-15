{-# LANGUAGE DeriveGeneric #-}

{- | Stable JSON schema for bench-results.json, intended to be consumed by an
external session that builds the public performance page. The schema is
versioned via 'soSchemaVersion'; bumps are visible to consumers.

A 'BenchResult' carries enough human-readable context (label, description,
metric, unit_of_work) that a non-LCA, non-Haskell reader can interpret each
row without looking at the bench source.
-}
module Bench.Json (
    SchemaOutput (..),
    Metadata (..),
    Hardware (..),
    BenchResult (..),
    UnitOfWork (..),
    Fixture (..),
    Derived (..),
    BenchSpec (..),
    schemaVersion,
    writeBenchResults,
) where

import Criterion.Main (Benchmarkable)
import Data.Aeson (ToJSON (..), genericToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Bumped only when the on-the-wire shape changes in a non-additive way.
schemaVersion :: Int
schemaVersion = 1

data SchemaOutput = SchemaOutput
    { soSchemaVersion :: !Int
    , soMetadata :: !Metadata
    , soResults :: ![BenchResult]
    }
    deriving (Generic, Show)

data Metadata = Metadata
    { mGitSha :: !(Maybe Text)
    , mGhc :: !(Maybe Text)
    , mTimestampIso :: !Text
    , mHardware :: !Hardware
    }
    deriving (Generic, Show)

data Hardware = Hardware
    { hwCpu :: !(Maybe Text)
    , hwCores :: !(Maybe Int)
    , hwRamGb :: !(Maybe Double)
    , hwOs :: !(Maybe Text)
    }
    deriving (Generic, Show)

data BenchResult = BenchResult
    { brCapability :: !Text
    -- ^ Stable slug, e.g. @"parser.ecospold2"@.
    , brLabel :: !Text
    -- ^ ≤80 chars, human-readable English.
    , brDescription :: !Text
    -- ^ 1–3 sentences explaining the operation and why it matters.
    , brUnitOfWork :: !UnitOfWork
    , brMetric :: !Text
    -- ^ Unit of @brMean@ / @brStddev@: @"seconds"@ or @"milliseconds"@.
    , brFixture :: !Fixture
    , brMean :: !Double
    , brStddev :: !Double
    , brSamples :: !Int
    , brDerived :: !Derived
    }
    deriving (Generic, Show)

data UnitOfWork = UnitOfWork
    { uowKind :: !Text
    -- ^ E.g. @"ecospold2_files"@, @"processes"@, @"characterization_factors"@.
    , uowN :: !Int
    -- ^ Item count this row reports on.
    }
    deriving (Generic, Show)

data Fixture = Fixture
    { fSource :: !Text
    -- ^ Provenance, e.g. @"ecoinvent-3.11"@. Stays in metadata, not in label.
    , fSlice :: !Text
    -- ^ How items were selected, e.g. @"first 1000 by uuid"@.
    }
    deriving (Generic, Show)

{- | Pre-computed display-friendly figure: items per second.

@mean@ already carries the « seconds for N items » headline, so it isn't
duplicated here.
-}
newtype Derived = Derived
    { dItemsPerSecond :: Double
    }
    deriving (Generic, Show)

{- | Static plan for one bench: everything except the measurement itself.
The orchestrator runs 'bsAction' through 'Criterion.benchmark'' and turns
the resulting Report into a 'BenchResult' using the descriptive fields
verbatim. We rely on Criterion's 'nf'/'nfIO' helpers because they wrap
the action so GHC's optimiser can't hoist the work out as a CAF.
-}
data BenchSpec = BenchSpec
    { bsCapability :: !Text
    , bsLabel :: !Text
    , bsDescription :: !Text
    , bsUnitOfWork :: !UnitOfWork
    , bsMetric :: !Text
    , bsFixture :: !Fixture
    , bsAction :: !Benchmarkable
    }

-- ---------------------------------------------------------------------------
-- Aeson instances — explicit field labelling for stable JSON keys.
-- ---------------------------------------------------------------------------

opts :: String -> Aeson.Options
opts prefix =
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = stripPrefix prefix
        , Aeson.omitNothingFields = True
        }
  where
    stripPrefix p s = case splitAt (length p) s of
        (pfx, rest) | pfx == p -> Aeson.camelTo2 '_' rest
        _ -> Aeson.camelTo2 '_' s

instance ToJSON SchemaOutput where
    toJSON = genericToJSON (opts "so")

instance ToJSON Metadata where
    toJSON = genericToJSON (opts "m")

instance ToJSON Hardware where
    toJSON = genericToJSON (opts "hw")

instance ToJSON BenchResult where
    toJSON = genericToJSON (opts "br")

instance ToJSON UnitOfWork where
    toJSON = genericToJSON (opts "uow")

instance ToJSON Fixture where
    toJSON = genericToJSON (opts "f")

instance ToJSON Derived where
    toJSON = genericToJSON (opts "d")

writeBenchResults :: FilePath -> SchemaOutput -> IO ()
writeBenchResults path out =
    BL.writeFile path (AesonPretty.encodePretty' prettyConfig out)
  where
    prettyConfig =
        AesonPretty.defConfig
            { AesonPretty.confIndent = AesonPretty.Spaces 2
            , AesonPretty.confTrailingNewline = True
            }
