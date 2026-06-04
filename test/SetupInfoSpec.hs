{-# LANGUAGE OverloadedStrings #-}

module SetupInfoSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.UUID as UUID
import Test.Hspec

import Config (DatabaseConfig (..))
import Database (buildDatabaseWithMatrices)
import Database.Manager (
    DatabaseSetupInfo (..),
    MissingSupplier (..),
    buildLoadedSetupInfo,
 )
import Types
import UnitConversion (defaultUnitConfig)

-- ---------------------------------------------------------------------------
-- Minimal fixtures
-- ---------------------------------------------------------------------------

consumerAct, consumerProd, supplierAct, supplierProd, missingAct :: UUID.UUID
consumerAct = read "cccccccc-0000-0000-0000-000000000001"
consumerProd = read "aaaaaaaa-0000-0000-0000-000000000001"
supplierAct = read "cccccccc-0000-0000-0000-000000000002"
supplierProd = read "bbbbbbbb-0000-0000-0000-000000000002"
missingAct = read "dddddddd-0000-0000-0000-000000000099"

minimalFlow :: UUID.UUID -> Text -> TechnosphereFlow
minimalFlow fid name =
    TechnosphereFlow
        { tfId = fid
        , tfName = name
        , tfUnitId = UUID.nil
        , tfSynonyms = M.empty
        , tfCAS = Nothing
        , tfSubstanceId = Nothing
        }

minimalActivity :: Text -> [Exchange] -> Activity
minimalActivity name exs =
    Activity
        { activityName = name
        , activityDescription = []
        , activitySynonyms = M.empty
        , activityClassification = M.empty
        , activityLocation = "GLO"
        , activityUnit = "kg"
        , exchanges = exs
        , activityParams = M.empty
        , activityParamExprs = M.empty
        , activityAllocationPercent = Nothing
        , activityAllocationFormula = Nothing
        , activityNativeType = Nothing
        }

refExchange :: UUID.UUID -> Exchange
refExchange fid =
    TechnosphereExchange
        { techFlowId = fid
        , techAmount = 1.0
        , techUnitId = UUID.nil
        , techRole = ReferenceProduct
        , techActivityLinkId = UUID.nil
        , techProcessLinkId = Nothing
        , techLocation = "GLO"
        , techComment = Nothing
        , techPedigree = Nothing
        }

-- | A technosphere input for @prodId@ linked to producer activity @actId@.
linkedInput :: UUID.UUID -> UUID.UUID -> Exchange
linkedInput actId prodId =
    TechnosphereExchange
        { techFlowId = prodId
        , techAmount = 0.5
        , techUnitId = UUID.nil
        , techRole = Input
        , techActivityLinkId = actId
        , techProcessLinkId = Nothing
        , techLocation = "GLO"
        , techComment = Nothing
        , techPedigree = Nothing
        }

{- | A loaded database has no UI picker; only its name/path matter to the setup
info, so a permissive stub config suffices.
-}
stubConfig :: DatabaseConfig
stubConfig =
    DatabaseConfig
        { dcName = "test"
        , dcDisplayName = "Test DB"
        , dcPath = ""
        , dcDescription = Nothing
        , dcLoad = True
        , dcDefault = False
        , dcDepends = []
        , dcLocationAliases = M.empty
        , dcFormat = Nothing
        , dcIsUploaded = False
        , dcDeletable = False
        , dcGeographyPolicy = GeoGlobal
        }

buildDb :: [((UUID.UUID, UUID.UUID), Activity)] -> [(UUID.UUID, Text)] -> IO Database
buildDb acts flows = do
    res <-
        buildDatabaseWithMatrices
            defaultUnitConfig
            (M.fromList acts)
            (M.fromList [(fid, minimalFlow fid name) | (fid, name) <- flows])
            M.empty
            M.empty
            M.empty
    case res of
        Left err -> error ("buildDatabaseWithMatrices failed: " <> show err)
        Right db -> pure db

-- | Setup info for a self-contained loaded database (no deps, no other DBs).
setupInfoFor :: Database -> DatabaseSetupInfo
setupInfoFor db = buildLoadedSetupInfo stubConfig db M.empty M.empty

-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
    -- A single-.spold import: the foreground consumer carries a non-nil
    -- activityLinkId to a background activity it doesn't ship. The matrix
    -- builder drops that input, so a loaded database (which bypasses the
    -- finalize gate) must report as not-ready with 0% completeness and name the
    -- missing background product — never a green "ready" badge over a silently
    -- zero score.
    describe "buildLoadedSetupInfo (partial EcoSpold2 import)" $ do
        it "reports a dangling background link as not ready / 0% / named" $ do
            let consumer =
                    minimalActivity
                        "lyocell fibre"
                        [refExchange consumerProd, linkedInput missingAct supplierProd]
            db <-
                buildDb
                    [((consumerAct, consumerProd), consumer)]
                    [(consumerProd, "lyocell fibre"), (supplierProd, "chemical, inorganic")]
            let info = setupInfoFor db
            dsiIsReady info `shouldBe` False
            dsiCompleteness info `shouldBe` 0.0
            map msProductName (dsiMissingSuppliers info) `shouldBe` ["chemical, inorganic"]

    -- The same shape, but the background activity is present: every input
    -- resolves internally, so the database stays ready at 100% with no gaps.
    describe "buildLoadedSetupInfo (well-formed self-contained database)" $ do
        it "reports a fully resolved database as ready / 100% / no gaps" $ do
            let consumer =
                    minimalActivity
                        "lyocell fibre"
                        [refExchange consumerProd, linkedInput supplierAct supplierProd]
                supplier = minimalActivity "chemical, inorganic" [refExchange supplierProd]
            db <-
                buildDb
                    [ ((consumerAct, consumerProd), consumer)
                    , ((supplierAct, supplierProd), supplier)
                    ]
                    [(consumerProd, "lyocell fibre"), (supplierProd, "chemical, inorganic")]
            let info = setupInfoFor db
            dsiIsReady info `shouldBe` True
            dsiCompleteness info `shouldBe` 100.0
            dsiMissingSuppliers info `shouldBe` []
