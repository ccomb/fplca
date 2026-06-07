{-# LANGUAGE OverloadedStrings #-}

{- | Relink-with-mapping tests.

A small target ("BAFU") database is indexed as a cross-DB supplier. A consumer
input flow whose name only matches the target *via* an alias from the mapping
CSV must link once the alias map is threaded into the 'LinkingContext'; without
it the same lookup yields 'NoNameMatch'. Unit incompatibility surfaces as an
error rather than being silently dropped, and the post-link score matches the
expected name+location total.
-}
module RelinkMappingSpec (spec) where

import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.UUID as UUID
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import Test.Hspec

import Database.CrossLinking (
    CrossDBLinkResult (..),
    IndexedDatabase,
    LinkingContext (..),
    buildIndexedDatabase,
    defaultLinkingThreshold,
    findSupplierInIndexedDBs,
    locationHierarchy,
 )
import Database.RelinkMapping (
    AliasRow (..),
    buildAliasMap,
    loadAliasMap,
    parseAliasCSV,
    rejectEmpty,
 )
import SynonymDB (emptySynonymDB)
import Types (
    Activity (..),
    Exchange (..),
    GeographyPolicy (..),
    LinkBlocker (..),
    SimpleDatabase (..),
    TechRole (..),
    TechnosphereFlow (..),
 )
import UnitConversion (defaultUnitConfig)

-- ---------------------------------------------------------------------------
-- Target ("BAFU") fixture: one activity, "wheat production" @ FR, in kg.
-- ---------------------------------------------------------------------------

targetIndexed :: IndexedDatabase
targetIndexed = buildIndexedDatabase "BAFU" emptySynonymDB targetDB

targetDB :: SimpleDatabase
targetDB =
    let flowUUID = read "aaaaaaaa-0000-0000-0000-000000000001"
        actUUID = read "cccccccc-0000-0000-0000-000000000001"
        ex =
            TechnosphereExchange
                { techFlowId = flowUUID
                , techAmount = 1.0
                , techUnitId = UUID.nil
                , techRole = ReferenceProduct
                , techActivityLinkId = UUID.nil
                , techProcessLinkId = Nothing
                , techLocation = ""
                , techComment = Nothing
                , techPedigree = Nothing
                }
        act =
            Activity
                { activityName = "wheat production"
                , activityDescription = []
                , activitySynonyms = M.empty
                , activityClassification = M.empty
                , activityLocation = "FR"
                , activityUnit = "kg"
                , exchanges = [ex]
                , activityParams = M.empty
                , activityParamExprs = M.empty
                , activityAllocationPercent = Nothing
                , activityAllocationFormula = Nothing
                , activityNativeType = Nothing
                }
        flow =
            TechnosphereFlow
                { tfId = flowUUID
                , tfName = "wheat production"
                , tfUnitId = UUID.nil
                , tfSynonyms = M.empty
                , tfCAS = Nothing
                , tfSubstanceId = Nothing
                }
     in SimpleDatabase
            { sdbActivities = M.singleton (actUUID, flowUUID) act
            , sdbTechFlows = M.singleton flowUUID flow
            , sdbBioFlows = M.empty
            , sdbWasteFlows = M.empty
            , sdbUnits = M.empty
            }

-- | Linking context against the target, with an optional alias map.
mkCtx :: Maybe (M.Map Text Text) -> LinkingContext
mkCtx aliases =
    LinkingContext
        { lcIndexedDatabases = [targetIndexed]
        , lcSynonymDB = emptySynonymDB
        , lcUnitConfig = defaultUnitConfig
        , lcThreshold = defaultLinkingThreshold
        , lcLocationHierarchy = locationHierarchy
        , lcGeographyPolicy = GeoGlobal
        , lcSupplierAliases = aliases
        }

-- | The consumer's Ecoinvent-style name that only resolves via the alias.
consumerName :: Text
consumerName = "wheat, ecoinvent name"

aliasMap :: M.Map Text Text
aliasMap = M.singleton consumerName "wheat production"

-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
    describe "alias CSV loading" $ do
        it "parses source/target columns and builds the map" $ do
            -- The source name contains a comma, so it must be CSV-quoted.
            let csv = BLC.pack "source,target\n\"wheat, ecoinvent name\",wheat production\n"
            rows <- either (fail . show) pure (parseAliasCSV csv)
            map arSource rows `shouldBe` [consumerName]
            map arTarget rows `shouldBe` ["wheat production"]
            buildAliasMap rows `shouldBe` Right aliasMap

        it "accepts the from/to column synonyms and optional locations" $ do
            let csv = BLC.pack "from,to,source_location\na,b,FR\n"
            rows <- either (fail . show) pure (parseAliasCSV csv)
            map arSourceLocation rows `shouldBe` [Just "FR"]
            buildAliasMap rows `shouldBe` Right (M.singleton "a" "b")

        it "rejects a conflicting alias for the same source" $ do
            let rows =
                    [ AliasRow "x" "y" Nothing Nothing
                    , AliasRow "x" "z" Nothing Nothing
                    ]
            buildAliasMap rows `shouldSatisfy` either (const True) (const False)

        it "fails when a required column is missing" $ do
            let csv = BLC.pack "source\nfoo\n"
            parseAliasCSV csv `shouldSatisfy` either (const True) (const False)

        it "rejects a present-but-blank target as a half-specified row" $ do
            -- Source filled, target blank: a curation mistake, not a no-op,
            -- so it must fail loudly rather than be silently dropped.
            let csv = BLC.pack "source,target\nwheat production,\n"
            parseAliasCSV csv `shouldSatisfy` either (const True) (const False)

        it "rejects a header-only file as having no usable rows" $
            withSystemTempFile "relink-empty.csv" $ \path h -> do
                BLC.hPut h (BLC.pack "source,target\n")
                hClose h
                result <- loadAliasMap path
                result `shouldSatisfy` either (const True) (const False)

        it "rejectEmpty rejects an empty map and passes a non-empty one" $ do
            -- The HTTP relink handler relies on this so a header-only CSV is a 4xx,
            -- not a silent 200 no-op (matching the CLI's loadAliasMap).
            rejectEmpty M.empty `shouldSatisfy` either (const True) (const False)
            rejectEmpty (M.singleton "a" "b") `shouldBe` Right (M.singleton "a" "b")

        it "collapses two identical duplicate rows harmlessly" $ do
            let rows =
                    [ AliasRow "x" "y" Nothing Nothing
                    , AliasRow "x" "y" Nothing Nothing
                    ]
            buildAliasMap rows `shouldBe` Right (M.singleton "x" "y")

        it "round-trips a quoted target that contains a comma" $ do
            -- The target name contains a comma, so it must be CSV-quoted.
            let csv = BLC.pack "source,target\nwheat,\"production, FR\"\n"
            rows <- either (fail . show) pure (parseAliasCSV csv)
            buildAliasMap rows `shouldBe` Right (M.singleton "wheat" "production, FR")

    describe "findSupplierInIndexedDBs with supplier aliases" $ do
        it "does NOT link the aliased consumer name without the alias map" $
            case findSupplierInIndexedDBs (mkCtx Nothing) consumerName "FR" "kg" of
                CrossDBNotLinked NoNameMatch -> pure ()
                CrossDBNotLinked other -> expectationFailure $ "Expected NoNameMatch, got: " ++ show other
                CrossDBLinked{} -> expectationFailure "Expected no link without alias map"

        it "links the aliased consumer name to the target via the alias map" $
            case findSupplierInIndexedDBs (mkCtx (Just aliasMap)) consumerName "FR" "kg" of
                CrossDBLinked{cdlrProductName = name, cdlrDatabaseName = db} -> do
                    name `shouldBe` "wheat production"
                    db `shouldBe` "BAFU"
                CrossDBNotLinked reason -> expectationFailure $ "Expected link, got: " ++ show reason

        it "scores name(50) + exact-location(30) = 80 for an FR→FR alias link" $
            case findSupplierInIndexedDBs (mkCtx (Just aliasMap)) consumerName "FR" "kg" of
                CrossDBLinked{cdlrScore = score} -> score `shouldBe` 80
                CrossDBNotLinked reason -> expectationFailure $ "Expected link, got: " ++ show reason

        it "surfaces a unit mismatch as UnitIncompatible, never a silent drop" $
            case findSupplierInIndexedDBs (mkCtx (Just aliasMap)) consumerName "FR" "m3" of
                CrossDBNotLinked (UnitIncompatible req got) -> do
                    req `shouldBe` "m3"
                    got `shouldBe` "kg"
                CrossDBNotLinked other -> expectationFailure $ "Expected UnitIncompatible, got: " ++ show other
                CrossDBLinked{} -> expectationFailure "Expected unit mismatch to block the link"

        it "still prefers a direct name match over the alias map" $
            -- The target's own canonical name resolves directly; the alias is
            -- only a last-resort retry, so a direct lookup must not depend on it.
            case findSupplierInIndexedDBs (mkCtx (Just aliasMap)) "wheat production" "FR" "kg" of
                CrossDBLinked{cdlrScore = score} -> score `shouldBe` 80
                CrossDBNotLinked reason -> expectationFailure $ "Expected direct link, got: " ++ show reason
