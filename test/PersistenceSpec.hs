{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Tests for making an edit outlive the process
('Database.Edit.mutateUploadedDatabase' and
'Database.Export.serializeDatabaseFiles').

Editing used to be transient by design: the sources and the matrix cache still
held the pre-edit database, so an unload and reload deliberately undid the
change. That is defensible for a database the engine reads from configuration
and does not own, and wrong for one it does — a restart quietly resurrected
every activity a user had removed.

What follows pins the three decisions that make persistence honest: which
formats may be written back (only those that record process identity), that a
database with no files of its own is given a home rather than writing through
to the database it was copied from, and that an edit which is not saved says
so instead of looking like one that was.
-}
module PersistenceSpec (spec) where

import Control.Concurrent.STM (atomically, modifyTVar')
import Control.Exception (bracket_)
import Data.List (sort)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import Data.Text (Text, isInfixOf)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.Vector.Unboxed as U
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Config (DatabaseConfig (..), defaultConfig)
import Database (buildDatabaseWithMatrices)
import Database.Edit (
    DeleteRequest (..),
    MutationOutcome (..),
    deleteActivitiesInDB,
    mutateUploadedDatabase,
 )
import Database.Export (serializeDatabaseFiles)
import Database.Manager (
    DatabaseManager (..),
    LoadedDatabase (..),
    initDatabaseManager,
    loadDatabase,
    unloadDatabase,
 )
import Database.Rebuild (deleteActivitiesWith)
import Database.Upload (DatabaseFormat (..))
import Database.UploadedDatabase (UploadMeta (..), readUploadMeta)
import SharedSolver (SharedSolver, createSharedSolver)
import Types (
    Activity (..),
    BioDirection (..),
    BiosphereFlow (..),
    Compartment (..),
    Database (..),
    Exchange (..),
    GeographyPolicy (..),
    LocationSource (..),
    SparseTriple (..),
    TechRole (..),
    TechnosphereFlow (..),
    UUID,
    Unit (..),
    findProcessId,
 )
import UnitConversion (defaultUnitConfig)

spec :: Spec
spec = describe "persisting an edit" $ do
    describe "serializeDatabaseFiles" $ do
        it "writes one EcoSpold 2 dataset per process, named by its identity" $ do
            db <- buildFixture
            case serializeDatabaseFiles EcoSpold2 db of
                Left err -> expectationFailure ("expected EcoSpold 2 to be writable: " <> show err)
                Right (entries, warnings) -> do
                    warnings `shouldBe` []
                    -- The name is the identity: the parser reads the pair back
                    -- off it, which is the whole reason this format may be
                    -- written in place.
                    sort (map fst entries)
                        `shouldBe` [T.unpack (UUID.toText supplierActId <> "_" <> UUID.toText supplierProdId) <> ".spold"]

        it "refuses every format that would re-mint identity on read" $
            -- Exporting to these stays available; writing a database back over
            -- its own sources does not, because the rows that survive would
            -- come back under different process ids after a restart, and every
            -- reference anyone kept to them would point elsewhere.
            mapM_
                refusesIdentity
                [(SimaProCSV, "SimaPro CSV"), (EcoSpold1, "EcoSpold 1"), (ILCDProcess, "ILCD"), (BrightwayExcel, "Brightway Excel")]

        it "refuses the formats that have no writer at all" $ do
            db <- buildFixture
            fmap fst (serializeDatabaseFiles OpenLcaJsonLd db)
                `shouldSatisfy` failsWith "not supported"
            fmap fst (serializeDatabaseFiles UnknownFormat db)
                `shouldSatisfy` failsWith "unknown format"

    describe "mutateUploadedDatabase" $ do
        it "rewrites the sources of a database that owns them" $
            withDataDir $ \dataRoot -> do
                manager <- initDatabaseManager defaultConfig True Nothing
                db <- buildTwoActivityFixture
                let dataDir = dataRoot </> "uploads" </> "databases" </> "own-files" </> "data"
                createDirectoryIfMissing True dataDir
                install manager "own-files" db (uploadedConfig "own-files" dataDir)
                r <- mutateUploadedDatabase manager "own-files" (dropSecond db)
                case r of
                    Left err -> expectationFailure ("mutateUploadedDatabase: " <> show err)
                    Right outcome -> do
                        moPersisted outcome `shouldBe` True
                        -- One process left, in a file named after its own
                        -- identity: the deletion reached the sources, not only
                        -- the copy of the database held in memory.
                        listDirectory dataDir
                            `shouldReturn` [T.unpack (keyText (supplierActId, supplierProdId)) <> ".spold"]
                        -- Nothing of the staging or the previous generation is left behind.
                        doesDirectoryExist (dataDir <> ".new") `shouldReturn` False
                        doesDirectoryExist (dataDir <> ".old") `shouldReturn` False

        it "gives a database with no files of its own a home instead of writing through" $
            -- A copy shares the source's value without duplicating its
            -- directory; writing through the shared path would edit a database
            -- nobody asked to edit.
            withDataDir $ \dataRoot -> do
                manager <- initDatabaseManager defaultConfig True Nothing
                db <- buildTwoActivityFixture
                let elsewhere = dataRoot </> "somebody-elses" </> "data"
                createDirectoryIfMissing True elsewhere
                writeFile (elsewhere </> "untouched.txt") "the source of the copy"
                install manager "the-copy" db{dbDependsOn = ["background"]} (uploadedConfig "the-copy" elsewhere)
                r <- mutateUploadedDatabase manager "the-copy" (dropSecond db)
                case r of
                    Left err -> expectationFailure ("mutateUploadedDatabase: " <> show err)
                    Right outcome -> do
                        moPersisted outcome `shouldBe` True
                        let home = dataRoot </> "uploads" </> "databases" </> "the-copy"
                        meta <- readUploadMeta home
                        fmap umFormat meta `shouldBe` Just EcoSpold2
                        fmap umDataPath meta `shouldBe` Just "data"
                        -- The dependency pin is written down, not left to the
                        -- binary cache that a restart may not read.
                        fmap umDepends meta `shouldBe` Just ["background"]
                        listDirectory (home </> "data") >>= (`shouldSatisfy` ((== 1) . length))
                        doesFileExist (elsewhere </> "untouched.txt") `shouldReturn` True
                        listDirectory elsewhere `shouldReturn` ["untouched.txt"]

        it "does not take a sibling's files for its own when its name is a prefix of theirs" $
            -- Ownership is judged on path components: a database named "agri"
            -- pointing at "agribalyse"'s data directory (a copy keeps its
            -- source's path) must be given a home of its own, not rewrite the
            -- sibling whose name it happens to prefix.
            withDataDir $ \dataRoot -> do
                manager <- initDatabaseManager defaultConfig True Nothing
                db <- buildTwoActivityFixture
                let siblingDir = dataRoot </> "uploads" </> "databases" </> "agribalyse" </> "data"
                createDirectoryIfMissing True siblingDir
                writeFile (siblingDir </> "untouched.spold") "the sibling's dataset"
                install manager "agri" db (uploadedConfig "agri" siblingDir)
                r <- mutateUploadedDatabase manager "agri" (dropSecond db)
                case r of
                    Left err -> expectationFailure ("mutateUploadedDatabase: " <> show err)
                    Right outcome -> do
                        moPersisted outcome `shouldBe` True
                        listDirectory siblingDir `shouldReturn` ["untouched.spold"]
                        meta <- readUploadMeta (dataRoot </> "uploads" </> "databases" </> "agri")
                        fmap umDataPath meta `shouldBe` Just "data"

        it "reloads to what was written, not to the pre-edit sources" $
            -- The point of the whole path: unloading and loading again returns
            -- the edited database, where it used to resurrect the original.
            withDataDir $ \dataRoot -> do
                manager <- initDatabaseManager defaultConfig True Nothing
                db <- buildTwoActivityFixture
                let dataDir = dataRoot </> "uploads" </> "databases" </> "reload-me" </> "data"
                createDirectoryIfMissing True dataDir
                install manager "reload-me" db (uploadedConfig "reload-me" dataDir)
                r <- mutateUploadedDatabase manager "reload-me" (dropSecond db)
                either (expectationFailure . ("mutateUploadedDatabase: " <>) . show) (const (pure ())) r
                unloadDatabase manager "reload-me" `shouldReturn` Right ()
                reloaded <- loadDatabase manager "reload-me"
                case reloaded of
                    Left err -> expectationFailure ("loadDatabase: " <> show err)
                    Right (loaded, _) -> dbActivityCount (ldDatabase loaded) `shouldBe` 1

        it "refuses a second edit while one is in progress" $
            withDataDir $ \_ -> do
                manager <- initDatabaseManager defaultConfig True Nothing
                db <- buildTwoActivityFixture
                install manager "busy" db (configuredConfig "busy")
                atomically $ modifyTVar' (dmStagingDbs manager) (Set.insert "busy")
                r <- mutateUploadedDatabase manager "busy" (dropSecond db)
                case r of
                    Left err -> err `shouldSatisfy` isInfixOf "already in progress"
                    Right _ -> expectationFailure "expected the second edit to be refused"

        it "says an edit is not saved when the database is one the engine only reads" $
            withDataDir $ \dataRoot -> do
                manager <- initDatabaseManager defaultConfig True Nothing
                db <- buildTwoActivityFixture
                install manager "configured" db (configuredConfig "configured")
                r <- mutateUploadedDatabase manager "configured" (dropSecond db)
                case r of
                    Left err -> expectationFailure ("mutateUploadedDatabase: " <> show err)
                    Right outcome -> do
                        moPersisted outcome `shouldBe` False
                        -- No home was made for it: a configured database owns its
                        -- files through the config file, and the engine writes none.
                        listDirectory (dataRoot </> "uploads" </> "databases") `shouldReturn` []

        it "refuses while another loaded database depends on this one" $
            withDataDir $ \_ -> do
                manager <- initDatabaseManager defaultConfig True Nothing
                db <- buildTwoActivityFixture
                install manager "background" db (configuredConfig "background")
                install manager "foreground" db{dbDependsOn = ["background"]} (configuredConfig "foreground")
                r <- mutateUploadedDatabase manager "background" (dropSecond db)
                case r of
                    Left err -> err `shouldSatisfy` isInfixOf "still required by"
                    Right _ -> expectationFailure "expected the edit to be refused while a dependent is loaded"

    describe "deleteActivitiesInDB" $
        it "refuses to write a deletion back in a format that cannot record identity" $
            withDataDir $ \dataRoot -> do
                manager <- initDatabaseManager defaultConfig True Nothing
                db <- buildTwoActivityFixture
                let dataDir = dataRoot </> "uploads" </> "databases" </> "simapro-db" </> "data"
                createDirectoryIfMissing True dataDir
                install manager "simapro-db" db (uploadedConfig "simapro-db" dataDir){dcFormat = Just SimaProCSV}
                r <- deleteActivitiesInDB manager "simapro-db" (deleteIds [keyText (supplierActId, supplierProdId)])
                case r of
                    Left err -> err `shouldSatisfy` isInfixOf "does not record process identifiers"
                    Right _ -> expectationFailure "expected the deletion to be refused rather than silently unsaved"

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

failsWith :: Text -> Either Text a -> Bool
failsWith needle = either (isInfixOf needle) (const False)

refusesIdentity :: (DatabaseFormat, Text) -> Expectation
refusesIdentity (fmt, label) = do
    db <- buildFixture
    case serializeDatabaseFiles fmt db of
        Right _ -> expectationFailure (T.unpack label <> " should be refused for writing in place")
        Left err -> do
            err `shouldSatisfy` isInfixOf label
            err `shouldSatisfy` isInfixOf "does not record process identifiers"

{- | Point the engine's data directory at a scratch tree for the duration of
one example, so nothing is written next to the sources.
-}
withDataDir :: (FilePath -> IO ()) -> IO ()
withDataDir act =
    withSystemTempDirectory "volca-persist" $ \dir ->
        bracket_ (setEnv "VOLCA_DATA_DIR" dir) (unsetEnv "VOLCA_DATA_DIR") (act dir)

keyText :: (UUID, UUID) -> Text
keyText (a, p) = UUID.toText a <> "_" <> UUID.toText p

{- | Remove the second activity of the two-activity fixture: any edit will do
here, and a deletion is the one every database supports, so these examples pin
the mutation path itself rather than what happened to be edited.
-}
dropSecond :: Database -> Database -> Either Text Database
dropSecond fixture db = case findProcessId fixture otherActId supplierProdId of
    Nothing -> Left "fixture: the activity to delete is not in it"
    Just pid -> deleteActivitiesWith defaultUnitConfig [pid] db

deleteIds :: [Text] -> DeleteRequest
deleteIds ids =
    DeleteRequest
        { drName = Nothing
        , drLocation = Nothing
        , drProduct = Nothing
        , drClassifications = []
        , drExactName = False
        , drKeep = []
        , drExtra = []
        , drIds = Just ids
        }

install :: DatabaseManager -> Text -> Database -> DatabaseConfig -> IO ()
install manager name db config = do
    solver <- mkSolver name db
    let loaded = LoadedDatabase{ldDatabase = db, ldSharedSolver = solver, ldConfig = config}
    atomically $ do
        modifyTVar' (dmLoadedDbs manager) (M.insert name loaded)
        modifyTVar' (dmAvailableDbs manager) (M.insert name config)

mkSolver :: Text -> Database -> IO SharedSolver
mkSolver name db =
    let triples = [(fromIntegral i, fromIntegral j, v) | SparseTriple i j v <- U.toList (dbTechnosphereTriples db)]
     in createSharedSolver name triples (fromIntegral (dbActivityCount db))

baseConfig :: Text -> DatabaseConfig
baseConfig name =
    DatabaseConfig
        { dcName = name
        , dcDisplayName = name
        , dcPath = ""
        , dcDescription = Nothing
        , dcLoad = True
        , dcDefault = False
        , dcDepends = []
        , dcLocationAliases = M.empty
        , dcFormat = Just EcoSpold2
        , dcIsUploaded = False
        , dcDeletable = True
        , dcGeographyPolicy = GeoGlobal
        }

uploadedConfig :: Text -> FilePath -> DatabaseConfig
uploadedConfig name dataDir = (baseConfig name){dcPath = dataDir, dcIsUploaded = True}

configuredConfig :: Text -> DatabaseConfig
configuredConfig = baseConfig

-- ---------------------------------------------------------------------------
-- Fixture
-- ---------------------------------------------------------------------------

buildFixture :: IO Database
buildFixture = buildFrom (M.singleton (supplierActId, supplierProdId) (milkActivity "milk production"))

buildTwoActivityFixture :: IO Database
buildTwoActivityFixture =
    buildFrom $
        M.fromList
            [ ((supplierActId, supplierProdId), milkActivity "milk production")
            , ((otherActId, supplierProdId), milkActivity "milk production, organic")
            ]

buildFrom :: M.Map (UUID, UUID) Activity -> IO Database
buildFrom activities = do
    r <-
        buildDatabaseWithMatrices
            defaultUnitConfig
            activities
            (M.singleton supplierProdId milkFlow)
            (M.singleton co2Id co2Flow)
            M.empty
            unitTable
    either (fail . show) pure r

mkUUID :: Int -> UUID
mkUUID n = UUID.fromWords64 (fromIntegral n) 0

supplierActId, otherActId, supplierProdId, co2Id, kgUnitId :: UUID
supplierActId = mkUUID 1
otherActId = mkUUID 4
supplierProdId = mkUUID 2
co2Id = mkUUID 3
kgUnitId = mkUUID 10

unitTable :: M.Map UUID Unit
unitTable = M.singleton kgUnitId Unit{unitId = kgUnitId, unitName = "kg", unitSymbol = "kg", unitComment = ""}

milkFlow :: TechnosphereFlow
milkFlow =
    TechnosphereFlow
        { tfId = supplierProdId
        , tfName = "milk"
        , tfUnitId = kgUnitId
        , tfSynonyms = M.empty
        , tfCAS = Nothing
        , tfSubstanceId = Nothing
        }

air :: Compartment
air = Compartment{compartmentName = "air", compartmentSub = Nothing}

co2Flow :: BiosphereFlow
co2Flow =
    BiosphereFlow
        { bfId = co2Id
        , bfName = "Carbon dioxide"
        , bfUnitId = kgUnitId
        , bfSynonyms = M.empty
        , bfCAS = Just "124-38-9"
        , bfSubstanceId = Nothing
        , bfCompartment = Just air
        }

milkActivity :: Text -> Activity
milkActivity name =
    Activity
        { activityName = name
        , activityDescription = []
        , activitySynonyms = M.empty
        , activityClassification = M.empty
        , activityLocation = "FR"
        , activityLocationSource = LocationDeclared
        , activityUnit = "kg"
        , exchanges =
            [ TechnosphereExchange
                { techFlowId = supplierProdId
                , techAmount = 1.0
                , techUnitId = kgUnitId
                , techRole = ReferenceProduct
                , techActivityLinkId = supplierActId
                , techProcessLinkId = Nothing
                , techLocation = ""
                , techComment = Nothing
                , techPedigree = Nothing
                }
            , BiosphereExchange
                { bioFlowId = co2Id
                , bioAmount = 1.2
                , bioUnitId = kgUnitId
                , bioDirection = Emission
                , bioLocation = ""
                , bioComment = Nothing
                , bioPedigree = Nothing
                }
            ]
        , activityParams = M.empty
        , activityParamExprs = M.empty
        , activityAllocationPercent = Nothing
        , activityAllocationFormula = Nothing
        , activityNativeType = Nothing
        , activityNativeId = Nothing
        , activityFormulaCheck = Nothing
        }
