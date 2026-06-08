{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      : API.DatabaseHandlers
Description : Database management API handlers

Extracted from API.Routes to reduce module size and improve organization.
Contains handlers for database listing, loading, unloading, uploading, and deletion.
-}
module API.DatabaseHandlers (
    -- * Handlers
    getDatabases,
    loadDatabaseHandler,
    unloadDatabaseHandler,
    relinkDatabaseHandler,
    copyDatabaseHandler,
    deleteDatabaseHandler,
    deleteActivitiesHandler,
    exportDatabaseHandler,
    uploadDatabaseHandler,
    uploadMethodHandler,
    deleteMethodHandler,

    -- * Setup Page Handlers
    getDatabaseSetupHandler,
    addDependencyHandler,
    removeDependencyHandler,
    setDataPathHandler,
    finalizeDatabaseHandler,

    -- * Reference data handlers
    RefDataKind (..),
    listRefData,
    loadRefData,
    unloadRefData,
    deleteRefData,
    uploadRefData,
    getFlowSynonymGroupsHandler,
    downloadRefDataHandler,

    -- * Helpers
    convertDbStatus,
    simpleAction,
    formatToText,
    checkUploadSize,
    uploadBodyCeiling,
) where

import Control.Exception (SomeException, try)
import Control.Monad (mfilter)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BSL
import Data.List (isPrefixOf)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Word (Word64)
import Servant (Header, Headers, ServerError, addHeader, err400, err404, err500, errBody, throwError)
import qualified System.Directory
import System.FilePath ((</>))

-- Flow synonyms

-- Compartment mappings

-- Unit definitions

import API.Types (
    ActivateResponse (..),
    BinaryContent (..),
    DatabaseListResponse (..),
    DatabaseStatusAPI (..),
    DeleteClassFilter (..),
    DeleteSelectionRequest (..),
    DeleteSelectionResponse (..),
    ExportRequest (..),
    ExportResponse (..),
    LoadDatabaseResponse (..),
    RefDataListResponse (..),
    RefDataStatusAPI (..),
    RelinkRequest (..),
    RelinkResponse (..),
    SynonymGroupsResponse (..),
    UploadRequest (..),
    UploadResponse (..),
 )
import App.Env (AppEnv (..), AppM)
import Config (DatabaseConfig (..), HostingConfig (..), MethodConfig (..), RefDataConfig (..))
import Control.Concurrent.STM (readTVarIO)
import Control.Monad.Reader (asks)
import Data.Aeson (Value, toJSON)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import Database.Edit (copyDatabase, deleteActivitiesInDB)
import Database.Export (exportWarnings, parseExportFormat, serializeDatabase)
import Database.Manager (
    DatabaseLoadStatus (..),
    DatabaseManager (..),
    DatabaseSetupInfo (..),
    DatabaseStatus (..),
    LoadedDatabase (..),
    RefDataStatus (..),
    RelinkResult (..),
    SetupError (..),
    addCompartmentMappings,
    addDatabase,
    addDependencyToStaged,
    addFlowSynonyms,
    addMethodCollection,
    addUnitDefs,
    finalizeDatabase,
    getDatabase,
    getDatabaseSetupInfo,
    getFlowSynonymGroups,
    listCompartmentMappings,
    listDatabases,
    listFlowSynonyms,
    listUnitDefs,
    loadCompartmentMappings,
    loadDatabase,
    loadFlowSynonyms,
    loadUnitDefs,
    relinkDatabase,
    relinkDatabaseWithMapping,
    removeCompartmentMappings,
    removeDatabase,
    removeDependencyFromStaged,
    removeFlowSynonyms,
    removeMethodCollection,
    removeUnitDefs,
    setDataPath,
    unloadCompartmentMappings,
    unloadDatabase,
    unloadFlowSynonyms,
    unloadUnitDefs,
 )
import Database.RelinkMapping (buildAliasMap, parseAliasCSV, rejectEmpty)
import Database.Upload (
    DatabaseFormat (..),
    UploadData (..),
    UploadResult (..),
    findMethodDirectory,
    handleUpload,
 )
import qualified Database.UploadedDatabase as UploadedDB
import Types (Database (..), GeographyPolicy (..), unresolvedCount)

-- | List all databases
getDatabases :: AppM DatabaseListResponse
getDatabases = do
    dbManager <- asks aeDbManager
    dbStatuses <- liftIO $ listDatabases dbManager
    let statusList = map convertDbStatus dbStatuses
    return $ DatabaseListResponse statusList

-- | Load a database on demand
loadDatabaseHandler :: Text -> AppM LoadDatabaseResponse
loadDatabaseHandler dbName = do
    dbManager <- asks aeDbManager
    eitherResult <- liftIO $ try $ loadDatabase dbManager dbName
    case eitherResult of
        Left (ex :: SomeException) ->
            return $ LoadFailed ("Server exception: " <> T.pack (show ex))
        Right (Left err) -> return $ LoadFailed err
        Right (Right (loadedDb, depResults)) -> do
            let status = makeStatusFromLoadedDb loadedDb
            return $ LoadSucceeded status depResults

-- | Unload a database from memory
unloadDatabaseHandler :: Text -> AppM ActivateResponse
unloadDatabaseHandler dbName = do
    dbManager <- asks aeDbManager
    simpleAction (unloadDatabase dbManager dbName) ("Unloaded database: " <> dbName)

{- | Re-run cross-DB linking for a loaded database. An empty @{}@ body
re-resolves links within the existing dependency pin (plain relink) — letting
the user recover from loads that happened in a suboptimal order without
reloading. A body carrying both @depDb@ and @mappingCsv@ switches to mapping
mode: relink against that one dependency using an inline name→name
supplier-alias CSV, so an Ecoinvent-named background (e.g. Agribalyse) resolves
against a differently-named dependency (e.g. BAFU). A loaded-but-undeclared
dependency is auto-pinned in-memory rather than rejected. Supplying exactly one
of the two is a client error. Parse/validation failures surface as 4xx rather
than a silent no-op; the only 404 from this path is an unloaded database or
dependency.
-}
relinkDatabaseHandler :: Text -> RelinkRequest -> AppM RelinkResponse
relinkDatabaseHandler dbName req = do
    dbManager <- asks aeDbManager
    case (rmrDepDb req, rmrMappingCsv req) of
        (Nothing, Nothing) ->
            runRelink (relinkDatabase dbManager dbName)
        (Just depDb, Just csv) ->
            case parseAliasCSV (BSL.fromStrict (T.encodeUtf8 csv)) >>= buildAliasMap >>= rejectEmpty of
                Left err -> throwError err400{errBody = BSL.fromStrict $ T.encodeUtf8 err}
                Right aliases -> runRelink (relinkDatabaseWithMapping dbManager dbName depDb aliases)
        (Just _, Nothing) -> incomplete
        (Nothing, Just _) -> incomplete
  where
    incomplete =
        throwError err400{errBody = "relink: depDb and mappingCsv must be supplied together"}
    runRelink :: IO (Either Text RelinkResult) -> AppM RelinkResponse
    runRelink act = do
        res <- liftIO act
        case res of
            Left err -> throwError err404{errBody = BSL.fromStrict $ T.encodeUtf8 err}
            Right r ->
                return
                    RelinkResponse
                        { rrDbName = rresDbName r
                        , rrUnresolvedBefore = rresUnresolvedBefore r
                        , rrUnresolvedAfter = rresUnresolvedAfter r
                        , rrCrossDBLinks = rresCrossDBLinks r
                        , rrDependsOn = rresDepsLoaded r
                        }

{- | Copy a loaded database under a new name. The copy is an independent
in-memory database registered under @newName@; the source is untouched.
-}
copyDatabaseHandler :: Text -> Text -> AppM ActivateResponse
copyDatabaseHandler dbName newName = do
    dbManager <- asks aeDbManager
    simpleAction (copyDatabase dbManager dbName newName) ("Copied database: " <> dbName <> " -> " <> newName)

-- | Delete an uploaded database (move to trash)
deleteDatabaseHandler :: Text -> AppM ActivateResponse
deleteDatabaseHandler dbName = do
    dbManager <- asks aeDbManager
    simpleAction (removeDatabase dbManager dbName) ("Deleted database: " <> dbName)

{- | Delete the whole filtered set of activities from a loaded database, in
place. The filter selects the full matching set (pagination ignored); the
keep/extra lists adjust the set individually. Rebuilds matrices and unlinks
surviving references; returns the count removed.
-}
deleteActivitiesHandler :: Text -> DeleteSelectionRequest -> AppM DeleteSelectionResponse
deleteActivitiesHandler dbName req = do
    dbManager <- asks aeDbManager
    let classFilters = [(dcfSystem f, dcfValue f, dcfExact f) | f <- dsqClassifications req]
        -- A present-but-blank filter (e.g. JSON "name":"") is no filter at all.
        -- Collapse it to Nothing so name candidates fall back to "all activities"
        -- instead of a BM25/full-scan path that yields zero (index present) or all
        -- (index absent) — an index-dependent ALL-vs-NONE divergence.
        nonBlank = mfilter (not . T.null . T.strip)
    result <-
        liftIO $
            deleteActivitiesInDB
                dbManager
                dbName
                (nonBlank (dsqName req))
                (nonBlank (dsqLocation req))
                (nonBlank (dsqProduct req))
                classFilters
                (fromMaybe False (dsqExact req))
                (dsqKeep req)
                (dsqExtra req)
    case result of
        -- A failed delete is a client error (bad filter, unknown DB, loaded
        -- dependents). Surface it as 4xx so a raw HTTP client can't read the
        -- 200 envelope as success.
        Left err -> throwError err400{errBody = BSL.fromStrict $ T.encodeUtf8 err}
        Right deleted ->
            return $
                DeleteSelectionResponse
                    True
                    ("Deleted " <> T.pack (show deleted) <> " activities from " <> dbName)
                    deleted

{- | Export a loaded database, returning the serialized bytes base64-encoded.
EcoSpold 2 / ILCD multi-file trees are zipped; single-file formats carry their
bytes directly. Mirrors the upload endpoint's base64 convention. Failures surface
as HTTP errors — 400 for an unknown format or data the target format cannot
represent, 404 for a database that is not loaded — never a 200 with a failure flag.
-}
exportDatabaseHandler :: Text -> ExportRequest -> AppM ExportResponse
exportDatabaseHandler dbName req = do
    dbManager <- asks aeDbManager
    fmt <- either (httpErr err400) pure (parseExportFormat (exrFormat req))
    mLoaded <- liftIO (getDatabase dbManager dbName)
    ld <- maybe (httpErr err404 ("Database not loaded: " <> dbName)) pure mLoaded
    bytes <- either (httpErr err400) pure (serializeDatabase fmt (ldDatabase ld))
    pure (ExportResponse (T.decodeUtf8 (B64.encode (BSL.toStrict bytes))) (exportWarnings fmt (ldDatabase ld)))
  where
    httpErr :: ServerError -> Text -> AppM a
    httpErr status msg = throwError status{errBody = BSL.fromStrict (T.encodeUtf8 msg)}

{- | Enforce the hosting upload-size policy on a decoded payload.
Local/CLI mode (no hosting config) is unlimited. A configured limit of 0
disables uploads; a negative limit is unlimited; a positive limit caps the
size in megabytes. Returns the failure message to surface, or () to proceed.
-}
checkUploadSize :: Maybe HostingConfig -> Int -> Either Text ()
checkUploadSize Nothing _ = Right ()
checkUploadSize (Just hc) sizeBytes =
    case hcMaxUploadMb hc of
        0 -> Left "Uploads are disabled on this plan."
        limitMb
            | limitMb < 0 -> Right ()
            | sizeBytes > limitMb * 1024 * 1024 ->
                Left $
                    "File too large ("
                        <> T.pack (show (sizeBytes `div` (1024 * 1024)))
                        <> " MB). The upload limit on this plan is "
                        <> T.pack (show limitMb)
                        <> " MB."
            | otherwise -> Right ()

{- | The WAI-level request-body ceiling (in bytes) for a request path, or
'Nothing' for no limit. This is the outer, pre-buffering backstop for
'checkUploadSize': only the database and method upload routes are bounded, and
only when the hosting config sets a positive cap. base64 inflates the payload by
~4/3 and the JSON envelope adds a little more, so we admit 2x the policy limit at
the HTTP layer — files between the real limit and that ceiling still reach the
handler, which returns the precise 'checkUploadSize' error. Unlimited (-1),
disabled (0), and local/CLI (no config) are left unbounded here: neither
unlimited nor disabled is a size bound, and the handler still rejects disabled
uploads.
-}
uploadBodyCeiling :: Maybe HostingConfig -> [Text] -> Maybe Word64
uploadBodyCeiling hostingConfig path
    | not (isPolicyUploadPath path) = Nothing
    | otherwise = case hostingConfig of
        Nothing -> Nothing
        Just hc
            | hcMaxUploadMb hc > 0 -> Just (fromIntegral (hcMaxUploadMb hc) * 2 * 1024 * 1024)
            | otherwise -> Nothing

{- | The upload routes governed by the size policy. These mirror the @db/upload@
and @method-collections/upload@ endpoints in "API.Routes"; the reference-data CSV
upload routes are intentionally excluded (small files, outside the policy).
-}
isPolicyUploadPath :: [Text] -> Bool
isPolicyUploadPath path =
    path
        `elem` [ ["api", "v1", "db", "upload"]
               , ["api", "v1", "method-collections", "upload"]
               ]

{- | Decode the base64 upload payload and enforce the hosting size policy,
then hand the raw bytes to the continuation. Shared by the database and
method upload handlers so both gate on one rule.
-}
withUploadBytes :: UploadRequest -> (BS.ByteString -> AppM UploadResponse) -> AppM UploadResponse
withUploadBytes req k =
    case B64.decode (T.encodeUtf8 (urFileData req)) of
        Left err -> return $ UploadResponse False ("Invalid base64 data: " <> T.pack err) Nothing Nothing
        Right zipBytes -> do
            hostingConfig <- asks aeHostingConfig
            case checkUploadSize hostingConfig (BS.length zipBytes) of
                Left rejection -> return $ UploadResponse False rejection Nothing Nothing
                Right () -> k zipBytes

-- | Upload a new database
uploadDatabaseHandler :: UploadRequest -> AppM UploadResponse
uploadDatabaseHandler req =
    withUploadBytes req $ \zipBytes -> do
        dbManager <- asks aeDbManager
        let uploadData =
                UploadData
                    { udName = urName req
                    , udDescription = urDescription req
                    , udZipData = BSL.fromStrict zipBytes
                    }
        -- Handle the upload (extract, detect format)
        uploadsDir <- liftIO UploadedDB.getDatabaseUploadsDir
        result <- liftIO $ handleUpload uploadsDir uploadData (\_ -> return ())

        case result of
            Left err ->
                return $ UploadResponse False err Nothing Nothing
            Right uploadResult -> do
                let uploadDir = uploadsDir </> T.unpack (urSlug uploadResult)

                -- Create meta.toml for self-describing upload
                let meta =
                        UploadedDB.UploadMeta
                            { UploadedDB.umVersion = 1
                            , UploadedDB.umDisplayName = urName req
                            , UploadedDB.umDescription = urDescription req
                            , UploadedDB.umFormat = urFormat uploadResult -- Types are now unified
                            , UploadedDB.umDataPath = makeRelative uploadDir (urPath uploadResult)
                            }
                liftIO $ UploadedDB.writeUploadMeta uploadDir meta

                -- Create database config for in-memory manager
                let dbConfig =
                        DatabaseConfig
                            { dcName = urSlug uploadResult
                            , dcDisplayName = urName req
                            , dcPath = urPath uploadResult
                            , dcDescription = urDescription req
                            , dcLoad = False -- Don't auto-load
                            , dcDefault = False
                            , dcDepends = []
                            , dcLocationAliases = M.empty
                            , dcFormat = Just (urFormat uploadResult)
                            , dcIsUploaded = True -- Freshly uploaded database
                            , dcDeletable = True
                            , dcGeographyPolicy = GeoGlobal
                            }

                -- Add to manager
                liftIO $ addDatabase dbManager dbConfig

                return $
                    UploadResponse
                        True
                        "Database uploaded successfully"
                        (Just $ urSlug uploadResult)
                        (Just $ formatToText $ urFormat uploadResult)

-- | Convert DatabaseManager.DatabaseStatus to API.DatabaseStatusAPI
convertDbStatus :: DatabaseStatus -> DatabaseStatusAPI
convertDbStatus ds =
    DatabaseStatusAPI
        { dsaName = dsName ds
        , dsaDisplayName = dsDisplayName ds
        , dsaDescription = dsDescription ds
        , dsaLoadAtStartup = dsLoadAtStartup ds
        , dsaStatus = statusToText (dsStatus ds)
        , dsaIsUploaded = dsIsUploaded ds
        , dsaPath = dsPath ds
        , dsaFormat = formatDisplayText <$> dsFormat ds
        , dsaActivityCount = dsActivityCount ds
        , dsaDependsOn = dsDependsOn ds
        }
  where
    statusToText Unloaded = "unloaded"
    statusToText PartiallyLinked = "partially_linked"
    statusToText Loaded = "loaded"

-- | Create DatabaseStatusAPI from a loaded database (derives status from linking stats)
makeStatusFromLoadedDb :: LoadedDatabase -> DatabaseStatusAPI
makeStatusFromLoadedDb loaded =
    let config = ldConfig loaded
        db = ldDatabase loaded
        status =
            if unresolvedCount (dbLinkingStats db) > 0
                then "partially_linked"
                else "loaded"
     in DatabaseStatusAPI
            { dsaName = dcName config
            , dsaDisplayName = dcDisplayName config
            , dsaDescription = dcDescription config
            , dsaLoadAtStartup = dcLoad config
            , dsaStatus = status
            , dsaIsUploaded = dcIsUploaded config
            , dsaPath = T.pack (dcPath config)
            , dsaFormat = formatDisplayText <$> dcFormat config
            , dsaActivityCount = V.length (dbActivities db)
            , dsaDependsOn = dcDepends config
            }

-- uploadFormatToMeta removed - types are now unified (UploadedDB re-exports from Upload)

-- | Make a path relative to a base directory
makeRelative :: FilePath -> FilePath -> FilePath
makeRelative base path
    | base `isPrefixOf` path = drop (length base + 1) path -- +1 for separator
    | otherwise = path

-- | Convert DatabaseFormat to display text (uses ToJSON instance: "EcoSpold 2", etc.)
formatDisplayText :: DatabaseFormat -> Text
formatDisplayText fmt = case toJSON fmt of
    A.String t -> t
    _ -> ""

-- | Convert DatabaseFormat to API slug text
formatToText :: DatabaseFormat -> Text
formatToText SimaProCSV = "simapro-csv"
formatToText EcoSpold1 = "ecospold1"
formatToText EcoSpold2 = "ecospold2"
formatToText ILCDProcess = "ilcd"
formatToText OpenLcaJsonLd = "openlca-jsonld"
formatToText BrightwayExcel = "brightway-excel"
formatToText UnknownFormat = "unknown"

--------------------------------------------------------------------------------
-- Setup Page Handlers
--------------------------------------------------------------------------------

{- | Get database setup info
Returns completeness, missing suppliers, and dependency suggestions
-}
getDatabaseSetupHandler :: Text -> AppM DatabaseSetupInfo
getDatabaseSetupHandler dbName = do
    dbManager <- asks aeDbManager
    eitherResult <- liftIO $ try $ getDatabaseSetupInfo dbManager dbName
    case eitherResult of
        Left (ex :: SomeException) ->
            throwError $ err500{errBody = BSL.fromStrict $ T.encodeUtf8 $ "Setup failed: " <> T.pack (show ex)}
        Right (Left (SetupNotFound msg)) -> throwError $ err404{errBody = BSL.fromStrict $ T.encodeUtf8 msg}
        Right (Left (SetupFailed msg)) -> throwError $ err500{errBody = BSL.fromStrict $ T.encodeUtf8 msg}
        Right (Right setupInfo) -> return setupInfo

{- | Add a dependency to a staged database
Runs cross-DB linking and returns updated setup info
-}
addDependencyHandler :: Text -> Text -> AppM DatabaseSetupInfo
addDependencyHandler dbName depName = do
    dbManager <- asks aeDbManager
    ioEither400 (addDependencyToStaged dbManager dbName depName)

{- | Remove a dependency from a staged database
Re-runs cross-DB linking and returns updated setup info
-}
removeDependencyHandler :: Text -> Text -> AppM DatabaseSetupInfo
removeDependencyHandler dbName depName = do
    dbManager <- asks aeDbManager
    ioEither400 (removeDependencyFromStaged dbManager dbName depName)

-- | Change the data path for an uploaded (staged) database
setDataPathHandler :: Text -> Value -> AppM DatabaseSetupInfo
setDataPathHandler dbName body = do
    dbManager <- asks aeDbManager
    -- Extract "path" from JSON body
    let mPath = case body of
            A.Object obj -> case KM.lookup "path" obj of
                Just (A.String p) -> Just p
                _ -> Nothing
            _ -> Nothing
    case mPath of
        Nothing -> throwError $ err400{errBody = "Missing \"path\" field in request body"}
        Just newPath -> ioEither400 (setDataPath dbManager dbName newPath)

{- | Finalize a staged database
Builds matrices and makes it ready for queries
-}
finalizeDatabaseHandler :: Text -> AppM ActivateResponse
finalizeDatabaseHandler dbName = do
    dbManager <- asks aeDbManager
    eitherResult <- liftIO $ try $ finalizeDatabase dbManager dbName
    case eitherResult of
        Left (ex :: SomeException) ->
            return $ ActivateResponse False ("Server exception: " <> T.pack (show ex)) Nothing
        Right (Left err) -> return $ ActivateResponse False err Nothing
        Right (Right loaded) -> do
            let status = makeStatusFromLoadedDb loaded
            return $ ActivateResponse True ("Finalized database: " <> dcDisplayName (ldConfig loaded)) (Just status)

{- | Upload a new method collection
Same flow as database upload but creates MethodConfig entry
-}
uploadMethodHandler :: UploadRequest -> AppM UploadResponse
uploadMethodHandler req =
    withUploadBytes req $ \zipBytes -> do
        dbManager <- asks aeDbManager
        let uploadData =
                UploadData
                    { udName = urName req
                    , udDescription = urDescription req
                    , udZipData = BSL.fromStrict zipBytes
                    }
        uploadsDir <- liftIO UploadedDB.getMethodUploadsDir
        result <- liftIO $ handleUpload uploadsDir uploadData (\_ -> return ())
        case result of
            Left err ->
                return $ UploadResponse False err Nothing Nothing
            Right uploadResult -> do
                let uploadDir = uploadsDir </> T.unpack (urSlug uploadResult)

                -- Find the actual method XML directory (e.g. ILCD/lciamethods/)
                methodDir <- liftIO $ findMethodDirectory uploadDir

                -- Create meta.toml (store path relative to upload dir)
                let meta =
                        UploadedDB.UploadMeta
                            { UploadedDB.umVersion = 1
                            , UploadedDB.umDisplayName = urName req
                            , UploadedDB.umDescription = urDescription req
                            , UploadedDB.umFormat = urFormat uploadResult
                            , UploadedDB.umDataPath = makeRelative uploadDir methodDir
                            }
                liftIO $ UploadedDB.writeUploadMeta uploadDir meta

                -- Create MethodConfig and add to manager
                let mc =
                        MethodConfig
                            { mcName = urName req
                            , mcPath = methodDir
                            , mcActive = False
                            , mcIsUploaded = True
                            , mcDescription = urDescription req
                            , mcFormat = Just $ formatToText $ urFormat uploadResult
                            , mcScoringSets = []
                            }
                liftIO $ addMethodCollection dbManager mc

                return $
                    UploadResponse
                        True
                        "Method uploaded successfully"
                        (Just $ urSlug uploadResult)
                        (Just $ formatToText $ urFormat uploadResult)

-- | Delete an uploaded method collection
deleteMethodHandler :: Text -> AppM ActivateResponse
deleteMethodHandler name = do
    dbManager <- asks aeDbManager
    simpleAction (removeMethodCollection dbManager name) ("Deleted method: " <> name)

-- | Common pattern: run an IO action that returns Either Text (), map to ActivateResponse
simpleAction :: IO (Either Text ()) -> Text -> AppM ActivateResponse
simpleAction action successMsg = do
    result <- liftIO action
    return $ case result of
        Left err -> ActivateResponse False err Nothing
        Right () -> ActivateResponse True successMsg Nothing

{- | @ioEither400 m@ runs an IO action that returns @Either Text a@; on
@Left@ throws a 400 with the message body, on @Right@ propagates. Used
to compress the @do result <- liftIO ...; case result of Left … Right …@
ladder that recurs across every handler returning a typed payload.
-}
ioEither400 :: IO (Either Text a) -> AppM a
ioEither400 action = do
    result <- liftIO action
    case result of
        Left err -> throwError err400{errBody = BSL.fromStrict $ T.encodeUtf8 err}
        Right v -> return v

--------------------------------------------------------------------------------
-- Reference Data Handlers (flow synonyms, compartment mappings, units)
--------------------------------------------------------------------------------

-- | Which kind of reference data we're operating on
data RefDataKind = FlowSynonyms | CompartmentMappings | UnitDefs

-- | Dispatch to the right Manager functions based on kind
rdOps ::
    RefDataKind ->
    ( DatabaseManager -> IO [RefDataStatus]
    , DatabaseManager -> Text -> IO (Either Text ())
    , DatabaseManager -> Text -> IO (Either Text ())
    , DatabaseManager -> RefDataConfig -> IO ()
    , DatabaseManager -> Text -> IO (Either Text ())
    , Text -- upload subdir
    )
rdOps FlowSynonyms =
    (listFlowSynonyms, loadFlowSynonyms, unloadFlowSynonyms, addFlowSynonyms, removeFlowSynonyms, "flow-synonyms")
rdOps CompartmentMappings =
    (listCompartmentMappings, loadCompartmentMappings, unloadCompartmentMappings, addCompartmentMappings, removeCompartmentMappings, "compartment-mappings")
rdOps UnitDefs =
    (listUnitDefs, loadUnitDefs, unloadUnitDefs, addUnitDefs, removeUnitDefs, "units")

convertRefDataStatus :: RefDataStatus -> RefDataStatusAPI
convertRefDataStatus s =
    RefDataStatusAPI
        { rdaName = rdsName s
        , rdaDisplayName = rdsDisplayName s
        , rdaDescription = rdsDescription s
        , rdaStatus = case rdsStatus s of Loaded -> "loaded"; _ -> "unloaded"
        , rdaIsUploaded = rdsIsUploaded s
        , rdaIsAuto = rdsIsAuto s
        , rdaEntryCount = rdsEntryCount s
        }

listRefData :: RefDataKind -> AppM RefDataListResponse
listRefData kind = do
    dbManager <- asks aeDbManager
    let (listFn, _, _, _, _, _) = rdOps kind
    statuses <- liftIO $ listFn dbManager
    return $ RefDataListResponse (map convertRefDataStatus statuses)

loadRefData :: RefDataKind -> Text -> AppM ActivateResponse
loadRefData kind name = do
    dbManager <- asks aeDbManager
    let (_, loadFn, _, _, _, _) = rdOps kind
    simpleAction (loadFn dbManager name) ("Loaded: " <> name)

unloadRefData :: RefDataKind -> Text -> AppM ActivateResponse
unloadRefData kind name = do
    dbManager <- asks aeDbManager
    let (_, _, unloadFn, _, _, _) = rdOps kind
    simpleAction (unloadFn dbManager name) ("Unloaded: " <> name)

deleteRefData :: RefDataKind -> Text -> AppM ActivateResponse
deleteRefData kind name = do
    dbManager <- asks aeDbManager
    let (_, _, _, _, removeFn, _) = rdOps kind
    simpleAction (removeFn dbManager name) ("Deleted: " <> name)

uploadRefData :: RefDataKind -> UploadRequest -> AppM UploadResponse
uploadRefData kind req = do
    dbManager <- asks aeDbManager
    let (_, _, _, addFn, _, subdir) = rdOps kind
    let csvDataResult = B64.decode $ T.encodeUtf8 $ urFileData req
    case csvDataResult of
        Left err -> return $ UploadResponse False ("Invalid base64 data: " <> T.pack err) Nothing Nothing
        Right csvBytes -> do
            baseDir <- liftIO UploadedDB.getDataDir
            let slug = T.toLower $ T.intercalate "-" $ T.words $ urName req
                uploadDir = baseDir </> "uploads" </> T.unpack subdir </> T.unpack slug
                csvPath = uploadDir </> "data.csv"
            liftIO $ do
                System.Directory.createDirectoryIfMissing True uploadDir
                BSL.writeFile csvPath (BSL.fromStrict csvBytes)
                let metaContent =
                        T.intercalate
                            "\n"
                            [ "[meta]"
                            , "version = 1"
                            , "displayName = " <> quote (urName req)
                            , maybe "" (\d -> "description = " <> quote d) (urDescription req)
                            , ""
                            ]
                T.writeFile (uploadDir </> "meta.toml") metaContent
            let rd =
                    RefDataConfig
                        { rdName = urName req
                        , rdPath = csvPath
                        , rdActive = False
                        , rdIsUploaded = True
                        , rdIsAuto = False
                        , rdDescription = urDescription req
                        }
            liftIO $ addFn dbManager rd
            return $ UploadResponse True "Uploaded successfully" (Just slug) Nothing
  where
    quote t = "\"" <> T.replace "\"" "\\\"" t <> "\""

getFlowSynonymGroupsHandler :: Text -> AppM SynonymGroupsResponse
getFlowSynonymGroupsHandler name = do
    dbManager <- asks aeDbManager
    result <- liftIO $ getFlowSynonymGroups dbManager name
    case result of
        Left err -> throwError $ err404{errBody = BSL.fromStrict $ T.encodeUtf8 err}
        Right groups -> return $ SynonymGroupsResponse groups

downloadRefDataHandler :: RefDataKind -> Text -> AppM (Headers '[Header "Content-Disposition" Text] BinaryContent)
downloadRefDataHandler kind name = do
    dbManager <- asks aeDbManager
    let tvar = case kind of
            FlowSynonyms -> dmAvailableFlowSyns dbManager
            CompartmentMappings -> dmAvailableCompMaps dbManager
            UnitDefs -> dmAvailableUnitDefs dbManager
    available <- liftIO $ readTVarIO tvar
    case M.lookup name available of
        Nothing -> throwError $ err404{errBody = "Not found"}
        Just rd -> do
            let csvPath = rdPath rd
            exists <- liftIO $ System.Directory.doesFileExist csvPath
            if not exists
                then throwError $ err404{errBody = "CSV file not found"}
                else do
                    content <- liftIO $ BSL.readFile csvPath
                    let disposition = "attachment; filename=\"" <> name <> ".csv\""
                    return $ addHeader disposition (BinaryContent content)
