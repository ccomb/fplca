module Models.Page exposing (Page(..), Route(..))

-- Route definitions for URL parsing
-- All activity-related routes include a database name as first parameter


type Route
    = RootRoute -- "/" - will redirect to default database
    | ActivitiesRoute { db : String, name : Maybe String, limit : Maybe Int }
    | ActivityRoute String String -- db, ProcessId
    | ActivityUpstreamRoute String String -- db, ProcessId (upstream activities)
    | ActivityEmissionsRoute String String -- db, ProcessId (direct emissions)
    | ActivityResourcesRoute String String -- db, ProcessId (natural resources)
    | ActivityProductsRoute String String -- db, ProcessId (outgoing products)
    | ActivityTreeRoute String String -- db, ProcessId (tree view)
    | ActivityInventoryRoute String String -- db, ProcessId
    | ActivityGraphRoute String String -- db, ProcessId
    | ActivityLCIARoute String String -- db, ProcessId (LCIA view)
    | DatabasesRoute -- Databases management page (global, no db prefix)
    | UploadRoute -- Database upload page (global, no db prefix)
    | DatabaseSetupRoute String -- Database setup page (configure dependencies)
    | NotFoundRoute



-- Page definitions for the SPA


type Page
    = ActivitiesPage
    | UpstreamPage
    | EmissionsPage
    | ResourcesPage
    | ProductsPage
    | TreePage
    | InventoryPage
    | GraphPage
    | LCIAPage
    | DatabasesPage
    | UploadPage
    | DatabaseSetupPage
