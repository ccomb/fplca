module Models.Page exposing (ExchangeTab(..), Page(..), Route(..))

-- Route definitions for URL parsing
-- All activity-related routes include a database name as first parameter


type Route
    = RootRoute -- "/" - will redirect to default database
    | ActivitiesRoute { db : String, name : Maybe String, limit : Maybe Int }
    | ActivityRoute String String -- db, ProcessId
    | ActivityDetailsRoute String String -- db, ProcessId (table view)
    | ActivityTreeRoute String String -- db, ProcessId (tree view)
    | ActivityInventoryRoute String String -- db, ProcessId
    | ActivityGraphRoute String String -- db, ProcessId
    | ActivityLCIARoute String String -- db, ProcessId (LCIA view)
    | DatabasesRoute -- Databases management page (global, no db prefix)
    | NotFoundRoute



-- Page definitions for the SPA


type Page
    = ActivitiesPage
    | DetailsPage
    | TreePage
    | InventoryPage
    | GraphPage
    | LCIAPage
    | DatabasesPage


-- Sub-tab selection for Table view exchanges


type ExchangeTab
    = UpstreamTab
    | EmissionsTab
    | ConsumptionsTab
    | ProductsTab
