module Models.Page exposing (ExchangeTab(..), Page(..), Route(..))

-- Route definitions for URL parsing


type Route
    = ActivitiesRoute { name : Maybe String, limit : Maybe Int }
    | ActivityRoute String -- ProcessId
    | ActivityDetailsRoute String -- ProcessId (table view)
    | ActivityTreeRoute String -- ProcessId (tree view)
    | ActivityInventoryRoute String -- ProcessId
    | ActivityGraphRoute String -- ProcessId
    | ActivityLCIARoute String -- ProcessId (LCIA view)
    | DatabasesRoute -- Databases management page
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

