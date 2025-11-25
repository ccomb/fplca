module Models.Page exposing (Page(..), Route(..))

-- Route definitions for URL parsing


type Route
    = ActivitiesRoute { name : Maybe String, limit : Maybe Int }
    | ActivityRoute String -- ProcessId
    | ActivityDetailsRoute String -- ProcessId
    | ActivityTreeRoute String -- ProcessId
    | ActivityInventoryRoute String -- ProcessId
    | ActivityGraphRoute String -- ProcessId
    | NotFoundRoute



-- Page definitions for the SPA


type Page
    = ActivitiesPage
    | DetailsPage
    | TreePage
    | InventoryPage
    | GraphPage

