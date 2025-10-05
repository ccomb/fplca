module Models.Page exposing (Page(..), Route(..))

-- Route definitions for URL parsing
type Route
    = ActivitiesRoute
    | ActivityRoute String  -- ProcessId
    | ActivityTreeRoute String  -- ProcessId
    | NotFoundRoute

-- Page definitions for the SPA
type Page
    = ActivitiesPage
    | GraphPage