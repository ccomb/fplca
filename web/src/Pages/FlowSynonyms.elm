module Pages.FlowSynonyms exposing (Model, Msg, page)

import Effect exposing (Effect)
import Html exposing (code, div, p, text)
import Html.Attributes exposing (style)
import Pages.RefData as RefData
import Route
import Shared
import Spa.Page
import View exposing (View)


config : RefData.Config
config =
    { title = "Flow synonyms"
    , subtitle = "Manage flow name synonym mappings for cross-nomenclature matching"
    , apiPath = "flow-synonyms"
    , countLabel = "Pairs"
    , formatHelp =
        [ div [ style "margin-top" "0.75rem", style "font-size" "0.9em" ]
            [ p [] [ text "Expected CSV format (2 columns):" ]
            , code [ style "display" "block", style "margin-top" "0.25rem", style "white-space" "pre" ]
                [ text "name1,name2\ncarbon dioxide fossil,carbon dioxide (fossil)\nmethane fossil,methane (fossil)" ]
            ]
        ]
    , detailRoute = Just Route.FlowSynonymDetailRoute
    }


type alias Model =
    RefData.Model


type alias Msg =
    RefData.Msg


page : Shared.Model -> Spa.Page.Page () Shared.Msg (View Msg) Model Msg
page _ =
    Spa.Page.element
        { init = \_ -> RefData.init config
        , update = \msg model -> RefData.update config msg model
        , view = \model -> RefData.view config model
        , subscriptions = \_ -> Sub.none
        }
