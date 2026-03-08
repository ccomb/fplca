module Pages.CompartmentMappings exposing (Model, Msg, page)

import Effect exposing (Effect)
import Html exposing (code, div, p, text)
import Html.Attributes exposing (style)
import Pages.RefData as RefData
import Shared
import Spa.Page
import View exposing (View)


config : RefData.Config
config =
    { title = "Compartment mappings"
    , subtitle = "Normalize compartment names across databases and methods"
    , apiPath = "compartment-mappings"
    , countLabel = "Mappings"
    , formatHelp =
        [ div [ style "margin-top" "0.75rem", style "font-size" "0.9em" ]
            [ p [] [ text "Expected CSV format (6 columns):" ]
            , code [ style "display" "block", style "margin-top" "0.25rem", style "white-space" "pre" ]
                [ text "source_medium,source_sub,source_qualifier,target_medium,target_sub,target_qualifier\nair,high population density,,air,urban air close to ground,\nwater,river,,water,surface water," ]
            ]
        ]
    , detailRoute = Nothing
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
