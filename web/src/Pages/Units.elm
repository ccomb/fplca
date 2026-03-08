module Pages.Units exposing (Model, Msg, page)

import Effect exposing (Effect)
import Html exposing (code, div, p, text)
import Html.Attributes exposing (style)
import Pages.RefData as RefData
import Shared
import Spa.Page
import View exposing (View)


config : RefData.Config
config =
    { title = "Units"
    , subtitle = "Manage unit definitions and conversion factors"
    , apiPath = "units"
    , countLabel = "Units"
    , formatHelp =
        [ div [ style "margin-top" "0.75rem", style "font-size" "0.9em" ]
            [ p [] [ text "Expected CSV format (3 columns):" ]
            , code [ style "display" "block", style "margin-top" "0.25rem", style "white-space" "pre" ]
                [ text "name,dimension,factor\nkg,mass,1.0\ng,mass,0.001\ntkm,mass*length,1e6\nkWh,energy,3.6e6" ]
            , p [ style "margin-top" "0.25rem" ]
                [ text "Dimensions: mass, length, time, energy, area, volume, count, currency. Combine with * and /." ]
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
