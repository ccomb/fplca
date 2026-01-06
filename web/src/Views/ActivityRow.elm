module Views.ActivityRow exposing (ActivityRowData, viewActivityRow)

import Html exposing (Html, a, td, text, tr)
import Html.Attributes exposing (class, href, style)
import Html.Events exposing (onClick)
import Utils.Format as Format


{-| Common data structure for rendering an activity row.
Used by both ActivitiesView (search results) and DetailsView (upstream activities).
-}
type alias ActivityRowData msg =
    { id : Maybe String -- Nothing if not clickable
    , name : String
    , product : String
    , location : String
    , quantity : Maybe ( Float, String ) -- (amount, unit) for upstream views
    , onNavigate : String -> msg
    }


{-| Render an activity row with optional navigation link and quantity columns.
-}
viewActivityRow : ActivityRowData msg -> Html msg
viewActivityRow data =
    tr []
        ([ td []
            [ case data.id of
                Just activityId ->
                    a
                        [ href "#"
                        , class "has-text-link"
                        , style "cursor" "pointer"
                        , onClick (data.onNavigate activityId)
                        ]
                        [ text data.name ]

                Nothing ->
                    text data.name
            ]
         , td [] [ text data.product ]
         , td [] [ text data.location ]
         ]
            ++ (case data.quantity of
                    Just ( amount, unit ) ->
                        [ td [ class "has-text-right" ] [ text (Format.formatScientific amount) ]
                        , td [] [ text unit ]
                        ]

                    Nothing ->
                        []
               )
        )
