module Views.ActivityRow exposing (ActivityRowData, viewActivityRow)

import Html exposing (Html, span, td, text, tr)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Utils.Format as Format


{-| Common data structure for rendering an activity row.
Used by both ActivitiesView (search results) and DetailsView (upstream activities).
-}
type alias ActivityRowData msg =
    { id : Maybe String -- Nothing if not clickable
    , name : String
    , amount : Float -- Production amount (search) or consumed quantity (upstream)
    , unit : String -- Unit for the amount
    , product : String
    , location : String
    , onNavigate : String -> msg
    }


{-| Render an activity row with unified column order.
Columns: Activity Name | Amount | Unit | Product | Location
-}
viewActivityRow : ActivityRowData msg -> Html msg
viewActivityRow data =
    tr []
        [ td []
            [ case data.id of
                Just activityId ->
                    span
                        [ class "has-text-link"
                        , style "cursor" "pointer"
                        , onClick (data.onNavigate activityId)
                        ]
                        [ text data.name ]

                Nothing ->
                    text data.name
            ]
        , td [ class "has-text-right" ] [ text (Format.formatScientific data.amount) ]
        , td [] [ text data.unit ]
        , td [] [ text data.product ]
        , td [] [ text data.location ]
        ]
