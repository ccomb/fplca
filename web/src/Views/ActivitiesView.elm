module Views.ActivitiesView exposing (viewActivitiesPage, Msg(..))

import Html exposing (Html, div, input, text, h2, p, span, button)
import Html.Attributes exposing (class, placeholder, value, type_, disabled)
import Html.Events exposing (onInput, onClick)
import Models.Activity exposing (ActivitySummary, SearchResults)


type Msg
    = UpdateSearchQuery String
    | SelectActivity String


viewActivitiesPage : String -> Maybe (SearchResults ActivitySummary) -> Bool -> Maybe String -> Html Msg
viewActivitiesPage searchQuery searchResults searchLoading error =
    div [ class "activities-page" ]
        [ div [ class "section" ]
            [ div [ class "container" ]
                [ h2 [ class "title is-3" ] [ text "Search Activities" ]
                , p [ class "subtitle" ] [ text "Find activities by name and explore their environmental impact trees" ]
                ]
            ]
        , div [ class "section" ]
            [ div [ class "container" ]
                [ searchInput searchQuery searchLoading
                , case error of
                    Just err ->
                        div [ class "notification is-danger" ]
                            [ text ("Error: " ++ err) ]

                    Nothing ->
                        text ""
                , viewSearchResults searchResults searchLoading
                ]
            ]
        ]


searchInput : String -> Bool -> Html Msg
searchInput query isLoading =
    div [ class "field" ]
        [ div [ if isLoading then class "control has-icons-left is-loading" else class "control has-icons-left" ]
            [ input
                [ class "input is-large"
                , type_ "text"
                , placeholder "Search activities by name..."
                , value query
                , onInput UpdateSearchQuery
                , disabled isLoading
                ]
                []
            , span [ class "icon is-left" ]
                [ Html.i [ class "fas fa-search" ] []
                ]
            ]
        ]


viewSearchResults : Maybe (SearchResults ActivitySummary) -> Bool -> Html Msg
viewSearchResults maybeResults isLoading =
    case ( isLoading, maybeResults ) of
        ( True, _ ) ->
            div [ class "has-text-centered" ]
                [ div [ class "is-size-5 has-text-grey" ] [ text "Searching..." ]
                ]

        ( False, Nothing ) ->
            div [ class "has-text-centered" ]
                [ div [ class "is-size-5 has-text-grey" ] [ text "Enter at least 2 characters to search" ]
                ]

        ( False, Just results ) ->
            if List.isEmpty results.results then
                div [ class "has-text-centered" ]
                    [ div [ class "is-size-5 has-text-grey" ] [ text "No activities found" ]
                    ]
            else
                div []
                    [ div [ class "mb-4" ]
                        [ span [ class "tag is-info is-light" ]
                            [ text (String.fromInt results.totalCount ++ " activities found")
                            ]
                        ]
                    , div [ class "columns is-multiline" ]
                        (List.map viewActivityCard results.results)
                    ]


viewActivityCard : ActivitySummary -> Html Msg
viewActivityCard activity =
    div [ class "column is-half" ]
        [ div [ class "card is-clickable" ]
            [ div [ class "card-content" ]
                [ div [ class "media" ]
                    [ div [ class "media-left" ]
                        [ span [ class "icon is-large" ]
                            [ Html.i [ class "fas fa-industry fa-2x has-text-primary" ] []
                            ]
                        ]
                    , div [ class "media-content" ]
                        [ p [ class "title is-5" ] [ text activity.name ]
                        , p [ class "subtitle is-6 has-text-grey" ] [ text activity.location ]
                        ]
                    ]
                , div [ class "buttons" ]
                    [ button
                        [ class "button is-primary is-fullwidth"
                        , onClick (SelectActivity activity.id)
                        ]
                        [ span [ class "icon" ] [ Html.i [ class "fas fa-project-diagram" ] [] ]
                        , span [] [ text "View Tree" ]
                        ]
                    ]
                ]
            ]
        ]