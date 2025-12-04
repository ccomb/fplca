module Views.ActivitiesView exposing (viewActivitiesPage, Msg(..))

import Html exposing (Html, a, button, div, input, table, tbody, td, text, th, thead, tr, h2, p, span)
import Html.Attributes exposing (class, href, placeholder, style, value, type_, disabled)
import Html.Events exposing (onInput, onClick)
import Models.Activity exposing (ActivitySummary, SearchResults)


type Msg
    = UpdateSearchQuery String
    | SelectActivity String
    | LoadMore


viewActivitiesPage : String -> Maybe (SearchResults ActivitySummary) -> Bool -> Bool -> Maybe String -> Html Msg
viewActivitiesPage searchQuery searchResults searchLoading loadingMore error =
    div [ class "activities-page" ]
        [ div [ class "section" ]
            [ div [ class "container" ]
                [ h2 [ class "title is-3" ] [ text "Search Activities" ]
                , p [ class "subtitle" ] [ text "Find activities by name and view their environmental inventory" ]
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
                , viewSearchResults searchResults searchLoading loadingMore
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


viewSearchResults : Maybe (SearchResults ActivitySummary) -> Bool -> Bool -> Html Msg
viewSearchResults maybeResults isLoading loadingMore =
    case ( isLoading, maybeResults ) of
        ( True, Nothing ) ->
            div [ class "has-text-centered" ]
                [ div [ class "is-size-5 has-text-grey" ] [ text "Searching..." ]
                ]

        ( _, Nothing ) ->
            text ""

        ( _, Just results ) ->
            if List.isEmpty results.results then
                div [ class "has-text-centered" ]
                    [ div [ class "is-size-5 has-text-grey" ] [ text "No activities found" ]
                    ]
            else
                div []
                    [ div [ class "mb-4" ]
                        [ span [ class "tag is-info is-light" ]
                            [ text (String.fromInt (List.length results.results) ++ " / " ++ String.fromInt results.totalCount ++ " activities")
                            ]
                        ]
                    , div [ class "table-container" ]
                        [ table [ class "table is-striped is-hoverable is-fullwidth" ]
                            [ thead []
                                [ tr []
                                    [ th [] [ text "Activity Name" ]
                                    , th [] [ text "Location" ]
                                    ]
                                ]
                            , tbody []
                                (List.map viewActivityRow results.results)
                            ]
                        ]
                    , if results.hasMore then
                        div [ class "has-text-centered mt-4" ]
                            [ button
                                [ class (if loadingMore then "button is-primary is-loading" else "button is-primary")
                                , onClick LoadMore
                                , disabled loadingMore
                                ]
                                [ text "Load more" ]
                            ]
                      else
                        text ""
                    ]


viewActivityRow : ActivitySummary -> Html Msg
viewActivityRow activity =
    tr
        [ class "is-clickable"
        , style "cursor" "pointer"
        , onClick (SelectActivity activity.id)
        ]
        [ td []
            [ a [ href "#", class "has-text-link" ] [ text activity.name ]
            ]
        , td [] [ text activity.location ]
        ]