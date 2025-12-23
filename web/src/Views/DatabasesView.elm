module Views.DatabasesView exposing (Msg(..), viewDatabasesPage)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Models.Database exposing (DatabaseList, DatabaseStatus)


type Msg
    = ActivateDatabase String


viewDatabasesPage : Maybe DatabaseList -> Bool -> Maybe String -> Html Msg
viewDatabasesPage maybeDatabases loading error =
    div [ class "databases-page" ]
        [ nav [ class "navbar is-light", style "height" "52px" ]
            [ div [ class "navbar-brand" ]
                [ div [ class "navbar-item" ]
                    [ h1 [ class "title is-4" ] [ text "Databases" ]
                    ]
                ]
            ]
        , div [ class "container", style "padding" "1rem" ]
            [ case error of
                Just err ->
                    div [ class "notification is-danger" ]
                        [ text err ]

                Nothing ->
                    text ""
            , case ( loading, maybeDatabases ) of
                ( True, _ ) ->
                    div [ class "has-text-centered", style "padding" "2rem" ]
                        [ div [ class "is-size-4" ] [ text "Loading databases..." ]
                        , progress [ class "progress is-primary", attribute "max" "100" ] []
                        ]

                ( False, Just dbList ) ->
                    viewDatabasesList dbList

                ( False, Nothing ) ->
                    div [ class "notification is-warning" ]
                        [ text "No database information available" ]
            ]
        ]


viewDatabasesList : DatabaseList -> Html Msg
viewDatabasesList dbList =
    div []
        [ div [ class "columns is-multiline" ]
            (List.map (viewDatabaseCard dbList.current) dbList.databases)
        ]


viewDatabaseCard : Maybe String -> DatabaseStatus -> Html Msg
viewDatabaseCard currentDb db =
    let
        isLoaded =
            currentDb == Just db.name

        statusTag =
            if isLoaded then
                span [ class "tag is-success is-medium" ] [ text "LOADED" ]

            else if db.cached then
                span [ class "tag is-info is-medium" ] [ text "CACHED" ]

            else
                span [ class "tag is-warning is-medium" ] [ text "NOT CACHED" ]

        statusIcon =
            if isLoaded then
                i [ class "fas fa-check-circle has-text-success", style "font-size" "1.5rem" ] []

            else
                i [ class "far fa-circle has-text-grey-light", style "font-size" "1.5rem" ] []
    in
    div [ class "column is-half" ]
        [ div
            [ class "box"
            , classList [ ( "has-background-success-light", isLoaded ) ]
            , style "border-left"
                (if isLoaded then
                    "4px solid #48c78e"

                 else
                    "4px solid #dbdbdb"
                )
            ]
            [ div [ class "level" ]
                [ div [ class "level-left" ]
                    [ div [ class "level-item" ]
                        [ statusIcon ]
                    , div [ class "level-item" ]
                        [ div []
                            [ h2 [ class "title is-5", style "margin-bottom" "0.25rem" ]
                                [ text db.displayName ]
                            , case db.description of
                                Just desc ->
                                    p [ class "has-text-grey", style "font-size" "0.9rem" ]
                                        [ text desc ]

                                Nothing ->
                                    text ""
                            ]
                        ]
                    ]
                , div [ class "level-right" ]
                    [ div [ class "level-item" ]
                        [ statusTag ]
                    ]
                ]
            , div [ class "content", style "margin-top" "1rem" ]
                [ p [ class "is-size-7 has-text-grey" ]
                    [ strong [] [ text "Path: " ]
                    , text db.path
                    ]
                ]
            , if not isLoaded && db.active then
                div [ class "has-text-right" ]
                    [ button
                        [ class "button is-primary"
                        , onClick (ActivateDatabase db.name)
                        ]
                        [ span [ class "icon" ]
                            [ i [ class "fas fa-database" ] [] ]
                        , span [] [ text "Load Database" ]
                        ]
                    ]

              else
                text ""
            ]
        ]
