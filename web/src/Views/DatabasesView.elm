module Views.DatabasesView exposing (Msg(..), viewDatabasesPage)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Models.Database exposing (DatabaseList, DatabaseStatus)


type Msg
    = ActivateDatabase String


viewDatabasesPage : Maybe DatabaseList -> Bool -> Maybe String -> Html Msg
viewDatabasesPage maybeDatabases loading error =
    div [ class "databases-page", style "display" "flex", style "flex-direction" "column", style "height" "100%" ]
        [ div [ class "box", style "margin-bottom" "0", style "flex-shrink" "0" ]
            [ h2 [ class "title is-3" ] [ text "Databases" ]
            , p [ class "subtitle" ] [ text "Select a database to load and search activities" ]
            , case error of
                Just err ->
                    div [ class "notification is-danger" ]
                        [ text err ]

                Nothing ->
                    text ""
            ]
        , case ( loading, maybeDatabases ) of
            ( True, _ ) ->
                div [ class "has-text-centered", style "padding" "2rem" ]
                    [ div [ class "is-size-4" ] [ text "Loading databases..." ]
                    , progress [ class "progress is-primary", attribute "max" "100" ] []
                    ]

            ( False, Just dbList ) ->
                viewDatabasesList dbList

            ( False, Nothing ) ->
                div [ class "notification is-warning", style "margin" "1rem" ]
                    [ text "No database information available" ]
        ]


viewDatabasesList : DatabaseList -> Html Msg
viewDatabasesList dbList =
    let
        dbCount =
            List.length dbList.databases
    in
    div [ style "flex" "1", style "display" "flex", style "flex-direction" "column", style "min-height" "0" ]
        [ div [ style "flex-shrink" "0", style "padding" "0.5rem 0" ]
            [ span [ class "tag is-info is-light" ]
                [ text (String.fromInt dbCount ++ " databases") ]
            ]
        , div [ style "flex" "1", style "overflow-y" "auto", style "min-height" "0" ]
            [ table [ class "table is-striped is-hoverable is-fullwidth" ]
                [ thead [ style "position" "sticky", style "top" "0", style "background-color" "white", style "z-index" "10" ]
                    [ tr []
                        [ th [ style "background-color" "white", style "width" "50px" ] [ text "" ]
                        , th [ style "background-color" "white" ] [ text "Name" ]
                        , th [ style "background-color" "white" ] [ text "Description" ]
                        , th [ style "background-color" "white", style "width" "120px" ] [ text "Status" ]
                        ]
                    ]
                , tbody []
                    (List.map (viewDatabaseRow dbList.current) dbList.databases)
                ]
            ]
        ]


viewDatabaseRow : Maybe String -> DatabaseStatus -> Html Msg
viewDatabaseRow currentDb db =
    let
        isLoaded =
            currentDb == Just db.name

        isInactive =
            not db.active

        statusTag =
            if isLoaded then
                span [ class "tag is-success" ] [ text "Loaded" ]

            else if isInactive then
                span [ class "tag is-light" ] [ text "Inactive" ]

            else if db.cached then
                span [ class "tag is-info" ] [ text "Cached" ]

            else
                span [ class "tag is-warning" ] [ text "Not cached" ]

        checkboxIcon =
            if isLoaded then
                i [ class "fas fa-check-square has-text-success", style "font-size" "1.2rem" ] []

            else
                i [ class "far fa-square has-text-grey-light", style "font-size" "1.2rem" ] []

        rowStyle =
            if isLoaded then
                [ style "background-color" "#effaf5" ]

            else if isInactive then
                [ style "opacity" "0.6" ]

            else
                []

        rowAttrs =
            if isInactive then
                [ style "cursor" "not-allowed"
                , title "Database is inactive (set active=true in config to load at startup)"
                ]
                    ++ rowStyle

            else
                [ class "is-clickable"
                , style "cursor" "pointer"
                , onClick (ActivateDatabase db.name)
                ]
                    ++ rowStyle
    in
    tr rowAttrs
        [ td [ style "text-align" "center", style "vertical-align" "middle" ]
            [ checkboxIcon ]
        , td []
            [ if isLoaded then
                strong [] [ text db.displayName ]

              else
                text db.displayName
            ]
        , td [ class "has-text-grey" ]
            [ text (db.description |> Maybe.withDefault "") ]
        , td []
            [ statusTag ]
        ]
