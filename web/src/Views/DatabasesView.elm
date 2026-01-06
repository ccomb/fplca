module Views.DatabasesView exposing (Msg(..), viewDatabasesPage)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, stopPropagationOn)
import Json.Decode as Decode
import Models.Database exposing (DatabaseList, DatabaseStatus)


type Msg
    = ActivateDatabase String
    | LoadDatabase String
    | UnloadDatabase String
    | DeleteDatabase String


viewDatabasesPage : Maybe DatabaseList -> Bool -> Maybe String -> Html Msg
viewDatabasesPage maybeDatabases loading error =
    div [ class "databases-page", style "display" "flex", style "flex-direction" "column", style "height" "100%" ]
        [ div [ class "box", style "margin-bottom" "0", style "flex-shrink" "0" ]
            [ div [ class "level" ]
                [ div [ class "level-left" ]
                    [ div [ class "level-item" ]
                        [ h2 [ class "title is-3", style "margin-bottom" "0" ] [ text "Databases" ]
                        ]
                    ]
                , div [ class "level-right" ]
                    [ div [ class "level-item" ]
                        [ a [ href "/databases/upload", class "button is-primary" ]
                            [ span [ class "icon" ] [ i [ class "fas fa-plus" ] [] ]
                            , span [] [ text "Add database" ]
                            ]
                        ]
                    ]
                ]
            , p [ class "subtitle" ] [ text "Manage your LCA databases" ]
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
                        , th [ style "background-color" "white", style "width" "120px" ] [ text "Format" ]
                        , th [ style "background-color" "white", style "width" "120px" ] [ text "Status" ]
                        , th [ style "background-color" "white", style "width" "200px" ] [ text "Actions" ]
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
            db.loaded

        isCurrent =
            currentDb == Just db.name

        -- Permission checks
        canLoad =
            not isLoaded

        canUnload =
            isLoaded  -- Can always close if loaded (even if current)

        canDelete =
            db.isUploaded && not isLoaded

        statusText =
            if isLoaded then
                span [ class "has-text-success has-text-weight-semibold" ] [ text "Open" ]

            else if db.cached then
                span [ class "has-text-info" ] [ text "Cached" ]

            else
                span [ class "has-text-grey" ] [ text "Closed" ]

        sourceText =
            if db.isUploaded then
                span [ class "has-text-warning-dark", style "margin-left" "0.5rem" ] [ text "(uploaded)" ]

            else
                text ""

        activeIndicator =
            if isCurrent then
                span [ class "has-text-success", style "font-size" "1.2rem" ] [ text "●" ]

            else
                span [ class "has-text-grey-lighter", style "font-size" "1.2rem" ] [ text "○" ]

        rowStyle =
            if isCurrent then
                [ style "background-color" "#effaf5" ]

            else
                []

        rowAttrs =
            if isLoaded then
                [ class "is-clickable"
                , style "cursor" "pointer"
                , onClick (ActivateDatabase db.name)
                ]
                    ++ rowStyle

            else
                rowStyle
    in
    tr rowAttrs
        [ td [ style "text-align" "center", style "vertical-align" "middle" ]
            [ activeIndicator ]
        , td []
            [ if isCurrent then
                strong [] [ text db.displayName ]

              else
                text db.displayName
            , sourceText
            ]
        , td [ class "has-text-grey" ]
            [ text (db.description |> Maybe.withDefault "") ]
        , td [ class "has-text-grey" ]
            [ text (db.format |> Maybe.withDefault "") ]
        , td []
            [ statusText ]
        , td []
            [ viewActionButtons db canLoad canUnload canDelete ]
        ]


viewActionButtons : DatabaseStatus -> Bool -> Bool -> Bool -> Html Msg
viewActionButtons db canLoad canUnload canDelete =
    div [ class "buttons are-small" ]
        [ if canLoad then
            button
                [ class "button is-primary is-small"
                , stopPropagationOn "click" (Decode.succeed ( LoadDatabase db.name, True ))
                ]
                [ span [ class "icon is-small" ] [ i [ class "fas fa-folder-open" ] [] ]
                , span [] [ text "Open" ]
                ]

          else if canUnload then
            button
                [ class "button is-warning is-small"
                , stopPropagationOn "click" (Decode.succeed ( UnloadDatabase db.name, True ))
                ]
                [ span [ class "icon is-small" ] [ i [ class "fas fa-times" ] [] ]
                , span [] [ text "Close" ]
                ]

          else
            text ""
        , if canDelete then
            button
                [ class "button is-danger is-small is-outlined"
                , stopPropagationOn "click" (Decode.succeed ( DeleteDatabase db.name, True ))
                ]
                [ span [ class "icon is-small" ] [ i [ class "fas fa-trash" ] [] ]
                ]

          else
            text ""
        ]
