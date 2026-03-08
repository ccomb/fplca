module Views.DatabasesView exposing (Msg(..), viewDatabasesPage)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (stopPropagationOn)
import Json.Decode as Decode
import Models.Database exposing (DatabaseList, DatabaseLoadStatus(..), DatabaseStatus)
import Set exposing (Set)
import Views.ListActions as ListActions


type Msg
    = NavigateToDatabase String
    | LoadDatabase String
    | UnloadDatabase String
    | DeleteDatabase String
    | ConfirmDeleteDatabase String
    | CancelDeleteDatabase
    | SetupDatabase String


viewDatabasesPage : Maybe DatabaseList -> Maybe String -> Maybe String -> Set String -> Maybe String -> Maybe String -> List String -> Html Msg
viewDatabasesPage maybeDatabases error confirmingDelete loadingDbs unloadingDb deletingDb progressLines =
    div [ class "databases-page" ]
        [ ListActions.viewPageHeader
            { title = "Databases", subtitle = "Manage your LCA databases", addHref = "/databases/upload", addLabel = "Add database" }
            error
        , case maybeDatabases of
            Just dbList ->
                viewDatabasesList dbList confirmingDelete loadingDbs unloadingDb deletingDb progressLines

            Nothing ->
                ListActions.viewLoadingSpinner
        ]


viewDatabasesList : DatabaseList -> Maybe String -> Set String -> Maybe String -> Maybe String -> List String -> Html Msg
viewDatabasesList dbList confirmingDelete loadingDbs unloadingDb deletingDb progressLines =
    let
        dbCount =
            List.length dbList.databases
    in
    div []
        [ div [ style "padding" "0.5rem 0" ]
            [ span [ class "tag is-info is-light" ]
                [ text (String.fromInt dbCount ++ " databases") ]
            ]
        , table [ class "table is-striped is-hoverable is-fullwidth" ]
            [ ListActions.viewTableHeader "Activities"
            , tbody []
                (List.concatMap (viewDatabaseRowWithProgress confirmingDelete loadingDbs unloadingDb deletingDb progressLines) dbList.databases)
            ]
        ]


viewDatabaseRowWithProgress : Maybe String -> Set String -> Maybe String -> Maybe String -> List String -> DatabaseStatus -> List (Html Msg)
viewDatabaseRowWithProgress confirmingDelete loadingDbs unloadingDb deletingDb progressLines db =
    let
        isLoading =
            Set.member db.name loadingDbs

        lastLines =
            List.reverse progressLines |> List.take 3 |> List.reverse
    in
    viewDatabaseRow confirmingDelete loadingDbs unloadingDb deletingDb db
        :: (if isLoading && not (List.isEmpty lastLines) then
                [ tr []
                    [ td [ attribute "colspan" "7", style "padding" "0.25rem 1rem 0.75rem", style "border-top" "none" ]
                        [ div
                            [ style "font-family" "'Consolas', 'Monaco', monospace"
                            , style "font-size" "0.75rem"
                            , style "color" "#7a7a7a"
                            , style "line-height" "1.5"
                            ]
                            (List.map (\line -> div [] [ text line ]) lastLines)
                        ]
                    ]
                ]

            else
                []
           )


viewDatabaseRow : Maybe String -> Set String -> Maybe String -> Maybe String -> DatabaseStatus -> Html Msg
viewDatabaseRow confirmingDelete loadingDbs unloadingDb deletingDb db =
    let
        isLoading =
            Set.member db.name loadingDbs

        isUnloading =
            unloadingDb == Just db.name

        isDeleting =
            deletingDb == Just db.name

        canDelete =
            db.isUploaded && db.status == Unloaded

        closeButton =
            ListActions.viewCloseButton
                { onClose = UnloadDatabase db.name, isClosing = isUnloading }

        actions =
            div [ class "buttons are-small" ]
                [ case db.status of
                    Unloaded ->
                        ListActions.viewOpenButton
                            { onOpen =
                                if db.isUploaded then
                                    SetupDatabase db.name

                                else
                                    LoadDatabase db.name
                            , isLoading = isLoading
                            }

                    PartiallyLinked ->
                        span [ class "buttons are-small", style "display" "inline-flex" ]
                            [ button
                                [ class "button is-info is-small"
                                , stopPropagationOn "click" (Decode.succeed ( SetupDatabase db.name, True ))
                                ]
                                [ span [ class "icon is-small" ] [ i [ class "fas fa-info-circle" ] [] ]
                                , span [] [ text "Info" ]
                                ]
                            , closeButton
                            ]

                    DbLoaded ->
                        span [ class "buttons are-small", style "display" "inline-flex" ]
                            [ button
                                [ class "button is-info is-small is-outlined"
                                , stopPropagationOn "click" (Decode.succeed ( SetupDatabase db.name, True ))
                                ]
                                [ span [ class "icon is-small" ] [ i [ class "fas fa-info-circle" ] [] ]
                                , span [] [ text "Info" ]
                                ]
                            , closeButton
                            ]
                , if canDelete then
                    ListActions.viewDeleteConfirmation
                        { onConfirm = ConfirmDeleteDatabase db.name
                        , onDelete = DeleteDatabase db.name
                        , onCancel = CancelDeleteDatabase
                        , isConfirming = confirmingDelete == Just db.name
                        , isDeleting = isDeleting
                        }

                  else
                    text ""
                ]
    in
    ListActions.viewRow
        { status = db.status
        , actions = actions
        , displayName = db.displayName
        , description = db.description |> Maybe.withDefault ""
        , count = db.activityCount
        , isUploaded = db.isUploaded
        , isAuto = False
        , format = db.format |> Maybe.withDefault ""
        , onNavigate =
            case db.status of
                DbLoaded ->
                    Just (NavigateToDatabase db.name)

                _ ->
                    Nothing
        }
