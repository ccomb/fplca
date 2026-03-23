module Views.ActivitiesView exposing (viewActivitiesPage, Msg(..))

import Html exposing (Html, button, div, input, option, select, table, tbody, text, th, thead, tr, h2, p, span)
import Html.Attributes exposing (class, disabled, id, list, placeholder, selected, style, type_, value)
import Html.Events exposing (onInput, onClick)
import Models.Activity exposing (ActivitySummary, ClassificationSystem, SearchResults)
import Models.Database exposing (DatabaseList, DatabaseLoadStatus(..), DatabaseStatus)
import Views.ActivityRow as ActivityRow


type Msg
    = UpdateSearchQuery String
    | SelectActivity String
    | LoadMore
    | SelectDatabase String
    | SelectClassificationSystem (Maybe String)
    | SelectClassificationValue (Maybe String)


viewActivitiesPage : String -> String -> Maybe (SearchResults ActivitySummary) -> Bool -> Bool -> Maybe String -> Maybe DatabaseList -> Maybe (List ClassificationSystem) -> Maybe String -> Maybe String -> Html Msg
viewActivitiesPage currentDbName searchQuery searchResults searchLoading loadingMore error maybeDatabaseList classificationSystems selectedSystem selectedValue =
    div [ class "activities-page" ]
        [ div [ class "box" ]
            [ h2 [ class "title is-3" ] [ text "Search Activities" ]
            , p [ class "subtitle" ] [ text "Find activities by name and view their environmental inventory" ]
            , viewFiltersRow maybeDatabaseList currentDbName classificationSystems selectedSystem selectedValue
            , viewSearchInput searchQuery searchLoading
            , case error of
                Just err ->
                    div [ class "notification is-danger" ]
                        [ text ("Error: " ++ err) ]

                Nothing ->
                    text ""
            ]
        , viewSearchResults searchResults searchLoading loadingMore
        ]


viewSearchInput : String -> Bool -> Html Msg
viewSearchInput query isLoading =
    div [ class "field", style "margin-bottom" "0.75rem" ]
        [ div [ class "control has-icons-left is-expanded", class (if isLoading then "is-loading" else "") ]
            [ input
                [ id "activity-search"
                , class "input is-large"
                , type_ "text"
                , placeholder "Search activities by name..."
                , value query
                , onInput UpdateSearchQuery
                ]
                []
            , span [ class "icon is-left" ]
                [ Html.i [ class "fas fa-search" ] []
                ]
            ]
        ]


viewFiltersRow : Maybe DatabaseList -> String -> Maybe (List ClassificationSystem) -> Maybe String -> Maybe String -> Html Msg
viewFiltersRow maybeDatabaseList currentDbName maybeSystems selectedSystem selectedValue =
    let
        dbDropdown =
            case maybeDatabaseList of
                Nothing ->
                    []

                Just dbList ->
                    let
                        loadedDatabases =
                            List.filter (\db -> db.status == DbLoaded) dbList.databases
                    in
                    [ div [ class "control" ]
                        [ div [ class "select" ]
                            [ select
                                [ onInput SelectDatabase ]
                                (List.map (viewDatabaseOption currentDbName) loadedDatabases)
                            ]
                        ]
                    ]

        classDropdown =
            case maybeSystems of
                Nothing ->
                    []

                Just systems ->
                    if List.isEmpty systems then
                        []

                    else
                        [ div [ class "control" ]
                            [ div [ class "select" ]
                                [ select
                                    [ onInput
                                        (\val ->
                                            if val == "" then
                                                SelectClassificationSystem Nothing

                                            else
                                                SelectClassificationSystem (Just val)
                                        )
                                    ]
                                    (option [ value "", selected (selectedSystem == Nothing) ] [ text "Classification..." ]
                                        :: List.map
                                            (\sys ->
                                                option
                                                    [ value sys.name
                                                    , selected (selectedSystem == Just sys.name)
                                                    ]
                                                    [ text (sys.name ++ " (" ++ String.fromInt sys.activityCount ++ ")") ]
                                            )
                                            systems
                                    )
                                ]
                            ]
                        ]

        valueInput =
            case ( maybeSystems, selectedSystem ) of
                ( Just _, Just sys ) ->
                    let
                        currentValues =
                            maybeSystems
                                |> Maybe.withDefault []
                                |> List.filter (\s -> s.name == sys)
                                |> List.head
                                |> Maybe.map .values
                                |> Maybe.withDefault []

                        datalistId =
                            "classification-values"
                    in
                    [ div [ class "control is-expanded" ]
                        [ input
                            [ class "input"
                            , type_ "text"
                            , placeholder "Filter by classification value..."
                            , value (Maybe.withDefault "" selectedValue)
                            , list datalistId
                            , onInput
                                (\val ->
                                    if String.isEmpty val then
                                        SelectClassificationValue Nothing

                                    else
                                        SelectClassificationValue (Just val)
                                )
                            ]
                            []
                        , Html.node "datalist"
                            [ id datalistId ]
                            (List.map (\v -> option [ value v ] []) currentValues)
                        ]
                    ]

                _ ->
                    []

        clearButton =
            case selectedValue of
                Just _ ->
                    [ div [ class "control" ]
                        [ button
                            [ class "button is-light"
                            , onClick (SelectClassificationValue Nothing)
                            ]
                            [ span [ class "icon" ] [ Html.i [ class "fas fa-times" ] [] ]
                            , span [] [ text "Clear" ]
                            ]
                        ]
                    ]

                Nothing ->
                    []

        allControls =
            dbDropdown ++ classDropdown ++ valueInput ++ clearButton
    in
    if List.isEmpty allControls then
        text ""

    else
        div [ class "field is-grouped is-grouped-multiline", style "margin-bottom" "0.5rem" ]
            allControls


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
                    [ div [ style "padding" "0.5rem 0" ]
                        [ span [ class "tag is-info is-light" ]
                            [ text (String.fromInt (List.length results.results) ++ " / " ++ String.fromInt results.totalCount ++ " activities")
                            ]
                        ]
                    , table [ class "table is-striped is-hoverable is-fullwidth" ]
                        [ thead []
                            [ tr []
                                [ th [] [ text "Activity Name" ]
                                , th [ class "has-text-right" ] [ text "Amount" ]
                                , th [] [ text "Unit" ]
                                , th [] [ text "Product" ]
                                , th [] [ text "Location" ]
                                ]
                            ]
                        , tbody []
                            (List.map viewActivityRow results.results)
                        ]
                    , if results.hasMore then
                        div [ class "has-text-centered", style "padding" "1rem 0" ]
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
    ActivityRow.viewActivityRow
        { id = Just activity.id
        , name = activity.name
        , amount = activity.productAmount
        , unit = activity.productUnit
        , product = activity.product
        , location = activity.location
        , onNavigate = SelectActivity
        }


viewDatabaseOption : String -> DatabaseStatus -> Html Msg
viewDatabaseOption currentDbName db =
    option
        [ value db.name
        , selected (db.name == currentDbName)
        ]
        [ text db.displayName ]
