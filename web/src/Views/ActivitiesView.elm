module Views.ActivitiesView exposing (viewActivitiesPage, Msg(..))

import Html exposing (Html, button, div, input, node, option, select, table, tbody, text, th, thead, tr, h2, p, span)
import Html.Attributes exposing (attribute, class, disabled, id, list, placeholder, selected, style, type_, value)
import Html.Events exposing (onInput, onClick, on)
import Json.Decode as Decode
import Models.Activity exposing (ActivitySummary, ClassificationSystem, FilterPreset, SearchResults)
import Models.Database exposing (DatabaseList, DatabaseLoadStatus(..), DatabaseStatus)
import Views.ActivityRow as ActivityRow


type Msg
    = UpdateSearchQuery String
    | UpdateProductQuery String
    | SelectActivity String
    | LoadMore
    | SelectDatabase String
    | SelectClassificationSystem (Maybe String)
    | UpdatePendingValue String
    | CommitFilter
    | CommitWithValue String String
    | RemoveFilter String
    | ApplyPreset String


viewActivitiesPage : String -> String -> String -> Maybe (SearchResults ActivitySummary) -> Bool -> Bool -> Maybe String -> Maybe DatabaseList -> Maybe (List ClassificationSystem) -> List FilterPreset -> List ( String, String ) -> Maybe String -> String -> Html Msg
viewActivitiesPage currentDbName searchQuery productQuery searchResults searchLoading loadingMore error maybeDatabaseList classificationSystems filterPresets activeFilters pendingSystem pendingValue =
    div [ class "activities-page" ]
        [ div [ class "box" ]
            [ h2 [ class "title is-3" ] [ text "Search Activities" ]
            , p [ class "subtitle" ] [ text "Find activities by name and view their environmental inventory" ]
            , viewFiltersRow maybeDatabaseList currentDbName classificationSystems filterPresets activeFilters pendingSystem pendingValue
            , viewSearchInputs searchQuery productQuery searchLoading
            , case error of
                Just err ->
                    div [ class "notification is-danger" ]
                        [ text ("Error: " ++ err) ]

                Nothing ->
                    text ""
            ]
        , viewSearchResults searchResults searchLoading loadingMore
        ]


viewSearchInputs : String -> String -> Bool -> Html Msg
viewSearchInputs query productQuery isLoading =
    div [ class "field is-grouped", style "margin-bottom" "0.75rem" ]
        [ div [ class "control has-icons-left is-expanded", class (if isLoading then "is-loading" else "") ]
            [ input
                [ id "activity-search"
                , class "input"
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
        , div [ class "control has-icons-left is-expanded" ]
            [ input
                [ id "product-search"
                , class "input"
                , type_ "text"
                , placeholder "Filter by product..."
                , value productQuery
                , onInput UpdateProductQuery
                ]
                []
            , span [ class "icon is-left" ]
                [ Html.i [ class "fas fa-cube" ] []
                ]
            ]
        ]


viewFiltersRow : Maybe DatabaseList -> String -> Maybe (List ClassificationSystem) -> List FilterPreset -> List ( String, String ) -> Maybe String -> String -> Html Msg
viewFiltersRow maybeDatabaseList currentDbName maybeSystems filterPresets activeFilters pendingSystem pendingValue =
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

        chipSep =
            span
                [ style "background-color" "#d0d0d0"
                , style "width" "1px"
                , style "align-self" "stretch"
                , style "flex-shrink" "0"
                ]
                []

        chipOrSep =
            span
                [ style "background-color" "#f8f8f8"
                , style "align-self" "stretch"
                , style "display" "inline-flex"
                , style "align-items" "center"
                , style "padding" "0 0.5em"
                , style "color" "#555"
                ]
                [ text "OR" ]

        chips =
            activeFilters
                |> groupBySystem
                |> List.map
                    (\( sys, vals ) ->
                        div [ class "control" ]
                            [ div
                                [ style "display" "inline-flex"
                                , style "align-items" "stretch"
                                , style "border" "1px solid #dbdbdb"
                                , style "border-radius" "4px"
                                , style "overflow" "hidden"
                                , style "font-size" "inherit"
                                , style "font-family" "inherit"
                                , style "min-height" "2.5em"
                                , style "height" "auto"
                                , style "max-width" "min(700px, calc(100vw - 220px))"
                                , style "background-color" "white"
                                ]
                                [ span
                                    [ style "background-color" "#f0f0f0"
                                    , style "display" "inline-flex"
                                    , style "align-items" "center"
                                    , style "padding" "0 0.75em"
                                    , style "color" "#363636"
                                    ]
                                    [ text sys ]
                                , chipSep
                                , span
                                    [ style "background-color" "white"
                                    , style "display" "flex"
                                    , style "flex-direction" "column"
                                    , style "justify-content" "center"
                                    , style "flex" "1"
                                    , style "min-width" "0"
                                    , style "color" "#363636"
                                    ]
                                    (case vals of
                                        [] ->
                                            []

                                        first :: rest ->
                                            span [ style "padding" "0.4em 0.75em", style "word-break" "break-all" ] [ text first ]
                                                :: List.map
                                                    (\v ->
                                                        span [ style "display" "flex", style "align-items" "flex-start" ]
                                                            [ chipOrSep
                                                            , span [ style "padding" "0.4em 0.75em", style "word-break" "break-all", style "flex" "1", style "min-width" "0" ] [ text v ]
                                                            ]
                                                    )
                                                    rest
                                    )
                                , chipSep
                                , span
                                    [ style "background-color" "#f0f0f0"
                                    , style "display" "inline-flex"
                                    , style "align-items" "center"
                                    , style "padding" "0 0.75em"
                                    , style "cursor" "pointer"
                                    , style "color" "#363636"
                                    , style "font-size" "1.1em"
                                    , style "line-height" "1"
                                    , onClick (RemoveFilter sys)
                                    ]
                                    [ text "×" ]
                                ]
                            ]
                    )

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

                                            else if String.startsWith "preset:" val then
                                                ApplyPreset (String.dropLeft 7 val)

                                            else
                                                SelectClassificationSystem (Just val)
                                        )
                                    ]
                                    ([ option [ value "", selected (pendingSystem == Nothing) ] [ text "Classification..." ] ]
                                        ++ List.map
                                            (\sys ->
                                                option
                                                    [ value sys.name
                                                    , selected (pendingSystem == Just sys.name)
                                                    ]
                                                    [ text (sys.name ++ " (" ++ String.fromInt sys.activityCount ++ ")") ]
                                            )
                                            systems
                                        ++ (if List.isEmpty filterPresets then
                                                []

                                            else
                                                [ node "optgroup"
                                                    [ attribute "label" "Presets" ]
                                                    (List.map
                                                        (\p ->
                                                            option [ value ("preset:" ++ p.name) ]
                                                                [ text ("→ " ++ p.label) ]
                                                        )
                                                        filterPresets
                                                    )
                                                ]
                                           )
                                    )
                                ]
                            ]
                        ]

        valueInput =
            case ( maybeSystems, pendingSystem ) of
                ( Just _, Just sys ) ->
                    let
                        currentValues =
                            maybeSystems
                                |> Maybe.withDefault []
                                |> List.filter (\s -> s.name == sys)
                                |> List.head
                                |> Maybe.map .values
                                |> Maybe.withDefault []

                        filteredValues =
                            if String.isEmpty pendingValue then
                                []
                            else
                                List.filter (\v -> String.contains (String.toLower pendingValue) (String.toLower v)) currentValues

                        suggestionsDropdown =
                            if List.isEmpty filteredValues then
                                text ""
                            else
                                div
                                    [ style "position" "absolute"
                                    , style "z-index" "10"
                                    , style "background" "white"
                                    , style "border" "1px solid #dbdbdb"
                                    , style "border-radius" "4px"
                                    , style "max-height" "200px"
                                    , style "overflow-y" "auto"
                                    , style "width" "100%"
                                    , style "top" "100%"
                                    , style "left" "0"
                                    ]
                                    (List.map
                                        (\v ->
                                            div
                                                [ class "dropdown-item"
                                                , style "cursor" "pointer"
                                                , onClick (CommitWithValue sys v)
                                                ]
                                                [ text v ]
                                        )
                                        filteredValues
                                    )
                    in
                    [ div [ class "control is-expanded" ]
                        [ div [ style "position" "relative" ]
                            [ input
                                [ class "input"
                                , type_ "text"
                                , placeholder "Filter by classification value..."
                                , value pendingValue
                                , onInput UpdatePendingValue
                                , on "keydown" (Decode.andThen (\key -> if key == "Enter" then Decode.succeed CommitFilter else Decode.fail "not enter") (Decode.field "key" Decode.string))
                                ]
                                []
                            , suggestionsDropdown
                            ]
                        ]
                    ]

                _ ->
                    []

        addButton =
            case pendingSystem of
                Just _ ->
                    if String.isEmpty pendingValue then
                        []

                    else
                        [ div [ class "control" ]
                            [ button
                                [ class "button is-info"
                                , onClick CommitFilter
                                ]
                                [ span [ class "icon" ] [ Html.i [ class "fas fa-plus" ] [] ]
                                , span [] [ text "Add" ]
                                ]
                            ]
                        ]

                Nothing ->
                    []

        allControls =
            dbDropdown ++ chips ++ classDropdown ++ valueInput ++ addButton
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


-- Group [(sys, val)] into [(sys, [val])] preserving first-seen system order
groupBySystem : List ( String, String ) -> List ( String, List String )
groupBySystem pairs =
    List.foldl
        (\( sys, val ) acc ->
            case List.filter (\( s, _ ) -> s == sys) acc of
                [] ->
                    acc ++ [ ( sys, [ val ] ) ]

                _ ->
                    List.map
                        (\( s, vs ) ->
                            if s == sys then
                                ( s, vs ++ [ val ] )

                            else
                                ( s, vs )
                        )
                        acc
        )
        []
        pairs


viewDatabaseOption : String -> DatabaseStatus -> Html Msg
viewDatabaseOption currentDbName db =
    option
        [ value db.name
        , selected (db.name == currentDbName)
        ]
        [ text db.displayName ]
