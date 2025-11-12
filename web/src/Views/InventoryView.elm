module Views.InventoryView exposing (viewInventoryPage, Msg(..))

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models.Inventory exposing (InventoryExport, InventoryFlowDetail)
import Utils.Format as Format

type Msg
    = UpdateSearchQuery String

viewInventoryPage : Maybe InventoryExport -> Bool -> Maybe String -> String -> Html Msg
viewInventoryPage maybeInventory loading error searchQuery =
    div [ class "inventory-page" ]
        [ -- Header with navigation
          nav [ class "navbar is-light" ]
            [ div [ class "navbar-brand" ]
                [ div [ class "navbar-item" ]
                    [ h1 [ class "title is-4" ] [ text "Activity Inventory" ]
                    ]
                ]
            ]
        , -- Main content
          div [ class "section" ]
            [ div [ class "container" ]
                [ case ( loading, error, maybeInventory ) of
                    ( True, _, _ ) ->
                        div [ class "has-text-centered" ]
                            [ div [ class "is-size-3" ] [ text "Loading inventory..." ]
                            , progress [ class "progress is-primary", attribute "max" "100" ] []
                            ]

                    ( _, Just errorMsg, _ ) ->
                        div [ class "notification is-danger" ]
                            [ strong [] [ text "Error: " ]
                            , text errorMsg
                            ]

                    ( _, _, Just inventory ) ->
                        div []
                            [ viewInventoryHeader inventory
                            , viewSearchBox searchQuery
                            , viewInventoryTable (filterFlows searchQuery inventory.ieFlows)
                            ]

                    ( _, _, Nothing ) ->
                        div [ class "has-text-centered" ]
                            [ text "No inventory data to display" ]
                ]
            ]
        ]

viewInventoryHeader : InventoryExport -> Html msg
viewInventoryHeader inventory =
    div [ class "box" ]
        [ h2 [ class "title is-5" ] [ text "Inventory Metadata" ]
        , div [ class "columns" ]
            [ div [ class "column" ]
                [ div [ class "field" ]
                    [ label [ class "label" ] [ text "Root Activity" ]
                    , div [ class "content" ]
                        [ strong [] [ text inventory.ieMetadata.imRootActivity.prsName ]
                        , br [] []
                        , text inventory.ieMetadata.imRootActivity.prsLocation
                        , br [] []
                        , small [ class "has-text-grey" ] [ text inventory.ieMetadata.imRootActivity.prsId ]
                        ]
                    ]
                ]
            , div [ class "column" ]
                [ div [ class "field" ]
                    [ label [ class "label" ] [ text "Calculation Details" ]
                    , div [ class "content" ]
                        [ div [] [ text ("Total flows: " ++ String.fromInt inventory.ieMetadata.imTotalFlows) ]
                        , div [] [ text ("Emission flows: " ++ String.fromInt inventory.ieMetadata.imEmissionFlows) ]
                        , div [] [ text ("Resource flows: " ++ String.fromInt inventory.ieMetadata.imResourceFlows) ]
                        ]
                    ]
                ]
            ]
        ]

viewInventoryTable : List InventoryFlowDetail -> Html msg
viewInventoryTable flows =
    div [ class "box" ]
        [ h2 [ class "title is-5" ] [ text "Inventory Flows" ]
        , div [ class "table-container" ]
            [ table [ class "table is-striped is-hoverable is-fullwidth" ]
                [ thead []
                    [ tr []
                        [ th [] [ text "Compartment" ]
                        , th [] [ text "Flow" ]
                        , th [] [ text "Amount" ]
                        , th [] [ text "Unit" ]
                        , th [] [ text "Type" ]
                        ]
                    ]
                , tbody []
                    (List.map viewInventoryRow flows)
                ]
            ]
        ]

viewInventoryRow : InventoryFlowDetail -> Html msg
viewInventoryRow flowDetail =
    tr []
        [ td [] [ text flowDetail.ifdFlow.flowCategory ]
        , td []
            [ div []
                [ div [] [ text flowDetail.ifdFlow.flowName ]
                , small [ class "has-text-grey" ] [ text flowDetail.ifdFlow.flowId ]
                ]
            ]
        , td [ class "has-text-right" ] [ text (Format.formatScientific flowDetail.ifdQuantity) ]
        , td [] [ text flowDetail.ifdUnitName ]
        , td []
            [ span
                [ class
                    (if flowDetail.ifdIsEmission then
                        "tag is-warning"
                     else
                        "tag is-info"
                    )
                ]
                [ text
                    (if flowDetail.ifdIsEmission then
                        "Emission"
                     else
                        "Resource"
                    )
                ]
            ]
        ]

viewSearchBox : String -> Html Msg
viewSearchBox searchQuery =
    div [ class "box" ]
        [ div [ class "field" ]
            [ label [ class "label" ] [ text "Search Flows" ]
            , div [ class "control has-icons-left" ]
                [ input
                    [ class "input"
                    , type_ "text"
                    , placeholder "Search by flow name (e.g., \"dioxide carbon\" for flows containing both words)"
                    , value searchQuery
                    , onInput UpdateSearchQuery
                    ]
                    []
                , span [ class "icon is-small is-left" ]
                    [ i [ class "fas fa-search" ] []
                    ]
                ]
            , p [ class "help" ]
                [ text "Enter multiple words to find flows containing all words (case-insensitive)" ]
            ]
        ]

filterFlows : String -> List InventoryFlowDetail -> List InventoryFlowDetail
filterFlows searchQuery flows =
    if String.isEmpty (String.trim searchQuery) then
        flows
    else
        let
            -- Split the search query into words and convert to lowercase
            searchWords =
                searchQuery
                    |> String.toLower
                    |> String.words
                    |> List.filter (\w -> not (String.isEmpty w))

            -- Check if a flow matches all search words
            matchesAllWords flow =
                let
                    -- Combine all searchable text from the flow
                    searchableText =
                        String.toLower
                            (flow.ifdFlow.flowName
                                ++ " "
                                ++ flow.ifdFlow.flowCategory
                                ++ " "
                                ++ flow.ifdUnitName
                            )
                in
                List.all (\word -> String.contains word searchableText) searchWords
        in
        List.filter matchesAllWords flows