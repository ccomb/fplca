module Views.InventoryView exposing (Msg(..), viewInventoryTables)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models.Inventory exposing (InventoryFlowDetail)
import Utils.Format as Format


type Msg
    = UpdateResourcesSearch String
    | UpdateEmissionsSearch String


viewInventoryTables : String -> String -> Int -> Int -> List InventoryFlowDetail -> Html Msg
viewInventoryTables resourcesSearch emissionsSearch resourcesCount emissionsCount flows =
    let
        resources =
            flows |> List.filter (not << .ifdIsEmission)

        emissions =
            flows |> List.filter .ifdIsEmission

        filteredResources =
            filterFlows resourcesSearch resources

        filteredEmissions =
            filterFlows emissionsSearch emissions
    in
    div [ class "columns", style "flex" "1", style "min-height" "0", style "margin" "0" ]
        [ div [ class "column", style "display" "flex", style "flex-direction" "column", style "min-height" "0", style "padding-right" "0.5rem" ]
            [ viewTableWithTitle ("Natural Resources (" ++ String.fromInt resourcesCount ++ ")") "fas fa-leaf" "has-text-success" resourcesSearch UpdateResourcesSearch filteredResources False
            ]
        , div [ class "column", style "display" "flex", style "flex-direction" "column", style "min-height" "0", style "padding-left" "0.5rem" ]
            [ viewTableWithTitle ("Emissions (" ++ String.fromInt emissionsCount ++ ")") "fas fa-cloud" "has-text-warning" emissionsSearch UpdateEmissionsSearch filteredEmissions True
            ]
        ]


viewTableWithTitle : String -> String -> String -> String -> (String -> Msg) -> List InventoryFlowDetail -> Bool -> Html Msg
viewTableWithTitle title iconClass iconColor searchQuery onSearch flows invertStripes =
    div [ style "display" "flex", style "flex-direction" "column", style "height" "100%" ]
        [ h3 [ class "title is-5", style "margin-bottom" "0.5rem" ]
            [ span [ class ("icon " ++ iconColor), style "margin-right" "0.5rem" ]
                [ i [ class iconClass ] []
                ]
            , text title
            ]
        , div [ style "flex" "1", style "overflow-y" "auto", style "min-height" "0" ]
            [ div [ class "table-container" ]
              [ table [ class "table is-hoverable is-fullwidth" ]
                [ thead [ style "position" "sticky", style "top" "0", style "background-color" "white", style "z-index" "10" ]
                    [ tr []
                        [ th [ class "has-text-right", style "background-color" "white" ] [ text "Amount" ]
                        , th [ style "background-color" "white" ] [ text "Unit" ]
                        , th [ style "background-color" "white" ] [ text "Flow" ]
                        , th [ style "background-color" "white" ] [ text "Compartment" ]
                        ]
                    , tr []
                        [ th [ style "background-color" "white" ] []
                        , th [ style "background-color" "white" ] []
                        , th [ style "background-color" "white", style "padding" "0.25rem 0.5rem" ]
                            [ div [ class "control has-icons-left" ]
                                [ input
                                    [ class "input is-small"
                                    , type_ "text"
                                    , placeholder "Search..."
                                    , value searchQuery
                                    , onInput onSearch
                                    ]
                                    []
                                , span [ class "icon is-small is-left" ]
                                    [ i [ class "fas fa-search" ] []
                                    ]
                                ]
                            ]
                        , th [ style "background-color" "white" ] []
                        ]
                    ]
                , tbody []
                    (List.indexedMap (viewInventoryRow invertStripes) flows)
                ]
              ]
            ]
        ]


viewInventoryRow : Bool -> Int -> InventoryFlowDetail -> Html msg
viewInventoryRow invertStripes index flowDetail =
    let
        isOdd =
            modBy 2 index == 1

        shouldHighlight =
            if invertStripes then
                not isOdd

            else
                isOdd

        bgColor =
            if shouldHighlight then
                "#fafafa"

            else
                "white"
    in
    tr [ style "background-color" bgColor ]
        [ td [ class "has-text-right" ] [ text (Format.formatScientific flowDetail.ifdQuantity) ]
        , td [] [ text flowDetail.ifdUnitName ]
        , td [] [ text flowDetail.ifdFlow.flowName ]
        , td [] [ text flowDetail.ifdFlow.flowCategory ]
        ]


filterFlows : String -> List InventoryFlowDetail -> List InventoryFlowDetail
filterFlows searchQuery flows =
    if String.isEmpty (String.trim searchQuery) then
        flows

    else
        let
            searchWords =
                searchQuery
                    |> String.toLower
                    |> String.words
                    |> List.filter (\w -> not (String.isEmpty w))

            matchesAllWords flow =
                let
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
