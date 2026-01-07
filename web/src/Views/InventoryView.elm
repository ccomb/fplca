module Views.InventoryView exposing (Msg(..), viewInventoryTable)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models.Inventory exposing (InventoryFlowDetail)
import Utils.Format as Format


type Msg
    = UpdateSearchQuery String


viewInventoryTable : String -> List InventoryFlowDetail -> Html Msg
viewInventoryTable searchQuery flows =
    let
        filteredFlows =
            filterFlows searchQuery flows
    in
    div [ style "flex" "1", style "overflow-y" "auto", style "min-height" "0" ]
        [ table [ class "table is-striped is-hoverable is-fullwidth" ]
            [ thead [ style "position" "sticky", style "top" "0", style "background-color" "white", style "z-index" "10" ]
                [ tr []
                    [ th [ class "has-text-right", style "background-color" "white" ] [ text "Amount" ]
                    , th [ style "background-color" "white" ] [ text "Unit" ]
                    , th [ style "background-color" "white" ]
                        [ text "Flow"
                        ]
                    , th [ style "background-color" "white" ] [ text "Compartment" ]
                    , th [ style "background-color" "white" ] [ text "Type" ]
                    ]
                , tr []
                    [ th [ style "background-color" "white" ] []
                    , th [ style "background-color" "white" ] []
                    , th [ style "background-color" "white", style "padding" "0.25rem 0.5rem" ]
                        [ div [ class "control has-icons-left" ]
                            [ input
                                [ class "input is-small"
                                , type_ "text"
                                , placeholder "Search flows..."
                                , value searchQuery
                                , onInput UpdateSearchQuery
                                ]
                                []
                            , span [ class "icon is-small is-left" ]
                                [ i [ class "fas fa-search" ] []
                                ]
                            ]
                        ]
                    , th [ style "background-color" "white" ] []
                    , th [ style "background-color" "white" ] []
                    ]
                ]
            , tbody []
                (List.map viewInventoryRow filteredFlows)
            ]
        ]


viewInventoryRow : InventoryFlowDetail -> Html msg
viewInventoryRow flowDetail =
    tr []
        [ td [ class "has-text-right" ] [ text (Format.formatScientific flowDetail.ifdQuantity) ]
        , td [] [ text flowDetail.ifdUnitName ]
        , td []
            [ div []
                [ div [] [ text flowDetail.ifdFlow.flowName ]
                , small [ class "has-text-grey" ] [ text flowDetail.ifdFlow.flowId ]
                ]
            ]
        , td [] [ text flowDetail.ifdFlow.flowCategory ]
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
