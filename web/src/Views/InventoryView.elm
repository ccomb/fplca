module Views.InventoryView exposing (viewInventoryPage)

import Html exposing (..)
import Html.Attributes exposing (..)
import Models.Inventory exposing (InventoryExport, InventoryFlowDetail)

viewInventoryPage : Maybe InventoryExport -> Bool -> Maybe String -> Html msg
viewInventoryPage maybeInventory loading error =
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
                            , viewInventoryTable inventory.ieFlows
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
        , td [ class "has-text-right" ] [ text (String.fromFloat flowDetail.ifdQuantity) ]
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