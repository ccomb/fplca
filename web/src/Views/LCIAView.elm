module Views.LCIAView exposing (Msg(..), viewLCIAPage, viewPageNavbar)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models.LCIA exposing (LCIAResult, MappingStatus, MethodSummary)
import Utils.Format as Format


type Msg
    = SelectMethod String
    | ComputeLCIA String


viewLCIAPage :
    Maybe (List MethodSummary)
    -> Maybe MethodSummary
    -> Maybe LCIAResult
    -> Maybe MappingStatus
    -> Bool
    -> Bool
    -> Maybe String
    -> Maybe ( String, String )
    -> Html Msg
viewLCIAPage maybeMethods selectedMethod maybeLCIAResult maybeMappingStatus loadingMethods loadingLCIA error maybeActivityInfo =
    div [ class "lcia-page" ]
        [ viewPageNavbar "Impact Assessment (LCIA)" maybeActivityInfo
        , div []
            [ case ( loadingMethods, error, maybeMethods ) of
                ( True, _, _ ) ->
                    div [ class "has-text-centered" ]
                        [ div [ class "is-size-3" ] [ text "Loading methods..." ]
                        , progress [ class "progress is-primary", attribute "max" "100" ] []
                        ]

                ( _, Just errorMsg, _ ) ->
                    div [ class "notification is-danger" ]
                        [ strong [] [ text "Error: " ]
                        , text errorMsg
                        ]

                ( _, _, Just methods ) ->
                    div []
                        [ viewMethodSelector methods selectedMethod
                        , case selectedMethod of
                            Just method ->
                                div []
                                    [ viewSelectedMethod method loadingLCIA
                                    , viewLCIAResults maybeLCIAResult loadingLCIA
                                    , viewMappingStatus maybeMappingStatus
                                    ]

                            Nothing ->
                                div [ class "box" ]
                                    [ p [ class "has-text-grey" ] [ text "Select a method to compute impact assessment" ]
                                    ]
                        ]

                ( _, _, Nothing ) ->
                    div [ class "notification is-warning" ]
                        [ text "No methods available. Make sure the server was started with --methods option pointing to ILCD method files." ]
            ]
        ]


viewMethodSelector : List MethodSummary -> Maybe MethodSummary -> Html Msg
viewMethodSelector methods selectedMethod =
    div [ class "box" ]
        [ h2 [ class "title is-5" ] [ text "Select Impact Category" ]
        , div [ class "field" ]
            [ div [ class "control" ]
                [ div [ class "select is-fullwidth" ]
                    [ select
                        [ onInput SelectMethod ]
                        (option [ value "" ] [ text "-- Select a method --" ]
                            :: List.map (viewMethodOption selectedMethod) methods
                        )
                    ]
                ]
            ]
        , p [ class "help" ] [ text (String.fromInt (List.length methods) ++ " methods available") ]
        ]


viewMethodOption : Maybe MethodSummary -> MethodSummary -> Html Msg
viewMethodOption selectedMethod method =
    let
        isSelected =
            case selectedMethod of
                Just sel ->
                    sel.msmId == method.msmId

                Nothing ->
                    False
    in
    option
        [ value method.msmId
        , selected isSelected
        ]
        [ text (method.msmCategory ++ " - " ++ method.msmName ++ " (" ++ method.msmUnit ++ ")") ]


viewSelectedMethod : MethodSummary -> Bool -> Html Msg
viewSelectedMethod method loading =
    div [ class "box" ]
        [ h3 [ class "title is-5" ] [ text method.msmName ]
        , div [ class "columns" ]
            [ div [ class "column" ]
                [ p [] [ strong [] [ text "Category: " ], text method.msmCategory ]
                , p [] [ strong [] [ text "Unit: " ], text method.msmUnit ]
                , p [] [ strong [] [ text "Characterization factors: " ], text (String.fromInt method.msmFactorCount) ]
                ]
            , div [ class "column is-narrow" ]
                [ button
                    [ class
                        (if loading then
                            "button is-primary is-loading"

                         else
                            "button is-primary"
                        )
                    , onClick (ComputeLCIA method.msmId)
                    , disabled loading
                    ]
                    [ span [ class "icon" ] [ i [ class "fas fa-calculator" ] [] ]
                    , span [] [ text "Compute Impact" ]
                    ]
                ]
            ]
        ]


viewLCIAResults : Maybe LCIAResult -> Bool -> Html msg
viewLCIAResults maybeResult loading =
    case maybeResult of
        Nothing ->
            if loading then
                div [ class "box" ]
                    [ h3 [ class "title is-5" ] [ text "Computing..." ]
                    , progress [ class "progress is-primary", attribute "max" "100" ] []
                    ]

            else
                text ""

        Just result ->
            div [ class "box" ]
                [ h3 [ class "title is-5" ] [ text "Impact Assessment Result" ]
                , div [ class "level" ]
                    [ div [ class "level-item has-text-centered" ]
                        [ div []
                            [ p [ class "heading" ] [ text "Impact Score" ]
                            , p [ class "title is-2" ]
                                [ text (Format.formatScientific result.lrScore)
                                ]
                            , p [ class "subtitle is-5" ] [ text result.lrUnit ]
                            ]
                        ]
                    ]
                , div [ class "columns" ]
                    [ div [ class "column" ]
                        [ div [ class "notification is-success is-light" ]
                            [ p []
                                [ strong [] [ text "Mapped flows: " ]
                                , text (String.fromInt result.lrMappedFlows)
                                ]
                            ]
                        ]
                    , div [ class "column" ]
                        [ div
                            [ class
                                (if result.lrUnmappedFlows > 0 then
                                    "notification is-warning is-light"

                                 else
                                    "notification is-success is-light"
                                )
                            ]
                            [ p []
                                [ strong [] [ text "Unmapped flows: " ]
                                , text (String.fromInt result.lrUnmappedFlows)
                                ]
                            ]
                        ]
                    ]
                ]


viewMappingStatus : Maybe MappingStatus -> Html msg
viewMappingStatus maybeMappingStatus =
    case maybeMappingStatus of
        Nothing ->
            text ""

        Just status ->
            div [ class "box" ]
                [ h3 [ class "title is-5" ] [ text "Flow Mapping Details" ]
                , div [ class "columns" ]
                    [ div [ class "column" ]
                        [ viewMappingBar status
                        ]
                    ]
                , div [ class "columns is-multiline" ]
                    [ viewMappingStat "fas fa-fingerprint" "By UUID" status.mstMappedByUUID "is-info"
                    , viewMappingStat "fas fa-font" "By Name" status.mstMappedByName "is-success"
                    , viewMappingStat "fas fa-exchange-alt" "By Synonym" status.mstMappedBySynonym "is-link"
                    , viewMappingStat "fas fa-question-circle" "Unmapped" status.mstUnmapped "is-warning"
                    ]
                , if List.length status.mstUnmappedFlows > 0 then
                    viewUnmappedFlows status.mstUnmappedFlows

                  else
                    text ""
                ]


viewMappingBar : MappingStatus -> Html msg
viewMappingBar status =
    let
        total =
            toFloat status.mstTotalFactors

        uuidPct =
            toFloat status.mstMappedByUUID / total * 100

        namePct =
            toFloat status.mstMappedByName / total * 100

        synPct =
            toFloat status.mstMappedBySynonym / total * 100

        unmappedPct =
            toFloat status.mstUnmapped / total * 100
    in
    div []
        [ p [ class "mb-2" ]
            [ strong [] [ text "Coverage: " ]
            , text (String.fromFloat (toFloat (round (status.mstCoverage * 10)) / 10) ++ "%")
            , text (" (" ++ String.fromInt (status.mstTotalFactors - status.mstUnmapped) ++ "/" ++ String.fromInt status.mstTotalFactors ++ " flows)")
            ]
        , div [ class "progress-wrapper" ]
            [ progress
                [ class "progress is-success"
                , value (String.fromFloat status.mstCoverage)
                , attribute "max" "100"
                ]
                []
            ]
        ]


viewMappingStat : String -> String -> Int -> String -> Html msg
viewMappingStat icon label count colorClass =
    div [ class "column is-3" ]
        [ div [ class ("notification " ++ colorClass ++ " is-light") ]
            [ div [ class "level" ]
                [ div [ class "level-left" ]
                    [ div [ class "level-item" ]
                        [ span [ class "icon" ] [ i [ class icon ] [] ]
                        ]
                    , div [ class "level-item" ]
                        [ span [] [ text label ]
                        ]
                    ]
                , div [ class "level-right" ]
                    [ div [ class "level-item" ]
                        [ strong [] [ text (String.fromInt count) ]
                        ]
                    ]
                ]
            ]
        ]


viewUnmappedFlows : List Models.LCIA.UnmappedFlow -> Html msg
viewUnmappedFlows unmappedFlows =
    details [ class "mt-4" ]
        [ summary [ class "has-text-weight-semibold", style "cursor" "pointer" ]
            [ text ("Show unmapped flows (" ++ String.fromInt (List.length unmappedFlows) ++ ")") ]
        , div [ class "table-container mt-2" ]
            [ table [ class "table is-striped is-hoverable is-fullwidth is-narrow" ]
                [ thead []
                    [ tr []
                        [ th [] [ text "Flow Name" ]
                        , th [] [ text "Direction" ]
                        , th [] [ text "UUID" ]
                        ]
                    ]
                , tbody []
                    (List.map viewUnmappedFlowRow unmappedFlows)
                ]
            ]
        ]


viewUnmappedFlowRow : Models.LCIA.UnmappedFlow -> Html msg
viewUnmappedFlowRow flow =
    tr []
        [ td [] [ text flow.ufaFlowName ]
        , td []
            [ span
                [ class
                    (if flow.ufaDirection == "Input" then
                        "tag is-info"

                     else
                        "tag is-warning"
                    )
                ]
                [ text flow.ufaDirection ]
            ]
        , td [] [ small [ class "has-text-grey" ] [ text flow.ufaFlowRef ] ]
        ]


{-| Shared navbar component for all pages
-}
viewPageNavbar : String -> Maybe ( String, String ) -> Html msg
viewPageNavbar title maybeActivity =
    nav [ class "navbar is-light" ]
        [ div [ class "navbar-brand" ]
            [ div [ class "navbar-item" ]
                [ h1 [ class "title is-4" ] [ text title ]
                ]
            ]
        , div [ class "navbar-menu is-active" ]
            [ div [ class "navbar-end" ]
                (case maybeActivity of
                    Just ( name, location ) ->
                        [ div [ class "navbar-item" ]
                            [ span [ class "title is-4" ] [ text name ]
                            ]
                        , div [ class "navbar-item" ]
                            [ span [ class "subtitle is-6" ] [ text location ]
                            ]
                        ]

                    Nothing ->
                        [ div [ class "navbar-item" ]
                            [ span [ class "subtitle is-6" ] [ text "Loading..." ]
                            ]
                        ]
                )
            ]
        ]
