module Views.DetailsView exposing (Model, Msg(..), init, view, viewContent, viewActivityInfoContent, viewUpstreamExchanges, viewBiosphereExchanges)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models.Activity exposing (ActivityEdge, ActivityExchange, ActivityInfo, ActivityNode, ActivityTree, EdgeType(..), ExchangeType(..), NodeType(..))
import Utils.Format as Format


type alias Model =
    { activityTree : ActivityTree
    , currentActivityId : String
    }


type Msg
    = NavigateToActivity String
    | NavigateBack


init : ActivityTree -> String -> Model
init tree currentActivityId =
    { activityTree = tree
    , currentActivityId = currentActivityId
    }


view : Model -> Bool -> Html Msg
view model canNavigateBack =
    let
        currentNode =
            Dict.get model.currentActivityId model.activityTree.nodes

        activityName =
            currentNode
                |> Maybe.map .name
                |> Maybe.withDefault "Unknown Activity"

        activityLocation =
            currentNode
                |> Maybe.map .location
                |> Maybe.withDefault ""

        technosphereEdges =
            model.activityTree.edges
                |> List.filter (\edge -> edge.from == model.currentActivityId && edge.edgeType == TechnosphereEdgeType)

        biosphereEmissions =
            model.activityTree.edges
                |> List.filter (\edge -> edge.from == model.currentActivityId && edge.edgeType == BiosphereEmissionEdgeType)

        biosphereResources =
            model.activityTree.edges
                |> List.filter (\edge -> edge.from == model.currentActivityId && edge.edgeType == BiosphereResourceEdgeType)
    in
    div [ class "details-page" ]
        [ viewHeader activityName activityLocation canNavigateBack
        , viewTablesContent model.activityTree.nodes technosphereEdges biosphereResources biosphereEmissions
        ]


viewContent : Model -> Html Msg
viewContent model =
    let
        technosphereEdges =
            model.activityTree.edges
                |> List.filter (\edge -> edge.from == model.currentActivityId && edge.edgeType == TechnosphereEdgeType)

        biosphereEmissions =
            model.activityTree.edges
                |> List.filter (\edge -> edge.from == model.currentActivityId && edge.edgeType == BiosphereEmissionEdgeType)

        biosphereResources =
            model.activityTree.edges
                |> List.filter (\edge -> edge.from == model.currentActivityId && edge.edgeType == BiosphereResourceEdgeType)
    in
    viewTablesContent model.activityTree.nodes technosphereEdges biosphereResources biosphereEmissions


{-| Render table content from ActivityInfo (from /api/v1/activity/{id} endpoint)
-}
viewActivityInfoContent : ActivityInfo -> (String -> msg) -> Html msg
viewActivityInfoContent activityInfo onNavigate =
    let
        -- Filter out reference products from upstream activities (they are outputs, not inputs)
        technosphereExchanges =
            activityInfo.exchanges
                |> List.filter (\ex -> ex.exchangeType == TechnosphereExchangeType && not ex.isReference)

        biosphereEmissions =
            activityInfo.exchanges
                |> List.filter (\ex -> ex.exchangeType == BiosphereEmissionType)

        biosphereResources =
            activityInfo.exchanges
                |> List.filter (\ex -> ex.exchangeType == BiosphereResourceType)
    in
    div []
        [ viewUpstreamActivitiesFromInfo technosphereExchanges onNavigate
        , viewConsumptionsFromInfo biosphereResources
        , viewEmissionsFromInfo biosphereEmissions
        ]


viewUpstreamActivitiesFromInfo : List ActivityExchange -> (String -> msg) -> Html msg
viewUpstreamActivitiesFromInfo exchanges onNavigate =
    div [ class "box" ]
        [ h2 [ class "title is-5" ] [ text "Upstream Activities" ]
        , if List.isEmpty exchanges then
            p [ class "has-text-grey" ] [ text "No direct upstream activities" ]

          else
            div [ class "table-container" ]
                [ table [ class "table is-striped is-hoverable is-fullwidth" ]
                    [ thead []
                        [ tr []
                            [ th [] [ text "Activity Name" ]
                            , th [] [ text "Location" ]
                            , th [ class "has-text-right" ] [ text "Quantity" ]
                            , th [] [ text "Unit" ]
                            ]
                        ]
                    , tbody []
                        (exchanges
                            |> List.map (viewUpstreamActivityFromInfoRow onNavigate)
                        )
                    ]
                ]
        ]


viewUpstreamActivityFromInfoRow : (String -> msg) -> ActivityExchange -> Html msg
viewUpstreamActivityFromInfoRow onNavigate exchange =
    let
        displayName =
            exchange.targetActivity
                |> Maybe.withDefault exchange.flowName

        displayLocation =
            exchange.targetLocation
                |> Maybe.withDefault ""

        isClickable =
            exchange.activityLinkId /= Nothing
    in
    tr
        (if isClickable then
            [ class "is-clickable"
            , style "cursor" "pointer"
            , onClick (onNavigate (exchange.activityLinkId |> Maybe.withDefault ""))
            ]

         else
            []
        )
        [ td []
            [ if isClickable then
                span [ class "has-text-link" ] [ text displayName ]

              else
                text displayName
            ]
        , td [] [ text displayLocation ]
        , td [ class "has-text-right" ]
            [ text (Format.formatScientific exchange.amount) ]
        , td [] [ text exchange.unitName ]
        ]


viewConsumptionsFromInfo : List ActivityExchange -> Html msg
viewConsumptionsFromInfo exchanges =
    let
        groupedByCompartment =
            groupExchangesByCompartment exchanges
    in
    div [ class "box" ]
        [ h2 [ class "title is-5" ] [ text "Consumptions from the Environment" ]
        , if List.isEmpty exchanges then
            p [ class "has-text-grey" ] [ text "No consumptions from the environment" ]

          else
            div []
                [ viewExchangeCompartmentSection "Soil" groupedByCompartment.soil
                , viewExchangeCompartmentSection "Water" groupedByCompartment.water
                , viewExchangeCompartmentSection "Air" groupedByCompartment.air
                , viewExchangeCompartmentSection "Other" groupedByCompartment.other
                ]
        ]


viewEmissionsFromInfo : List ActivityExchange -> Html msg
viewEmissionsFromInfo exchanges =
    let
        groupedByCompartment =
            groupExchangesByCompartment exchanges
    in
    div [ class "box" ]
        [ h2 [ class "title is-5" ] [ text "Emissions to the Environment" ]
        , if List.isEmpty exchanges then
            p [ class "has-text-grey" ] [ text "No emissions to the environment" ]

          else
            div []
                [ viewExchangeCompartmentSection "Soil" groupedByCompartment.soil
                , viewExchangeCompartmentSection "Water" groupedByCompartment.water
                , viewExchangeCompartmentSection "Air" groupedByCompartment.air
                , viewExchangeCompartmentSection "Other" groupedByCompartment.other
                ]
        ]


type alias ExchangeCompartmentGroups =
    { soil : List ActivityExchange
    , water : List ActivityExchange
    , air : List ActivityExchange
    , other : List ActivityExchange
    }


groupExchangesByCompartment : List ActivityExchange -> ExchangeCompartmentGroups
groupExchangesByCompartment exchanges =
    let
        categorizeExchange exchange groups =
            let
                compartment =
                    String.toLower exchange.flowCategory
            in
            if String.contains "soil" compartment then
                { groups | soil = exchange :: groups.soil }

            else if String.contains "water" compartment || String.contains "ocean" compartment then
                { groups | water = exchange :: groups.water }

            else if String.contains "air" compartment then
                { groups | air = exchange :: groups.air }

            else
                { groups | other = exchange :: groups.other }
    in
    List.foldl categorizeExchange
        { soil = [], water = [], air = [], other = [] }
        exchanges
        |> (\groups ->
                { soil = List.sortBy (\e -> -(abs e.amount)) groups.soil
                , water = List.sortBy (\e -> -(abs e.amount)) groups.water
                , air = List.sortBy (\e -> -(abs e.amount)) groups.air
                , other = List.sortBy (\e -> -(abs e.amount)) groups.other
                }
           )


viewExchangeCompartmentSection : String -> List ActivityExchange -> Html msg
viewExchangeCompartmentSection compartmentName exchanges =
    if List.isEmpty exchanges then
        text ""

    else
        div [ class "mt-4" ]
            [ h3 [ class "title is-6 has-text-grey" ] [ text compartmentName ]
            , div [ class "table-container" ]
                [ table [ class "table is-striped is-fullwidth" ]
                    [ thead []
                        [ tr []
                            [ th [] [ text "Flow" ]
                            , th [ class "has-text-right" ] [ text "Quantity" ]
                            , th [] [ text "Unit" ]
                            ]
                        ]
                    , tbody []
                        (exchanges
                            |> List.map viewExchangeFlowRow
                        )
                    ]
                ]
            ]


viewExchangeFlowRow : ActivityExchange -> Html msg
viewExchangeFlowRow exchange =
    tr []
        [ td [] [ text exchange.flowName ]
        , td [ class "has-text-right" ]
            [ text (Format.formatScientific exchange.amount) ]
        , td [] [ text exchange.unitName ]
        ]


{-| View upstream exchanges without box wrapper (for use in tabs)
-}
viewUpstreamExchanges : List ActivityExchange -> (String -> msg) -> Html msg
viewUpstreamExchanges exchanges onNavigate =
    if List.isEmpty exchanges then
        p [ class "has-text-grey" ] [ text "No direct upstream activities" ]

    else
        div [ class "table-container" ]
            [ table [ class "table is-striped is-hoverable is-fullwidth" ]
                [ thead []
                    [ tr []
                        [ th [] [ text "Activity Name" ]
                        , th [] [ text "Location" ]
                        , th [ class "has-text-right" ] [ text "Quantity" ]
                        , th [] [ text "Unit" ]
                        ]
                    ]
                , tbody []
                    (exchanges
                        |> List.map (viewUpstreamActivityFromInfoRow onNavigate)
                    )
                ]
            ]


{-| View biosphere exchanges (emissions or consumptions) without box wrapper (for use in tabs)
-}
viewBiosphereExchanges : List ActivityExchange -> Html msg
viewBiosphereExchanges exchanges =
    if List.isEmpty exchanges then
        p [ class "has-text-grey" ] [ text "No exchanges" ]

    else
        let
            groupedByCompartment =
                groupExchangesByCompartment exchanges

            -- Only show compartments that have data
            compartments =
                [ ( "Air", groupedByCompartment.air )
                , ( "Water", groupedByCompartment.water )
                , ( "Soil", groupedByCompartment.soil )
                , ( "Other", groupedByCompartment.other )
                ]
                    |> List.filter (\( _, items ) -> not (List.isEmpty items))
        in
        div [ class "columns is-desktop is-multiline" ]
            (compartments
                |> List.map
                    (\( name, items ) ->
                        div [ class "column" ]
                            [ viewExchangeCompartmentColumn name items ]
                    )
            )


viewExchangeCompartmentColumn : String -> List ActivityExchange -> Html msg
viewExchangeCompartmentColumn compartmentName exchanges =
    div []
        [ h3 [ class "title is-6 has-text-grey mb-2" ] [ text compartmentName ]
        , div [ class "table-container" ]
            [ table [ class "table is-striped is-fullwidth is-narrow" ]
                [ thead []
                    [ tr []
                        [ th [] [ text "Flow" ]
                        , th [ class "has-text-right" ] [ text "Quantity" ]
                        , th [] [ text "Unit" ]
                        ]
                    ]
                , tbody []
                    (exchanges
                        |> List.map viewExchangeFlowRow
                    )
                ]
            ]
        ]


viewTablesContent : Dict String ActivityNode -> List ActivityEdge -> List ActivityEdge -> List ActivityEdge -> Html Msg
viewTablesContent nodes technosphereEdges biosphereResources biosphereEmissions =
    div []
        [ viewUpstreamActivitiesTable nodes technosphereEdges
        , viewConsumptionsTable nodes biosphereResources
        , viewEmissionsTable nodes biosphereEmissions
        ]


viewHeader : String -> String -> Bool -> Html Msg
viewHeader activityName location canNavigateBack =
    div [ class "box" ]
        [ div [ class "level" ]
            [ div [ class "level-left" ]
                ([ if canNavigateBack then
                    div [ class "level-item" ]
                        [ button
                            [ class "button is-primary"
                            , onClick NavigateBack
                            ]
                            [ span [ class "icon" ]
                                [ i [ class "fas fa-arrow-left" ] []
                                ]
                            , span [] [ text "Previous Activity" ]
                            ]
                        ]

                   else
                    text ""
                 , div [ class "level-item" ]
                    [ div []
                        [ h1 [ class "title is-4" ] [ text activityName ]
                        , p [ class "subtitle is-6 has-text-grey" ] [ text location ]
                        ]
                    ]
                 ]
                )
            ]
        ]


viewUpstreamActivitiesTable : Dict String ActivityNode -> List ActivityEdge -> Html Msg
viewUpstreamActivitiesTable nodes edges =
    div [ class "box" ]
        [ h2 [ class "title is-5" ] [ text "Upstream Activities" ]
        , if List.isEmpty edges then
            p [ class "has-text-grey" ] [ text "No direct upstream activities" ]

          else
            div [ class "table-container" ]
                [ table [ class "table is-striped is-hoverable is-fullwidth" ]
                    [ thead []
                        [ tr []
                            [ th [] [ text "Activity Name" ]
                            , th [] [ text "Location" ]
                            , th [ class "has-text-right" ] [ text "Quantity" ]
                            , th [] [ text "Unit" ]
                            ]
                        ]
                    , tbody []
                        (edges
                            |> List.map (viewUpstreamActivityRow nodes)
                        )
                    ]
                ]
        ]


viewUpstreamActivityRow : Dict String ActivityNode -> ActivityEdge -> Html Msg
viewUpstreamActivityRow nodes edge =
    let
        targetNode =
            Dict.get edge.to nodes

        nodeName =
            targetNode
                |> Maybe.map .name
                |> Maybe.withDefault edge.flow.name

        nodeLocation =
            targetNode
                |> Maybe.map .location
                |> Maybe.withDefault ""

        nodeDescription =
            targetNode
                |> Maybe.map (.description >> String.join " ")
                |> Maybe.withDefault ""
    in
    tr
        [ class "is-clickable"
        , onClick (NavigateToActivity edge.to)
        , title nodeDescription
        , style "cursor" "pointer"
        ]
        [ td []
            [ span [ class "has-text-link" ]
                [ text nodeName ]
            ]
        , td [] [ text nodeLocation ]
        , td [ class "has-text-right" ]
            [ text (Format.formatScientific edge.quantity) ]
        , td [] [ text edge.unit ]
        ]


viewConsumptionsTable : Dict String ActivityNode -> List ActivityEdge -> Html Msg
viewConsumptionsTable nodes edges =
    let
        groupedByCompartment =
            groupByCompartment nodes edges
    in
    div [ class "box" ]
        [ h2 [ class "title is-5" ] [ text "Consumptions from the Environment" ]
        , if List.isEmpty edges then
            p [ class "has-text-grey" ] [ text "No consumptions from the environment" ]

          else
            div []
                [ viewCompartmentSection "Soil" groupedByCompartment.soil
                , viewCompartmentSection "Water" groupedByCompartment.water
                , viewCompartmentSection "Air" groupedByCompartment.air
                , viewCompartmentSection "Other" groupedByCompartment.other
                ]
        ]


viewEmissionsTable : Dict String ActivityNode -> List ActivityEdge -> Html Msg
viewEmissionsTable nodes edges =
    let
        groupedByCompartment =
            groupByCompartment nodes edges
    in
    div [ class "box" ]
        [ h2 [ class "title is-5" ] [ text "Emissions to the Environment" ]
        , if List.isEmpty edges then
            p [ class "has-text-grey" ] [ text "No emissions to the environment" ]

          else
            div []
                [ viewCompartmentSection "Soil" groupedByCompartment.soil
                , viewCompartmentSection "Water" groupedByCompartment.water
                , viewCompartmentSection "Air" groupedByCompartment.air
                , viewCompartmentSection "Other" groupedByCompartment.other
                ]
        ]


type alias CompartmentGroups =
    { soil : List ActivityEdge
    , water : List ActivityEdge
    , air : List ActivityEdge
    , other : List ActivityEdge
    }


groupByCompartment : Dict String ActivityNode -> List ActivityEdge -> CompartmentGroups
groupByCompartment nodes edges =
    let
        getCompartment edge =
            Dict.get edge.to nodes
                |> Maybe.andThen .compartment
                |> Maybe.withDefault ""

        categorizeEdge edge groups =
            let
                compartment =
                    getCompartment edge |> String.toLower
            in
            if String.contains "soil" compartment then
                { groups | soil = edge :: groups.soil }

            else if String.contains "water" compartment || String.contains "ocean" compartment then
                { groups | water = edge :: groups.water }

            else if String.contains "air" compartment then
                { groups | air = edge :: groups.air }

            else
                { groups | other = edge :: groups.other }
    in
    List.foldl categorizeEdge
        { soil = [], water = [], air = [], other = [] }
        edges
        |> (\groups ->
                { soil = List.sortBy (\e -> -(abs e.quantity)) groups.soil
                , water = List.sortBy (\e -> -(abs e.quantity)) groups.water
                , air = List.sortBy (\e -> -(abs e.quantity)) groups.air
                , other = List.sortBy (\e -> -(abs e.quantity)) groups.other
                }
           )


viewCompartmentSection : String -> List ActivityEdge -> Html Msg
viewCompartmentSection compartmentName edges =
    if List.isEmpty edges then
        text ""

    else
        div [ class "mt-4" ]
            [ h3 [ class "title is-6 has-text-grey" ] [ text compartmentName ]
            , div [ class "table-container" ]
                [ table [ class "table is-striped is-fullwidth" ]
                    [ thead []
                        [ tr []
                            [ th [] [ text "Flow" ]
                            , th [ class "has-text-right" ] [ text "Quantity" ]
                            , th [] [ text "Unit" ]
                            ]
                        ]
                    , tbody []
                        (edges
                            |> List.map viewBiosphereFlowRow
                        )
                    ]
                ]
            ]


viewBiosphereFlowRow : ActivityEdge -> Html Msg
viewBiosphereFlowRow edge =
    tr []
        [ td [] [ text edge.flow.name ]
        , td [ class "has-text-right" ]
            [ text (Format.formatScientific edge.quantity) ]
        , td [] [ text edge.unit ]
        ]
