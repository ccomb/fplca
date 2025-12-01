module Views.DetailsView exposing (Model, Msg(..), init, view)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models.Activity exposing (ActivityEdge, ActivityNode, ActivityTree, EdgeType(..), NodeType(..))
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
        , viewUpstreamActivitiesTable model.activityTree.nodes technosphereEdges
        , viewConsumptionsTable model.activityTree.nodes biosphereResources
        , viewEmissionsTable model.activityTree.nodes biosphereEmissions
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
