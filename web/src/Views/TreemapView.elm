module Views.TreemapView exposing (view)

import Color exposing (Color)
import Dict exposing (Dict)
import Hierarchy
import Html exposing (Html, div, h2, p, text)
import Html.Attributes exposing (class)
import Models.Activity exposing (ActivityEdge, ActivityNode, ActivityTree, EdgeType(..), NodeType(..))
import Scale
import Scale.Color
import Svg exposing (Svg, g, rect, svg, text_, title)
import Svg.Attributes as SvgA exposing (fill, fontSize, height, stroke, strokeWidth, textAnchor, viewBox, width, x, y)
import Svg.Events
import Tree exposing (Tree)
import Utils.Format as Format


type alias TreemapNode =
    { name : String
    , location : String
    , value : Float
    , unit : String
    , processId : String
    , category : String -- For coloring
    }


view : ActivityTree -> String -> (String -> msg) -> Html msg
view activityTree currentActivityId onNavigate =
    let
        allTechnosphereEdges =
            activityTree.edges
                |> List.filter (\edge -> edge.edgeType == TechnosphereEdgeType)

        biosphereEmissions =
            activityTree.edges
                |> List.filter (\edge -> edge.from == currentActivityId && edge.edgeType == BiosphereEmissionEdgeType)

        biosphereResources =
            activityTree.edges
                |> List.filter (\edge -> edge.from == currentActivityId && edge.edgeType == BiosphereResourceEdgeType)
    in
    div []
        [ viewUpstreamTreemap activityTree.nodes allTechnosphereEdges currentActivityId onNavigate
        , viewBiosphereTreemaps activityTree.nodes biosphereResources biosphereEmissions
        ]


viewUpstreamTreemap : Dict String ActivityNode -> List ActivityEdge -> String -> (String -> msg) -> Html msg
viewUpstreamTreemap nodes allEdges currentActivityId onNavigate =
    let
        directEdges =
            allEdges |> List.filter (\edge -> edge.from == currentActivityId)

        -- Group direct edges by unit
        edgesByUnit =
            directEdges
                |> List.foldl
                    (\edge groups ->
                        Dict.update edge.unit
                            (\maybeList ->
                                case maybeList of
                                    Just list ->
                                        Just (edge :: list)

                                    Nothing ->
                                        Just [ edge ]
                            )
                            groups
                    )
                    Dict.empty
                |> Dict.toList
                |> List.sortBy (\( unit, edges ) -> -(List.length edges))
    in
    div [ class "box" ]
        [ h2 [ class "title is-5" ] [ text "Upstream Activities" ]
        , if List.isEmpty directEdges then
            p [ class "has-text-grey" ] [ text "No upstream activities" ]

          else
            div []
                (edgesByUnit
                    |> List.map
                        (\( unit, unitEdges ) ->
                            viewUnitTreemap nodes allEdges currentActivityId unit unitEdges onNavigate
                        )
                )
        ]


viewUnitTreemap : Dict String ActivityNode -> List ActivityEdge -> String -> String -> List ActivityEdge -> (String -> msg) -> Html msg
viewUnitTreemap nodes allEdges currentActivityId unit unitEdges onNavigate =
    let
        treemapData =
            buildUpstreamTreeForUnit nodes allEdges currentActivityId unitEdges

        treemapWidth =
            800

        -- Scale height based on number of items
        itemCount =
            List.length unitEdges

        treemapHeight =
            max 150 (min 400 (itemCount * 40))
    in
    div [ class "mt-4" ]
        [ h3 [ class "title is-6 has-text-grey" ] [ text ("Unit: " ++ unit) ]
        , div [ class "treemap-container" ]
            [ svg
                [ viewBox ("0 0 " ++ String.fromInt treemapWidth ++ " " ++ String.fromInt treemapHeight)
                , width "100%"
                , height (String.fromInt treemapHeight)
                , SvgA.style "max-width: 100%"
                ]
                (renderUpstreamTreemap treemapWidth treemapHeight treemapData onNavigate)
            ]
        ]


viewBiosphereTreemaps : Dict String ActivityNode -> List ActivityEdge -> List ActivityEdge -> Html msg
viewBiosphereTreemaps nodes resources emissions =
    let
        hasResources =
            not (List.isEmpty resources)

        hasEmissions =
            not (List.isEmpty emissions)
    in
    if not hasResources && not hasEmissions then
        text ""

    else
        div [ class "box" ]
            [ h2 [ class "title is-5" ] [ text "Biosphere Flows" ]
            , div [ class "columns" ]
                [ div [ class "column" ]
                    [ h3 [ class "title is-6" ] [ text "Consumptions" ]
                    , if hasResources then
                        viewBiosphereTreemap nodes resources "resource"

                      else
                        p [ class "has-text-grey" ] [ text "No consumptions" ]
                    ]
                , div [ class "column" ]
                    [ h3 [ class "title is-6" ] [ text "Emissions" ]
                    , if hasEmissions then
                        viewBiosphereTreemap nodes emissions "emission"

                      else
                        p [ class "has-text-grey" ] [ text "No emissions" ]
                    ]
                ]
            ]


viewBiosphereTreemap : Dict String ActivityNode -> List ActivityEdge -> String -> Html msg
viewBiosphereTreemap nodes edges category =
    let
        treemapData =
            buildBiosphereTree nodes edges category

        treemapWidth =
            400

        treemapHeight =
            300
    in
    div [ class "treemap-container" ]
        [ svg
            [ viewBox ("0 0 " ++ String.fromInt treemapWidth ++ " " ++ String.fromInt treemapHeight)
            , width "100%"
            , height (String.fromInt treemapHeight)
            , SvgA.style "max-width: 100%"
            ]
            (renderBiosphereTreemap treemapWidth treemapHeight treemapData)
        ]


h3 : List (Html.Attribute msg) -> List (Html msg) -> Html msg
h3 =
    Html.node "h3"


buildUpstreamTreeForUnit : Dict String ActivityNode -> List ActivityEdge -> String -> List ActivityEdge -> Tree TreemapNode
buildUpstreamTreeForUnit nodes allEdges currentActivityId unitEdges =
    let
        -- Get the unit from the first edge (all edges in unitEdges have the same unit)
        unit =
            List.head unitEdges
                |> Maybe.map .unit
                |> Maybe.withDefault ""

        rootNode =
            { name = "Upstream (" ++ unit ++ ")"
            , location = ""
            , value = 0
            , unit = unit
            , processId = ""
            , category = "root"
            }

        -- Build children with their own children (depth 2), filtering grandchildren by same unit
        children =
            unitEdges
                |> List.map
                    (\edge ->
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

                            childNode =
                                { name = nodeName
                                , location = nodeLocation
                                , value = abs edge.quantity
                                , unit = edge.unit
                                , processId = edge.to
                                , category = getCategoryFromName nodeName
                                }

                            -- Get grandchildren (depth 2) - filter by same unit
                            grandchildEdges =
                                allEdges
                                    |> List.filter (\e -> e.from == edge.to && e.unit == unit)

                            grandchildren =
                                grandchildEdges
                                    |> List.map
                                        (\grandEdge ->
                                            let
                                                grandTargetNode =
                                                    Dict.get grandEdge.to nodes

                                                grandNodeName =
                                                    grandTargetNode
                                                        |> Maybe.map .name
                                                        |> Maybe.withDefault grandEdge.flow.name

                                                grandNodeLocation =
                                                    grandTargetNode
                                                        |> Maybe.map .location
                                                        |> Maybe.withDefault ""
                                            in
                                            Tree.singleton
                                                { name = grandNodeName
                                                , location = grandNodeLocation
                                                , value = abs grandEdge.quantity
                                                , unit = grandEdge.unit
                                                , processId = grandEdge.to
                                                , category = getCategoryFromName grandNodeName
                                                }
                                        )
                        in
                        Tree.tree childNode grandchildren
                    )
    in
    Tree.tree rootNode children


buildUpstreamTree : Dict String ActivityNode -> List ActivityEdge -> String -> Tree TreemapNode
buildUpstreamTree nodes allEdges currentActivityId =
    let
        rootNode =
            { name = "Upstream"
            , location = ""
            , value = 0
            , unit = ""
            , processId = ""
            , category = "root"
            }

        -- Get direct children (depth 1)
        directEdges =
            allEdges |> List.filter (\edge -> edge.from == currentActivityId)

        -- Build children with their own children (depth 2)
        children =
            directEdges
                |> List.map
                    (\edge ->
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

                            childNode =
                                { name = nodeName
                                , location = nodeLocation
                                , value = abs edge.quantity
                                , unit = edge.unit
                                , processId = edge.to
                                , category = getCategoryFromName nodeName
                                }

                            -- Get grandchildren (depth 2)
                            grandchildEdges =
                                allEdges |> List.filter (\e -> e.from == edge.to)

                            grandchildren =
                                grandchildEdges
                                    |> List.map
                                        (\grandEdge ->
                                            let
                                                grandTargetNode =
                                                    Dict.get grandEdge.to nodes

                                                grandNodeName =
                                                    grandTargetNode
                                                        |> Maybe.map .name
                                                        |> Maybe.withDefault grandEdge.flow.name

                                                grandNodeLocation =
                                                    grandTargetNode
                                                        |> Maybe.map .location
                                                        |> Maybe.withDefault ""
                                            in
                                            Tree.singleton
                                                { name = grandNodeName
                                                , location = grandNodeLocation
                                                , value = abs grandEdge.quantity
                                                , unit = grandEdge.unit
                                                , processId = grandEdge.to
                                                , category = getCategoryFromName grandNodeName
                                                }
                                        )
                        in
                        Tree.tree childNode grandchildren
                    )
    in
    Tree.tree rootNode children


buildBiosphereTree : Dict String ActivityNode -> List ActivityEdge -> String -> Tree TreemapNode
buildBiosphereTree nodes edges category =
    let
        rootNode =
            { name = category
            , location = ""
            , value = 0
            , unit = ""
            , processId = ""
            , category = category
            }

        -- Group by compartment
        groupedEdges =
            edges
                |> List.foldl
                    (\edge groups ->
                        let
                            compartment =
                                Dict.get edge.to nodes
                                    |> Maybe.andThen .compartment
                                    |> Maybe.withDefault "Other"
                                    |> getMainCompartment
                        in
                        Dict.update compartment
                            (\maybeList ->
                                case maybeList of
                                    Just list ->
                                        Just (edge :: list)

                                    Nothing ->
                                        Just [ edge ]
                            )
                            groups
                    )
                    Dict.empty

        compartmentTrees =
            groupedEdges
                |> Dict.toList
                |> List.map
                    (\( compartmentName, compartmentEdges ) ->
                        let
                            compartmentNode =
                                { name = compartmentName
                                , location = ""
                                , value = 0
                                , unit = ""
                                , processId = ""
                                , category = String.toLower compartmentName
                                }

                            flowChildren =
                                compartmentEdges
                                    |> List.map
                                        (\edge ->
                                            Tree.singleton
                                                { name = edge.flow.name
                                                , location = ""
                                                , value = abs edge.quantity
                                                , unit = edge.unit
                                                , processId = edge.to
                                                , category = String.toLower compartmentName
                                                }
                                        )
                        in
                        Tree.tree compartmentNode flowChildren
                    )
    in
    Tree.tree rootNode compartmentTrees


getMainCompartment : String -> String
getMainCompartment compartment =
    let
        lower =
            String.toLower compartment
    in
    if String.contains "air" lower then
        "Air"

    else if String.contains "water" lower || String.contains "ocean" lower then
        "Water"

    else if String.contains "soil" lower then
        "Soil"

    else
        "Other"


getCategoryFromName : String -> String
getCategoryFromName name =
    let
        lower =
            String.toLower name
    in
    if String.contains "electricity" lower || String.contains "power" lower then
        "energy"

    else if String.contains "transport" lower || String.contains "lorry" lower || String.contains "freight" lower then
        "transport"

    else if String.contains "water" lower then
        "water"

    else if String.contains "heat" lower || String.contains "steam" lower then
        "heat"

    else if String.contains "treatment" lower || String.contains "waste" lower then
        "waste"

    else
        "other"


renderUpstreamTreemap : Int -> Int -> Tree TreemapNode -> (String -> msg) -> List (Svg msg)
renderUpstreamTreemap treemapWidth treemapHeight tree onNavigate =
    let
        layoutTree =
            tree
                |> Tree.sortWith (\_ a b -> compare (Tree.label b).value (Tree.label a).value)
                |> Hierarchy.treemap
                    [ Hierarchy.size (toFloat treemapWidth) (toFloat treemapHeight)
                    , Hierarchy.padding (always 2)
                    , Hierarchy.tile Hierarchy.squarify
                    ]
                    .value

        -- Get all leaf nodes
        leafNodes =
            getLeafNodes layoutTree

        categoryColors =
            [ ( "energy", Color.rgb255 255 193 7 )
            , ( "transport", Color.rgb255 33 150 243 )
            , ( "water", Color.rgb255 3 169 244 )
            , ( "heat", Color.rgb255 255 87 34 )
            , ( "waste", Color.rgb255 139 195 74 )
            , ( "other", Color.rgb255 156 39 176 )
            ]
                |> Dict.fromList
    in
    leafNodes
        |> List.map
            (\node ->
                let
                    bounds =
                        Tree.label node

                    color =
                        Dict.get bounds.category categoryColors
                            |> Maybe.withDefault (Color.rgb255 158 158 158)

                    showLabel =
                        bounds.w > 40 && bounds.h > 20
                in
                g
                    [ Svg.Events.onClick (onNavigate bounds.processId)
                    , SvgA.style "cursor: pointer"
                    ]
                    [ rect
                        [ x (String.fromFloat bounds.x)
                        , y (String.fromFloat bounds.y)
                        , width (String.fromFloat bounds.w)
                        , height (String.fromFloat bounds.h)
                        , fill (Color.toCssString color)
                        , stroke "white"
                        , strokeWidth "1"
                        ]
                        [ title [] [ Svg.text (bounds.name ++ " (" ++ bounds.location ++ ")\n" ++ Format.formatScientific bounds.value ++ " " ++ bounds.unit) ]
                        ]
                    , if showLabel then
                        text_
                            [ x (String.fromFloat (bounds.x + 4))
                            , y (String.fromFloat (bounds.y + 14))
                            , fontSize "11"
                            , fill "white"
                            , SvgA.style "pointer-events: none"
                            ]
                            [ Svg.text (truncateLabel (floor bounds.w - 8) bounds.name) ]

                      else
                        Svg.text ""
                    ]
            )


renderBiosphereTreemap : Int -> Int -> Tree TreemapNode -> List (Svg msg)
renderBiosphereTreemap treemapWidth treemapHeight tree =
    let
        layoutTree =
            tree
                |> Tree.sortWith (\_ a b -> compare (Tree.label b).value (Tree.label a).value)
                |> Hierarchy.treemap
                    [ Hierarchy.size (toFloat treemapWidth) (toFloat treemapHeight)
                    , Hierarchy.padding (always 2)
                    , Hierarchy.tile Hierarchy.squarify
                    ]
                    .value

        leafNodes =
            getLeafNodes layoutTree

        compartmentColors =
            [ ( "air", Color.rgb255 135 206 235 )
            , ( "water", Color.rgb255 30 144 255 )
            , ( "soil", Color.rgb255 139 90 43 )
            , ( "other", Color.rgb255 169 169 169 )
            , ( "resource", Color.rgb255 76 175 80 )
            , ( "emission", Color.rgb255 244 67 54 )
            ]
                |> Dict.fromList
    in
    leafNodes
        |> List.map
            (\node ->
                let
                    bounds =
                        Tree.label node

                    color =
                        Dict.get bounds.category compartmentColors
                            |> Maybe.withDefault (Color.rgb255 158 158 158)

                    showLabel =
                        bounds.w > 30 && bounds.h > 15
                in
                g []
                    [ rect
                        [ x (String.fromFloat bounds.x)
                        , y (String.fromFloat bounds.y)
                        , width (String.fromFloat bounds.w)
                        , height (String.fromFloat bounds.h)
                        , fill (Color.toCssString color)
                        , stroke "white"
                        , strokeWidth "1"
                        ]
                        [ title [] [ Svg.text (bounds.name ++ "\n" ++ Format.formatScientific bounds.value ++ " " ++ bounds.unit) ]
                        ]
                    , if showLabel then
                        text_
                            [ x (String.fromFloat (bounds.x + 3))
                            , y (String.fromFloat (bounds.y + 12))
                            , fontSize "10"
                            , fill "white"
                            , SvgA.style "pointer-events: none"
                            ]
                            [ Svg.text (truncateLabel (floor bounds.w - 6) bounds.name) ]

                      else
                        Svg.text ""
                    ]
            )


type alias TreemapBounds =
    { x : Float
    , y : Float
    , w : Float
    , h : Float
    , name : String
    , location : String
    , value : Float
    , unit : String
    , processId : String
    , category : String
    }


type alias TreemapLayoutNode =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    , value : Float
    , node : TreemapNode
    }


getLeafNodes : Tree TreemapLayoutNode -> List (Tree TreemapBounds)
getLeafNodes tree =
    let
        nodeData =
            Tree.label tree

        children =
            Tree.children tree

        bounds =
            { x = nodeData.x
            , y = nodeData.y
            , w = nodeData.width
            , h = nodeData.height
            , name = nodeData.node.name
            , location = nodeData.node.location
            , value = nodeData.node.value
            , unit = nodeData.node.unit
            , processId = nodeData.node.processId
            , category = nodeData.node.category
            }
    in
    if List.isEmpty children then
        -- This is a leaf node (but skip the root)
        if nodeData.node.processId /= "" then
            [ Tree.singleton bounds ]

        else
            []

    else
        -- Recurse into children
        children
            |> List.concatMap getLeafNodes


truncateLabel : Int -> String -> String
truncateLabel maxWidth label =
    let
        -- Approximate character width at font size 11
        charWidth =
            7

        maxChars =
            maxWidth // charWidth
    in
    if String.length label <= maxChars then
        label

    else if maxChars <= 3 then
        ""

    else
        String.left (maxChars - 2) label ++ ".."
