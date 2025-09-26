module Views.TreeView exposing (viewTree)

import Dict exposing (Dict)
import Html exposing (Html, div)
import Html.Attributes
import Html.Events
import Models.Activity exposing (ActivityTree, ActivityNode, ActivityEdge, NodeType(..))
import Svg exposing (Svg, svg, g, rect, text_, path, text)
import Svg.Attributes as SvgA exposing (width, height, viewBox, x, y, rx, ry, fill, stroke, strokeWidth, textAnchor, fontSize, fontWeight, d, markerEnd)
import Svg.Events


type alias Msg =
    String


viewTree : ActivityTree -> Html Msg
viewTree activityTree =
    let
        svgWidth = 1200
        svgHeight = 800
        nodePositions = calculateNodePositions activityTree
    in
    div []
        [ Html.div [ Html.Attributes.class "tree-container" ]
            [ svg
                [ width (String.fromInt svgWidth)
                , height (String.fromInt svgHeight)
                , viewBox ("0 0 " ++ String.fromInt svgWidth ++ " " ++ String.fromInt svgHeight)
                ]
                (drawEdges activityTree.edges nodePositions
                    ++ drawNodes activityTree.nodes nodePositions
                )
            ]
        ]


type alias Position =
    { x : Float
    , y : Float
    }


calculateNodePositions : ActivityTree -> Dict String Position
calculateNodePositions activityTree =
    let
        nodesList = Dict.values activityTree.nodes
        maxDepth = List.maximum (List.map .depth nodesList) |> Maybe.withDefault 0

        -- Group nodes by depth
        nodesByDepth = List.foldl
            (\node acc ->
                let
                    depth = node.depth
                    existingNodes = Dict.get depth acc |> Maybe.withDefault []
                in
                Dict.insert depth (node :: existingNodes) acc
            )
            Dict.empty
            nodesList

        -- Calculate positions for each depth level
        positions = Dict.foldl
            (\depth nodes acc ->
                let
                    nodeCount = List.length nodes
                    ySpacing = 600 / (toFloat (Basics.max 1 (nodeCount - 1)))
                    startY = 100
                    xPos = 100 + toFloat depth * 200
                in
                List.indexedMap
                    (\index node ->
                        ( node.id
                        , { x = xPos
                          , y = startY + toFloat index * ySpacing
                          }
                        )
                    )
                    nodes
                |> List.foldl (\(id, pos) accInner -> Dict.insert id pos accInner) acc
            )
            Dict.empty
            nodesByDepth
    in
    positions


drawNodes : Dict String ActivityNode -> Dict String Position -> List (Svg Msg)
drawNodes nodes positions =
    Dict.values nodes
        |> List.filterMap
            (\node ->
                Dict.get node.id positions
                    |> Maybe.map (drawNode node)
            )


drawNode : ActivityNode -> Position -> Svg Msg
drawNode node position =
    let
        nodeWidth = 150
        nodeHeight = 80
        cornerRadius = 5

        nodeColor = case node.nodeType of
            ActivityNodeType -> "#e3f2fd"
            LoopNodeType -> "#fff3e0"

        strokeColor = case node.nodeType of
            ActivityNodeType -> "#2196f3"
            LoopNodeType -> "#ff9800"

        textColor = "#333"
    in
    g [ Svg.Events.onClick node.id ]
        [ rect
            [ x (String.fromFloat (position.x - nodeWidth / 2))
            , y (String.fromFloat (position.y - nodeHeight / 2))
            , width (String.fromFloat nodeWidth)
            , height (String.fromFloat nodeHeight)
            , rx (String.fromFloat cornerRadius)
            , ry (String.fromFloat cornerRadius)
            , fill nodeColor
            , stroke strokeColor
            , strokeWidth "2"
            , SvgA.style "cursor: pointer;"
            ]
            []
        , text_
            [ x (String.fromFloat position.x)
            , y (String.fromFloat (position.y - 15))
            , textAnchor "middle"
            , fontSize "12"
            , fontWeight "bold"
            , fill textColor
            ]
            [ text (truncateText node.name 18) ]
        , text_
            [ x (String.fromFloat position.x)
            , y (String.fromFloat position.y)
            , textAnchor "middle"
            , fontSize "10"
            , fill textColor
            ]
            [ text node.location ]
        , text_
            [ x (String.fromFloat position.x)
            , y (String.fromFloat (position.y + 15))
            , textAnchor "middle"
            , fontSize "10"
            , fill "#666"
            ]
            [ text ("Children: " ++ String.fromInt node.childrenCount) ]
        ]


drawEdges : List ActivityEdge -> Dict String Position -> List (Svg Msg)
drawEdges edges positions =
    edges
        |> List.filterMap (drawEdge positions)


drawEdge : Dict String Position -> ActivityEdge -> Maybe (Svg Msg)
drawEdge positions edge =
    Maybe.map2
        (\fromPos toPos ->
            let
                -- Offset positions to connect node edges, not centers
                nodeWidth = 150
                fromX = fromPos.x + nodeWidth / 2
                fromY = fromPos.y
                toX = toPos.x - nodeWidth / 2
                toY = toPos.y

                -- Calculate control points for curved line
                controlX1 = fromX + (toX - fromX) / 3
                controlY1 = fromY
                controlX2 = toX - (toX - fromX) / 3
                controlY2 = toY

                pathData =
                    "M " ++ String.fromFloat fromX ++ " " ++ String.fromFloat fromY ++
                    " C " ++ String.fromFloat controlX1 ++ " " ++ String.fromFloat controlY1 ++
                    " " ++ String.fromFloat controlX2 ++ " " ++ String.fromFloat controlY2 ++
                    " " ++ String.fromFloat toX ++ " " ++ String.fromFloat toY

                -- Label position at the middle of the curve
                labelX = (fromX + toX) / 2
                labelY = (fromY + toY) / 2 - 10
            in
            g []
                [ path
                    [ SvgA.d pathData
                    , fill "none"
                    , stroke "#666"
                    , strokeWidth "1.5"
                    , markerEnd "url(#arrowhead)"
                    ]
                    []
                , -- Flow quantity label
                  text_
                    [ x (String.fromFloat labelX)
                    , y (String.fromFloat labelY)
                    , textAnchor "middle"
                    , fontSize "9"
                    , fill "#444"
                    , SvgA.style "background: white; padding: 2px;"
                    ]
                    [ text (String.fromFloat edge.quantity ++ " " ++ edge.unit) ]
                ]
        )
        (Dict.get edge.from positions)
        (Dict.get edge.to positions)


truncateText : String -> Int -> String
truncateText text maxLength =
    if String.length text <= maxLength then
        text
    else
        String.left (maxLength - 3) text ++ "..."