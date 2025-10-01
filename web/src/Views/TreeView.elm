module Views.TreeView exposing (viewTree, Msg(..))

import Dict exposing (Dict)
import Html exposing (Html, div, h4, p, strong, ul, li)
import Html.Attributes
import Html.Events
import Models.Activity exposing (ActivityTree, ActivityNode, ActivityEdge, NodeType(..))
import Svg exposing (Svg, svg, g, circle, text_, path, defs, marker, polygon)
import Svg.Attributes as SvgA exposing (width, height, viewBox, cx, cy, r, fill, stroke, strokeWidth, textAnchor, fontSize, fontWeight, d, markerEnd, id, markerWidth, markerHeight, refX, refY, orient, points, x, y, class)
import Svg.Events


type Msg
    = NodeClicked String
    | NodeHovered (Maybe String)


viewTree : ActivityTree -> Maybe String -> Html Msg
viewTree activityTree hoveredNodeId =
    let
        svgWidth = 1200
        svgHeight = 800
        nodePositions = calculateNodePositions activityTree
        nodeSizes = calculateNodeSizes activityTree
    in
    div []
        [ Html.div [ Html.Attributes.class "tree-container" ]
            [ svg
                [ width (String.fromInt svgWidth)
                , height (String.fromInt svgHeight)
                , viewBox ("0 0 " ++ String.fromInt svgWidth ++ " " ++ String.fromInt svgHeight)
                ]
                ([ defs []
                    [ marker
                        [ id "arrowhead"
                        , markerWidth "10"
                        , markerHeight "7"
                        , refX "9"
                        , refY "3.5"
                        , orient "auto"
                        ]
                        [ polygon
                            [ points "0 0, 10 3.5, 0 7"
                            , fill "#666"
                            ]
                            []
                        ]
                    ]
                 ]
                ++ drawEdges activityTree.edges nodePositions nodeSizes
                ++ drawNodes activityTree.nodes nodePositions nodeSizes
                )
            ]
        , -- Tooltip overlay
          case hoveredNodeId of
            Just nodeId ->
                case Dict.get nodeId activityTree.nodes of
                    Just node ->
                        viewTooltip node

                    Nothing ->
                        Html.text ""

            Nothing ->
                Html.text ""
        ]


viewTooltip : ActivityNode -> Html Msg
viewTooltip node =
    div
        [ Html.Attributes.class "node-tooltip"
        , Html.Attributes.style "position" "fixed"
        , Html.Attributes.style "top" "20px"
        , Html.Attributes.style "left" "20px"
        , Html.Attributes.style "background" "rgba(0, 0, 0, 0.9)"
        , Html.Attributes.style "color" "white"
        , Html.Attributes.style "padding" "0.75rem"
        , Html.Attributes.style "border-radius" "6px"
        , Html.Attributes.style "font-size" "0.85rem"
        , Html.Attributes.style "max-width" "300px"
        , Html.Attributes.style "word-wrap" "break-word"
        , Html.Attributes.style "z-index" "1000"
        , Html.Attributes.style "pointer-events" "none"
        ]
        [ h4 [ Html.Attributes.style "margin" "0 0 0.5rem 0", Html.Attributes.style "font-weight" "bold" ]
            [ Html.text node.name ]
        , p [ Html.Attributes.style "margin" "0 0 0.25rem 0" ]
            [ strong [] [ Html.text "Location: " ], Html.text node.location ]
        , p [ Html.Attributes.style "margin" "0 0 0.25rem 0" ]
            [ strong [] [ Html.text "Unit: " ], Html.text node.unit ]
        , p [ Html.Attributes.style "margin" "0 0 0.25rem 0" ]
            [ strong [] [ Html.text "Children: " ], Html.text (String.fromInt node.childrenCount) ]
        , p [ Html.Attributes.style "margin" "0 0 0.25rem 0" ]
            [ strong [] [ Html.text "Depth: " ], Html.text (String.fromInt node.depth) ]
        , if List.isEmpty node.description then
            Html.text ""
          else
            div []
                [ strong [] [ Html.text "Description:" ]
                , ul [ Html.Attributes.style "margin" "0.25rem 0 0 1rem", Html.Attributes.style "padding" "0" ]
                    (List.map (\desc -> li [] [ Html.text desc ]) node.description)
                ]
        ]


type alias Position =
    { x : Float
    , y : Float
    }


type alias NodeSize =
    { radius : Float
    , showText : Bool
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


calculateNodeSizes : ActivityTree -> Dict String NodeSize
calculateNodeSizes activityTree =
    let
        -- Get all quantities from edges to calculate range
        quantities = List.map .quantity activityTree.edges

        -- Find min and max quantities for scaling
        minQuantity = List.minimum quantities |> Maybe.withDefault 0
        maxQuantity = List.maximum quantities |> Maybe.withDefault 1

        -- Size constraints
        minRadius = 8
        maxRadius = 50
        textThreshold = 20

        -- Create a map from node ID to incoming quantity (for sizing)
        nodeQuantities =
            List.foldl
                (\edge acc ->
                    Dict.insert edge.to edge.quantity acc
                )
                Dict.empty
                activityTree.edges

        -- Calculate size for each node
        calculateNodeSize nodeId =
            case Dict.get nodeId nodeQuantities of
                Nothing ->
                    -- Root node or node without incoming edges - use medium size
                    { radius = (minRadius + maxRadius) / 2
                    , showText = True
                    }

                Just quantity ->
                    let
                        -- Use logarithmic scaling for better visualization
                        -- Avoid log(0) by adding 1 to all values
                        logQuantity = logBase 10 (quantity + 1)
                        logMax = logBase 10 (maxQuantity + 1)
                        logMin = logBase 10 (minQuantity + 1)

                        -- Normalize to 0-1 range
                        normalizedSize =
                            if logMax == logMin then
                                0.5
                            else
                                (logQuantity - logMin) / (logMax - logMin)

                        radius = minRadius + normalizedSize * (maxRadius - minRadius)
                    in
                    { radius = radius
                    , showText = radius >= textThreshold
                    }

        nodeIds = Dict.keys activityTree.nodes
    in
    List.foldl
        (\nodeId acc ->
            Dict.insert nodeId (calculateNodeSize nodeId) acc
        )
        Dict.empty
        nodeIds


drawNodes : Dict String ActivityNode -> Dict String Position -> Dict String NodeSize -> List (Svg Msg)
drawNodes nodes positions sizes =
    Dict.values nodes
        |> List.filterMap
            (\node ->
                Maybe.map2 (drawNode node)
                    (Dict.get node.id positions)
                    (Dict.get node.id sizes)
            )


drawNode : ActivityNode -> Position -> NodeSize -> Svg Msg
drawNode node position nodeSize =
    let
        nodeColor = case node.nodeType of
            ActivityNodeType -> "#e3f2fd"
            LoopNodeType -> "#fff3e0"

        strokeColor = case node.nodeType of
            ActivityNodeType -> "#2196f3"
            LoopNodeType -> "#ff9800"

        textColor = "#333"
    in
    g [ Svg.Events.onClick (NodeClicked node.id)
      , Svg.Events.onMouseOver (NodeHovered (Just node.id))
      , Svg.Events.onMouseOut (NodeHovered Nothing)
      , SvgA.class "node-circle"
      ]
        [ -- Main circle
          circle
            [ cx (String.fromFloat position.x)
            , cy (String.fromFloat position.y)
            , r (String.fromFloat nodeSize.radius)
            , fill nodeColor
            , stroke strokeColor
            , strokeWidth "2"
            , SvgA.style "cursor: pointer;"
            ]
            []
        , -- Conditional text rendering based on size
          if nodeSize.showText then
            g []
                [ text_
                    [ SvgA.x (String.fromFloat position.x)
                    , SvgA.y (String.fromFloat (position.y - 5))
                    , textAnchor "middle"
                    , fontSize (String.fromFloat (min 12 (nodeSize.radius / 3)))
                    , fontWeight "bold"
                    , fill textColor
                    ]
                    [ Svg.text (truncateText node.name (round (nodeSize.radius / 3))) ]
                , text_
                    [ SvgA.x (String.fromFloat position.x)
                    , SvgA.y (String.fromFloat (position.y + 8))
                    , textAnchor "middle"
                    , fontSize (String.fromFloat (min 10 (nodeSize.radius / 4)))
                    , fill "#666"
                    ]
                    [ Svg.text node.location ]
                ]
          else
            g [] [] -- Empty group for small circles
        ]


drawEdges : List ActivityEdge -> Dict String Position -> Dict String NodeSize -> List (Svg Msg)
drawEdges edges positions sizes =
    edges
        |> List.filterMap (drawEdge positions sizes)


drawEdge : Dict String Position -> Dict String NodeSize -> ActivityEdge -> Maybe (Svg Msg)
drawEdge positions sizes edge =
    Maybe.map3
        (\fromPos toPos fromSize ->
            let
                -- Get the radius of the target node for proper connection
                toSize = Dict.get edge.to sizes |> Maybe.withDefault { radius = 25, showText = True }

                -- Calculate edge connection points using circle radius
                fromX = fromPos.x + fromSize.radius
                fromY = fromPos.y
                toX = toPos.x - toSize.radius
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
                    [ Svg.text (String.fromFloat edge.quantity ++ " " ++ edge.unit) ]
                ]
        )
        (Dict.get edge.from positions)
        (Dict.get edge.to positions)
        (Dict.get edge.from sizes)


truncateText : String -> Int -> String
truncateText text maxLength =
    if String.length text <= maxLength then
        text
    else
        String.left (maxLength - 3) text ++ "..."