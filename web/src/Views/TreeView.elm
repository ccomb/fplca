module Views.TreeView exposing (Msg(..), viewTree)

import Dict exposing (Dict)
import Html exposing (Html, div, h4, li, p, strong, ul)
import Html.Attributes
import Html.Events
import Models.Activity exposing (ActivityEdge, ActivityNode, ActivityTree, EdgeType(..), NodeType(..))
import Svg exposing (Svg, circle, defs, g, line, marker, path, polygon, rect, svg, text_)
import Svg.Attributes as SvgA exposing (class, cx, cy, d, fill, fontSize, fontWeight, height, id, markerEnd, markerHeight, markerWidth, orient, points, r, refX, refY, stroke, strokeDasharray, strokeWidth, style, textAnchor, viewBox, width, x, x1, x2, y, y1, y2)
import Svg.Events
import Utils.Format as Format


type Msg
    = NodeClicked String
    | NodeHovered (Maybe String)


viewTree : ActivityTree -> Maybe String -> Html Msg
viewTree activityTree hoveredNodeId =
    let
        svgWidth =
            1400

        svgHeight =
            900

        nodePositions =
            calculateNodePositions activityTree

        nodeSizes =
            calculateNodeSizes activityTree
    in
    div []
        [ Html.div [ Html.Attributes.class "tree-container" ]
            [ svg
                [ width "100%"
                , height "100%"
                , viewBox ("0 0 " ++ String.fromInt svgWidth ++ " " ++ String.fromInt svgHeight)
                , style "display: block;"
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
                 , -- Label for biosphere section (top)
                   text_
                    [ x "20"
                    , y "30"
                    , fontSize "14"
                    , fontWeight "bold"
                    , fill "#555"
                    ]
                    [ Svg.text "Elementary flows (Biosphere)" ]
                 , -- Separator line between biosphere and technosphere
                   line
                    [ x1 "0"
                    , y1 "200"
                    , x2 (String.fromInt svgWidth)
                    , y2 "200"
                    , stroke "#ccc"
                    , strokeWidth "2"
                    , strokeDasharray "5,5"
                    ]
                    []
                 , -- Label for technosphere section (bottom)
                   text_
                    [ x "20"
                    , y "230"
                    , fontSize "14"
                    , fontWeight "bold"
                    , fill "#555"
                    ]
                    [ Svg.text "Intermediate flows (Technosphere)" ]
                 ]
                    ++ drawNodes activityTree.nodes nodePositions nodeSizes
                    ++ drawEdges activityTree.edges nodePositions nodeSizes
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
        nodesList =
            Dict.values activityTree.nodes

        -- Separate nodes into technosphere (intermediate) and biosphere (elementary)
        ( technosphereNodes, biosphereNodes ) =
            List.partition
                (\node ->
                    case node.nodeType of
                        ActivityNodeType ->
                            True

                        LoopNodeType ->
                            True

                        BiosphereEmissionNodeType ->
                            False

                        BiosphereResourceNodeType ->
                            False
                )
                nodesList

        -- Calculate positions for biosphere nodes (at top, grouped horizontally)
        biosphereStartY =
            100

        -- Start biosphere section at top
        biosphereCount =
            List.length biosphereNodes

        biosphereXSpacing =
            if biosphereCount > 1 then
                1200 / toFloat (biosphereCount - 1)

            else
                0

        biosphereStartX =
            100

        -- Calculate positions for technosphere nodes (by depth, below biosphere)
        nodesByDepth =
            List.foldl
                (\node acc ->
                    let
                        depth =
                            node.depth

                        existingNodes =
                            Dict.get depth acc |> Maybe.withDefault []
                    in
                    Dict.insert depth (node :: existingNodes) acc
                )
                Dict.empty
                technosphereNodes

        technospherePositions =
            Dict.foldl
                (\depth nodes acc ->
                    let
                        nodeCount =
                            List.length nodes

                        ySpacing =
                            400 / toFloat (Basics.max 1 (nodeCount - 1))

                        startY =
                            300

                        -- Start technosphere below biosphere
                        xPos =
                            100 + toFloat depth * 200
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
                        |> List.foldl (\( id, pos ) accInner -> Dict.insert id pos accInner) acc
                )
                Dict.empty
                nodesByDepth

        biospherePositions =
            List.indexedMap
                (\index node ->
                    ( node.id
                    , { x = biosphereStartX + toFloat index * biosphereXSpacing
                      , y = biosphereStartY
                      }
                    )
                )
                biosphereNodes
                |> Dict.fromList

        -- Merge both position dictionaries
        allPositions =
            Dict.union technospherePositions biospherePositions
    in
    allPositions


calculateNodeSizes : ActivityTree -> Dict String NodeSize
calculateNodeSizes activityTree =
    let
        -- Get all quantities from edges to calculate range
        -- Use absolute values to handle negative quantities
        quantities =
            List.map (abs << .quantity) activityTree.edges

        -- Find min and max quantities for scaling (now all positive)
        minQuantity =
            List.minimum quantities |> Maybe.withDefault 0

        maxQuantity =
            List.maximum quantities |> Maybe.withDefault 1

        -- Size constraints
        minRadius =
            8

        maxRadius =
            50

        textThreshold =
            20

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
                        -- Use absolute value to handle negative quantities
                        -- Avoid log(0) by adding 1 to all values
                        absQuantity =
                            abs quantity

                        logQuantity =
                            logBase 10 (absQuantity + 1)

                        logMax =
                            logBase 10 (maxQuantity + 1)

                        logMin =
                            logBase 10 (minQuantity + 1)

                        -- Normalize to 0-1 range
                        normalizedSize =
                            if logMax == logMin then
                                0.5

                            else
                                (logQuantity - logMin) / (logMax - logMin)

                        radius =
                            minRadius + normalizedSize * (maxRadius - minRadius)
                    in
                    { radius = radius
                    , showText = radius >= textThreshold
                    }

        nodeIds =
            Dict.keys activityTree.nodes
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



-- Helper functions to draw different node shapes


drawCircle : Position -> NodeSize -> String -> String -> Svg msg
drawCircle position nodeSize fillColor strokeColor =
    circle
        [ cx (String.fromFloat position.x)
        , cy (String.fromFloat position.y)
        , r (String.fromFloat nodeSize.radius)
        , fill fillColor
        , stroke strokeColor
        , strokeWidth "2"
        , SvgA.style "cursor: pointer;"
        ]
        []


drawSquare : Position -> NodeSize -> String -> String -> Svg msg
drawSquare position nodeSize fillColor strokeColor =
    let
        size =
            nodeSize.radius * 1.5

        halfSize =
            size / 2
    in
    rect
        [ SvgA.x (String.fromFloat (position.x - halfSize))
        , SvgA.y (String.fromFloat (position.y - halfSize))
        , SvgA.width (String.fromFloat size)
        , SvgA.height (String.fromFloat size)
        , fill fillColor
        , stroke strokeColor
        , strokeWidth "2"
        , SvgA.style "cursor: pointer;"
        ]
        []


drawDiamond : Position -> NodeSize -> String -> String -> Svg msg
drawDiamond position nodeSize fillColor strokeColor =
    let
        size =
            nodeSize.radius * 1.5

        -- Diamond points: top, right, bottom, left
        top =
            ( position.x, position.y - size )

        right =
            ( position.x + size, position.y )

        bottom =
            ( position.x, position.y + size )

        left =
            ( position.x - size, position.y )

        pointsStr =
            String.fromFloat (Tuple.first top)
                ++ ","
                ++ String.fromFloat (Tuple.second top)
                ++ " "
                ++ String.fromFloat (Tuple.first right)
                ++ ","
                ++ String.fromFloat (Tuple.second right)
                ++ " "
                ++ String.fromFloat (Tuple.first bottom)
                ++ ","
                ++ String.fromFloat (Tuple.second bottom)
                ++ " "
                ++ String.fromFloat (Tuple.first left)
                ++ ","
                ++ String.fromFloat (Tuple.second left)
    in
    polygon
        [ SvgA.points pointsStr
        , fill fillColor
        , stroke strokeColor
        , strokeWidth "2"
        , SvgA.style "cursor: pointer;"
        ]
        []


drawNode : ActivityNode -> Position -> NodeSize -> Svg Msg
drawNode node position nodeSize =
    let
        ( nodeColor, strokeColor ) =
            case node.nodeType of
                ActivityNodeType ->
                    ( "#e3f2fd", "#2196f3" )

                LoopNodeType ->
                    ( "#fff3e0", "#ff9800" )

                BiosphereEmissionNodeType ->
                    -- Color based on compartment
                    case node.compartment of
                        Just "air" ->
                            ( "#e1f5fe", "#03a9f4" )

                        -- Light blue for air
                        Just "water" ->
                            ( "#b3e5fc", "#0277bd" )

                        -- Blue for water
                        Just "soil" ->
                            ( "#d7ccc8", "#6d4c41" )

                        -- Brown for soil
                        _ ->
                            ( "#ffebee", "#e53935" )

                -- Red for other
                BiosphereResourceNodeType ->
                    ( "#e8f5e9", "#4caf50" )

        -- Green for resources
        textColor =
            "#333"

        nodeShape =
            case node.nodeType of
                ActivityNodeType ->
                    drawCircle position nodeSize nodeColor strokeColor

                LoopNodeType ->
                    drawCircle position nodeSize nodeColor strokeColor

                BiosphereEmissionNodeType ->
                    drawDiamond position nodeSize nodeColor strokeColor

                BiosphereResourceNodeType ->
                    drawSquare position nodeSize nodeColor strokeColor
    in
    g
        [ Svg.Events.onClick (NodeClicked node.id)
        , Svg.Events.onMouseOver (NodeHovered (Just node.id))
        , Svg.Events.onMouseOut (NodeHovered Nothing)
        , SvgA.class "node-shape"
        ]
        [ nodeShape
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
            g [] []

        -- Empty group for small circles
        ]


drawEdges : List ActivityEdge -> Dict String Position -> Dict String NodeSize -> List (Svg Msg)
drawEdges edges positions sizes =
    edges
        |> List.filterMap (drawEdge positions sizes)


drawEdge : Dict String Position -> Dict String NodeSize -> ActivityEdge -> Maybe (Svg Msg)
drawEdge positions sizes edge =
    Maybe.map3
        (\toPos fromPos toSize ->
            let
                -- Get the radius of the source node for proper connection
                fromSize =
                    Dict.get edge.from sizes |> Maybe.withDefault { radius = 25, showText = True }

                -- Arrow marker size (needs to be kept in sync with marker definition)
                arrowMarkerOffset =
                    2

                -- Calculate edge connection points based on edge type
                { fromX, fromY, toX, toY, controlX1, controlY1, controlX2, controlY2 } =
                    case edge.edgeType of
                        TechnosphereEdgeType ->
                            -- Horizontal arrows for technosphere (intermediate flows)
                            -- LCA semantics: edge.from = consumer, edge.to = supplier
                            -- Arrow shows material flow: FROM supplier (edge.to) TO consumer (edge.from)
                            -- Suppliers are positioned on the right, consumers on the left
                            -- Start from LEFT edge of supplier node (edge.to)
                            -- End just before the RIGHT edge of consumer node (edge.from)
                            { fromX = toPos.x - toSize.radius
                            , fromY = toPos.y
                            , toX = fromPos.x + fromSize.radius + arrowMarkerOffset
                            , toY = fromPos.y
                            , controlX1 = toPos.x - toSize.radius + ((fromPos.x + fromSize.radius + arrowMarkerOffset) - (toPos.x - toSize.radius)) / 3
                            , controlY1 = toPos.y
                            , controlX2 = (fromPos.x + fromSize.radius + arrowMarkerOffset) - ((fromPos.x + fromSize.radius + arrowMarkerOffset) - (toPos.x - toSize.radius)) / 3
                            , controlY2 = fromPos.y
                            }

                        BiosphereEmissionEdgeType ->
                            -- Vertical arrows for emissions: FROM activity (bottom) TO biosphere (top)
                            -- Emissions flow from activities upward to the biosphere
                            -- Start from TOP of activity circle (edge.from)
                            -- End at BOTTOM of biosphere shape (edge.to)
                            { fromX = fromPos.x
                            , fromY = fromPos.y - fromSize.radius
                            , toX = toPos.x
                            , toY = toPos.y + toSize.radius + arrowMarkerOffset
                            , controlX1 = fromPos.x
                            , controlY1 = (fromPos.y - fromSize.radius) - ((fromPos.y - fromSize.radius) - (toPos.y + toSize.radius + arrowMarkerOffset)) / 3
                            , controlX2 = toPos.x
                            , controlY2 = (toPos.y + toSize.radius + arrowMarkerOffset) + ((fromPos.y - fromSize.radius) - (toPos.y + toSize.radius + arrowMarkerOffset)) / 3
                            }

                        BiosphereResourceEdgeType ->
                            -- Vertical arrows for resources: FROM biosphere (top) TO activity (bottom)
                            -- Resources flow from biosphere downward to activities
                            -- Start from BOTTOM of biosphere shape (edge.from = biosphere)
                            -- End at TOP of activity circle (edge.to = activity)
                            { fromX = fromPos.x
                            , fromY = fromPos.y + fromSize.radius
                            , toX = toPos.x
                            , toY = toPos.y - toSize.radius - arrowMarkerOffset
                            , controlX1 = fromPos.x
                            , controlY1 = (fromPos.y + fromSize.radius) + ((toPos.y - toSize.radius - arrowMarkerOffset) - (fromPos.y + fromSize.radius)) / 3
                            , controlX2 = toPos.x
                            , controlY2 = (toPos.y - toSize.radius - arrowMarkerOffset) - ((toPos.y - toSize.radius - arrowMarkerOffset) - (fromPos.y + fromSize.radius)) / 3
                            }

                pathData =
                    "M "
                        ++ String.fromFloat fromX
                        ++ " "
                        ++ String.fromFloat fromY
                        ++ " C "
                        ++ String.fromFloat controlX1
                        ++ " "
                        ++ String.fromFloat controlY1
                        ++ " "
                        ++ String.fromFloat controlX2
                        ++ " "
                        ++ String.fromFloat controlY2
                        ++ " "
                        ++ String.fromFloat toX
                        ++ " "
                        ++ String.fromFloat toY

                -- Label position at the middle of the curve
                labelX =
                    (fromX + toX) / 2

                labelY =
                    (fromY + toY) / 2 - 10
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
                    [ Svg.text (Format.formatScientific edge.quantity ++ " " ++ edge.unit) ]
                ]
        )
        (Dict.get edge.to positions)
        (Dict.get edge.from positions)
        (Dict.get edge.to sizes)


truncateText : String -> Int -> String
truncateText text maxLength =
    if String.length text <= maxLength then
        text

    else
        String.left (maxLength - 3) text ++ "..."
