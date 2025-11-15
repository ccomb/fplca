module Views.GraphView exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Events
import Dict exposing (Dict)
import ForceDirected
import Html exposing (Html, div, text)
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Models.Graph exposing (GraphData, GraphEdge, GraphNode)
import Svg exposing (Svg, circle, defs, g, line, marker, polygon, svg, text_)
import Svg.Attributes as SvgA exposing (class, cx, cy, d, fill, fontSize, height, id, markerEnd, markerHeight, markerWidth, orient, points, r, refX, refY, stroke, strokeWidth, textAnchor, viewBox, width, x, x1, x2, y, y1, y2)
import Svg.Events


{-| Model for the graph visualization with force-directed layout
-}
type alias Model =
    { nodes : List Entity
    , edges : List Link
    , simulation : ForceDirected.State Int
    , drag : Maybe Drag
    , hoveredNode : Maybe Int
    , selectedNode : Maybe Int
    , viewBoxX : Float
    , viewBoxY : Float
    , viewBoxWidth : Float
    , viewBoxHeight : Float
    , isPanning : Maybe ( Float, Float )
    , nodeCountWarning : Maybe String
    }


{-| Entity represents a node with position and velocity
-}
type alias Entity =
    { id : Int
    , x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , value : Float
    , label : String
    , unit : String
    , processId : String
    , location : String
    }


{-| Link represents an edge in the graph
-}
type alias Link =
    { source : Int
    , target : Int
    , value : Float
    , unit : String
    , flowName : String
    }


{-| Drag state for interactive node dragging
-}
type alias Drag =
    { startMousePos : ( Float, Float )
    , currentMousePos : ( Float, Float )
    , startNodePos : ( Float, Float )
    , nodeId : Int
    }


{-| Messages for graph interactions
-}
type Msg
    = Tick
    | DragStart Int ( Float, Float ) ( Float, Float ) -- nodeId, mousePos, nodePos
    | DragAt ( Float, Float )
    | DragEnd ( Float, Float )
    | NodeHover (Maybe Int)
    | NodeClick Int
    | ZoomIn
    | ZoomOut
    | ResetZoom
    | AutoFit
    | StartBackgroundPan ( Float, Float )
    | MoveBackgroundPan ( Float, Float )
    | EndBackgroundPan
    | WheelZoom Float


{-| Initialize the graph model with force-directed layout
-}
init : GraphData -> Model
init graphData =
    let
        -- Initialize nodes with random positions
        nodes =
            graphData.nodes
                |> List.indexedMap
                    (\index gn ->
                        let
                            -- Use a simple pseudo-random distribution based on index
                            -- Spread nodes over a larger area
                            angle =
                                (toFloat index * 2.4) * pi

                            radius =
                                200 + (toFloat (modBy 7 index) * 100)

                            x =
                                svgWidth / 2 + radius * cos angle

                            y =
                                svgHeight / 2 + radius * sin angle
                        in
                        { id = gn.id
                        , x = x
                        , y = y
                        , vx = 0
                        , vy = 0
                        , value = gn.value
                        , label = gn.label
                        , unit = gn.unit
                        , processId = gn.processId
                        , location = gn.location
                        }
                    )

        -- Convert edges to links
        edges =
            graphData.edges
                |> List.map
                    (\ge ->
                        { source = ge.source
                        , target = ge.target
                        , value = ge.value
                        , unit = ge.unit
                        , flowName = ge.flowName
                        }
                    )

        -- Create force simulation
        links =
            edges
                |> List.map
                    (\edge ->
                        { source = edge.source
                        , target = edge.target
                        , distance = 150  -- Increased from 100 for more spacing
                        , strength = Nothing
                        }
                    )

        forces =
            [ ForceDirected.links links
            , ForceDirected.manyBodyStrength -300 <| List.map .id nodes  -- Increased repulsion from -100 to -300
            , ForceDirected.center (svgWidth / 2) (svgHeight / 2)
            ]

        simulation =
            ForceDirected.simulation forces

        -- Check if node count is too high
        nodeCount =
            List.length nodes

        maxNodes =
            150

        warning =
            if nodeCount > maxNodes then
                Just
                    ("⚠️ Large graph with "
                        ++ String.fromInt nodeCount
                        ++ " nodes may cause performance issues. Consider increasing the cutoff percentage."
                    )

            else
                Nothing
    in
    { nodes = nodes
    , edges = edges
    , simulation = simulation
    , drag = Nothing
    , hoveredNode = Nothing
    , selectedNode = Nothing
    , viewBoxX = 0
    , viewBoxY = 0
    , viewBoxWidth = svgWidth
    , viewBoxHeight = svgHeight
    , isPanning = Nothing
    , nodeCountWarning = warning
    }


{-| SVG dimensions
-}
svgWidth : Float
svgWidth =
    1400


svgHeight : Float
svgHeight =
    900


{-| Calculate bounding box of all nodes
-}
calculateBoundingBox : List Entity -> { minX : Float, minY : Float, maxX : Float, maxY : Float }
calculateBoundingBox nodes =
    let
        margin =
            50

        xs =
            List.map .x nodes

        ys =
            List.map .y nodes

        -- Get max radius from node values
        maxValue =
            nodes
                |> List.map .value
                |> List.maximum
                |> Maybe.withDefault 1

        maxRadius =
            10 + 30

        minX =
            (List.minimum xs |> Maybe.withDefault 0) - maxRadius - margin

        maxX =
            (List.maximum xs |> Maybe.withDefault svgWidth) + maxRadius + margin

        minY =
            (List.minimum ys |> Maybe.withDefault 0) - maxRadius - margin

        maxY =
            (List.maximum ys |> Maybe.withDefault svgHeight) + maxRadius + margin
    in
    { minX = minX, minY = minY, maxX = maxX, maxY = maxY }


{-| Auto-fit viewBox to show all nodes
-}
autoFitViewBox : List Entity -> { x : Float, y : Float, width : Float, height : Float }
autoFitViewBox nodes =
    let
        bbox =
            calculateBoundingBox nodes
    in
    { x = bbox.minX
    , y = bbox.minY
    , width = bbox.maxX - bbox.minX
    , height = bbox.maxY - bbox.minY
    }


{-| Update function with force simulation
-}
update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick ->
            let
                ( newSimulation, newNodes ) =
                    ForceDirected.tick model.simulation model.nodes

                -- Check if simulation just completed and auto-fit
                wasRunning =
                    not (ForceDirected.isCompleted model.simulation)

                justCompleted =
                    wasRunning && ForceDirected.isCompleted newSimulation

                updatedModel =
                    case model.drag of
                        Nothing ->
                            { model
                                | simulation = newSimulation
                                , nodes = newNodes
                            }

                        Just drag ->
                            let
                                ( dx, dy ) =
                                    ( Tuple.first drag.currentMousePos - Tuple.first drag.startMousePos
                                    , Tuple.second drag.currentMousePos - Tuple.second drag.startMousePos
                                    )

                                ( newX, newY ) =
                                    ( Tuple.first drag.startNodePos + dx
                                    , Tuple.second drag.startNodePos + dy
                                    )
                            in
                            { model
                                | simulation = newSimulation
                                , nodes = updateNodePosition drag.nodeId ( newX, newY ) newNodes
                            }
            in
            -- Auto-fit when simulation completes
            if justCompleted then
                let
                    fitBox =
                        autoFitViewBox updatedModel.nodes
                in
                { updatedModel
                    | viewBoxX = fitBox.x
                    , viewBoxY = fitBox.y
                    , viewBoxWidth = fitBox.width
                    , viewBoxHeight = fitBox.height
                }

            else
                updatedModel

        DragStart nodeId mousePos nodePos ->
            { model
                | drag =
                    Just
                        { startMousePos = mousePos
                        , currentMousePos = mousePos
                        , startNodePos = nodePos
                        , nodeId = nodeId
                        }
            }

        DragAt mousePos ->
            case model.drag of
                Just drag ->
                    let
                        -- Calculate delta and apply to node
                        ( dx, dy ) =
                            ( Tuple.first mousePos - Tuple.first drag.startMousePos
                            , Tuple.second mousePos - Tuple.second drag.startMousePos
                            )

                        ( newX, newY ) =
                            ( Tuple.first drag.startNodePos + dx
                            , Tuple.second drag.startNodePos + dy
                            )
                    in
                    { model
                        | drag = Just { drag | currentMousePos = mousePos }
                        , nodes = updateNodePosition drag.nodeId ( newX, newY ) model.nodes
                    }

                Nothing ->
                    model

        DragEnd _ ->
            { model | drag = Nothing }

        NodeHover maybeId ->
            { model | hoveredNode = maybeId }

        NodeClick nodeId ->
            { model | selectedNode = Just nodeId }

        ZoomIn ->
            let
                zoomFactor =
                    0.8

                newWidth =
                    model.viewBoxWidth * zoomFactor

                newHeight =
                    model.viewBoxHeight * zoomFactor

                centerX =
                    model.viewBoxX + model.viewBoxWidth / 2

                centerY =
                    model.viewBoxY + model.viewBoxHeight / 2

                newX =
                    centerX - newWidth / 2

                newY =
                    centerY - newHeight / 2
            in
            { model
                | viewBoxX = newX
                , viewBoxY = newY
                , viewBoxWidth = newWidth
                , viewBoxHeight = newHeight
            }

        ZoomOut ->
            let
                zoomFactor =
                    1.25

                newWidth =
                    model.viewBoxWidth * zoomFactor

                newHeight =
                    model.viewBoxHeight * zoomFactor

                centerX =
                    model.viewBoxX + model.viewBoxWidth / 2

                centerY =
                    model.viewBoxY + model.viewBoxHeight / 2

                newX =
                    centerX - newWidth / 2

                newY =
                    centerY - newHeight / 2
            in
            { model
                | viewBoxX = newX
                , viewBoxY = newY
                , viewBoxWidth = newWidth
                , viewBoxHeight = newHeight
            }

        ResetZoom ->
            { model
                | viewBoxX = 0
                , viewBoxY = 0
                , viewBoxWidth = svgWidth
                , viewBoxHeight = svgHeight
            }

        AutoFit ->
            let
                fitBox =
                    autoFitViewBox model.nodes
            in
            { model
                | viewBoxX = fitBox.x
                , viewBoxY = fitBox.y
                , viewBoxWidth = fitBox.width
                , viewBoxHeight = fitBox.height
            }

        StartBackgroundPan mousePos ->
            { model | isPanning = Just mousePos }

        MoveBackgroundPan mousePos ->
            case model.isPanning of
                Just startPos ->
                    let
                        dx =
                            (Tuple.first startPos - Tuple.first mousePos) * (model.viewBoxWidth / svgWidth)

                        dy =
                            (Tuple.second startPos - Tuple.second mousePos) * (model.viewBoxHeight / svgHeight)
                    in
                    { model
                        | viewBoxX = model.viewBoxX + dx
                        , viewBoxY = model.viewBoxY + dy
                        , isPanning = Just mousePos
                    }

                Nothing ->
                    model

        EndBackgroundPan ->
            { model | isPanning = Nothing }

        WheelZoom delta ->
            let
                zoomFactor =
                    if delta < 0 then
                        0.9

                    else
                        1.1

                newWidth =
                    model.viewBoxWidth * zoomFactor

                newHeight =
                    model.viewBoxHeight * zoomFactor

                centerX =
                    model.viewBoxX + model.viewBoxWidth / 2

                centerY =
                    model.viewBoxY + model.viewBoxHeight / 2

                newX =
                    centerX - newWidth / 2

                newY =
                    centerY - newHeight / 2
            in
            { model
                | viewBoxX = newX
                , viewBoxY = newY
                , viewBoxWidth = newWidth
                , viewBoxHeight = newHeight
            }


{-| Update a specific node's position (for dragging)
-}
updateNodePosition : Int -> ( Float, Float ) -> List Entity -> List Entity
updateNodePosition targetId ( x, y ) =
    List.map
        (\node ->
            if node.id == targetId then
                { node | x = x, y = y, vx = 0, vy = 0 }

            else
                node
        )


{-| View function - takes mainActivityId to highlight it without storing in model
-}
view : String -> Model -> Html Msg
view mainActivityId model =
    div
        [ Html.Attributes.style "position" "relative"
        , Html.Events.custom "wheel"
            (Decode.map
                (\delta ->
                    { message = WheelZoom delta
                    , stopPropagation = True
                    , preventDefault = True
                    }
                )
                (Decode.field "deltaY" Decode.float)
            )
        ]
        ([ case model.nodeCountWarning of
            Just warning ->
                div
                    [ Html.Attributes.class "notification is-warning"
                    , Html.Attributes.style "margin" "0"
                    , Html.Attributes.style "border-radius" "0"
                    ]
                    [ text warning ]

            Nothing ->
                text ""
         , svg
            [ width "100%"
            , height "100%"
            , viewBox
                (String.fromFloat model.viewBoxX
                    ++ " "
                    ++ String.fromFloat model.viewBoxY
                    ++ " "
                    ++ String.fromFloat model.viewBoxWidth
                    ++ " "
                    ++ String.fromFloat model.viewBoxHeight
                )
            , SvgA.style "border: 1px solid #ccc; background-color: white; display: block; cursor: grab;"
            , Svg.Events.custom "mousedown"
                (Decode.map
                    (\mousePos ->
                        { message = StartBackgroundPan mousePos
                        , stopPropagation = False
                        , preventDefault = True
                        }
                    )
                    decodeMousePosition
                )
            ]
            [ defs []
                [ marker
                    [ id "arrowhead"
                    , markerWidth "10"
                    , markerHeight "7"
                    , refX "20"
                    , refY "3.5"
                    , orient "auto"
                    ]
                    [ polygon
                        [ points "0 0, 10 3.5, 0 7"
                        , fill "#999"
                        ]
                        []
                    ]
                ]
            , drawEdges model.nodes model.edges
            , drawNodes model.nodes model.hoveredNode mainActivityId
            ]
         , viewTooltip model
         , viewZoomControls model
         ]
        )


{-| Draw all edges
-}
drawEdges : List Entity -> List Link -> Svg Msg
drawEdges nodes edges =
    let
        nodePositions =
            nodes
                |> List.map (\node -> ( node.id, ( node.x, node.y ) ))
                |> Dict.fromList

        drawEdge edge =
            case ( Dict.get edge.source nodePositions, Dict.get edge.target nodePositions ) of
                ( Just ( x1Val, y1Val ), Just ( x2Val, y2Val ) ) ->
                    line
                        [ x1 (String.fromFloat x1Val)
                        , y1 (String.fromFloat y1Val)
                        , x2 (String.fromFloat x2Val)
                        , y2 (String.fromFloat y2Val)
                        , stroke "#999"
                        , strokeWidth "1.5"
                        , markerEnd "url(#arrowhead)"
                        , SvgA.style "pointer-events: none;"
                        ]
                        []

                _ ->
                    g [] []
    in
    g [] (List.map drawEdge edges)


{-| Get color for a unit using a color palette
-}
getColorForUnit : Dict String String -> String -> String
getColorForUnit unitColors unit =
    Dict.get unit unitColors
        |> Maybe.withDefault "#3273dc"  -- Default blue


{-| Build a color map for all unique units
-}
buildUnitColorMap : List Entity -> Dict String String
buildUnitColorMap nodes =
    let
        -- Color palette for different units
        colorPalette =
            [ "#3273dc"  -- Blue
            , "#48c774"  -- Green
            , "#ffdd57"  -- Yellow
            , "#f14668"  -- Red
            , "#b5a7d6"  -- Purple
            , "#f093a2"  -- Pink
            , "#4ecdc4"  -- Teal
            , "#ff6b6b"  -- Coral
            , "#95e1d3"  -- Mint
            , "#f38181"  -- Salmon
            ]

        uniqueUnits =
            nodes
                |> List.map .unit
                |> List.sortBy identity
                |> List.foldl (\u acc -> if List.member u acc then acc else u :: acc) []
                |> List.reverse

        unitColorPairs =
            List.map2 (\unit color -> ( unit, color ))
                uniqueUnits
                (List.repeat (List.length uniqueUnits) colorPalette |> List.concat)
    in
    Dict.fromList unitColorPairs


{-| Draw all nodes
-}
drawNodes : List Entity -> Maybe Int -> String -> Svg Msg
drawNodes nodes hoveredId mainActivityId =
    let
        unitColors =
            buildUnitColorMap nodes

        maxValue =
            nodes
                |> List.map .value
                |> List.maximum
                |> Maybe.withDefault 1

        nodeRadius entity =
            10 + (abs entity.value / maxValue) * 30

        drawNode node =
            let
                isHovered =
                    hoveredId == Just node.id

                isMainActivity =
                    node.processId == mainActivityId

                nodeColor =
                    if isHovered then
                        "#ff9800"  -- Orange for hover
                    else if isMainActivity then
                        "#00d1b2"  -- Bulma primary color (turquoise) for main activity
                    else
                        getColorForUnit unitColors node.unit

                radius =
                    nodeRadius node
            in
            g
                [ Svg.Events.custom "mousedown"
                    (Decode.map
                        (\mousePos ->
                            { message = DragStart node.id mousePos ( node.x, node.y )
                            , stopPropagation = True
                            , preventDefault = True
                            }
                        )
                        decodeMousePosition
                    )
                , Svg.Events.onMouseOver (NodeHover (Just node.id))
                , Svg.Events.onMouseOut (NodeHover Nothing)
                , Svg.Events.onClick (NodeClick node.id)
                , SvgA.style "cursor: move;"
                ]
                [ circle
                    [ cx (String.fromFloat node.x)
                    , cy (String.fromFloat node.y)
                    , r (String.fromFloat radius)
                    , fill nodeColor
                    , stroke "#fff"
                    , strokeWidth "2"
                    ]
                    []
                , text_
                    [ x (String.fromFloat node.x)
                    , y (String.fromFloat (node.y - radius - 5))
                    , textAnchor "middle"
                    , fontSize "10"
                    , fill "#333"
                    ]
                    [ Svg.text (truncate 30 node.label) ]
                ]
    in
    g [] (List.map drawNode nodes)


{-| Truncate text to max length
-}
truncate : Int -> String -> String
truncate maxLen str =
    if String.length str <= maxLen then
        str

    else
        String.left (maxLen - 3) str ++ "..."


{-| View tooltip for hovered node
-}
viewTooltip : Model -> Html Msg
viewTooltip model =
    case model.hoveredNode of
        Nothing ->
            text ""

        Just nodeId ->
            case List.filter (\n -> n.id == nodeId) model.nodes |> List.head of
                Nothing ->
                    text ""

                Just node ->
                    div
                        [ Html.Attributes.style "position" "absolute"
                        , Html.Attributes.style "top" "10px"
                        , Html.Attributes.style "right" "10px"
                        , Html.Attributes.style "background" "white"
                        , Html.Attributes.style "padding" "10px"
                        , Html.Attributes.style "border" "1px solid #ccc"
                        , Html.Attributes.style "border-radius" "4px"
                        , Html.Attributes.style "box-shadow" "0 2px 8px rgba(0,0,0,0.1)"
                        , Html.Attributes.style "max-width" "300px"
                        ]
                        [ Html.h4 [ Html.Attributes.style "margin" "0 0 0.5rem 0" ]
                            [ text node.label ]
                        , Html.p [ Html.Attributes.style "margin" "0 0 0.25rem 0" ]
                            [ Html.strong [] [ text "Location: " ]
                            , text node.location
                            ]
                        , Html.p [ Html.Attributes.style "margin" "0 0 0.25rem 0" ]
                            [ Html.strong [] [ text "Value: " ]
                            , text (String.fromFloat node.value ++ " " ++ node.unit)
                            ]
                        , Html.p [ Html.Attributes.style "margin" "0" ]
                            [ Html.strong [] [ text "Process ID: " ]
                            , text node.processId
                            ]
                        ]


{-| View zoom controls
-}
viewZoomControls : Model -> Html Msg
viewZoomControls model =
    div
        [ Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "bottom" "20px"
        , Html.Attributes.style "right" "20px"
        , Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        , Html.Attributes.style "gap" "8px"
        , Html.Attributes.style "background" "white"
        , Html.Attributes.style "padding" "8px"
        , Html.Attributes.style "border-radius" "4px"
        , Html.Attributes.style "box-shadow" "0 2px 8px rgba(0,0,0,0.15)"
        ]
        [ Html.button
            [ Html.Attributes.class "button is-small"
            , Html.Events.onClick ZoomIn
            , Html.Attributes.title "Zoom In"
            , Html.Attributes.style "width" "36px"
            , Html.Attributes.style "height" "36px"
            ]
            [ Html.text "+" ]
        , Html.button
            [ Html.Attributes.class "button is-small"
            , Html.Events.onClick ZoomOut
            , Html.Attributes.title "Zoom Out"
            , Html.Attributes.style "width" "36px"
            , Html.Attributes.style "height" "36px"
            ]
            [ Html.text "−" ]
        , Html.button
            [ Html.Attributes.class "button is-small"
            , Html.Events.onClick AutoFit
            , Html.Attributes.title "Fit to View"
            , Html.Attributes.style "width" "36px"
            , Html.Attributes.style "height" "36px"
            ]
            [ Html.text "⊡" ]
        , div
            [ Html.Attributes.style "font-size" "10px"
            , Html.Attributes.style "text-align" "center"
            , Html.Attributes.style "color" "#666"
            ]
            [ Html.text (String.fromInt (round ((svgWidth / model.viewBoxWidth) * 100)) ++ "%") ]
        ]


{-| Subscriptions for force simulation and drag
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    let
        animationSub =
            if ForceDirected.isCompleted model.simulation then
                Sub.none

            else
                Browser.Events.onAnimationFrame (always Tick)

        nodeDragSubs =
            case model.drag of
                Just _ ->
                    [ Browser.Events.onMouseMove (Decode.map DragAt decodeMousePosition)
                    , Browser.Events.onMouseUp (Decode.map DragEnd decodeMousePosition)
                    ]

                Nothing ->
                    []

        panSubs =
            case model.isPanning of
                Just _ ->
                    [ Browser.Events.onMouseMove (Decode.map MoveBackgroundPan decodeMousePosition)
                    , Browser.Events.onMouseUp (Decode.succeed EndBackgroundPan)
                    ]

                Nothing ->
                    []
    in
    Sub.batch (animationSub :: (nodeDragSubs ++ panSubs))


{-| Decode mouse position from event (relative to SVG element)
-}
decodeMousePosition : Decode.Decoder ( Float, Float )
decodeMousePosition =
    Decode.map2 Tuple.pair
        (Decode.field "offsetX" Decode.float)
        (Decode.field "offsetY" Decode.float)
