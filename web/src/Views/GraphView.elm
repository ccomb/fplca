module Views.GraphView exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Events
import Dict exposing (Dict)
import ForceDirected
import Html exposing (Html, div, text)
import Html.Attributes
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
                            angle =
                                (toFloat index * 2.4) * pi

                            radius =
                                100 + (toFloat (modBy 5 index) * 50)

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
                        , distance = 100
                        , strength = Nothing
                        }
                    )

        forces =
            [ ForceDirected.links links
            , ForceDirected.manyBodyStrength -100 <| List.map .id nodes
            , ForceDirected.center (svgWidth / 2) (svgHeight / 2)
            ]

        simulation =
            ForceDirected.simulation forces
    in
    { nodes = nodes
    , edges = edges
    , simulation = simulation
    , drag = Nothing
    , hoveredNode = Nothing
    , selectedNode = Nothing
    }


{-| SVG dimensions
-}
svgWidth : Float
svgWidth =
    1400


svgHeight : Float
svgHeight =
    900


{-| Update function with force simulation
-}
update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick ->
            let
                ( newSimulation, newNodes ) =
                    ForceDirected.tick model.simulation model.nodes
            in
            case model.drag of
                Nothing ->
                    { model
                        | simulation = newSimulation
                        , nodes = newNodes
                    }

                Just drag ->
                    let
                        -- Calculate delta and apply to node
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
    div [ Html.Attributes.style "position" "relative" ]
        [ svg
            [ width "100%"
            , height "100%"
            , viewBox ("0 0 " ++ String.fromFloat svgWidth ++ " " ++ String.fromFloat svgHeight)
            , SvgA.style "border: 1px solid #ccc; background-color: white; display: block;"
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
        ]


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


{-| Draw all nodes
-}
drawNodes : List Entity -> Maybe Int -> String -> Svg Msg
drawNodes nodes hoveredId mainActivityId =
    let
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
                        "#ff9800"
                    else if isMainActivity then
                        "#4caf50"  -- Green for main activity
                    else
                        "#2196f3"  -- Blue for other activities

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


{-| Subscriptions for force simulation and drag
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    case model.drag of
        Nothing ->
            -- If not dragging, subscribe to animation frames for force simulation
            if ForceDirected.isCompleted model.simulation then
                Sub.none

            else
                Browser.Events.onAnimationFrame (always Tick)

        Just _ ->
            -- If dragging, subscribe to mouse events and continue simulation
            Sub.batch
                [ Browser.Events.onMouseMove (Decode.map DragAt decodeMousePosition)
                , Browser.Events.onMouseUp (Decode.map DragEnd decodeMousePosition)
                , Browser.Events.onAnimationFrame (always Tick)
                ]


{-| Decode mouse position from event (relative to SVG element)
-}
decodeMousePosition : Decode.Decoder ( Float, Float )
decodeMousePosition =
    Decode.map2 Tuple.pair
        (Decode.field "offsetX" Decode.float)
        (Decode.field "offsetY" Decode.float)
