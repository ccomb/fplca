module Views.GraphView exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes
import Json.Decode as Decode
import Models.Graph exposing (GraphData, GraphEdge, GraphNode)
import Svg exposing (Svg, circle, defs, g, line, marker, polygon, svg, text_)
import Svg.Attributes as SvgA exposing (class, cx, cy, d, fill, fontSize, height, id, markerEnd, markerHeight, markerWidth, orient, points, r, refX, refY, stroke, strokeWidth, textAnchor, viewBox, width, x, x1, x2, y, y1, y2)
import Svg.Events


{-| Model for the graph visualization - uses circular layout to avoid Force module conflict
-}
type alias Model =
    { nodes : List Entity
    , edges : List Link
    , hoveredNode : Maybe Int
    , selectedNode : Maybe Int
    }


{-| Entity represents a node with fixed circular layout position
-}
type alias Entity =
    { id : Int
    , x : Float
    , y : Float
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


{-| Messages for graph interactions
-}
type Msg
    = NodeHover (Maybe Int)
    | NodeClick Int


{-| Initialize the graph model with circular layout
-}
init : GraphData -> Model
init graphData =
    let
        nodeCount =
            List.length graphData.nodes

        -- Calculate circular layout positions
        nodes =
            graphData.nodes
                |> List.indexedMap
                    (\index gn ->
                        let
                            angle =
                                (toFloat index / toFloat nodeCount) * 2 * pi

                            radius =
                                min (svgWidth / 2 - 100) (svgHeight / 2 - 100)

                            x =
                                svgWidth / 2 + radius * cos angle

                            y =
                                svgHeight / 2 + radius * sin angle
                        in
                        { id = gn.id
                        , x = x
                        , y = y
                        , value = gn.value
                        , label = gn.label
                        , unit = gn.unit
                        , processId = gn.processId
                        , location = gn.location
                        }
                    )

        -- Convert edges
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
    in
    { nodes = nodes
    , edges = edges
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


{-| Update function - static layout so only handles hover and click
-}
update : Msg -> Model -> Model
update msg model =
    case msg of
        NodeHover maybeId ->
            { model | hoveredNode = maybeId }

        NodeClick nodeId ->
            { model | selectedNode = Just nodeId }


{-| View function
-}
view : Model -> Html Msg
view model =
    div [ Html.Attributes.style "position" "relative" ]
        [ svg
            [ width (String.fromFloat svgWidth)
            , height (String.fromFloat svgHeight)
            , viewBox ("0 0 " ++ String.fromFloat svgWidth ++ " " ++ String.fromFloat svgHeight)
            , SvgA.style "border: 1px solid #ccc"
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
            , drawNodes model.nodes model.hoveredNode
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
drawNodes : List Entity -> Maybe Int -> Svg Msg
drawNodes nodes hoveredId =
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

                radius =
                    nodeRadius node
            in
            g
                [ Svg.Events.onMouseOver (NodeHover (Just node.id))
                , Svg.Events.onMouseOut (NodeHover Nothing)
                , Svg.Events.onClick (NodeClick node.id)
                , SvgA.style "cursor: pointer;"
                ]
                [ circle
                    [ cx (String.fromFloat node.x)
                    , cy (String.fromFloat node.y)
                    , r (String.fromFloat radius)
                    , fill (if isHovered then "#ff9800" else "#2196f3")
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


{-| Subscriptions - none needed for static layout
-}
subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
