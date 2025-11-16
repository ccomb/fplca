module Views.TreeView exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser.Events
import Dict exposing (Dict)
import ForceDirected
import Html exposing (Html, div, strong, text)
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Models.Activity exposing (ActivityNode, ActivityTree, EdgeType(..), NodeType(..))
import Set exposing (Set)
import Svg exposing (Svg, defs, g, line, marker, polygon, rect, svg, text_)
import Svg.Attributes as SvgA exposing (fill, fontSize, height, id, markerEnd, markerHeight, markerWidth, orient, points, refX, refY, stroke, strokeWidth, textAnchor, viewBox, width, x, x1, x2, y, y1, y2)
import Svg.Events
import Utils.Format as Format


{-| Model for the tree visualization with force-directed layout
-}
type alias Model =
    { nodes : List Entity
    , edges : List Link
    , simulation : ForceDirected.State Int
    , drag : Maybe Drag
    , hoveredNode : Maybe Int
    , selectedNode : Maybe Int
    , idMapping : Dict Int String -- Int → ProcessId String
    , reverseIdMapping : Dict String Int -- ProcessId String → Int
    , activityTree : ActivityTree -- Keep reference to original tree data
    , viewBoxX : Float
    , viewBoxY : Float
    , viewBoxWidth : Float
    , viewBoxHeight : Float
    , isPanning : Maybe ( Float, Float ) -- Track background pan drag start position
    , nodeCountWarning : Maybe String -- Warning if too many nodes
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
    , nodeType : NodeType
    , compartment : Maybe String
    , depth : Int
    , childrenCount : Int
    , description : List String
    }


{-| Link represents an edge in the graph
-}
type alias Link =
    { source : Int
    , target : Int
    , value : Float
    , unit : String
    , edgeType : EdgeType
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
    | DragStart Int ( Float, Float ) ( Float, Float )
    | DragAt ( Float, Float )
    | DragEnd ( Float, Float )
    | NodeHover (Maybe Int)
    | NodeClick Int
    | NodeDoubleClick Int
    | ZoomIn
    | ZoomOut
    | ResetZoom
    | AutoFit
    | StartBackgroundPan ( Float, Float )
    | MoveBackgroundPan ( Float, Float )
    | EndBackgroundPan
    | WheelZoom Float


{-| Initialize the tree model with force-directed layout
-}
init : ActivityTree -> Model
init activityTree =
    let
        -- Convert ActivityNodes to indexed entities
        nodesList =
            Dict.values activityTree.nodes

        ( entities, idMapping, reverseIdMapping ) =
            createEntitiesWithMapping nodesList

        -- Convert edges to links using the ID mapping
        edges =
            activityTree.edges
                |> List.filterMap
                    (\edge ->
                        Maybe.map2
                            (\sourceId targetId ->
                                { source = sourceId
                                , target = targetId
                                , value = edge.quantity
                                , unit = edge.unit
                                , edgeType = edge.edgeType
                                }
                            )
                            (Dict.get edge.from reverseIdMapping)
                            (Dict.get edge.to reverseIdMapping)
                    )

        -- Helper to find entity by ID
        findEntityById : Int -> Maybe Entity
        findEntityById id =
            List.filter (\e -> e.id == id) entities
                |> List.head

        -- Create force simulation with depth-aware link distances
        links =
            edges
                |> List.filterMap
                    (\edge ->
                        Maybe.map2
                            (\sourceEntity targetEntity ->
                                let
                                    -- Calculate distance based on depth relationship
                                    depthDiff =
                                        abs (sourceEntity.depth - targetEntity.depth)

                                    -- Parent-child edges should be shorter to maintain hierarchy
                                    -- Same-depth edges should be longer to prevent crossing
                                    distance =
                                        if depthDiff == 1 then
                                            200

                                        else if depthDiff == 0 then
                                            300

                                        else
                                            350

                                    -- Stronger links maintain hierarchy better
                                    strength =
                                        Just 0.8
                                in
                                { source = edge.source
                                , target = edge.target
                                , distance = distance
                                , strength = strength
                                }
                            )
                            (findEntityById edge.source)
                            (findEntityById edge.target)
                    )

        forces =
            [ ForceDirected.links links
            , ForceDirected.manyBodyStrength -50000 <| List.map .id entities -- Stronger repulsion to spread nodes
            , ForceDirected.center (svgWidth / 2) (svgHeight / 2)
            ]

        simulation =
            ForceDirected.simulation forces

        -- Check if node count is too high
        nodeCount =
            List.length entities

        maxNodes =
            150

        warning =
            if nodeCount > maxNodes then
                Just
                    ("⚠️ Large tree with "
                        ++ String.fromInt nodeCount
                        ++ " nodes may cause performance issues. Consider reducing tree depth or using a smaller activity."
                    )

            else
                Nothing
    in
    { nodes = entities
    , edges = edges
    , simulation = simulation
    , drag = Nothing
    , hoveredNode = Nothing
    , selectedNode = Nothing
    , idMapping = idMapping
    , reverseIdMapping = reverseIdMapping
    , activityTree = activityTree
    , viewBoxX = 0
    , viewBoxY = 0
    , viewBoxWidth = svgWidth
    , viewBoxHeight = svgHeight
    , isPanning = Nothing
    , nodeCountWarning = warning
    }


{-| Create entities with ID mapping (String ProcessId → Int)
Uses hierarchical circular layout: root at center, depth 1 in circle, depth 2 in mini-circles around parents
-}
createEntitiesWithMapping : List ActivityNode -> ( List Entity, Dict Int String, Dict String Int )
createEntitiesWithMapping nodesList =
    let
        -- Build a map of parent ID to children nodes for hierarchical positioning
        childrenByParent : Dict.Dict String (List ( Int, ActivityNode ))
        childrenByParent =
            List.indexedMap Tuple.pair nodesList
                |> List.foldl
                    (\( idx, node ) acc ->
                        case node.parentId of
                            Just parentId ->
                                Dict.update parentId
                                    (\maybeChildren ->
                                        Just (( idx, node ) :: Maybe.withDefault [] maybeChildren)
                                    )
                                    acc

                            Nothing ->
                                acc
                    )
                    Dict.empty

        -- Calculate hierarchical position for each node with cycle detection
        calculatePosition : Set.Set String -> ActivityNode -> ( Float, Float )
        calculatePosition visited node =
            let
                centerX =
                    svgWidth / 2

                centerY =
                    svgHeight / 2
            in
            if node.depth == 0 then
                -- Root node at center
                ( centerX, centerY )

            else if node.depth == 1 then
                -- Depth 1 nodes in circle around center
                let
                    depth1Nodes =
                        List.filter (\n -> n.depth == 1) nodesList

                    nodesCount =
                        List.length depth1Nodes

                    positionIndex =
                        List.indexedMap Tuple.pair depth1Nodes
                            |> List.filter (\( _, n ) -> n.id == node.id)
                            |> List.head
                            |> Maybe.map Tuple.first
                            |> Maybe.withDefault 0

                    angle =
                        if nodesCount > 0 then
                            2 * pi * toFloat positionIndex / toFloat nodesCount

                        else
                            0

                    radius =
                        300
                in
                ( centerX + radius * cos angle, centerY + radius * sin angle )

            else
                -- Depth 2+ nodes in mini-circles around their parent
                case node.parentId of
                    Nothing ->
                        -- Fallback if no parent
                        ( centerX, centerY )

                    Just parentId ->
                        let
                            -- Check if we've already visited this parent (cycle detection)
                            ( parentX, parentY ) =
                                if Set.member parentId visited then
                                    -- Cycle detected, use center as fallback
                                    ( centerX, centerY )

                                else
                                    -- Find parent position
                                    let
                                        parentNode =
                                            List.filter (\n -> n.id == parentId) nodesList
                                                |> List.head
                                    in
                                    case parentNode of
                                        Just parent ->
                                            -- Add current node to visited set before recursing
                                            calculatePosition (Set.insert node.id visited) parent

                                        Nothing ->
                                            ( centerX, centerY )

                            -- Get siblings (children of same parent)
                            siblings =
                                Dict.get parentId childrenByParent
                                    |> Maybe.withDefault []

                            siblingsCount =
                                List.length siblings

                            positionIndex =
                                siblings
                                    |> List.indexedMap (\i ( _, n ) -> ( i, n ))
                                    |> List.filter (\( _, n ) -> n.id == node.id)
                                    |> List.head
                                    |> Maybe.map Tuple.first
                                    |> Maybe.withDefault 0

                            angle =
                                if siblingsCount > 0 then
                                    2 * pi * toFloat positionIndex / toFloat siblingsCount

                                else
                                    0

                            -- Mini-circle radius around parent
                            miniRadius =
                                150
                        in
                        ( parentX + miniRadius * cos angle, parentY + miniRadius * sin angle )

        indexed =
            List.indexedMap
                (\index node ->
                    let
                        ( x, y ) =
                            calculatePosition Set.empty node
                    in
                    ( index
                    , { id = index
                      , x = x
                      , y = y
                      , vx = 0
                      , vy = 0
                      , value = 1.0 -- Tree doesn't have cumulative values
                      , label = node.name
                      , unit = node.unit
                      , processId = node.id
                      , location = node.location
                      , nodeType = node.nodeType
                      , compartment = node.compartment
                      , depth = node.depth
                      , childrenCount = node.childrenCount
                      , description = node.description
                      }
                    , node.id
                    )
                )
                nodesList

        entities =
            List.map (\( _, entity, _ ) -> entity) indexed

        idMapping =
            List.map (\( intId, _, stringId ) -> ( intId, stringId )) indexed
                |> Dict.fromList

        reverseIdMapping =
            List.map (\( intId, _, stringId ) -> ( stringId, intId )) indexed
                |> Dict.fromList
    in
    ( entities, idMapping, reverseIdMapping )


{-| SVG dimensions
-}
svgWidth : Float
svgWidth =
    1400


svgHeight : Float
svgHeight =
    900


{-| Calculate bounding box of all nodes, filtering out invalid positions
-}
calculateBoundingBox : List Entity -> { minX : Float, minY : Float, maxX : Float, maxY : Float }
calculateBoundingBox nodes =
    let
        nodeRadius =
            25

        margin =
            50

        -- Filter out nodes with NaN or Infinity positions
        validNodes =
            List.filter (\n -> not (isNaN n.x || isInfinite n.x || isNaN n.y || isInfinite n.y)) nodes

        xs =
            List.map .x validNodes

        ys =
            List.map .y validNodes

        -- Use sensible defaults if no valid nodes exist
        minX =
            (List.minimum xs |> Maybe.withDefault (svgWidth / 2 - 200)) - nodeRadius - margin

        maxX =
            (List.maximum xs |> Maybe.withDefault (svgWidth / 2 + 200)) + nodeRadius + margin

        minY =
            (List.minimum ys |> Maybe.withDefault (svgHeight / 2 - 200)) - nodeRadius - margin

        maxY =
            (List.maximum ys |> Maybe.withDefault (svgHeight / 2 + 200)) + nodeRadius + margin
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

        NodeDoubleClick nodeId ->
            -- This will be handled by Main.elm to trigger navigation
            model

        ZoomIn ->
            let
                zoomFactor =
                    0.8

                newWidth =
                    model.viewBoxWidth * zoomFactor

                newHeight =
                    model.viewBoxHeight * zoomFactor

                -- Keep center position
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

                -- Keep center position
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

                -- Keep center position
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


{-| View function - takes mainActivityId to highlight it
-}
view : String -> Model -> Html Msg
view mainActivityId model =
    div
        [ Html.Attributes.style "position" "relative"
        , Html.Attributes.style "height" "calc(100vh - 122px)"
        , Html.Attributes.style "overflow" "hidden"
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
        [ case model.nodeCountWarning of
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
            , drawNodes model.nodes model.hoveredNode model.selectedNode mainActivityId
            ]
        , viewTooltip model
        , viewZoomControls model
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


{-| Get color for a unit using a color palette
-}
getColorForUnit : Dict String String -> String -> String
getColorForUnit unitColors unit =
    Dict.get unit unitColors
        |> Maybe.withDefault "#3273dc"


{-| Build a color map for all unique units
-}
buildUnitColorMap : List Entity -> Dict String String
buildUnitColorMap nodes =
    let
        colorPalette =
            [ "#3273dc" -- Blue
            , "#48c774" -- Green
            , "#ffdd57" -- Yellow
            , "#f14668" -- Red
            , "#b5a7d6" -- Purple
            , "#f093a2" -- Pink
            , "#4ecdc4" -- Teal
            , "#ff6b6b" -- Coral
            , "#95e1d3" -- Mint
            , "#f38181" -- Salmon
            ]

        uniqueUnits =
            nodes
                |> List.map .unit
                |> List.sortBy identity
                |> List.foldl
                    (\u acc ->
                        if List.member u acc then
                            acc

                        else
                            u :: acc
                    )
                    []
                |> List.reverse

        unitColorPairs =
            List.map2 (\unit color -> ( unit, color ))
                uniqueUnits
                (List.repeat (List.length uniqueUnits) colorPalette |> List.concat)
    in
    Dict.fromList unitColorPairs


{-| Get compartment-based color for biosphere emissions
-}
getCompartmentColor : Maybe String -> ( String, String )
getCompartmentColor compartment =
    case compartment of
        Just "air" ->
            ( "#e1f5fe", "#03a9f4" )

        Just "water" ->
            ( "#b3e5fc", "#0277bd" )

        Just "soil" ->
            ( "#d7ccc8", "#6d4c41" )

        _ ->
            ( "#ffebee", "#e53935" )


{-| Draw all nodes with different shapes based on nodeType
-}
drawNodes : List Entity -> Maybe Int -> Maybe Int -> String -> Svg Msg
drawNodes nodes hoveredId selectedId mainActivityId =
    let
        unitColors =
            buildUnitColorMap nodes

        drawNode node =
            let
                isHovered =
                    hoveredId == Just node.id

                isSelected =
                    selectedId == Just node.id

                isMainActivity =
                    node.processId == mainActivityId

                ( nodeColor, strokeColor ) =
                    case node.nodeType of
                        ActivityNodeType ->
                            if isHovered then
                                ( "#ff9800", "#f57c00" )

                            else if isMainActivity then
                                ( "#00d1b2", "#00b89c" )

                            else
                                let
                                    baseColor =
                                        getColorForUnit unitColors node.unit
                                in
                                ( baseColor, "#666" )

                        LoopNodeType ->
                            ( "#fff3e0", "#ff9800" )

                        BiosphereEmissionNodeType ->
                            getCompartmentColor node.compartment

                        BiosphereResourceNodeType ->
                            ( "#e8f5e9", "#4caf50" )

                radius =
                    25

                selectedStroke =
                    if isSelected then
                        "4"

                    else
                        "2"

                ( shapeElement, maybeTextElement ) =
                    drawNodeShape node.nodeType ( node.x, node.y ) radius nodeColor strokeColor selectedStroke node.label

                -- For biosphere nodes (which don't have text inside), show label above
                labelElement =
                    case maybeTextElement of
                        Just textEl ->
                            textEl

                        Nothing ->
                            text_
                                [ x (String.fromFloat node.x)
                                , y (String.fromFloat (node.y - radius - 5))
                                , textAnchor "middle"
                                , fontSize "10"
                                , fill "#333"
                                ]
                                [ Svg.text (truncate 30 node.label) ]
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
                , Svg.Events.custom "dblclick"
                    (Decode.succeed
                        { message = NodeDoubleClick node.id
                        , stopPropagation = True
                        , preventDefault = True
                        }
                    )
                , SvgA.style "cursor: move;"
                ]
                [ shapeElement
                , labelElement
                ]
    in
    g [] (List.map drawNode nodes)


{-| Draw node shape based on nodeType
For Activity and Loop nodes, returns (shape, Just text) where text should be rendered
For biosphere nodes, returns (shape, Nothing) since they keep their original shapes
-}
drawNodeShape : NodeType -> ( Float, Float ) -> Float -> String -> String -> String -> String -> ( Svg msg, Maybe (Svg msg) )
drawNodeShape nodeType ( posX, posY ) radius fillColor strokeColor strokeW label =
    case nodeType of
        ActivityNodeType ->
            let
                -- Calculate text width with better estimation
                charWidth =
                    5.5

                -- Character width estimation
                padding =
                    0

                -- No padding
                textWidth =
                    toFloat (String.length label) * charWidth + padding

                rectHeight =
                    16.0

                rectX =
                    posX - textWidth / 2

                rectY =
                    posY - rectHeight / 2

                rectShape =
                    rect
                        [ SvgA.x (String.fromFloat rectX)
                        , SvgA.y (String.fromFloat rectY)
                        , SvgA.width (String.fromFloat textWidth)
                        , SvgA.height (String.fromFloat rectHeight)
                        , fill fillColor
                        , stroke strokeColor
                        , strokeWidth strokeW
                        , SvgA.rx "3"
                        ]
                        []

                textElement =
                    text_
                        [ x (String.fromFloat posX)
                        , y (String.fromFloat posY)
                        , textAnchor "middle"
                        , SvgA.dominantBaseline "middle"
                        , fontSize "11"
                        , fill "#333"
                        , SvgA.style "pointer-events: none;"
                        ]
                        [ Svg.text label ]
            in
            ( rectShape, Just textElement )

        LoopNodeType ->
            let
                -- Calculate text width with better estimation
                charWidth =
                    5.5

                -- Character width estimation
                padding =
                    0

                -- No padding
                textWidth =
                    toFloat (String.length label) * charWidth + padding

                rectHeight =
                    16.0

                rectX =
                    posX - textWidth / 2

                rectY =
                    posY - rectHeight / 2

                rectShape =
                    rect
                        [ SvgA.x (String.fromFloat rectX)
                        , SvgA.y (String.fromFloat rectY)
                        , SvgA.width (String.fromFloat textWidth)
                        , SvgA.height (String.fromFloat rectHeight)
                        , fill fillColor
                        , stroke strokeColor
                        , strokeWidth strokeW
                        , SvgA.rx "3"
                        ]
                        []

                textElement =
                    text_
                        [ x (String.fromFloat posX)
                        , y (String.fromFloat posY)
                        , textAnchor "middle"
                        , SvgA.dominantBaseline "middle"
                        , fontSize "11"
                        , fill "#333"
                        , SvgA.style "pointer-events: none;"
                        ]
                        [ Svg.text label ]
            in
            ( rectShape, Just textElement )

        BiosphereEmissionNodeType ->
            let
                size =
                    radius * 1.2

                pointsStr =
                    String.fromFloat posX
                        ++ ","
                        ++ String.fromFloat (posY - size)
                        ++ " "
                        ++ String.fromFloat (posX + size)
                        ++ ","
                        ++ String.fromFloat posY
                        ++ " "
                        ++ String.fromFloat posX
                        ++ ","
                        ++ String.fromFloat (posY + size)
                        ++ " "
                        ++ String.fromFloat (posX - size)
                        ++ ","
                        ++ String.fromFloat posY
            in
            ( polygon
                [ SvgA.points pointsStr
                , fill fillColor
                , stroke strokeColor
                , strokeWidth strokeW
                ]
                []
            , Nothing
            )

        BiosphereResourceNodeType ->
            let
                size =
                    radius * 1.4

                halfSize =
                    size / 2
            in
            ( rect
                [ SvgA.x (String.fromFloat (posX - halfSize))
                , SvgA.y (String.fromFloat (posY - halfSize))
                , SvgA.width (String.fromFloat size)
                , SvgA.height (String.fromFloat size)
                , fill fillColor
                , stroke strokeColor
                , strokeWidth strokeW
                ]
                []
            , Nothing
            )


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
                            [ strong [] [ text "Location: " ]
                            , text node.location
                            ]
                        , Html.p [ Html.Attributes.style "margin" "0 0 0.25rem 0" ]
                            [ strong [] [ text "Amount: " ]
                            , text (Format.formatScientific node.value ++ " " ++ node.unit)
                            ]
                        , Html.p [ Html.Attributes.style "margin" "0 0 0.25rem 0" ]
                            [ strong [] [ text "Depth: " ]
                            , text (String.fromInt node.depth)
                            ]
                        , Html.p [ Html.Attributes.style "margin" "0 0 0.25rem 0" ]
                            [ strong [] [ text "Children: " ]
                            , text (String.fromInt node.childrenCount)
                            ]
                        , Html.p [ Html.Attributes.style "margin" "0" ]
                            [ strong [] [ text "Process ID: " ]
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


{-| Decode mouse position from event
-}
decodeMousePosition : Decode.Decoder ( Float, Float )
decodeMousePosition =
    Decode.map2 Tuple.pair
        (Decode.field "offsetX" Decode.float)
        (Decode.field "offsetY" Decode.float)
