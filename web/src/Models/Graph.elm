module Models.Graph exposing
    ( GraphData
    , GraphNode
    , GraphEdge
    , graphDataDecoder
    , graphNodeDecoder
    , graphEdgeDecoder
    )

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)


{-| Complete graph data from backend
-}
type alias GraphData =
    { nodes : List GraphNode
    , edges : List GraphEdge
    , unitGroups : Dict String String
    }


{-| Graph node representing an activity
-}
type alias GraphNode =
    { id : Int
    , label : String
    , value : Float
    , unit : String
    , processId : String
    , location : String
    }


{-| Graph edge representing a flow between activities
-}
type alias GraphEdge =
    { source : Int
    , target : Int
    , value : Float
    , unit : String
    , flowName : String
    }


{-| JSON decoder for GraphData
-}
graphDataDecoder : Decoder GraphData
graphDataDecoder =
    Decode.succeed GraphData
        |> required "geNodes" (Decode.list graphNodeDecoder)
        |> required "geEdges" (Decode.list graphEdgeDecoder)
        |> required "geUnitGroups" (Decode.dict Decode.string)


{-| JSON decoder for GraphNode
-}
graphNodeDecoder : Decoder GraphNode
graphNodeDecoder =
    Decode.succeed GraphNode
        |> required "gnId" Decode.int
        |> required "gnLabel" Decode.string
        |> required "gnValue" Decode.float
        |> required "gnUnit" Decode.string
        |> required "gnProcessId" Decode.string
        |> required "gnLocation" Decode.string


{-| JSON decoder for GraphEdge
-}
graphEdgeDecoder : Decoder GraphEdge
graphEdgeDecoder =
    Decode.succeed GraphEdge
        |> required "geSource" Decode.int
        |> required "geTarget" Decode.int
        |> required "geValue" Decode.float
        |> required "geUnit" Decode.string
        |> required "geFlowName" Decode.string
