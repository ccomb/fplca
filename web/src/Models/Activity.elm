module Models.Activity exposing
    ( ActivityTree
    , ActivityNode
    , ActivityEdge
    , FlowInfo
    , NodeType(..)
    , TreeMetadata
    , activityTreeDecoder
    , activityNodeDecoder
    , activityEdgeDecoder
    , flowInfoDecoder
    , nodeTypeDecoder
    , treeMetadataDecoder
    )

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required, optional)


type alias ActivityTree =
    { tree : TreeMetadata
    , nodes : Dict String ActivityNode
    , edges : List ActivityEdge
    }


type alias TreeMetadata =
    { rootId : String
    , maxDepth : Int
    , totalNodes : Int
    , loopNodes : Int
    , leafNodes : Int
    , expandableNodes : Int
    }


type alias ActivityNode =
    { id : String
    , name : String
    , description : List String
    , location : String
    , unit : String
    , nodeType : NodeType
    , depth : Int
    , loopTarget : Maybe String
    , parentId : Maybe String
    , childrenCount : Int
    }


type NodeType
    = ActivityNodeType
    | LoopNodeType


type alias ActivityEdge =
    { from : String
    , to : String
    , flow : FlowInfo
    , quantity : Float
    , unit : String
    }


type alias FlowInfo =
    { id : String
    , name : String
    , category : String
    }



-- JSON Decoders


activityTreeDecoder : Decoder ActivityTree
activityTreeDecoder =
    Decode.succeed ActivityTree
        |> required "teTree" treeMetadataDecoder
        |> required "teNodes" (Decode.dict activityNodeDecoder)
        |> required "teEdges" (Decode.list activityEdgeDecoder)


treeMetadataDecoder : Decoder TreeMetadata
treeMetadataDecoder =
    Decode.succeed TreeMetadata
        |> required "tmRootId" Decode.string
        |> required "tmMaxDepth" Decode.int
        |> required "tmTotalNodes" Decode.int
        |> required "tmLoopNodes" Decode.int
        |> required "tmLeafNodes" Decode.int
        |> required "tmExpandableNodes" Decode.int


activityNodeDecoder : Decoder ActivityNode
activityNodeDecoder =
    Decode.succeed ActivityNode
        |> required "enId" Decode.string
        |> required "enName" Decode.string
        |> required "enDescription" (Decode.list Decode.string)
        |> required "enLocation" Decode.string
        |> required "enUnit" Decode.string
        |> required "enNodeType" nodeTypeDecoder
        |> required "enDepth" Decode.int
        |> required "enLoopTarget" (Decode.nullable Decode.string)
        |> required "enParentId" (Decode.nullable Decode.string)
        |> required "enChildrenCount" Decode.int


nodeTypeDecoder : Decoder NodeType
nodeTypeDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "ActivityNode" ->
                        Decode.succeed ActivityNodeType

                    "LoopNode" ->
                        Decode.succeed LoopNodeType

                    _ ->
                        Decode.fail ("Unknown node type: " ++ str)
            )


activityEdgeDecoder : Decoder ActivityEdge
activityEdgeDecoder =
    Decode.succeed ActivityEdge
        |> required "teFrom" Decode.string
        |> required "teTo" Decode.string
        |> required "teFlow" flowInfoDecoder
        |> required "teQuantity" Decode.float
        |> required "teUnit" Decode.string


flowInfoDecoder : Decoder FlowInfo
flowInfoDecoder =
    Decode.succeed FlowInfo
        |> required "fiId" Decode.string
        |> required "fiName" Decode.string
        |> required "fiCategory" Decode.string