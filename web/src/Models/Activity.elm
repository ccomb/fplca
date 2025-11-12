module Models.Activity exposing
    ( ActivityTree
    , ActivityNode
    , ActivityEdge
    , FlowInfo
    , NodeType(..)
    , EdgeType(..)
    , TreeMetadata
    , ActivitySummary
    , SearchResults
    , activityTreeDecoder
    , activityNodeDecoder
    , activityEdgeDecoder
    , flowInfoDecoder
    , nodeTypeDecoder
    , edgeTypeDecoder
    , treeMetadataDecoder
    , activitySummaryDecoder
    , searchResultsDecoder
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
    , compartment : Maybe String
    }


type NodeType
    = ActivityNodeType
    | LoopNodeType
    | BiosphereEmissionNodeType
    | BiosphereResourceNodeType


type EdgeType
    = TechnosphereEdgeType
    | BiosphereEmissionEdgeType
    | BiosphereResourceEdgeType


type alias ActivityEdge =
    { from : String
    , to : String
    , flow : FlowInfo
    , quantity : Float
    , unit : String
    , edgeType : EdgeType
    }


type alias FlowInfo =
    { id : String
    , name : String
    , category : String
    }


type alias ActivitySummary =
    { id : String
    , name : String
    , location : String
    }


type alias SearchResults a =
    { results : List a
    , totalCount : Int
    , offset : Int
    , limit : Int
    , hasMore : Bool
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
        |> required "enCompartment" (Decode.nullable Decode.string)


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

                    "BiosphereEmissionNode" ->
                        Decode.succeed BiosphereEmissionNodeType

                    "BiosphereResourceNode" ->
                        Decode.succeed BiosphereResourceNodeType

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
        |> required "teEdgeType" edgeTypeDecoder


edgeTypeDecoder : Decoder EdgeType
edgeTypeDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "TechnosphereEdge" ->
                        Decode.succeed TechnosphereEdgeType

                    "BiosphereEmissionEdge" ->
                        Decode.succeed BiosphereEmissionEdgeType

                    "BiosphereResourceEdge" ->
                        Decode.succeed BiosphereResourceEdgeType

                    _ ->
                        Decode.fail ("Unknown edge type: " ++ str)
            )


flowInfoDecoder : Decoder FlowInfo
flowInfoDecoder =
    Decode.succeed FlowInfo
        |> required "fiId" Decode.string
        |> required "fiName" Decode.string
        |> required "fiCategory" Decode.string


activitySummaryDecoder : Decoder ActivitySummary
activitySummaryDecoder =
    Decode.succeed ActivitySummary
        |> required "prsId" Decode.string
        |> required "prsName" Decode.string
        |> required "prsLocation" Decode.string


searchResultsDecoder : Decoder a -> Decoder (SearchResults a)
searchResultsDecoder itemDecoder =
    Decode.succeed SearchResults
        |> required "srResults" (Decode.list itemDecoder)
        |> required "srTotal" Decode.int
        |> required "srOffset" Decode.int
        |> required "srLimit" Decode.int
        |> required "srHasMore" Decode.bool