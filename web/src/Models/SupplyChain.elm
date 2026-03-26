module Models.SupplyChain exposing
    ( SupplyChainEntry
    , SupplyChainResponse
    , supplyChainResponseDecoder
    )

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Models.Activity exposing (ActivitySummary, activitySummaryDecoder)


type alias SupplyChainEntry =
    { processId : String
    , name : String
    , location : String
    , quantity : Float
    , unit : String
    , scalingFactor : Float
    , classifications : Dict String String
    , depth : Int
    , upstreamCount : Int
    }


type alias SupplyChainResponse =
    { totalActivities : Int
    , filteredActivities : Int
    , supplyChain : List SupplyChainEntry
    , root : ActivitySummary
    }


supplyChainResponseDecoder : Decoder SupplyChainResponse
supplyChainResponseDecoder =
    Decode.succeed SupplyChainResponse
        |> required "scrTotalActivities" Decode.int
        |> required "scrFilteredActivities" Decode.int
        |> required "scrSupplyChain" (Decode.list entryDecoder)
        |> required "scrRoot" activitySummaryDecoder


entryDecoder : Decoder SupplyChainEntry
entryDecoder =
    Decode.succeed SupplyChainEntry
        |> required "sceProcessId" Decode.string
        |> required "sceName" Decode.string
        |> required "sceLocation" Decode.string
        |> required "sceQuantity" Decode.float
        |> required "sceUnit" Decode.string
        |> required "sceScalingFactor" Decode.float
        |> required "sceClassifications" (Decode.dict Decode.string)
        |> required "sceDepth" Decode.int
        |> required "sceUpstreamCount" Decode.int
