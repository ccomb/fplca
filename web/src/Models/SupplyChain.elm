module Models.SupplyChain exposing
    ( SupplyChainEntry
    , SupplyChainResponse
    , supplyChainResponseDecoder
    )

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)


type alias SupplyChainEntry =
    { processId : String
    , name : String
    , location : String
    , quantity : Float
    , unit : String
    , scalingFactor : Float
    , classifications : Dict String String
    }


type alias SupplyChainResponse =
    { totalActivities : Int
    , filteredActivities : Int
    , supplyChain : List SupplyChainEntry
    }


supplyChainResponseDecoder : Decoder SupplyChainResponse
supplyChainResponseDecoder =
    Decode.map3 SupplyChainResponse
        (Decode.field "scrTotalActivities" Decode.int)
        (Decode.field "scrFilteredActivities" Decode.int)
        (Decode.field "scrSupplyChain" (Decode.list entryDecoder))


entryDecoder : Decoder SupplyChainEntry
entryDecoder =
    Decode.map7 SupplyChainEntry
        (Decode.field "sceProcessId" Decode.string)
        (Decode.field "sceName" Decode.string)
        (Decode.field "sceLocation" Decode.string)
        (Decode.field "sceQuantity" Decode.float)
        (Decode.field "sceUnit" Decode.string)
        (Decode.field "sceScalingFactor" Decode.float)
        (Decode.field "sceClassifications" (Decode.dict Decode.string))
