module Models.LCIA exposing
    ( LCIAResult
    , MappingStatus
    , MethodSummary
    , UnmappedFlow
    , lciaResultDecoder
    , mappingStatusDecoder
    , methodSummaryDecoder
    , methodsListDecoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)


{-| Summary of an LCIA method (for listing)
-}
type alias MethodSummary =
    { msmId : String
    , msmName : String
    , msmCategory : String
    , msmUnit : String
    , msmFactorCount : Int
    }


{-| LCIA computation result
-}
type alias LCIAResult =
    { lrMethodId : String
    , lrMethodName : String
    , lrCategory : String
    , lrScore : Float
    , lrUnit : String
    , lrMappedFlows : Int
    , lrUnmappedFlows : Int
    }


{-| Flow mapping status for a method
-}
type alias MappingStatus =
    { mstMethodId : String
    , mstMethodName : String
    , mstTotalFactors : Int
    , mstMappedByUUID : Int
    , mstMappedByName : Int
    , mstMappedBySynonym : Int
    , mstUnmapped : Int
    , mstCoverage : Float
    , mstUnmappedFlows : List UnmappedFlow
    }


{-| Details about an unmapped flow
-}
type alias UnmappedFlow =
    { ufaFlowRef : String
    , ufaFlowName : String
    , ufaDirection : String
    }



-- Decoders


methodSummaryDecoder : Decoder MethodSummary
methodSummaryDecoder =
    D.succeed MethodSummary
        |> required "msmId" D.string
        |> required "msmName" D.string
        |> required "msmCategory" D.string
        |> required "msmUnit" D.string
        |> required "msmFactorCount" D.int


methodsListDecoder : Decoder (List MethodSummary)
methodsListDecoder =
    D.list methodSummaryDecoder


lciaResultDecoder : Decoder LCIAResult
lciaResultDecoder =
    D.succeed LCIAResult
        |> required "lrMethodId" D.string
        |> required "lrMethodName" D.string
        |> required "lrCategory" D.string
        |> required "lrScore" D.float
        |> required "lrUnit" D.string
        |> required "lrMappedFlows" D.int
        |> required "lrUnmappedFlows" D.int


unmappedFlowDecoder : Decoder UnmappedFlow
unmappedFlowDecoder =
    D.succeed UnmappedFlow
        |> required "ufaFlowRef" D.string
        |> required "ufaFlowName" D.string
        |> required "ufaDirection" D.string


mappingStatusDecoder : Decoder MappingStatus
mappingStatusDecoder =
    D.succeed MappingStatus
        |> required "mstMethodId" D.string
        |> required "mstMethodName" D.string
        |> required "mstTotalFactors" D.int
        |> required "mstMappedByUUID" D.int
        |> required "mstMappedByName" D.int
        |> required "mstMappedBySynonym" D.int
        |> required "mstUnmapped" D.int
        |> required "mstCoverage" D.float
        |> required "mstUnmappedFlows" (D.list unmappedFlowDecoder)
