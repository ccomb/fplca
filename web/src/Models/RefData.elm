module Models.RefData exposing
    ( RefDataList
    , RefDataStatus
    , refDataListDecoder
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Models.Database exposing (DatabaseLoadStatus, databaseLoadStatusDecoder)


type alias RefDataStatus =
    { name : String
    , displayName : String
    , description : Maybe String
    , status : DatabaseLoadStatus
    , isUploaded : Bool
    , isAuto : Bool
    , entryCount : Int
    }


type alias RefDataList =
    { items : List RefDataStatus
    }


refDataStatusDecoder : Decoder RefDataStatus
refDataStatusDecoder =
    Decode.succeed RefDataStatus
        |> required "rdaName" Decode.string
        |> required "rdaDisplayName" Decode.string
        |> optional "rdaDescription" (Decode.map Just Decode.string) Nothing
        |> required "rdaStatus" databaseLoadStatusDecoder
        |> required "rdaIsUploaded" Decode.bool
        |> required "rdaIsAuto" Decode.bool
        |> required "rdaEntryCount" Decode.int


refDataListDecoder : Decoder RefDataList
refDataListDecoder =
    Decode.succeed RefDataList
        |> required "rdlItems" (Decode.list refDataStatusDecoder)
