module Models.Method exposing
    ( MethodCollectionList
    , MethodCollectionStatus
    , methodCollectionListDecoder
    , methodCollectionStatusDecoder
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Models.Database exposing (DatabaseLoadStatus(..), databaseLoadStatusDecoder)


{-| Status of a method collection (e.g., EF-3.1)
-}
type alias MethodCollectionStatus =
    { name : String
    , displayName : String
    , description : Maybe String
    , status : DatabaseLoadStatus
    , isUploaded : Bool
    , path : String
    , methodCount : Int
    , format : Maybe String
    }


{-| List of method collections
-}
type alias MethodCollectionList =
    { methods : List MethodCollectionStatus
    }


methodCollectionStatusDecoder : Decoder MethodCollectionStatus
methodCollectionStatusDecoder =
    Decode.succeed MethodCollectionStatus
        |> required "mcaName" Decode.string
        |> required "mcaDisplayName" Decode.string
        |> optional "mcaDescription" (Decode.map Just Decode.string) Nothing
        |> required "mcaStatus" databaseLoadStatusDecoder
        |> required "mcaIsUploaded" Decode.bool
        |> required "mcaPath" Decode.string
        |> required "mcaMethodCount" Decode.int
        |> optional "mcaFormat" (Decode.nullable Decode.string) Nothing


methodCollectionListDecoder : Decoder MethodCollectionList
methodCollectionListDecoder =
    Decode.succeed MethodCollectionList
        |> required "mclMethods" (Decode.list methodCollectionStatusDecoder)
