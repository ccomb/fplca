module Models.Database exposing
    ( ActivateResponse
    , DatabaseList
    , DatabaseStatus
    , activateResponseDecoder
    , databaseListDecoder
    , databaseStatusDecoder
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)


{-| Database status from the API
-}
type alias DatabaseStatus =
    { name : String
    , displayName : String
    , description : Maybe String
    , active : Bool
    , loaded : Bool
    , cached : Bool
    , path : String
    }


{-| List of databases with current database info
-}
type alias DatabaseList =
    { databases : List DatabaseStatus
    , current : Maybe String
    }


{-| Response from activating a database
-}
type alias ActivateResponse =
    { success : Bool
    , message : String
    , database : Maybe DatabaseStatus
    }


{-| JSON decoder for DatabaseStatus
-}
databaseStatusDecoder : Decoder DatabaseStatus
databaseStatusDecoder =
    Decode.succeed DatabaseStatus
        |> required "dsaName" Decode.string
        |> required "dsaDisplayName" Decode.string
        |> optional "dsaDescription" (Decode.nullable Decode.string) Nothing
        |> required "dsaActive" Decode.bool
        |> required "dsaLoaded" Decode.bool
        |> required "dsaCached" Decode.bool
        |> required "dsaPath" Decode.string


{-| JSON decoder for DatabaseList
-}
databaseListDecoder : Decoder DatabaseList
databaseListDecoder =
    Decode.succeed DatabaseList
        |> required "dlrDatabases" (Decode.list databaseStatusDecoder)
        |> optional "dlrCurrent" (Decode.nullable Decode.string) Nothing


{-| JSON decoder for ActivateResponse
-}
activateResponseDecoder : Decoder ActivateResponse
activateResponseDecoder =
    Decode.succeed ActivateResponse
        |> required "arSuccess" Decode.bool
        |> required "arMessage" Decode.string
        |> optional "arDatabase" (Decode.nullable databaseStatusDecoder) Nothing
