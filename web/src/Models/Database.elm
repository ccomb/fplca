module Models.Database exposing
    ( ActivateResponse
    , DatabaseList
    , DatabaseSetupInfo
    , DatabaseStatus
    , DependencySuggestion
    , MissingSupplier
    , UploadResponse
    , activateResponseDecoder
    , databaseListDecoder
    , databaseSetupInfoDecoder
    , databaseStatusDecoder
    , uploadResponseDecoder
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)


{-| Database status from the API
-}
type alias DatabaseStatus =
    { name : String
    , displayName : String
    , description : Maybe String
    , loadAtStartup : Bool
    , loaded : Bool
    , cached : Bool
    , isUploaded : Bool
    , path : String
    , format : Maybe String
    }


{-| List of databases
-}
type alias DatabaseList =
    { databases : List DatabaseStatus
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
        |> required "dsaLoadAtStartup" Decode.bool
        |> required "dsaLoaded" Decode.bool
        |> required "dsaCached" Decode.bool
        |> required "dsaIsUploaded" Decode.bool
        |> required "dsaPath" Decode.string
        |> optional "dsaFormat" (Decode.nullable Decode.string) Nothing


{-| JSON decoder for DatabaseList
-}
databaseListDecoder : Decoder DatabaseList
databaseListDecoder =
    Decode.succeed DatabaseList
        |> required "dlrDatabases" (Decode.list databaseStatusDecoder)


{-| JSON decoder for ActivateResponse
-}
activateResponseDecoder : Decoder ActivateResponse
activateResponseDecoder =
    Decode.succeed ActivateResponse
        |> required "arSuccess" Decode.bool
        |> required "arMessage" Decode.string
        |> optional "arDatabase" (Decode.nullable databaseStatusDecoder) Nothing


{-| Response from uploading a database
-}
type alias UploadResponse =
    { success : Bool
    , message : String
    , slug : Maybe String
    , format : Maybe String
    }


{-| JSON decoder for UploadResponse
-}
uploadResponseDecoder : Decoder UploadResponse
uploadResponseDecoder =
    Decode.succeed UploadResponse
        |> required "uprSuccess" Decode.bool
        |> required "uprMessage" Decode.string
        |> optional "uprSlug" (Decode.nullable Decode.string) Nothing
        |> optional "uprFormat" (Decode.nullable Decode.string) Nothing


{-| Information about a missing supplier product
-}
type alias MissingSupplier =
    { productName : String
    , count : Int
    , location : Maybe String
    , reason : String
    , detail : Maybe String
    }


{-| Suggestion for a dependency database
-}
type alias DependencySuggestion =
    { databaseName : String
    , displayName : String
    , matchCount : Int
    }


{-| Setup info for a database (for the setup page)
-}
type alias DatabaseSetupInfo =
    { name : String
    , displayName : String
    , activityCount : Int
    , inputCount : Int
    , completeness : Float
    , internalLinks : Int
    , crossDBLinks : Int
    , unresolvedLinks : Int
    , missingSuppliers : List MissingSupplier
    , selectedDependencies : List String
    , suggestions : List DependencySuggestion
    , isReady : Bool
    , unknownUnits : List String
    }


{-| JSON decoder for MissingSupplier
-}
missingSupplierDecoder : Decoder MissingSupplier
missingSupplierDecoder =
    Decode.succeed MissingSupplier
        |> required "productName" Decode.string
        |> required "count" Decode.int
        |> optional "location" (Decode.nullable Decode.string) Nothing
        |> optional "reason" Decode.string "no_name_match"
        |> optional "detail" (Decode.nullable Decode.string) Nothing


{-| JSON decoder for DependencySuggestion
-}
dependencySuggestionDecoder : Decoder DependencySuggestion
dependencySuggestionDecoder =
    Decode.succeed DependencySuggestion
        |> required "databaseName" Decode.string
        |> required "displayName" Decode.string
        |> required "matchCount" Decode.int


{-| JSON decoder for DatabaseSetupInfo
-}
databaseSetupInfoDecoder : Decoder DatabaseSetupInfo
databaseSetupInfoDecoder =
    Decode.succeed DatabaseSetupInfo
        |> required "name" Decode.string
        |> required "displayName" Decode.string
        |> required "activityCount" Decode.int
        |> required "inputCount" Decode.int
        |> required "completeness" Decode.float
        |> required "internalLinks" Decode.int
        |> required "crossDBLinks" Decode.int
        |> required "unresolvedLinks" Decode.int
        |> required "missingSuppliers" (Decode.list missingSupplierDecoder)
        |> required "selectedDependencies" (Decode.list Decode.string)
        |> required "suggestions" (Decode.list dependencySuggestionDecoder)
        |> required "isReady" Decode.bool
        |> optional "unknownUnits" (Decode.list Decode.string) []
