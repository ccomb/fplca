module Views.UploadView exposing (Config, Model, Msg(..), databaseConfig, init, methodConfig, update, view)

import File exposing (File)
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, preventDefaultOn)
import Json.Decode as Decode
import Task


type alias Model =
    { name : String
    , description : String
    , selectedFile : Maybe File
    , fileContent : Maybe String
    , uploading : Bool
    , error : Maybe String
    , success : Maybe String
    }


type alias Config =
    { title : String
    , subtitle : String
    , nameLabel : String
    , namePlaceholder : String
    , nameIcon : String
    , fileLabel : String
    , fileHelp : String
    , buttonLabel : String
    }


databaseConfig : Config
databaseConfig =
    { title = "Upload Database"
    , subtitle = "Upload a ZIP, 7z, XML, or CSV file containing your LCA database (EcoSpold v1/v2 or SimaPro CSV)"
    , nameLabel = "Database Name"
    , namePlaceholder = "e.g., My Custom Database"
    , nameIcon = "fas fa-database"
    , fileLabel = "Database File (ZIP, 7z, XML, or CSV)"
    , fileHelp = "Supported formats: EcoSpold v1 (.xml, single or multi-dataset), EcoSpold v2 (.spold), SimaPro CSV (.csv)"
    , buttonLabel = "Upload Database"
    }


methodConfig : Config
methodConfig =
    { title = "Upload Method"
    , subtitle = "Upload a ZIP archive containing ILCD method XML files (e.g., EF 3.1)"
    , nameLabel = "Method Name"
    , namePlaceholder = "e.g., EF 3.1"
    , nameIcon = "fas fa-flask"
    , fileLabel = "Method File (ZIP, 7z)"
    , fileHelp = "Supported formats: ILCD method packages (.zip, .7z)"
    , buttonLabel = "Upload Method"
    }


type Msg
    = SetName String
    | SetDescription String
    | SelectFile
    | FileSelected File
    | FileContentLoaded String
    | UploadDatabase
    | CancelUpload


init : Model
init =
    { name = ""
    , description = ""
    , selectedFile = Nothing
    , fileContent = Nothing
    , uploading = False
    , error = Nothing
    , success = Nothing
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetName name ->
            ( { model | name = name, error = Nothing }, Cmd.none )

        SetDescription desc ->
            ( { model | description = desc }, Cmd.none )

        SelectFile ->
            ( model, Select.file [ ".zip", ".7z", ".tar.gz", ".tgz", ".tar.xz", ".xml", ".csv" ] FileSelected )

        FileSelected file ->
            ( { model | selectedFile = Just file, error = Nothing }
            , Task.perform FileContentLoaded (File.toUrl file)
            )

        FileContentLoaded content ->
            -- Extract base64 data from data URL
            let
                base64Data =
                    case String.split "base64," content of
                        [ _, data ] ->
                            Just data

                        _ ->
                            Nothing
            in
            ( { model | fileContent = base64Data }, Cmd.none )

        UploadDatabase ->
            -- This will be handled by Main.elm
            ( { model | uploading = True }, Cmd.none )

        CancelUpload ->
            ( init, Cmd.none )


view : Config -> Model -> Html Msg
view cfg model =
    div [ class "upload-page" ]
        [ div [ class "box" ]
            [ h2 [ class "title is-3" ]
                [ span [ class "icon-text" ]
                    [ span [ class "icon" ] [ i [ class "fas fa-cloud-upload-alt" ] [] ]
                    , span [] [ text (" " ++ cfg.title) ]
                    ]
                ]
            , p [ class "subtitle" ]
                [ text cfg.subtitle ]
            , viewError model.error
            , viewSuccess model.success
            , viewForm cfg model
            ]
        ]


viewError : Maybe String -> Html Msg
viewError maybeError =
    case maybeError of
        Just err ->
            div [ class "notification is-danger" ]
                [ button [ class "delete", onClick (SetName "") ] []
                , text err
                ]

        Nothing ->
            text ""


viewSuccess : Maybe String -> Html Msg
viewSuccess maybeSuccess =
    case maybeSuccess of
        Just msg ->
            div [ class "notification is-success" ]
                [ text msg ]

        Nothing ->
            text ""


viewForm : Config -> Model -> Html Msg
viewForm cfg model =
    div []
        [ div [ class "field" ]
            [ label [ class "label" ] [ text cfg.nameLabel ]
            , div [ class "control has-icons-left" ]
                [ input
                    [ class "input"
                    , type_ "text"
                    , placeholder cfg.namePlaceholder
                    , value model.name
                    , onInput SetName
                    , disabled model.uploading
                    , Html.Attributes.autofocus True
                    ]
                    []
                , span [ class "icon is-small is-left" ]
                    [ i [ class cfg.nameIcon ] [] ]
                ]
            , p [ class "help" ]
                [ text "A URL-safe slug will be generated: "
                , strong [] [ text (slugify model.name) ]
                ]
            ]
        , div [ class "field" ]
            [ label [ class "label" ] [ text "Description (optional)" ]
            , div [ class "control" ]
                [ textarea
                    [ class "textarea"
                    , placeholder "Describe this..."
                    , value model.description
                    , onInput SetDescription
                    , disabled model.uploading
                    , rows 3
                    ]
                    []
                ]
            ]
        , div [ class "field" ]
            [ label [ class "label" ] [ text cfg.fileLabel ]
            , div [ class "file has-name is-fullwidth" ]
                [ div [ class "file-label" ]
                    [ span
                        [ class "file-cta"
                        , onClick SelectFile
                        , tabindex 0
                        , attribute "role" "button"
                        , preventDefaultOn "keydown"
                            (Decode.field "key" Decode.string
                                |> Decode.andThen
                                    (\key ->
                                        if key == "Enter" || key == " " then
                                            Decode.succeed ( SelectFile, True )

                                        else
                                            Decode.fail ""
                                    )
                            )
                        ]
                        [ span [ class "file-icon" ]
                            [ i [ class "fas fa-upload" ] [] ]
                        , span [ class "file-label" ]
                            [ text "Choose a file..." ]
                        ]
                    , span [ class "file-name" ]
                        [ text (fileNameDisplay model.selectedFile) ]
                    ]
                ]
            , p [ class "help" ]
                [ text cfg.fileHelp ]
            ]
        , div [ class "field is-grouped", style "margin-top" "2rem" ]
            [ div [ class "control" ]
                [ button
                    [ class
                        (if model.uploading then
                            "button is-primary is-loading"

                         else
                            "button is-primary"
                        )
                    , onClick UploadDatabase
                    , disabled (not (canUpload model))
                    ]
                    [ span [ class "icon" ] [ i [ class "fas fa-cloud-upload-alt" ] [] ]
                    , span [] [ text cfg.buttonLabel ]
                    ]
                ]
            , div [ class "control" ]
                [ button
                    [ class "button is-light"
                    , onClick CancelUpload
                    , disabled model.uploading
                    ]
                    [ text "Cancel" ]
                ]
            ]
        ]


canUpload : Model -> Bool
canUpload model =
    not (String.isEmpty model.name)
        && model.selectedFile
        /= Nothing
        && model.fileContent
        /= Nothing
        && not model.uploading


fileNameDisplay : Maybe File -> String
fileNameDisplay maybeFile =
    case maybeFile of
        Just file ->
            File.name file

        Nothing ->
            "No file selected"


slugify : String -> String
slugify name =
    name
        |> String.toLower
        |> String.words
        |> String.join "-"
        |> String.filter isValidSlugChar


isValidSlugChar : Char -> Bool
isValidSlugChar c =
    Char.isAlphaNum c || c == '-' || c == '_'
