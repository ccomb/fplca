module Views.UploadView exposing (Model, Msg(..), init, update, view)

import File exposing (File)
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
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
            ( model, Select.file [ ".zip", ".7z", ".tar.gz", ".tgz", ".tar.xz", ".csv" ] FileSelected )

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


view : Model -> Html Msg
view model =
    div [ class "upload-page" ]
        [ div [ class "box" ]
            [ h2 [ class "title is-3" ]
                [ span [ class "icon-text" ]
                    [ span [ class "icon" ] [ i [ class "fas fa-cloud-upload-alt" ] [] ]
                    , span [] [ text " Upload Database" ]
                    ]
                ]
            , p [ class "subtitle" ]
                [ text "Upload a ZIP, 7z, or CSV file containing your LCA database (EcoSpold v1/v2 or SimaPro CSV)" ]
            , viewError model.error
            , viewSuccess model.success
            , viewForm model
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


viewForm : Model -> Html Msg
viewForm model =
    div []
        [ div [ class "field" ]
            [ label [ class "label" ] [ text "Database Name" ]
            , div [ class "control has-icons-left" ]
                [ input
                    [ class "input"
                    , type_ "text"
                    , placeholder "e.g., My Custom Database"
                    , value model.name
                    , onInput SetName
                    , disabled model.uploading
                    ]
                    []
                , span [ class "icon is-small is-left" ]
                    [ i [ class "fas fa-database" ] [] ]
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
                    , placeholder "Describe this database..."
                    , value model.description
                    , onInput SetDescription
                    , disabled model.uploading
                    , rows 3
                    ]
                    []
                ]
            ]
        , div [ class "field" ]
            [ label [ class "label" ] [ text "Database File (ZIP, 7z, or CSV)" ]
            , div [ class "file has-name is-fullwidth" ]
                [ div [ class "file-label" ]
                    [ span
                        [ class "file-cta"
                        , onClick SelectFile
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
                [ text "Supported formats: EcoSpold v1 (.xml), EcoSpold v2 (.spold), SimaPro CSV (.csv)" ]
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
                    , span [] [ text "Upload Database" ]
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
