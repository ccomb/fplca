module Pages.FlowSynonymDetail exposing (Model, Msg, page)

import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as D
import Route exposing (FlowSynonymDetailFlags)
import Shared exposing (RemoteData(..))
import Spa.Page
import Url
import View exposing (View)


type alias Model =
    { name : String
    , groups : RemoteData (List (List String))
    }


type Msg
    = GroupsLoaded (Result Http.Error (List (List String)))
    | GoBack


page : Shared.Model -> Spa.Page.Page FlowSynonymDetailFlags Shared.Msg (View Msg) Model Msg
page _ =
    Spa.Page.element
        { init = init
        , update = \msg model -> update msg model
        , view = view
        , subscriptions = \_ -> Sub.none
        }


init : FlowSynonymDetailFlags -> ( Model, Effect Shared.Msg Msg )
init flags =
    let
        name =
            Url.percentDecode flags.name |> Maybe.withDefault flags.name
    in
    ( { name = name, groups = Loading }
    , Effect.fromCmd (fetchGroups name)
    )


update : Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update msg model =
    case msg of
        GroupsLoaded (Ok groups) ->
            ( { model | groups = Loaded groups }, Effect.none )

        GroupsLoaded (Err err) ->
            ( { model | groups = Failed (Shared.httpErrorToString err) }, Effect.none )

        GoBack ->
            ( model, Effect.fromShared (Shared.NavigateTo Route.FlowSynonymsRoute) )


view : Model -> View Msg
view model =
    { title = model.name
    , body =
        div [ class "databases-page" ]
            [ div [ class "level", style "margin-bottom" "1rem" ]
                [ div [ class "level-left" ]
                    [ div [ class "level-item" ]
                        [ button [ class "button is-text", onClick GoBack ]
                            [ span [ class "icon" ] [ i [ class "fas fa-arrow-left" ] [] ]
                            ]
                        ]
                    , div [ class "level-item" ]
                        [ div []
                            [ h2 [ class "title is-3", style "margin-bottom" "0" ] [ text model.name ]
                            , p [ class "subtitle is-6 has-text-grey", style "margin-bottom" "0" ] [ text "Synonym groups" ]
                            ]
                        ]
                    ]
                , div [ class "level-right" ]
                    [ div [ class "level-item" ]
                        [ a
                            [ href ("/api/v1/flow-synonyms/" ++ Url.percentEncode model.name ++ "/download")
                            , attribute "download" ""
                            , class "button is-small is-link is-outlined"
                            ]
                            [ span [ class "icon is-small" ] [ i [ class "fas fa-download" ] [] ]
                            , span [] [ text "CSV" ]
                            ]
                        ]
                    ]
                ]
            , case model.groups of
                Loading ->
                    div [ style "padding" "2rem", style "text-align" "center" ]
                        [ span [ class "icon" ] [ i [ class "fas fa-spinner fa-spin" ] [] ]
                        , text " Loading groups..."
                        ]

                Failed err ->
                    div [ class "notification is-danger is-light" ] [ text err ]

                Loaded groups ->
                    viewGroups groups

                _ ->
                    text ""
            ]
    }


viewGroups : List (List String) -> Html Msg
viewGroups groups =
    let
        sorted =
            List.sortBy (\g -> String.toLower (String.join "" g)) groups
    in
    div []
        [ div [ style "padding" "0.5rem 0" ]
            [ span [ class "tag is-info is-light" ]
                [ text (String.fromInt (List.length sorted) ++ " group(s)") ]
            ]
        , table [ class "table is-striped is-hoverable is-fullwidth" ]
            [ tbody [] (List.map viewGroup sorted) ]
        ]


viewGroup : List String -> Html Msg
viewGroup names =
    tr []
        [ td [ style "padding" "0.4rem 0.75rem" ]
            [ div [ style "display" "flex", style "flex-wrap" "wrap", style "align-items" "center", style "gap" "0.3rem" ]
                (List.map viewSynonym names)
            ]
        ]


viewSynonym : String -> Html Msg
viewSynonym name =
    span
        [ style "background-color" "#e8e8e8"
        , style "color" "#333"
        , style "padding" "0.15rem 0.5rem"
        , style "border-radius" "3px"
        , style "white-space" "nowrap"
        ]
        [ text name ]



-- HTTP


fetchGroups : String -> Cmd Msg
fetchGroups name =
    Http.get
        { url = "/api/v1/flow-synonyms/" ++ Url.percentEncode name ++ "/groups"
        , expect = Http.expectJson GroupsLoaded groupsDecoder
        }


groupsDecoder : D.Decoder (List (List String))
groupsDecoder =
    D.field "sgrGroups" (D.list (D.list D.string))
