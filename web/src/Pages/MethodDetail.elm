module Pages.MethodDetail exposing (Model, Msg, page)

import Browser.Navigation as Nav
import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Models.LCIA exposing (MethodSummary, methodsListDecoder)
import Route
import Shared exposing (RemoteData(..))
import Spa.Page
import Url
import View exposing (View)


type alias Model =
    { collectionName : String
    , methods : RemoteData (List MethodSummary)
    }


type Msg
    = MethodsLoaded (Result Http.Error (List MethodSummary))
    | GoBack


page : Shared.Model -> Spa.Page.Page String Shared.Msg (View Msg) Model Msg
page shared =
    Spa.Page.element
        { init = init shared
        , update = update shared
        , view = view shared
        , subscriptions = \_ -> Sub.none
        }


init : Shared.Model -> String -> ( Model, Effect Shared.Msg Msg )
init _ rawName =
    let
        collectionName =
            Url.percentDecode rawName |> Maybe.withDefault rawName
    in
    ( { collectionName = collectionName
      , methods = Loading
      }
    , Effect.fromCmd fetchMethods
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update shared msg model =
    case msg of
        MethodsLoaded (Ok allMethods) ->
            let
                filtered =
                    List.filter (\m -> m.msmCollection == model.collectionName) allMethods
            in
            ( { model | methods = Loaded filtered }
            , Effect.none
            )

        MethodsLoaded (Err err) ->
            ( { model | methods = Failed (Shared.httpErrorToString err) }
            , Effect.none
            )

        GoBack ->
            ( model
            , Effect.fromCmd (Nav.pushUrl shared.key (Route.routeToUrl Route.MethodsRoute))
            )


view : Shared.Model -> Model -> View Msg
view _ model =
    { title = "Method: " ++ model.collectionName
    , body =
        div [ class "databases-page" ]
            [ div [ class "box" ]
                [ div [ class "level" ]
                    [ div [ class "level-left" ]
                        [ div [ class "level-item" ]
                            [ button [ class "button is-text", onClick GoBack ]
                                [ span [ class "icon" ] [ i [ class "fas fa-arrow-left" ] [] ]
                                ]
                            ]
                        , div [ class "level-item" ]
                            [ h2 [ class "title is-3", style "margin-bottom" "0" ]
                                [ text model.collectionName ]
                            ]
                        ]
                    ]
                , p [ class "subtitle", style "margin-top" "0.5rem" ] [ text "Impact categories in this method collection" ]
                ]
            , case model.methods of
                Loading ->
                    div [ class "has-text-centered", style "padding" "2rem" ]
                        [ span [ class "icon is-large has-text-primary" ]
                            [ i [ class "fas fa-spinner fa-spin fa-2x" ] [] ]
                        ]

                Failed err ->
                    div [ class "notification is-danger" ] [ text err ]

                Loaded methods ->
                    viewMethodsTable methods

                NotAsked ->
                    text ""
            ]
    }


viewMethodsTable : List MethodSummary -> Html Msg
viewMethodsTable methods =
    div []
        [ div [ style "padding" "0.5rem 0" ]
            [ span [ class "tag is-info is-light" ]
                [ text (String.fromInt (List.length methods) ++ " impact categories") ]
            ]
        , table [ class "table is-striped is-hoverable is-fullwidth" ]
            [ thead []
                [ tr []
                    [ th [] [ text "Name" ]
                    , th [] [ text "Impact Category" ]
                    , th [] [ text "Unit" ]
                    , th [ style "text-align" "right" ] [ text "Characterization Factors" ]
                    ]
                ]
            , tbody []
                (List.map viewMethodRow methods)
            ]
        ]


viewMethodRow : MethodSummary -> Html Msg
viewMethodRow method =
    tr []
        [ td [] [ text method.msmName ]
        , td [ class "has-text-grey" ] [ text method.msmCategory ]
        , td [ class "has-text-grey" ] [ text method.msmUnit ]
        , td [ style "text-align" "right" ] [ text (String.fromInt method.msmFactorCount) ]
        ]



-- HTTP


fetchMethods : Cmd Msg
fetchMethods =
    Http.get
        { url = "/api/v1/methods"
        , expect = Http.expectJson MethodsLoaded methodsListDecoder
        }
