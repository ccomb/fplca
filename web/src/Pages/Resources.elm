module Pages.Resources exposing (Model, Msg, page)

import Browser.Navigation as Nav
import Dict
import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Models.Activity exposing (ActivityInfo, ExchangeType(..), activityInfoDecoder)
import Route
import Shared exposing (RemoteData(..))
import Spa.Page
import Utils.Format as Format
import View exposing (View)
import Views.DetailsView as DetailsView


type alias Model =
    { activityId : String
    , dbName : String
    , activityInfo : RemoteData ActivityInfo
    }


type Msg
    = ActivityInfoLoaded (Result Http.Error ActivityInfo)
    | NavigateBack
    | RequestLoadDatabase


page : Shared.Model -> Spa.Page.Page ( String, String ) Shared.Msg (View Msg) Model Msg
page shared =
    Spa.Page.element
        { init = init shared
        , update = update shared
        , view = view shared
        , subscriptions = \_ -> Sub.none
        }


init : Shared.Model -> ( String, String ) -> ( Model, Effect Shared.Msg Msg )
init shared ( db, activityId ) =
    if not (Shared.isDatabaseLoaded shared db) then
        ( { activityId = activityId
          , dbName = db
          , activityInfo = NotAsked
          }
        , Effect.none
        )

    else
        let
            cached =
                Dict.get activityId shared.cachedActivityInfo
        in
        ( { activityId = activityId
          , dbName = db
          , activityInfo =
                case cached of
                    Just info ->
                        Loaded info

                    Nothing ->
                        Loading
          }
        , case cached of
            Just _ ->
                Effect.none

            Nothing ->
                Effect.fromCmd (loadActivityInfo activityId)
        )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update shared msg model =
    case msg of
        ActivityInfoLoaded (Ok info) ->
            ( { model | activityInfo = Loaded info }
            , Effect.fromShared (Shared.CacheActivityInfo model.activityId info)
            )

        ActivityInfoLoaded (Err error) ->
            ( { model | activityInfo = Failed (Shared.httpErrorToString error) }
            , Effect.none
            )

        NavigateBack ->
            ( model
            , Effect.fromCmd (Nav.back shared.key 1)
            )

        RequestLoadDatabase ->
            ( model
            , Effect.fromShared (Shared.LoadDatabase model.dbName)
            )


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Natural Resources"
    , body =
        if not (Shared.isDatabaseLoaded shared model.dbName) then
            viewLoadDatabasePrompt shared model.dbName

        else
            div [ class "details-page-container" ]
                [ case model.activityInfo of
                    Loading ->
                        div [ class "has-text-centered" ]
                            [ div [ class "is-size-3" ] [ text "Loading..." ]
                            , progress [ class "progress is-primary", attribute "max" "100" ] []
                            ]

                    Failed err ->
                        div [ class "notification is-danger" ]
                            [ strong [] [ text "Error: " ], text err ]

                    Loaded activityInfo ->
                        let
                            exchanges =
                                List.filter (\ex -> ex.exchangeType == BiosphereResourceType) activityInfo.exchanges
                        in
                        div [ style "display" "flex", style "flex-direction" "column", style "height" "100%" ]
                            [ div [ style "flex-shrink" "0" ]
                                [ viewActivityHeader activityInfo "Natural Resources" ]
                            , div [ style "flex" "1", style "display" "flex", style "flex-direction" "column", style "min-height" "0" ]
                                [ DetailsView.viewNaturalResourcesExchanges exchanges ]
                            ]

                    NotAsked ->
                        text ""
                ]
    }


viewActivityHeader : ActivityInfo -> String -> Html Msg
viewActivityHeader activityInfo pageTitle =
    div [ class "box", style "margin-bottom" "0" ]
        [ div [ class "level", style "margin-bottom" "0" ]
            [ div [ class "level-left" ]
                [ div [ class "level-item" ]
                    [ button [ class "button is-primary", onClick NavigateBack ]
                        [ span [ class "icon" ] [ Html.i [ class "fas fa-arrow-left" ] [] ]
                        , span [] [ text "Back" ]
                        ]
                    ]
                , div [ class "level-item" ]
                    [ h1 [ class "title is-4", style "margin-bottom" "0" ]
                        (case activityInfo.referenceProduct of
                            Just product ->
                                let
                                    productText =
                                        case ( activityInfo.referenceProductAmount, activityInfo.referenceProductUnit ) of
                                            ( Just amount, Just unit ) ->
                                                Format.formatScientific amount ++ " " ++ unit ++ " " ++ product

                                            _ ->
                                                product
                                in
                                [ text activityInfo.name
                                , span [ style "color" "#888", style "margin" "0 0.5rem" ] [ text "\u{2192}" ]
                                , span [ style "font-weight" "normal" ] [ text productText ]
                                ]

                            Nothing ->
                                [ text activityInfo.name ]
                        )
                    ]
                , div [ class "level-item" ]
                    [ span [ class "tag is-light" ] [ text activityInfo.location ] ]
                ]
            ]
        , if not (List.isEmpty activityInfo.description) then
            div [ style "font-size" "0.85rem", style "line-height" "1.4", style "margin-top" "0.5rem" ]
                (activityInfo.description |> List.map (\para -> p [ style "margin-bottom" "0.25rem" ] [ text para ]))

          else
            text ""
        , h2 [ class "title is-5", style "margin-top" "1rem", style "margin-bottom" "0" ] [ text pageTitle ]
        ]


viewLoadDatabasePrompt : Shared.Model -> String -> Html Msg
viewLoadDatabasePrompt shared dbName =
    div [ class "notification is-warning", style "margin" "2rem" ]
        [ p [] [ text ("Database '" ++ Shared.getDatabaseDisplayName shared dbName ++ "' is not loaded.") ]
        , button [ class "button is-primary", style "margin-top" "1rem", onClick RequestLoadDatabase ]
            [ text ("Load " ++ Shared.getDatabaseDisplayName shared dbName) ]
        ]


loadActivityInfo : String -> Cmd Msg
loadActivityInfo activityId =
    Http.get
        { url = "/api/v1/activity/" ++ activityId
        , expect = Http.expectJson ActivityInfoLoaded activityInfoDecoder
        }
