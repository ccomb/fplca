module Pages.LCIA exposing (Model, Msg, page)

import Dict
import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Models.Activity exposing (ActivityInfo, activityInfoDecoder)
import Models.LCIA exposing (LCIAResult, MappingStatus, MethodSummary, lciaResultDecoder, mappingStatusDecoder, methodsListDecoder)
import Shared exposing (RemoteData(..))
import Spa.Page
import View exposing (View)
import Views.LCIAView as LCIAView


type alias Model =
    { activityId : String
    , dbName : String
    , state : LCIAState
    , mappingStatus : RemoteData MappingStatus
    }


type LCIAState
    = LoadingMethods
    | MethodsFailed String
    | MethodsReady
        { methods : List MethodSummary
        , computation : LCIAComputation
        }


type LCIAComputation
    = NoMethodSelected
    | MethodSelected MethodSummary
    | Computing MethodSummary
    | Computed MethodSummary LCIAResult
    | ComputeFailed MethodSummary String


type Msg
    = MethodsLoaded (Result Http.Error (List MethodSummary))
    | ActivityInfoLoaded (Result Http.Error ActivityInfo)
    | LCIAViewMsg LCIAView.Msg
    | LCIAResultLoaded (Result Http.Error LCIAResult)
    | MappingStatusLoaded (Result Http.Error MappingStatus)
    | RequestLoadDatabase
    | NewFlags ( String, String )


page : Shared.Model -> Spa.Page.Page ( String, String ) Shared.Msg (View Msg) Model Msg
page shared =
    Spa.Page.element
        { init = init shared
        , update = update shared
        , view = view shared
        , subscriptions = \_ -> Sub.none
        }
        |> Spa.Page.onNewFlags NewFlags


init : Shared.Model -> ( String, String ) -> ( Model, Effect Shared.Msg Msg )
init shared ( db, activityId ) =
    if not (Shared.isDatabaseLoaded shared db) then
        ( { activityId = activityId
          , dbName = db
          , state = LoadingMethods
          , mappingStatus = NotAsked
          }
        , Effect.none
        )

    else
        ( { activityId = activityId
          , dbName = db
          , state = LoadingMethods
          , mappingStatus = NotAsked
          }
        , Effect.batch
            [ Effect.fromCmd (loadMethods)
            , case Dict.get activityId shared.cachedActivityInfo of
                Just _ ->
                    Effect.none

                Nothing ->
                    Effect.fromCmd (loadActivityInfo activityId)
            ]
        )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update shared msg model =
    case msg of
        MethodsLoaded (Ok methods) ->
            ( { model
                | state =
                    MethodsReady
                        { methods = methods
                        , computation = NoMethodSelected
                        }
              }
            , Effect.none
            )

        MethodsLoaded (Err error) ->
            ( { model | state = MethodsFailed (Shared.httpErrorToString error) }
            , Effect.none
            )

        ActivityInfoLoaded (Ok info) ->
            ( model
            , Effect.fromShared (Shared.CacheActivityInfo model.activityId info)
            )

        ActivityInfoLoaded (Err _) ->
            ( model, Effect.none )

        LCIAViewMsg viewMsg ->
            case viewMsg of
                LCIAView.SelectMethod methodId ->
                    case model.state of
                        MethodsReady ready ->
                            let
                                selectedMethod =
                                    List.filter (\m -> m.msmId == methodId) ready.methods
                                        |> List.head
                            in
                            case selectedMethod of
                                Just method ->
                                    ( { model
                                        | state =
                                            MethodsReady
                                                { ready | computation = MethodSelected method }
                                      }
                                    , Effect.none
                                    )

                                Nothing ->
                                    ( { model
                                        | state =
                                            MethodsReady
                                                { ready | computation = NoMethodSelected }
                                      }
                                    , Effect.none
                                    )

                        _ ->
                            ( model, Effect.none )

                LCIAView.ComputeLCIA methodId ->
                    case model.state of
                        MethodsReady ready ->
                            let
                                selectedMethod =
                                    List.filter (\m -> m.msmId == methodId) ready.methods
                                        |> List.head
                            in
                            case selectedMethod of
                                Just method ->
                                    ( { model
                                        | state =
                                            MethodsReady
                                                { ready | computation = Computing method }
                                        , mappingStatus = Loading
                                      }
                                    , Effect.batch
                                        [ Effect.fromCmd (computeLCIA model.activityId methodId)
                                        , Effect.fromCmd (loadMappingStatus model.activityId methodId)
                                        ]
                                    )

                                Nothing ->
                                    ( model, Effect.none )

                        _ ->
                            ( model, Effect.none )

        LCIAResultLoaded (Ok result) ->
            case model.state of
                MethodsReady ready ->
                    case ready.computation of
                        Computing method ->
                            ( { model
                                | state =
                                    MethodsReady
                                        { ready | computation = Computed method result }
                              }
                            , Effect.none
                            )

                        _ ->
                            ( model, Effect.none )

                _ ->
                    ( model, Effect.none )

        LCIAResultLoaded (Err error) ->
            case model.state of
                MethodsReady ready ->
                    case ready.computation of
                        Computing method ->
                            ( { model
                                | state =
                                    MethodsReady
                                        { ready | computation = ComputeFailed method (Shared.httpErrorToString error) }
                              }
                            , Effect.none
                            )

                        _ ->
                            ( model, Effect.none )

                _ ->
                    ( model, Effect.none )

        MappingStatusLoaded (Ok status) ->
            ( { model | mappingStatus = Loaded status }
            , Effect.none
            )

        MappingStatusLoaded (Err error) ->
            ( { model | mappingStatus = Failed (Shared.httpErrorToString error) }
            , Effect.none
            )

        RequestLoadDatabase ->
            ( model
            , Effect.fromShared (Shared.LoadDatabase model.dbName)
            )

        NewFlags flags ->
            if flags == ( model.dbName, model.activityId ) then
                ( model, Effect.none )

            else
                init shared flags


view : Shared.Model -> Model -> View Msg
view shared model =
    if not (Shared.isDatabaseLoaded shared model.dbName) then
        { title = "LCIA"
        , body = Shared.viewLoadDatabasePrompt shared model.dbName RequestLoadDatabase
        }

    else
        viewLoaded shared model


viewLoaded : Shared.Model -> Model -> View Msg
viewLoaded shared model =
    let
        activityInfo =
            Dict.get model.activityId shared.cachedActivityInfo
                |> Maybe.map (\info -> ( info.name, info.location ))

        maybeMethods =
            case model.state of
                MethodsReady ready ->
                    Just ready.methods

                _ ->
                    Nothing

        selectedMethod =
            case model.state of
                MethodsReady ready ->
                    case ready.computation of
                        MethodSelected m ->
                            Just m

                        Computing m ->
                            Just m

                        Computed m _ ->
                            Just m

                        ComputeFailed m _ ->
                            Just m

                        NoMethodSelected ->
                            Nothing

                _ ->
                    Nothing

        maybeLCIAResult =
            case model.state of
                MethodsReady ready ->
                    case ready.computation of
                        Computed _ result ->
                            Just result

                        _ ->
                            Nothing

                _ ->
                    Nothing

        maybeMappingStatus =
            case model.mappingStatus of
                Loaded status ->
                    Just status

                _ ->
                    Nothing

        loadingMethods =
            case model.state of
                LoadingMethods ->
                    True

                _ ->
                    False

        loadingLCIA =
            case model.state of
                MethodsReady ready ->
                    case ready.computation of
                        Computing _ ->
                            True

                        _ ->
                            False

                _ ->
                    False

        error =
            case model.state of
                MethodsFailed err ->
                    Just err

                MethodsReady ready ->
                    case ready.computation of
                        ComputeFailed _ err ->
                            Just err

                        _ ->
                            Nothing

                _ ->
                    Nothing
    in
    { title = "LCIA"
    , body =
        Html.map LCIAViewMsg
            (LCIAView.viewLCIAPage
                maybeMethods
                selectedMethod
                maybeLCIAResult
                maybeMappingStatus
                loadingMethods
                loadingLCIA
                error
                activityInfo
            )
    }



-- HTTP


loadMethods : Cmd Msg
loadMethods =
    Http.get
        { url = "/api/v1/methods"
        , expect = Http.expectJson MethodsLoaded methodsListDecoder
        }


loadActivityInfo : String -> Cmd Msg
loadActivityInfo activityId =
    Http.get
        { url = "/api/v1/activity/" ++ activityId
        , expect = Http.expectJson ActivityInfoLoaded activityInfoDecoder
        }


computeLCIA : String -> String -> Cmd Msg
computeLCIA activityId methodId =
    Http.get
        { url = "/api/v1/activity/" ++ activityId ++ "/lcia/" ++ methodId
        , expect = Http.expectJson LCIAResultLoaded lciaResultDecoder
        }


loadMappingStatus : String -> String -> Cmd Msg
loadMappingStatus activityId methodId =
    Http.get
        { url = "/api/v1/activity/" ++ activityId ++ "/lcia/" ++ methodId ++ "/mapping"
        , expect = Http.expectJson MappingStatusLoaded mappingStatusDecoder
        }
