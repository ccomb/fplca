module Pages.DatabaseSetup exposing (Model, Msg, page)

import Browser.Navigation as Nav
import Effect exposing (Effect)
import Html exposing (..)
import Http
import Models.Database exposing (ActivateResponse, DatabaseSetupInfo, activateResponseDecoder, databaseSetupInfoDecoder)
import Route
import Shared exposing (RemoteData(..))
import Spa.Page
import View exposing (View)
import Views.DatabaseSetupView as DatabaseSetupView


type alias Model =
    { dbName : String
    , setupInfo : RemoteData DatabaseSetupInfo
    }


type Msg
    = SetupInfoLoaded (Result Http.Error DatabaseSetupInfo)
    | DatabaseSetupViewMsg DatabaseSetupView.Msg
    | DependencyActionResult (Result Http.Error DatabaseSetupInfo)
    | FinalizeResult (Result Http.Error ActivateResponse)


page : Shared.Model -> Spa.Page.Page String Shared.Msg (View Msg) Model Msg
page shared =
    Spa.Page.element
        { init = init shared
        , update = update shared
        , view = view shared
        , subscriptions = \_ -> Sub.none
        }


init : Shared.Model -> String -> ( Model, Effect Shared.Msg Msg )
init shared dbName =
    ( { dbName = dbName
      , setupInfo = Loading
      }
    , Effect.fromCmd (loadSetupInfo dbName)
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update shared msg model =
    case msg of
        SetupInfoLoaded (Ok info) ->
            ( { model | setupInfo = Loaded info }
            , Effect.none
            )

        SetupInfoLoaded (Err error) ->
            ( { model | setupInfo = Failed (Shared.httpErrorToString error) }
            , Effect.none
            )

        DatabaseSetupViewMsg viewMsg ->
            case viewMsg of
                DatabaseSetupView.AddDependency depName ->
                    ( { model | setupInfo = Loading }
                    , Effect.fromCmd (addDependency model.dbName depName)
                    )

                DatabaseSetupView.RemoveDependency depName ->
                    ( { model | setupInfo = Loading }
                    , Effect.fromCmd (removeDependency model.dbName depName)
                    )

                DatabaseSetupView.FinalizeDatabase ->
                    ( { model | setupInfo = Loading }
                    , Effect.fromCmd (finalizeDatabase model.dbName)
                    )

                DatabaseSetupView.GoBack ->
                    ( model
                    , Effect.fromCmd (Nav.pushUrl shared.key (Route.routeToUrl Route.DatabasesRoute))
                    )

        DependencyActionResult (Ok info) ->
            ( { model | setupInfo = Loaded info }
            , Effect.none
            )

        DependencyActionResult (Err error) ->
            ( { model | setupInfo = Failed (Shared.httpErrorToString error) }
            , Effect.none
            )

        FinalizeResult (Ok response) ->
            if response.success then
                ( model
                , Effect.batch
                    [ Effect.fromShared Shared.LoadDatabases
                    , Effect.fromCmd (Nav.pushUrl shared.key (Route.routeToUrl Route.DatabasesRoute))
                    ]
                )

            else
                ( { model | setupInfo = Failed response.message }
                , Effect.none
                )

        FinalizeResult (Err error) ->
            ( { model | setupInfo = Failed (Shared.httpErrorToString error) }
            , Effect.none
            )


view : Shared.Model -> Model -> View Msg
view _ model =
    let
        maybeSetupInfo =
            case model.setupInfo of
                Loaded info ->
                    Just info

                _ ->
                    Nothing

        loading =
            case model.setupInfo of
                Loading ->
                    True

                _ ->
                    False

        error =
            case model.setupInfo of
                Failed err ->
                    Just err

                _ ->
                    Nothing
    in
    { title = "Database Setup"
    , body =
        Html.map DatabaseSetupViewMsg
            (DatabaseSetupView.viewDatabaseSetupPage
                maybeSetupInfo
                loading
                error
            )
    }



-- HTTP


loadSetupInfo : String -> Cmd Msg
loadSetupInfo dbName =
    Http.get
        { url = "/api/v1/databases/" ++ dbName ++ "/setup"
        , expect = Http.expectJson SetupInfoLoaded databaseSetupInfoDecoder
        }


addDependency : String -> String -> Cmd Msg
addDependency dbName depName =
    Http.post
        { url = "/api/v1/databases/" ++ dbName ++ "/add-dependency/" ++ depName
        , body = Http.emptyBody
        , expect = Http.expectJson DependencyActionResult databaseSetupInfoDecoder
        }


removeDependency : String -> String -> Cmd Msg
removeDependency dbName depName =
    Http.post
        { url = "/api/v1/databases/" ++ dbName ++ "/remove-dependency/" ++ depName
        , body = Http.emptyBody
        , expect = Http.expectJson DependencyActionResult databaseSetupInfoDecoder
        }


finalizeDatabase : String -> Cmd Msg
finalizeDatabase dbName =
    Http.post
        { url = "/api/v1/databases/" ++ dbName ++ "/finalize"
        , body = Http.emptyBody
        , expect = Http.expectJson FinalizeResult activateResponseDecoder
        }
