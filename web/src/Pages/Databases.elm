module Pages.Databases exposing (Model, Msg, page)

import Browser.Navigation as Nav
import Effect exposing (Effect)
import Html exposing (..)
import Http
import Models.Database exposing (ActivateResponse, LoadDatabaseResponse(..), activateResponseDecoder, loadDatabaseResponseDecoder)
import Route
import Shared exposing (RemoteData(..))
import Spa.Page
import View exposing (View)
import Views.DatabasesView as DatabasesView


type alias Model =
    { pendingAction : Maybe PendingAction
    , actionError : Maybe String
    }


type PendingAction
    = LoadingDb String
    | UnloadingDb String
    | DeletingDb String


type Msg
    = DatabasesViewMsg DatabasesView.Msg
    | LoadResult (Result Http.Error LoadDatabaseResponse)
    | ActionResult (Result Http.Error ActivateResponse)


page : Shared.Model -> Spa.Page.Page () Shared.Msg (View Msg) Model Msg
page shared =
    Spa.Page.element
        { init = init shared
        , update = update shared
        , view = view shared
        , subscriptions = \_ -> Sub.none
        }


init : Shared.Model -> () -> ( Model, Effect Shared.Msg Msg )
init shared _ =
    ( { pendingAction = Nothing
      , actionError = Nothing
      }
    , case shared.databases of
        NotAsked ->
            Effect.fromShared Shared.LoadDatabases

        Failed _ ->
            Effect.fromShared Shared.LoadDatabases

        _ ->
            Effect.none
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update shared msg model =
    case msg of
        DatabasesViewMsg dbMsg ->
            case dbMsg of
                DatabasesView.NavigateToDatabase dbName ->
                    ( model
                    , Effect.fromCmd (Nav.pushUrl shared.key (Route.routeToUrl (Route.ActivitiesRoute { db = dbName, name = Nothing, limit = Just 20 })))
                    )

                DatabasesView.LoadDatabase dbName ->
                    ( { model | pendingAction = Just (LoadingDb dbName), actionError = Nothing }
                    , Effect.fromCmd (loadDatabaseCmd dbName)
                    )

                DatabasesView.UnloadDatabase dbName ->
                    ( { model | pendingAction = Just (UnloadingDb dbName), actionError = Nothing }
                    , Effect.fromCmd (unloadDatabaseCmd dbName)
                    )

                DatabasesView.DeleteDatabase dbName ->
                    ( { model | pendingAction = Just (DeletingDb dbName), actionError = Nothing }
                    , Effect.fromCmd (deleteDatabaseCmd dbName)
                    )

                DatabasesView.SetupDatabase dbName ->
                    ( model
                    , Effect.fromCmd (Nav.pushUrl shared.key (Route.routeToUrl (Route.DatabaseSetupRoute dbName)))
                    )

        LoadResult (Ok response) ->
            case response of
                LoadSucceeded _ _ ->
                    ( { model | pendingAction = Nothing, actionError = Nothing }
                    , Effect.fromShared Shared.LoadDatabases
                    )

                LoadFailed err ->
                    ( { model | pendingAction = Nothing, actionError = Just err }
                    , Effect.none
                    )

        LoadResult (Err err) ->
            ( { model | pendingAction = Nothing, actionError = Just (Shared.httpErrorToString err) }
            , Effect.none
            )

        ActionResult (Ok response) ->
            if response.success then
                ( { model | pendingAction = Nothing, actionError = Nothing }
                , Effect.fromShared Shared.LoadDatabases
                )

            else
                ( { model | pendingAction = Nothing, actionError = Just response.message }
                , Effect.none
                )

        ActionResult (Err err) ->
            ( { model | pendingAction = Nothing, actionError = Just (Shared.httpErrorToString err) }
            , Effect.none
            )


view : Shared.Model -> Model -> View Msg
view shared model =
    let
        isLoading =
            model.pendingAction /= Nothing

        maybeDatabases =
            case shared.databases of
                Loaded dbList ->
                    Just dbList

                _ ->
                    Nothing

        loadingDatabases =
            case shared.databases of
                Loading ->
                    True

                _ ->
                    False

        error =
            case model.actionError of
                Just err ->
                    Just err

                Nothing ->
                    case shared.databases of
                        Failed err ->
                            Just err

                        _ ->
                            Nothing
    in
    { title = "Databases"
    , body =
        Html.map DatabasesViewMsg
            (DatabasesView.viewDatabasesPage
                maybeDatabases
                (loadingDatabases || isLoading)
                error
            )
    }



-- HTTP commands


loadDatabaseCmd : String -> Cmd Msg
loadDatabaseCmd dbName =
    Http.post
        { url = "/api/v1/databases/" ++ dbName ++ "/load"
        , body = Http.emptyBody
        , expect = Http.expectJson LoadResult loadDatabaseResponseDecoder
        }


unloadDatabaseCmd : String -> Cmd Msg
unloadDatabaseCmd dbName =
    Http.post
        { url = "/api/v1/databases/" ++ dbName ++ "/unload"
        , body = Http.emptyBody
        , expect = Http.expectJson ActionResult activateResponseDecoder
        }


deleteDatabaseCmd : String -> Cmd Msg
deleteDatabaseCmd dbName =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = "/api/v1/databases/" ++ dbName
        , body = Http.emptyBody
        , expect = Http.expectJson ActionResult activateResponseDecoder
        , timeout = Nothing
        , tracker = Nothing
        }
