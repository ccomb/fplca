module Pages.Activities exposing (Model, Msg, page)

import Browser.Navigation as Nav
import Effect exposing (Effect)
import Html exposing (..)
import Http
import Models.Activity exposing (ActivitySummary, SearchResults, activitySummaryDecoder, searchResultsDecoder)
import Models.Database exposing (DatabaseList)
import Route exposing (Route(..))
import Shared exposing (RemoteData(..))
import Spa.Page
import Url.Builder
import View exposing (View)
import Views.ActivitiesView as ActivitiesView
import Views.DatabasesView as DatabasesView


type alias Model =
    { searchQuery : String
    , dbName : String
    , results : SearchState
    }


type SearchState
    = NotSearched
    | Searching
    | Results (SearchResults ActivitySummary)
    | LoadingMore (SearchResults ActivitySummary)
    | SearchFailed String


type Msg
    = ActivitiesViewMsg ActivitiesView.Msg
    | SearchResultsLoaded (Result Http.Error (SearchResults ActivitySummary))
    | MoreResultsLoaded (Result Http.Error (SearchResults ActivitySummary))
    | NewFlags Route.ActivitiesFlags


page : Shared.Model -> Spa.Page.Page Route.ActivitiesFlags Shared.Msg (View Msg) Model Msg
page shared =
    Spa.Page.element
        { init = init shared
        , update = update shared
        , view = view shared
        , subscriptions = \_ -> Sub.none
        }
        |> Spa.Page.onNewFlags NewFlags


init : Shared.Model -> Route.ActivitiesFlags -> ( Model, Effect Shared.Msg Msg )
init shared flags =
    let
        dbName =
            if String.isEmpty flags.db then
                Shared.getCurrentDbName shared

            else
                flags.db

        searchQuery =
            Maybe.withDefault "" flags.name

        shouldSearch =
            not (String.isEmpty searchQuery)
    in
    ( { searchQuery = searchQuery
      , dbName = dbName
      , results =
            if shouldSearch then
                Searching

            else
                NotSearched
      }
    , if shouldSearch then
        Effect.fromCmd (searchActivities dbName searchQuery)

      else
        Effect.none
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update shared msg model =
    case msg of
        ActivitiesViewMsg viewMsg ->
            case viewMsg of
                ActivitiesView.UpdateSearchQuery query ->
                    let
                        dbName =
                            model.dbName

                        queryName =
                            if String.isEmpty query then
                                Nothing

                            else
                                Just query

                        newRoute =
                            ActivitiesRoute { db = dbName, name = queryName, limit = Just 20 }

                        cmds =
                            if String.isEmpty query then
                                Effect.fromCmd (Nav.replaceUrl shared.key (Route.routeToUrl newRoute))

                            else
                                Effect.batch
                                    [ Effect.fromCmd (Nav.replaceUrl shared.key (Route.routeToUrl newRoute))
                                    , Effect.fromCmd (searchActivities model.dbName query)
                                    ]
                    in
                    ( { model
                        | searchQuery = query
                        , results =
                            if String.isEmpty query then
                                NotSearched

                            else
                                case model.results of
                                    Results _ ->
                                        model.results

                                    _ ->
                                        Searching
                      }
                    , cmds
                    )

                ActivitiesView.SelectActivity activityId ->
                    ( model
                    , Effect.fromCmd (Nav.pushUrl shared.key (Route.routeToUrl (ActivityUpstreamRoute model.dbName activityId)))
                    )

                ActivitiesView.LoadMore ->
                    case model.results of
                        Results results ->
                            let
                                newOffset =
                                    results.offset + results.limit
                            in
                            ( { model | results = LoadingMore results }
                            , Effect.fromCmd (searchActivitiesWithOffset model.dbName model.searchQuery newOffset results.limit)
                            )

                        _ ->
                            ( model, Effect.none )

                ActivitiesView.SelectDatabase dbName ->
                    let
                        queryName =
                            if String.isEmpty model.searchQuery then
                                Nothing

                            else
                                Just model.searchQuery

                        shouldSearch =
                            not (String.isEmpty model.searchQuery)
                    in
                    ( { model
                        | dbName = dbName
                        , results =
                            if shouldSearch then
                                Searching

                            else
                                NotSearched
                      }
                    , Effect.batch
                        [ Effect.fromCmd (Nav.pushUrl shared.key (Route.routeToUrl (ActivitiesRoute { db = dbName, name = queryName, limit = Just 20 })))
                        , if shouldSearch then
                            Effect.fromCmd (searchActivities dbName model.searchQuery)

                          else
                            Effect.none
                        ]
                    )

        SearchResultsLoaded (Ok results) ->
            ( { model | results = Results results }
            , Effect.none
            )

        SearchResultsLoaded (Err error) ->
            ( { model | results = SearchFailed (Shared.httpErrorToString error) }
            , Effect.none
            )

        MoreResultsLoaded (Ok newResults) ->
            case model.results of
                LoadingMore existingResults ->
                    ( { model
                        | results =
                            Results
                                { existingResults
                                    | results = existingResults.results ++ newResults.results
                                    , offset = newResults.offset
                                    , hasMore = newResults.hasMore
                                }
                      }
                    , Effect.none
                    )

                _ ->
                    ( model, Effect.none )

        MoreResultsLoaded (Err error) ->
            case model.results of
                LoadingMore existingResults ->
                    ( { model | results = Results existingResults }
                    , Effect.none
                    )

                _ ->
                    ( model, Effect.none )

        NewFlags flags ->
            let
                newQuery =
                    Maybe.withDefault "" flags.name
            in
            if newQuery == model.searchQuery then
                -- Flags from our own replaceUrl — ignore to preserve focus
                ( model, Effect.none )

            else
                -- External navigation (browser back/forward)
                ( { model
                    | searchQuery = newQuery
                    , results =
                        if String.isEmpty newQuery then
                            NotSearched

                        else
                            Searching
                  }
                , if String.isEmpty newQuery then
                    Effect.none

                  else
                    Effect.fromCmd (searchActivities model.dbName newQuery)
                )


view : Shared.Model -> Model -> View Msg
view shared model =
    let
        searchResults =
            case model.results of
                Results r ->
                    Just r

                LoadingMore r ->
                    Just r

                _ ->
                    Nothing

        searchLoading =
            case model.results of
                Searching ->
                    True

                _ ->
                    False

        loadingMore =
            case model.results of
                LoadingMore _ ->
                    True

                _ ->
                    False

        error =
            case model.results of
                SearchFailed err ->
                    Just err

                _ ->
                    Nothing

        maybeDatabaseList =
            case shared.databases of
                Loaded dbList ->
                    Just dbList

                _ ->
                    Nothing
    in
    { title = "Activities"
    , body =
        Html.map ActivitiesViewMsg
            (ActivitiesView.viewActivitiesPage
                model.dbName
                model.searchQuery
                searchResults
                searchLoading
                loadingMore
                error
                maybeDatabaseList
            )
    }



-- HTTP commands


searchActivities : String -> String -> Cmd Msg
searchActivities dbName query =
    Http.get
        { url =
            Url.Builder.absolute
                [ "api", "v1", "search", "activities" ]
                [ Url.Builder.string "name" query
                , Url.Builder.string "db" dbName
                , Url.Builder.int "limit" 20
                ]
        , expect = Http.expectJson SearchResultsLoaded (searchResultsDecoder activitySummaryDecoder)
        }


searchActivitiesWithOffset : String -> String -> Int -> Int -> Cmd Msg
searchActivitiesWithOffset dbName query offset limit =
    Http.get
        { url =
            Url.Builder.absolute
                [ "api", "v1", "search", "activities" ]
                [ Url.Builder.string "name" query
                , Url.Builder.string "db" dbName
                , Url.Builder.int "limit" limit
                , Url.Builder.int "offset" offset
                ]
        , expect = Http.expectJson MoreResultsLoaded (searchResultsDecoder activitySummaryDecoder)
        }
