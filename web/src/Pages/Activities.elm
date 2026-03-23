module Pages.Activities exposing (Model, Msg, page)

import Browser.Dom
import Browser.Navigation as Nav
import Effect exposing (Effect)
import Html exposing (..)
import Http
import Json.Decode as Decode
import Models.Activity exposing (ActivitySummary, ClassificationSystem, SearchResults, activitySummaryDecoder, classificationSystemDecoder, searchResultsDecoder)
import Models.Database exposing (DatabaseList)
import Route exposing (ActivityTab(..), Route(..))
import Shared exposing (RemoteData(..))
import Spa.Page
import Task
import Url.Builder
import View exposing (View)
import Views.ActivitiesView as ActivitiesView
import Views.DatabasesView as DatabasesView


type alias Model =
    { searchQuery : String
    , dbName : String
    , results : SearchState
    , classificationSystems : Maybe (List ClassificationSystem)
    , selectedSystem : Maybe String
    , selectedValue : Maybe String
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
    | ClassificationsLoaded (Result Http.Error (List ClassificationSystem))
    | NewFlags Route.ActivitiesFlags
    | ScrollDone
    | RequestLoadDatabase


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
        searchQuery =
            Maybe.withDefault "" flags.name

        dbLoaded =
            Shared.isDatabaseLoaded shared flags.db

        hasTextQuery =
            not (String.isEmpty searchQuery)

        hasClassFilter =
            flags.classificationValue /= Nothing

        shouldSearch =
            (hasTextQuery || hasClassFilter) && dbLoaded
    in
    ( { searchQuery = searchQuery
      , dbName = flags.db
      , results =
            if shouldSearch then
                Searching

            else
                NotSearched
      , classificationSystems = Nothing
      , selectedSystem = flags.classification
      , selectedValue = flags.classificationValue
      }
    , Effect.batch
        [ if shouldSearch then
            Effect.fromCmd (searchActivities flags.db searchQuery flags.classification flags.classificationValue)

          else
            Effect.none
        , if dbLoaded then
            Effect.fromCmd (fetchClassifications flags.db)

          else
            Effect.none
        , Effect.fromCmd (Browser.Dom.focus "activity-search" |> Task.attempt (\_ -> ScrollDone))
        ]
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update shared msg model =
    case msg of
        ActivitiesViewMsg viewMsg ->
            case viewMsg of
                ActivitiesView.UpdateSearchQuery query ->
                    if not (Shared.isDatabaseLoaded shared model.dbName) then
                        ( { model | searchQuery = query }, Effect.none )

                    else
                        let
                            dbName =
                                model.dbName

                            queryName =
                                if String.isEmpty query then
                                    Nothing

                                else
                                    Just query

                            newRoute =
                                ActivitiesRoute { db = dbName, name = queryName, limit = Just 20, classification = model.selectedSystem, classificationValue = model.selectedValue }

                            hasQuery =
                                not (String.isEmpty query) || model.selectedValue /= Nothing

                            cmds =
                                if not hasQuery then
                                    Effect.fromCmd (Nav.replaceUrl shared.key (Route.routeToUrl newRoute))

                                else
                                    Effect.batch
                                        [ Effect.fromCmd (Nav.replaceUrl shared.key (Route.routeToUrl newRoute))
                                        , Effect.fromCmd (searchActivities model.dbName query model.selectedSystem model.selectedValue)
                                        ]
                        in
                        ( { model
                            | searchQuery = query
                            , results =
                                if not hasQuery then
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
                    , Effect.fromCmd (Nav.pushUrl shared.key (Route.routeToUrl (ActivityRoute Upstream model.dbName activityId)))
                    )

                ActivitiesView.LoadMore ->
                    case model.results of
                        Results results ->
                            let
                                newOffset =
                                    results.offset + results.limit
                            in
                            ( { model | results = LoadingMore results }
                            , Effect.fromCmd (searchActivitiesWithOffset model.dbName model.searchQuery model.selectedSystem model.selectedValue newOffset results.limit)
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
                            not (String.isEmpty model.searchQuery) || model.selectedValue /= Nothing
                    in
                    ( { model
                        | dbName = dbName
                        , results =
                            if shouldSearch then
                                Searching

                            else
                                NotSearched
                        , classificationSystems = Nothing
                        , selectedSystem = Nothing
                        , selectedValue = Nothing
                      }
                    , Effect.batch
                        [ Effect.fromCmd (Nav.pushUrl shared.key (Route.routeToUrl (ActivitiesRoute { db = dbName, name = queryName, limit = Just 20, classification = Nothing, classificationValue = Nothing })))
                        , if shouldSearch then
                            Effect.fromCmd (searchActivities dbName model.searchQuery Nothing Nothing)

                          else
                            Effect.none
                        , Effect.fromCmd (fetchClassifications dbName)
                        ]
                    )

                ActivitiesView.SelectClassificationSystem system ->
                    ( { model | selectedSystem = system, selectedValue = Nothing }
                    , Effect.none
                    )

                ActivitiesView.SelectClassificationValue value ->
                    let
                        newRoute =
                            ActivitiesRoute
                                { db = model.dbName
                                , name =
                                    if String.isEmpty model.searchQuery then
                                        Nothing

                                    else
                                        Just model.searchQuery
                                , limit = Just 20
                                , classification = model.selectedSystem
                                , classificationValue = value
                                }
                    in
                    ( { model
                        | selectedValue = value
                        , results =
                            if value /= Nothing || not (String.isEmpty model.searchQuery) then
                                Searching

                            else
                                NotSearched
                      }
                    , Effect.batch
                        [ Effect.fromCmd (Nav.replaceUrl shared.key (Route.routeToUrl newRoute))
                        , if value /= Nothing || not (String.isEmpty model.searchQuery) then
                            Effect.fromCmd (searchActivities model.dbName model.searchQuery model.selectedSystem value)

                          else
                            Effect.none
                        ]
                    )

        RequestLoadDatabase ->
            ( model
            , Effect.fromShared (Shared.LoadDatabase model.dbName)
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
                    , Effect.fromCmd (scrollToBottom "main-content")
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

        ClassificationsLoaded (Ok systems) ->
            ( { model | classificationSystems = Just systems }
            , Effect.none
            )

        ClassificationsLoaded (Err _) ->
            ( model, Effect.none )

        ScrollDone ->
            ( model, Effect.none )

        NewFlags flags ->
            let
                newQuery =
                    Maybe.withDefault "" flags.name

                dbNowLoaded =
                    Shared.isDatabaseLoaded shared flags.db

                -- Re-init if DB just became available with a pending query
                needsRetry =
                    model.results == NotSearched && not (String.isEmpty newQuery) && dbNowLoaded

                classChanged =
                    flags.classification /= model.selectedSystem || flags.classificationValue /= model.selectedValue
            in
            if newQuery == model.searchQuery && not needsRetry && not classChanged then
                ( model, Effect.none )

            else if newQuery == model.searchQuery && not needsRetry then
                -- Only classification changed — update state without stealing focus
                ( { model | selectedSystem = flags.classification, selectedValue = flags.classificationValue }
                , Effect.none
                )

            else
                init shared flags


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
    if not (Shared.isDatabaseLoaded shared model.dbName) then
        { title = "Activities"
        , body = Shared.viewLoadDatabasePrompt shared model.dbName RequestLoadDatabase
        }

    else
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
                    model.classificationSystems
                    model.selectedSystem
                    model.selectedValue
                )
        }



-- HTTP commands


searchActivities : String -> String -> Maybe String -> Maybe String -> Cmd Msg
searchActivities dbName query classification classificationValue =
    Http.get
        { url =
            Url.Builder.absolute
                [ "api", "v1", "db", dbName, "activities" ]
                (List.filterMap identity
                    [ if String.isEmpty query then
                        Nothing

                      else
                        Just (Url.Builder.string "name" query)
                    , Just (Url.Builder.int "limit" 20)
                    , Maybe.map (Url.Builder.string "classification") classification
                    , Maybe.map (Url.Builder.string "classification-value") classificationValue
                    ]
                )
        , expect = Http.expectJson SearchResultsLoaded (searchResultsDecoder activitySummaryDecoder)
        }


fetchClassifications : String -> Cmd Msg
fetchClassifications dbName =
    Http.get
        { url = Url.Builder.absolute [ "api", "v1", "db", dbName, "classifications" ] []
        , expect = Http.expectJson ClassificationsLoaded (Decode.list classificationSystemDecoder)
        }


scrollToBottom : String -> Cmd Msg
scrollToBottom containerId =
    Browser.Dom.getViewportOf containerId
        |> Task.andThen (\info -> Browser.Dom.setViewportOf containerId 0 info.scene.height)
        |> Task.attempt (\_ -> ScrollDone)


searchActivitiesWithOffset : String -> String -> Maybe String -> Maybe String -> Int -> Int -> Cmd Msg
searchActivitiesWithOffset dbName query classification classificationValue offset limit =
    Http.get
        { url =
            Url.Builder.absolute
                [ "api", "v1", "db", dbName, "activities" ]
                (List.filterMap identity
                    [ if String.isEmpty query then
                        Nothing

                      else
                        Just (Url.Builder.string "name" query)
                    , Just (Url.Builder.int "limit" limit)
                    , Just (Url.Builder.int "offset" offset)
                    , Maybe.map (Url.Builder.string "classification") classification
                    , Maybe.map (Url.Builder.string "classification-value") classificationValue
                    ]
                )
        , expect = Http.expectJson MoreResultsLoaded (searchResultsDecoder activitySummaryDecoder)
        }
