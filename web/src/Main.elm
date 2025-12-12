module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode
import Models.Activity exposing (ActivityInfo, ActivitySummary, ActivityTree, SearchResults, activityInfoDecoder, activitySummaryDecoder, activityTreeDecoder, searchResultsDecoder)
import Models.Graph exposing (GraphData, graphDataDecoder)
import Models.Inventory exposing (InventoryExport, inventoryExportDecoder)
import Models.LCIA exposing (LCIAResult, MappingStatus, MethodSummary, lciaResultDecoder, mappingStatusDecoder, methodsListDecoder)
import Models.Page exposing (ExchangeTab(..), Page(..), Route(..))
import Url
import Url.Builder
import Url.Parser as Parser exposing ((</>), (<?>), Parser, oneOf, parse, string, top)
import Url.Parser.Query as Query
import Views.ActivitiesView as ActivitiesView
import Views.DetailsView as DetailsView
import Views.GraphView as GraphView
import Views.InventoryView as InventoryView
import Views.LCIAView as LCIAView
import Views.LeftMenu as LeftMenu
import Views.TreeView as TreeView


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = \model -> { title = "fpLCA", body = [ view model ] }
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , currentPage : Page
    , currentExchangeTab : ExchangeTab -- Current sub-tab for exchanges (upstream/emissions/consumptions)
    , cachedTrees : Dict.Dict String ActivityTree -- Cache trees by activity ID (for tree tab)
    , cachedActivityInfo : Dict.Dict String ActivityInfo -- Cache activity info by activity ID (for table tab)
    , cachedInventories : Dict.Dict String InventoryExport -- Cache inventories by activity ID
    , cachedGraphs : Dict.Dict String GraphData -- Cache graphs by activity ID
    , graphViewModel : Maybe GraphView.Model -- Current graph view model
    , treeViewModel : Maybe TreeView.Model -- Current tree view model
    , detailsViewModel : Maybe DetailsView.Model -- Current details view model
    , graphCutoffInput : String -- Cutoff input as string to allow partial edits like "0."
    , currentActivityId : String
    , loading : Bool
    , error : Maybe String
    , navigationHistory : List String
    , activitiesSearchQuery : String
    , searchResults : Maybe (SearchResults ActivitySummary)
    , searchLoading : Bool
    , loadingMore : Bool
    , hoveredNode : Maybe String
    , inventorySearchQuery : String
    , skipNextUrlChange : Bool -- Flag to prevent processing self-initiated URL changes
    , methods : Maybe (List MethodSummary) -- Available LCIA methods
    , selectedMethod : Maybe MethodSummary -- Currently selected method
    , lciaResult : Maybe LCIAResult -- LCIA computation result
    , mappingStatus : Maybe MappingStatus -- Flow mapping status
    , loadingMethods : Bool -- Loading methods list
    , loadingLCIA : Bool -- Loading LCIA computation
    }


type Msg
    = LoadActivity String
    | ActivityLoaded (Result Http.Error ActivityTree)
    | LoadActivityInfo String
    | ActivityInfoLoaded (Result Http.Error ActivityInfo)
    | LoadInventory String
    | InventoryLoaded (Result Http.Error InventoryExport)
    | LoadGraph String
    | GraphLoaded (Result Http.Error GraphData)
    | GraphViewMsg GraphView.Msg
    | TreeViewMsg TreeView.Msg
    | DetailsViewMsg DetailsView.Msg
    | SwitchExchangeTab ExchangeTab
    | UpdateGraphCutoff String
    | NavigateToParent
    | NodeClicked String
    | NavigateToPage Page
    | UpdateSearchQuery String
    | SearchActivities String
    | ActivitiesSearchResults (Result Http.Error (SearchResults ActivitySummary))
    | LoadMoreActivities
    | MoreActivitiesLoaded (Result Http.Error (SearchResults ActivitySummary))
    | SelectActivity String
    | NodeHovered (Maybe String)
    | UpdateInventorySearchQuery String
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
      -- LCIA messages
    | LoadMethods
    | MethodsLoaded (Result Http.Error (List MethodSummary))
    | LCIAViewMsg LCIAView.Msg
    | ComputeLCIA String
    | LCIAResultLoaded (Result Http.Error LCIAResult)
    | MappingStatusLoaded (Result Http.Error MappingStatus)



-- URL parsing


activitiesQueryParser : Query.Parser { name : Maybe String, limit : Maybe Int }
activitiesQueryParser =
    Query.map2 (\name limit -> { name = name, limit = limit })
        (Query.string "name")
        (Query.int "limit")


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ Parser.map ActivitiesRoute (top <?> activitiesQueryParser)
        , Parser.map ActivitiesRoute (Parser.s "activities" <?> activitiesQueryParser)
        , Parser.map ActivityDetailsRoute (Parser.s "activity" </> string </> Parser.s "details")
        , Parser.map ActivityTreeRoute (Parser.s "activity" </> string </> Parser.s "tree")
        , Parser.map ActivityInventoryRoute (Parser.s "activity" </> string </> Parser.s "inventory")
        , Parser.map ActivityGraphRoute (Parser.s "activity" </> string </> Parser.s "graph")
        , Parser.map ActivityLCIARoute (Parser.s "activity" </> string </> Parser.s "lcia")
        , Parser.map ActivityRoute (Parser.s "activity" </> string)
        ]


parseUrl : Url.Url -> Route
parseUrl url =
    Parser.parse routeParser url
        |> Maybe.withDefault NotFoundRoute


routeToPage : Route -> Page
routeToPage route =
    case route of
        ActivitiesRoute _ ->
            ActivitiesPage

        ActivityRoute _ ->
            DetailsPage

        ActivityDetailsRoute _ ->
            DetailsPage

        ActivityTreeRoute _ ->
            TreePage

        ActivityInventoryRoute _ ->
            InventoryPage

        ActivityGraphRoute _ ->
            GraphPage

        ActivityLCIARoute _ ->
            LCIAPage

        NotFoundRoute ->
            ActivitiesPage


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        route =
            parseUrl url

        defaultActivityId =
            "22222222-3333-4444-5555-666666666661_chemical-b-uuid"

        routeConfig =
            case route of
                ActivitiesRoute { name } ->
                    { activityId = defaultActivityId
                    , shouldLoad = False
                    , loadType = "none"
                    , searchQuery = Maybe.withDefault "" name
                    }

                ActivityRoute processId ->
                    { activityId = processId
                    , shouldLoad = True
                    , loadType = "details"
                    , searchQuery = ""
                    }

                ActivityDetailsRoute processId ->
                    { activityId = processId
                    , shouldLoad = True
                    , loadType = "details"
                    , searchQuery = ""
                    }

                ActivityTreeRoute processId ->
                    { activityId = processId
                    , shouldLoad = True
                    , loadType = "tree"
                    , searchQuery = ""
                    }

                ActivityInventoryRoute processId ->
                    { activityId = processId
                    , shouldLoad = True
                    , loadType = "inventory"
                    , searchQuery = ""
                    }

                ActivityGraphRoute processId ->
                    { activityId = processId
                    , shouldLoad = True
                    , loadType = "graph"
                    , searchQuery = ""
                    }

                ActivityLCIARoute processId ->
                    { activityId = processId
                    , shouldLoad = True
                    , loadType = "lcia"
                    , searchQuery = ""
                    }

                NotFoundRoute ->
                    { activityId = defaultActivityId
                    , shouldLoad = False
                    , loadType = "none"
                    , searchQuery = ""
                    }

        initialPage =
            routeToPage route

        shouldSearch =
            not (String.isEmpty routeConfig.searchQuery)

        model =
            { key = key
            , url = url
            , currentPage = initialPage
            , currentExchangeTab = UpstreamTab
            , cachedTrees = Dict.empty
            , cachedActivityInfo = Dict.empty
            , cachedInventories = Dict.empty
            , cachedGraphs = Dict.empty
            , graphViewModel = Nothing
            , treeViewModel = Nothing
            , detailsViewModel = Nothing
            , graphCutoffInput = "1.0"
            , currentActivityId = routeConfig.activityId
            , loading = routeConfig.shouldLoad
            , error = Nothing
            , navigationHistory = []
            , activitiesSearchQuery = routeConfig.searchQuery
            , searchResults = Nothing
            , searchLoading = shouldSearch
            , loadingMore = False
            , hoveredNode = Nothing
            , inventorySearchQuery = ""
            , skipNextUrlChange = False
            , methods = Nothing
            , selectedMethod = Nothing
            , lciaResult = Nothing
            , mappingStatus = Nothing
            , loadingMethods = routeConfig.loadType == "lcia"
            , loadingLCIA = False
            }

        cmd =
            if routeConfig.shouldLoad then
                case routeConfig.loadType of
                    "details" ->
                        loadActivityInfo routeConfig.activityId

                    "tree" ->
                        loadActivityTree routeConfig.activityId

                    "inventory" ->
                        loadInventoryData routeConfig.activityId

                    "graph" ->
                        let
                            cutoff =
                                String.toFloat model.graphCutoffInput |> Maybe.withDefault 1.0
                        in
                        -- Load both graph and tree data to get activity name
                        Cmd.batch
                            [ loadGraphData routeConfig.activityId cutoff
                            , loadActivityTree routeConfig.activityId
                            ]

                    "lcia" ->
                        -- Load methods and activity tree for LCIA page
                        Cmd.batch
                            [ loadMethods
                            , loadActivityTree routeConfig.activityId
                            ]

                    _ ->
                        Cmd.none

            else if shouldSearch then
                searchActivities routeConfig.searchQuery

            else
                Cmd.none
    in
    ( model, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadActivity activityId ->
            let
                shouldLoad =
                    not (Dict.member activityId model.cachedTrees)
            in
            ( { model
                | loading = shouldLoad
                , error = Nothing
                , currentActivityId = activityId
              }
            , if shouldLoad then
                loadActivityTree activityId

              else
                Cmd.none
            )

        ActivityLoaded (Ok tree) ->
            let
                treeViewModel =
                    TreeView.init tree

                detailsViewModel =
                    DetailsView.init tree model.currentActivityId
            in
            ( { model
                | cachedTrees = Dict.insert model.currentActivityId tree model.cachedTrees
                , treeViewModel = Just treeViewModel
                , detailsViewModel = Just detailsViewModel
                , loading = False
                , error = Nothing
              }
            , Cmd.none
            )

        ActivityLoaded (Err error) ->
            ( { model
                | loading = False
                , error = Just (httpErrorToString error)
              }
            , Cmd.none
            )

        LoadActivityInfo activityId ->
            let
                shouldLoad =
                    not (Dict.member activityId model.cachedActivityInfo)
            in
            ( { model
                | loading = shouldLoad
                , error = Nothing
                , currentActivityId = activityId
              }
            , if shouldLoad then
                loadActivityInfo activityId

              else
                Cmd.none
            )

        ActivityInfoLoaded (Ok activityInfo) ->
            ( { model
                | cachedActivityInfo = Dict.insert model.currentActivityId activityInfo model.cachedActivityInfo
                , loading = False
                , error = Nothing
              }
            , Cmd.none
            )

        ActivityInfoLoaded (Err error) ->
            ( { model
                | loading = False
                , error = Just (httpErrorToString error)
              }
            , Cmd.none
            )

        LoadInventory activityId ->
            let
                shouldLoad =
                    not (Dict.member activityId model.cachedInventories)
            in
            ( { model
                | loading = shouldLoad
                , error = Nothing
                , currentActivityId = activityId
              }
            , if shouldLoad then
                loadInventoryData activityId

              else
                Cmd.none
            )

        InventoryLoaded (Ok inventory) ->
            ( { model
                | cachedInventories = Dict.insert model.currentActivityId inventory model.cachedInventories
                , loading = False
                , error = Nothing
              }
            , Cmd.none
            )

        InventoryLoaded (Err error) ->
            ( { model
                | loading = False
                , error = Just (httpErrorToString error)
              }
            , Cmd.none
            )

        LoadGraph activityId ->
            -- Always reload when explicitly requested (e.g., after changing cutoff)
            let
                cutoff =
                    String.toFloat model.graphCutoffInput |> Maybe.withDefault 1.0
            in
            ( { model
                | loading = True
                , error = Nothing
                , currentActivityId = activityId
              }
            , loadGraphData activityId cutoff
            )

        GraphLoaded (Ok graphData) ->
            let
                graphViewModel =
                    GraphView.init graphData
            in
            ( { model
                | cachedGraphs = Dict.insert model.currentActivityId graphData model.cachedGraphs
                , graphViewModel = Just graphViewModel
                , loading = False
                , error = Nothing
              }
            , Cmd.none
            )

        GraphLoaded (Err error) ->
            ( { model
                | loading = False
                , error = Just (httpErrorToString error)
              }
            , Cmd.none
            )

        GraphViewMsg graphMsg ->
            case model.graphViewModel of
                Just graphModel ->
                    let
                        updatedGraphModel =
                            GraphView.update graphMsg graphModel
                    in
                    ( { model | graphViewModel = Just updatedGraphModel }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        TreeViewMsg treeMsg ->
            case model.treeViewModel of
                Just treeModel ->
                    let
                        updatedTreeModel =
                            TreeView.update treeMsg treeModel
                    in
                    -- Handle double-click navigation
                    case treeMsg of
                        TreeView.NodeDoubleClick nodeId ->
                            -- Look up the ProcessId from the Int node ID
                            case Dict.get nodeId treeModel.idMapping of
                                Just processId ->
                                    if processId /= model.currentActivityId then
                                        ( { model
                                            | treeViewModel = Just updatedTreeModel
                                            , navigationHistory = model.currentActivityId :: model.navigationHistory
                                          }
                                        , Nav.pushUrl model.key (routeToUrl (ActivityTreeRoute processId))
                                        )

                                    else
                                        ( { model | treeViewModel = Just updatedTreeModel }, Cmd.none )

                                Nothing ->
                                    ( { model | treeViewModel = Just updatedTreeModel }, Cmd.none )

                        _ ->
                            ( { model | treeViewModel = Just updatedTreeModel }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        DetailsViewMsg detailsMsg ->
            case detailsMsg of
                DetailsView.NavigateToActivity processId ->
                    ( { model
                        | navigationHistory = model.currentActivityId :: model.navigationHistory
                      }
                    , Nav.pushUrl model.key (routeToUrl (ActivityDetailsRoute processId))
                    )

                DetailsView.NavigateBack ->
                    case model.navigationHistory of
                        parentId :: rest ->
                            ( { model
                                | navigationHistory = rest
                              }
                            , Nav.pushUrl model.key (routeToUrl (ActivityDetailsRoute parentId))
                            )

                        [] ->
                            ( model, Cmd.none )

        SwitchExchangeTab tab ->
            ( { model | currentExchangeTab = tab }, Cmd.none )

        UpdateGraphCutoff cutoffStr ->
            -- Allow any string input, validation happens when loading
            ( { model | graphCutoffInput = cutoffStr }, Cmd.none )

        NodeClicked nodeId ->
            if nodeId /= model.currentActivityId then
                ( { model
                    | navigationHistory = model.currentActivityId :: model.navigationHistory
                  }
                , Nav.pushUrl model.key (routeToUrl (ActivityDetailsRoute nodeId))
                )

            else
                ( model, Cmd.none )

        NavigateToParent ->
            case Dict.get model.currentActivityId model.cachedTrees of
                Just tree ->
                    case Dict.get model.currentActivityId tree.nodes of
                        Just currentNode ->
                            case currentNode.parentId of
                                Just parentId ->
                                    ( { model
                                        | navigationHistory = List.drop 1 model.navigationHistory
                                      }
                                    , Nav.pushUrl model.key (routeToUrl (ActivityTreeRoute parentId))
                                    )

                                Nothing ->
                                    case model.navigationHistory of
                                        parentId :: rest ->
                                            ( { model
                                                | navigationHistory = rest
                                              }
                                            , Nav.pushUrl model.key (routeToUrl (ActivityTreeRoute parentId))
                                            )

                                        [] ->
                                            ( model, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        NavigateToPage page ->
            let
                route =
                    case page of
                        ActivitiesPage ->
                            let
                                queryName =
                                    if String.isEmpty model.activitiesSearchQuery then
                                        Nothing
                                    else
                                        Just model.activitiesSearchQuery
                            in
                            ActivitiesRoute { name = queryName, limit = Just 20 }

                        DetailsPage ->
                            ActivityDetailsRoute model.currentActivityId

                        TreePage ->
                            ActivityTreeRoute model.currentActivityId

                        InventoryPage ->
                            ActivityInventoryRoute model.currentActivityId

                        GraphPage ->
                            ActivityGraphRoute model.currentActivityId

                        LCIAPage ->
                            ActivityLCIARoute model.currentActivityId
            in
            ( model, Nav.pushUrl model.key (routeToUrl route) )

        UpdateSearchQuery query ->
            let
                queryName =
                    if String.isEmpty query then
                        Nothing

                    else
                        Just query

                newRoute =
                    ActivitiesRoute { name = queryName, limit = Just 20 }

                -- Search if query is not empty, clear results if empty
                cmds =
                    if String.isEmpty query then
                        Cmd.batch
                            [ Nav.replaceUrl model.key (routeToUrl newRoute)
                            ]

                    else
                        Cmd.batch
                            [ Nav.replaceUrl model.key (routeToUrl newRoute)
                            , searchActivities query
                            ]

                newModel =
                    if String.isEmpty query then
                        { model
                            | activitiesSearchQuery = query
                            , searchResults = Nothing
                            , skipNextUrlChange = True
                        }

                    else
                        { model
                            | activitiesSearchQuery = query
                            , skipNextUrlChange = True
                        }
            in
            ( newModel, cmds )

        SearchActivities query ->
            ( { model | searchLoading = True, error = Nothing }
            , searchActivities query
            )

        ActivitiesSearchResults (Ok results) ->
            ( { model
                | searchResults = Just results
                , searchLoading = False
                , loadingMore = False
                , error = Nothing
              }
            , Cmd.none
            )

        ActivitiesSearchResults (Err error) ->
            ( { model
                | searchResults = Nothing
                , searchLoading = False
                , loadingMore = False
                , error = Just (httpErrorToString error)
              }
            , Cmd.none
            )

        LoadMoreActivities ->
            case model.searchResults of
                Just results ->
                    let
                        newOffset =
                            results.offset + results.limit
                    in
                    ( { model | loadingMore = True }
                    , searchActivitiesWithOffset model.activitiesSearchQuery newOffset results.limit
                    )

                Nothing ->
                    ( model, Cmd.none )

        MoreActivitiesLoaded (Ok newResults) ->
            case model.searchResults of
                Just existingResults ->
                    ( { model
                        | searchResults =
                            Just
                                { existingResults
                                    | results = existingResults.results ++ newResults.results
                                    , offset = newResults.offset
                                    , hasMore = newResults.hasMore
                                }
                        , loadingMore = False
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( { model | loadingMore = False }, Cmd.none )

        MoreActivitiesLoaded (Err error) ->
            ( { model
                | loadingMore = False
                , error = Just (httpErrorToString error)
              }
            , Cmd.none
            )

        SelectActivity activityId ->
            ( { model
                | navigationHistory = model.currentActivityId :: model.navigationHistory
              }
            , navigateToActivity model.key activityId
            )

        NodeHovered nodeId ->
            ( { model | hoveredNode = nodeId }, Cmd.none )

        UpdateInventorySearchQuery query ->
            ( { model | inventorySearchQuery = query }, Cmd.none )

        -- LCIA message handlers
        LoadMethods ->
            ( { model | loadingMethods = True, error = Nothing }
            , loadMethods
            )

        MethodsLoaded (Ok methodsList) ->
            ( { model
                | methods = Just methodsList
                , loadingMethods = False
                , error = Nothing
              }
            , Cmd.none
            )

        MethodsLoaded (Err error) ->
            ( { model
                | loadingMethods = False
                , error = Just (httpErrorToString error)
              }
            , Cmd.none
            )

        LCIAViewMsg lciaMsg ->
            case lciaMsg of
                LCIAView.SelectMethod methodId ->
                    let
                        selectedMethod =
                            model.methods
                                |> Maybe.andThen (List.filter (\m -> m.msmId == methodId) >> List.head)
                    in
                    ( { model
                        | selectedMethod = selectedMethod
                        , lciaResult = Nothing
                        , mappingStatus = Nothing
                      }
                    , Cmd.none
                    )

                LCIAView.ComputeLCIA methodId ->
                    ( { model | loadingLCIA = True, error = Nothing }
                    , Cmd.batch
                        [ computeLCIA model.currentActivityId methodId
                        , loadMappingStatus methodId
                        ]
                    )

        ComputeLCIA methodId ->
            ( { model | loadingLCIA = True, error = Nothing }
            , Cmd.batch
                [ computeLCIA model.currentActivityId methodId
                , loadMappingStatus methodId
                ]
            )

        LCIAResultLoaded (Ok result) ->
            ( { model
                | lciaResult = Just result
                , loadingLCIA = False
                , error = Nothing
              }
            , Cmd.none
            )

        LCIAResultLoaded (Err error) ->
            ( { model
                | loadingLCIA = False
                , error = Just (httpErrorToString error)
              }
            , Cmd.none
            )

        MappingStatusLoaded (Ok status) ->
            ( { model | mappingStatus = Just status }
            , Cmd.none
            )

        MappingStatusLoaded (Err _) ->
            -- Non-critical error, just ignore
            ( model, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            -- Check if we should skip this URL change (it was triggered by us)
            if model.skipNextUrlChange then
                ( { model | skipNextUrlChange = False }, Cmd.none )

            else
                let
                    route =
                        parseUrl url

                    newPage =
                        routeToPage route

                    routeInfo =
                        case route of
                            ActivitiesRoute { name } ->
                                { activityId = model.currentActivityId, needsActivity = False, searchQuery = Maybe.withDefault "" name }

                            ActivityRoute processId ->
                                { activityId = processId, needsActivity = True, searchQuery = model.activitiesSearchQuery }

                            ActivityDetailsRoute processId ->
                                { activityId = processId, needsActivity = True, searchQuery = model.activitiesSearchQuery }

                            ActivityTreeRoute processId ->
                                { activityId = processId, needsActivity = True, searchQuery = model.activitiesSearchQuery }

                            ActivityInventoryRoute processId ->
                                { activityId = processId, needsActivity = True, searchQuery = model.activitiesSearchQuery }

                            ActivityGraphRoute processId ->
                                { activityId = processId, needsActivity = True, searchQuery = model.activitiesSearchQuery }

                            ActivityLCIARoute processId ->
                                { activityId = processId, needsActivity = True, searchQuery = model.activitiesSearchQuery }

                            NotFoundRoute ->
                                { activityId = model.currentActivityId, needsActivity = False, searchQuery = model.activitiesSearchQuery }

                    -- For DetailsPage: load activity info
                    shouldLoadActivityInfo =
                        routeInfo.needsActivity
                            && newPage
                            == DetailsPage
                            && not (Dict.member routeInfo.activityId model.cachedActivityInfo)

                    -- For TreePage: load tree data
                    shouldLoadTree =
                        routeInfo.needsActivity
                            && newPage
                            == TreePage
                            && not (Dict.member routeInfo.activityId model.cachedTrees)

                    shouldLoadInventory =
                        routeInfo.needsActivity && newPage == InventoryPage && not (Dict.member routeInfo.activityId model.cachedInventories)

                    shouldLoadGraph =
                        routeInfo.needsActivity && newPage == GraphPage && not (Dict.member routeInfo.activityId model.cachedGraphs)

                    -- Also load tree data for graph page to get activity name
                    shouldLoadTreeForGraph =
                        shouldLoadGraph && not (Dict.member routeInfo.activityId model.cachedTrees)

                    shouldLoadLCIA =
                        routeInfo.needsActivity && newPage == LCIAPage && model.methods == Nothing

                    shouldLoad =
                        shouldLoadActivityInfo || shouldLoadTree || shouldLoadInventory || shouldLoadGraph || shouldLoadLCIA

                    shouldSearch =
                        newPage == ActivitiesPage && not (String.isEmpty routeInfo.searchQuery)

                    -- Re-initialize view models when navigating to TreePage with cached tree data
                    newTreeViewModel =
                        if newPage == TreePage then
                            case Dict.get routeInfo.activityId model.cachedTrees of
                                Just cachedTree ->
                                    Just (TreeView.init cachedTree)

                                Nothing ->
                                    model.treeViewModel

                        else
                            model.treeViewModel

                    updatedModel =
                        { model
                            | url = url
                            , currentPage = newPage
                            , currentActivityId = routeInfo.activityId
                            , loading = shouldLoad
                            , navigationHistory = model.navigationHistory
                            , activitiesSearchQuery = routeInfo.searchQuery
                            , searchLoading = shouldSearch
                            , treeViewModel = newTreeViewModel
                            , loadingMethods = shouldLoadLCIA
                        }

                    cmd =
                        if shouldLoadActivityInfo then
                            loadActivityInfo routeInfo.activityId

                        else if shouldLoadTree then
                            loadActivityTree routeInfo.activityId

                        else if shouldLoadInventory then
                            loadInventoryData routeInfo.activityId

                        else if shouldSearch then
                            searchActivities routeInfo.searchQuery

                        else if shouldLoadGraph then
                            let
                                cutoff =
                                    String.toFloat updatedModel.graphCutoffInput |> Maybe.withDefault 1.0

                                graphCmd =
                                    loadGraphData routeInfo.activityId cutoff

                                treeCmd =
                                    if shouldLoadTreeForGraph then
                                        loadActivityTree routeInfo.activityId

                                    else
                                        Cmd.none
                            in
                            Cmd.batch [ graphCmd, treeCmd ]

                        else if shouldLoadLCIA then
                            Cmd.batch
                                [ loadMethods
                                , loadActivityTree routeInfo.activityId
                                ]

                        else
                            Cmd.none
                in
                ( updatedModel, cmd )



-- Navigation helpers


routeToUrl : Route -> String
routeToUrl route =
    case route of
        ActivitiesRoute { name, limit } ->
            let
                queryParams =
                    [ Maybe.map (\n -> "name=" ++ n) name
                    , Maybe.map (\l -> "limit=" ++ String.fromInt l) limit
                    ]
                        |> List.filterMap identity
                        |> String.join "&"

                queryString =
                    if String.isEmpty queryParams then
                        ""

                    else
                        "?" ++ queryParams
            in
            "/activities" ++ queryString

        ActivityRoute processId ->
            "/activity/" ++ processId

        ActivityDetailsRoute processId ->
            "/activity/" ++ processId ++ "/details"

        ActivityTreeRoute processId ->
            "/activity/" ++ processId ++ "/tree"

        ActivityInventoryRoute processId ->
            "/activity/" ++ processId ++ "/inventory"

        ActivityGraphRoute processId ->
            "/activity/" ++ processId ++ "/graph"

        ActivityLCIARoute processId ->
            "/activity/" ++ processId ++ "/lcia"

        NotFoundRoute ->
            "/"


navigateToActivity : Nav.Key -> String -> Cmd Msg
navigateToActivity key processId =
    Nav.pushUrl key (routeToUrl (ActivityDetailsRoute processId))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ case model.graphViewModel of
            Just graphModel ->
                Sub.map GraphViewMsg (GraphView.subscriptions graphModel)

            Nothing ->
                Sub.none
        , case model.treeViewModel of
            Just treeModel ->
                Sub.map TreeViewMsg (TreeView.subscriptions treeModel)

            Nothing ->
                Sub.none
        ]


view : Model -> Html Msg
view model =
    div [ class "app-container" ]
        [ Html.map NavigateToPage (LeftMenu.viewLeftMenu model.currentPage model.currentActivityId)
        , div [ class "main-content" ]
            [ case model.currentPage of
                ActivitiesPage ->
                    Html.map
                        (\msg ->
                            case msg of
                                ActivitiesView.UpdateSearchQuery query ->
                                    UpdateSearchQuery query

                                ActivitiesView.SelectActivity activityId ->
                                    SelectActivity activityId

                                ActivitiesView.LoadMore ->
                                    LoadMoreActivities
                        )
                        (ActivitiesView.viewActivitiesPage
                            model.activitiesSearchQuery
                            model.searchResults
                            model.searchLoading
                            model.loadingMore
                            model.error
                        )

                DetailsPage ->
                    viewDetailsPage model

                TreePage ->
                    viewTreePage model

                InventoryPage ->
                    Html.map
                        (\msg ->
                            case msg of
                                InventoryView.UpdateSearchQuery query ->
                                    UpdateInventorySearchQuery query
                        )
                        (InventoryView.viewInventoryPage
                            (Dict.get model.currentActivityId model.cachedInventories)
                            model.loading
                            model.error
                            model.inventorySearchQuery
                        )

                GraphPage ->
                    viewGraphPage model

                LCIAPage ->
                    viewLCIAPage model
            ]
        ]


viewLCIAPage : Model -> Html Msg
viewLCIAPage model =
    let
        activityInfo =
            Dict.get model.currentActivityId model.cachedTrees
                |> Maybe.andThen (\tree -> Dict.get model.currentActivityId tree.nodes)
                |> Maybe.map (\node -> ( node.name, node.location ))
    in
    Html.map LCIAViewMsg
        (LCIAView.viewLCIAPage
            model.methods
            model.selectedMethod
            model.lciaResult
            model.mappingStatus
            model.loadingMethods
            model.loadingLCIA
            model.error
            activityInfo
        )


viewDetailsPage : Model -> Html Msg
viewDetailsPage model =
    div [ class "details-page-container" ]
        [ case ( model.loading, model.error ) of
            ( True, _ ) ->
                div [ class "has-text-centered" ]
                    [ div [ class "is-size-3" ] [ text "Loading..." ]
                    , progress [ class "progress is-primary", attribute "max" "100" ] []
                    ]

            ( _, Just err ) ->
                div [ class "notification is-danger" ]
                    [ button [ class "delete", onClick (LoadActivityInfo model.currentActivityId) ] []
                    , strong [] [ text "Error: " ]
                    , text err
                    ]

            ( False, Nothing ) ->
                case Dict.get model.currentActivityId model.cachedActivityInfo of
                    Just activityInfo ->
                        div []
                            [ viewActivityHeaderWithDoc activityInfo (canNavigateBack model)
                            , viewExchangeTabs model activityInfo
                            ]

                    Nothing ->
                        div [ class "has-text-centered" ]
                            [ text "Loading activity data..." ]
        ]


viewActivityHeaderWithDoc : Models.Activity.ActivityInfo -> Bool -> Html Msg
viewActivityHeaderWithDoc activityInfo canGoBack =
    let
        hasDescription =
            not (List.isEmpty activityInfo.description)
    in
    div [ class "box", style "margin-bottom" "0" ]
        [ -- Title and location on same line
          div [ class "level", style "margin-bottom" "0" ]
            [ div [ class "level-left" ]
                ([ if canGoBack then
                    div [ class "level-item" ]
                        [ button
                            [ class "button is-primary"
                            , onClick (DetailsViewMsg DetailsView.NavigateBack)
                            ]
                            [ span [ class "icon" ]
                                [ i [ class "fas fa-arrow-left" ] []
                                ]
                            , span [] [ text "Previous Activity" ]
                            ]
                        ]

                   else
                    text ""
                 , div [ class "level-item" ]
                    [ h1 [ class "title is-4", style "margin-bottom" "0" ]
                        (case activityInfo.referenceProduct of
                            Just product ->
                                [ text activityInfo.name
                                , span [ style "color" "#888", style "margin" "0 0.5rem" ] [ text "→" ]
                                , span [ style "font-weight" "normal" ] [ text product ]
                                ]

                            Nothing ->
                                [ text activityInfo.name ]
                        )
                    ]
                 , div [ class "level-item" ]
                    [ span [ class "tag is-light" ] [ text activityInfo.location ]
                    ]
                 ]
                )
            ]
        , -- Description below
          if hasDescription then
            div [ style "font-size" "0.85rem", style "line-height" "1.4", style "margin-top" "0.5rem" ]
                (activityInfo.description
                    |> List.map (\para -> p [ style "margin-bottom" "0.25rem" ] [ text para ])
                )

          else
            text ""
        ]


viewExchangeTabs : Model -> Models.Activity.ActivityInfo -> Html Msg
viewExchangeTabs model activityInfo =
    let
        -- Upstream: technosphere inputs that are not reference products
        upstreamExchanges =
            List.filter (\ex -> ex.exchangeType == Models.Activity.TechnosphereExchangeType && ex.isInput && not ex.isReference) activityInfo.exchanges

        emissionExchanges =
            List.filter (\ex -> ex.exchangeType == Models.Activity.BiosphereEmissionType) activityInfo.exchanges

        consumptionExchanges =
            List.filter (\ex -> ex.exchangeType == Models.Activity.BiosphereResourceType) activityInfo.exchanges

        -- Products: all products from same activityUUID (from backend)
        allProducts =
            activityInfo.allProducts

        upstreamCount =
            List.length upstreamExchanges

        emissionCount =
            List.length emissionExchanges

        consumptionCount =
            List.length consumptionExchanges

        productCount =
            List.length allProducts
    in
    div [ class "box" ]
        [ div [ class "tabs is-boxed" ]
            [ ul []
                [ viewExchangeTabItem UpstreamTab model.currentExchangeTab ("Upstream activities (" ++ String.fromInt upstreamCount ++ ")") (upstreamCount > 0)
                , viewExchangeTabItem EmissionsTab model.currentExchangeTab ("Direct emissions (" ++ String.fromInt emissionCount ++ ")") (emissionCount > 0)
                , viewExchangeTabItem ConsumptionsTab model.currentExchangeTab ("Natural resources (" ++ String.fromInt consumptionCount ++ ")") (consumptionCount > 0)
                , viewExchangeTabItem ProductsTab model.currentExchangeTab ("Products (" ++ String.fromInt productCount ++ ")") (productCount > 0)
                ]
            ]
        , case model.currentExchangeTab of
            UpstreamTab ->
                DetailsView.viewUpstreamExchanges upstreamExchanges (\processId -> DetailsViewMsg (DetailsView.NavigateToActivity processId))

            EmissionsTab ->
                DetailsView.viewEmissionsExchanges emissionExchanges

            ConsumptionsTab ->
                DetailsView.viewNaturalResourcesExchanges consumptionExchanges

            ProductsTab ->
                DetailsView.viewAllProducts allProducts model.currentActivityId (\processId -> DetailsViewMsg (DetailsView.NavigateToActivity processId))
        ]


viewExchangeTabItem : ExchangeTab -> ExchangeTab -> String -> Bool -> Html Msg
viewExchangeTabItem tab currentTab label isEnabled =
    let
        isActive =
            currentTab == tab
    in
    li
        [ classList
            [ ( "is-active", isActive )
            ]
        ]
        [ if isActive then
            -- Active tab: white text on black background
            a
                [ href "#"
                , style "background-color" "#363636"
                , style "color" "white"
                , style "border-color" "#363636"
                , preventDefaultOn "click" (Json.Decode.succeed ( SwitchExchangeTab tab, True ))
                ]
                [ text label ]

          else if isEnabled then
            -- Inactive enabled tab: dark grey text
            a
                [ href "#"
                , style "color" "#4a4a4a"
                , preventDefaultOn "click" (Json.Decode.succeed ( SwitchExchangeTab tab, True ))
                ]
                [ text label ]

          else
            -- Disabled tab: light grey text
            a
                [ style "color" "#b5b5b5"
                , style "cursor" "not-allowed"
                , style "pointer-events" "none"
                ]
                [ text label ]
        ]


canNavigateBack : Model -> Bool
canNavigateBack model =
    not (List.isEmpty model.navigationHistory)


viewTreePage : Model -> Html Msg
viewTreePage model =
    let
        activityInfo =
            Dict.get model.currentActivityId model.cachedTrees
                |> Maybe.andThen (\tree -> Dict.get model.currentActivityId tree.nodes)
                |> Maybe.map (\node -> ( node.name, node.location ))
    in
    div [ class "tree-page" ]
        [ viewPageNavbar "Activity Tree Navigator" activityInfo
        , -- Main content
          div []
            [ div []
                [ case ( model.loading, model.error, Dict.get model.currentActivityId model.cachedTrees ) of
                    ( True, _, _ ) ->
                        div [ class "has-text-centered" ]
                            [ div [ class "is-size-3" ] [ text "Loading..." ]
                            , progress [ class "progress is-primary", attribute "max" "100" ] []
                            ]

                    ( _, Just error, _ ) ->
                        div [ class "notification is-danger" ]
                            [ button [ class "delete", onClick (LoadActivity model.currentActivityId) ] []
                            , strong [] [ text "Error: " ]
                            , text error
                            ]

                    ( _, _, Just tree ) ->
                        div []
                            [ -- Navigation and metadata in single banner
                              div [ class "level", style "height" "70px" ]
                                [ div [ class "level-left" ]
                                    [ div [ class "level-item" ]
                                        [ button
                                            [ class "button is-primary"
                                            , onClick NavigateToParent
                                            , disabled (not (canNavigateToParent model))
                                            ]
                                            [ span [ class "icon" ] [ i [ class "fas fa-arrow-up" ] [] ]
                                            , span [] [ text "Parent Activity" ]
                                            ]
                                        ]
                                    ]
                                , div [ class "level-right" ]
                                    [ div [ class "level-item" ]
                                        [ text ("Total nodes: " ++ String.fromInt tree.tree.totalNodes) ]
                                    , div [ class "level-item" ]
                                        [ text ("Max depth: " ++ String.fromInt tree.tree.maxDepth) ]
                                    , div [ class "level-item" ]
                                        [ text ("Expandable: " ++ String.fromInt tree.tree.expandableNodes) ]
                                    ]
                                ]
                            , -- Force-directed Tree visualization
                              case model.treeViewModel of
                                Just treeModel ->
                                    Html.map TreeViewMsg (TreeView.view model.currentActivityId treeModel)

                                Nothing ->
                                    div [ class "has-text-centered" ]
                                        [ text "Initializing tree visualization..." ]
                            ]

                    ( _, _, Nothing ) ->
                        div [ class "has-text-centered" ]
                            [ text "No data to display" ]
                ]
            ]
        ]


viewGraphPage : Model -> Html Msg
viewGraphPage model =
    let
        activityInfo =
            -- Try to get from graph data first
            Dict.get model.currentActivityId model.cachedGraphs
                |> Maybe.andThen
                    (\graphData ->
                        List.filter (\node -> node.processId == model.currentActivityId) graphData.nodes
                            |> List.head
                            |> Maybe.map (\node -> ( node.label, node.location ))
                    )
                |> -- Fallback to tree data if graph data doesn't have it
                   (\maybeInfo ->
                        case maybeInfo of
                            Just info ->
                                Just info

                            Nothing ->
                                Dict.get model.currentActivityId model.cachedTrees
                                    |> Maybe.andThen (\tree -> Dict.get model.currentActivityId tree.nodes)
                                    |> Maybe.map (\node -> ( node.name, node.location ))
                   )
    in
    div [ class "graph-page" ]
        [ viewPageNavbar "Activity Network Graph" activityInfo
        , -- Controls
          div [ style "height" "90px" ]
            [ div []
                [ div [ class "level" ]
                    [ div [ class "level-left" ]
                        [ div [ class "level-item" ]
                            [ Html.form
                                [ class "field has-addons"
                                , onSubmit (LoadGraph model.currentActivityId)
                                ]
                                [ div [ class "control" ]
                                    [ Html.label [ class "label" ] [ text "Cutoff (%)" ]
                                    , input
                                        [ class "input"
                                        , type_ "text"
                                        , value model.graphCutoffInput
                                        , onInput UpdateGraphCutoff
                                        , Html.Attributes.placeholder "e.g., 0.1, 1.0, 5.0"
                                        ]
                                        []
                                    ]
                                , div [ class "control" ]
                                    [ button
                                        [ class "button is-primary"
                                        , type_ "submit"
                                        ]
                                        [ text "Reload Graph" ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        , -- Main graph content
          div []
            [ div []
                [ case ( model.loading, model.error, model.graphViewModel ) of
                    ( True, _, _ ) ->
                        div [ class "has-text-centered" ]
                            [ div [ class "is-size-3" ] [ text "Loading graph..." ]
                            , progress [ class "progress is-primary", attribute "max" "100" ] []
                            ]

                    ( _, Just error, _ ) ->
                        div [ class "notification is-danger" ]
                            [ button [ class "delete", onClick (LoadGraph model.currentActivityId) ] []
                            , strong [] [ text "Error: " ]
                            , text error
                            ]

                    ( _, _, Just graphModel ) ->
                        Html.map GraphViewMsg (GraphView.view model.currentActivityId graphModel)

                    ( _, _, Nothing ) ->
                        div [ class "has-text-centered" ]
                            [ text "No graph data available" ]
                ]
            ]
        ]


canNavigateToParent : Model -> Bool
canNavigateToParent model =
    -- First check navigation history (works even before tree is loaded)
    if not (List.isEmpty model.navigationHistory) then
        True
    else
        -- Otherwise check if current node has a parentId
        case Dict.get model.currentActivityId model.cachedTrees of
            Just tree ->
                case Dict.get model.currentActivityId tree.nodes of
                    Just currentNode ->
                        currentNode.parentId /= Nothing

                    Nothing ->
                        False

            Nothing ->
                False


loadActivityTree : String -> Cmd Msg
loadActivityTree activityId =
    Http.get
        { url = "/api/v1/activity/" ++ activityId ++ "/tree"
        , expect = Http.expectJson ActivityLoaded activityTreeDecoder
        }


loadActivityInfo : String -> Cmd Msg
loadActivityInfo activityId =
    Http.get
        { url = "/api/v1/activity/" ++ activityId
        , expect = Http.expectJson ActivityInfoLoaded activityInfoDecoder
        }


loadInventoryData : String -> Cmd Msg
loadInventoryData activityId =
    Http.get
        { url = "/api/v1/activity/" ++ activityId ++ "/inventory"
        , expect = Http.expectJson InventoryLoaded inventoryExportDecoder
        }


loadGraphData : String -> Float -> Cmd Msg
loadGraphData activityId cutoff =
    Http.get
        { url = "/api/v1/activity/" ++ activityId ++ "/graph?cutoff=" ++ String.fromFloat cutoff
        , expect = Http.expectJson GraphLoaded graphDataDecoder
        }


searchActivities : String -> Cmd Msg
searchActivities query =
    Http.get
        { url =
            Url.Builder.absolute
                [ "api", "v1", "search", "activities" ]
                [ Url.Builder.string "name" query
                , Url.Builder.int "limit" 20
                ]
        , expect = Http.expectJson ActivitiesSearchResults (searchResultsDecoder activitySummaryDecoder)
        }


searchActivitiesWithOffset : String -> Int -> Int -> Cmd Msg
searchActivitiesWithOffset query offset limit =
    Http.get
        { url =
            Url.Builder.absolute
                [ "api", "v1", "search", "activities" ]
                [ Url.Builder.string "name" query
                , Url.Builder.int "limit" limit
                , Url.Builder.int "offset" offset
                ]
        , expect = Http.expectJson MoreActivitiesLoaded (searchResultsDecoder activitySummaryDecoder)
        }


loadMethods : Cmd Msg
loadMethods =
    Http.get
        { url = "/api/v1/methods"
        , expect = Http.expectJson MethodsLoaded methodsListDecoder
        }


computeLCIA : String -> String -> Cmd Msg
computeLCIA activityId methodId =
    Http.get
        { url = "/api/v1/activity/" ++ activityId ++ "/lcia/" ++ methodId
        , expect = Http.expectJson LCIAResultLoaded lciaResultDecoder
        }


loadMappingStatus : String -> Cmd Msg
loadMappingStatus methodId =
    Http.get
        { url = "/api/v1/method/" ++ methodId ++ "/mapping"
        , expect = Http.expectJson MappingStatusLoaded mappingStatusDecoder
        }


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "Bad URL: " ++ url

        Http.Timeout ->
            "Request timed out"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus status ->
            "Bad status: " ++ String.fromInt status

        Http.BadBody body ->
            "Bad body: " ++ body


{-| Shared navbar component for all pages
-}
viewPageNavbar : String -> Maybe ( String, String ) -> Html Msg
viewPageNavbar title maybeActivity =
    nav [ class "navbar is-light", style "height" "52px" ]
        [ div [ class "navbar-brand" ]
            [ div [ class "navbar-item" ]
                [ h1 [ class "title is-4" ] [ text title ]
                ]
            ]
        , div [ class "navbar-menu is-active" ]
            [ div [ class "navbar-end" ]
                (case maybeActivity of
                    Just ( name, location ) ->
                        [ div [ class "navbar-item" ]
                            [ span [ class "title is-4" ] [ text name ]
                            ]
                        , div [ class "navbar-item" ]
                            [ span [ class "subtitle is-6" ] [ text location ]
                            ]
                        ]

                    Nothing ->
                        [ div [ class "navbar-item" ]
                            [ span [ class "subtitle is-6" ] [ text "Loading..." ]
                            ]
                        ]
                )
            ]
        ]
