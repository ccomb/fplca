module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode
import Json.Encode
import Models.Activity exposing (ActivityInfo, ActivitySummary, ActivityTree, SearchResults, activityInfoDecoder, activitySummaryDecoder, activityTreeDecoder, searchResultsDecoder)
import Models.Database exposing (ActivateResponse, DatabaseList, UploadResponse, activateResponseDecoder, databaseListDecoder, uploadResponseDecoder)
import Models.Graph exposing (GraphData, graphDataDecoder)
import Models.Inventory exposing (InventoryExport, inventoryExportDecoder)
import Models.LCIA exposing (LCIAResult, MappingStatus, MethodSummary, lciaResultDecoder, mappingStatusDecoder, methodsListDecoder)
import Models.Page exposing (Page(..), Route(..))
import Url
import Url.Builder
import Url.Parser as Parser exposing ((</>), (<?>), Parser, oneOf, parse, string, top)
import Url.Parser.Query as Query
import Views.ActivitiesView as ActivitiesView
import Views.DatabasesView as DatabasesView
import Views.DetailsView as DetailsView
import Views.GraphView as GraphView
import Views.InventoryView as InventoryView
import Views.LCIAView as LCIAView
import Views.LeftMenu as LeftMenu
import Views.TreeView as TreeView
import Views.UploadView as UploadView


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
    , cachedTrees : Dict.Dict String ActivityTree -- Cache trees by activity ID (for tree tab)
    , cachedActivityInfo : Dict.Dict String ActivityInfo -- Cache activity info by activity ID (for table tab)
    , cachedInventories : Dict.Dict String InventoryExport -- Cache inventories by activity ID
    , cachedGraphs : Dict.Dict String GraphData -- Cache graphs by activity ID
    , graphViewModel : Maybe GraphView.Model -- Current graph view model
    , treeViewModel : Maybe TreeView.Model -- Current tree view model
    , detailsViewModel : Maybe DetailsView.Model -- Current details view model
    , graphCutoffInput : String -- Cutoff input as string to allow partial edits like "0."
    , currentActivityId : String
    , currentDatabaseId : Maybe String -- Database ID from URL (for URL-based navigation)
    , loading : Bool
    , error : Maybe String
    , navigationHistory : List String
    , activitiesSearchQuery : String
    , searchResults : Maybe (SearchResults ActivitySummary)
    , searchLoading : Bool
    , loadingMore : Bool
    , hoveredNode : Maybe String
    , inventorySearchQuery : String
    , methods : Maybe (List MethodSummary) -- Available LCIA methods
    , selectedMethod : Maybe MethodSummary -- Currently selected method
    , lciaResult : Maybe LCIAResult -- LCIA computation result
    , mappingStatus : Maybe MappingStatus -- Flow mapping status
    , loadingMethods : Bool -- Loading methods list
    , loadingLCIA : Bool -- Loading LCIA computation
    , databaseList : Maybe DatabaseList -- Available databases
    , loadingDatabases : Bool -- Loading database list
    , activatingDatabase : Bool -- Activating a database
    , uploadModel : UploadView.Model -- Upload page model
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
      -- Database messages
    | LoadDatabases
    | DatabasesLoaded (Result Http.Error DatabaseList)
    | DatabasesViewMsg DatabasesView.Msg
    | ActivateDatabaseResult (Result Http.Error ActivateResponse)
      -- Upload messages
    | UploadViewMsg UploadView.Msg
    | UploadDatabaseResult (Result Http.Error UploadResponse)



-- URL parsing


activitiesQueryParser : Query.Parser { name : Maybe String, limit : Maybe Int }
activitiesQueryParser =
    Query.map2 (\name limit -> { name = name, limit = limit })
        (Query.string "name")
        (Query.int "limit")


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ -- Root route - will redirect to default database
          Parser.map RootRoute top
          -- Global routes (no database prefix)
        , Parser.map UploadRoute (Parser.s "databases" </> Parser.s "upload")
        , Parser.map DatabasesRoute (Parser.s "databases")
          -- Database-scoped routes: /db/{dbName}/...
        , Parser.map (\db query -> ActivitiesRoute { db = db, name = query.name, limit = query.limit })
            (Parser.s "db" </> string </> Parser.s "activities" <?> activitiesQueryParser)
        , Parser.map ActivityUpstreamRoute (Parser.s "db" </> string </> Parser.s "activity" </> string </> Parser.s "upstream")
        , Parser.map ActivityEmissionsRoute (Parser.s "db" </> string </> Parser.s "activity" </> string </> Parser.s "emissions")
        , Parser.map ActivityResourcesRoute (Parser.s "db" </> string </> Parser.s "activity" </> string </> Parser.s "resources")
        , Parser.map ActivityProductsRoute (Parser.s "db" </> string </> Parser.s "activity" </> string </> Parser.s "products")
        , Parser.map ActivityTreeRoute (Parser.s "db" </> string </> Parser.s "activity" </> string </> Parser.s "tree")
        , Parser.map ActivityInventoryRoute (Parser.s "db" </> string </> Parser.s "activity" </> string </> Parser.s "inventory")
        , Parser.map ActivityGraphRoute (Parser.s "db" </> string </> Parser.s "activity" </> string </> Parser.s "graph")
        , Parser.map ActivityLCIARoute (Parser.s "db" </> string </> Parser.s "activity" </> string </> Parser.s "lcia")
        , Parser.map ActivityRoute (Parser.s "db" </> string </> Parser.s "activity" </> string)
        ]


parseUrl : Url.Url -> Route
parseUrl url =
    Parser.parse routeParser url
        |> Maybe.withDefault NotFoundRoute


routeToPage : Route -> Page
routeToPage route =
    case route of
        RootRoute ->
            ActivitiesPage

        ActivitiesRoute _ ->
            ActivitiesPage

        ActivityRoute _ _ ->
            UpstreamPage

        ActivityUpstreamRoute _ _ ->
            UpstreamPage

        ActivityEmissionsRoute _ _ ->
            EmissionsPage

        ActivityResourcesRoute _ _ ->
            ResourcesPage

        ActivityProductsRoute _ _ ->
            ProductsPage

        ActivityTreeRoute _ _ ->
            TreePage

        ActivityInventoryRoute _ _ ->
            InventoryPage

        ActivityGraphRoute _ _ ->
            GraphPage

        ActivityLCIARoute _ _ ->
            LCIAPage

        DatabasesRoute ->
            DatabasesPage

        UploadRoute ->
            UploadPage

        NotFoundRoute ->
            ActivitiesPage


{-| Extract database name from route (if applicable)
-}
routeToDatabase : Route -> Maybe String
routeToDatabase route =
    case route of
        RootRoute ->
            Nothing

        ActivitiesRoute { db } ->
            Just db

        ActivityRoute db _ ->
            Just db

        ActivityUpstreamRoute db _ ->
            Just db

        ActivityEmissionsRoute db _ ->
            Just db

        ActivityResourcesRoute db _ ->
            Just db

        ActivityProductsRoute db _ ->
            Just db

        ActivityTreeRoute db _ ->
            Just db

        ActivityInventoryRoute db _ ->
            Just db

        ActivityGraphRoute db _ ->
            Just db

        ActivityLCIARoute db _ ->
            Just db

        DatabasesRoute ->
            Nothing

        UploadRoute ->
            Nothing

        NotFoundRoute ->
            Nothing


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        route =
            parseUrl url

        defaultActivityId =
            ""

        -- Extract database from route
        urlDatabase =
            routeToDatabase route

        routeConfig =
            case route of
                RootRoute ->
                    { activityId = defaultActivityId
                    , shouldLoad = True
                    , loadType = "redirect"
                    , searchQuery = ""
                    }

                ActivitiesRoute { name } ->
                    { activityId = defaultActivityId
                    , shouldLoad = False
                    , loadType = "none"
                    , searchQuery = Maybe.withDefault "" name
                    }

                ActivityRoute _ processId ->
                    { activityId = processId
                    , shouldLoad = True
                    , loadType = "upstream"
                    , searchQuery = ""
                    }

                ActivityUpstreamRoute _ processId ->
                    { activityId = processId
                    , shouldLoad = True
                    , loadType = "upstream"
                    , searchQuery = ""
                    }

                ActivityEmissionsRoute _ processId ->
                    { activityId = processId
                    , shouldLoad = True
                    , loadType = "emissions"
                    , searchQuery = ""
                    }

                ActivityResourcesRoute _ processId ->
                    { activityId = processId
                    , shouldLoad = True
                    , loadType = "resources"
                    , searchQuery = ""
                    }

                ActivityProductsRoute _ processId ->
                    { activityId = processId
                    , shouldLoad = True
                    , loadType = "products"
                    , searchQuery = ""
                    }

                ActivityTreeRoute _ processId ->
                    { activityId = processId
                    , shouldLoad = True
                    , loadType = "tree"
                    , searchQuery = ""
                    }

                ActivityInventoryRoute _ processId ->
                    { activityId = processId
                    , shouldLoad = True
                    , loadType = "inventory"
                    , searchQuery = ""
                    }

                ActivityGraphRoute _ processId ->
                    { activityId = processId
                    , shouldLoad = True
                    , loadType = "graph"
                    , searchQuery = ""
                    }

                ActivityLCIARoute _ processId ->
                    { activityId = processId
                    , shouldLoad = True
                    , loadType = "lcia"
                    , searchQuery = ""
                    }

                DatabasesRoute ->
                    { activityId = defaultActivityId
                    , shouldLoad = True
                    , loadType = "databases"
                    , searchQuery = ""
                    }

                UploadRoute ->
                    { activityId = defaultActivityId
                    , shouldLoad = False
                    , loadType = "none"
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
            , cachedTrees = Dict.empty
            , cachedActivityInfo = Dict.empty
            , cachedInventories = Dict.empty
            , cachedGraphs = Dict.empty
            , graphViewModel = Nothing
            , treeViewModel = Nothing
            , detailsViewModel = Nothing
            , graphCutoffInput = "1.0"
            , currentActivityId = routeConfig.activityId
            , currentDatabaseId = urlDatabase
            , loading = routeConfig.shouldLoad
            , error = Nothing
            , navigationHistory = []
            , activitiesSearchQuery = routeConfig.searchQuery
            , searchResults = Nothing
            , searchLoading = shouldSearch
            , loadingMore = False
            , hoveredNode = Nothing
            , inventorySearchQuery = ""
            , methods = Nothing
            , selectedMethod = Nothing
            , lciaResult = Nothing
            , mappingStatus = Nothing
            , loadingMethods = routeConfig.loadType == "lcia"
            , loadingLCIA = False
            , databaseList = Nothing
            , loadingDatabases = routeConfig.loadType == "databases" || routeConfig.loadType == "redirect"
            , activatingDatabase = False
            , uploadModel = UploadView.init
            }

        cmd =
            if routeConfig.shouldLoad then
                case routeConfig.loadType of
                    "upstream" ->
                        loadActivityInfo routeConfig.activityId

                    "emissions" ->
                        loadActivityInfo routeConfig.activityId

                    "resources" ->
                        loadActivityInfo routeConfig.activityId

                    "products" ->
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

                    "databases" ->
                        loadDatabases

                    _ ->
                        Cmd.none

            else if shouldSearch then
                searchActivities routeConfig.searchQuery

            else
                Cmd.none

        -- Always load database list on startup for the header display
        finalCmd =
            Cmd.batch [ cmd, loadDatabases ]
    in
    ( model, finalCmd )


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
                                        let
                                            db =
                                                getCurrentDbName model
                                        in
                                        ( { model
                                            | treeViewModel = Just updatedTreeModel
                                            , navigationHistory = model.currentActivityId :: model.navigationHistory
                                          }
                                        , Nav.pushUrl model.key (routeToUrl (ActivityTreeRoute db processId))
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
                    let
                        db =
                            getCurrentDbName model
                    in
                    ( { model
                        | navigationHistory = model.currentActivityId :: model.navigationHistory
                      }
                    , Nav.pushUrl model.key (routeToUrl (ActivityUpstreamRoute db processId))
                    )

                DetailsView.NavigateBack ->
                    case model.navigationHistory of
                        parentId :: rest ->
                            let
                                db =
                                    getCurrentDbName model
                            in
                            ( { model
                                | navigationHistory = rest
                              }
                            , Nav.pushUrl model.key (routeToUrl (ActivityUpstreamRoute db parentId))
                            )

                        [] ->
                            -- No history = came from search, go back to search
                            let
                                db =
                                    getCurrentDbName model

                                queryName =
                                    if String.isEmpty model.activitiesSearchQuery then
                                        Nothing

                                    else
                                        Just model.activitiesSearchQuery
                            in
                            ( model
                            , Nav.pushUrl model.key (routeToUrl (ActivitiesRoute { db = db, name = queryName, limit = Just 20 }))
                            )

        UpdateGraphCutoff cutoffStr ->
            -- Allow any string input, validation happens when loading
            ( { model | graphCutoffInput = cutoffStr }, Cmd.none )

        NodeClicked nodeId ->
            let
                db =
                    getCurrentDbName model
            in
            if nodeId /= model.currentActivityId then
                ( { model
                    | navigationHistory = model.currentActivityId :: model.navigationHistory
                  }
                , Nav.pushUrl model.key (routeToUrl (ActivityUpstreamRoute db nodeId))
                )

            else
                ( model, Cmd.none )

        NavigateToParent ->
            let
                db =
                    getCurrentDbName model
            in
            case Dict.get model.currentActivityId model.cachedTrees of
                Just tree ->
                    case Dict.get model.currentActivityId tree.nodes of
                        Just currentNode ->
                            case currentNode.parentId of
                                Just parentId ->
                                    ( { model
                                        | navigationHistory = List.drop 1 model.navigationHistory
                                      }
                                    , Nav.pushUrl model.key (routeToUrl (ActivityTreeRoute db parentId))
                                    )

                                Nothing ->
                                    case model.navigationHistory of
                                        parentId :: rest ->
                                            ( { model
                                                | navigationHistory = rest
                                              }
                                            , Nav.pushUrl model.key (routeToUrl (ActivityTreeRoute db parentId))
                                            )

                                        [] ->
                                            ( model, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        NavigateToPage page ->
            let
                maybeDb =
                    case model.currentDatabaseId of
                        Just db ->
                            Just db

                        Nothing ->
                            model.databaseList |> Maybe.andThen .current

                -- Pages that don't require a database
                route =
                    case page of
                        DatabasesPage ->
                            Just DatabasesRoute

                        UploadPage ->
                            Just UploadRoute

                        -- All other pages require a database
                        _ ->
                            case maybeDb of
                                Just db ->
                                    case page of
                                        ActivitiesPage ->
                                            let
                                                queryName =
                                                    if String.isEmpty model.activitiesSearchQuery then
                                                        Nothing
                                                    else
                                                        Just model.activitiesSearchQuery
                                            in
                                            Just (ActivitiesRoute { db = db, name = queryName, limit = Just 20 })

                                        UpstreamPage ->
                                            Just (ActivityUpstreamRoute db model.currentActivityId)

                                        EmissionsPage ->
                                            Just (ActivityEmissionsRoute db model.currentActivityId)

                                        ResourcesPage ->
                                            Just (ActivityResourcesRoute db model.currentActivityId)

                                        ProductsPage ->
                                            Just (ActivityProductsRoute db model.currentActivityId)

                                        TreePage ->
                                            Just (ActivityTreeRoute db model.currentActivityId)

                                        InventoryPage ->
                                            Just (ActivityInventoryRoute db model.currentActivityId)

                                        GraphPage ->
                                            Just (ActivityGraphRoute db model.currentActivityId)

                                        LCIAPage ->
                                            Just (ActivityLCIARoute db model.currentActivityId)

                                        _ ->
                                            Nothing

                                Nothing ->
                                    -- No database loaded, go to databases page
                                    Just DatabasesRoute
            in
            case route of
                Just r ->
                    ( model, Nav.pushUrl model.key (routeToUrl r) )

                Nothing ->
                    ( model, Cmd.none )

        UpdateSearchQuery query ->
            let
                db =
                    getCurrentDbName model

                queryName =
                    if String.isEmpty query then
                        Nothing

                    else
                        Just query

                newRoute =
                    ActivitiesRoute { db = db, name = queryName, limit = Just 20 }

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
                        }

                    else
                        { model
                            | activitiesSearchQuery = query
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
            let
                db =
                    getCurrentDbName model
            in
            ( model  -- Don't modify history - stays empty when coming from search
            , navigateToActivity model.key db activityId
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

        -- Database message handlers
        LoadDatabases ->
            ( { model | loadingDatabases = True, error = Nothing }
            , loadDatabases
            )

        DatabasesLoaded (Ok dbList) ->
            let
                -- Check if we need to redirect from root to default database
                needsRootRedirect =
                    model.currentDatabaseId == Nothing && model.currentPage == ActivitiesPage

                redirectCmd =
                    if needsRootRedirect then
                        case dbList.current of
                            Just defaultDb ->
                                Nav.replaceUrl model.key (routeToUrl (ActivitiesRoute { db = defaultDb, name = Nothing, limit = Just 20 }))

                            Nothing ->
                                Cmd.none

                    else
                        Cmd.none
            in
            ( { model
                | databaseList = Just dbList
                , loadingDatabases = False
                , error = Nothing
                , currentDatabaseId =
                    if model.currentDatabaseId == Nothing then
                        dbList.current

                    else
                        model.currentDatabaseId
              }
            , redirectCmd
            )

        DatabasesLoaded (Err error) ->
            ( { model
                | loadingDatabases = False
                , error = Just (httpErrorToString error)
              }
            , Cmd.none
            )

        DatabasesViewMsg dbMsg ->
            case dbMsg of
                DatabasesView.ActivateDatabase dbName ->
                    ( { model | activatingDatabase = True, error = Nothing }
                    , activateDatabase dbName
                    )

                DatabasesView.LoadDatabase dbName ->
                    ( { model | activatingDatabase = True, error = Nothing }
                    , loadDatabaseCmd dbName
                    )

                DatabasesView.UnloadDatabase dbName ->
                    ( { model | activatingDatabase = True, error = Nothing }
                    , unloadDatabaseCmd dbName
                    )

                DatabasesView.DeleteDatabase dbName ->
                    ( { model | activatingDatabase = True, error = Nothing }
                    , deleteDatabaseCmd dbName
                    )

        ActivateDatabaseResult (Ok response) ->
            if response.success then
                let
                    -- Get the new database name from the response
                    newDbName =
                        response.database
                            |> Maybe.map .name
                            |> Maybe.withDefault (getCurrentDbName model)

                    -- Re-run search if on Activities page with a query
                    searchCmd =
                        if model.currentPage == ActivitiesPage && not (String.isEmpty model.activitiesSearchQuery) then
                            searchActivities model.activitiesSearchQuery

                        else
                            Cmd.none

                    -- Update URL with new database
                    urlCmd =
                        case model.currentPage of
                            ActivitiesPage ->
                                let
                                    queryName =
                                        if String.isEmpty model.activitiesSearchQuery then
                                            Nothing

                                        else
                                            Just model.activitiesSearchQuery
                                in
                                Nav.replaceUrl model.key (routeToUrl (ActivitiesRoute { db = newDbName, name = queryName, limit = Just 20 }))

                            UpstreamPage ->
                                Nav.replaceUrl model.key (routeToUrl (ActivityUpstreamRoute newDbName model.currentActivityId))

                            EmissionsPage ->
                                Nav.replaceUrl model.key (routeToUrl (ActivityEmissionsRoute newDbName model.currentActivityId))

                            ResourcesPage ->
                                Nav.replaceUrl model.key (routeToUrl (ActivityResourcesRoute newDbName model.currentActivityId))

                            ProductsPage ->
                                Nav.replaceUrl model.key (routeToUrl (ActivityProductsRoute newDbName model.currentActivityId))

                            TreePage ->
                                Nav.replaceUrl model.key (routeToUrl (ActivityTreeRoute newDbName model.currentActivityId))

                            InventoryPage ->
                                Nav.replaceUrl model.key (routeToUrl (ActivityInventoryRoute newDbName model.currentActivityId))

                            GraphPage ->
                                Nav.replaceUrl model.key (routeToUrl (ActivityGraphRoute newDbName model.currentActivityId))

                            LCIAPage ->
                                Nav.replaceUrl model.key (routeToUrl (ActivityLCIARoute newDbName model.currentActivityId))

                            DatabasesPage ->
                                Cmd.none

                            UploadPage ->
                                Cmd.none
                    -- Set searchLoading if we're re-running search
                    isSearching =
                        model.currentPage == ActivitiesPage && not (String.isEmpty model.activitiesSearchQuery)
                in
                -- Reload database list to show updated status
                ( { model
                    | activatingDatabase = False
                    , error = Nothing
                    , currentDatabaseId = Just newDbName
                    -- Clear cached data since we switched databases
                    , cachedTrees = Dict.empty
                    , cachedActivityInfo = Dict.empty
                    , cachedInventories = Dict.empty
                    , cachedGraphs = Dict.empty
                    , searchResults = Nothing
                    , searchLoading = isSearching
                  }
                , Cmd.batch [ loadDatabases, urlCmd, searchCmd ]
                )

            else
                ( { model
                    | activatingDatabase = False
                    , error = Just response.message
                  }
                , Cmd.none
                )

        ActivateDatabaseResult (Err error) ->
            ( { model
                | activatingDatabase = False
                , error = Just (httpErrorToString error)
              }
            , Cmd.none
            )

        UploadViewMsg uploadMsg ->
            let
                ( newUploadModel, uploadCmd ) =
                    UploadView.update uploadMsg model.uploadModel
            in
            -- Handle UploadDatabase by triggering API call
            case uploadMsg of
                UploadView.UploadDatabase ->
                    case model.uploadModel.fileContent of
                        Just content ->
                            ( { model | uploadModel = newUploadModel }
                            , uploadDatabase model.uploadModel.name model.uploadModel.description content
                            )

                        Nothing ->
                            ( { model | uploadModel = newUploadModel }
                            , Cmd.none
                            )

                _ ->
                    ( { model | uploadModel = newUploadModel }
                    , Cmd.map UploadViewMsg uploadCmd
                    )

        UploadDatabaseResult (Ok response) ->
            let
                currentUpload =
                    model.uploadModel
            in
            if response.success then
                -- Upload succeeded - redirect to databases page
                ( { model
                    | uploadModel = { currentUpload | uploading = False, success = Just response.message }
                  }
                , Cmd.batch
                    [ loadDatabases
                    , Nav.pushUrl model.key (routeToUrl DatabasesRoute)
                    ]
                )

            else
                ( { model
                    | uploadModel = { currentUpload | uploading = False, error = Just response.message }
                  }
                , Cmd.none
                )

        UploadDatabaseResult (Err error) ->
            let
                currentUpload =
                    model.uploadModel
            in
            ( { model
                | uploadModel = { currentUpload | uploading = False, error = Just (httpErrorToString error) }
              }
            , Cmd.none
            )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            let
                route =
                    parseUrl url

                newPage =
                    routeToPage route

                urlDatabase =
                    routeToDatabase route

                -- Check if this is just a search query update on the same page
                -- If so, skip processing to avoid focus loss on input field
                urlSearchQuery =
                    case route of
                        ActivitiesRoute { name } ->
                            Maybe.withDefault "" name

                        _ ->
                            ""

                isSamePageSameQuery =
                    newPage == model.currentPage
                        && newPage == ActivitiesPage
                        && urlSearchQuery == model.activitiesSearchQuery
            in
            if isSamePageSameQuery then
                ( { model | url = url }, Cmd.none )

            else
                let
                    routeInfo =
                        case route of
                            RootRoute ->
                                { activityId = model.currentActivityId, needsActivity = False, searchQuery = "", needsRedirect = True }

                            ActivitiesRoute { name } ->
                                { activityId = model.currentActivityId, needsActivity = False, searchQuery = Maybe.withDefault "" name, needsRedirect = False }

                            ActivityRoute _ processId ->
                                { activityId = processId, needsActivity = True, searchQuery = model.activitiesSearchQuery, needsRedirect = False }

                            ActivityUpstreamRoute _ processId ->
                                { activityId = processId, needsActivity = True, searchQuery = model.activitiesSearchQuery, needsRedirect = False }

                            ActivityEmissionsRoute _ processId ->
                                { activityId = processId, needsActivity = True, searchQuery = model.activitiesSearchQuery, needsRedirect = False }

                            ActivityResourcesRoute _ processId ->
                                { activityId = processId, needsActivity = True, searchQuery = model.activitiesSearchQuery, needsRedirect = False }

                            ActivityProductsRoute _ processId ->
                                { activityId = processId, needsActivity = True, searchQuery = model.activitiesSearchQuery, needsRedirect = False }

                            ActivityTreeRoute _ processId ->
                                { activityId = processId, needsActivity = True, searchQuery = model.activitiesSearchQuery, needsRedirect = False }

                            ActivityInventoryRoute _ processId ->
                                { activityId = processId, needsActivity = True, searchQuery = model.activitiesSearchQuery, needsRedirect = False }

                            ActivityGraphRoute _ processId ->
                                { activityId = processId, needsActivity = True, searchQuery = model.activitiesSearchQuery, needsRedirect = False }

                            ActivityLCIARoute _ processId ->
                                { activityId = processId, needsActivity = True, searchQuery = model.activitiesSearchQuery, needsRedirect = False }

                            DatabasesRoute ->
                                { activityId = model.currentActivityId, needsActivity = False, searchQuery = model.activitiesSearchQuery, needsRedirect = False }

                            UploadRoute ->
                                { activityId = model.currentActivityId, needsActivity = False, searchQuery = model.activitiesSearchQuery, needsRedirect = False }

                            NotFoundRoute ->
                                { activityId = model.currentActivityId, needsActivity = False, searchQuery = model.activitiesSearchQuery, needsRedirect = False }

                    -- For activity exchange pages: load activity info
                    shouldLoadActivityInfo =
                        routeInfo.needsActivity
                            && (newPage == UpstreamPage || newPage == EmissionsPage || newPage == ResourcesPage || newPage == ProductsPage)
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

                    shouldLoadDatabases =
                        newPage == DatabasesPage && model.databaseList == Nothing

                    shouldLoad =
                        shouldLoadActivityInfo || shouldLoadTree || shouldLoadInventory || shouldLoadGraph || shouldLoadLCIA || shouldLoadDatabases

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

                    -- If root route, we need to redirect to default database
                    needsRedirect =
                        routeInfo.needsRedirect

                    -- Check if URL database differs from current server database
                    needsDbActivation =
                        case ( urlDatabase, model.databaseList ) of
                            ( Just urlDb, Just dbList ) ->
                                case dbList.current of
                                    Just currentDb ->
                                        urlDb /= currentDb

                                    Nothing ->
                                        True

                            _ ->
                                False

                    updatedModel =
                        { model
                            | url = url
                            , currentPage = newPage
                            , currentActivityId = routeInfo.activityId
                            , currentDatabaseId = urlDatabase
                            , loading = shouldLoad || needsRedirect
                            , navigationHistory = model.navigationHistory
                            , activitiesSearchQuery = routeInfo.searchQuery
                            , searchLoading = shouldSearch
                            , treeViewModel = newTreeViewModel
                            , loadingMethods = shouldLoadLCIA
                            , loadingDatabases = shouldLoadDatabases || needsRedirect
                            , activatingDatabase = needsDbActivation
                        }

                    cmd =
                        if needsRedirect then
                            -- Load databases to get default, will redirect after
                            loadDatabases

                        else if needsDbActivation then
                            -- Activate the database from URL
                            case urlDatabase of
                                Just dbName ->
                                    activateDatabase dbName

                                Nothing ->
                                    Cmd.none

                        else if shouldLoadActivityInfo then
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

                        else if shouldLoadDatabases then
                            loadDatabases

                        else
                            Cmd.none
                in
                ( updatedModel, cmd )



-- Navigation helpers


routeToUrl : Route -> String
routeToUrl route =
    case route of
        RootRoute ->
            "/"

        ActivitiesRoute { db, name, limit } ->
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
            "/db/" ++ db ++ "/activities" ++ queryString

        ActivityRoute db processId ->
            "/db/" ++ db ++ "/activity/" ++ processId

        ActivityUpstreamRoute db processId ->
            "/db/" ++ db ++ "/activity/" ++ processId ++ "/upstream"

        ActivityEmissionsRoute db processId ->
            "/db/" ++ db ++ "/activity/" ++ processId ++ "/emissions"

        ActivityResourcesRoute db processId ->
            "/db/" ++ db ++ "/activity/" ++ processId ++ "/resources"

        ActivityProductsRoute db processId ->
            "/db/" ++ db ++ "/activity/" ++ processId ++ "/products"

        ActivityTreeRoute db processId ->
            "/db/" ++ db ++ "/activity/" ++ processId ++ "/tree"

        ActivityInventoryRoute db processId ->
            "/db/" ++ db ++ "/activity/" ++ processId ++ "/inventory"

        ActivityGraphRoute db processId ->
            "/db/" ++ db ++ "/activity/" ++ processId ++ "/graph"

        ActivityLCIARoute db processId ->
            "/db/" ++ db ++ "/activity/" ++ processId ++ "/lcia"

        DatabasesRoute ->
            "/databases"

        UploadRoute ->
            "/databases/upload"

        NotFoundRoute ->
            "/"


{-| Get current database name from model (URL database or server's current)
-}
getCurrentDbName : Model -> String
getCurrentDbName model =
    model.currentDatabaseId
        |> Maybe.withDefault
            (model.databaseList
                |> Maybe.andThen .current
                |> Maybe.withDefault "default"
            )


{-| Navigate to activity details page with current database
-}
navigateToActivity : Nav.Key -> String -> String -> Cmd Msg
navigateToActivity key db processId =
    Nav.pushUrl key (routeToUrl (ActivityUpstreamRoute db processId))


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
    let
        currentDatabaseName =
            model.databaseList
                |> Maybe.andThen
                    (\dbList ->
                        dbList.current
                            |> Maybe.andThen
                                (\currentName ->
                                    List.filter (\db -> db.name == currentName) dbList.databases
                                        |> List.head
                                        |> Maybe.map .displayName
                                )
                    )

        -- Get activity name from cached data (check both cachedTrees and cachedActivityInfo)
        currentActivityName =
            case Dict.get model.currentActivityId model.cachedTrees of
                Just tree ->
                    Dict.get model.currentActivityId tree.nodes
                        |> Maybe.map .name

                Nothing ->
                    Dict.get model.currentActivityId model.cachedActivityInfo
                        |> Maybe.map .name
    in
    div [ class "app-container" ]
        [ Html.map
            (\msg ->
                case msg of
                    LeftMenu.NavigateTo page ->
                        NavigateToPage page
            )
            (LeftMenu.viewLeftMenu model.currentPage model.currentActivityId currentDatabaseName currentActivityName)
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

                                ActivitiesView.ActivateDatabase dbName ->
                                    DatabasesViewMsg (DatabasesView.ActivateDatabase dbName)
                        )
                        (ActivitiesView.viewActivitiesPage
                            model.activitiesSearchQuery
                            model.searchResults
                            model.searchLoading
                            model.loadingMore
                            model.error
                            model.databaseList
                        )

                UpstreamPage ->
                    viewUpstreamPage model

                EmissionsPage ->
                    viewEmissionsPage model

                ResourcesPage ->
                    viewResourcesPage model

                ProductsPage ->
                    viewProductsPage model

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

                DatabasesPage ->
                    Html.map DatabasesViewMsg
                        (DatabasesView.viewDatabasesPage
                            model.databaseList
                            (model.loadingDatabases || model.activatingDatabase)
                            model.error
                        )

                UploadPage ->
                    Html.map UploadViewMsg (UploadView.view model.uploadModel)
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


viewUpstreamPage : Model -> Html Msg
viewUpstreamPage model =
    viewExchangePage model
        "Upstream Activities"
        (\activityInfo ->
            let
                exchanges =
                    List.filter (\ex -> ex.exchangeType == Models.Activity.TechnosphereExchangeType && ex.isInput && not ex.isReference) activityInfo.exchanges
            in
            DetailsView.viewUpstreamExchanges exchanges (\processId -> DetailsViewMsg (DetailsView.NavigateToActivity processId))
        )


viewEmissionsPage : Model -> Html Msg
viewEmissionsPage model =
    viewExchangePage model
        "Direct Emissions"
        (\activityInfo ->
            let
                exchanges =
                    List.filter (\ex -> ex.exchangeType == Models.Activity.BiosphereEmissionType) activityInfo.exchanges
            in
            DetailsView.viewEmissionsExchanges exchanges
        )


viewResourcesPage : Model -> Html Msg
viewResourcesPage model =
    viewExchangePage model
        "Natural Resources"
        (\activityInfo ->
            let
                exchanges =
                    List.filter (\ex -> ex.exchangeType == Models.Activity.BiosphereResourceType) activityInfo.exchanges
            in
            DetailsView.viewNaturalResourcesExchanges exchanges
        )


viewProductsPage : Model -> Html Msg
viewProductsPage model =
    viewExchangePage model
        "Outgoing Products"
        (\activityInfo ->
            DetailsView.viewAllProducts activityInfo.allProducts model.currentActivityId (\processId -> DetailsViewMsg (DetailsView.NavigateToActivity processId))
        )


viewExchangePage : Model -> String -> (Models.Activity.ActivityInfo -> Html Msg) -> Html Msg
viewExchangePage model pageTitle viewContent =
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
                        div [ style "display" "flex", style "flex-direction" "column", style "height" "100%" ]
                            [ div [ style "flex-shrink" "0" ]
                                [ viewActivityHeaderWithTitle activityInfo (not (List.isEmpty model.navigationHistory)) pageTitle
                                ]
                            , div [ style "flex" "1", style "display" "flex", style "flex-direction" "column", style "min-height" "0" ]
                                [ viewContent activityInfo
                                ]
                            ]

                    Nothing ->
                        div [ class "has-text-centered" ]
                            [ text "Loading activity data..." ]
        ]


viewActivityHeaderWithDoc : Models.Activity.ActivityInfo -> Bool -> Html Msg
viewActivityHeaderWithDoc activityInfo hasHistory =
    let
        hasDescription =
            not (List.isEmpty activityInfo.description)

        backButtonText =
            if hasHistory then
                "Previous Activity"

            else
                "Search results"
    in
    div [ class "box", style "margin-bottom" "0" ]
        [ -- Title and location on same line
          div [ class "level", style "margin-bottom" "0" ]
            [ div [ class "level-left" ]
                ([ div [ class "level-item" ]
                    [ button
                        [ class "button is-primary"
                        , onClick (DetailsViewMsg DetailsView.NavigateBack)
                        ]
                        [ span [ class "icon" ]
                            [ i [ class "fas fa-arrow-left" ] []
                            ]
                        , span [] [ text backButtonText ]
                        ]
                    ]
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


viewActivityHeaderWithTitle : Models.Activity.ActivityInfo -> Bool -> String -> Html Msg
viewActivityHeaderWithTitle activityInfo hasHistory pageTitle =
    let
        hasDescription =
            not (List.isEmpty activityInfo.description)

        backButtonText =
            if hasHistory then
                "Previous Activity"

            else
                "Search results"
    in
    div [ class "box", style "margin-bottom" "0" ]
        [ -- Title and location on same line
          div [ class "level", style "margin-bottom" "0" ]
            [ div [ class "level-left" ]
                ([ div [ class "level-item" ]
                    [ button
                        [ class "button is-primary"
                        , onClick (DetailsViewMsg DetailsView.NavigateBack)
                        ]
                        [ span [ class "icon" ]
                            [ i [ class "fas fa-arrow-left" ] []
                            ]
                        , span [] [ text backButtonText ]
                        ]
                    ]
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
        , -- Page title
          h2 [ class "title is-5", style "margin-top" "1rem", style "margin-bottom" "0" ] [ text pageTitle ]
        ]


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


loadDatabases : Cmd Msg
loadDatabases =
    Http.get
        { url = "/api/v1/databases"
        , expect = Http.expectJson DatabasesLoaded databaseListDecoder
        }


activateDatabase : String -> Cmd Msg
activateDatabase dbName =
    Http.post
        { url = "/api/v1/databases/" ++ dbName ++ "/activate"
        , body = Http.emptyBody
        , expect = Http.expectJson ActivateDatabaseResult activateResponseDecoder
        }


loadDatabaseCmd : String -> Cmd Msg
loadDatabaseCmd dbName =
    Http.post
        { url = "/api/v1/databases/" ++ dbName ++ "/load"
        , body = Http.emptyBody
        , expect = Http.expectJson ActivateDatabaseResult activateResponseDecoder
        }


unloadDatabaseCmd : String -> Cmd Msg
unloadDatabaseCmd dbName =
    Http.post
        { url = "/api/v1/databases/" ++ dbName ++ "/unload"
        , body = Http.emptyBody
        , expect = Http.expectJson ActivateDatabaseResult activateResponseDecoder
        }


deleteDatabaseCmd : String -> Cmd Msg
deleteDatabaseCmd dbName =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = "/api/v1/databases/" ++ dbName
        , body = Http.emptyBody
        , expect = Http.expectJson ActivateDatabaseResult activateResponseDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


uploadDatabase : String -> String -> String -> Cmd Msg
uploadDatabase name description fileContent =
    Http.post
        { url = "/api/v1/databases/upload"
        , body =
            Http.jsonBody
                (Json.Encode.object
                    [ ( "urName", Json.Encode.string name )
                    , ( "urDescription"
                      , if String.isEmpty description then
                            Json.Encode.null

                        else
                            Json.Encode.string description
                      )
                    , ( "urFileData", Json.Encode.string fileContent )
                    ]
                )
        , expect = Http.expectJson UploadDatabaseResult uploadResponseDecoder
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
