module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Models.Activity exposing (ActivitySummary, ActivityTree, SearchResults, activitySummaryDecoder, activityTreeDecoder, searchResultsDecoder)
import Models.Graph exposing (GraphData, graphDataDecoder)
import Models.Inventory exposing (InventoryExport, inventoryExportDecoder)
import Models.Page exposing (Page(..), Route(..))
import Url
import Url.Builder
import Url.Parser as Parser exposing ((</>), Parser, oneOf, parse, string, top)
import Views.ActivitiesView as ActivitiesView
import Views.GraphView as GraphView
import Views.InventoryView as InventoryView
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
    , cachedTrees : Dict.Dict String ActivityTree -- Cache trees by activity ID
    , cachedInventories : Dict.Dict String InventoryExport -- Cache inventories by activity ID
    , cachedGraphs : Dict.Dict String GraphData -- Cache graphs by activity ID
    , graphViewModel : Maybe GraphView.Model -- Current graph view model
    , graphCutoffInput : String -- Cutoff input as string to allow partial edits like "0."
    , currentActivityId : String
    , loading : Bool
    , error : Maybe String
    , navigationHistory : List String
    , activitiesSearchQuery : String
    , searchResults : Maybe (SearchResults ActivitySummary)
    , searchLoading : Bool
    , hoveredNode : Maybe String
    , inventorySearchQuery : String
    }


type Msg
    = LoadActivity String
    | ActivityLoaded (Result Http.Error ActivityTree)
    | LoadInventory String
    | InventoryLoaded (Result Http.Error InventoryExport)
    | LoadGraph String
    | GraphLoaded (Result Http.Error GraphData)
    | GraphViewMsg GraphView.Msg
    | UpdateGraphCutoff String
    | NavigateToParent
    | NodeClicked String
    | NavigateToPage Page
    | UpdateSearchQuery String
    | SearchActivities String
    | ActivitiesSearchResults (Result Http.Error (SearchResults ActivitySummary))
    | SelectActivity String
    | NodeHovered (Maybe String)
    | UpdateInventorySearchQuery String
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url



-- URL parsing


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ Parser.map ActivitiesRoute top
        , Parser.map ActivitiesRoute (Parser.s "activities")
        , Parser.map ActivityTreeRoute (Parser.s "activity" </> string </> Parser.s "tree")
        , Parser.map ActivityInventoryRoute (Parser.s "activity" </> string </> Parser.s "inventory")
        , Parser.map ActivityGraphRoute (Parser.s "activity" </> string </> Parser.s "graph")
        , Parser.map ActivityRoute (Parser.s "activity" </> string)
        ]


parseUrl : Url.Url -> Route
parseUrl url =
    -- Use fragment instead of path for hash-based routing
    case url.fragment of
        Nothing ->
            ActivitiesRoute

        Just fragment ->
            case Parser.parse routeParser { url | path = "/" ++ fragment, fragment = Nothing } of
                Just route ->
                    route

                Nothing ->
                    NotFoundRoute


routeToPage : Route -> Page
routeToPage route =
    case route of
        ActivitiesRoute ->
            ActivitiesPage

        ActivityRoute _ ->
            TreePage

        ActivityTreeRoute _ ->
            TreePage

        ActivityInventoryRoute _ ->
            InventoryPage

        ActivityGraphRoute _ ->
            GraphPage

        NotFoundRoute ->
            ActivitiesPage


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        route =
            parseUrl url

        defaultActivityId =
            "22222222-3333-4444-5555-666666666661_chemical-b-uuid"

        ( activityId, shouldLoad, loadType ) =
            case route of
                ActivityRoute processId ->
                    ( processId, True, "tree" )

                ActivityTreeRoute processId ->
                    ( processId, True, "tree" )

                ActivityInventoryRoute processId ->
                    ( processId, True, "inventory" )

                ActivityGraphRoute processId ->
                    ( processId, True, "graph" )

                _ ->
                    ( defaultActivityId, False, "none" )

        initialPage =
            routeToPage route

        model =
            { key = key
            , url = url
            , currentPage = initialPage
            , cachedTrees = Dict.empty
            , cachedInventories = Dict.empty
            , cachedGraphs = Dict.empty
            , graphViewModel = Nothing
            , graphCutoffInput = "1.0"
            , currentActivityId = activityId
            , loading = shouldLoad
            , error = Nothing
            , navigationHistory = []
            , activitiesSearchQuery = ""
            , searchResults = Nothing
            , searchLoading = False
            , hoveredNode = Nothing
            , inventorySearchQuery = ""
            }

        cmd =
            if shouldLoad then
                case loadType of
                    "tree" ->
                        loadActivityTree activityId

                    "inventory" ->
                        loadInventoryData activityId

                    "graph" ->
                        let
                            cutoff =
                                String.toFloat model.graphCutoffInput |> Maybe.withDefault 1.0
                        in
                        -- Load both graph and tree data to get activity name
                        Cmd.batch
                            [ loadGraphData activityId cutoff
                            , loadActivityTree activityId
                            ]

                    _ ->
                        Cmd.none

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
            ( { model
                | cachedTrees = Dict.insert model.currentActivityId tree model.cachedTrees
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

        UpdateGraphCutoff cutoffStr ->
            -- Allow any string input, validation happens when loading
            ( { model | graphCutoffInput = cutoffStr }, Cmd.none )

        NodeClicked nodeId ->
            if nodeId /= model.currentActivityId then
                ( { model
                    | navigationHistory = model.currentActivityId :: model.navigationHistory
                  }
                , navigateToActivity model.key nodeId
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
                                    , navigateToActivity model.key parentId
                                    )

                                Nothing ->
                                    case model.navigationHistory of
                                        parentId :: rest ->
                                            ( { model
                                                | navigationHistory = rest
                                              }
                                            , navigateToActivity model.key parentId
                                            )

                                        [] ->
                                            ( model, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        NavigateToPage page ->
            let
                url =
                    case page of
                        ActivitiesPage ->
                            "/#activities"

                        TreePage ->
                            "/#activity/" ++ model.currentActivityId ++ "/tree"

                        InventoryPage ->
                            "/#activity/" ++ model.currentActivityId ++ "/inventory"

                        GraphPage ->
                            "/#activity/" ++ model.currentActivityId ++ "/graph"
            in
            ( model, Nav.pushUrl model.key url )

        UpdateSearchQuery query ->
            ( { model | activitiesSearchQuery = query }
            , if String.length query >= 2 then
                searchActivities query

              else
                Cmd.none
            )

        SearchActivities query ->
            ( { model | searchLoading = True, error = Nothing }
            , searchActivities query
            )

        ActivitiesSearchResults (Ok results) ->
            ( { model
                | searchResults = Just results
                , searchLoading = False
                , error = Nothing
              }
            , Cmd.none
            )

        ActivitiesSearchResults (Err error) ->
            ( { model
                | searchResults = Nothing
                , searchLoading = False
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

                ( newActivityId, needsActivity ) =
                    case route of
                        ActivityRoute processId ->
                            ( processId, True )

                        ActivityTreeRoute processId ->
                            ( processId, True )

                        ActivityInventoryRoute processId ->
                            ( processId, True )

                        ActivityGraphRoute processId ->
                            ( processId, True )

                        _ ->
                            ( model.currentActivityId, False )

                shouldLoadTree =
                    needsActivity && newPage == TreePage && not (Dict.member newActivityId model.cachedTrees)

                shouldLoadInventory =
                    needsActivity && newPage == InventoryPage && not (Dict.member newActivityId model.cachedInventories)

                shouldLoadGraph =
                    needsActivity && newPage == GraphPage && not (Dict.member newActivityId model.cachedGraphs)

                -- Also load tree data for graph page to get activity name
                shouldLoadTreeForGraph =
                    shouldLoadGraph && not (Dict.member newActivityId model.cachedTrees)

                shouldLoad =
                    shouldLoadTree || shouldLoadInventory || shouldLoadGraph

                updatedModel =
                    { model
                        | url = url
                        , currentPage = newPage
                        , currentActivityId = newActivityId
                        , loading = shouldLoad
                        , navigationHistory = model.navigationHistory  -- Preserve navigation history
                    }

                cmd =
                    if shouldLoadTree then
                        loadActivityTree newActivityId

                    else if shouldLoadInventory then
                        loadInventoryData newActivityId

                    else if shouldLoadGraph then
                        let
                            cutoff =
                                String.toFloat updatedModel.graphCutoffInput |> Maybe.withDefault 1.0

                            graphCmd =
                                loadGraphData newActivityId cutoff

                            treeCmd =
                                if shouldLoadTreeForGraph then
                                    loadActivityTree newActivityId

                                else
                                    Cmd.none
                        in
                        Cmd.batch [ graphCmd, treeCmd ]

                    else
                        Cmd.none
            in
            ( updatedModel, cmd )



-- Navigation helpers


routeToUrl : Route -> String
routeToUrl route =
    case route of
        ActivitiesRoute ->
            "/#activities"

        ActivityRoute processId ->
            "/#activity/" ++ processId

        ActivityTreeRoute processId ->
            "/#activity/" ++ processId ++ "/tree"

        ActivityInventoryRoute processId ->
            "/#activity/" ++ processId ++ "/inventory"

        ActivityGraphRoute processId ->
            "/#activity/" ++ processId ++ "/graph"

        NotFoundRoute ->
            "/"


navigateToActivity : Nav.Key -> String -> Cmd Msg
navigateToActivity key processId =
    Nav.pushUrl key (routeToUrl (ActivityTreeRoute processId))


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.graphViewModel of
        Just graphModel ->
            Sub.map GraphViewMsg (GraphView.subscriptions graphModel)

        Nothing ->
            Sub.none


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
                        )
                        (ActivitiesView.viewActivitiesPage
                            model.activitiesSearchQuery
                            model.searchResults
                            model.searchLoading
                            model.error
                        )

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
            ]
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
                              div [ class "level" ]
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
                            , -- SVG Tree visualization
                              Html.map
                                (\msg ->
                                    case msg of
                                        TreeView.NodeClicked nodeId ->
                                            NodeClicked nodeId

                                        TreeView.NodeHovered maybeNodeId ->
                                            NodeHovered maybeNodeId
                                )
                                (TreeView.viewTree tree model.hoveredNode)
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
          div []
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
    nav [ class "navbar is-light" ]
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
