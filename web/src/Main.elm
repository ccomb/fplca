module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Url
import Url.Builder
import Url.Parser as Parser exposing (Parser, oneOf, string, (</>), parse, top)
import Models.Activity exposing (ActivityTree, ActivitySummary, SearchResults, activityTreeDecoder, activitySummaryDecoder, searchResultsDecoder)
import Models.Inventory exposing (InventoryExport, inventoryExportDecoder)
import Models.Page exposing (Page(..), Route(..))
import Views.TreeView as TreeView
import Views.LeftMenu as LeftMenu
import Views.ActivitiesView as ActivitiesView
import Views.InventoryView as InventoryView


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = \model -> { title = "ACV Engine", body = [view model] }
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , currentPage : Page
    , cachedTrees : Dict.Dict String ActivityTree  -- Cache trees by activity ID
    , cachedInventories : Dict.Dict String InventoryExport  -- Cache inventories by activity ID
    , currentActivityId : String
    , loading : Bool
    , error : Maybe String
    , navigationHistory : List String
    , activitiesSearchQuery : String
    , searchResults : Maybe (SearchResults ActivitySummary)
    , searchLoading : Bool
    , hoveredNode : Maybe String
    }


type Msg
    = LoadActivity String
    | ActivityLoaded (Result Http.Error ActivityTree)
    | LoadInventory String
    | InventoryLoaded (Result Http.Error InventoryExport)
    | NavigateToParent
    | NodeClicked String
    | NavigateToPage Page
    | UpdateSearchQuery String
    | SearchActivities String
    | ActivitiesSearchResults (Result Http.Error (SearchResults ActivitySummary))
    | SelectActivity String
    | NodeHovered (Maybe String)
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
            GraphPage

        ActivityTreeRoute _ ->
            GraphPage

        ActivityInventoryRoute _ ->
            InventoryPage

        NotFoundRoute ->
            ActivitiesPage


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        route = parseUrl url
        defaultActivityId = "22222222-3333-4444-5555-666666666661_chemical-b-uuid"

        (activityId, shouldLoad, loadType) =
            case route of
                ActivityRoute processId ->
                    (processId, True, "tree")

                ActivityTreeRoute processId ->
                    (processId, True, "tree")

                ActivityInventoryRoute processId ->
                    (processId, True, "inventory")

                _ ->
                    (defaultActivityId, False, "none")

        initialPage = routeToPage route

        model =
            { key = key
            , url = url
            , currentPage = initialPage
            , cachedTrees = Dict.empty
            , cachedInventories = Dict.empty
            , currentActivityId = activityId
            , loading = shouldLoad
            , error = Nothing
            , navigationHistory = []
            , activitiesSearchQuery = ""
            , searchResults = Nothing
            , searchLoading = False
            , hoveredNode = Nothing
            }
        cmd =
            if shouldLoad then
                case loadType of
                    "tree" ->
                        loadActivityTree activityId
                    "inventory" ->
                        loadInventoryData activityId
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
                shouldLoad = not (Dict.member activityId model.cachedTrees)
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
                shouldLoad = not (Dict.member activityId model.cachedInventories)
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
                                        , currentActivityId = parentId
                                        , loading = not (Dict.member parentId model.cachedTrees)
                                      }
                                    , if Dict.member parentId model.cachedTrees then
                                        Cmd.none
                                      else
                                        loadActivityTree parentId
                                    )

                                Nothing ->
                                    case model.navigationHistory of
                                        parentId :: rest ->
                                            ( { model
                                                | navigationHistory = rest
                                                , currentActivityId = parentId
                                                , loading = not (Dict.member parentId model.cachedTrees)
                                              }
                                            , if Dict.member parentId model.cachedTrees then
                                                Cmd.none
                                              else
                                                loadActivityTree parentId
                                            )

                                        [] ->
                                            ( model, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        NavigateToPage page ->
            let
                url = case page of
                    ActivitiesPage -> "/#activities"
                    GraphPage -> "/#activity/" ++ model.currentActivityId ++ "/tree"
                    InventoryPage -> "/#activity/" ++ model.currentActivityId ++ "/inventory"
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

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            let
                route = parseUrl url
                newPage = routeToPage route
                (newActivityId, needsActivity) =
                    case route of
                        ActivityRoute processId ->
                            (processId, True)

                        ActivityTreeRoute processId ->
                            (processId, True)

                        ActivityInventoryRoute processId ->
                            (processId, True)

                        _ ->
                            (model.currentActivityId, False)

                shouldLoadTree = needsActivity && newActivityId /= model.currentActivityId && not (Dict.member newActivityId model.cachedTrees) && newPage == GraphPage
                shouldLoadInventory = needsActivity && newActivityId /= model.currentActivityId && not (Dict.member newActivityId model.cachedInventories) && newPage == InventoryPage
                shouldLoad = shouldLoadTree || shouldLoadInventory

                updatedModel =
                    { model
                    | url = url
                    , currentPage = newPage
                    , currentActivityId = newActivityId
                    , loading = shouldLoad
                    }

                cmd =
                    if shouldLoadTree then
                        loadActivityTree newActivityId
                    else if shouldLoadInventory then
                        loadInventoryData newActivityId
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

        NotFoundRoute ->
            "/"


navigateToActivity : Nav.Key -> String -> Cmd Msg
navigateToActivity key processId =
    Nav.pushUrl key (routeToUrl (ActivityTreeRoute processId))


subscriptions : Model -> Sub Msg
subscriptions _ =
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

                GraphPage ->
                    viewGraphPage model

                InventoryPage ->
                    InventoryView.viewInventoryPage
                        (Dict.get model.currentActivityId model.cachedInventories)
                        model.loading
                        model.error
            ]
        ]


viewGraphPage : Model -> Html Msg
viewGraphPage model =
    div [ class "graph-page" ]
        [ -- Header with navigation
          nav [ class "navbar is-light" ]
            [ div [ class "navbar-brand" ]
                [ div [ class "navbar-item" ]
                    [ h1 [ class "title is-4" ] [ text "Activity Tree Navigator" ]
                    ]
                ]
            ]
        , -- Navigation bar
          div [ class "section" ]
            [ div [ class "container" ]
                [ div [ class "level" ]
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
                            [ case Dict.get model.currentActivityId model.cachedTrees of
                                Just tree ->
                                    case Dict.get model.currentActivityId tree.nodes of
                                        Just currentNode ->
                                            div [ class "tags has-addons" ]
                                                [ span [ class "tag is-dark" ] [ text "Current:" ]
                                                , span [ class "tag is-info" ] [ text currentNode.name ]
                                                , span [ class "tag is-light" ] [ text currentNode.location ]
                                                ]

                                        Nothing ->
                                            span [ class "tag is-warning" ] [ text "Unknown activity" ]

                                Nothing ->
                                    span [ class "tag is-light" ] [ text "Loading..." ]
                            ]
                        ]
                    ]
                ]
            ]
        , -- Main content
          div [ class "section" ]
            [ div [ class "container is-fluid" ]
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
                            [ -- Tree statistics
                              div [ class "notification is-info is-light" ]
                                [ div [ class "columns" ]
                                    [ div [ class "column" ]
                                        [ text ("Total nodes: " ++ String.fromInt tree.tree.totalNodes) ]
                                    , div [ class "column" ]
                                        [ text ("Max depth: " ++ String.fromInt tree.tree.maxDepth) ]
                                    , div [ class "column" ]
                                        [ text ("Expandable: " ++ String.fromInt tree.tree.expandableNodes) ]
                                    ]
                                ]
                            , -- SVG Tree visualization
                              div [ class "box" ]
                                [ Html.map
                                    (\msg ->
                                        case msg of
                                            TreeView.NodeClicked nodeId ->
                                                NodeClicked nodeId

                                            TreeView.NodeHovered maybeNodeId ->
                                                NodeHovered maybeNodeId
                                    )
                                    (TreeView.viewTree tree model.hoveredNode)
                                ]
                            ]

                    ( _, _, Nothing ) ->
                        div [ class "has-text-centered" ]
                            [ text "No data to display" ]
                ]
            ]
        ]


canNavigateToParent : Model -> Bool
canNavigateToParent model =
    case Dict.get model.currentActivityId model.cachedTrees of
        Just tree ->
            case Dict.get model.currentActivityId tree.nodes of
                Just currentNode ->
                    currentNode.parentId /= Nothing || not (List.isEmpty model.navigationHistory)

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


searchActivities : String -> Cmd Msg
searchActivities query =
    Http.get
        { url = Url.Builder.absolute
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
