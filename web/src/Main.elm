module Main exposing (main)

import Browser
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Url.Builder
import Models.Activity exposing (ActivityTree, ActivitySummary, SearchResults, activityTreeDecoder, activitySummaryDecoder, searchResultsDecoder)
import Models.Page exposing (Page(..))
import Views.TreeView as TreeView
import Views.LeftMenu as LeftMenu
import Views.ActivitiesView as ActivitiesView


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { currentPage : Page
    , currentTree : Maybe ActivityTree
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
    | NavigateToParent
    | NodeClicked String
    | NavigateToPage Page
    | UpdateSearchQuery String
    | SearchActivities String
    | ActivitiesSearchResults (Result Http.Error (SearchResults ActivitySummary))
    | SelectActivity String
    | NodeHovered (Maybe String)


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initialActivityId =
            "22222222-3333-4444-5555-666666666661_chemical-b-uuid"  -- Updated for SAMPLE.switching
    in
    ( { currentPage = GraphPage
      , currentTree = Nothing
      , currentActivityId = initialActivityId
      , loading = True
      , error = Nothing
      , navigationHistory = []
      , activitiesSearchQuery = ""
      , searchResults = Nothing
      , searchLoading = False
      , hoveredNode = Nothing
      }
    , loadActivityTree initialActivityId
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadActivity activityId ->
            ( { model
                | loading = True
                , error = Nothing
                , currentActivityId = activityId
              }
            , loadActivityTree activityId
            )

        ActivityLoaded (Ok tree) ->
            ( { model
                | currentTree = Just tree
                , loading = False
                , error = Nothing
              }
            , Cmd.none
            )

        ActivityLoaded (Err error) ->
            ( { model
                | currentTree = Nothing
                , loading = False
                , error = Just (httpErrorToString error)
              }
            , Cmd.none
            )

        NodeClicked nodeId ->
            if nodeId /= model.currentActivityId then
                ( { model
                    | navigationHistory = model.currentActivityId :: model.navigationHistory
                  }
                , Cmd.batch
                    [ loadActivityTree nodeId ]
                )

            else
                ( model, Cmd.none )

        NavigateToParent ->
            case model.currentTree of
                Just tree ->
                    case Dict.get model.currentActivityId tree.nodes of
                        Just currentNode ->
                            case currentNode.parentId of
                                Just parentId ->
                                    ( { model
                                        | navigationHistory = List.drop 1 model.navigationHistory
                                      }
                                    , loadActivityTree parentId
                                    )

                                Nothing ->
                                    case model.navigationHistory of
                                        parentId :: rest ->
                                            ( { model
                                                | navigationHistory = rest
                                              }
                                            , loadActivityTree parentId
                                            )

                                        [] ->
                                            ( model, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        NavigateToPage page ->
            ( { model | currentPage = page }, Cmd.none )

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
                | currentActivityId = activityId
                , currentPage = GraphPage
                , navigationHistory = model.currentActivityId :: model.navigationHistory
              }
            , loadActivityTree activityId
            )

        NodeHovered nodeId ->
            ( { model | hoveredNode = nodeId }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    div [ class "app-container" ]
        [ Html.map NavigateToPage (LeftMenu.viewLeftMenu model.currentPage)
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
                            [ case model.currentTree of
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
                [ case ( model.loading, model.error, model.currentTree ) of
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
    case model.currentTree of
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
