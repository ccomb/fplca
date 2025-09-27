module Main exposing (main)

import Browser
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Models.Activity exposing (ActivityTree, activityTreeDecoder)
import Views.TreeView as TreeView


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { currentTree : Maybe ActivityTree
    , currentActivityId : String
    , loading : Bool
    , error : Maybe String
    , navigationHistory : List String
    }


type Msg
    = LoadActivity String
    | ActivityLoaded (Result Http.Error ActivityTree)
    | NavigateToParent
    | NodeClicked String


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initialActivityId =
            "cfc47a90-26ad-5e47-87d6-16e89be827b1"
    in
    ( { currentTree = Nothing
      , currentActivityId = initialActivityId
      , loading = True
      , error = Nothing
      , navigationHistory = []
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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    div [ class "container is-fluid" ]
        [ -- Header with Bulma styling
          nav [ class "navbar is-light" ]
            [ div [ class "navbar-brand" ]
                [ div [ class "navbar-item" ]
                    [ h1 [ class "title is-4" ] [ text "ACV Engine - Activity Tree Navigator" ]
                    ]
                ]
            ]
        , -- Navigation bar
          div []
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
        , -- Main content
          div []
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
                            [ Html.map NodeClicked (TreeView.viewTree tree)
                            ]
                        ]

                ( _, _, Nothing ) ->
                    div [ class "has-text-centered" ]
                        [ text "No data to display" ]
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
