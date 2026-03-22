module Pages.Composition exposing (Model, Msg, page)

import Browser.Navigation as Nav
import Dict exposing (Dict)
import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Api
import Http
import Models.Activity exposing (ActivityInfo)
import Models.SupplyChain exposing (SupplyChainEntry, SupplyChainResponse)
import Route
import Set exposing (Set)
import Shared exposing (RemoteData(..))
import Spa.Page
import Utils.Format as Format
import View exposing (View)
import Views.ActivityHeader


type alias Model =
    { activityId : String
    , dbName : String
    , activityInfo : RemoteData ActivityInfo
    , supplyChain : RemoteData SupplyChainResponse
    , expanded : Set String -- expanded classification paths
    }


type Msg
    = ActivityInfoLoaded (Result Http.Error ActivityInfo)
    | SupplyChainLoaded (Result Http.Error SupplyChainResponse)
    | ToggleNode String
    | NavigateToActivity String
    | NavigateBack
    | RequestLoadDatabase
    | NewFlags ( String, String )


page : Shared.Model -> Spa.Page.Page ( String, String ) Shared.Msg (View Msg) Model Msg
page shared =
    Spa.Page.element
        { init = init shared
        , update = update shared
        , view = view shared
        , subscriptions = \_ -> Sub.none
        }
        |> Spa.Page.onNewFlags NewFlags


init : Shared.Model -> ( String, String ) -> ( Model, Effect Shared.Msg Msg )
init shared ( db, activityId ) =
    if not (Shared.isDatabaseLoaded shared db) then
        ( { activityId = activityId
          , dbName = db
          , activityInfo = NotAsked
          , supplyChain = NotAsked
          , expanded = Set.empty
          }
        , Effect.none
        )

    else
        let
            cached =
                Dict.get activityId shared.cachedActivityInfo
        in
        ( { activityId = activityId
          , dbName = db
          , activityInfo =
                case cached of
                    Just info ->
                        Loaded info

                    Nothing ->
                        Loading
          , supplyChain = Loading
          , expanded = Set.empty
          }
        , Effect.batch
            [ case cached of
                Just _ ->
                    Effect.none

                Nothing ->
                    Effect.fromCmd (Api.loadActivityInfo ActivityInfoLoaded db activityId)
            , Effect.fromCmd (Api.loadSupplyChain SupplyChainLoaded db activityId)
            ]
        )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update shared msg model =
    case msg of
        ActivityInfoLoaded (Ok info) ->
            ( { model | activityInfo = Loaded info }
            , Effect.fromShared (Shared.CacheActivityInfo model.activityId info)
            )

        ActivityInfoLoaded (Err error) ->
            ( { model | activityInfo = Failed (Shared.httpErrorToString error) }
            , Effect.none
            )

        SupplyChainLoaded (Ok data) ->
            -- Auto-expand first level
            let
                firstLevel =
                    data.supplyChain
                        |> List.filterMap (\e -> classificationPath e |> List.head)
                        |> List.foldl Set.insert Set.empty
            in
            ( { model | supplyChain = Loaded data, expanded = firstLevel }
            , Effect.none
            )

        SupplyChainLoaded (Err error) ->
            ( { model | supplyChain = Failed (Shared.httpErrorToString error) }
            , Effect.none
            )

        ToggleNode path ->
            ( { model
                | expanded =
                    if Set.member path model.expanded then
                        Set.remove path model.expanded

                    else
                        Set.insert path model.expanded
              }
            , Effect.none
            )

        NavigateToActivity processId ->
            let
                ( dbName, actualProcessId ) =
                    case String.split "::" processId of
                        [ db, pid ] ->
                            ( db, pid )

                        _ ->
                            ( model.dbName, processId )
            in
            ( model
            , Effect.batch
                [ Effect.fromShared (Shared.PushActivity model.dbName model.activityId)
                , Effect.fromCmd (Nav.pushUrl shared.key (Route.routeToUrl (Route.ActivityRoute Route.Upstream dbName actualProcessId)))
                ]
            )

        NavigateBack ->
            ( model
            , Effect.fromShared Shared.NavigateBackToParent
            )

        RequestLoadDatabase ->
            ( model
            , Effect.fromShared (Shared.LoadDatabase model.dbName)
            )

        NewFlags flags ->
            init shared flags


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Composition"
    , body = viewBody shared model
    }


viewBody : Shared.Model -> Model -> Html Msg
viewBody shared model =
    if not (Shared.isDatabaseLoaded shared model.dbName) then
        Shared.viewLoadDatabasePrompt shared model.dbName RequestLoadDatabase

    else
        div [ class "details-page-container" ]
            [ case model.activityInfo of
                Loading ->
                    viewLoading

                Failed err ->
                    viewError err

                Loaded activityInfo ->
                    div []
                        [ Views.ActivityHeader.viewActivityHeader activityInfo "Composition" NavigateBack
                        , viewSupplyChainTree model
                        ]

                NotAsked ->
                    text ""
            ]


viewLoading : Html msg
viewLoading =
    div [ class "has-text-centered" ]
        [ div [ class "is-size-3" ] [ text "Loading..." ]
        , progress [ class "progress is-primary", attribute "max" "100" ] []
        ]


viewError : String -> Html msg
viewError err =
    div [ class "notification is-danger" ]
        [ strong [] [ text "Error: " ]
        , text err
        ]


viewSupplyChainTree : Model -> Html Msg
viewSupplyChainTree model =
    case model.supplyChain of
        Loading ->
            viewLoading

        Failed err ->
            viewError err

        Loaded data ->
            let
                tree =
                    buildClassificationTree data.supplyChain
            in
            div [ style "margin-top" "1rem" ]
                [ div [ style "margin-bottom" "0.75rem", style "color" "#666", style "font-size" "0.9rem" ]
                    [ text (String.fromInt data.totalActivities ++ " upstream activities, showing " ++ String.fromInt (List.length data.supplyChain) ++ " largest") ]
                , viewTree model.expanded 0 tree
                ]

        NotAsked ->
            text ""



-- Classification tree data structure


type Tree
    = Branch String (List Tree) (List SupplyChainEntry) Float
      -- name, children, leaf entries at this level, total quantity


classificationPath : SupplyChainEntry -> List String
classificationPath entry =
    case Dict.get "Category" entry.classifications of
        Just cat ->
            if String.isEmpty cat then
                [ "(unclassified)" ]

            else
                String.split "\\" cat

        Nothing ->
            [ "(unclassified)" ]


buildClassificationTree : List SupplyChainEntry -> List Tree
buildClassificationTree entries =
    buildTreeLevel entries 0


buildTreeLevel : List SupplyChainEntry -> Int -> List Tree
buildTreeLevel entries depth =
    let
        -- Group entries by their segment at this depth
        grouped =
            List.foldl
                (\entry acc ->
                    let
                        path =
                            classificationPath entry

                        key =
                            List.drop depth path |> List.head |> Maybe.withDefault "(other)"
                    in
                    Dict.update key
                        (\existing ->
                            case existing of
                                Just list ->
                                    Just (entry :: list)

                                Nothing ->
                                    Just [ entry ]
                        )
                        acc
                )
                Dict.empty
                entries

        -- Build tree nodes, sorted by total quantity descending
        nodes =
            Dict.toList grouped
                |> List.map
                    (\( key, groupEntries ) ->
                        let
                            totalQty =
                                List.foldl (\e acc -> acc + abs e.quantity) 0 groupEntries

                            -- Entries whose path ends at this depth = leaves here
                            -- Entries whose path continues = recurse deeper
                            ( leaves, deeper ) =
                                List.partition
                                    (\e -> List.length (classificationPath e) <= depth + 1)
                                    groupEntries

                            children =
                                if List.isEmpty deeper then
                                    []

                                else
                                    buildTreeLevel deeper (depth + 1)
                        in
                        Branch key children leaves totalQty
                    )
                |> List.sortBy (\(Branch _ _ _ qty) -> negate qty)
    in
    nodes


viewTree : Set String -> Int -> List Tree -> Html Msg
viewTree expanded depth nodes =
    div [ style "margin-left" (String.fromInt (depth * 16) ++ "px") ]
        (List.map (viewTreeNode expanded depth) nodes)


viewTreeNode : Set String -> Int -> Tree -> Html Msg
viewTreeNode expanded depth (Branch name children leaves totalQty) =
    let
        hasChildren =
            not (List.isEmpty children) || not (List.isEmpty leaves)

        pathKey =
            name

        isExpanded =
            Set.member pathKey expanded

        icon =
            if not hasChildren then
                "fas fa-minus"

            else if isExpanded then
                "fas fa-chevron-down"

            else
                "fas fa-chevron-right"

        unit =
            leaves
                |> List.head
                |> Maybe.map .unit
                |> Maybe.withDefault
                    (children
                        |> List.head
                        |> Maybe.andThen
                            (\(Branch _ _ childLeaves _) ->
                                List.head childLeaves |> Maybe.map .unit
                            )
                        |> Maybe.withDefault ""
                    )
    in
    div [ style "margin-bottom" "2px" ]
        [ div
            [ style "display" "flex"
            , style "align-items" "center"
            , style "padding" "0.35rem 0.5rem"
            , style "cursor"
                (if hasChildren then
                    "pointer"

                 else
                    "default"
                )
            , style "border-radius" "4px"
            , style "background"
                (if depth == 0 then
                    "#f5f5f5"

                 else
                    "transparent"
                )
            , onClick (ToggleNode pathKey)
            ]
            [ span [ class "icon is-small", style "margin-right" "0.35rem", style "color" "#888", style "font-size" "0.7rem" ]
                [ i [ class icon ] [] ]
            , span [ style "font-weight"
                        (if depth == 0 then
                            "600"

                         else
                            "normal"
                        )
                   , style "flex" "1"
                   ]
                [ text name ]
            , span [ style "color" "#888", style "font-size" "0.9rem", style "margin-left" "1rem" ]
                [ text (Format.formatScientific totalQty ++ " " ++ unit) ]
            ]
        , if isExpanded then
            div []
                [ if not (List.isEmpty leaves) then
                    viewLeaves leaves

                  else
                    text ""
                , viewTree expanded (depth + 1) children
                ]

          else
            text ""
        ]


viewLeaves : List SupplyChainEntry -> Html Msg
viewLeaves entries =
    let
        sorted =
            List.sortBy (\e -> negate (abs e.quantity)) entries
    in
    div [ style "margin-left" "24px" ]
        (List.map viewLeafEntry sorted)


viewLeafEntry : SupplyChainEntry -> Html Msg
viewLeafEntry entry =
    div
        [ style "display" "flex"
        , style "align-items" "center"
        , style "padding" "0.2rem 0.5rem"
        , style "font-size" "0.9rem"
        , style "cursor" "pointer"
        , style "border-radius" "3px"
        , class "leaf-entry"
        , onClick (NavigateToActivity entry.processId)
        ]
        [ span [ style "flex" "1", style "color" "#333" ]
            [ text entry.name
            , span [ style "color" "#aaa", style "margin-left" "0.5rem", style "font-size" "0.8rem" ]
                [ text entry.location ]
            ]
        , span [ style "color" "#555", style "font-family" "monospace", style "font-size" "0.85rem" ]
            [ text (Format.formatScientific entry.quantity ++ " " ++ entry.unit) ]
        ]
