module Main exposing (main)

import Browser exposing (Document)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode
import Models.Database exposing (DatabaseList)
import Pages.Activities
import Pages.DatabaseSetup
import Pages.Databases
import Pages.Emissions
import Pages.Graph
import Pages.Inventory
import Pages.LCIA
import Pages.Products
import Pages.Resources
import Pages.Tree
import Pages.Upload
import Pages.Upstream
import Route exposing (ActivePage(..), Route(..))
import Shared exposing (ConsoleVisibility(..), RemoteData(..))
import Spa
import View exposing (View)
import Views.LeftMenu as LeftMenu


type alias Flags =
    { version : String }


mappers : ( (a -> b) -> View a -> View b, (c -> d) -> View c -> View d )
mappers =
    ( View.map, View.map )


toDocument :
    Shared.Model
    -> View (Spa.Msg Shared.Msg pageMsg)
    -> Document (Spa.Msg Shared.Msg pageMsg)
toDocument shared pageView =
    { title = pageView.title ++ " - fpLCA"
    , body =
        [ div [ class "app-container" ]
            [ -- Hamburger button (mobile only)
              button
                [ class "button is-dark is-hidden-tablet"
                , style "position" "fixed"
                , style "top" "0.5rem"
                , style "left" "0.5rem"
                , style "z-index" "41"
                , onClick (Spa.mapSharedMsg Shared.ToggleMenu)
                ]
                [ span [ class "icon" ] [ Html.i [ class "fas fa-bars" ] [] ] ]
            , -- Backdrop (mobile only, when menu open)
              if shared.menuOpen then
                div
                    [ style "position" "fixed"
                    , style "top" "0"
                    , style "left" "0"
                    , style "right" "0"
                    , style "bottom" "0"
                    , style "background" "rgba(0,0,0,0.4)"
                    , style "z-index" "39"
                    , onClick (Spa.mapSharedMsg Shared.CloseMenu)
                    ]
                    []

              else
                text ""
            , viewLeftMenu shared
            , div [ class "main-content" ]
                [ pageView.body
                ]
            , case shared.console.visibility of
                Visible _ ->
                    viewConsoleOverlay shared

                Hidden ->
                    text ""
            ]
        ]
    }


viewLeftMenu : Shared.Model -> Html (Spa.Msg Shared.Msg pageMsg)
viewLeftMenu shared =
    let
        activePage =
            Route.routeToActivePage shared.currentRoute

        currentActivityId =
            routeToActivityId shared.currentRoute

        currentDatabaseName =
            case ( shared.currentDatabaseId, shared.databases ) of
                ( Just dbId, Loaded dbList ) ->
                    List.filter (\db -> db.name == dbId) dbList.databases
                        |> List.head
                        |> Maybe.map .displayName

                _ ->
                    Nothing

        currentActivityName =
            case currentActivityId of
                Just actId ->
                    Dict.get actId shared.cachedActivityInfo
                        |> Maybe.map .name

                Nothing ->
                    Nothing

        showConsole =
            case shared.console.visibility of
                Visible _ ->
                    True

                Hidden ->
                    False
    in
    Html.map
        (\leftMenuMsg ->
            LeftMenu.mapMsg
                { onNavigate = \page -> Spa.mapSharedMsg (navigateToPage shared page)
                , onToggleConsole = Spa.mapSharedMsg Shared.ToggleConsole
                , onCloseConsole = Spa.mapSharedMsg Shared.CloseConsole
                }
                leftMenuMsg
        )
        (LeftMenu.viewLeftMenu
            activePage
            (currentActivityId |> Maybe.withDefault "")
            currentDatabaseName
            currentActivityName
            shared.version
            showConsole
            shared.menuOpen
        )


navigateToPage : Shared.Model -> ActivePage -> Shared.Msg
navigateToPage shared page =
    let
        currentActivityId =
            routeToActivityId shared.currentRoute
                |> Maybe.withDefault ""

        dbName =
            Shared.getCurrentDbName shared
    in
    case page of
        DatabasesActive ->
            Shared.NavigateTo DatabasesRoute

        UploadActive ->
            Shared.NavigateTo UploadRoute

        ActivitiesActive ->
            Shared.NavigateTo (ActivitiesRoute { db = dbName, name = Nothing, limit = Just 20 })

        UpstreamActive ->
            Shared.NavigateTo (ActivityUpstreamRoute dbName currentActivityId)

        EmissionsActive ->
            Shared.NavigateTo (ActivityEmissionsRoute dbName currentActivityId)

        ResourcesActive ->
            Shared.NavigateTo (ActivityResourcesRoute dbName currentActivityId)

        ProductsActive ->
            Shared.NavigateTo (ActivityProductsRoute dbName currentActivityId)

        TreeActive ->
            Shared.NavigateTo (ActivityTreeRoute dbName currentActivityId)

        InventoryActive ->
            Shared.NavigateTo (ActivityInventoryRoute dbName currentActivityId)

        GraphActive ->
            Shared.NavigateTo (ActivityGraphRoute dbName currentActivityId)

        LCIAActive ->
            Shared.NavigateTo (ActivityLCIARoute dbName currentActivityId)

        DatabaseSetupActive ->
            Shared.NavigateTo DatabasesRoute


routeToActivityId : Route -> Maybe String
routeToActivityId route =
    case route of
        ActivityRoute _ pid ->
            Just pid

        ActivityUpstreamRoute _ pid ->
            Just pid

        ActivityEmissionsRoute _ pid ->
            Just pid

        ActivityResourcesRoute _ pid ->
            Just pid

        ActivityProductsRoute _ pid ->
            Just pid

        ActivityTreeRoute _ pid ->
            Just pid

        ActivityInventoryRoute _ pid ->
            Just pid

        ActivityGraphRoute _ pid ->
            Just pid

        ActivityLCIARoute _ pid ->
            Just pid

        _ ->
            Nothing


viewConsoleOverlay : Shared.Model -> Html (Spa.Msg Shared.Msg pageMsg)
viewConsoleOverlay shared =
    div
        [ style "position" "fixed"
        , style "top" "0"
        , style "left" "0"
        , style "right" "0"
        , style "bottom" "0"
        , style "background" "rgba(0, 0, 0, 0.5)"
        , style "z-index" "100"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "padding" "2rem"
        , onClick (Spa.mapSharedMsg Shared.ToggleConsole)
        ]
        [ div
            [ style "display" "flex"
            , style "flex-direction" "column"
            , style "flex" "1"
            , style "background" "#1a1a2e"
            , style "border-radius" "8px"
            , style "overflow" "hidden"
            , Html.Events.stopPropagationOn "click" (Json.Decode.succeed ( Spa.mapSharedMsg Shared.NoOp, True ))
            ]
            [ div
                [ style "display" "flex"
                , style "justify-content" "space-between"
                , style "align-items" "center"
                , style "padding" "0.75rem 1rem"
                , style "background" "#16213e"
                , style "color" "#e0e0e0"
                ]
                [ span [ style "font-weight" "bold" ] [ text "Console output" ]
                , button
                    [ onClick (Spa.mapSharedMsg Shared.ToggleConsole)
                    , style "background" "none"
                    , style "border" "none"
                    , style "color" "#e0e0e0"
                    , style "cursor" "pointer"
                    , style "font-size" "1.2rem"
                    , style "padding" "0.25rem 0.5rem"
                    ]
                    [ text "\u{00D7}" ]
                ]
            , div
                [ style "flex" "1"
                , style "overflow-y" "auto"
                , style "color" "#c8c8c8"
                , style "font-family" "'Consolas', 'Monaco', monospace"
                , style "font-size" "0.85rem"
                , style "line-height" "1.5"
                , style "padding" "1rem"
                , id "console-log-container"
                , Html.Events.on "scroll" decodeScrollAtBottom
                ]
                (List.map
                    (\line -> div [] [ text line ])
                    shared.console.logs
                )
            ]
        ]


decodeScrollAtBottom : Json.Decode.Decoder (Spa.Msg Shared.Msg pageMsg)
decodeScrollAtBottom =
    Json.Decode.map3
        (\scrollTop scrollHeight clientHeight ->
            Spa.mapSharedMsg (Shared.ConsoleScrolled (scrollTop + clientHeight >= scrollHeight - 10))
        )
        (Json.Decode.at [ "target", "scrollTop" ] Json.Decode.float)
        (Json.Decode.at [ "target", "scrollHeight" ] Json.Decode.float)
        (Json.Decode.at [ "target", "clientHeight" ] Json.Decode.float)


main =
    Spa.init
        { defaultView = View.defaultView
        , extractIdentity = \_ -> Just ()
        }
        |> Spa.addPublicPage mappers Route.matchActivities Pages.Activities.page
        |> Spa.addPublicPage mappers Route.matchUpstream Pages.Upstream.page
        |> Spa.addPublicPage mappers Route.matchEmissions Pages.Emissions.page
        |> Spa.addPublicPage mappers Route.matchResources Pages.Resources.page
        |> Spa.addPublicPage mappers Route.matchProducts Pages.Products.page
        |> Spa.addPublicPage mappers Route.matchTree Pages.Tree.page
        |> Spa.addPublicPage mappers Route.matchInventory Pages.Inventory.page
        |> Spa.addPublicPage mappers Route.matchGraph Pages.Graph.page
        |> Spa.addPublicPage mappers Route.matchLCIA Pages.LCIA.page
        |> Spa.addPublicPage mappers Route.matchDatabases Pages.Databases.page
        |> Spa.addPublicPage mappers Route.matchUpload Pages.Upload.page
        |> Spa.addPublicPage mappers Route.matchDatabaseSetup Pages.DatabaseSetup.page
        |> Spa.beforeRouteChange Shared.RouteChanged
        |> Spa.application View.map
            { toRoute = Route.toRoute
            , init = Shared.init
            , update = Shared.update
            , subscriptions = Shared.subscriptions
            , toDocument = toDocument
            , protectPage = \_ -> "/"
            }
        |> Browser.application
