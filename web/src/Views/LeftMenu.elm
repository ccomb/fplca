module Views.LeftMenu exposing (Msg(..), viewLeftMenu)

import Html exposing (Html, button, div, i, nav, p, span, text)
import Html.Attributes exposing (class, classList, style, title)
import Html.Events exposing (onClick)
import Models.Page exposing (Page(..))


type Msg
    = NavigateTo Page


viewLeftMenu : Page -> String -> Maybe String -> Maybe String -> Html Msg
viewLeftMenu currentPage currentActivityId currentDatabaseName currentActivityName =
    nav [ class "left-menu" ]
        [ -- Database name as header (clickable to go to Databases page)
          case currentDatabaseName of
            Just dbName ->
                button
                    [ class "database-header"
                    , onClick (NavigateTo DatabasesPage)
                    , style "background" "transparent"
                    , style "border" "none"
                    , style "color" "#000"
                    , style "padding" "1.25rem 1rem"
                    , style "margin" "0"
                    , style "cursor" "pointer"
                    , style "font-size" "1.4rem"
                    , style "font-weight" "bold"
                    , style "text-align" "left"
                    , style "width" "100%"
                    , style "display" "flex"
                    , style "align-items" "center"
                    , style "gap" "0.5rem"
                    , title ("Go to Databases page - " ++ dbName)
                    ]
                    [ span [ style "overflow" "hidden", style "text-overflow" "ellipsis", style "white-space" "nowrap" ] [ text dbName ]
                    ]

            Nothing ->
                div [ class "database-header", style "padding" "1rem", style "color" "#888" ]
                    [ text "Loading..." ]
        , -- SEARCH section
          div [ class "menu-items" ]
            [ menuLabel "Search"
            , menuItem currentPage ActivitiesPage "fas fa-search" "Activities" False
            ]
        , -- Activity section (only show if an activity is selected)
          case currentActivityName of
            Just actName ->
                div [ class "menu-items" ]
                    [ activityLabel actName
                    , menuItem currentPage UpstreamPage "fas fa-arrow-up" "Upstream activities" False
                    , menuItem currentPage EmissionsPage "fas fa-cloud" "Direct emissions" False
                    , menuItem currentPage ResourcesPage "fas fa-leaf" "Natural resources" False
                    , menuItem currentPage ProductsPage "fas fa-box" "Outgoing products" False
                    , menuItem currentPage InventoryPage "fas fa-list-ul" "Inventory" False
                    ]

            Nothing ->
                text ""
        , -- LAB section
          div [ class "menu-items" ]
            [ menuLabel "Lab"
            , menuItem currentPage LCIAPage "fas fa-chart-bar" "Impacts" True
            , menuItem currentPage TreePage "fas fa-project-diagram" "Tree" True
            , menuItem currentPage GraphPage "fas fa-network-wired" "Graph" True
            ]
        , -- fpLCA at bottom
          div
            [ style "position" "absolute"
            , style "bottom" "0"
            , style "left" "0"
            , style "right" "0"
            , style "padding" "0.75rem 1rem"
            , style "color" "#666"
            , style "font-size" "0.85rem"
            , style "text-align" "center"
            , style "border-top" "1px solid #444"
            ]
            [ text "fpLCA" ]
        ]


menuLabel : String -> Html Msg
menuLabel label =
    p [ class "menu-label has-text-grey-light", style "padding" "0.5rem 1rem", style "margin-top" "0.5rem", style "font-size" "0.75rem", style "text-transform" "uppercase" ]
        [ text label ]


activityLabel : String -> Html Msg
activityLabel actName =
    let
        truncatedName =
            if String.length actName > 25 then
                String.left 22 actName ++ "..."

            else
                actName
    in
    p
        [ class "menu-label has-text-white"
        , style "padding" "0.5rem 1rem"
        , style "margin-top" "0.5rem"
        , style "font-size" "0.8rem"
        , style "font-weight" "500"
        , style "overflow" "hidden"
        , style "text-overflow" "ellipsis"
        , style "white-space" "nowrap"
        , title actName
        ]
        [ text truncatedName ]


menuItem : Page -> Page -> String -> String -> Bool -> Html Msg
menuItem currentPage targetPage iconClass label isLab =
    button
        [ classList
            [ ( "menu-item", True )
            , ( "is-active", currentPage == targetPage )
            , ( "is-lab", isLab )
            ]
        , onClick (NavigateTo targetPage)
        ]
        [ span [ class "icon" ]
            [ i [ class iconClass ] []
            ]
        , span [ class "menu-label" ] [ text label ]
        ]
