module Views.LeftMenu exposing (Msg(..), viewLeftMenu)

import Char
import Html exposing (Html, button, div, i, nav, p, span, text)
import Html.Attributes exposing (class, classList, style)
import Html.Events exposing (onClick, stopPropagationOn)
import Json.Decode
import Models.Page exposing (Page(..))


type Msg
    = NavigateTo Page
    | ToggleConsole
    | CloseConsole


viewLeftMenu : Page -> String -> Maybe String -> Maybe String -> String -> Bool -> Html Msg
viewLeftMenu currentPage currentActivityId currentDatabaseName currentActivityName version showConsole =
    nav [ class "left-menu", style "display" "flex", style "flex-direction" "column", style "height" "100%", onClick CloseConsole ]
        [ -- Top section (scrollable content)
          div [ style "flex" "1", style "overflow-y" "auto" ]
            [ -- Database name as header (not clickable)
              div
                [ class "database-header"
                , style "padding" "1.25rem 1rem"
                , style "font-size" "1.4rem"
                , style "font-weight" "bold"
                , style "text-align" "center"
                ]
                [ case currentDatabaseName of
                    Just dbName ->
                        span [ style "overflow" "hidden", style "text-overflow" "ellipsis", style "white-space" "nowrap", style "display" "block" ] [ text dbName ]

                    Nothing ->
                        span [ style "color" "#888" ]
                            [ span [ style "color" "#00d1b2", style "font-style" "italic" ] [ text "f" ]
                            , text "·"
                            , span [ style "color" "#e87c23", style "font-style" "italic" ] [ text "p" ]
                            , text "·LCA"
                            ]
                ]
            , -- DATABASES section
              div [ class "menu-items" ]
                [ menuLabel "Databases"
                , menuItem currentPage DatabasesPage "fas fa-database" "Databases" False
                ]
            , -- SEARCH section
              div [ class "menu-items" ]
                [ menuLabel "Search"
                , menuItem currentPage ActivitiesPage "fas fa-search" "Activities" False
                ]
            , -- Activity section (only show if an activity is selected)
              -- White background to look like vertical tabs connected to main content
              case currentActivityName of
                Just _ ->
                    div
                        [ class "menu-items explore-section"
                        , style "background-color" "white"
                        , style "margin" "0.5rem 0 0.5rem 0.5rem"
                        , style "padding" "0.5rem 0"
                        , style "border-radius" "6px 0 0 6px"
                        ]
                        [ menuLabel "Explore"
                        , menuItem currentPage UpstreamPage "fas fa-arrow-up" "Upstream activities" False
                        , menuItem currentPage EmissionsPage "fas fa-cloud" "Direct emissions" False
                        , menuItem currentPage ResourcesPage "fas fa-leaf" "Natural resources" False
                        , menuItem currentPage ProductsPage "fas fa-box" "Outgoing products" False
                        , menuItem currentPage InventoryPage "fas fa-list-ul" "Inventory" False
                        , menuLabel "Lab"
                        , menuItem currentPage LCIAPage "fas fa-chart-bar" "Impacts" True
                        , menuItem currentPage TreePage "fas fa-project-diagram" "Tree" True
                        , menuItem currentPage GraphPage "fas fa-network-wired" "Graph" True
                        ]

                Nothing ->
                    text ""
            ]
        , -- Footer (fixed)
          div
            [ class "menu-footer"
            , style "flex-shrink" "0"
            , style "padding" "0.5rem 1rem"
            , style "color" "#888"
            , style "font-size" "0.8rem"
            , style "text-align" "center"
            , style "border-top" "1px solid #ddd"
            ]
            [ button
                [ style "background" "none"
                , style "border" "none"
                , style "cursor" "pointer"
                , style "color" (if showConsole then "#3273dc" else "#888")
                , style "font-size" "0.8rem"
                , style "font-family" "inherit"
                , style "padding" "0.25rem 0"
                , style "margin-bottom" "0.5rem"
                , style "display" "inline-flex"
                , style "align-items" "center"
                , style "gap" "0.4rem"
                , stopPropagationOn "click" (Json.Decode.succeed ( ToggleConsole, True ))
                ]
                [ i [ class "fas fa-terminal", style "font-size" "0.7rem" ] []
                , span [] [ text "Console output" ]
                ]
            , div []
                [ span [ style "color" "#00d1b2", style "font-style" "italic" ] [ text "f" ]
                , text "·"
                , span [ style "color" "#e87c23", style "font-style" "italic" ] [ text "p" ]
                , text "·LCA"
                ]
            , div [ style "font-size" "0.8em", style "margin-top" "0.25rem" ]
                [ text (formatVersion version) ]
            ]
        ]


formatVersion : String -> String
formatVersion version =
    -- Check if version looks like a semantic version (starts with digit or 'v' followed by digit)
    case String.uncons version of
        Just ( first, _ ) ->
            if Char.isDigit first then
                "version " ++ version

            else if first == 'v' then
                -- Remove 'v' prefix for display
                "version " ++ String.dropLeft 1 version

            else
                -- Commit hash - just show as-is
                version

        Nothing ->
            version


menuLabel : String -> Html Msg
menuLabel label =
    p [ class "menu-label has-text-grey-light", style "padding" "0.5rem 1rem", style "margin-top" "0.5rem", style "font-size" "0.75rem", style "text-transform" "uppercase" ]
        [ text label ]


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
