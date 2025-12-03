module Views.LeftMenu exposing (viewLeftMenu)

import Html exposing (Html, button, div, i, nav, p, span, text)
import Html.Attributes exposing (class, classList, style)
import Html.Events exposing (onClick)
import Models.Page exposing (Page(..))


type alias Msg =
    Page


viewLeftMenu : Page -> String -> Html Msg
viewLeftMenu currentPage currentActivityId =
    nav [ class "left-menu" ]
        [ div [ class "menu-header" ]
            [ span [ class "title is-5 has-text-white" ] [ text "fpLCA" ]
            ]
        , div [ class "menu-items" ]
            [ menuItem currentPage ActivitiesPage "fas fa-search" "Activities" False
            , menuItem currentPage DetailsPage "fas fa-table" "Details" False
            , menuItem currentPage InventoryPage "fas fa-list-ul" "Inventory" False
            , menuLabel "Lab"
            , menuItem currentPage TreePage "fas fa-project-diagram" "Tree" True
            , menuItem currentPage GraphPage "fas fa-network-wired" "Graph" True
            ]
        ]


menuLabel : String -> Html Msg
menuLabel label =
    p [ class "menu-label has-text-grey-light", style "padding" "0.5rem 1rem", style "margin-top" "0.5rem", style "font-size" "0.75rem" ]
        [ text label ]


menuItem : Page -> Page -> String -> String -> Bool -> Html Msg
menuItem currentPage targetPage iconClass label isLab =
    button
        [ classList
            [ ( "menu-item", True )
            , ( "is-active", currentPage == targetPage )
            , ( "is-lab", isLab )
            ]
        , onClick targetPage
        ]
        [ span [ class "icon" ]
            [ i [ class iconClass ] []
            ]
        , span [ class "menu-label" ] [ text label ]
        ]

