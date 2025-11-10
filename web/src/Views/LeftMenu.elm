module Views.LeftMenu exposing (viewLeftMenu)

import Html exposing (Html, button, div, i, nav, span, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Models.Page exposing (Page(..))


type alias Msg =
    Page


viewLeftMenu : Page -> String -> Html Msg
viewLeftMenu currentPage currentActivityId =
    nav [ class "left-menu" ]
        [ div [ class "menu-header" ]
            [ span [ class "title is-5 has-text-white" ] [ text "ACV Engine" ]
            ]
        , div [ class "menu-items" ]
            [ menuItem currentPage ActivitiesPage "fas fa-search" "Activities"
            , menuItem currentPage TreePage "fas fa-project-diagram" "Tree"
            , menuItem currentPage InventoryPage "fas fa-list-ul" "Inventory"
            ]
        ]


menuItem : Page -> Page -> String -> String -> Html Msg
menuItem currentPage targetPage iconClass label =
    button
        [ classList
            [ ( "menu-item", True )
            , ( "is-active", currentPage == targetPage )
            ]
        , onClick targetPage
        ]
        [ span [ class "icon" ]
            [ i [ class iconClass ] []
            ]
        , span [ class "menu-label" ] [ text label ]
        ]

