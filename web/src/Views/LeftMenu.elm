module Views.LeftMenu exposing (viewLeftMenu)

import Html exposing (Html, div, nav, a, span, i, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Models.Page exposing (Page(..))


type alias Msg =
    Page


viewLeftMenu : Page -> Html Msg
viewLeftMenu currentPage =
    nav [ class "left-menu" ]
        [ div [ class "menu-header" ]
            [ span [ class "title is-5 has-text-white" ] [ text "ACV Engine" ]
            ]
        , div [ class "menu-items" ]
            [ menuItem currentPage ActivitiesPage "fas fa-search" "Activities"
            , menuItem currentPage GraphPage "fas fa-project-diagram" "Graph"
            ]
        ]


menuItem : Page -> Page -> String -> String -> Html Msg
menuItem currentPage targetPage iconClass label =
    a
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