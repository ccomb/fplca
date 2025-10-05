module Views.LeftMenu exposing (viewLeftMenu)

import Html exposing (Html, div, nav, a, span, i, text)
import Html.Attributes exposing (class, classList, href)
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
            [ menuItem currentPage ActivitiesPage "fas fa-search" "Activities" "/#activities"
            , menuItem currentPage GraphPage "fas fa-project-diagram" "Graph" ("/#activity/" ++ currentActivityId ++ "/tree")
            ]
        ]


menuItem : Page -> Page -> String -> String -> String -> Html Msg
menuItem currentPage targetPage iconClass label url =
    a
        [ classList
            [ ( "menu-item", True )
            , ( "is-active", currentPage == targetPage )
            ]
        , href url
        , onClick targetPage
        ]
        [ span [ class "icon" ]
            [ i [ class iconClass ] []
            ]
        , span [ class "menu-label" ] [ text label ]
        ]