module Views.ActivityHeader exposing (viewActivityHeader)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models.Activity exposing (ActivityInfo)
import Utils.Format as Format


viewActivityHeader : ActivityInfo -> String -> msg -> Html msg
viewActivityHeader activityInfo pageTitle backMsg =
    div [ class "box", style "margin-bottom" "0" ]
        [ div [ class "level", style "margin-bottom" "0" ]
            [ div [ class "level-left" ]
                [ div [ class "level-item" ]
                    [ button
                        [ class "button is-primary"
                        , onClick backMsg
                        ]
                        [ span [ class "icon" ] [ Html.i [ class "fas fa-arrow-left" ] [] ]
                        , span [] [ text "Back" ]
                        ]
                    ]
                , div [ class "level-item" ]
                    [ h1 [ class "title is-4", style "margin-bottom" "0" ]
                        (case activityInfo.referenceProduct of
                            Just product ->
                                let
                                    productText =
                                        case ( activityInfo.referenceProductAmount, activityInfo.referenceProductUnit ) of
                                            ( Just amount, Just unit ) ->
                                                Format.formatScientific amount ++ " " ++ unit ++ " " ++ product

                                            _ ->
                                                product
                                in
                                [ text activityInfo.name
                                , span [ style "color" "#888", style "margin" "0 0.5rem" ] [ text "\u{2192}" ]
                                , span [ style "font-weight" "normal" ] [ text productText ]
                                ]

                            Nothing ->
                                [ text activityInfo.name ]
                        )
                    ]
                , div [ class "level-item" ]
                    [ span [ class "tag is-light" ] [ text activityInfo.location ] ]
                ]
            ]
        , if not (List.isEmpty activityInfo.description) then
            div [ style "font-size" "0.85rem", style "line-height" "1.4", style "margin-top" "0.5rem" ]
                (activityInfo.description |> List.map (\para -> p [ style "margin-bottom" "0.25rem" ] [ text para ]))

          else
            text ""
        , h2 [ class "title is-5", style "margin-top" "1rem", style "margin-bottom" "0" ] [ text pageTitle ]
        ]
