module Views.ListActions exposing (ListRow, viewCloseButton, viewDeleteConfirmation, viewLoadingSpinner, viewOpenButton, viewPageHeader, viewRow, viewTableHeader)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, stopPropagationOn)
import Json.Decode as Decode
import Models.Database exposing (DatabaseLoadStatus(..))


type alias ListRow msg =
    { status : DatabaseLoadStatus
    , actions : Html msg
    , displayName : String
    , description : String
    , count : Int
    , isUploaded : Bool
    , format : String
    , onNavigate : Maybe msg
    }


viewTableHeader : String -> Html msg
viewTableHeader countTitle =
    thead []
        [ tr []
            [ th [ style "width" "50px" ] [ text "" ]
            , th [ style "width" "200px" ] [ text "Actions" ]
            , th [] [ text "Name" ]
            , th [] [ text "Description" ]
            , th [] [ text countTitle ]
            , th [ style "width" "100px" ] [ text "Source" ]
            , th [ style "width" "120px" ] [ text "Format" ]
            ]
        ]


viewRow : ListRow msg -> Html msg
viewRow row =
    let
        statusIndicator =
            case row.status of
                DbLoaded ->
                    span [ class "has-text-success", style "font-size" "1.5rem" ] [ text "●" ]

                PartiallyLinked ->
                    span [ class "has-text-warning", style "font-size" "1.5rem" ] [ text "●" ]

                Unloaded ->
                    span [ class "has-text-grey-lighter", style "font-size" "1.5rem" ] [ text "○" ]

        rowAttrs =
            case row.onNavigate of
                Just msg ->
                    [ class "is-clickable", style "cursor" "pointer", onClick msg ]

                Nothing ->
                    []
    in
    tr rowAttrs
        [ td [ style "text-align" "center", style "vertical-align" "middle" ]
            [ statusIndicator ]
        , td [] [ row.actions ]
        , td [] [ text row.displayName ]
        , td [ class "has-text-grey" ] [ text row.description ]
        , td [ class "has-text-grey" ]
            [ if row.count > 0 then
                text (String.fromInt row.count)

              else
                text ""
            ]
        , td [ class "has-text-grey" ]
            [ if row.isUploaded then
                text "Uploaded"

              else
                text "Preinstalled"
            ]
        , td [ class "has-text-grey" ] [ text row.format ]
        ]


viewOpenButton :
    { onOpen : msg, isLoading : Bool }
    -> Html msg
viewOpenButton config =
    button
        [ class
            ("button is-primary is-small"
                ++ (if config.isLoading then
                        " is-loading"

                    else
                        ""
                   )
            )
        , disabled config.isLoading
        , stopPropagationOn "click" (Decode.succeed ( config.onOpen, True ))
        ]
        [ span [ class "icon is-small" ] [ i [ class "fas fa-folder-open" ] [] ]
        , span [] [ text "Open" ]
        ]


viewDeleteConfirmation :
    { onConfirm : msg, onDelete : msg, onCancel : msg
    , isConfirming : Bool, isDeleting : Bool
    }
    -> Html msg
viewDeleteConfirmation config =
    if config.isConfirming then
        span [ class "buttons are-small", style "display" "inline-flex" ]
            [ span [ class "has-text-danger", style "font-weight" "bold", style "margin-right" "0.25rem", style "line-height" "2em" ] [ text "Delete?" ]
            , button
                [ class
                    ("button is-danger is-small"
                        ++ (if config.isDeleting then
                                " is-loading"

                            else
                                ""
                           )
                    )
                , disabled config.isDeleting
                , stopPropagationOn "click" (Decode.succeed ( config.onDelete, True ))
                ]
                [ text "Yes" ]
            , button
                [ class "button is-light is-small"
                , disabled config.isDeleting
                , stopPropagationOn "click" (Decode.succeed ( config.onCancel, True ))
                ]
                [ text "No" ]
            ]

    else
        button
            [ class "button is-danger is-small is-outlined"
            , stopPropagationOn "click" (Decode.succeed ( config.onConfirm, True ))
            ]
            [ span [ class "icon is-small" ] [ i [ class "fas fa-trash" ] [] ]
            ]


viewPageHeader :
    { title : String, subtitle : String, addHref : String, addLabel : String }
    -> Maybe String
    -> Html msg
viewPageHeader config error =
    div [ class "box" ]
        [ div [ class "level" ]
            [ div [ class "level-left" ]
                [ div [ class "level-item" ]
                    [ h2 [ class "title is-3", style "margin-bottom" "0" ] [ text config.title ]
                    ]
                ]
            , div [ class "level-right" ]
                [ div [ class "level-item" ]
                    [ a [ href config.addHref, class "button is-primary" ]
                        [ span [ class "icon" ] [ i [ class "fas fa-plus" ] [] ]
                        , span [] [ text config.addLabel ]
                        ]
                    ]
                ]
            ]
        , p [ class "subtitle" ] [ text config.subtitle ]
        , case error of
            Just err ->
                div [ class "notification is-danger" ] [ text err ]

            Nothing ->
                text ""
        ]


viewCloseButton : { onClose : msg, isClosing : Bool } -> Html msg
viewCloseButton config =
    button
        [ class
            ("button is-warning is-small"
                ++ (if config.isClosing then
                        " is-loading"

                    else
                        ""
                   )
            )
        , Html.Attributes.disabled config.isClosing
        , stopPropagationOn "click" (Decode.succeed ( config.onClose, True ))
        ]
        [ span [ class "icon is-small" ] [ i [ class "fas fa-times" ] [] ]
        , span [] [ text "Close" ]
        ]


viewLoadingSpinner : Html msg
viewLoadingSpinner =
    div [ class "has-text-centered", style "padding" "2rem" ]
        [ span [ class "icon is-large has-text-primary" ]
            [ i [ class "fas fa-spinner fa-spin fa-2x" ] [] ]
        ]
