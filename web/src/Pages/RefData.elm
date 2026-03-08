module Pages.RefData exposing (Config, Model, Msg, init, update, view)

import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Models.Database exposing (ActivateResponse, DatabaseLoadStatus(..), activateResponseDecoder)
import Models.RefData exposing (RefDataList, RefDataStatus, refDataListDecoder)
import Route exposing (Route)
import Set exposing (Set)
import Shared exposing (RemoteData(..))
import Url
import View exposing (View)
import Views.ListActions as ListActions


type alias Config =
    { title : String
    , subtitle : String
    , apiPath : String -- e.g. "flow-synonyms"
    , countLabel : String -- e.g. "Synonym pairs"
    , formatHelp : List (Html Never) -- CSV format description shown on empty state
    , detailRoute : Maybe (String -> Route) -- route builder for clickable loaded items
    }


type alias Model =
    { items : RemoteData RefDataList
    , loadingItems : Set String
    , pendingAction : Maybe PendingAction
    , confirmingDelete : Maybe String
    , actionError : Maybe String
    }


type PendingAction
    = Unloading String
    | Deleting String


type Msg
    = ItemsLoaded (Result Http.Error RefDataList)
    | ActionResult (Result Http.Error ActivateResponse)
    | LoadItem String
    | UnloadItem String
    | ConfirmDelete String
    | CancelDelete
    | DeleteItem String
    | NavigateToDetail Route


init : Config -> ( Model, Effect Shared.Msg Msg )
init config =
    ( { items = Loading
      , loadingItems = Set.empty
      , pendingAction = Nothing
      , confirmingDelete = Nothing
      , actionError = Nothing
      }
    , Effect.fromCmd (fetchItems config)
    )


update : Config -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update config msg model =
    case msg of
        ItemsLoaded (Ok list) ->
            ( { model
                | items = Loaded list
                , loadingItems =
                    Set.filter
                        (\name ->
                            not (List.any (\m -> m.name == name && m.status /= Unloaded) list.items)
                        )
                        model.loadingItems
              }
            , Effect.none
            )

        ItemsLoaded (Err err) ->
            ( { model | items = Failed (Shared.httpErrorToString err) }, Effect.none )

        LoadItem name ->
            ( { model | loadingItems = Set.insert name model.loadingItems, actionError = Nothing }
            , Effect.fromCmd (loadItemCmd config name)
            )

        UnloadItem name ->
            ( { model | pendingAction = Just (Unloading name), actionError = Nothing }
            , Effect.fromCmd (unloadItemCmd config name)
            )

        ConfirmDelete name ->
            ( { model | confirmingDelete = Just name }, Effect.none )

        CancelDelete ->
            ( { model | confirmingDelete = Nothing }, Effect.none )

        DeleteItem name ->
            ( { model | pendingAction = Just (Deleting name), actionError = Nothing }
            , Effect.fromCmd (deleteItemCmd config name)
            )

        NavigateToDetail route ->
            ( model, Effect.fromShared (Shared.NavigateTo route) )

        ActionResult (Ok response) ->
            if response.success then
                ( { model | loadingItems = Set.empty, pendingAction = Nothing, confirmingDelete = Nothing, actionError = Nothing }
                , Effect.fromCmd (fetchItems config)
                )

            else
                ( { model | loadingItems = Set.empty, pendingAction = Nothing, confirmingDelete = Nothing, actionError = Just response.message }
                , Effect.fromCmd (fetchItems config)
                )

        ActionResult (Err err) ->
            ( { model | loadingItems = Set.empty, pendingAction = Nothing, confirmingDelete = Nothing, actionError = Just (Shared.httpErrorToString err) }
            , Effect.fromCmd (fetchItems config)
            )


view : Config -> Model -> View Msg
view config model =
    let
        error =
            case ( model.actionError, model.items ) of
                ( Just err, _ ) ->
                    Just err

                ( _, Failed err ) ->
                    Just err

                _ ->
                    Nothing

        unloadingName =
            case model.pendingAction of
                Just (Unloading name) ->
                    Just name

                _ ->
                    Nothing

        deletingName =
            case model.pendingAction of
                Just (Deleting name) ->
                    Just name

                _ ->
                    Nothing
    in
    { title = config.title
    , body =
        div [ class "databases-page" ]
            [ ListActions.viewPageHeader
                { title = config.title
                , subtitle = config.subtitle
                , addHref = "/" ++ config.apiPath ++ "/upload"
                , addLabel = "Upload CSV"
                }
                error
            , case model.items of
                Loaded list ->
                    viewItemsList config list model.confirmingDelete model.loadingItems unloadingName deletingName

                _ ->
                    ListActions.viewLoadingSpinner
            ]
    }


viewItemsList : Config -> RefDataList -> Maybe String -> Set String -> Maybe String -> Maybe String -> Html Msg
viewItemsList config list confirmingDelete loadingItems unloadingName deletingName =
    let
        itemCount =
            List.length list.items
    in
    div []
        [ div [ style "padding" "0.5rem 0" ]
            [ span [ class "tag is-info is-light" ]
                [ text (String.fromInt itemCount ++ " resource(s)") ]
            ]
        , if itemCount == 0 then
            div [ class "notification is-info is-light", style "margin-top" "1rem" ]
                (text "No resources configured. Upload a CSV file to get started."
                    :: List.map (Html.map never) config.formatHelp
                )

          else
            table [ class "table is-striped is-hoverable is-fullwidth" ]
                [ ListActions.viewTableHeader config.countLabel
                , tbody []
                    (List.map (viewItemRow config confirmingDelete loadingItems unloadingName deletingName) list.items)
                ]
        ]


viewItemRow : Config -> Maybe String -> Set String -> Maybe String -> Maybe String -> RefDataStatus -> Html Msg
viewItemRow config confirmingDelete loadingItems unloadingName deletingName item =
    let
        isLoading =
            Set.member item.name loadingItems

        isUnloading =
            unloadingName == Just item.name

        isDeleting =
            deletingName == Just item.name

        isLoaded =
            item.status /= Unloaded

        actions =
            div [ class "buttons are-small" ]
                (case item.status of
                    Unloaded ->
                        [ ListActions.viewOpenButton
                            { onOpen = LoadItem item.name, isLoading = isLoading }
                        ]
                            ++ (if item.isUploaded || item.isAuto then
                                    [ ListActions.viewDeleteConfirmation
                                        { onConfirm = ConfirmDelete item.name
                                        , onDelete = DeleteItem item.name
                                        , onCancel = CancelDelete
                                        , isConfirming = confirmingDelete == Just item.name
                                        , isDeleting = isDeleting
                                        }
                                    ]

                                else
                                    []
                               )

                    _ ->
                        [ ListActions.viewCloseButton
                            { onClose = UnloadItem item.name, isClosing = isUnloading }
                        ]
                )

        onNav =
            case config.detailRoute of
                Just toRoute ->
                    if isLoaded then
                        Just (NavigateToDetail (toRoute (Url.percentEncode item.name)))

                    else
                        Nothing

                Nothing ->
                    Nothing
    in
    ListActions.viewRow
        { status = item.status
        , actions = actions
        , displayName = item.displayName
        , description = item.description |> Maybe.withDefault ""
        , count = item.entryCount
        , isUploaded = item.isUploaded
        , isAuto = item.isAuto
        , format = "CSV"
        , onNavigate = onNav
        }



-- HTTP


fetchItems : Config -> Cmd Msg
fetchItems config =
    Http.get
        { url = "/api/v1/" ++ config.apiPath
        , expect = Http.expectJson ItemsLoaded refDataListDecoder
        }


loadItemCmd : Config -> String -> Cmd Msg
loadItemCmd config name =
    Http.post
        { url = "/api/v1/" ++ config.apiPath ++ "/" ++ Url.percentEncode name ++ "/load"
        , body = Http.emptyBody
        , expect = Http.expectJson ActionResult activateResponseDecoder
        }


unloadItemCmd : Config -> String -> Cmd Msg
unloadItemCmd config name =
    Http.post
        { url = "/api/v1/" ++ config.apiPath ++ "/" ++ Url.percentEncode name ++ "/unload"
        , body = Http.emptyBody
        , expect = Http.expectJson ActionResult activateResponseDecoder
        }


deleteItemCmd : Config -> String -> Cmd Msg
deleteItemCmd config name =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = "/api/v1/" ++ config.apiPath ++ "/" ++ Url.percentEncode name
        , body = Http.emptyBody
        , expect = Http.expectJson ActionResult activateResponseDecoder
        , timeout = Nothing
        , tracker = Nothing
        }
