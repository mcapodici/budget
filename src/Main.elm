module Main exposing (main)

import Data.Master exposing (Master)
import Data.Notification exposing (Notification, NotificationSet, emptyNotificationSet, addNotification, removeNotification)
import Guards exposing ((|=),(=>))
import Html exposing (Html, div, h1, input, text, button, span)
import Html.App
import Html.Events exposing (onClick)
import Platform.Sub
import Storage.Storage as Storage
import Pages.Accounts
import Pages.Categories
import Process
import Task
import Basics.Extra exposing (never)
import Time exposing (second)

main : Program Never
main =
  Html.App.program
    { init = init master
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model =
  { currentTab : Tab
  , accountsModel : Pages.Accounts.Model
  , categoriesModel : Pages.Categories.Model
  , errorMessage : String
  , notifications : NotificationSet
  }

type Tab =
  Accounts
  | Categories
  | Budget
  | Transactions

toMaster : Model -> Master
toMaster model =
  { accounts = model.accountsModel.accounts
  , categories = model.categoriesModel.categories }

init : Master -> ( Model, Cmd Msg )
init master =
  let
    model =
      { currentTab = Accounts
      , accountsModel = Pages.Accounts.init master
      , categoriesModel = Pages.Categories.init master
      , errorMessage = ""
      , notifications = emptyNotificationSet
      }
  in
    ( model
    , Storage.getMaster )

-- UPDATE

type Msg =
  AccountsMsg Pages.Accounts.Msg
  | CategoriesMsg Pages.Categories.Msg
  | SaveComplete
  | LoadComplete Master
  | Error String
  | SetTab Tab
  | Save
  | RemoveNotification Notification

notify : Model -> String -> ( Model, Cmd Msg )
notify model msg =
  let (notifications, notification) = addNotification msg model.notifications in
  ( { model | notifications = notifications }
  , Task.perform (never) (always <| RemoveNotification notification ) (Process.sleep (5 * second)) )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    AccountsMsg msg ->
      let (newModel, command) = Pages.Accounts.update msg model.accountsModel in
        ({ model | accountsModel = newModel}, Cmd.map AccountsMsg command)
    CategoriesMsg msg ->
      let (newModel, command) = Pages.Categories.update msg model.categoriesModel in
        ({ model | categoriesModel = newModel}, Cmd.map CategoriesMsg command)
    Save ->
      ( model
      , Storage.setMaster (toMaster model))
    SaveComplete ->
      notify model "Your data has been saved"
    RemoveNotification n ->
      ( { model | notifications = removeNotification n model.notifications }
      , Cmd.none )
    LoadComplete master ->
      ( { model
          | accountsModel = Pages.Accounts.updateFromMaster master model.accountsModel
          , categoriesModel = Pages.Categories.updateFromMaster master model.categoriesModel }
      , Cmd.none )
    Error message ->
      ( { model | errorMessage = message }
      , Cmd.none )
    SetTab tab ->
      ( { model | currentTab = tab }
      , Cmd.none )

-- VIEW
view : Model -> Html Msg
view model =
  div []
  [
    div [] <| List.map (\note -> text note.message) model.notifications.notifications,
    div []
      [ button [ onClick <| Save ] [ text "Save" ] ],
    div []
      [ button [ onClick <| SetTab Accounts ] [ text "Accounts" ]
      , button [ onClick <| SetTab Categories ] [ text "Categories" ]
      , button [ onClick <| SetTab Budget ] [ text "Budget" ]
      , button [ onClick <| SetTab Transactions ] [ text "Transactions" ]
      ],
    div [] [tabView model]
  ]

tabView : Model -> Html Msg
tabView model =
  case model.currentTab of
    Accounts ->
      Html.App.map AccountsMsg <| Pages.Accounts.view model.accountsModel
    Categories ->
      Html.App.map CategoriesMsg <| Pages.Categories.view model.categoriesModel
    Budget ->
      text "TODO: BUDGET"
    Transactions ->
      text "TODO: TXNS"

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Platform.Sub.batch [
  Storage.setMasterSub (\val ->
    val => SaveComplete
    |= Error "Save failed"),
  Storage.getMasterSub (\val ->
    case val of
      Ok master -> LoadComplete master
      Err message -> Error message
    )
  ]

-- INITIAL DATA

master : Master
master = {
    accounts = [],
    categories = []
  }
