module Main exposing (main)

import Data.Master exposing (Master)
import Guards exposing ((|=),(=>))
import Html exposing (Html, div, h1, input, text, button, span)
import Html.App
import Html.Events exposing (onClick)
import Platform.Sub
import Storage.Storage as Storage
import Pages.Accounts

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
  { currentTab : Tab,
    accountsModel : Pages.Accounts.Model,
    errorMessage : String
  }

type Tab =
  Accounts
  | Budget
  | Transactions

toMaster : Model -> Master
toMaster model = { accounts = model.accountsModel.accounts }

init : Master -> ( Model, Cmd Msg )
init master =
  let
    model =
      { currentTab = Accounts
      , accountsModel = Pages.Accounts.init master
      , errorMessage = ""
      }
  in
    ( model
    , Storage.getMaster )

-- UPDATE

type Msg =
  AccountsMsg Pages.Accounts.Msg
  | SaveComplete
  | LoadComplete Master
  | Error String
  | SetTab Tab

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    AccountsMsg msg ->
      -- TODO AUTOSAVE!
      let (newModel, command) = Pages.Accounts.update msg model.accountsModel in
      let commands =
        newModel.unsaved => [Storage.setMaster <| Pages.Accounts.updateMaster newModel (toMaster model) ]
        |= [] in
      let commands2 =
        commands ++
        [Cmd.map AccountsMsg command] in
        ({ model | accountsModel = { newModel | unsaved = False }}, Cmd.batch commands2)
    SaveComplete ->
      ( model
      , Cmd.none )
    LoadComplete master ->
      ( { model | accountsModel = Pages.Accounts.updateFromMaster master model.accountsModel }
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
    div []
      [ button [ onClick <| SetTab Accounts ] [ text "Accounts" ]
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
    accounts = []
  }
