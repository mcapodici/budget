module Main exposing (main)

import Data.Account exposing (Account)
import Data.Master exposing (Master)
import Data.Validation exposing (ErrorMessage)
import Guards exposing ((|=),(=>))
import Html exposing (Html, div, h1, input, text, button, span)
import Html.App as App
import Html.Attributes exposing (class, value)
import Html.Events exposing (onInput, onClick)
import Platform.Sub
import Storage.Storage as Storage
import Table

main : Program Never
main =
  App.program
    { init = init master
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model =
  { master : Master
  , tableState : Table.State
  , newAccountName : String
  , errorMessage : String
  , message : String
  }

init : Master -> ( Model, Cmd Msg )
init master =
  let
    model =
      { master = master
      , tableState = Table.initialSort "Name"
      , newAccountName = ""
      , errorMessage = ""
      , message = ""
      }
  in
    ( model
    , Storage.getMaster )

-- UPDATE

type Msg =
  SetTableState Table.State
  | SetNewAccountName String
  | AddNewAccount
  | Save
  | SaveComplete
  | LoadComplete Master
  | Error String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({master} as model) =
  case msg of
    SetTableState newState ->
      ( { model | tableState = newState }
      , Cmd.none
      )
    SetNewAccountName name ->
      ( { model | newAccountName = name, errorMessage = "" }
      , Cmd.none
      )
    AddNewAccount ->
      let newAccount = {name = model.newAccountName} in
      let validationResult = validateAccount newAccount master.accounts in
      let newModel =
        case validationResult of
          Just errorMessage -> { model | errorMessage = errorMessage }
          Nothing ->
            { model | master =
               { master | accounts =
                  newAccount :: master.accounts
               },
               newAccountName = ""
            } in
      ( newModel
      , case validationResult of
          Just _ -> Cmd.none
          Nothing -> Storage.setMaster newModel.master )
    Save ->
      ( model
      , Storage.setMaster master)
    SaveComplete ->
      ( { model | message = "Successfully Saved" }
      , Cmd.none )
    LoadComplete result ->
      ( { model | master = result }
      , Cmd.none )
    Error message ->
      ( { model | message = "Error: " ++ message }
      , Cmd.none )

validateAccount : Account -> List Account -> Maybe ErrorMessage
validateAccount account existingAccounts =
  account.name == ""
    => Just "Account Name Required"
  |= (List.any (\existingAccount -> existingAccount.name == account.name)) existingAccounts
    => Just "Account Name Already In Use"
  |= Nothing

-- VIEW

view : Model -> Html Msg
view ({ master, tableState, errorMessage, message } as model) =
  div []
    [ h1 [] [ text "Accounts" ]
    , text message
    , addAccountView model
    , span [ class "error" ] [ text errorMessage ]
    , Table.view config tableState master.accounts
    , button [ onClick Save ] [ text "Save" ]
    ]

addAccountView : Model -> Html Msg
addAccountView { newAccountName } =
  div []
    [ text "Add Account:"
    , input [ onInput SetNewAccountName, value newAccountName ] []
    , button [ onClick AddNewAccount ] [ text "Add" ]
    ]


config : Table.Config Account Msg
config =
  Table.config
    { toId = .name
    , toMsg = SetTableState
    , columns =
        [ Table.stringColumn "Name" .name
        ]
    }

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
