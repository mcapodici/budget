module Pages.Accounts exposing (..)

import Data.Account exposing (Account)
import Data.Master exposing (Master)
import Data.Validation exposing (ErrorMessage)
import Guards exposing ((|=),(=>))
import Html exposing (Html, div, h1, input, text, button, span)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onInput, onClick)
import Table

type alias Model =
  { accounts : List Account
  , tableState : Table.State
  , newAccountName : String
  , errorMessage : String
  , message : String
  }

updateFromMaster : Master -> Model -> Model
updateFromMaster master model =
  { model | accounts = master.accounts }

updateMaster : Model -> Master -> Master
updateMaster model master =
  { master | accounts = model.accounts }

init : Master -> Model
init master =
  { accounts = master.accounts
  , tableState = Table.initialSort "Name"
  , newAccountName = ""
  , errorMessage = ""
  , message = ""
  }

-- UPDATE

type Msg =
  SetTableState Table.State
  | SetNewAccountName String
  | AddNewAccount
  | DeleteAccount String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({accounts} as model) =
  let doNothing = (model, Cmd.none) in
  case msg of
    SetTableState newState ->
      ( { model | tableState = newState }
      , Cmd.none
      )
    SetNewAccountName name ->
      ( { model | newAccountName = name, errorMessage = "" }
      , Cmd.none
      )
    DeleteAccount name ->
      let newModel =
        { model |
            accounts = List.filter (\acc -> acc.name /= name) accounts
        } in
      (newModel
      , Cmd.none
      )
    AddNewAccount ->
      let newAccount = {name = model.newAccountName} in
      let validationResult = validateAccount newAccount accounts in
      let newModel =
        case validationResult of
          Just errorMessage -> { model | errorMessage = errorMessage }
          Nothing ->
            { model |
                accounts = newAccount :: accounts,
                newAccountName = ""
            } in
      ( newModel
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
view ({ accounts, tableState, errorMessage, message } as model) =
  div []
    [ h1 [] [ text "Accounts" ]
    , text message
    , addAccountView model
    , span [ class "error" ] [ text errorMessage ]
    , Table.view config tableState accounts
    ]

addAccountView : Model -> Html Msg
addAccountView { newAccountName } =
  div []
    [ text "Add Account:"
    , input [ onInput SetNewAccountName, value newAccountName ] []
    , button [ onClick AddNewAccount ] [ text "Add" ]
    ]

deleteColumn : Table.Column Account Msg
deleteColumn = Table.veryCustomColumn
  { name = ""
  , viewData = (\account ->
    { attributes = []
    , children = [ button [ onClick <| DeleteAccount account.name ] [ text "Delete" ] ]
    })
  , sorter = Table.unsortable }


config : Table.Config Account Msg
config =
  Table.config
    { toId = .name
    , toMsg = SetTableState
    , columns =
        [ Table.stringColumn "Name" .name
        , deleteColumn
        ]
    }
