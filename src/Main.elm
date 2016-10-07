import Html exposing (Html, div, h1, input, text, button, span)
import Html.Attributes exposing (class, value)
import Html.App as App
import Html.Events exposing (onInput, onClick)
import Table
import Guards exposing ((|=),(=>))
import Data.Account exposing (Account)
import Data.Validation exposing (ErrorMessage)

main : Program Never
main =
  App.program
    { init = init accounts
    , update = update
    , view = view
    , subscriptions = \_ -> Sub.none
    }

-- MODEL

type alias Model =
  { accounts : List Account
  , tableState : Table.State
  , newAccountName : String
  , errorMessage : String
  }

init : List Account -> ( Model, Cmd Msg )
init accounts =
  let
    model =
      { accounts = accounts
      , tableState = Table.initialSort "Name"
      , newAccountName = ""
      , errorMessage = ""
      }
  in
    ( model, Cmd.none )

-- UPDATE

type Msg =
  SetTableState Table.State
  | SetNewAccountName String
  | AddNewAccount

update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({accounts} as model) =
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
      let newModel =
        case validateAccount newAccount accounts of
          Just errorMessage -> { model | errorMessage = errorMessage }
          Nothing -> { model | accounts = newAccount :: accounts, newAccountName = "" } in
      ( newModel, Cmd.none )

validateAccount : Account -> List Account -> Maybe ErrorMessage
validateAccount account existingAccounts =
  account.name == ""
    => Just "Account Name Required"
  |= (List.any (\existingAccount -> existingAccount.name == account.name)) existingAccounts
    => Just "Account Name Already In Use"
  |= Nothing

-- VIEW

view : Model -> Html Msg
view ({ accounts, tableState, errorMessage } as model) =
  div []
    [ h1 [] [ text "Accounts" ]
    , addAccountView model
    , span [ class "error" ] [ text errorMessage ]
    , Table.view config tableState accounts
    ]

addAccountView : Model -> Html Msg
addAccountView { accounts, newAccountName } =
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

-- INITIAL DATA : TODO : GET FROM STORE!

accounts : List Account
accounts =
  [ Account "Credit Card",
    Account "Savings"
  ]
