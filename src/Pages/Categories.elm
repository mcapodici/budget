module Pages.Categories exposing (..)

import Data.Category exposing (Category)
import Data.Master exposing (Master)
import Data.Validation exposing (ErrorMessage)
import Guards exposing ((|=),(=>))
import Html exposing (Html, div, h1, input, text, button, span)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onInput, onClick)
import Table

type alias Model =
  { categories : List Category
  , tableState : Table.State
  , newCategoryName : String
  , errorMessage : String
  , message : String
  , unsaved : Bool
  }

updateFromMaster : Master -> Model -> Model
updateFromMaster master model =
  { model | categories = master.categories }

updateMaster : Model -> Master -> Master
updateMaster model master =
  { master | categories = model.categories }

init : Master -> Model
init master =
  { categories = master.categories
  , tableState = Table.initialSort "Name"
  , newCategoryName = ""
  , errorMessage = ""
  , message = ""
  , unsaved = False
  }

-- UPDATE

type Msg =
  SetTableState Table.State
  | SetNewCategoryName String
  | AddNewCategory
  | DeleteCategory String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({categories} as model) =
  let doNothing = (model, Cmd.none) in
  case msg of
    SetTableState newState ->
      ( { model | tableState = newState }
      , Cmd.none
      )
    SetNewCategoryName name ->
      ( { model | newCategoryName = name, errorMessage = "" }
      , Cmd.none
      )
    DeleteCategory name ->
      let newModel =
        { model |
            categories = List.filter (\acc -> acc.name /= name) categories,
            unsaved = True
        } in
      (newModel
      , Cmd.none
      )
    AddNewCategory ->
      let newCategory = {name = model.newCategoryName} in
      let validationResult = validateCategory newCategory categories in
      let newModel =
        case validationResult of
          Just errorMessage -> { model | errorMessage = errorMessage }
          Nothing ->
            { model |
                categories = newCategory :: categories,
                newCategoryName = "",
                unsaved = True
            } in
      ( newModel
      , Cmd.none )

validateCategory : Category -> List Category -> Maybe ErrorMessage
validateCategory category existingCategorys =
  category.name == ""
    => Just "Category Name Required"
  |= (List.any (\existingCategory -> existingCategory.name == category.name)) existingCategorys
    => Just "Category Name Already In Use"
  |= Nothing

-- VIEW

view : Model -> Html Msg
view ({ categories, tableState, errorMessage, message } as model) =
  div []
    [ h1 [] [ text "Categorys" ]
    , text message
    , addCategoryView model
    , span [ class "error" ] [ text errorMessage ]
    , Table.view config tableState categories
    ]

addCategoryView : Model -> Html Msg
addCategoryView { newCategoryName } =
  div []
    [ text "Add Category:"
    , input [ onInput SetNewCategoryName, value newCategoryName ] []
    , button [ onClick AddNewCategory ] [ text "Add" ]
    ]

deleteColumn : Table.Column Category Msg
deleteColumn = Table.veryCustomColumn
  { name = ""
  , viewData = (\category ->
    { attributes = []
    , children = [ button [ onClick <| DeleteCategory category.name ] [ text "Delete" ] ]
    })
  , sorter = Table.unsortable }


config : Table.Config Category Msg
config =
  Table.config
    { toId = .name
    , toMsg = SetTableState
    , columns =
        [ Table.stringColumn "Name" .name
        , deleteColumn
        ]
    }
