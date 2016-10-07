module Storage.Storer exposing (..)

import Task exposing (Task)

{--| Defines an asyncronous key/value store of data
--}
type Storer value = Storer {
  getItem : String -> Task String value,
  setItem : String -> value -> Task String ()
}
