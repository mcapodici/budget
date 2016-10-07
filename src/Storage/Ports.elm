port module Storage.Ports exposing (..)

import Json.Encode exposing (Value)

port getStoredItem : String -> Cmd msg
port getStoredItemSub : (Value -> msg) -> Sub msg

port storeItem : (String, Value) -> Cmd msg
port storeItemSub : (Bool -> msg) -> Sub msg
