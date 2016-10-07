port module Storage.Ports exposing (..)

import Json.Encode exposing (Value)

port getStoredItem : String -> Cmd msg
port storageData : (Value -> msg) -> Sub msg

port setStoredItem : (String, Value) -> Cmd msg
port storageNotification : (Bool -> msg) -> Sub msg
