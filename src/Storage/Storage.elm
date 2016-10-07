module Storage.Storage exposing (..)

import Data.Master exposing (Master)
import Storage.Keys
import Storage.Ports
import Json.Decode

{-| Requests to get the master data record from storage
--}
getMaster : Cmd msg
getMaster =
  Storage.Ports.getStoredItem Storage.Keys.master

{-| Subsription for the retrieved master data record
--}
getMasterSub : (Result String Master -> msg) -> Sub msg
getMasterSub map =
    Storage.Ports.getStoredItemSub (Json.Decode.decodeValue Data.Master.decode >> map)

{-| Requests to store the master record into storage
--}
setMaster : Master -> Cmd msg
setMaster master =
  Storage.Ports.storeItem (Storage.Keys.master, Data.Master.encode master)

{-| Subscription for the success of storing the master recor]]

--}
setMasterSub : (Bool -> msg) -> Sub msg
setMasterSub =
  Storage.Ports.storeItemSub


{--

port getStoredItem : String -> Cmd msg
port getStoredItemSub : (Value -> msg) -> Sub msg

port storeItem : (String, Value) -> Cmd msg
port storeItemSub : (Bool -> msg) -> Sub msg

--}
