module Data.Master exposing (..)

import Data.Account exposing (..)
import Json.Encode exposing (..)
import Json.Decode exposing ((:=))

type alias Master = {
  accounts : List Account
}

encode : Master -> Json.Encode.Value
encode m = Json.Encode.object [
  ("accounts", list <| List.map Data.Account.encode m.accounts)]

decode : Json.Decode.Decoder Master
decode =
    Json.Decode.object1 (\a -> { accounts = a })
      ("accounts" := (Json.Decode.list Data.Account.decode))
