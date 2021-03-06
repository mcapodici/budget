module Data.Account exposing (Account, encode, decode)

import Json.Encode exposing (string)
import Json.Decode exposing ((:=))

type alias Account =
  { name : String }


encode : Account -> Json.Encode.Value
encode a = Json.Encode.object [
  ("name", string a.name)]

decode : Json.Decode.Decoder Account
decode =
    Json.Decode.object1 (\name -> { name = name })
      ("name" := Json.Decode.string)
