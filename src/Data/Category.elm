module Data.Category exposing (Category, encode, decode)

import Json.Encode exposing (string)
import Json.Decode exposing ((:=))

type alias Category =
  { name : String }

encode : Category -> Json.Encode.Value
encode a = Json.Encode.object [
  ("name", string a.name)]

decode : Json.Decode.Decoder Category
decode =
    Json.Decode.object1 (\name -> { name = name })
      ("name" := Json.Decode.string)
