module Data.Master exposing (Master, encode, decode)

import Data.Account exposing (Account)
import Data.Category exposing (Category)
import Json.Encode exposing (..)
import Json.Decode exposing ((:=))

type alias Master =
  { accounts : List Account
  , categories : List Category
  }

encode : Master -> Json.Encode.Value
encode m =
  Json.Encode.object
    [ ("accounts", list <| List.map Data.Account.encode m.accounts)
    , ("categories", list <| List.map Data.Category.encode m.categories) ]

decode : Json.Decode.Decoder Master
decode =
    Json.Decode.object2 (\accs cats -> { accounts = accs, categories = cats })
      ("accounts" := (Json.Decode.list Data.Account.decode))
      ("categories" := (Json.Decode.list Data.Category.decode))
