module JsonStorer exposing (..)

import Json.Decode exposing (Decoder, decodeValue)
import Json.Encode exposing (Value)
import Storage.Storer as Storer exposing (Storer(..))
import Task exposing (Task, succeed, fail, andThen)

{-| Creates a storer of a type based on a Json encoder and decoder
and an underlying storer of Json values.
--}
jsonStorer : Decoder a -> (a -> Value) -> Storer Value -> Storer a
jsonStorer decoder encoder (Storer baseStorer) =
  let
    decode json = case decodeValue decoder json of
      Ok object -> succeed object
      Err err -> fail <| "Failed to decode: " ++ err in
  Storer {
    getItem = \key -> (baseStorer.getItem key) `andThen` decode,
    setItem = \key value -> baseStorer.setItem key (encoder value)
  }
