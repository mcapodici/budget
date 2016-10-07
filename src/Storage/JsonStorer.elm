module JsonStorer exposing (..)

import Storage.Storer as Storer exposing (Storer(..))
import Json.Decode exposing (Decoder, decodeString)
import Task exposing (Task, succeed, fail, andThen)

{-| Creates a storer of a type based on a Json encoder and decoder
and an underlying storer of string values.

--}
jsonStorer : Decoder a -> (a -> String) -> Storer String -> Storer a
jsonStorer decoder encoder (Storer baseStorer) =
  let
    decode json = case decodeString decoder json of
      Ok object -> succeed object
      Err err -> fail <| "Failed to decode: " ++ err in
  Storer {
    getItem = \key -> (baseStorer.getItem key) `andThen` decode,
    setItem = \key value -> baseStorer.setItem key (encoder value)
  }
