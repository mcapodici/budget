module Data.Notification exposing (..)

import Time exposing (Time)

type alias Notification =
  { message : String
  , expiry : Time
  }
