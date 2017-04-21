module Helpers exposing (..)

import Maybe exposing (Maybe(..))
import Result exposing (Result(..))
import Debug

fromJust : Maybe a -> a
fromJust m =
  case m of
    Just x -> x
    Nothing -> Debug.crash "Expected Just; got Nothing"

fromOk : Result x a -> a
fromOk r =
  case r of
    Ok x -> x
    Err _ -> Debug.crash "Expected Ok; got Err"
