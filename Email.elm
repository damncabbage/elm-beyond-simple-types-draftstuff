module Email exposing
  ( Email
  , EmailError(..)
  , create
  , unwrap
  )

import Regex
import Regex exposing (regex)
import Result exposing (Result(..))


type Email = Email String


type EmailError =
    InvalidChars
  | NoDomain

unwrap : Email -> String
unwrap (Email address) =
  address

create : String -> Result EmailError Email
create str =
  let
    check err rx = boolToResult err (Regex.contains (regex rx))
  in
    (Ok str)
      |> Result.andThen (check InvalidChars "^[\\w@+_.-]+$")
      |> Result.andThen (check NoDomain "^[^@]+@[^@]+$")
      |> Result.andThen (\str -> Ok (Email str))


boolToResult : e -> (r -> Bool) -> r -> Result e r
boolToResult failure predicate val =
  if predicate val
    then Ok val
    else Err failure
