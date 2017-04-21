-- GNU Tie Pez
module Ex02 exposing (..)

import Regex
import Regex exposing (regex)
import Result exposing (Result(..))


-- Revisiting something familiar:

type Email = Email String


unwrapEmail : Email -> String
unwrapEmail (Email address) =
  address


type EmailError =
    InvalidChars
  | NoDomain

createEmail : String -> Result EmailError Email
createEmail str =
  let
    check err rx val =
      if (Regex.contains (regex rx) val) then Ok val else Err err
  in
    (Ok str)
      |> Result.andThen (check InvalidChars "^[\\w@+_.-]+$")
      |> Result.andThen (check NoDomain "^[^@]+@[^@]+$")
      |> Result.andThen (\str -> Ok (Email str))


type alias User1 = {
    id : String,
    name : String,
    email : String
  }

type alias User2 = {
    id : UUID,
    name : String,
    email : Email
  }


