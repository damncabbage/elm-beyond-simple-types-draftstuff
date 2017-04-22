-- "Newtypes", "Type Wrappers"
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

-- it's a Result now
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




-- Mixup and Separation

type alias User1 = {
    id : String,
    name : String,
    email : String
  }

type UUID = UUID String
type alias User2 = {
    id : UUID,
    name : String,
    email : Email
  }

-- User1 has a bunch of Strings, which can be confused.
-- These have context in the form of record fields, though,
-- so it'e not super-likely we're going to mix them up.
-- Unless...

emailFromUser1 : User1 -> String
emailFromUser1 user =
  user.email
  -- ^== Once we've extracted the email from the record,
  -- it's just a String. Which can be passed along
  -- accidentally instead of some other String, and
  -- the compiler won't care.

emailFromUser2 : User2 -> Email
emailFromUser2 user =
  user.email
  -- ^== It's still an Email, even after being extracted.
  -- It won't get mixed up with any other type (eg. String).
