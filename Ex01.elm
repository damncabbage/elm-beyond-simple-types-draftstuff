module Ex01 exposing (..)

import Helpers exposing (..)
import Regex
import Regex exposing (regex)
import Result
import Result exposing (Result(..))
import Date exposing (Date)
import Time exposing (Time)






---- Using some in-built types: ----

greet : String -> String
greet who =
  "Hello, " ++ who ++ "!"

addFive : Int -> Int
addFive n =
  n + 5

isRed : String -> Bool
isRed x =
  x == "Red"

fullName : { given : String, surname : String } -> String
fullName name =
  name.given ++ " " ++ name.surname



-- Declaring some simple types: ----


type Thing = Thing

-- AKA: (), or "Unit"





type Email = Email String

getEmail : Email -> String
getEmail (Email address) =
  address





type Title = Title String






type Color =
    Red
  | Green

colorToString : Color -> String
colorToString col =
  case col of
    Red -> "red"
    Green -> "green"






type Light =
    LightOff
  | LightOn Color





type alias Name = {
    given : String,
    surname : String
  }





type BlogPost = BlogPost {
    id : String,
    title : String,
    at : Date
  }

type TalkSession = TalkSession {
    id : String,
    title : String, 
    at : Date
  }

whenIsTalk : TalkSession -> Date
whenIsTalk (TalkSession details) =
  details.at






-- tie per parra metres

type Wrapped thing = Wrapped thing


wrapString : String -> Wrapped String
wrapString str =
  Wrapped str

unwrapString : Wrapped String -> String
unwrapString (Wrapped str) =
  str

wrapInt : Int -> Wrapped Int
wrapInt int =
  Wrapped int




-- polly more fizz 'em

wrap : a -> Wrapped a
wrap thing =
  Wrapped thing

unwrap : Wrapped a -> a
unwrap (Wrapped thing) =
  thing





type Optional a =
    None
  | Some a


some : a -> Optional a
some item = Some item



map : (from -> to) -> Optional from -> Optional to
map func opt =
  case opt of
    Some x ->
      Some (func x)
    None ->
      None





---- It's not real, but we can pretend it is: ----
-- type List a = ...





type Duration timeType = Duration {
    from : timeType,
    until : timeType
  }

-- eg.
daysInDuration : Duration Date -> Int
daysInDuration (Duration { from, until }) =
  7 -- TODO: Write answer here before running the talk.



type Class = Class {
    subject : String,
    dailyAt : Duration Time
  }

classLengthInHours : Class -> Int
classLengthInHours (Class { dailyAt }) =
  7 -- TODO: Definitely don't leave this here either.







-- Multiple variables!

type Either a b =
    Left a
  | Right b







type EmailError =
    InvalidChars
  | NoDomain

validateEmailString : String -> Either EmailError String
validateEmailString str =
  case Regex.contains (regex "^[\\w@+_.-]+$") str of
    False ->
      Left InvalidChars
    True ->
      case Regex.contains (regex "^[^@]+@[^@]+$") str of
        False ->
          Left NoDomain
        True ->
          Right str








validateEmailString2 : String -> Result EmailError String
validateEmailString2 str =
  let
    check err rx = boolToResult err (Regex.contains (regex rx))
  in
    (Ok str)
      |> Result.andThen (check InvalidChars "^[\\w@+_.-]+$")
      |> Result.andThen (check NoDomain "^[^@]+@[^@]+$")

boolToResult : e -> (r -> Bool) -> r -> Result e r
boolToResult failure predicate val =
  if predicate val
    then Ok val
    else Err failure











-- helpers
someDate : Date
someDate =
  fromOk (Date.fromString "2017-04-22 13:40:00")
