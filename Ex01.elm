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

fullName : { given : String, surname : String }
  -> String
fullName name =
  name.given ++ " " ++ name.surname



-- Declaring some simple types: ----


-- Type called "Thing", that has a "constructor" or function
-- called "Thing". (They are separate namespaces; it's just
-- convention that the names are the same if it's just
-- a simple thing like this.)
type Thing = Thing

-- ^== AKA: (), or "Unit", from Elm's standard library.



-- Another simple type; an Email that has a String.
-- "Email" is a type, and "Email" is also a function that
-- is "String -> Email".
type Email = Email String

getEmail : Email -> String
getEmail (Email address) =
  address

-- Email is similar in structure (a thing with a String),
-- but Email and Title can't be confused.
type Title = Title String

-- eg.
-- > getEmail (Email "foo@bar.com")
-- "foo@bar.com" : String
-- > getEmail (Title "hello")
-- Kaboom! 💥






-- Type is "Color", and has two constructors / functions
-- called "Red" and "Green".
type Color =
    Red
  | Green

colorToString : Color -> String
colorToString col =
  case col of
    Red -> "red"
    Green -> "green"





-- Let's combine two unions (Color and Light):
type Light =
    LightOff
  | LightOn Color




-- Aliases
-- From the standard library:
type alias Time = Float

-- Can substitute any Time with Float and vice-versa, with
-- no error or warning. It's just a shortcut:
addThree : Float -> Time
addThree x = x + 3

seven : Float
seven = addThree 4


-- Same deal; Name is an alias for a record, and there's
-- no differentiation.

type alias Name = {
    given : String,
    surname : String
  }



-- eg.
type BlogPost = BlogPost {
    id : Int,
    title : String,
    at : Date
  }

type TalkSession = TalkSession {
    id : Int,
    title : String,
    at : Date
  }

whenIsTalk : TalkSession -> Date
whenIsTalk (TalkSession details) =
  details.at






-- Type Parameters
-- Something you declare as a placeholder; it's a slot
-- that something using the "Wrapped" type has to fill.
type Wrapped thing = Wrapped thing

-- eg.
wrapString : String -> Wrapped String
wrapString str =
  Wrapped str

unwrapString : Wrapped String -> String
unwrapString (Wrapped str) =
  str

wrapInt : Int -> Wrapped Int
wrapInt int =
  Wrapped int




-- Polymorphism
-- We're declaring a function that works for any type, and
-- representing that with a type variable called "a".
-- (Both "a"s must be the same type; like with wrapString
-- above.)
wrap : a -> Wrapped a
wrap thing =
  Wrapped thing

unwrap : Wrapped a -> a
unwrap (Wrapped thing) =
  thing

-- That "wrap" function is the same as the "Wrapped" constructor, eg.
-- > wrap
-- <function> : a -> Ex01.Wrapped a
-- > Wrapped
-- <function> : a -> Ex01.Wrapped a






type Optional a =
    None   -- Nothing
  | Some a -- Just

-- ^== AKA type Maybe a = ...
-- from the standard library.

some : a -> Optional a
some item = Some item


-- Implementing our own Maybe.map:
map : (from -> to) ->
  Optional from ->
  Optional to
map func opt =
  case opt of
    Some x ->
      Some (func x)
    None ->
      None








-- It's not real, but we can pretend it is:

--
-- type List a = ...
--





-- You can use type parameters in all sorts of things:

type Duration timeType = Duration {
    from : timeType,
    until : timeType
  }

-- eg.
daysInDuration : Duration Date -> Int
daysInDuration (Duration { from, until }) =
  7 -- TODO: Write answer here before running the talk.


-- Or using Duration:
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


-- eg.

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


-- ^== AKA Result, which looks like:
-- type Result error value =
--     Err error
--   | Ok value





-- Position

-- The right-hand side isn't just a straight "wrapping".
-- eg.

type Two a = Two a a
-- Or could be: type Two a = Two a a String Int Boolean
-- if we really want to; the left is just declaring the
-- type, not the internal structure.

twoEg : Two String
twoEg = Two "hello" "world"




-- Or more things with "strange" right-hand sides:
--
-- (You usually wouldn't write these things yourself in
-- UI code, but this is just showing some of the
-- wacky possibilities.)

type Serializer input =
  Serializer (input -> String)


stringSerializer : Serializer String
stringSerializer =
  Serializer (\str -> "\"" ++ str ++ "\"")
  -- We're not escaping quotes, but this is a toy.

intSerializer : Serializer Int
intSerializer =
  Serializer (\num -> toString num)

thingSerializer : Serializer { id : Int, title : String }
thingSerializer =
  Serializer (\thing ->
    String.join "," [
      runSerializer intSerializer thing.id,
      runSerializer stringSerializer thing.title
    ]
  )

runSerializer : Serializer a -> a -> String
runSerializer (Serializer func) input =
  func input

-- You could write something like Json.Encode like this,
-- to match Json.Decode's (... : Decoder a) type.
-- TODO: lifts, maps, andThens, all possible if you wanna.


-- And the flip-side:

type ParseError =
    BadCharacter String
  | BlahBlahBadThings

type Parser a =
  Parser
    (String -> Result ParseError (String, a))










-- Demo helpers
someDate : Date
someDate =
  fromOk (Date.fromString "2017-04-22 13:40:00")
