---- Phantom Types ----
module Ex04 exposing (..)

import Email exposing (Email, EmailError, create)
import Email


-- Define a type; this is a simple wrapper over number. BUT, it's a bit weird:

type Temperature a = Temperature Float

-- And some types. Only used for the type;
-- the "a" param on the left side of
-- Temperature definition is only for differentiating
-- by type.
-- All the same thing at run-time (wrapper over Float),
-- after type-checking.
type Celsius = Celsius
type Fahrenheit = Fahrenheit


celsiusToFahrenheit :
  Temperature Celsius ->
  Temperature Fahrenheit
celsiusToFahrenheit (Temperature num) =
  Temperature ((num - 32) / 1.8)

createFahr : Float -> Temperature Fahrenheit
createFahr num =
  Temperature num

createCels : Float -> Temperature Celsius
createCels num =
  Temperature num

boilingFahr : Temperature Fahrenheit
boilingFahr =
  celsiusToFahrenheit (createCels 212)
-- Can't do:
-- > celsiusToFahrenheit boilingFahr
-- (Kaboom! Type error. You need to explicitly
-- convert Fahrenheit to Celsius before being
-- able to use it with that function.)




-- Same idea:

type FormData value tag = FormData value
type Validated = Validated
type Unvalidated = Unvalidated

-- Export only the type and these functions; can't
-- create a FormData value that is Validated without
-- using them.

create : a -> FormData a Unvalidated
create a = FormData a

validateEmail :
  FormData String Unvalidated ->
  Result EmailError (FormData Email Validated)
validateEmail (FormData rawEmail) =
  Result.map
    (\email -> FormData email)
    (Email.create rawEmail)





---- A state machine, with types ----

type Plane state = Plane { flightCode : String }

type OnGround = OnGround
type Taxiing = Taxiing
type InAir = InAir
type Landing = Landing

taxi : Plane OnGround -> Plane Taxiing
taxi (Plane x) = Plane x

takeOff : Plane Taxiing -> Plane InAir
takeOff (Plane x) = Plane x

approach : Plane InAir -> Plane Landing
approach (Plane x) = Plane x

-- a little morbid
crash : Plane a -> Plane OnGround
crash (Plane x) = Plane x
