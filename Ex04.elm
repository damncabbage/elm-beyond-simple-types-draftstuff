-- fan tom
module Ex04 exposing (..)

import Email exposing (Email, EmailError, create)
import Email


-- Define a type; this is a simple wrapper over number. BUT, it's a bit weird:

type Temperature a = Temperature Float


-- And some types; only used for the type; the right-hand side is meaningless.
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

boiling : Temperature Fahrenheit
boiling =
  celsiusToFahrenheit (createCels 212)





-- Same idea:

type FormData value tag = FormData value
type Validated = Validated
type Unvalidated = Unvalidated

create : a -> FormData a Unvalidated
create a = FormData a

validateEmail :
  FormData String Unvalidated ->
  Result EmailError (FormData Email Validated)
validateEmail (FormData rawEmail) =
  Result.map
    (\email -> FormData email)
    (Email.create rawEmail)





-- State machine:

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

crash : Plane a -> Plane OnGround
crash (Plane x) = Plane x
