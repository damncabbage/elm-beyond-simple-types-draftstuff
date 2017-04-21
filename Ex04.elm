module ExNNGhostStories exposing (..)

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






--console.log(Temperature.unwrap(boiling)); // => 100



{-
class OnGround {}
class Taxiing {}
class InAir {}
class Landing {}
type PlaneState = OnGround | Taxiing | InAir | Landing;

class Plane<S: PlaneState> extends TypeWrapper<{flightCode: string}> {}

function taxi(p: Plane<OnGround>): Plane<Taxiing> { return Plane.wrap(Plane.unwrap(p)); }
function takeOff(p: Plane<Taxiing>): Plane<InAir> { return Plane.wrap(Plane.unwrap(p)); }
function approach(p: Plane<InAir>): Plane<Landing> { return Plane.wrap(Plane.unwrap(p)); }
function crash<T: PlaneState>(p: Plane<T>): Plane<OnGround> { return Plane.wrap(Plane.unwrap(p)); }
-}

{-
class FormData<V,T> extends TypeWrapper<V> {}
class Validated {}
class Unvalidated {}

function validateEmail(
  x: FormData<string,Unvalidated>
): (FormData<string,Validated> | null) {
  const email = FormData.unwrap(x);
  if (email.match(/@/)) {
    return FormData.wrap(email);
  }
  return null;
}
-}
