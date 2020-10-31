module Number exposing (parser, Number, add, subtract, multiply, divide, toString)

import Basics exposing (Bool, Int)
import Parser exposing (Parser, (|.), (|=), succeed, float, oneOf, map, symbol, andThen)
import Flip exposing (flip)
import Svg.Attributes exposing (exponent)
import Parser exposing (number)
import String exposing (fromFloat)

type alias Significand = Int
type alias Exponent = Int
type alias Number = (Significand, Exponent)

normalize: Number -> Number
normalize (s, e) =
  if (s == 0 || e == 0 || remainderBy 10 s /= 0)
  then (s, e)
  else normalize (s // 10, e - 1)

toString: Number -> String
toString (s, e) =
  if e == 0
  then s |> String.fromInt
  else
    let numeric_string = s |> String.fromInt |> String.padLeft -e '0' in
    let characteristic = numeric_string |> String.slice 0 e in
    let mantissa = numeric_string |> String.right -e in
    characteristic ++ "." ++ mantissa

withExponent: Int -> Number -> Number
withExponent exp (s, e)  =
  let final = min e exp
  in
    if final >= e
    then (s, e)
    else (s * (10 ^ -(final - e)), final)

add: Number -> Number -> Number
add (xs, xe) (ys, ye) =
  let exponent = min xe ye in
  let normalizedWithExponent = exponent |> withExponent in
  let (a, _) = normalizedWithExponent (xs, xe) in
  let (b, _) = normalizedWithExponent (ys, ye) in
  normalize (a + b, exponent)

subtract: Number -> Number -> Number
subtract (xs, xe) (ys, ye) =
  let exponent = min xe ye in
  let normalizedWithExponent = exponent |> withExponent in
  let (a, _) = normalizedWithExponent (xs, xe) in
  let (b, _) = normalizedWithExponent (ys, ye) in
  normalize (a - b, exponent)

multiply: Number -> Number -> Number
multiply (xs, xe) (ys, ye) =
  normalize (xs * ys, xe + ye)

divide: Number -> Number -> Number
divide (xs, xe) (ys, ye) =
  ((toFloat xs) * (10.0 ^ (toFloat xe))) / ((Basics.toFloat ys) * (10.0 ^ (toFloat ye)))
  |> toNumber
  |> normalize

parser : Parser Number
parser =
  succeed applySign
    |= sign
    |= float

type Sign
  = Positive
  | Negative

sign : Parser Sign
sign =
  oneOf
    [ symbol "-" |> map (always Negative)
    , symbol "+" |> map (always Positive)
    , succeed Positive
    ]

toNumber: Float -> Number
toNumber value =
  ( value |> String.fromFloat |> String.replace "." "" |> String.toInt |> Maybe.withDefault 0
  , -(value |> String.fromFloat |> String.split "." |> List.tail |> Maybe.withDefault [] |> List.head |> Maybe.withDefault "" |> String.length)
  )

applySign : Sign -> Float -> Number
applySign s v =
  let factor = if s == Positive then 1 else -1 in
  factor * v |> toNumber
