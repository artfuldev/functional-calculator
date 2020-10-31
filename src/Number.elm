module Number exposing (parser, Number, add, subtract, multiply, divide, toString)

import Basics exposing (Bool, Int)
import Parser exposing (Parser, (|.), (|=), succeed, float, oneOf, map, symbol, andThen)
import Flip exposing (flip)
import Svg.Attributes exposing (exponent)
import Parser exposing (number)
import String exposing (fromFloat)

type alias Number =
  { significand: Int
  , exponent: Int
  }

normalize: Number -> Number
normalize number =
  if (number.significand == 0 || number.exponent == 0 || Basics.remainderBy 10 number.significand /= 0)
  then number
  else
    normalize
      { significand = number.significand // 10
      , exponent = number.exponent - 1
      }

toString: Number -> String
toString number =
  if number.exponent == 0
  then number.significand |> String.fromInt
  else
    let significand = number.significand |> String.fromInt |> String.padLeft -number.exponent '0' in
    let characteristic = significand |> String.slice 0 number.exponent in
    let mantissa = significand |> String.right -number.exponent in
    characteristic ++ "." ++ mantissa

withExponent: Int -> Number -> Number
withExponent exponent number  =
  let final = min number.exponent exponent
  in
    if final >= number.exponent
    then number
    else
      { significand = number.significand * (10 ^ -(final - number.exponent))
      , exponent = final
      }

add: Number -> Number -> Number
add x y =
  let exponent = min x.exponent y.exponent in
  let normalizedWithExponent = exponent |> withExponent in
  let a = normalizedWithExponent x in
  let b = normalizedWithExponent y in
    normalize
      { significand = a.significand + b.significand
      , exponent = exponent
      }

subtract: Number -> Number -> Number
subtract x y =
  let exponent = min x.exponent y.exponent in
  let normalizedWithExponent = exponent |> withExponent in
  let a = normalizedWithExponent x in
  let b = normalizedWithExponent y in
    normalize
      { significand = a.significand - b.significand
      , exponent = exponent
      }

multiply: Number -> Number -> Number
multiply x y =
  normalize
    { significand = x.significand * y.significand
    , exponent = x.exponent + y.exponent
    }

divide: Number -> Number -> Number
divide x y =
  let result = (Basics.toFloat x.significand) / (Basics.toFloat y.significand) |> fromFloat in
  normalize result

parser : Parser Number
parser =
  succeed applySign
    |= maybeNegative
    |= parseNumber

maybeNegative : Parser Sign
maybeNegative =
  oneOf
    [ symbol "-" |> map (always Negative)
    , succeed Positive
    ]

parseNumber : Parser Float
parseNumber =
    float

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

fromFloat: Float -> Number
fromFloat number =
  { significand = number |> String.fromFloat |> String.replace "." "" |> String.toInt |> Maybe.withDefault 0
  , exponent = -(number |> String.fromFloat |> String.split "." |> List.tail |> Maybe.withDefault [] |> List.head |> Maybe.withDefault "" |> String.length)
  }

applySign : Sign -> Float -> Number
applySign s number =
  case s of
    Positive -> fromFloat number      
    Negative -> fromFloat -number
