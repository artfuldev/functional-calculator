module Operation exposing (Operation, operate, parser)

import Parser exposing (Parser, (|.), (|=), symbol, oneOf, map, inContext)

type Operation
  = Addition
  | Subtraction
  | Multiplication
  | Division

operate: Operation -> Float -> Float -> Float
operate operation x y =
  case operation of
    Addition -> x + y
    Subtraction -> x - y
    Multiplication -> multiply x y
    Division -> x / y

parser: Parser Operation
parser =
  inContext "an operator" <|
    oneOf
      [ symbol "+" |> map (always Addition)
      , symbol "-" |> map (always Subtraction)
      , symbol "x" |> map (always Multiplication)
      , symbol "/" |> map (always Division)
      ]

precision: Float -> Int
precision float =
  toString float
    |> String.split "."
    |> List.tail
    |> Maybe.withDefault []
    |> List.head
    |> Maybe.withDefault ""
    |> String.length

fixPrecision: Int -> Float -> Float
fixPrecision precision value =
  toFloat (round (value * toFloat (10 ^ precision))) / toFloat (10 ^ precision)

multiply: Float -> Float -> Float
multiply x y =
  x * y
  |> fixPrecision (precision x + precision y)
