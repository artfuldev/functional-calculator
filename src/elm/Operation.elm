module Operation exposing (Operation, operate, parser)

import Parser exposing (Parser, (|.), (|=), symbol, oneOf, map, inContext)
import ArithmeticSign exposing (ArithmeticSign(..), parser)

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

toOperation : ArithmeticSign -> Operation
toOperation sign =
  case sign of
    Plus -> Addition
    Minus -> Subtraction
    Times -> Multiplication
    Obelus -> Division

parser: Parser Operation
parser =
  ArithmeticSign.parser
    |> map toOperation
    |> inContext "an operator"

precision: Float -> Int
precision float =
  float
    |> toString
    |> String.split "."
    |> List.tail
    |> Maybe.withDefault []
    |> List.head
    |> Maybe.withDefault ""
    |> String.length

fixPrecision: Int -> Float -> Float
fixPrecision precision value =
  let
    multiplier = 10 ^ precision |> toFloat
    correct = flip (/) multiplier
  in
    value * multiplier
      |> round
      |> toFloat
      |> correct

multiply: Float -> Float -> Float
multiply x y =
  x * y
    |> fixPrecision (precision x + precision y)
