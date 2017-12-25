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
    Multiplication -> x * y
    Division -> x / y

parser: Parser Operation
parser =
  inContext "an operator" <|
    oneOf
      [ symbol "+" |> map (always Addition)
      , symbol "-" |> map (always Subtraction)
      , symbol "*" |> map (always Multiplication)
      , symbol "/" |> map (always Division)
      ]
