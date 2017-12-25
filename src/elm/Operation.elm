module Operation exposing (..)

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

operator: Parser Operation
operator =
  inContext "an operator" <|
    oneOf
      [ symbol "+" |> map (always Addition)
      , symbol "-" |> map (always Subtraction)
      , symbol "*" |> map (always Multiplication)
      , symbol "/" |> map (always Division)
      ]
