module Operation exposing (Operation, operate, parser)

import Parser exposing (Parser, (|.), (|=), symbol, oneOf, map)
import ArithmeticSign exposing (ArithmeticSign(..), parser)
import Flip exposing (flip)
import Number exposing (Number, add, multiply, subtract, divide)

type Operation
  = Addition
  | Subtraction
  | Multiplication
  | Division

operate: Operation -> Number -> Number -> Number
operate operation x y =
  case operation of
    Addition -> add x y
    Subtraction -> subtract x y
    Multiplication -> multiply x y
    Division -> divide x y

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
