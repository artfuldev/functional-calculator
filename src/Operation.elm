module Operation exposing (Operation, operate, parser)

import Parser exposing (Parser, (|.), (|=), end, succeed)
import ArithmeticSign exposing (ArithmeticSign(..), parser)
import Number exposing (Number, add, multiply, subtract, divide, parser)

type alias Operation =
  { sign: ArithmeticSign
  , value: Number
  }

operate: Operation -> Number -> Number
operate { sign, value } x =
  case sign of
    Plus -> add x value
    Minus -> subtract x value
    Times -> multiply x value
    Obelus -> divide x value

parser: Parser Operation
parser =
  succeed Operation
  |= ArithmeticSign.parser
  |= Number.parser
  |. end
  
