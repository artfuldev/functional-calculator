module Calculation exposing (parser, perform)

import Operation exposing (Operation, operate)
import Number exposing (Number)
import Parser exposing (Parser, (|.), (|=), succeed, float, end, run)

type alias Calculation =
  { left: Number
  , operation: Operation
  , right:  Number
  }

perform: Calculation -> Number
perform { left, operation, right } =
  operate operation left right 

parser: Parser Calculation
parser =
  succeed Calculation
    |= Number.parser
    |= Operation.parser
    |= Number.parser
    |. end
