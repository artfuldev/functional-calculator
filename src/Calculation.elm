module Calculation exposing (parser, perform)

import Operation exposing (Operation, operate)
import Number
import Parser exposing (Parser, (|.), (|=), succeed, float, end, run)

type alias Calculation =
  { left: Float
  , operation: Operation
  , right: Float
  }

perform: Calculation -> Float
perform { left, operation, right } =
  operate operation left right 

parser: Parser Calculation
parser =
  succeed Calculation
    |= Number.parser
    |= Operation.parser
    |= Number.parser
    |. end
