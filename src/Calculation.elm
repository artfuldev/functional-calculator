module Calculation exposing (parser, perform)

import Operation exposing (Operation, operate)
import Number exposing (Number)
import Parser exposing (Parser, (|.), (|=), succeed, float, end, run)

type alias Calculation =
  { start: Number
  , operation: Operation
  }

perform: Calculation -> Number
perform { start, operation } =
  operate operation start

parser: Parser Calculation
parser =
  succeed Calculation
    |= Number.parser
    |= Operation.parser
    |. end
