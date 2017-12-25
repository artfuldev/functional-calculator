module Calculation exposing (parser, perform)

import Operation exposing (Operation, operate)
import Parser exposing (Parser, (|.), (|=), succeed, float, inContext, end, run)

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
  inContext "a calculation expression" <|
    succeed Calculation
      |= float
      |= Operation.parser
      |= float
      |. end
