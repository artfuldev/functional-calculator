module Calculation exposing (parse, perform)

import Operation exposing (Operation(..), operate, operator)
import Parser exposing (Parser, (|.), (|=), succeed, float, inContext, end, run)

type alias Calculation =
  { left: Float
  , operation: Operation
  , right: Float
  }

perform: Calculation -> Float
perform { left, operation, right } =
  operate operation left right

calculation: Parser Calculation
calculation =
  inContext "a calculation expression" <|
    succeed Calculation
      |= float
      |= operator
      |= float
      |. end

parse: String -> Maybe Calculation
parse input =
  case run calculation input of
    Ok result -> Just result
    Err _ -> Nothing
