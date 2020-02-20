module Evaluation exposing (Evaluation, evaluate, toString)

import Expression exposing (Expression)
import Calculation exposing (perform)
import Parser exposing (run)

type alias Evaluation
  = Maybe Float

evaluate : Expression -> Evaluation
evaluate expression =
  case run Calculation.parser expression of
    Ok calculation -> Just <| perform calculation
    Err _ -> Nothing

toString : Evaluation -> String
toString result =
  case result of
    Just float -> String.fromFloat float
    Nothing -> ""
