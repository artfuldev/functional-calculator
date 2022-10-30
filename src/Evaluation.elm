module Evaluation exposing (Evaluation, evaluate, toString)

import Calculation exposing (perform)
import Expression exposing (Expression)
import Number exposing (Number)
import Parser exposing (run)


type alias Evaluation =
    Maybe Number


evaluate : Expression -> Evaluation
evaluate expression =
    case run Calculation.parser expression of
        Ok calculation ->
            calculation |> perform |> Just

        Err _ ->
            Nothing


toString : Evaluation -> String
toString result =
    case result of
        Just number ->
            Number.toString number

        Nothing ->
            ""
