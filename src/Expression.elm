module Expression exposing (Expression, update)

import ArithmeticSign exposing (ArithmeticSign(..))
import Key exposing (Key(..))


type alias Expression =
    String


addOperator : ArithmeticSign -> Expression -> Expression
addOperator sign expression =
    case sign of
        Plus ->
            expression ++ ArithmeticSign.toString sign

        Minus ->
            expression ++ ArithmeticSign.toString sign

        Times ->
            expression ++ ArithmeticSign.toString sign

        Obelus ->
            expression ++ ArithmeticSign.toString sign


removeLastCharacter : Expression -> Expression
removeLastCharacter =
    String.slice 0 -1


update : Key -> Expression -> Expression
update key expression =
    case key of
        Digit digit ->
            expression ++ String.fromInt digit

        Period ->
            expression ++ "."

        Delete ->
            removeLastCharacter expression

        Sign sign ->
            addOperator sign expression

        Cancel ->
            ""

        Evaluate ->
            expression
