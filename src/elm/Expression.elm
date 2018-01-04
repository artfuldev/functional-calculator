module Expression exposing (Expression, update)

import Key exposing (Key(..))
import ArithmeticSign exposing (ArithmeticSign(..))

type alias Expression
  = String

addOperator : Expression -> ArithmeticSign -> Expression
addOperator expression sign =
  case sign of
    Plus -> expression ++ ArithmeticSign.toString sign
    Minus -> expression ++ ArithmeticSign.toString sign
    Times -> expression ++ ArithmeticSign.toString sign
    Obelus -> expression ++ ArithmeticSign.toString sign

removeLastCharacter : Expression -> Expression
removeLastCharacter = String.slice 0 -1

update : Expression -> Key -> Expression
update expression key =
  case key of
    Digit d -> expression ++ toString d
    Period -> expression ++ "."
    Delete -> removeLastCharacter expression
    Sign s -> addOperator expression s
    Cancel -> ""
    Evaluate -> expression
