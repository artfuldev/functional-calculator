module Expression exposing (update)

import Key exposing (Key(..), ArithmeticSign(..))

type alias Expression
  = String

addOperator : Expression -> ArithmeticSign -> Expression
addOperator expression sign =
  case sign of
    Plus -> expression ++ "+"
    Minus -> expression ++ "-"
    Times -> expression ++ "×"
    Obelus -> expression ++ "÷"

removeLastCharacter : Expression -> Expression
removeLastCharacter = String.slice 0 -1

update : Expression -> Key -> Expression
update expression key =
  case key of
    Digit d -> expression ++ toString d
    Period -> expression ++ "."
    Delete -> removeLastCharacter expression
    Sign s -> addOperator expression s
