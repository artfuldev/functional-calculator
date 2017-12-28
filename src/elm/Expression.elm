module Expression exposing (update)

type alias Expression
  = String

isDigit : Expression -> Bool
isDigit expression =
  List.member expression <| String.split "" "0123456789."

isDelete : Expression -> Bool
isDelete expression =
  List.member expression [ "Backspace", "Delete" ]

isOperator : Expression -> Bool
isOperator expression =
  List.member expression [ "+", "-", "×", "÷", "*", "/", "x", "X" ]

fixOperator : Expression -> Expression
fixOperator operator =
  case operator of
    "*" -> "×"
    "x" -> "×"
    "X" -> "×"
    "/" -> "÷"
    _ -> operator

addOperator : Expression -> Expression -> Expression
addOperator expression addendum =
  case addendum of
    "+" -> expression ++ addendum
    "-" -> expression ++ addendum
    "×" -> expression ++ addendum
    "÷" -> expression ++ addendum
    _ -> expression

removeLastCharacter : Expression -> Expression
removeLastCharacter = String.slice 0 -1

update : Expression -> Expression -> Expression
update expression addendum =
  if isDigit addendum then expression ++ addendum
  else if isDelete addendum then removeLastCharacter addendum
  else if isOperator addendum then addOperator expression <| fixOperator addendum
  else expression
