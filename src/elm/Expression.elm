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
  List.member expression <| String.split "" "+-*xX/"

addOperator : Expression -> Expression -> Expression
addOperator expression addendum =
  case addendum of
    "+" -> expression ++ addendum
    "-" -> expression ++ addendum
    "*" -> expression ++ "x"
    "x" -> expression ++ addendum
    "X" -> expression ++ "x"
    "/" -> expression ++ addendum
    _ -> expression

removeLastCharacter : Expression -> Expression
removeLastCharacter = String.slice 0 -1

update : Expression -> Expression -> Expression
update expression addendum =
  if isDigit addendum then expression ++ addendum
  else if isDelete addendum then removeLastCharacter expression
  else if isOperator addendum then addOperator expression addendum
  else expression
