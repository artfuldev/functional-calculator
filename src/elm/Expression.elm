module Expression exposing (update)

type alias Expression
  = String

removeLastCharacter : Expression -> Expression
removeLastCharacter = String.slice 0 -1

update : Expression -> Expression -> Expression
update expression addendum =
  case addendum of
    "1" -> expression ++ addendum
    "2" -> expression ++ addendum
    "3" -> expression ++ addendum
    "4" -> expression ++ addendum
    "5" -> expression ++ addendum
    "6" -> expression ++ addendum
    "7" -> expression ++ addendum
    "8" -> expression ++ addendum
    "9" -> expression ++ addendum
    "0" -> expression ++ addendum
    "+" -> expression ++ addendum
    "-" -> expression ++ addendum
    "*" -> expression ++ "x"
    "x" -> expression ++ addendum
    "X" -> expression ++ "x"
    "/" -> expression ++ addendum
    "." -> expression ++ addendum
    "e" -> expression ++ addendum
    "E" -> expression ++ addendum
    "Backspace" -> removeLastCharacter expression
    "Delete" -> removeLastCharacter expression
    _ -> expression
