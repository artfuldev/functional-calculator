module Key exposing (Key(..), parser, toString)

import Parser exposing (Parser, inContext, (|=), (|.), map, oneOf, int, keyword, succeed, symbol)
import ArithmeticSign exposing (ArithmeticSign)

type Key
  = Digit Int
  | Period
  | Sign ArithmeticSign
  | Delete
  | Cancel
  | Evaluate

parser : Parser Key
parser =
  inContext "a key" <|
    oneOf
      [ digit
      , symbol "." |> map (always Period)
      , ArithmeticSign.parser |> map Sign
      , delete
      , keyword "Esc" |> map (always Cancel)
      , equals
      ]

digit : Parser Key
digit =
  int |> map Digit

delete : Parser Key
delete =
  oneOf
    [ keyword "Delete"
    , keyword "Backspace"
    ]
    |> map (always Delete)

equals : Parser Key
equals =
  oneOf
    [ symbol "="
    , keyword "Enter"
    ]
    |> map (always Evaluate)

toString : Key -> String
toString key =
  case key of
    Digit digit -> Basics.toString digit
    Period -> "."
    Sign sign -> ArithmeticSign.toString sign
    Delete -> "DEL"
    Cancel -> "C"
    Evaluate -> "="
