module Key exposing (Key(..), parser)

import Parser exposing (Parser, inContext, (|=), (|.), map, oneOf, int, keyword, succeed, symbol)
import ArithmeticSign exposing (ArithmeticSign, parser)

type Key
  = Digit Int
  | Period
  | Sign ArithmeticSign
  | Delete
  | Cancel

parser : Parser Key
parser =
  inContext "a key" <|
    oneOf
      [ digit
      , symbol "." |> map (always Period)
      , ArithmeticSign.parser |> map Sign
      , delete
      , keyword "Esc" |> map (always Cancel)
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
