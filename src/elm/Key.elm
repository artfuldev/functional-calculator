module Key exposing (Key(..), parser, ArithmeticSign(..))

import Parser exposing (Parser, inContext, (|=), (|.), map, oneOf, int, keyword, succeed, symbol)

type ArithmeticSign
  = Plus
  | Minus
  | Times
  | Obelus

type Key
  = Digit Int
  | Period
  | Sign ArithmeticSign
  | Delete

parser : Parser Key
parser =
  inContext "a key" <|
    oneOf
      [ digit
      , symbol "." |> map (always Period)
      , sign |> map (\s -> Sign s)
      , delete
      ]

digit : Parser Key
digit =
  int |> map Digit

sign : Parser ArithmeticSign
sign =
  oneOf
    [ symbol "*" |> map (always Times)
    , symbol "x" |> map (always Times)
    , symbol "X" |> map (always Times)
    , symbol "/" |> map (always Obelus)
    , symbol "+" |> map (always Plus)
    , symbol "-" |> map (always Minus)
    ]

delete : Parser Key
delete =
  oneOf
    [ keyword "Delete"
    , keyword "Backspace"
    ]
    |> map (always Delete)
