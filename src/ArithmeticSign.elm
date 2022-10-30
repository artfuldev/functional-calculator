module ArithmeticSign exposing (ArithmeticSign(..), parser, toString)

import Parser exposing (Parser, map, oneOf, symbol)


type ArithmeticSign
    = Plus
    | Minus
    | Times
    | Obelus


parser : Parser ArithmeticSign
parser =
    oneOf
        [ symbol "*" |> map (always Times)
        , symbol "×" |> map (always Times)
        , symbol "x" |> map (always Times)
        , symbol "X" |> map (always Times)
        , symbol "/" |> map (always Obelus)
        , symbol "÷" |> map (always Obelus)
        , symbol "+" |> map (always Plus)
        , symbol "-" |> map (always Minus)
        ]


toString : ArithmeticSign -> String
toString sign =
    case sign of
        Plus ->
            "+"

        Minus ->
            "-"

        Times ->
            "×"

        Obelus ->
            "÷"
