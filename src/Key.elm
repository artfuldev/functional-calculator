module Key exposing (Key(..), parser, toString)

import ArithmeticSign exposing (ArithmeticSign)
import Parser exposing ((|.), (|=), Parser, int, keyword, map, oneOf, symbol)


type Key
    = Digit Int
    | Period
    | Sign ArithmeticSign
    | Delete
    | Cancel
    | Evaluate


parser : Parser Key
parser =
    oneOf
        [ oneOf [ keyword "Enter", symbol "=" ] |> map (always Evaluate)
        , keyword "Escape" |> map (always Cancel)
        , symbol "." |> map (always Period)
        , int |> map Digit
        , ArithmeticSign.parser |> map Sign
        , oneOf [ keyword "Delete", keyword "Backspace" ] |> map (always Delete)
        ]


toString : Key -> String
toString key =
    case key of
        Digit d ->
            String.fromInt d

        Period ->
            "."

        Sign s ->
            ArithmeticSign.toString s

        Delete ->
            "DEL"

        Cancel ->
            "C"

        Evaluate ->
            "="
