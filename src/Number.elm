module Number exposing (parser)

import Parser exposing (Parser, (|.), (|=), succeed, float, oneOf, map, symbol, andThen)

-- The implementation is a simplified version adapted from https://github.com/zwilias/elm-json
parser : Parser Float
parser =
  succeed applySign
    |= maybeNegative
    |= parseNumber

maybeNegative : Parser Sign
maybeNegative =
  oneOf
    [ symbol "-" |> map (always Negative)
    , succeed Positive
    ]

parseNumber : Parser Float
parseNumber =
    float
      |> andThen maybeExponentiate

maybeExponentiate : Float -> Parser Float
maybeExponentiate number =
  oneOf
    [ exponent |> map (applyExponent number)
    , succeed number
    ]

exponent : Parser Float
exponent =
  succeed applySign
    |. oneOf [ symbol "e", symbol "E" ]
    |= sign
    |= float

applyExponent : Float -> Float -> Float
applyExponent coefficient e =
  coefficient * (10 ^ e)

type Sign
  = Positive
  | Negative

sign : Parser Sign
sign =
  oneOf
    [ symbol "-" |> map (always Negative)
    , symbol "+" |> map (always Positive)
    , succeed Positive
    ]

applySign : Sign -> number -> number
applySign s number =
  case s of
    Positive -> number
    Negative -> -number
