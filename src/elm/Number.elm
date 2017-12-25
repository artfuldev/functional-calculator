module Number exposing (parser)

import Parser exposing (Parser, (|.), (|=), inContext, succeed, float, oneOf, map, symbol, andThen)

-- The implementation is a simplified version adapted from https://github.com/zwilias/elm-json
parser : Parser Float
parser =
  inContext "a number" <|
    succeed (\sign number -> applySign sign number)
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
  inContext "maybe exponentiate" <|
    oneOf
      [ exponent |> map (applyExponent number)
      , succeed number
      ]

exponent : Parser Float
exponent =
  inContext "exponent" <|
    succeed applySign
      |. oneOf [ symbol "e", symbol "E" ]
      |= sign
      |= float

applyExponent : Float -> Float -> Float
applyExponent coeff exponent =
  coeff * (10 ^ exponent)

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
applySign sign number =
  case sign of
    Positive -> number
    Negative -> -number
