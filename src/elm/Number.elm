module Number exposing (parser)

import Parser exposing (Parser, (|.), (|=), inContext, succeed, fail, float, keep, oneOf, map, symbol, andThen, oneOrMore)

-- The implementation is a simplified version adapted from https://github.com/zwilias/elm-json
parser : Parser Float
parser =
  {- This involves quite a few brittle things:
      - decode the sign (so possibly a unary minus)
      - decodes digits, then checks if its scientific notation or a float
      - and finally, it applies the sign.
    The sign has to be applied at the end, or we risk messing up float-stuff.
  -}
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
  {- At this point, we're dealing with either an Int or a Float, and may
    encounter an exponent (in the scientific notation sense).
    For example, `0.01e2` means we'll have a `Right 0.01` value here, and
    want to multiply it by `10 ^ 2`.

    Writing this down makes me realize that `0.01e2` could conceivably be
    considered an Int; and I may need to revise my strategy. Especially since
    we already make sure to parse `4e-1` as a Float (since having a `0.4` as
    an Int seems _really_ wrong).
  -}
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
