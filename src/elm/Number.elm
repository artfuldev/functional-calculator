module Number exposing (parser)

import Char
import Parser exposing (Parser, (|.), (|=), inContext, succeed, fail, keep, oneOf, map, symbol, andThen, oneOrMore)

{- The implementation is found in the project elm-json
  https://github.com/zwilias/elm-json 
  
  The exact implementation code is at
  https://github.com/zwilias/elm-json/blob/dfda64af641f66330c83e88bf8c0cd28492fc0dc/src/Json/Parser.elm
  as jsonNumber that returns Either Int Float, we are
  concerned with just Float.
-}
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

digitString : Parser String
digitString =
  keep oneOrMore Char.isDigit

digits : Parser Float
digits =
  digitString
    |> andThen (String.toInt >> result fail succeed >> map toFloat)

maybeNegative : Parser Sign
maybeNegative =
  oneOf
    [ symbol "-" |> map (always Negative)
    , succeed Positive
    ]

parseNumber : Parser Float
parseNumber =
    digits
      |> andThen maybeFractional
      |> andThen maybeExponentiate

maybeFractional : Float -> Parser Float
maybeFractional integerPart =
    inContext "fractional" <|
      oneOf
        [ succeed (combine integerPart)
            |. symbol "."
            |= digitString
        , succeed integerPart
        ]

combine : Float -> String -> Float
combine integerPart fracPart =
    integerPart + toFractional fracPart

toFractional : String -> Float
toFractional floatString =
  {- We parse "0.1" in 2 parts - a Float `0` and a _String_ `1`. The reason
    for parsing the fractional as a String is because we need to handle cases
    like `1.01`. So, to turn the string `"01"` into `0.01`, we turn it into
    an integer and move if right by dividing by 10 a couple of times.
  -}
  floatString
    |> String.toInt
    |> Result.withDefault 0
    |> dividedBy (10 ^ String.length floatString)

dividedBy : Int -> Int -> Float
dividedBy divisor dividend =
  toFloat dividend / toFloat divisor

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
      |= digits

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

result : (e -> b) -> (a -> b) -> Result e a -> b
result onError onSuccess res =
  case res of
    Err e -> onError e
    Ok a -> onSuccess a
