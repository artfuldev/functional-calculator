module Operation exposing (Operation, operate, parser)

import ArithmeticSign exposing (ArithmeticSign(..))
import Number exposing (Number, add, divide, multiply, subtract)
import Parser exposing ((|.), (|=), Parser, succeed)


type alias Operation =
    { sign : ArithmeticSign
    , value : Number
    }


operate : Operation -> Number -> Number
operate { sign, value } x =
    case sign of
        Plus ->
            add x value

        Minus ->
            subtract x value

        Times ->
            multiply x value

        Obelus ->
            divide x value


parser : Parser Operation
parser =
    succeed Operation
        |= ArithmeticSign.parser
        |= Number.parser
