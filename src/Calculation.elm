module Calculation exposing (parser, perform)

import Number exposing (Number)
import Operation exposing (Operation, operate)
import Parser exposing ((|.), (|=), Parser, Step(..), Trailing(..), end, sequence, spaces, succeed)


type alias Calculation =
    { start : Number
    , operations : List Operation
    }


perform : Calculation -> Number
perform { start, operations } =
    List.foldl operate start operations


operations_parser : Parser (List Operation)
operations_parser =
    sequence
        { start = ""
        , separator = ""
        , end = ""
        , spaces = spaces
        , item = Operation.parser
        , trailing = Forbidden
        }


parser : Parser Calculation
parser =
    succeed Calculation
        |= Number.parser
        |= operations_parser
        |. end
