module Parser exposing (parseNumber)

type ParserErrorMessage
  = NothingToParse
  | CharacterMismatch

parseDigit: Maybe String -> Result ParserErrorMessage (Float, Maybe String)
parseDigit input =
  case input of
    Just value ->
      case String.length value of
        0 -> Err NothingToParse
        1 ->
          case String.toFloat value of
            Ok number -> Ok (number, Nothing)
            Err _ -> Err CharacterMismatch
        length ->
          case String.toFloat <| String.dropRight (length - 1) value of
            Ok number -> Ok (number, Just <| String.dropLeft 1 value)
            Err _ -> Err CharacterMismatch
    Nothing -> Err NothingToParse

parseDigits: Char -> Result ParserErrorMessage Float -> Result ParserErrorMessage Float
parseDigits char previous =
  case previous of
    Err msg -> Err msg
    Ok value ->
      case parseDigit <| Just <| String.fromChar char of
        Err CharacterMismatch -> Err CharacterMismatch
        Err NothingToParse -> Ok value
        Ok (digit, remaining) -> Ok <| value * 10 + digit

parseNumber: Maybe String -> Result ParserErrorMessage (Float, Maybe String)
parseNumber input =
  case input of
    Just value ->
      case String.length value of
        0 -> Err NothingToParse
        1 -> parseDigit <| Just value
        _ ->
          case String.foldl parseDigits (Ok 0) value of
            Err msg -> Err msg
            Ok result -> Ok (result, Nothing)
    Nothing -> Err NothingToParse
