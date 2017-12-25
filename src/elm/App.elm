module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Events exposing (onKeyDown)

import Calculation exposing (parser, perform)
import Parser exposing (run)

main : Program Never Model Msg
main =
  Html.beginnerProgram { model = model, view = view, update = update }

type alias Model =
  { expression: String
  , result: Maybe Float
  }

model : Model
model =
  { expression = ""
  , result = Nothing
  }

type Msg
  = KeyPressed String

removeLastCharacter : String -> String
removeLastCharacter = String.slice 0 -1

updateExpression : String -> Model -> Model
updateExpression key model =
  let
    { expression } = model
    exp =
      case key of
        "1" -> expression ++ key
        "2" -> expression ++ key
        "3" -> expression ++ key
        "4" -> expression ++ key
        "5" -> expression ++ key
        "6" -> expression ++ key
        "7" -> expression ++ key
        "8" -> expression ++ key
        "9" -> expression ++ key
        "0" -> expression ++ key
        "+" -> expression ++ key
        "-" -> expression ++ key
        "*" -> expression ++ key
        "/" -> expression ++ key
        "." -> expression ++ key
        "e" -> expression ++ key
        "E" -> expression ++ key
        "Backspace" -> removeLastCharacter expression
        "Delete" -> removeLastCharacter expression
        _ -> expression
  in { model | expression = exp }

updateResult : Model -> Model
updateResult model =
  { model | result = result model.expression }

result : String -> Maybe Float
result expression =
  case run parser expression of
    Ok calculation -> Just <| perform calculation
    Err _ -> Nothing

update : Msg -> Model -> Model
update msg model =
  case msg of
    KeyPressed key -> model |> updateExpression key |> updateResult

toResultString : Maybe Float -> String
toResultString result =
  case result of
    Just float -> toString float
    Nothing -> ""

view : Model -> Html Msg
view model =
  div [ id "app" ]
    [ header []
      [ input [ placeholder "0", onKeyDown KeyPressed, value model.expression ] []
      , p [] [ text <| toResultString model.result ]
      ]
    , main_ [] []  
    ]
