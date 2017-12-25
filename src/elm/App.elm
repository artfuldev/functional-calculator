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

removeLastCharacter: String -> String
removeLastCharacter = String.slice 0 -1

expression: String -> String -> String
expression previous current =
  case current of
    "1" -> previous ++ current
    "2" -> previous ++ current
    "3" -> previous ++ current
    "4" -> previous ++ current
    "5" -> previous ++ current
    "6" -> previous ++ current
    "7" -> previous ++ current
    "8" -> previous ++ current
    "9" -> previous ++ current
    "0" -> previous ++ current
    "+" -> previous ++ current
    "-" -> previous ++ current
    "*" -> previous ++ current
    "/" -> previous ++ current
    "." -> previous ++ current
    "e" -> previous ++ current
    "E" -> previous ++ current
    "Backspace" -> removeLastCharacter previous
    "Delete" -> removeLastCharacter previous
    _ -> previous

result : String -> Maybe Float
result value =
  case run parser value of
    Ok calculation -> Just <| perform calculation
    Err _ -> Nothing

update : Msg -> Model -> Model
update msg model =
  case msg of
    KeyPressed value ->
      let expr = expression model.expression value
      in { model | expression = expr, result = result <| expr }

toResultString : Maybe Float -> String
toResultString value =
  case value of
    Just x -> toString x
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
