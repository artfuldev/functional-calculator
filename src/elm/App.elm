module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Events exposing (onKeyDown)

import Calculation exposing (parser, perform)
import Parser exposing (run)
import Expression

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

updateExpression : String -> Model -> Model
updateExpression key model =
  { model | expression = Expression.update model.expression key }

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
