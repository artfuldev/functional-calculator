module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Events exposing (onKeyDown)

import Calculation exposing (perform)
import Parser exposing (run)
import Expression
import Key exposing (Key)

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

updateExpression : Key -> Model -> Model
updateExpression key model =
  { model | expression = Expression.update model.expression key }

updateResult : Model -> Model
updateResult model =
  { model | result = result model.expression }

result : String -> Maybe Float
result expression =
  case run Calculation.parser expression of
    Ok calculation -> Just <| perform calculation
    Err _ -> Nothing

update : Msg -> Model -> Model
update msg model =
  case msg of
    KeyPressed keyString ->
      case run Key.parser keyString of
        Ok key -> model |> updateExpression key |> updateResult
        Err _ -> model

toResultString : Maybe Float -> String
toResultString result =
  case result of
    Just float -> toString float
    Nothing -> ""

view : Model -> Html Msg
view { expression, result } =
  div [ id "app", onKeyDown KeyPressed, tabindex 1 ]
    [ header []
      [ input [ placeholder "0", value expression, readonly True, autofocus True ] []
      , p [] [ text <| toResultString result ]
      ]
    , main_ [] []  
    ]
