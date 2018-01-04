module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Events exposing (onKeyDown)

import Parser exposing (run)
import Expression exposing (Expression)
import Key exposing (Key)
import Evaluation exposing (Evaluation)

main : Program Never Model Msg
main =
  Html.beginnerProgram { model = model, view = view, update = update }

type alias Model =
  { expression: Expression
  , evaluation: Evaluation
  }

model : Model
model =
  { expression = ""
  , evaluation = Nothing
  }

type Msg
  = KeyPressed String

updateExpression : Key -> Model -> Model
updateExpression key model =
  { model | expression = Expression.update model.expression key }

evaluate : Model -> Model
evaluate model =
  { model | evaluation = Evaluation.evaluate model.expression }

update : Msg -> Model -> Model
update msg model =
  case msg of
    KeyPressed keyString ->
      case run Key.parser keyString of
        Ok key -> model |> updateExpression key |> evaluate
        Err _ -> model

view : Model -> Html Msg
view { expression, evaluation } =
  div [ id "app", onKeyDown KeyPressed, tabindex 1 ]
    [ header []
      [ input [ placeholder "0", value expression, readonly True, autofocus True ] []
      , p [] [ text <| Evaluation.toString evaluation ]
      ]
    , main_ [] []  
    ]
