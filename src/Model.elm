module Model exposing (Model, default, update)

import Expression exposing (Expression)
import Evaluation exposing (Evaluation)
import Key exposing (Key(..))
import Msg exposing (Msg(..))
import Parser exposing (run)

type alias Model =
  { expression: Expression
  , evaluation: Evaluation
  }

default : Model
default = Model "" Nothing

updateExpression : Key -> Model -> Model
updateExpression key model =
  { model | expression = Expression.update key model.expression }

evaluate : Key -> Model -> Model
evaluate key model =
  case key of
    Evaluate ->
      case model.evaluation of
        Just evaluation -> { model | expression = String.fromFloat evaluation , evaluation = Nothing }
        Nothing -> model
    _ -> { model | evaluation = Evaluation.evaluate model.expression }

processKey : Key -> Model -> Model
processKey key =
  updateExpression key >> evaluate key
  
update : Msg -> Model -> Model
update msg =
  case msg of
    KeyPressed keyString ->
      case run Key.parser keyString of
        Ok key -> processKey key
        Err _ -> identity
    KeyTapped key -> processKey key