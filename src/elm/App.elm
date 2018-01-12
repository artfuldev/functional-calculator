module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Events exposing (onKeyDown)

import Parser exposing (run)
import Expression exposing (Expression)
import Key exposing (Key(..))
import Evaluation exposing (Evaluation)
import ArithmeticSign exposing (ArithmeticSign(..))
import Logo exposing (logo)

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
  | KeyTapped Key

updateExpression : Key -> Model -> Model
updateExpression key model =
  { model | expression = Expression.update model.expression key }

evaluate : Key -> Model -> Model
evaluate key model =
  case key of
    Evaluate ->
      case model.evaluation of
        Just evaluation -> { model | expression = toString evaluation, evaluation = Nothing }
        Nothing -> model
    _ -> { model | evaluation = Evaluation.evaluate model.expression }

processKey : Key -> Model -> Model
processKey key model =
  model
    |> updateExpression key
    |> evaluate key

  
update : Msg -> Model -> Model
update msg model =
  case msg of
    KeyPressed keyString ->
      case run Key.parser keyString of
        Ok key -> processKey key model
        Err _ -> model
    KeyTapped key -> processKey key model

keyView : Key -> Html Msg
keyView key =
  button [ onClick <| KeyTapped <| key ] [ text <| Key.toString key ]

view : Model -> Html Msg
view { expression, evaluation } =
  div [ id "app", onKeyDown KeyPressed, tabindex 1 ]
    [ header []
      [ input [ placeholder "0", value expression, readonly True, autofocus True ] []
      , p [] [ text <| Evaluation.toString evaluation ]
      ]
    , main_ []
      [ div [ class "keypad" ]
        [ keyView <| Digit 9
        , keyView <| Digit 8
        , keyView <| Digit 7
        , keyView <| Digit 6
        , keyView <| Digit 5
        , keyView <| Digit 4
        , keyView <| Digit 3
        , keyView <| Digit 2
        , keyView <| Digit 1
        , keyView Evaluate
        , keyView <| Digit 0
        , keyView Period
        ]
      , div [ class "operations" ]
        [ keyView Delete
        , keyView <| Sign Obelus
        , keyView <| Sign Times
        , keyView <| Sign Minus
        , keyView <| Sign Plus
        ]
      ]
    , footer []
      [ div []
        [ text "Made with "
        , span [ class "heart" ] [ text "❤" ]
        , text " using "
        , logo
        , text " elm by "
        , a [ class "author", href "https://github.com/artfuldev" ] [ text "@artfuldev" ]
        , div [ class "view-source" ]
          [ a [ class "source", href "https://github.com/artfuldev/functional-calculator/tree/gh-pages/" ] [ text "View Source" ]
          ]
        ]
      ]
    ]
