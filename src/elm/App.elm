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
  { model | expression = Expression.update key model.expression }

evaluate : Key -> Model -> Model
evaluate key model =
  case key of
    Evaluate ->
      case model.evaluation of
        Just evaluation -> { model | expression = toString evaluation, evaluation = Nothing }
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

keyView : Key -> Html Msg
keyView key =
  button [ onClick <| KeyTapped <| key ] [ text <| Key.toString key ]

keypad : List (Html Msg)
keypad =
  [ Digit 9 , Digit 8, Digit 7, Digit 6, Digit 5 , Digit 4 , Digit 3 , Digit 2 , Digit 1 , Evaluate , Digit 0 , Period ]
  |> List.map keyView

operations : List (Html Msg)
operations =
  [ Delete , Sign Obelus , Sign Times , Sign Minus , Sign Plus ]
  |> List.map keyView
  

view : Model -> Html Msg
view { expression, evaluation } =
  div [ id "app", onKeyDown KeyPressed, tabindex 1 ]
    [ header []
      [ input [ placeholder "0", value expression, readonly True, autofocus True ] []
      , p [] [ text <| Evaluation.toString evaluation ]
      ]
    , main_ []
      [ div [ class "keypad" ] keypad
      , div [ class "operations" ] operations
      ]
    , footer []
      [ div []
        [ text "Made with ❤ using "
        , logo
        , text " elm by "
        , a [ class "author", href "https://github.com/artfuldev" ] [ text "@artfuldev" ]
        , div [ class "view-source" ]
          [ a [ class "source", href "https://github.com/artfuldev/functional-calculator/" ] [ text "View Source" ]
          ]
        ]
      ]
    ]
