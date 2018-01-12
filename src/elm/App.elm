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
import Model exposing (Model, update)
import Msg exposing (Msg(..))

main : Program Never Model Msg
main =
  Html.beginnerProgram { model = Model.default, view = view, update = update }

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
