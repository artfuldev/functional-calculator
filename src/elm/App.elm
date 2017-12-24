module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Calculation exposing (parse, perform)

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
  = ExpressionInputChanged String

result : String -> Maybe Float
result value =
  case parse value of
    Just calculation -> Just <| perform calculation
    Nothing -> Nothing

update : Msg -> Model -> Model
update msg model =
  case msg of
    ExpressionInputChanged value -> { model | expression = value, result = result value }

toResultString : Maybe Float -> String
toResultString value =
  case value of
    Just x -> toString x
    Nothing -> ""
    

view : Model -> Html Msg
view model =
  div [ id "app" ]
    [ header []
      [ input [ placeholder "0", onInput ExpressionInputChanged ] []
      , p [] [ text <| toResultString model.result ]
      ]
    , main_ [] []  
    ]
