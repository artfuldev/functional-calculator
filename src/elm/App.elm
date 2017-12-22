module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

main : Program Never Model Msg
main =
  Html.beginnerProgram { model = model, view = view, update = update }

type Operator
  = Addition

type alias Number
  = Float

type alias Calculation =
  { left: Maybe Number
  , operator: Maybe Operator
  , right: Maybe Number
  , result: Maybe Number
  }

type alias Model = Calculation

model : Model
model =
  { left = Nothing
  , operator = Nothing
  , right = Nothing
  , result = Nothing
  }

type Msg
  = Reset
  | SetLeft (Maybe Number)
  | SetOperator (Maybe Operator)
  | SetRight (Maybe Number)
  | Operate

update : Msg -> Model -> Model
update msg model =
  case msg of
    Reset -> { model | left = Nothing, operator = Nothing, right = Nothing, result = Nothing }
    SetLeft number -> { model | left = number }
    SetOperator operator -> { model | operator = operator }
    SetRight number -> { model | right = number }
    Operate -> { model | result = operate model.operator model.left model.right }

parseNumber : String -> Maybe Number
parseNumber value =
  case String.toFloat value of
    Ok number -> Just number
    Err _ -> Nothing

parseOperator: String -> Maybe Operator
parseOperator value =
  Just Addition

operate : Maybe Operator -> Maybe Number -> Maybe Number -> Maybe Number
operate operator left right =
  case operator of
    Just Addition ->
      case left of
        Just x ->
          case right of
            Just y -> Just <| x + y
            Nothing -> Nothing
        Nothing -> Nothing
    _ -> Nothing

toResultString: Maybe Number -> String
toResultString result =
  case result of
    Just number -> toString number
    Nothing -> ""

view : Model -> Html Msg
view model =
  div []
    [ text "Hello World!"
    , input [ placeholder "Left", type_ "number", onInput <| SetLeft << parseNumber ] []
    , input [ placeholder "+", onInput <| SetOperator << parseOperator ] []
    , input [ placeholder "Right", type_ "number", onInput <| SetRight << parseNumber ] []
    , button [ onClick Operate ] [ text "Operate" ]
    , text " = "
    , text <| toResultString model.result
    ]
