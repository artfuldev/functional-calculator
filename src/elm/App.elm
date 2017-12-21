module App exposing (..)

import Html exposing (..)

-- APP
main : Program Never Model Msg
main =
  Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL
type alias Model = String

model : Model
model = "Hello World from ELM!"


-- UPDATE
type Msg = None

update : Msg -> Model -> Model
update msg model =
  model


-- VIEW
view : Model -> Html Msg
view model =
  div [] [ text model ]
