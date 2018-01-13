module App exposing (main)

import Html exposing (beginnerProgram)
import Model exposing (Model, update)
import Msg exposing (Msg)
import View exposing (view)

main : Program Never Model Msg
main =
  beginnerProgram { model = Model.default, view = view, update = update }
