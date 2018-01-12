module App exposing (..)

import Html exposing (..)
import Model exposing (Model, update)
import Msg exposing (Msg(..))
import View exposing (view)

main : Program Never Model Msg
main =
  Html.beginnerProgram { model = Model.default, view = view, update = update }
