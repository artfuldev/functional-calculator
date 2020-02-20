module App exposing (main)

import Browser
import Html exposing (..)
import Model exposing (Model, update)
import Msg exposing (Msg)
import View exposing (view)

main : Program () Model Msg
main =
  Browser.sandbox { init = Model.default, view = view, update = update }
