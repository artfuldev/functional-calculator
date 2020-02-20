module Events exposing (onKeyDown)

import Html exposing (Attribute)
import Html.Events exposing (keyCode, custom)
import Json.Decode as Json exposing (field, string, map)

withOptions : msg -> { message: msg, stopPropagation: Bool, preventDefault: Bool }
withOptions msg = 
  { message = msg
  , stopPropagation = True
  , preventDefault = True
  }

key : Json.Decoder String
key =
  field "key" string

onKeyDown : (String -> msg) -> Attribute msg
onKeyDown handler =
  custom "keydown" <| map withOptions <| map handler key
