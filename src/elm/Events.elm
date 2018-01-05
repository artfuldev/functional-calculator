module Events exposing (onKeyDown)

import Html exposing (Attribute)
import Html.Events exposing (onWithOptions, Options, keyCode)
import Json.Decode as Json exposing (field, string, map)

eventOptions : Options
eventOptions = 
  { stopPropagation = True
  , preventDefault = True
  }

key : Json.Decoder String
key =
  field "key" string

onKeyDown : (String -> msg) -> Attribute msg
onKeyDown handler =
  onWithOptions "keydown" eventOptions <| map handler key
