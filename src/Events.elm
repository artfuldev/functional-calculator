module Events exposing (onKeyDown)

import Html exposing (Attribute)
import Html.Events exposing (custom, keyCode)
import Json.Decode as Json exposing (field, map, string)


withOptions : msg -> { message : msg, stopPropagation : Bool, preventDefault : Bool }
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
