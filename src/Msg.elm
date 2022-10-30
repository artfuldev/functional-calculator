module Msg exposing (Msg(..))

import Key exposing (Key)


type Msg
    = KeyPressed String
    | KeyTapped Key
