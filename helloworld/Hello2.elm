module Hello2 exposing (..)

import Html exposing (text)


type Name
    = Gergo
    | Yamasato


main =
    text (sayhi Gergo)


sayhi : Name -> String
sayhi user =
    case user of
        Yamasato ->
            "こんにちは"

        Gergo ->
            "Szia"
