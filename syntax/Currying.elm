module Currying exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


main =
    div []
        [ div [ style [ ( "backgroundColor", "blue" ), ( "color", "white" ) ] ] [ text "simple" ]
        , br [] []
        , blueLine [ text "with children argument" ]
        , br [] []
        , blueLineCurried [ text "curried" ]
        ]


blueLine : List (Html msg) -> Html msg
blueLine children =
    div [ style [ ( "backgroundColor", "blue" ), ( "color", "white" ) ] ] children


blueLineCurried : List (Html msg) -> Html msg
blueLineCurried =
    div [ style [ ( "backgroundColor", "blue" ), ( "color", "white" ) ] ]
