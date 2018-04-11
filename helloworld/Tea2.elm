module Tea2 exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)


type Msg
    = ChangeGreeting


main : Program Never String Msg
main =
    beginnerProgram
        { model = model
        , view = view
        , update = update
        }


model : String
model =
    "World"


view : String -> Html Msg
view model =
    div []
        [ button [ onClick ChangeGreeting ] [ text "Change Greeting" ]
        , text ("Hello " ++ model)
        ]


update : Msg -> String -> String
update msg model =
    case msg of
        ChangeGreeting ->
            "Elm lovers"
