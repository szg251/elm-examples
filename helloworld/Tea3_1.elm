module Tea3_1 exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)


type Msg
    = ChangeGreeting


main : Program Never { name : String } Msg
main =
    beginnerProgram
        { model = model
        , view = view
        , update = update
        }


model : { name : String }
model =
    { name = "World" }


view : { name : String } -> Html Msg
view model =
    div []
        [ button [ onClick ChangeGreeting ] [ text "Change Greeting" ]
        , text ("Hello " ++ model.name)
        ]


update : Msg -> { name : String } -> { name : String }
update msg model =
    case msg of
        ChangeGreeting ->
            { name = "Elm lovers" }
