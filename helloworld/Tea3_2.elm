module Tea3_2 exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)


type Msg
    = ChangeGreeting


type alias Model =
    { name : String }


main : Program Never Model Msg
main =
    beginnerProgram
        { model = model
        , view = view
        , update = update
        }


model : Model
model =
    { name = "World" }


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick ChangeGreeting ] [ text "Change Greeting" ]
        , text ("Hello " ++ model.name)
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeGreeting ->
            { name = "Elm lovers" }
