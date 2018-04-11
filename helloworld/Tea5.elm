module Tea5 exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type Msg
    = ChangeGreeting String
    | ResetName


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
        [ input [ onInput ChangeGreeting, value model.name ] []
        , button [ onClick ResetName ] [ text "Reset" ]
        , text ("Hello " ++ model.name)
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeGreeting newname ->
            { name = newname }

        ResetName ->
            { name = "World" }
