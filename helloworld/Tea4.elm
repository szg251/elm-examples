module Tea4 exposing (..)

import Html exposing (..)
import Html.Events exposing (..)


type Msg
    = ChangeGreeting String


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
        [ input [ onInput ChangeGreeting ] []
        , text ("Hello " ++ model.name)
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeGreeting name ->
            { name = name }
