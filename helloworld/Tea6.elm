module Tea5 exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type Msg
    = ChangeGreeting String
    | ResetName
    | ToggleColor


type alias Model =
    { name : String
    , color : String
    , colors : List String
    }


main : Program Never Model Msg
main =
    beginnerProgram
        { model = model
        , view = view
        , update = update
        }


model : Model
model =
    { name = "World"
    , color = "red"
    , colors = [ "blue", "green", "black" ]
    }


view : Model -> Html Msg
view model =
    div []
        [ input [ onInput ChangeGreeting, value model.name ] []
        , button [ onClick ResetName ] [ text "Reset" ]
        , button [ onClick ToggleColor ] [ text "Toggle Color" ]
        , div [ style [ ( "color", model.color ) ] ] [ text ("Hello " ++ model.name) ]
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeGreeting newname ->
            { model | name = newname }

        ResetName ->
            { model | name = "World" }

        ToggleColor ->
            case model.colors of
                newColor :: rest ->
                    { model
                        | color = newColor
                        , colors = rest ++ [ model.color ]
                    }

                [] ->
                    model
