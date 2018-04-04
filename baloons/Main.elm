module Main exposing (main)

import Html exposing (Html, program)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Mouse exposing (Position)
import Time exposing (Time, second)


type alias Model =
    { baloons : List Baloon }


type alias Baloon =
    { position : Position
    , inertia : Int
    }


type Msg
    = NoOp
    | Click Position
    | Gravity Time


fallDown : Baloon -> Baloon
fallDown baloon =
    let
        newPosition =
            { x = baloon.position.x
            , y = baloon.position.y + baloon.inertia
            }

        newInertia =
            Basics.min (baloon.inertia + 1) 20
    in
        { baloon | position = newPosition, inertia = newInertia }


initmodel : Model
initmodel =
    { baloons = [] }


init : ( Model, Cmd msg )
init =
    initmodel ! [ Cmd.none ]


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        NoOp ->
            model ! [ Cmd.none ]

        Click position ->
            let
                newBaloon =
                    { position = position, inertia = 1 }
            in
                { model | baloons = newBaloon :: model.baloons }
                    ! [ Cmd.none ]

        Gravity time ->
            { model | baloons = List.map fallDown model.baloons }
                ! [ Cmd.none ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.clicks Click
        , Time.every (second / 24) Gravity
        ]


view : Model -> Html Msg
view model =
    svg
        [ width "1000", height "1000" ]
        (viewCircles model.baloons)


viewCircles : List Baloon -> List (Svg Msg)
viewCircles circles =
    let
        drawCircle : Baloon -> Svg Msg
        drawCircle { position } =
            circle
                [ cx (toString position.x)
                , cy (toString position.y)
                , r "10"
                , fill "green"
                ]
                []
    in
        List.map drawCircle circles


main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
