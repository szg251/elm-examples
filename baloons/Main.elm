module Main exposing (main)

import Html exposing (Html, program)
import Maybe.Extra
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Mouse exposing (Position)
import Time exposing (Time, second)


type alias Model =
    { circles : List Baloon, newBaloon : Maybe Baloon }


type alias Baloon =
    { position : Position
    , speed : Int
    , size : Int
    }


type Msg
    = NoOp
    | MouseDown Position
    | MouseUp Position
    | Tick Time


fallDown : Baloon -> Baloon
fallDown baloon =
    let
        newPosition =
            { x = baloon.position.x
            , y = baloon.position.y + baloon.speed
            }

        newSpeed =
            Basics.min (baloon.speed + 1) 20
    in
        { baloon | position = newPosition, speed = newSpeed }


outOfScreen : List Baloon -> List Baloon
outOfScreen baloons =
    List.filter (\{ position } -> position.x <= 1000 && position.y <= 1000) baloons


initmodel : Model
initmodel =
    { circles = [], newBaloon = Nothing }


init : ( Model, Cmd msg )
init =
    initmodel ! [ Cmd.none ]


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        NoOp ->
            model ! [ Cmd.none ]

        MouseDown position ->
            { model | newBaloon = Just { position = position, speed = -5, size = 5 } }
                ! [ Cmd.none ]

        MouseUp position ->
            { model
                | circles = Maybe.Extra.toList (model.newBaloon) ++ model.circles
                , newBaloon = Nothing
            }
                ! [ Cmd.none ]

        Tick time ->
            let
                inflate baloon =
                    { baloon | size = baloon.size + 1 }
            in
                { model
                    | circles = List.map fallDown model.circles |> outOfScreen
                    , newBaloon = Maybe.map inflate model.newBaloon
                }
                    ! [ Cmd.none ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.downs MouseDown
        , Mouse.ups MouseUp
        , Time.every (second / 24) Tick
        ]


view : Model -> Html Msg
view model =
    svg
        [ width "1000", height "1000" ]
        (viewCircles (Maybe.Extra.toList model.newBaloon ++ model.circles))


viewCircles : List Baloon -> List (Svg Msg)
viewCircles circles =
    let
        drawCircle : Baloon -> Svg Msg
        drawCircle { position, size } =
            circle
                [ cx (toString position.x)
                , cy (toString position.y)
                , r (toString size)
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
