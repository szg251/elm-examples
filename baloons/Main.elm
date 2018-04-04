module Main exposing (main)

import Html exposing (Html, program)
import Maybe.Extra
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Mouse exposing (Position)
import Time exposing (Time, second)
import Window exposing (Size)
import Task


type alias Model =
    { baloons : List Baloon
    , newBaloon : Maybe Baloon
    , windowSize : Size
    }


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
    | WindowResize Size


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


outOfScreen : Size -> List Baloon -> List Baloon
outOfScreen window baloons =
    List.filter (\{ position } -> position.y < window.height) baloons


initmodel : Model
initmodel =
    { baloons = [], newBaloon = Nothing, windowSize = { width = 0, height = 0 } }


init : ( Model, Cmd Msg )
init =
    initmodel ! [ Task.perform WindowResize Window.size ]


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        NoOp ->
            model ! [ Cmd.none ]

        WindowResize size ->
            { model | windowSize = size } ! [ Cmd.none ]

        MouseDown position ->
            { model | newBaloon = Just { position = position, speed = -5, size = 5 } }
                ! [ Cmd.none ]

        MouseUp position ->
            { model
                | baloons = Maybe.Extra.toList model.newBaloon ++ model.baloons
                , newBaloon = Nothing
            }
                ! [ Cmd.none ]

        Tick time ->
            let
                inflate baloon =
                    { baloon | size = baloon.size + 1 }
            in
                { model
                    | baloons = List.map fallDown model.baloons |> outOfScreen model.windowSize
                    , newBaloon = Maybe.map inflate model.newBaloon
                }
                    ! [ Cmd.none ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.downs MouseDown
        , Mouse.ups MouseUp
        , Time.every (second / 24) Tick
        , Window.resizes WindowResize
        ]


view : Model -> Html Msg
view model =
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
        svg
            [ width (toString model.windowSize.width)
            , height (toString model.windowSize.height)
            ]
            (List.map drawCircle model.baloons)


main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
