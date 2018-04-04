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
    , heading : { x : Int, y : Int }
    , size : Int
    }


type Msg
    = NoOp
    | MouseDown Position
    | MouseUp Position
    | Tick Time
    | WindowResize Size


gravity : Baloon -> Baloon
gravity baloon =
    let
        newPosition =
            { x = baloon.position.x + baloon.heading.x
            , y = baloon.position.y + baloon.heading.y
            }

        newSpeed =
            { x = baloon.heading.x
            , y = Basics.min (baloon.heading.y + 1) 20
            }
    in
        { baloon | position = newPosition, heading = newSpeed }


outOfScreen : Size -> List Baloon -> List Baloon
outOfScreen window baloons =
    List.filter (\{ position, size } -> position.y - size < window.height) baloons


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
            { model | newBaloon = Just { position = position, heading = { x = 0, y = 0 }, size = 5 } }
                ! [ Cmd.none ]

        MouseUp position ->
            let
                throwAway baloon =
                    { baloon
                        | heading =
                            { x = (baloon.position.x - position.x) // 5
                            , y = (baloon.position.y - position.y) // 5
                            }
                    }
            in
                { model
                    | baloons =
                        model.newBaloon
                            |> Maybe.map throwAway
                            |> Maybe.Extra.toList
                            |> List.append model.baloons
                    , newBaloon = Nothing
                }
                    ! [ Cmd.none ]

        Tick time ->
            let
                inflate baloon =
                    { baloon | size = baloon.size + 1 }
            in
                { model
                    | baloons =
                        model.baloons
                            |> List.map gravity
                            |> outOfScreen model.windowSize
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
            (List.map drawCircle (Maybe.Extra.toList model.newBaloon ++ model.baloons))


main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
