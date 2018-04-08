module Main exposing (main)

import Html exposing (Html, program)
import Maybe.Extra as MaybeE
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Mouse exposing (Position)
import Time exposing (Time, second)
import Window exposing (Size)
import Task


type alias Model =
    { baloons : List Baloon
    , newBaloon : Maybe Baloon
    , arrow : Maybe Triangle
    , windowSize : Size
    }


type alias Baloon =
    { position : Position
    , heading : { x : Int, y : Int }
    , size : Int
    }


type alias Triangle =
    { top : Position
    , left : Position
    , right : Position
    , angle : Float
    }


type Msg
    = NoOp
    | MouseDown Position
    | MouseMove Position
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
    let
        isIn { position, size } =
            position.y - size < window.height && position.x - size < window.width
    in
        List.filter isIn baloons


initmodel : Model
initmodel =
    { baloons = []
    , newBaloon = Nothing
    , arrow = Nothing
    , windowSize = { width = 0, height = 0 }
    }


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
            { model
                | newBaloon =
                    Just
                        { position = position
                        , heading = { x = 0, y = 0 }
                        , size = 5
                        }
                , arrow =
                    Just
                        { top = position
                        , left = position
                        , right = position
                        , angle = 0
                        }
            }
                ! [ Cmd.none ]

        MouseMove position ->
            let
                moveBottom triangle =
                    let
                        distX =
                            triangle.top.x - position.x

                        distY =
                            triangle.top.y - position.y

                        angle =
                            atan2 (toFloat distX) (toFloat distY)

                        dist =
                            sqrt (toFloat ((distX ^ 2) + (distY ^ 2)))
                    in
                        { triangle
                            | left =
                                { position
                                    | x = triangle.top.x - round (sin (angle + 0.07) * dist / 1.5)
                                    , y = triangle.top.y - round (cos (angle + 0.07) * dist / 1.5)
                                }
                            , right =
                                { position
                                    | x = triangle.top.x - round (sin (angle - 0.07) * dist / 1.5)
                                    , y = triangle.top.y - round (cos (angle - 0.07) * dist / 1.5)
                                }
                        }
            in
                { model | arrow = Maybe.map moveBottom model.arrow }
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
                            |> MaybeE.toList
                            |> List.append model.baloons
                    , newBaloon = Nothing
                    , arrow = Nothing
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
        , Mouse.moves MouseMove
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

        drawTriangle : Triangle -> Svg Msg
        drawTriangle triangle =
            let
                toStringPoints { top, left, right } =
                    String.join " "
                        [ toStringPoint top, toStringPoint left, toStringPoint right ]

                toStringPoint { x, y } =
                    String.join ","
                        [ toString x, toString y ]
            in
                polygon
                    [ stroke "red"
                    , strokeLinejoin "round"
                    , strokeWidth "5"
                    , fill "red"
                    , points (toStringPoints triangle)
                    ]
                    []
    in
        svg
            [ width (toString model.windowSize.width)
            , height (toString model.windowSize.height)
            ]
            ((Maybe.map drawTriangle model.arrow |> MaybeE.toList)
                ++ List.map drawCircle (MaybeE.toList model.newBaloon ++ model.baloons)
            )


main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
