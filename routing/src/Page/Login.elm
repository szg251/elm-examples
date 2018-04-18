module Page.Login exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Data.Model exposing (..)
import Data.Routes exposing (UserRole(..))
import Msg exposing (..)
import Navigation
import Dict


update : LoginMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputName name ->
            ( { model | loginName = name }, Cmd.none )

        InputPass pass ->
            ( { model | loginPass = pass }, Cmd.none )

        Login ->
            let
                authorized =
                    Dict.get model.loginName model.users
                        |> Maybe.andThen
                            (\{ pass, roles } ->
                                if pass == model.loginPass then
                                    Just ( model.loginName, roles )
                                else
                                    Nothing
                            )
            in
                ( { model
                    | loginName = ""
                    , loginPass = ""
                    , currentUser = authorized
                  }
                , Navigation.newUrl <|
                    case authorized of
                        Just _ ->
                            "#main"

                        Nothing ->
                            "#login/invalid"
                )


view : Maybe String -> Model -> Html LoginMsg
view maybeError model =
    div []
        [ h1 [] [ text "Login" ]
        , viewError maybeError
        , Html.form [ onSubmit Login ]
            [ input
                [ placeholder "Name"
                , onInput InputName
                , value model.loginName
                ]
                []
            , input
                [ type_ "password"
                , placeholder "Password"
                , onInput InputPass
                , value model.loginPass
                ]
                []
            , button [] [ text "Login" ]
            ]
        ]


viewError : Maybe String -> Html LoginMsg
viewError maybeError =
    let
        nicErr =
            case maybeError of
                Just "invalid" ->
                    Just "Invalid username or password."

                Just "unauthorized" ->
                    Just "You are not authorized to view this page."

                Just "unauthenticated" ->
                    Just "You are not logged in. Please log in to view this page."

                _ ->
                    Nothing
    in
        case nicErr of
            Nothing ->
                text ""

            Just err ->
                div
                    [ style
                        [ ( "backgroundColor", "red" )
                        , ( "color", "white" )
                        ]
                    ]
                    [ text err ]
