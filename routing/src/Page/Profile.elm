module Page.Profile exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict
import Data.Model exposing (..)
import Msg exposing (..)


view : String -> Model -> Html Msg
view user model =
    div []
        [ case Dict.get user model.profiles of
            Nothing ->
                h1 [] [ text "User doesn't exist" ]

            Just profile ->
                div []
                    [ h1 [] [ text "Profile" ]
                    , div [] [ text ("Fullname: " ++ profile.fullname) ]
                    , div [] [ text ("Age: " ++ toString profile.age) ]
                    ]
        , a [ href "#main" ] [ text "Back to main page" ]
        ]
