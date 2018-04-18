module Page.Admin exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Msg exposing (Msg)
import Data.Model exposing (Model)


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "This is the admin page" ]
        , a [ href "#main" ] [ text "Back to main page" ]
        ]
