module Main exposing (..)

import Navigation exposing (program)
import Data.Model as Model exposing (Model)
import Msg exposing (Msg(UrlChange))
import Update
import Router


init : Navigation.Location -> ( Model, Cmd Msg )
init currentUrl =
    ( Model.initModel, Cmd.none )


main : Program Never Model Msg
main =
    program UrlChange
        { view = Router.view
        , init = init
        , update = Update.update
        , subscriptions = always Sub.none
        }
