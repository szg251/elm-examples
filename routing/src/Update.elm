module Update exposing (..)

import Msg exposing (..)
import Data.Model exposing (..)
import Navigation
import Dict
import Router
import Page.Login


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UrlChange url ->
            Router.update url model

        LoginMsg loginMsg ->
            Page.Login.update loginMsg model

        Logout ->
            ( { model | currentUser = Nothing }, Navigation.newUrl ("#main") )
