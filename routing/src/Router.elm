module Router exposing (..)

import Html exposing (Html)
import Navigation exposing (Location)
import UrlParser as Url exposing (parseHash)
import Msg exposing (Msg)
import Data.Model exposing (Model)
import Data.Routes as Routes
import Page.Home
import Page.Login
import Page.Profile
import Page.Admin


update : Location -> Model -> ( Model, Cmd msg )
update url model =
    case parseHash Routes.router url of
        Nothing ->
            ( model, Cmd.none )

        Just path ->
            case path of
                Routes.RestrictedPage neededRole _ ->
                    case model.currentUser of
                        Just ( _, roles ) ->
                            if List.member neededRole roles then
                                ( { model | currentUrl = path }, Cmd.none )
                            else
                                ( model, Navigation.newUrl "#login/unauthorized" )

                        Nothing ->
                            ( model, Navigation.newUrl "#login/unauthenticated" )

                _ ->
                    ( { model | currentUrl = path }, Cmd.none )


view : Model -> Html Msg
view model =
    case model.currentUrl of
        Routes.HomePage ->
            Page.Home.view model

        Routes.LoginPage errorMsg ->
            Html.map Msg.LoginMsg <| Page.Login.view errorMsg model

        Routes.RestrictedPage _ (Routes.ProfilePage user) ->
            Page.Profile.view user model

        Routes.RestrictedPage _ Routes.AdminPage ->
            Page.Admin.view model
