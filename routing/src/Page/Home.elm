module Page.Home exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict
import Data.Model exposing (..)
import Data.Routes exposing (UserRole(..))
import Msg exposing (..)


view : Model -> Html Msg
view model =
    case model.currentUser of
        Just ( user, roles ) ->
            div []
                [ h1 [] [ text ("Welcome " ++ user) ]
                , h3 [] [ text "Users:" ]
                , ul []
                    (Dict.keys model.profiles
                        |> List.filter ((/=) user)
                        |> List.map
                            (\usr ->
                                li [] [ a [ href ("#profile/" ++ usr) ] [ text usr ] ]
                            )
                    )
                , a [ href ("#profile/" ++ user) ] [ text "Profile page" ]
                , if List.member Admin roles then
                    div [] [ a [ href "#admin" ] [ text "Admin page" ] ]
                  else
                    text ""
                , button [ onClick Logout ] [ text "Logout" ]
                ]

        Nothing ->
            div []
                [ h1 [] [ text "Welcome" ]
                , a [ href "#login" ] [ text "Please login" ]
                ]
