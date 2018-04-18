module Msg exposing (..)

import Navigation exposing (Location)


type Msg
    = NoOp
    | UrlChange Navigation.Location
    | Logout
    | LoginMsg LoginMsg


type LoginMsg
    = Login
    | InputName String
    | InputPass String
