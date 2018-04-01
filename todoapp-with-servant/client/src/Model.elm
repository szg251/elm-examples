module Model exposing (..)

import RemoteData exposing (WebData)
import Json.Decode as Decode
import Json.Decode.Pipeline as P
import Json.Encode as Encode


-- MODEL


type alias Todo =
    { id : Int
    , value : String
    , done : Bool
    }


type alias Model =
    { newtodo : String
    , todos : WebData (List Todo)
    }


initialModel : Model
initialModel =
    { newtodo = ""
    , todos = RemoteData.NotAsked
    }
