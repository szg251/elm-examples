module Model exposing (..)

import RemoteData exposing (WebData)
import Json.Decode as Decode
import Json.Decode.Pipeline as P
import Json.Encode as Encode
import Dict exposing (Dict)


-- MODEL


type alias Todo =
    { id : String
    , value : String
    , done : Bool
    , order : Int
    }


type alias Model =
    { newtodo : String
    , todos : WebData (Dict String Todo)
    }


initialModel : Model
initialModel =
    { newtodo = ""
    , todos = RemoteData.Loading
    }



-- DECODERS


todosDecoder : Decode.Decoder (List Todo)
todosDecoder =
    Decode.list todoDecoder


todoDecoder : Decode.Decoder Todo
todoDecoder =
    P.decode Todo
        |> P.required "id" Decode.string
        |> P.required "value" Decode.string
        |> P.required "done" Decode.bool
        |> P.required "order" Decode.int



-- ENCODERS


todoEncoder : Todo -> Encode.Value
todoEncoder todo =
    let
        attributes =
            [ ( "id", Encode.string todo.id )
            , ( "value", Encode.string todo.value )
            , ( "done", Encode.bool todo.done )
            , ( "order", Encode.int todo.order )
            ]
    in
        Encode.object attributes


idEncoder : String -> Encode.Value
idEncoder todoId =
    let
        attributes =
            [ ( "id", Encode.string todoId ) ]
    in
        Encode.object attributes
