module Requests
    exposing
        ( fetchTodos
        , putNewTodo
        , deleteTodo
        , patchTodo
        )

import Http exposing (..)
import RemoteData exposing (WebData)
import Model exposing (..)
import Msgs exposing (..)
import Json.Decode exposing (Decoder, Value)


type alias Endpoint =
    { method : String, path : List String }


apiHost : String
apiHost =
    "http://localhost:3030"


type alias RequestOptions a =
    { action : Endpoint
    , body : Maybe Value
    , decoder : Decoder a
    , onResponse : WebData a -> Msg
    }


mkRequest : RequestOptions a -> Cmd Msg
mkRequest { action, body, decoder, onResponse } =
    let
        jsonBody =
            case body of
                Nothing ->
                    Http.emptyBody

                Just requestBody ->
                    Http.jsonBody requestBody

        options =
            { method = action.method
            , headers = []
            , url = String.join "/" (apiHost :: action.path)
            , body = jsonBody
            , expect = expectJson decoder
            , timeout = Nothing
            , withCredentials = False
            }
    in
        Http.request options
            |> RemoteData.sendRequest
            |> Cmd.map onResponse


fetchTodos : Cmd Msg
fetchTodos =
    mkRequest
        { action = { method = "GET", path = [ "todo" ] }
        , body = Nothing
        , decoder = todosDecoder
        , onResponse = AfterFetchTodos
        }


putNewTodo : Todo -> Cmd Msg
putNewTodo newtodo =
    mkRequest
        { action = { method = "POST", path = [ "todo" ] }
        , body = Just (todoEncoder newtodo)
        , decoder = todoDecoder
        , onResponse = (AfterPutNewTodo newtodo.id)
        }


patchTodo : Todo -> Cmd Msg
patchTodo todo =
    mkRequest
        { action = { method = "PUT", path = [ "todo", toString todo.id ] }
        , body = Just (todoEncoder todo)
        , decoder = todoDecoder
        , onResponse = always NoOp
        }


deleteTodo : Int -> Cmd Msg
deleteTodo todoId =
    mkRequest
        { action = { method = "DELETE", path = [ "todo", toString todoId ] }
        , body = Nothing
        , decoder = todosDecoder
        , onResponse = always NoOp
        }
