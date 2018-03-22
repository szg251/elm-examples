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
import Json.Decode exposing (Decoder)


type alias Endpoint =
    ( String, String )


apiHost : String
apiHost =
    "http://localhost:3030"


endpoints =
    { fetchTodos = ( "/todo", "GET" )
    , putNewTodo = ( "/todo", "PUT" )
    , deleteTodo = ( "/todo", "DELETE" )
    , patchTodo = ( "/todo", "PATCH" )
    }


mkRequest ( endpoint, method ) maybeBody decoder msg =
    let
        body =
            case maybeBody of
                Just body ->
                    Http.jsonBody body

                Nothing ->
                    Http.emptyBody

        options =
            { method = method
            , headers = []
            , url = apiHost ++ endpoint
            , body = body
            , expect = expectJson decoder
            , timeout = Nothing
            , withCredentials = False
            }
    in
        Http.request options
            |> RemoteData.sendRequest
            |> Cmd.map msg


fetchTodos : Cmd Msg
fetchTodos =
    mkRequest
        endpoints.fetchTodos
        Nothing
        todosDecoder
        AfterFetchTodos


putNewTodo : Todo -> Cmd Msg
putNewTodo newtodo =
    mkRequest
        endpoints.putNewTodo
        (Just (todoEncoder newtodo))
        todoDecoder
        (AfterPutNewTodo newtodo.id)


patchTodo : Todo -> Cmd Msg
patchTodo todo =
    mkRequest
        endpoints.patchTodo
        (Just (todoEncoder todo))
        todoDecoder
        (\_ -> NoOp)


deleteTodo : Int -> Cmd Msg
deleteTodo todoId =
    mkRequest
        endpoints.deleteTodo
        (Just (idEncoder todoId))
        todosDecoder
        (\_ -> NoOp)
