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
    ( String, String )


apiHost : String
apiHost =
    "http://localhost:3030"


mkRequest : Endpoint -> Maybe Value -> Decoder a -> (WebData a -> Msg) -> Cmd Msg
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
        ( "/todo", "GET" )
        Nothing
        todosDecoder
        AfterFetchTodos


putNewTodo : Todo -> Cmd Msg
putNewTodo newtodo =
    mkRequest
        ( "/todo", "POST" )
        (Just <| todoEncoder newtodo)
        todoDecoder
        (AfterPutNewTodo newtodo.id)


patchTodo : Todo -> Cmd Msg
patchTodo todo =
    mkRequest
        ( "/todo/" ++ (toString (todo.id)), "PUT" )
        (Just <| todoEncoder todo)
        todoDecoder
        (\_ -> NoOp)


deleteTodo : Int -> Cmd Msg
deleteTodo todoId =
    mkRequest
        ( "/todo/" ++ (toString todoId), "DELETE" )
        (Nothing)
        todosDecoder
        (\_ -> NoOp)
