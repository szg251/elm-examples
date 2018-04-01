module Requests exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String


type NoContent
    = NoContent


type alias Todo =
    { id : Int
    , value : String
    , done : Bool
    }


type alias NewTodo =
    { value : String
    }


decodeTodo : Decoder Todo
decodeTodo =
    decode Todo
        |> required "id" int
        |> required "value" string
        |> required "done" bool


encodeTodo : Todo -> Json.Encode.Value
encodeTodo x =
    Json.Encode.object
        [ ( "id", Json.Encode.int x.id )
        , ( "value", Json.Encode.string x.value )
        , ( "done", Json.Encode.bool x.done )
        ]


encodeNewTodo : NewTodo -> Json.Encode.Value
encodeNewTodo x =
    Json.Encode.object
        [ ( "value", Json.Encode.string x.value )
        ]


getTodo : Http.Request (List (Todo))
getTodo =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ "http://localhost:3030"
                , "todo"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeTodo)
        , timeout =
            Nothing
        , withCredentials =
            False
        }


getTodoByTodoId : Int -> Http.Request (Maybe (Todo))
getTodoByTodoId capture_todoId =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ "http://localhost:3030"
                , "todo"
                , capture_todoId |> toString |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (maybe decodeTodo)
        , timeout =
            Nothing
        , withCredentials =
            False
        }


postTodo : NewTodo -> Http.Request (Todo)
postTodo body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ "http://localhost:3030"
                , "todo"
                ]
        , body =
            Http.jsonBody (encodeNewTodo body)
        , expect =
            Http.expectJson decodeTodo
        , timeout =
            Nothing
        , withCredentials =
            False
        }


deleteTodoByTodoId : Int -> Http.Request (NoContent)
deleteTodoByTodoId capture_todoId =
    Http.request
        { method =
            "DELETE"
        , headers =
            []
        , url =
            String.join "/"
                [ "http://localhost:3030"
                , "todo"
                , capture_todoId |> toString |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectStringResponse
                (\{ body } ->
                    if String.isEmpty body then
                        Ok NoContent
                    else
                        Err "Expected the response body to be empty"
                )
        , timeout =
            Nothing
        , withCredentials =
            False
        }


putTodoByTodoId : Int -> Todo -> Http.Request (NoContent)
putTodoByTodoId capture_todoId body =
    Http.request
        { method =
            "PUT"
        , headers =
            []
        , url =
            String.join "/"
                [ "http://localhost:3030"
                , "todo"
                , capture_todoId |> toString |> Http.encodeUri
                ]
        , body =
            Http.jsonBody (encodeTodo body)
        , expect =
            Http.expectStringResponse
                (\{ body } ->
                    if String.isEmpty body then
                        Ok NoContent
                    else
                        Err "Expected the response body to be empty"
                )
        , timeout =
            Nothing
        , withCredentials =
            False
        }