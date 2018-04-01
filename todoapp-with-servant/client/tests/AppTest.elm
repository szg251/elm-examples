module AppTest exposing (..)

import Test exposing (..)
import Expect exposing (Expectation)
import App exposing (..)
import Model exposing (..)
import Msgs exposing (..)
import RemoteData


-- STUBS


stubTodos : List Todo
stubTodos =
    [ { id = "1", value = "valb", done = True }
    , { id = "0", value = "vala", done = False }
    ]


stubModel : Model
stubModel =
    { newtodo = ""
    , todos = RemoteData.succeed stubTodos
    }



-- TESTS


updateTests : Test
updateTests =
    describe "Update"
        [ test "SubmitTodo" <|
            \_ ->
                let
                    expectedTodos =
                        RemoteData.succeed
                            [ { id = "2", value = "newtodo", done = False }
                            , { id = "1", value = "valb", done = True }
                            , { id = "0", value = "vala", done = False }
                            ]
                in
                    update (SubmitTodo "newtodo") stubModel
                        |> Tuple.first
                        |> Expect.equal { stubModel | todos = expectedTodos }
        , test "DelTodo" <|
            \_ ->
                let
                    expectedTodos =
                        RemoteData.succeed
                            [ { id = "0", value = "vala", done = False } ]
                in
                    update (TodoMsg << DelTodo <| "1") stubModel
                        |> Tuple.first
                        |> .todos
                        |> Expect.equal expectedTodos
        , test "ToggleTodo" <|
            \_ ->
                let
                    expectedTodos =
                        RemoteData.succeed
                            [ { id = "1", value = "valb", done = True }
                            , { id = "0", value = "vala", done = True }
                            ]
                in
                    update (TodoMsg << ToggleTodo <| "0") stubModel
                        |> Tuple.first
                        |> .todos
                        |> Expect.equal expectedTodos
        , test "AfterPutNewTodo" <|
            \_ ->
                let
                    expectedTodos =
                        RemoteData.succeed
                            [ { id = "newid", value = "valb", done = True }
                            , { id = "0", value = "vala", done = False }
                            ]

                    newtodo =
                        RemoteData.succeed
                            { id = "newid", value = "valb", done = True }
                in
                    update (ApiMsg (AfterPutNewTodo "1" newtodo)) stubModel
                        |> Tuple.first
                        |> .todos
                        |> Expect.equal expectedTodos
        ]
