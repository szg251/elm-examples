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
    [ { id = "a", value = "vala", done = False } ]


stubModel : Model
stubModel =
    { newtodo = ""
    , todos = RemoteData.succeed stubTodos
    }



-- TESTS


myfirsttest : Test
myfirsttest =
    describe "Update function"
        [ test "Submit a todo" <|
            \_ ->
                let
                    expectedTodos =
                        RemoteData.succeed
                            [ { id = "0", value = "newtodo", done = False }
                            , { id = "a", value = "vala", done = False }
                            ]
                in
                    update (SubmitTodo "newtodo") stubModel
                        |> Tuple.first
                        |> Expect.equal { stubModel | todos = expectedTodos }
        ]
