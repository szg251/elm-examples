module Msgs exposing (..)

import RemoteData exposing (WebData)
import Model exposing (..)


-- MESSAGES


type Msg
    = NoOp
    | InputTodoField String
    | SubmitTodo String
    | DelTodo Int
    | ToggleTodo Int
    | AfterFetchTodos (WebData (List Todo))
    | AfterPutNewTodo Int (WebData Todo)
