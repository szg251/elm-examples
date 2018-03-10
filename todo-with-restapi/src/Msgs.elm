module Msgs exposing (..)

import RemoteData exposing (WebData)
import Model exposing (..)


-- MESSAGES


type Msg
    = NoOp
    | InputTodoField String
    | SubmitTodo String
    | DelTodo String
    | ToggleTodo String
    | AfterFetchTodos (WebData (List Todo))
    | AfterPutNewTodo String (WebData Todo)
