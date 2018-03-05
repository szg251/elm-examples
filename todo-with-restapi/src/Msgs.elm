module Msgs exposing (..)

import RemoteData exposing (WebData)
import Model exposing (..)


-- MESSAGES


type ApiMsg
    = AfterFetchTodos (WebData (List Todo))
    | AfterPutNewTodo String (WebData Todo)
    | AfterDeleteTodo (WebData (List Todo))
    | AfterPatchTodo (WebData Todo)


type TodoMsg
    = SaveTodo String
    | DelTodo String
    | ToggleTodo String


type Msg
    = InputTodoField String
    | SubmitTodo String
    | ApiMsg ApiMsg
    | TodoMsg TodoMsg
