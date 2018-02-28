module Msgs exposing (..)

import RemoteData exposing (WebData)
import Model exposing (..)


-- MESSAGES


type Msg
    = InputTodo String
    | SubmitTodo
    | DelTodo String
    | ToggleTodo String
    | AfterFetchTodos (WebData (List Todo))
    | AfterPutNewTodo String (WebData Todo)
    | AfterDeleteTodo (WebData (List Todo))
    | AfterPatchTodo (WebData Todo)
