{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}


module Api where

import           GHC.Int                (Int64)
import           Servant
import           Servant.Docs

import           Models.ApiModel
import qualified TodoActions
import           Config                 (AppM)


-- API


type TodoApi =
  -- GET /todo Returns every todo
  "todo" :> Get '[JSON] [Todo] :<|>

  -- GET /todo/:todoId Returns the todo with the given ID
  "todo" :> Capture "todoId" Int64 :> Get '[JSON] (Maybe (Todo)) :<|>

  -- POST /todo Creates a new todo
  "todo" :> ReqBody '[JSON] NewTodo :> PostCreated '[JSON] (Todo) :<|>

  -- DELETE /todo/:todoid Deletes todo with the given ID
  "todo" :> Capture "todoId" Int64 :> DeleteAccepted '[JSON] NoContent :<|>

  -- PUT /todo/:todoid Replaces todo of the given ID
  "todo" :> Capture "todoId" Int64 :> ReqBody '[JSON] (Todo) :> PutAccepted '[JSON] NoContent


todoApi :: Proxy TodoApi
todoApi =
  Proxy


server :: ServerT TodoApi AppM
server =
  TodoActions.getTodos :<|>
  TodoActions.getTodoById :<|>
  TodoActions.putTodo :<|>
  TodoActions.delTodo :<|>
  TodoActions.updateTodo


-- DOCS


instance ToCapture (Capture "todoId" Int64) where
  toCapture _ = DocCapture "todoId" "(integer) Todo ID"

instance ToSample Int64 where
  toSamples _ = singleSample 1

instance ToSample Todo where
  toSamples _ = singleSample (Todo 0 "buy me a beer" False)

instance ToSample NewTodo where
  toSamples _ = singleSample (NewTodo "buy me a beer")

