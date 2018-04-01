{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE DuplicateRecordFields      #-}

module TodoActions where

import           GHC.Int                 (Int64)
import           Database.Persist        (Entity(Entity),
                                          (==.))

import           Database.Persist.Sqlite (toSqlKey, fromSqlKey, selectList,
                                          selectFirst, deleteWhere,
                                          insert, replace)

import           Servant                 (NoContent(NoContent))

import qualified Models.DbModel as Db
import qualified Models.ApiModel as Api
import           Config                  (AppM, runDB)


toApiModel :: Entity Db.Todo -> Api.Todo
toApiModel (Entity key todo) =
  Api.Todo (fromSqlKey key) (Db.todoValue todo) (Db.todoDone todo)


getTodos :: AppM [Api.Todo]
getTodos = do
  todos :: [Entity Db.Todo] <- runDB $ selectList [] []
  return $ map toApiModel todos


getTodoById :: Int64 -> AppM (Maybe (Api.Todo))
getTodoById todoId = do
  todo <- runDB $ selectFirst [Db.TodoId ==. (toSqlKey todoId)] []
  return $ fmap toApiModel todo


putTodo :: Api.NewTodo -> AppM (Api.Todo)
putTodo todoVal = do
  let newTodo = Db.Todo (Api.value (todoVal :: Api.NewTodo)) False
  todoKey <- runDB $ insert newTodo
  return $ Api.Todo (fromSqlKey todoKey) (Api.value (todoVal :: Api.NewTodo)) False


delTodo :: Int64 -> AppM NoContent
delTodo todoId = do
  runDB $ deleteWhere [Db.TodoId ==. (toSqlKey todoId)]
  return NoContent


updateTodo :: Int64 -> Api.Todo -> AppM NoContent
updateTodo todoId todo = do
  let dbTodo = Db.Todo (Api.value (todo :: Api.Todo)) (Api.done todo)
  runDB $ replace (toSqlKey todoId :: Db.TodoId) dbTodo
  return NoContent

