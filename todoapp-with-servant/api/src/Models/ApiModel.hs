{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DuplicateRecordFields      #-}

module Models.ApiModel where

import           GHC.Int             (Int64)
import           Data.Text           (Text)
import           GHC.Generics        (Generic)
import           Data.Aeson          (FromJSON, ToJSON)
import           Servant.Elm         (ElmType)


-- TODO

data Todo = Todo 
  { id :: Int64
  , value :: Text
  , done :: Bool
  } deriving (Eq, Show, Generic)


instance FromJSON Todo
instance ToJSON Todo
instance ElmType Todo


data NewTodo = NewTodo
  { value :: Text } deriving Generic

instance FromJSON NewTodo 
instance ToJSON NewTodo 
instance ElmType NewTodo
