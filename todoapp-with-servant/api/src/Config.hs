{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Config where

import           Servant                     (Handler)
import           Control.Monad.Reader        (ReaderT, asks)
import           Control.Monad.IO.Class      (liftIO)
import           Data.ByteString             (ByteString)
import           Data.Pool                   (Pool)
import           Data.Text                   (Text)
import           Data.Version                (Version, makeVersion)
import           Database.Persist.Sql        (SqlBackend)
import           Network.Wai.Middleware.Cors (Origin)
import           Database.Persist.Sqlite     (runSqlPool)


data Config =
  Config { dbpool :: Pool SqlBackend }

type AppM =
  ReaderT Config Handler


feHost :: Origin
feHost =
  "http://localhost:8000"


apiVersion :: Version
apiVersion =
  makeVersion [1, 0, 0]


filePath :: FilePath
filePath =
  "todos.json"


defaultPort :: Int
defaultPort =
  3030


sqlConnection :: Text
sqlConnection =
  "test.sqlite" 


corsMethods :: [ByteString]
corsMethods =
  [ "GET" , "PUT" , "DELETE" , "OPTIONS" ]


getPool :: AppM (Pool SqlBackend)
getPool = 
  asks dbpool


runDB :: ReaderT SqlBackend IO a -> AppM a
runDB action = do
  pool <- getPool
  liftIO $ runSqlPool action pool

