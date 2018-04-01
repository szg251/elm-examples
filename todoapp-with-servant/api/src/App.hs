{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}

module App where

import           System.IO                   (hPutStrLn, stderr)
import           System.Environment          (lookupEnv)
import           Data.Maybe                  (maybe)
import           Network.Wai.Middleware.Cors (simpleCorsResourcePolicy, cors,
                                              corsMethods, corsOrigins,
                                              corsRequestHeaders)
                                    
import           Network.Wai.Handler.Warp    (defaultSettings, setPort,
                                              setBeforeMainLoop, runSettings)


import           Api                         (todoApi, server)
import           Control.Monad.Reader        (runReaderT)
import           Control.Monad.Logger        (runStdoutLoggingT)
import           Database.Persist.Sqlite     (createSqlitePool, runSqlPersistMPool,
                                              runMigration)
import           Servant.Server 
import           Config                      (AppM, Config(Config))
import qualified Config as Config

import           Models.DbModel              (migrateAll)


-- APP


run :: IO ()
run = do
  envPort <- lookupEnv "PORT"
  pool <- runStdoutLoggingT $ createSqlitePool Config.sqlConnection 10
  runSqlPersistMPool (runMigration migrateAll) pool
  let config = Config pool
  let port = maybe Config.defaultPort read envPort :: Int
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< (mkApp config)


appToHandler :: Config -> AppM a -> Handler a
appToHandler conf app = runReaderT app conf


mkApp :: Config -> IO Application
mkApp config =
  return
    $ cors (const $ Just policy)
    $ serve todoApi
    $ hoistServer todoApi (appToHandler config) server

  where policy = simpleCorsResourcePolicy
                  { corsOrigins =
                      Just ([ Config.feHost ], False)
                  , corsMethods =
                      Config.corsMethods
                  , corsRequestHeaders =
                      [ "content-type" ]
                  }

