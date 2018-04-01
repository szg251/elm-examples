{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models.DbModel where

import           Data.Text           (Text)
import           GHC.Generics        (Generic)
import           Database.Persist.TH (mkPersist, mkMigrate,
                                      sqlSettings, share,
                                      persistLowerCase)

-- TODO

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Todo json
  value Text
  done Bool
  deriving Eq Show Generic
|]
