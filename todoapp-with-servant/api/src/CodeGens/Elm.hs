{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE DataKinds         #-}

module CodeGens.Elm where

import           Data.Text                   (Text, pack, unpack, intercalate)
import           Data.Monoid                 ((<>))
import           Elm                         (toElmDecoderSource,
                                              toElmEncoderSource, toElmTypeSource)
import           Servant.Elm                 (Proxy (Proxy), ElmOptions,
                                              defElmOptions, UrlPrefix (..),
                                              urlPrefix, generateElmForAPIWith)
import           Models.ApiModel             (Todo, NewTodo)
import           Api                         (TodoApi)


elmPath :: String
elmPath =
  "../client/src/Requests.elm"

options :: ElmOptions
options = defElmOptions { urlPrefix = Static "http://localhost:3030" }

elmHeader :: String -> Text
elmHeader moduleName =
  "module " <> pack moduleName <> " exposing (..)\n\
  \\n\  
  \import Json.Decode exposing (..)\n\
  \import Json.Decode.Pipeline exposing (..)\n\
  \import Json.Encode\n\
  \import Http\n\
  \import String"


elmfile :: [Text]
elmfile =
          ( elmHeader "Requests" 
            : "type NoContent\n    = NoContent"
            : toElmTypeSource    (Proxy :: Proxy Todo)
            : toElmTypeSource    (Proxy :: Proxy NewTodo)
            : toElmDecoderSource (Proxy :: Proxy Todo)
            : toElmEncoderSource (Proxy :: Proxy Todo)
            : toElmEncoderSource (Proxy :: Proxy NewTodo)
            : generateElmForAPIWith options  (Proxy :: Proxy TodoApi))


generate :: IO ()
generate = do
  putStrLn $ "Writing Elm queries functions to " ++ elmPath
  writeFile elmPath $ (unpack . intercalate (pack "\n\n\n")) elmfile
