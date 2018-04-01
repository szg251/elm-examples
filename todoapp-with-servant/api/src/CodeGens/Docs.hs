{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}

module CodeGens.Docs where

import           Data.Version     (showVersion)
import           Servant.Docs
import           Api              (todoApi)
import           Config           (apiVersion)


docPath :: String
docPath =
  "README.md"

intro :: DocIntro
intro =
  DocIntro "Todo API documentation"
    [ "This is a simple REST API in Servant for the even simpler Todo App."
    , "Elm query functions and API documentations are generated (servant-elm, servant-docs)"
    , "API version: " ++ showVersion apiVersion
    ]


generate :: IO ()
generate = do
  putStrLn $ "Writing API documentation to " ++ docPath
  writeFile docPath $ markdown $ docsWithIntros [intro] todoApi
