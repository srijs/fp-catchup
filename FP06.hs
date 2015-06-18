{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Data.Monoid
import Data.String
import Data.Text

import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp

greet :: Text -> Text
greet name = "Hello, " <> name <> "!"

app :: Application
app req respond = case pathInfo req of
  ["greet", name] -> do
    respond $ responseLBS status200 [] (fromString . unpack $ greet name)
  _ -> error "not found"

main :: IO ()
main = run 6060 app
