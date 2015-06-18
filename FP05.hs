{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Data.ByteString (ByteString)
import Network.HTTP.Conduit
import Data.Aeson
import Data.Time.Clock (UTCTime)
import Data.Text (Text)
import GHC.Generics

data Repo = Repo
  { name :: Text
  , description :: Text
  , created_at :: UTCTime
  , language :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON Repo

repos :: String -> IO (Either String [Repo])
repos name = do
  initReq <- parseUrl $ "https://api.github.com/users/" ++ name ++ "/repos"
  let req = initReq { requestHeaders = [("User-Agent", "FP-Catchup")] }
  res <- withManager $ httpLbs req
  return . eitherDecode $ responseBody res

main :: IO ()
main = do
  rs <- repos "srijs"
  case rs of
    Left err -> putStrLn err
    Right rs'  -> mapM_ print $ take 5 rs'
