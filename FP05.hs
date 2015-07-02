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
  , language :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON Repo
instance FromJSON Repo

repo :: IO (Either String Repo)
repo = do
  initReq <- parseUrl $ "https://api.bitbucket.org/2.0/repositories/jwesleysmith/aws-scala"
  let req = initReq { requestHeaders = [("User-Agent", "FP-Catchup")] }
  res <- withManager $ httpLbs req
  return . eitherDecode $ responseBody res

main :: IO ()
main = do
  r <- repo
  case r of
    Left err -> putStrLn err
    Right r'  -> print r'
