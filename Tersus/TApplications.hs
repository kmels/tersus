module Tersus.TApplications where

import Data.ByteString
import Data.Text.Encoding
import Database.Redis
import Prelude
import Tersus.DataTypes
import Tersus.Database

getApplications :: Connection -> IO [TApplication]
getApplications conn = runRedis conn $ do 
  return []
  
getApplication :: Connection -> ApplicationIdentifier -> IO (Maybe TApplication)
getApplication conn appid = runRedis conn $ do
  name <- get $ "tapps" <:> appid
  return $ TApp (decodeUtf8 name)  
  