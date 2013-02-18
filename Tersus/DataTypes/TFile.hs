module Tersus.DataTypes.TFile where

import Control.Monad(join)
import Data.ByteString
import Data.Text
import Database.Redis
import Prelude
import Tersus.DataTypes.Permission
import Tersus.DataTypes.TypeSynonyms
import Tersus.Database
import Tersus.Filesystem
data FileType = AFile | ADirectory deriving Show

data TFile = File {  
    fileId :: FileId
    , owner :: UserId
    , rawpath :: Text -- this one is meant to be unique in the db
    , parent :: Maybe FileId 
    , filename :: Text -- can be extracted from rawpath too
    , contentType :: Text
    , fileType :: FileType    
    , permissions :: [Permission]
  }

-- | Returns invalidPath if no file under that path exists
redisGetFileId :: Connection -> Path -> IO (Either Reply (Maybe ByteString))
redisGetFileId conn fp = 
  let
    pathToFile = pathToByteString fp
  in runRedis conn $ get $ "f" .> pathToFile <. "id"
  

-- | Returns invalidPath if no file under that path exists
getFileId :: Connection -> Path -> IO (Maybe FileId)
getFileId conn fp = do -- IO monad
  redisReply <- redisGetFileId conn fp
  return $ do --Maybe monad
    idByteString <- getRedisResponse redisReply :: Maybe ByteString
    return $ byteStringToInteger idByteString
