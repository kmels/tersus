module Tersus.DataTypes.TFile where

import Control.Monad(join)
import Data.ByteString
import Data.Text
import Database.Redis
import Prelude
import Tersus.DataTypes.Permission
import Tersus.DataTypes.TError
import Tersus.DataTypes.TypeSynonyms
import Tersus.Database
import Tersus.Filesystem
--import Tersus.Responses(errorResponse)
--yesod
import Yesod.Handler

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

-- | Given a path, finds an id. If the path doesn't correspond to any file, an error response is thrown.

findFileId :: Connection -> Path -> GHandler s m FileId
findFileId conn filePath = do
  maybeFileId <- io $ getFileId conn filePath
  --TODOmaybe (return . errorResponse $ TFilePathNotFound filePath) return maybeFileId
  maybe notFound return maybeFileId
  