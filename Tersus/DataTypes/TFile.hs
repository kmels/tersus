module Tersus.DataTypes.TFile where

import           Control.Applicative
import           Control.Monad(join,when)
import           Data.ByteString
import qualified           Data.ByteString.Char8 as Char8
import           Data.Maybe(isJust)
import           Data.Text
import qualified Data.Text as T
import           Data.Text.Encoding
import           Database.Redis
import           Prelude
import           Tersus.DataTypes.Permission
import           Tersus.DataTypes.TError
import           Tersus.DataTypes.TypeSynonyms
import           Tersus.Database
import           Tersus.Filesystem
--import Tersus.Responses(errorResponse)
--yesod
import           Yesod.Handler

data FileType = AFile | ADirectory deriving Show

data TFile = File {  
    fileId :: FileId
    , owner :: UserId
    , rawpath :: Text -- this one is meant to be unique in the db, should contain the owner preceded
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
  
getFile :: Connection -> FileId -> IO (Either TError TFile)
getFile conn fileID = runRedis conn $ do
  let fileid = integerToByteString fileID
  tfile <- multiExec $ do
    owner' <- get $ "tfile" .> fileid <. "owner"
    rawpath' <- get $ "tfile" .> fileid <. "rawpath"
    parent' <- get $ "tfile" .> fileid <. "parent"
    filename' <- get $ "tfile" .> fileid <. "filename"
    contentType' <- get $ "tfile" .> fileid <. "contentType"
    fileType' <- get $ "tfile" .> fileid <. "fileType"
    --sharePerm <- get $ "tfile" .> fileid <. "permissions" <.> "share"
    --readPerm <- get $ "tfile" .> fileid <. "permissions" <.> "read"
    return $ tFileFromRedis fileid <$> owner' <*> rawpath' <*> parent' <*> filename' <*> contentType' <*> fileType' 
  return $ case tfile of
    TxSuccess a -> case a of
      Just tfile -> Right tfile
      Nothing -> Left . TFileIdNotFound . byteStringToInteger $ fileid
    TxAborted -> Left . RedisTError $ "TxAborted"
    TxError msg -> Left . RedisTError . T.pack $ msg
    
tFileFromRedis :: ByteString -> MaybeBS -> MaybeBS -> MaybeBS -> MaybeBS -> MaybeBS -> MaybeBS -> Maybe TFile
tFileFromRedis fileid (Just o) (Just rp) (Just p) (Just fn) (Just ct) (Just ft) = Just $ File {
  fileId = byteStringToInteger fileid
  , owner = byteStringToInteger o
  , rawpath = decodeUtf8 rp
  , parent = if (p == "") then Nothing else Just . read . Char8.unpack $ p
  , filename = decodeUtf8 fn
  , contentType = decodeUtf8 ct
  , fileType = if (decodeUtf8 ft == "AFile") then AFile else ADirectory
  , permissions = [] --TODO
}
tFileFromRedis _ _ _ _ _ _ _ = Nothing

insertNewFile :: Connection -> UserId -> Path -> Maybe FileId -> Text -> Text -> FileType -> [Permission] -> IO (Maybe FileId)
insertNewFile conn ownerID rawPath maybeParentID filename' contentType' fileType' permissions = 
  let 
    -- encode fields to utf8, in a bytestring
    oidb = integerToByteString ownerID
    rp   = encodeUtf8 . pathToText $ rawPath
    mp   = maybe Char8.empty integerToByteString maybeParentID
    fn   = encodeUtf8 filename'
    ct   = encodeUtf8 contentType'
    ft   = Char8.pack . show $ fileType'
  in runRedis conn $ do
    fileID <- incr "files:max_id"
    case fileID of
      Left _ -> return Nothing
      Right fileid -> 
        let 
          fid = integerToByteString fileid 
        in do
          set ("tfile" .> fid <. "owner") oidb
          set ("tfile" .> fid <. "rawpath") rp
          set ("tfile" .> fid <. "parent") mp
          set ("tfile" .> fid <. "filename") fn
          set ("tfile" .> fid <. "contentType") ct
          set ("tfile" .> fid <. "fileType") ft          
          return . Just $ fileid