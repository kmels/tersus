{-# LANGUAGE ImplicitParams #-}
module Tersus.DataTypes.TFile where

import           Control.Applicative
import           Control.Monad                 (join, when)
import           Data.ByteString
import qualified Data.ByteString.Char8         as Char8
import           Data.Maybe                    (isJust)
import           Data.Text
import qualified Data.Text                     as T
import           Data.Text.Encoding
import           Database.Redis
import           Prelude
import           Tersus.Database
import           Tersus.DataTypes.Permission
import           Tersus.DataTypes.TError
import           Tersus.DataTypes.TypeSynonyms
import           Tersus.Filesystem
--import Tersus.Responses(errorResponse)
--yesod
import           Yesod.Content
import           Yesod.Handler
data FileType = AFile | ADirectory deriving Show

data TFile = File {
    fileId        :: FileId
    , owner       :: UserId
    , rawpath     :: Text -- this one is meant to be unique in the db, should contain the owner preceded
    , filename    :: Text -- could be extracted from rawpath too
    , contentType :: Text
    , fileType    :: FileType
    , permissions :: [Permission]
  }

-- | Gets a TFile from a raw path. Remember, it must be preceded by the owner (e.g. appname or userid
tFileFromPath :: Connection -> Path -> IO (Either TError TFile)
tFileFromPath conn path = getFileId conn path >>= either (return . Left) (getFile conn)

-- | Returns invalidPath if no file under that path exists
redisGetFileId :: Connection -> Path -> IO (Either Reply (Maybe ByteString))
redisGetFileId conn fp =
  let
    pathToFile = pathToByteString fp
  in runRedis conn $ get $ "f" .> pathToFile <. "id"


-- | Returns invalidPath if no file under that path exists
getFileId :: Connection -> Path -> IO (Either TError FileId)
getFileId conn fp = do -- IO monad
  redisReply <- redisGetFileId conn fp -- Either Reply (Maybe ByteString)
  return $ case redisReply of
    Right maybeFID -> maybe (Left . TFilePathNotFound $ fp) (Right . byteStringToInteger) maybeFID
    Left reply -> Left . RedisTError . T.pack . show $ reply

-- | Given a path, finds an id. If the path doesn't correspond to any file, an error response is thrown.

findFileId :: Connection -> Path -> GHandler s m FileId
findFileId conn filePath = do
  maybeFileId <- io $ getFileId conn filePath
  --TODOmaybe (return . errorResponse $ TFilePathNotFound filePath) return maybeFileId
  either (\terror -> notFound) return maybeFileId

getFile :: Connection -> FileId -> IO (Either TError TFile)
getFile conn fileID = runRedis conn $ do
  let fileid = integerToByteString fileID
  tfile <- multiExec $ do
    owner' <- get $ "tfile" .> fileid <. "owner"
    rawpath' <- get $ "tfile" .> fileid <. "rawpath"
    filename' <- get $ "tfile" .> fileid <. "filename"
    contentType' <- get $ "tfile" .> fileid <. "contentType"
    fileType' <- get $ "tfile" .> fileid <. "fileType"
    --sharePerm <- get $ "tfile" .> fileid <. "permissions" <.> "share"
    --readPerm <- get $ "tfile" .> fileid <. "permissions" <.> "read"
    return $ tFileFromRedis fileid <$> owner' <*> rawpath' <*> filename' <*> contentType' <*> fileType'
  return $ case tfile of
    TxSuccess a -> case a of
      Just tfile -> Right tfile
      Nothing -> Left . TFileIdNotFound . byteStringToInteger $ fileid
    TxAborted -> Left . RedisTError $ "TxAborted"
    TxError msg -> Left . RedisTError . T.pack $ msg

tFileFromRedis :: ByteString -> MaybeBS -> MaybeBS -> MaybeBS -> MaybeBS -> MaybeBS -> Maybe TFile
tFileFromRedis fileid (Just o) (Just rp) (Just fn) (Just ct) (Just ft) = Just $ File {
  fileId = byteStringToInteger fileid
  , owner = byteStringToInteger o
  , rawpath = decodeUtf8 rp
  , filename = decodeUtf8 fn
  , contentType = decodeUtf8 ct
  , fileType = if (decodeUtf8 ft == "AFile") then AFile else ADirectory
  , permissions = [] --TODO
}
tFileFromRedis _ _ _ _ _ _ = Nothing

insertNewFile :: Connection -> UserId -> Path -> Text -> ContentType -> FileType -> [Permission] -> IO (Either TError FileId)
insertNewFile conn ownerID rawPath filename' contentType' fileType' permissions =
  let
    -- encode fields to utf8, in a bytestring
    oidb = integerToByteString ownerID
    rp   = encodeUtf8 . pathToText $ rawPath
    fn   = encodeUtf8 filename'
    ct   = contentType'
    ft   = Char8.pack . show $ fileType'
  in runRedis conn $ do
    fileID <- incr "files:max_id"
    case fileID of
      Left _ -> return . Left . RedisTError $ "Could not create index (id) for a new file"
      Right fileid ->
        let
          ?fid = integerToByteString fileid
        in do
          setProperty tFileOwner oidb
          setProperty tFileRawPath rp
          setProperty tFileFilename fn
          setProperty tFileContentType ct
          setProperty tFileFileType ft
          return . Right $ fileid

setProperty :: (?fid :: ByteString, RedisCtx m f) => (ByteString -> ByteString) -> ByteString -> m (f Status)
setProperty f p = set (f ?fid) p

tFileOwner fid  = ("tfile" .> fid <. "owner")
tFileRawPath fid =  ("tfile" .> fid <. "rawpath")
tFileFilename fid = ("tfile" .> fid <. "filename")
tFileContentType fid = ("tfile" .> fid <. "contentType")
tFileFileType fid = ("tfile" .> fid <. "fileType")
