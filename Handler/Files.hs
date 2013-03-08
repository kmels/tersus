----------------------------------------------------------------------------
-- |
-- Module      :  Tersus.TFiles.Permissions
-- Copyright   :  (c) Carlos López-Camey, Ernesto Rodriguez
-- License     :
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
--
-- Contains handler functions for the Files API
-----------------------------------------------------------------------------
{-# LANGUAGE DoAndIfThenElse       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Handler.Files where


import           Control.Exception.Extensible hiding (Handler, handle)
import           Data.Aeson                   as J
import           Data.ByteString              (ByteString)
import           Data.Maybe
import           Data.Text                    as T
import           Handler.User                 (verifyUserKeyM)
import           Import                       hiding (catch)
import           Prelude
import           Tersus.Database
import           Tersus.TFiles
import           Text.Regex.TDFA
import           Yesod.Json                   (Value (..))
--OS file system
import           System.Directory             (doesDirectoryExist,
                                               doesFileExist)
import           Tersus.AccessKeys            (requireAccessKey)
import           Tersus.Filesystem            (byteStringToText,
                                               fullPathForUser, pathToString,
                                               pathToText, userDirPath,
                                               writeFileContents)
import           Tersus.Global                (accessKeyParameterName)

-- Control
import           Control.Monad                (when)
import           Control.Monad.Trans.Maybe

-- Tersus
import           Control.Monad.Trans.Either
import           Handler.Permission
import           Tersus.AccessKeys
import           Tersus.DataTypes
import           Tersus.Responses
import           Tersus.Yesod.Handler

-- A way to convert between urls and a file path.
-- See: Dynamic multi in http://www.yesodweb.com/book/routing-and-handlers
data TFilePath = TFilePath [Text]  -- 2 or more
instance PathMultiPiece TFilePath where
    toPathMultiPiece (TFilePath anyPath) = anyPath
    fromPathMultiPiece (p:ps) = Just $ TFilePath ([p] ++ ps)
    fromPathMultiPiece _ = Nothing

-- | Returns the json content type according to RFC 4627
jsonContentType :: ContentType
jsonContentType = "application/json"

-- | Finds an appropiate mime-type given a filename (or path to filename) with an extension. Returns "text/plain" as default.
-- TODO: Change signature to Maybe ContentType
filenameContentType :: FilePath -> ContentType
filenameContentType f = let
  ext :: Maybe String
  ext = f =~~ ("\\.[a-zA-Z0-9]+$" :: String)
  in case ext of
    Just ".html" -> "text/html"
    Just ".js" -> "text/javascript"
    Just ".css" -> "text/css"
    _ -> "text/plain"

-- | Matches a resource given as name and path with the mime type
-- of the resource. The mime type is matched using the extension
-- of the file.
-- TODO: Change signature to Maybe ContentType
pathContentType :: Path -> ContentType
pathContentType = filenameContentType . T.unpack . Prelude.last

newtype JsonFileList = FileList [FilePath]

instance ToJSON JsonFileList where
  toJSON (FileList fs) = array (Import.filter (\f -> f /= "." && f /= "..") fs) -- drop the "." and ".."

instance ToContent JsonFileList where
  toContent = toContent . toJSON

getFileR :: Text -> Path -> Handler (ContentType,Content)
getFileR username' path = do
  -- the request must contain an access key
  accessKey <- requireAccessKey

  -- the access key must be well encoded, and valid
  maybeValidUser <- accessKey `verifyUserKeyM` username'
  case maybeValidUser of
    Just username'' -> do
      let fsPath = path `fullPathForUser` username''
      let fsPathStr = pathToString fsPath

      liftIO $ putStrLn $ "Looking: " ++ show fsPath
      isDirectory <- liftIO $ doesDirectoryExist fsPathStr
      fileExists <- liftIO $ doesFileExist fsPathStr
      if isDirectory
      then liftIO $ directoryContents fsPathStr >>= \fs -> return $ (jsonContentType, toContent $ fs)
      else if fileExists
      then return $ (filenameContentType fsPathStr, ContentFile fsPathStr Nothing)
      else fileDoesNotExistErrorResponse
    _ -> return $ (typeJson, toContent ("todo: error, invalid access key for user" :: String))
    where
        addUserPath (Resource n _ t) = return $ Resource n (pathToText path) t
        directoryContents fsPath = do
           files <- getDirectoryContentsTyped fsPath -- IO [Resource]
           mapM addUserPath files -- IO [Resource]

-- | Handler that writes a new file, if successful
-- It is successful if and only if:
--   * The access key represents a user with permissions
--   * The user has permissions iff he's the owner/has write permissions to the folder or file
-- Expected GET Parameters: "access_key" and content", the content of the file to write.
putFileR :: Username -> Path -> Handler RepJson
putFileR username' filePath = do
  -- the request must contain an access key
  accessKey <- requireAccessKey

  -- it must be well encoded, and be valid
  (user,tapplication) <- requireValidAuthPair accessKey --

  --get content parameter
  fileContent <- "content" `requireParameterOr` (MissingParameter "content" $ "The contents of the file you are writing in " `T.append` (pathToText filePath))

  master <- getYesod
  let
    -- path to file
    userLocalPath = userDirPath username'
    fsPath = filePath `fullPathForUser` username'
    rawFilePath = pathToText filePath
    -- get the connection to the db
    conn = redisConnection master

  -- find the file id based on its path
  eFileIdTError <- io $ getFileId conn filePath  -- Either TError FileID

  -- if we have the id, then try to fetch a TFile with it.
  -- if we don't have the id, insert a new TFile
  eTFileTError <- io $ case eFileIdTError of
    Right fid -> getFile conn fid
    Left err -> let
      filename' = Prelude.last filePath
      contentType' = pathContentType filePath
      in do
        eFileIdTError' <- insertNewFile conn (uid user) filePath filename' contentType' AFile []
        case eFileIdTError' of
          Left err -> return . Left $ err
          Right fid' -> getFile conn fid'

  io $ writeFileContents fsPath fileContent

  case eTFileTError of
    Right tfile -> jsonToRepJson $ (show "Wrote file "++(T.unpack $ pathToText fsPath))
    Left err -> tError err

  {-case maybeUsername of
    Just username -> case content of
      Just content' -> do

        fileMetadataId <- case file of
          -- if found, return the one that exists
          Just (Entity tfi tf) -> return . Just $ tfi

          -- if the file doesn't exist, create it :: Maybe TFile
          -- where the owner is the username in the request
          Nothing -> do
            --ensure that username from accesskey is in DB
            user <- runDB $ getBy $ UniqueNickname $ username

            case user of
              Just (Entity ownerid _) -> do
                parentTFile <- runDB $ getBy $ UniqueRawPath $ (getParent filePath)
                let
                  parentTFileID = case parentTFile of
                      Just (Entity tpfi _) -> Just tpfi
                      _ -> Nothing

                tappKey <- applicationKey
                sharePermission <- ownerid `permissionToShare` (fromJust tappKey)
                fm <- runDB $ insert $ TFile ownerid rawFilePath parentTFileID (getFilename filePath) ({- TODO, see contentTypeOf -} Just $ byteStringToText $ pathContentType filePath) fileType [sharePermission]
                return . Just $ fm
              _ -> return Nothing --no user found

        -- write to the file system
        liftIO $ writeFileContents fsPath content'
        $(logInfo) $ "Wrote file " `T.append` pathToText fsPath

        jsonToRepJson $ (show "Wrote file "++(T.unpack $ pathToText fsPath))

      Nothing -> jsonToRepJson $ (show "No content provided")
    Nothing -> jsonToRepJson $ (show "No user could be deduced") -}

  where
    getFilename :: Path -> Text
    getFilename = Prelude.last
    fileType = File --TODO: Directory
    --only called if we are creating a new permission (iff inserting new file metadata).
    {-applicationKey :: (YesodPersist m, YesodPersistBackend m ~ SqlPersist) => GHandler s m (Maybe TApplicationId)
    applicationKey = runMaybeT $ do
      accessKey' <- MaybeT $ lookupGetParam accessKeyParameterName
      userAppAuthPair <- MaybeT $ decomposeM accessKey'
      app <- MaybeT $ runDB $ getBy $ UniqueIdentifier $ snd userAppAuthPair
      MaybeT $ entityKeyM app -}

-- TODO: Move to Tersus.Helpers.Persistent or Tersus.Tranformers or something
{-entityKeyM :: Entity entity -> GHandler s m (Maybe (Key (PersistEntityBackend entity) entity))-}
entityKeyM e = return $ Just $ entityKey e
