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



module Handler.TFile where


import Control.Exception.Extensible hiding (Handler, handle)

import Data.Aeson                   as J
import Data.ByteString              (ByteString)
import Data.Maybe
import Data.Text                    as T
import Handler.User                 (verifyUserKeyM)
import Import                       hiding (catch)
import Tersus.TFiles
import Text.Regex.TDFA
import Yesod.Json                   (Value (..))
import Prelude
--OS file system
import System.Directory             (doesDirectoryExist,
                                               doesFileExist)
import Tersus.Filesystem (byteStringToText,fullPathForUser,pathToText,pathToString,userDirPath,writeFileContents)
import Tersus.Global                (accessKeyParameterName)
import Tersus.AccessKeys (requireAccessKey)

-- Control
import Control.Monad.Trans.Maybe
import Control.Monad                (when)

-- Tersus
import Handler.Permission
import Tersus.AccessKeys(decomposeM)
import Tersus.DataTypes
import Tersus.Responses

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
  permissionDenied "TODO"
{-  accessKey <- requireAccessKey
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
      else return $ (typeJson, toContent . toJSON $ fileDoesNotExistError)
    _ -> return $ (typeJson, toContent ("todo: error, invalid access key for user" :: String))
    where
        addUserPath (Resource n _ t) = return $ Resource n (pathToText path) t
        directoryContents fsPath = do
           files <- getDirectoryContentsTyped fsPath
           mapM addUserPath files -}

fileDoesNotExistError :: TersusResult
fileDoesNotExistError = TersusErrorResult InexistentFile "File does not exist"

-- | Handler that writes a new file, if successful
-- It is successful if and only if:
--   * The access key represents a user with permissions 
--   * The user has permissions iff he's the owner/has write permissions to the folder or file
-- Expected GET Parameters: "access_key" and content", the content of the file to write.
putFileR :: Username -> Path -> Handler RepJson
putFileR username' filePath = do
  permissionDenied "TODO" 
  --verify user
  {-accessKey <- requireAccessKey
  maybeUsername <- accessKey `verifyUserKeyM` username' --TODO: if accessKey is not provided, we should return an error (do it in a generic way)  

  --get content parameter
  content <- lookupPostParam "content"
  
  case maybeUsername of
    Just username -> case content of
      Just content' -> do
        let
          --get file path for the file
          userLocalPath = userDirPath username'
          fsPath = filePath `fullPathForUser` username'
          rawFilePath = pathToText filePath

        -- check if the file already exists
        file <- runDB $ getBy $ UniqueRawPath $ rawFilePath
                            
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
    Nothing -> jsonToRepJson $ (show "No user could be deduced")

  where
    getParent,getFilename :: Path -> Text
    getFilename = Prelude.last
    getParent = pathToText . Prelude.init
    fileType = File --TODO: Directory
    --only called if we are creating a new permission (iff inserting new file metadata). 
    applicationKey :: (YesodPersist m, YesodPersistBackend m ~ SqlPersist) => GHandler s m (Maybe TApplicationId)
    applicationKey = runMaybeT $ do 
      accessKey' <- MaybeT $ lookupGetParam accessKeyParameterName
      userAppAuthPair <- MaybeT $ decomposeM accessKey'
      app <- MaybeT $ runDB $ getBy $ UniqueIdentifier $ snd userAppAuthPair      
      MaybeT $ entityKeyM app -}
      
-- TODO: Move to Tersus.Helpers.Persistent or Tersus.Tranformers or something
{-entityKeyM :: Entity entity -> GHandler s m (Maybe (Key (PersistEntityBackend entity) entity))-}
entityKeyM e = return $ Just $ entityKey e
