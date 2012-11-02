{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DoAndIfThenElse          #-}
{-# LANGUAGE UndecidableInstances          #-}
module Handler.TFile where


import Control.Exception.Extensible hiding (Handler, handle)
import Control.Monad(when)
import Data.Maybe
import Data.Text                    as T
import Handler.User                 (verifyUserKeyM)
import Import                       hiding (catch)
import Text.Regex.TDFA
import Data.ByteString          (ByteString)
import Data.Aeson as J
import Tersus.TFiles
import Yesod.Json(Value(..))
import Model.TersusResult

--OS file system
import Tersus.Filesystem
import System.Directory             (doesDirectoryExist,doesFileExist)

{- Handler methods for operations on files. -}

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
filenameContentType :: FilePath -> ContentType
filenameContentType f = let
  ext :: Maybe String
  ext = f =~~ ("\\.[a-zA-Z0-9]+$" :: String)
  in case ext of
    Just ".html" -> "text/html"
    Just ".js" -> "text/javascript"
    Just ".css" -> "text/css"
    _ -> "text/plain"

newtype JsonFileList = FileList [FilePath]

instance ToJSON JsonFileList where
  toJSON (FileList fs) = array (Import.filter (\f -> f /= "." && f /= "..") fs) -- drop the "." and ".."
  
instance ToContent JsonFileList where
  toContent = toContent . toJSON
              
getFileR :: Text -> AccessKey -> Path -> Handler (ContentType,Content)
getFileR username' accessToken path = do
  maybeValidUser <- accessToken `verifyUserKeyM` username'
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
           mapM addUserPath files

fileDoesNotExistError :: TRequestError
fileDoesNotExistError = TRequestError InexistentFile "File does not exist"

-- Temporal function to test uploading of documents
putFileR :: Username -> AccessKey -> Path -> Handler RepJson
putFileR username' accessToken filePath = do
  maybeUsername <- accessToken `verifyUserKeyM` username'
  content <- lookupPostParam "content"

  case maybeUsername of
    Just user' -> case content of
      Just content' -> do
        let 
          userLocalPath = userDirPath username'
          fsPath = filePath `fullPathForUser` username'

        liftIO $ writeFileContents fsPath content'
        jsonToRepJson $ (show "Wrote file "++(T.unpack $ pathToText fsPath))

      Nothing -> jsonToRepJson $ (show "No content provided")
    Nothing -> jsonToRepJson $ (show "No user could be deduced")

