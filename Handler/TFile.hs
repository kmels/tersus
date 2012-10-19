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
import Prelude                      (writeFile,last)
import System.Directory             (createDirectoryIfMissing,getDirectoryContents,doesDirectoryExist,doesFileExist)
import           Text.Regex.TDFA
import           Data.ByteString          (ByteString)
import Data.Aeson as J
import Yesod.Json(Value(..))
import Model.TersusResult
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
      let fsPath = T.unpack . pathToText $ path `fullPathForUser` username''
      liftIO $ putStrLn $ "Looking: " ++ fsPath
      isDirectory <- liftIO $ doesDirectoryExist fsPath
      fileExists <- liftIO $ doesFileExist fsPath
      
      if isDirectory
      then liftIO $ getDirectoryContents fsPath >>= \fs -> return $ (jsonContentType, toContent $ FileList fs)
      else if fileExists
      then return $ (filenameContentType fsPath, ContentFile fsPath Nothing)
      else return $ (typeJson, toContent . toJSON $ fileDoesNotExistError)
    _ -> return $ (typeJson, toContent ("todo: error, invalid access key for user" :: String))

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
        let userLocalPath = userDirPath username'
            fileLocalPath = userLocalPath ++ filePath

        liftIO $ writeFileContents fileLocalPath content'
        jsonToRepJson $ (show "Wrote file "++(T.unpack $ pathToText fileLocalPath))

      Nothing -> jsonToRepJson $ (show "No content provided")
    Nothing -> jsonToRepJson $ (show "No user could be deduced")

-- | Creates necessary directories in the filesystem for the given file path
mkDirsFor :: Path -> IO ()
mkDirsFor path = createDirectoryIfMissing createParents (T.unpack dir)
                 where
                   createParents = True
		   init' [x] = []
		   init' (x:xs) =  x : init' xs
		   init' [] =  error "ERROR: mkDirsFor init"
                   dir = pathToText (init' path)

-- | Writes a file to the filesystem
writeFileContents :: Path -> Text -> IO ()
writeFileContents path content = mkDirsFor path >>= \_ -> writeFile (T.unpack . pathToText $ path) (T.unpack content)

-- | Converts a list of path components into a list by interacalating a "/"
pathToText :: Path -> Text
pathToText p = (T.pack "/") `T.append` T.intercalate (T.pack "/") p

-- | Returns the user director in the filesystem
userDirPath :: Username -> Path
userDirPath uname = (T.pack "tmp") : [uname]

fullPathForUser :: Path -> Username -> Path
fullPathForUser p u = userDirPath u ++ p

