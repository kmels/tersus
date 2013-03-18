----------------------------------------------------------------------------
-- |
-- Module      :  Tersus.Filesystem
-- Copyright   :  (c) Carlos López-Camey, Ernesto Rodriguez, 
-- License     :  
--
-- Maintainer  :  c.lopez@kmels.net, neto@netowork.me
-- Stability   :  stable
--
--
-- Functions to interact with the OS filesystem
-----------------------------------------------------------------------------


module Tersus.Filesystem(
  --constants
  tersus_dir, apps_dir, users_dir,
  --path
  pathToString, pathToText, pathToByteString,
  --path creators
  -- full paths
  fullStrPath,  
  --fs actions
  getPathContents, writePathContents, deletePath, 
  pathIsDir, existsPath,
  -- content type
  pathContentType, filenameContentType,
  module System.Directory
) where

import Prelude

--os
import System.IO.Unsafe
import System.Directory(getAppUserDataDirectory)
import System.Directory             (createDirectoryIfMissing,getDirectoryContents,doesDirectoryExist,doesFileExist, removeFile, removeDirectory)

-- control
import Control.Monad(when)
--types
import Data.ByteString hiding (putStrLn)
import Data.Text                    as T
import Data.Text.Encoding

-- tersus
import Tersus.DataTypes.TypeSynonyms
import Tersus.Debug
-- content types
import Text.Regex.TDFA
import Yesod.Content
-- yesod
--import Yesod.Content(Copyright
-- | Directory where tersus data is saved in the fileystem, DEPRECATE for tersusDir
tersus_dir :: String
tersus_dir = unsafePerformIO $ getAppUserDataDirectory "tersus-data"
apps_dir :: Text
apps_dir = "apps"
users_dir :: Text
users_dir = "users"

-- | Creates necessary directories in the filesystem for the given file path
makeDir :: Path -> IO ()
makeDir path = do
  putStrLn $ "Creating dir " ++ parent_dir
  createDirectoryIfMissing create_parents parent_dir
  where
    create_parents = True
    parent_dir = (fullStrPath . Prelude.init $ path) 

-- | Converts a list of path components into a Text by interacalating a "/"
pathToText :: Path -> Text
pathToText p = (T.pack "/") `T.append` T.intercalate (T.pack "/") p

-- | Converts a list of path components into a ByteString by interacalating a "/"
pathToByteString :: Path -> ByteString
pathToByteString = textToByteString . pathToText 

-- | Converts a list of path components into a String by interacalating a "/"
pathToString :: Path -> String
pathToString = T.unpack . pathToText

byteStringToText :: ByteString -> Text
byteStringToText = decodeUtf8

textToByteString :: Text -> ByteString
textToByteString = encodeUtf8

-- | Returns the user directory in the filesystem
userDirPath :: Username -> Path
userDirPath uname = T.pack tersus_dir : [T.pack "users", uname]

-- | Given an applications identifier, returns a full path for it in the filesystem
tAppDirPath :: ApplicationIdentifier -> Path
tAppDirPath appIdentifier = T.pack tersus_dir : [apps_dir, appIdentifier]
  
-- | Given a file's path and a user, returns a full path in the filesystem
fullPathForUser :: Path -> Username -> Path
fullPathForUser filePath username = 
  let 
    userDirPath' = userDirPath username
  in userDirPath' ++ filePath

-- | Writes a file to the filesystem, creates necessary directories
writePathContents :: Path -> Text -> IO ()
writePathContents path content = do
  makeDir path
  Prelude.writeFile (fullStrPath path) (T.unpack content)

-- | Gets a list of file names, Path must be a directory
getPathContents :: Path -> IO [String]
getPathContents path = do
  filenames <- getDirectoryContents . fullStrPath $ path
  return $ Prelude.filter notDots filenames

pathIsDir :: Path -> IO Bool
pathIsDir = doesDirectoryExist . fullStrPath 

existsPath :: Path -> IO Bool
existsPath = doesFileExist . fullStrPath 

notDots :: String -> Bool
notDots "." = False
notDots ".." = False
notDots _ = True

fullPath :: Path -> Path
fullPath p = (T.pack tersus_dir):p

fullStrPath :: Path -> String
fullStrPath = pathToString . fullPath

-- | Matches a resource given as name and path with the mime type
-- of the resource. The mime type is matched using the extension
-- of the file.
-- TODO: Change signature to Maybe ContentType
pathContentType :: Path -> ContentType
pathContentType = filenameContentType . T.unpack . Prelude.last

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

-- | Deletes a file, a directory with its contents too.
deletePath :: Path -> IO ()
deletePath p = do
  debugM $ "Deleting path " ++ show p
  is_dir <- pathIsDir p
  when (is_dir) $ do
    children <- getPathContents p
    mapM_ (\fn -> deletePath $ p++[T.pack fn]) children
    removeDirectory $ fullStrPath p
  debugM $ "Deleting file " ++ fullStrPath p
  (when $ not is_dir) $ 
    removeFile $ fullStrPath p
