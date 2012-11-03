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


module Tersus.Filesystem where

--haskell platform
import Import
import Prelude                      (writeFile,last)

--os
import System.IO.Unsafe
import System.Directory(getAppUserDataDirectory)
import System.Directory             (createDirectoryIfMissing,getDirectoryContents,doesDirectoryExist,doesFileExist)

--types
import Data.Text                    as T

-- | Creates necessary directories in the filesystem for the given file path
makeDir :: Path -> IO ()
makeDir path = createDirectoryIfMissing createParents (T.unpack dir)
                 where
                   createParents = True
		   init' [x] = []
		   init' (x:xs) =  x : init' xs
		   init' [] =  error "ERROR: mkDirsFor init"
                   dir = pathToText (init' path)



-- | Writes a file to the filesystem
writeFileContents :: Path -> Text -> IO ()
writeFileContents path content = makeDir path >>= \_ -> writeFile (T.unpack . pathToText $ path) (T.unpack content)

-- | Converts a list of path components into a Text by interacalating a "/"
pathToText :: Path -> Text
pathToText p = (T.pack "/") `T.append` T.intercalate (T.pack "/") p

-- | Converts a list of path components into a String by interacalating a "/"
pathToString :: Path -> String
pathToString = T.unpack . pathToText

-- | Returns the user directory in the filesystem
userDirPath :: Username -> Path
userDirPath uname = T.pack localDataDir : [T.pack "users", uname]

-- | Directory where tersus data is saved in the fileystem
localDataDir :: String
localDataDir = unsafePerformIO $ getAppUserDataDirectory "tersus-data"

-- | Given an applications identifier, returns a full path for it in the filesystem
tAppDirPath :: ApplicationIdentifier -> Path
tAppDirPath appIdentifier = T.pack localDataDir : [T.pack "apps", appIdentifier]
  
-- | Given a file's path and a user, returns a full path in the filesystem
fullPathForUser :: Path -> Username -> Path
fullPathForUser filePath username = 
  let 
    userDirPath' = userDirPath username
  in userDirPath' ++ filePath
  