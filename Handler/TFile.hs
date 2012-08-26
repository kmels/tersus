{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.TFile where


import           Control.Exception.Extensible hiding (Handler, handle)
import           Data.Maybe
import           Data.Text                    as T
import           Handler.User                 (verifyUserKeyM)
import           Import                       hiding (catch)
import           Prelude                      (writeFile)
import           System.Directory             (createDirectoryIfMissing)
{- Handler methods for operations on files. -}

-- A way to convert between urls and a file path.
-- See: Dynamic multi in http://www.yesodweb.com/book/routing-and-handlers
data TFilePath = TFilePath [Text]  -- 2 or more
instance PathMultiPiece TFilePath where
    toPathMultiPiece (TFilePath anyPath) = anyPath
    fromPathMultiPiece (p:ps) = Just $ TFilePath ([p] ++ ps)
    fromPathMultiPiece _ = Nothing

getFileR :: Text -> AccessKey -> Path -> Handler RepPlain
getFileR username' accessToken path = do
  maybeValidUser <- accessToken `verifyUserKeyM` username'
  case maybeValidUser of
    Just _ -> do
      let fsPath = T.unpack . pathToText $ path `fullPathForUser` username'
      liftIO $ putStrLn $ "Looking: " ++ fsPath
      return $ RepPlain $ ContentFile fsPath Nothing
    _ -> error("No user")

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

