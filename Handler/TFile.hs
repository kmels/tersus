{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.TFile where


import           Data.Maybe
import           Data.Text         as T
import           Handler.User      (getValidUser)
import           Import
import           Prelude           (writeFile)
import           System.Directory  (createDirectoryIfMissing)
import           Tersus.AccessKeys
{- Handler methods for operations on files. -}

-- A way to convert between urls and a file path.
-- See: Dynamic multi in http://www.yesodweb.com/book/routing-and-handlers
data TFilePath = TFilePath [Text]  -- 2 or more
instance PathMultiPiece TFilePath where
    toPathMultiPiece (TFilePath anyPath) = anyPath
    fromPathMultiPiece (p:ps) = Just $ TFilePath ([p] ++ ps)
    fromPathMultiPiece _ = Nothing

getFileR :: Text -> AccessKey -> Path -> Handler RepHtml
getFileR username' appKey path = do
  defaultLayout $ do
    [whamlet|
     <h1>Username: #{username'}
     <h1>AppKey: #{appKey}
     <h1>Path: #{show path}
            |]

-- Temporal function to test uploading of documents
postWriteFileR :: Username -> AccessKey -> Path -> Handler RepJson
postWriteFileR username' accessToken filePath = do
  maybeValidUser <- getValidUser username' accessToken
  content <- lookupPostParam "content"

  case maybeValidUser of
    Just user' -> case content of
      Just content' -> do
        let userLocalPath = userDirPath username'
            fileLocalPath = userLocalPath ++ filePath

        liftIO $ writeFileContents fileLocalPath content'
        jsonToRepJson $ (show "Wrote file "++(T.unpack $ pathToText fileLocalPath))

      Nothing -> jsonToRepJson $ (show "No content provided")
    Nothing -> jsonToRepJson $ (show "No user")

-- | Creates necessary directories in the filesystem for the given file path
mkDirsFor :: Path -> IO ()
mkDirsFor path = createDirectoryIfMissing True (T.unpack dir)
                 where
                   createParents = True
                   dir = T.pack ("/tmp/") `T.append` (pathToText . Import.reverse $ Import.drop 1 path)

-- | Writes a file to the filesystem
writeFileContents :: Path -> Text -> IO ()
writeFileContents path content = mkDirsFor path >>= \_ -> writeFile (T.unpack . pathToText $ path) (T.unpack content)

-- | Converts a list of path components into a list by interacalating a "/"
pathToText :: Path -> Text
pathToText p = (T.pack "/") `T.append` T.intercalate (T.pack "/") p

-- | Returns the user director in the filesystem
userDirPath :: Username -> Path
userDirPath uname = (T.pack "tmp") : [uname]
