{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.TFile where

import qualified Codec.Binary.UTF8.String as UTF8
import qualified Codec.Crypto.AES         as AES
import           Control.Applicative      ((<$>), (<*>))
import qualified Data.ByteString          as B
import           Data.Text                as T
import qualified Data.Text.Encoding       as TE
import           Import
import           Yesod
import           Yesod.Form.Jquery

{- Handler methods for operations on files. -}

-- A way to convert between urls and a file path.
-- See: Dynamic multi in http://www.yesodweb.com/book/routing-and-handlers
data TFilePath = TFilePath [Text]  -- 2 or more
instance PathMultiPiece TFilePath where
    toPathMultiPiece (TFilePath anyPath) = anyPath
    fromPathMultiPiece (p:ps) = Just $ TFilePath ([p] ++ ps)
    fromPathMultiPiece _ = Nothing

getFileR :: Text -> AccessToken -> Path -> Handler RepHtml
getFileR username appKey path = do
  defaultLayout $ do
    [whamlet|
     <h1>Username: #{username}
     <h1>AppKey: #{appKey}
     <h1>Path: #{show path}
            |]


-- Form to handle writing to files.
--
-- The **HTML** form (i.e. GET) is only used for testing reasons, the value is used to handle
-- requests coming from REST (i.e. POST)
--
--

--Use standard english (to i18n, supply a translation function)
data WFileLike = WFileLike{
  fileContentContent :: Text
  , fileLikeAccessToken :: AccessToken
} deriving Show

writeFileForm :: AccessToken -> Html -> MForm App App (FormResult WFileLike, Widget)
writeFileForm accessToken = renderDivs $ WFileLike
    <$> areq textField "Content" Nothing
    <*> areq hiddenField "AccessToken" Nothing

-- This won't really exist after, it is used for testing purposes only.
getWriteFileR :: Username -> AccessToken -> Path -> Handler RepHtml
getWriteFileR username accessToken path = do
  --get the form
  (formWidget, enctype) <- generateFormPost $ writeFileForm accessToken
  defaultLayout [whamlet|
     <h1>Dear user #{username}, you are going to write content to file #{show path}
     <form method=post action=@{WriteFileR username accessToken path} enctype=#{enctype}>
                  ^{formWidget}
                  <input type=submit>
     |]

aesUserAppKey :: User -> AccessToken -> AES.Direction -> Maybe T.Text
aesUserAppKey user appkey dir = do
  splitIndex <- T.findIndex ((==) ':') $ TE.decodeUtf8 decryptedRawAuth
  (userNickname,applicationName) <- Just $ B.splitAt splitIndex decryptedRawAuth
  return $ TE.decodeUtf8 applicationName
  where
    decryptedRawAuth :: B.ByteString
    decryptedRawAuth = let -- in the form of "$usernameNickname:$userApplication"
      aesKey = B.pack . UTF8.encode $ "01234567890abcdef" --16,24 or 32 byte symmetric key
      aesIV = B.pack . UTF8.encode $ "tersus>>=lorenzo" -- initialization vector, 16 byte
      in AES.crypt' AES.CTR aesKey aesIV dir (TE.encodeUtf8 appkey)

-- | Returns the app responsible for the request, from the AccessToken,
-- this is our security layer used for incoming requests.
--decryptUserAppAuth :: User -> AccessToken -> B.ByteString
--decryptUserAppAuth u a = aesUserAppKey u a AES.Decrypt

-- | Generate an app key for a given user and an application name
-- this will the key that an application will use to make requests
--encryptUserAppAuth :: User -> ApplicationName -> AccessToken
--encryptUserAppAuth u a = aesUserAppKey u a AES.Encrypt

  --username = usernameNickname user
postWriteFileR :: Username -> AccessToken -> Path -> Handler RepHtml
postWriteFileR username accessToken path = do
  user <- runDB $ getBy $ UniqueNickname $ username --find user by username
  case user of
    -- Do we have a user?
    Just (Entity uid _ ) -> do
      -- process form
      ((result, widget), enctype) <- runFormPost $ writeFileForm accessToken
      --      file <- runDB $ insert $ Email "asdf" (Just "zasdf") (Just "as")
      --  let file = user >>= \u -> Just $ TFile u
      case result of
        FormSuccess fc -> defaultLayout $ do
          [whamlet|
           <h1>Lieber #{show $ user}
           <h1>You wrote: #{show $ fileContentContent fc}
           <h3>in #{show path}
              |]
        _ -> defaultLayout [whamlet|<p>Invalid input!|]
   -- username in url doesn't exist
    Nothing -> defaultLayout $ do
        [whamlet|
         <h1> No user|]
{-  where
    -- | Returns a list of this file ancestors.
    fileAncestors :: Path -> [Maybe Id]
    fileAncestors xs = let x = 1 in []
-}
