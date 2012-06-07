{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.TApplication where

import           Import
import           Data.Maybe(fromMaybe)
import           Yesod
import           Yesod.Form.Jquery\

import qualified Codec.Binary.UTF8.String as BinaryUTF8

import qualified Codec.Crypto.AES         as AES
import           Codec.Crypto.AES.Random(randBytes)
import           Control.Arrow     (first, (&&&), (***))
import qualified Data.ByteString          as B
import           Data.Time.Clock   (getCurrentTime)
import           qualified Data.Text as T
--getAppKey =

-- The data type that is expected from registerAppForm

data AppLike = AppLike {
  appLikeName :: Text
  , appLikeDescription :: Text
  , appLikeRepositoryURL :: Maybe Text
  , appLikeContactEmail :: Text
} deriving Show

-- A monadic form
registerAppForm :: Html -> MForm App App (FormResult AppLike,Widget)
registerAppForm extra = do
  (nameRes, nameView) <- mreq textField "{-This is not used-}" Nothing
  (descriptionRes, descriptionView) <- mreq textField "{-This is not used-}" Nothing
  (repositoryUrlRes, repositoryUrlView) <- mopt textField "RepositoryURL" Nothing
  (contactEmailRes, contactEmailView) <- mreq textField "ContactEmail" Nothing
  let appLikeResult = AppLike <$> nameRes <*> descriptionRes <*> repositoryUrlRes <*> contactEmailRes
  let widget = do
            toWidget [lucius|
            ##{fvId contactEmailView} {
            width: 3em;
            }|]
            [whamlet|#{extra}
              <p>
              App name: #
              ^{fvInput nameView}
              <p>
              Description: : #
              ^{fvInput descriptionView}
              <p>
              If open source, repository url is: ^{fvInput repositoryUrlView} #
              <p>
              contact email ^{fvInput contactEmailView}
              <p>
             |]
  return (appLikeResult, widget)

getRegisterTAppR :: Handler RepHtml
getRegisterTAppR = do
  (formWidget, enctype) <- generateFormPost registerAppForm
  defaultLayout [whamlet|
                 <h1>Register your app:
                 <form method=post action=@{RegisterTAppR} enctype=#{enctype}>
                              ^{formWidget}
                              <input type=submit value="Createapp">
                              |]
   
postRegisterTAppR :: Handler RepHtml
postRegisterTAppR = do
  ((result, widget), enctype) <- runFormPost registerAppForm
  case result of
    FormSuccess appLike -> do
      -- get data from the form
      let 
        (appName,(appDescription,(appRepositoryUrl,appContactEmail))) = (appLikeName &&& appLikeDescription &&& appLikeRepositoryURL &&& appLikeContactEmail) appLike
                
      creationDate <- liftIO getCurrentTime --ask date
      appKey <- liftIO $ newAppRandomKey  --create a new appkey
      
      --insert in database
      appid <- runDB $ insert $ TApplication appName appDescription appRepositoryUrl appContactEmail creationDate appKey
      
      defaultLayout $ [whamlet|
      <h1>Received, your generated key: #{appKey}
       |]
    _ -> defaultLayout $ [whamlet|<p>Invalid input|]

-- | Generates a new random application key (32 bytes)
-- This one is saved in the database for each application. It is then used as a seed to generate a access_token for a (user,app) combo.
newAppRandomKey :: IO Text
newAppRandomKey = do 
  byteString <- randBytes 32 --generate 32 random bytes
  let 
    decodedString = BinaryUTF8.decode $ B.unpack byteString --decode [word8] to utf8
  return $ T.pack $ decodedString
  