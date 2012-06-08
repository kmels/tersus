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
import qualified Data.Text as T

--getAppKey =

-- The data type that is expected from registerAppForm

data AppLike = AppLike {
  appLikeName :: Text
  , appLikeIdentifier :: Text
  , appLikeDescription :: Text
  , appLikeRepositoryURL :: Maybe Text
  , appLikeContactEmail :: Text
} deriving Show

-- A monadic form
registerAppForm :: Html -> MForm App App (FormResult AppLike,Widget)
registerAppForm extra = do
  (nameRes, nameView) <- mreq textField "{- not used -}" Nothing
  (descriptionRes, descriptionView) <- mreq textField "{- not used-}" Nothing
  (repositoryUrlRes, repositoryUrlView) <- mopt textField "{- not used -}" Nothing
  (contactEmailRes, contactEmailView) <- mreq textField "{- not used -}" Nothing
  (identifierRes, identifierView) <- mreq textField "{- not used -}" Nothing
  let appLikeResult = AppLike <$> nameRes <*> identifierRes <*> descriptionRes <*> repositoryUrlRes <*> contactEmailRes
  let widget = do
            toWidget [lucius|
            ##{fvId contactEmailView} {
            width: 3em;
            }|]
            [whamlet|#{extra}
              <p> App name: # ^{fvInput nameView}              
              <p> Url: ^{fvInput identifierView}
              <p> Description: : ^{fvInput descriptionView}
              <p> If open source, repository url is: ^{fvInput repositoryUrlView} #
              <p> contact email ^{fvInput contactEmailView}
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
        (appName,(appDescription,(appRepositoryUrl,(appContactEmail,appIdentifier)))) = (appLikeName &&& appLikeDescription &&& appLikeRepositoryURL &&& appLikeContactEmail &&& appLikeIdentifier) appLike
                
      creationDate <- liftIO getCurrentTime --ask date
      appKey <- liftIO $ newAppRandomKey  --create a new appkey
      
      --insert in database
      appid <- runDB $ insert $ TApplication appName appIdentifier appDescription appRepositoryUrl appContactEmail creationDate appKey
      
      defaultLayout $ [whamlet|
      <h1>Received, your generated key: #{appKey}
       |]
    _ -> defaultLayout $ [whamlet|<p>Invalid input|]

-- | Generates a new random application key (32 bytes)
-- This one is saved in the database for each application. It is then used as a seed to generate a access_token for a (user,app) combo.
newAppRandomKey :: IO Text
newAppRandomKey = (randBytes 32) {-generate 32 random bytes -} >>= \byteString -> return $ T.pack {- String -> Text -} $ BinaryUTF8.decode {-[word8] -> Utf8 String-} $ B.unpack {- ByteString -> [Word8]-} byteString 
  