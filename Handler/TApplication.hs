{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.TApplication where

import Import
import Data.Maybe(fromMaybe)
import Yesod
import Yesod.Form.Jquery\

import Control.Arrow     (first, (&&&), (***))
import Data.Time.Clock   (getCurrentTime)
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
    FormSuccess applike -> do
      let (appName,(appDescription,(appRepositoryUrl,appContactEmail))) = (appLikeName &&& appLikeDescription &&& appLikeRepositoryURL &&& appLikeContactEmail) applike
      creationDate <- liftIO getCurrentTime
      appid <- runDB $ insert $ TApplication appName appDescription appContactEmail appRepositoryUrl creationDate
      defaultLayout $ [whamlet|
      <h1>Received
       |]
    _ -> defaultLayout $ [whamlet|<p>Invalid input|]
