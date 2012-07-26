{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.TApplication where

import           Control.Arrow            ((&&&))
import qualified Data.Text                as T
import           Data.Time.Clock          (getCurrentTime)
import           Handler.TApplication.Git (pullChanges)
import           Import
import           Tersus.AccessKeys
import           Yesod.Auth

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
  ((result, _), _) <- runFormPost registerAppForm
  case result of
    FormSuccess appLike -> do
      -- get data from the form
      let
        (appName,(appDescription,(appRepositoryUrl,(appContactEmail,appIdentifier)))) = (appLikeName &&& appLikeDescription &&& appLikeRepositoryURL &&& appLikeContactEmail &&& appLikeIdentifier) appLike

      creationDate <- liftIO getCurrentTime --ask date
      appKey <- liftIO $ newRandomKey 32  --create a new appkey

      --insert in database
      _ <- runDB $ insert $ TApplication appName appIdentifier appDescription appRepositoryUrl appContactEmail creationDate appKey

      defaultLayout $ [whamlet|
      <h1>Received, your generated key: #{appKey}
       |]
    _ -> defaultLayout $ [whamlet|<p>Invalid input|]

getHomeTApplicationR :: ApplicationIdentifier -> Handler RepHtml
getHomeTApplicationR appIdentifier = do
  appMaybe <- runDB $ getBy $ UniqueIdentifier $ appIdentifier
  maybeUserId <- maybeAuth
  case appMaybe of
    Just (Entity _ app') -> do --is there an app with this identifier?
      _ <- liftIO $ pullChanges app'
      case maybeUserId of
        Just (Entity userId user) -> defaultLayout $ do
          accessKey <- liftIO $ newHexRandomAccessKey (userNickname user) (tApplicationIdentifier app')
          $(widgetFile "TApplication/application-root")
        Nothing -> defaultLayout $ do [whamlet| Welcome to the application #{appIdentifier}
                                                <p>Welcome stranger: |]
    _ -> error "Not implemented yet; app doesn't exist"

