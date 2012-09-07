{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.TApplication where

import           Control.Arrow             ((&&&))
import           Control.Monad             (when)
import           Control.Monad.Trans.Class (lift)
import           Data.Maybe                (isNothing)
import qualified Data.Text                 as T
import           Data.Time.Clock           (getCurrentTime)
import           Handler.Messages          (initApplication)
import           Handler.TApplication.Git  (pullChanges)
import           Import
import           Network.HTTP.Types        (status200)
--import           Network.Wai
import           Tersus.AccessKeys         (decompose, newAccessKey,
                                            newRandomKey)
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

getTAppHomeR :: ApplicationIdentifier -> AccessKey -> Handler RepHtml
getTAppHomeR appIdentifier key = do
  appMaybe <- runDB $ getBy404 $ UniqueIdentifier $ appIdentifier
  maybeUserId <- maybeAuth
  let keyAuth = decompose key
  if (isNothing keyAuth) then
    error "Invalid access key"
    else case appMaybe of
      Entity _ app' -> do --is there an app with this identifier?
        _ <- liftIO $ pullChanges app'
        case maybeUserId of
          Just (Entity userId user) -> return $ RepHtml $ ContentFile ("/tmp/" ++ (T.unpack appIdentifier) ++ "/index.html") Nothing

          Nothing -> defaultLayout $ do [whamlet|<h3>TODO: user not logged, application index of #{appIdentifier}|]
      x -> error $ "Not implemented yetxx; app doesn't exist" ++ (show x)

getTAppHomeAuthR :: ApplicationIdentifier -> Handler RepHtml
getTAppHomeAuthR appIdentifier = do
  appMaybe <- runDB $ getBy404 $ UniqueIdentifier $ appIdentifier --find app or return 404
  maybeUserId <- maybeAuth
  case appMaybe of
    Entity _ app' -> do 
      _ <- liftIO $ pullChanges app'
      case maybeUserId of
        Just (Entity userId user) -> do
          accessKey <- liftIO $ newAccessKey (userNickname user) (tApplicationIdentifier app')
          initApplication $ AppInstance (T.unpack $ userNickname user) (T.unpack $ appIdentifier)
          redirect $ TAppHomeR appIdentifier accessKey
        Nothing -> defaultLayout $ do [whamlet|<h3>TODO: user not logged, return application index of #{appIdentifier}|]
    v -> error $ "Exception in route TAppHomeAuthR: " ++ (show v)

