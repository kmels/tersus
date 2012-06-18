{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.TApplication where

import           Control.Arrow   ((&&&))
import qualified Data.Text       as T
import           Data.Time.Clock (getCurrentTime)
import           Import

import           System.Random   (newStdGen, randomRs)

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
      appKey <- liftIO $ newAppRandomKey  --create a new appkey

      --insert in database
      _ <- runDB $ insert $ TApplication appName appIdentifier appDescription appRepositoryUrl appContactEmail creationDate appKey

      defaultLayout $ [whamlet|
      <h1>Received, your generated key: #{appKey}
       |]
    _ -> defaultLayout $ [whamlet|<p>Invalid input|]

getHomeTApplicationR :: ApplicationIdentifier -> Handler RepHtml
getHomeTApplicationR appIdentifier = do
--  _ <- maybeAuthId
  defaultLayout $ do [whamlet| Welcome to the application #{appIdentifier}|]

-- | Generate a random String of alphanumerical characters
-- (a-z, A-Z, and 0-9) of the given length using the given
-- random number generator. (Copied from hidden module Yesod.Internal.Request)
getRandomText :: Int -> IO Text
getRandomText n = do
  stdgen <- liftIO newStdGen
  return $ T.pack $ take n $ map toChar (randomRs (0, 61) stdgen) --32 chars
  where
    toChar i
      | i < 26 = toEnum $ i + fromEnum 'A'
      | i < 52 = toEnum $ i + fromEnum 'a' - 26
      | otherwise = toEnum $ i + fromEnum '0' - 52


-- | Generatate a 32-length random alphanumerical text.  This one is saved in the database for each application. It is then used as a seed to generate a access_token for a (user,app) combo.
newAppRandomKey :: IO Text
newAppRandomKey = getRandomText 32
