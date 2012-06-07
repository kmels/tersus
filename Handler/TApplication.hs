{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.TApplication where

import           Import
import           Yesod
import           Yesod.Form.Jquery

--getAppKey =

-- The data type that is expected from registerAppForm

data ApplicationLike = ApplicationLike {
  applicationLikeName :: Text
  , applicationLikeDescription :: Text
  , applicationLikeRepositoryURL :: Maybe Text
  , applicationLikeContactEmail :: Maybe Text
} deriving Show

-- A monadic form
registerAppForm :: Html -> MForm App App (FormResult ApplicationLike,Widget)
registerAppForm extra = do
  (nameRes, nameView) <- mreq textField "{-This is not used-}" Nothing
  (descriptionRes, descriptionView) <- mreq textField "{-This is not used-}" Nothing
  (repositoryUrlRes, repositoryUrlView) <- mopt textField "RepositoryURL" Nothing
  (contactEmailRes, contactEmailView) <- mopt textField "ContactEmail" Nothing
  let applicationLikeResult = ApplicationLike <$> nameRes <*> descriptionRes <*> repositoryUrlRes <*> contactEmailRes
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
  return (applicationLikeResult, widget)

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
    FormSuccess applike -> defaultLayout $ do
      [whamlet|
       <h1>Received
       |]
    _ -> defaultLayout [whamlet|<p>Invalid input|]

