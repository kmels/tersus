{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Handler.Home where

import           Import
import           Yesod.Auth

-- Respondes to URL: /
getHomeR :: Handler RepHtml
getHomeR = do
    maid <- maybeAuthId
    tapps <- runDB $ selectList [] [Desc TApplicationIdentifier]
    defaultLayout $ do
      aDomId <- lift newIdent
      setTitle "Welcome To Yesod!"
      $(widgetFile "homepage")
