{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Handler.Home where

import Import
import Tersus.TApplications
import Yesod.Auth
-- Respondes to URL: /
getHomeR :: Handler RepHtml
getHomeR = do
    maid <- maybeAuthId
    --
    tapps <- getApplications --runDB $ selectList [] [Desc TApplicationIdentifier]
    defaultLayout $ do
      aDomId <- lift newIdent
      setTitle "Tersusland!"
      $(widgetFile "homepage")


-- | Responds to the url /about

getAboutR :: Handler RepHtml
getAboutR = defaultLayout $ do
  aDomId <- lift newIdent
  setTitle "About!"
  $(widgetFile "about")
