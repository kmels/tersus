{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Handler.Home where

import Import
import Tersus.DataTypes.TApplication
import Tersus.TApplications
import Yesod.Auth
-- Respondes to URL: /
getHomeR :: Handler RepHtml
getHomeR = do
    maid <- maybeAuthId
    --
    master <- getYesod
    let conn = redisConnection master
    tapps <- liftIO $ getApplications conn --runDB $ selectList [] [Desc TApplicationIdentifier]
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
