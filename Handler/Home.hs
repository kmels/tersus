{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Handler.Home where

import Import
import Tersus.DataTypes.TApplication
import Yesod.Auth
import Tersus.Debug

-- Respondes to URL: /
getHomeR :: Handler RepHtml
getHomeR = do
    maid <- maybeAuthId
    --
    master <- getYesod
    let conn = redisConnection master
    tapps <- liftIO $ getApplications conn
    liftIO $ debugM $ "getHomeR#tapps " ++ show tapps
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
