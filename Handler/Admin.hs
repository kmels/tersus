----------------------------------------------------------------------------
-- |
-- Module      :  Admin
-- Copyright   :  (c) Carlos López-Camey
-- License     :  
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
--
-- The following methods are handlers for pages indented for super admins only, users who run a tersus platform.
-----------------------------------------------------------------------------

module Handler.Admin where

import Import
import Model
import Data.Maybe
import Yesod.Auth
import Control.Monad.Trans.Maybe
import Control.Monad(guard)
import           Control.Arrow            ((&&&))

import Database.Persist.Store

import Model
import Handler.TApplication

getAdminR :: Handler RepHtml
getAdminR = do
  superAdmin <- requireSuperAdmin
  case superAdmin of
    Just admin -> defaultLayout $(widgetFile "admin/dashboard")
    _ -> defaultLayout [whamlet| "TODO Permission denied"|]

getTApplicationEditR :: ApplicationIdentifier -> Handler RepHtml 
getTApplicationEditR appIdentifier = do
  Entity _ tapp <- runDB $ getBy404 $ UniqueIdentifier $ appIdentifier  
  (formWidget, enctype) <- generateFormPost $ tAppForm [] $ Just tapp
  defaultLayout $(widgetFile "admin/TApplication/edit")

-- | replies to /admin/applications with a list of applications and links to manage it (edit,deactivate)
getTApplicationsAdminR :: Handler RepHtml 
getTApplicationsAdminR = do
  superAdmin <- requireSuperAdmin
  tapps <- runDB $ selectList [] [Desc TApplicationIdentifier]
  case superAdmin of
    Just admin -> defaultLayout $(widgetFile "admin/applications")
    _ -> defaultLayout [whamlet| "TODO Permission denied"|]

-- | processes a form produced by TApplicationeditR GET
postTApplicationEditR :: ApplicationIdentifier -> Handler RepHtml
postTApplicationEditR appIdentifier = do
  Entity tappkey tapp <- runDB $ getBy404 $ UniqueIdentifier $ appIdentifier
  ((result, _), _) <- runFormPost $ tAppForm [] $ Just tapp
  case result of
    FormSuccess appLike -> do
      -- get data from the form
      let
        (appName,(appDescription,(appRepositoryUrl,(appContactEmail,appIdentifier')))) = (appLikeName &&& unTextarea . appLikeDescription &&& appLikeRepositoryURL &&& appLikeContactEmail &&& appLikeIdentifier) appLike
        
      --update in database
      _ <- runDB $ update tappkey [TApplicationName =. appName,TApplicationDescription =. appDescription,TApplicationRepositoryUrl =. appRepositoryUrl, TApplicationContactEmail =. appContactEmail, TApplicationIdentifier =. appIdentifier']
      getTApplicationsAdminR

    --form isn't success
    FormFailure errorMessages -> do
      liftIO $ putStrLn $ "FORM FAILURE"
      (formWidget, enctype) <- generateFormPost $ tAppForm errorMessages $ Just tapp
      defaultLayout $(widgetFile "admin/TApplication/edit")
    -- form missing
    _ -> do
      liftIO $ putStrLn $ "----------------------------------------"
      getTApplicationEditR appIdentifier

requireSuperAdmin :: ( YesodAuth m
             , b ~ YesodPersistBackend m
             , b ~ PersistEntityBackend val
             , Key b val ~ AuthId m
             , PersistStore b (GHandler s m)
             , PersistEntity val
             , YesodPersist m
             , val ~ UserGeneric b
             ) => GHandler s m (Maybe val)
requireSuperAdmin = runMaybeT $ do
  aid <- MaybeT $ maybeAuthId
  user   <- MaybeT $ runDB $ get aid
  guard (userIsSuperAdmin user)
  return user
  
               