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

getAdminR :: Handler RepHtml
getAdminR = do
  superAdmin <- requireSuperAdmin
  case superAdmin of
    Just admin -> defaultLayout $(widgetFile "admin/dashboard")
    _ -> defaultLayout [whamlet| "TODO Permission denied"|]

getTApplicationsAdminR :: Handler RepHtml 
getTApplicationsAdminR = do
  superAdmin <- requireSuperAdmin
  tapps <- runDB $ selectList [] [Desc TApplicationIdentifier]
  case superAdmin of
    Just admin -> defaultLayout $(widgetFile "admin/applications")
    _ -> defaultLayout [whamlet| "TODO Permission denied"|]

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
  
               