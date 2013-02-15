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

module Handler.Admin(
  getAdminR, -- admin dashboard
  getTApplicationsAdminR --list of applications
) where

import Import
import Data.Maybe
import Yesod.Auth
import           Control.Arrow            ((&&&))

import Model
import Handler.User(requireSuperAdmin)

getAdminR :: Handler RepHtml
getAdminR = do
  superAdmin <- requireSuperAdmin
  case superAdmin of
    Just admin -> defaultLayout $(widgetFile "admin/dashboard")
    _ -> defaultLayout [whamlet| "TODO Permission denied"|]

-- | replies to /admin/applications with a list of applications and links to manage it (edit,deactivate)
getTApplicationsAdminR :: Handler RepHtml 
getTApplicationsAdminR = do
  superAdmin <- requireSuperAdmin
  tapps <- runDB $ selectList [] [Desc TApplicationIdentifier]
  case superAdmin of
    Just admin -> defaultLayout $(widgetFile "admin/applications")
    _ -> defaultLayout [whamlet| "TODO Permission denied"|]
