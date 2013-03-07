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
import Control.Arrow            ((&&&))

import Handler.User(requireSuperAdmin)
import Model
import Tersus.DataTypes.User
import Tersus.HandlerMachinery

getAdminR :: Handler RepHtml
getAdminR = do
  admin <- requireSuperAdmin
  defaultLayout $(widgetFile "admin/dashboard")

-- | replies to /admin/applications with a list of applications and links to manage it (edit,deactivate)
getTApplicationsAdminR :: Handler RepHtml 
getTApplicationsAdminR = do
  superAdmin <- requireSuperAdmin
  conn <- getConn
  tapps <- io $ getApplications conn
  defaultLayout $(widgetFile "admin/applications")
