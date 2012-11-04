----------------------------------------------------------------------------
-- |
-- Module      :  Handler.Permission
-- Copyright   :  (c) Carlos López-Camey, Ernesto Rodriguez, 
-- License     :  
--
-- Maintainer  :  c.lopez@kmels.net, neto@netowork.me
-- Stability   :  stable
--
--
-- Functions for handling permission related requests
-----------------------------------------------------------------------------

module Handler.Permission where

import Import

putReadFilePermissionForUserR :: Username -> Path -> Handler RepJson
putReadFilePermissionForUserR username filePath = jsonToRepJson $ show "TODO"

deleteReadFilePermissionForUserR :: Username -> Path -> Handler RepJson
deleteReadFilePermissionForUserR username filePath = jsonToRepJson $ show "TODO"

