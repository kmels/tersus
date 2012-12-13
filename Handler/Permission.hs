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

import           Import
import Tersus.AccessKeys(requireAccessKey)

putReadFilePermissionForUserR :: Username -> Path -> Handler RepJson
putReadFilePermissionForUserR username filePath = do
  --verify user
  accessKey <- requireAccessKey  
  jsonToRepJson $ show "TODO"

deleteReadFilePermissionForUserR :: Username -> Path -> Handler RepJson
deleteReadFilePermissionForUserR username filePath = jsonToRepJson $ show "TODO"

putWriteFilePermissionForUserR :: Username -> Path -> Handler RepJson
putWriteFilePermissionForUserR username filePath = jsonToRepJson $ show "TODO"

deleteWriteFilePermissionForUserR :: Username -> Path -> Handler RepJson
deleteWriteFilePermissionForUserR username filePath = jsonToRepJson $ show "TODO"

putShareFilePermissionForUserR :: Username -> Path -> Handler RepJson
putShareFilePermissionForUserR username filePath = jsonToRepJson $ show "TODO"

deleteShareFilePermissionForUserR :: Username -> Path -> Handler RepJson
deleteShareFilePermissionForUserR username filePath = jsonToRepJson $ show "TODO"

data PermissionType = READ | WRITE | RESHARE

