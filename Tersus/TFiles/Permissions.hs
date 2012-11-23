----------------------------------------------------------------------------
-- |
-- Module      :  Tersus.TFiles.Permissions
-- Copyright   :  (c) Carlos López-Camey, Ernesto Rodriguez
-- License     :  
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
--
-- Contains functions on the permissions of files
-- The model permission is defined as follows:
-- Permission
--    user UserId
--    read Bool
--    write Bool
--    share Bool
--    app TApplicationId
-----------------------------------------------------------------------------
module Tersus.TFiles.Permissions where

import Import
import Database.Persist.GenericSql.Raw(SqlPersist(..))

-- | If a user has the power of sharing a file with others, then the
-- powers of writing and reading are certainly implicit
permissionToShare :: UserId -> TApplicationId -> Permission
permissionToShare uid tappid = 
  let
    write = True
    read = True
    share = True
  in Permission uid read write share tappid
  
permissionToShareId :: (YesodPersist m, YesodPersistBackend m ~ SqlPersist) => UserId -> TApplicationId -> GHandler s m PermissionId
permissionToShareId uid tappid = runDB $ insert $ permissionToShare uid tappid
