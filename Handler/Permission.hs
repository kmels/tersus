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
--
-- Contains functions on the permissions of files
-- The model permission is defined as follows:

-----------------------------------------------------------------------------

module Handler.Permission where

import           Control.Monad.IO.Class
import qualified Data.Text as T
import           Data.Text.Encoding
import           Database.Redis
import           Import
import           Tersus.AccessKeys
import           Tersus.DataTypes
import           Tersus.Database((.>),(<.)) -- Ts for Tersus
import           Tersus.Filesystem
import           Yesod.Content
import           Yesod.Handler
import           Yesod.Json

-- | Returns 404 if the username doesn't exist.
-- Returns permissionDenied if filePath is not valid
putReadFilePermissionForUserR :: Username -> Path -> Handler RepJson
putReadFilePermissionForUserR = putPermission READ
  
deleteReadFilePermissionForUserR :: Username -> Path -> Handler RepJson
deleteReadFilePermissionForUserR = deletePermission READ

putWriteFilePermissionForUserR :: Username -> Path -> Handler RepJson
putWriteFilePermissionForUserR = putPermission WRITE

deleteWriteFilePermissionForUserR :: Username -> Path -> Handler RepJson
deleteWriteFilePermissionForUserR = deletePermission WRITE

putShareFilePermissionForUserR :: Username -> Path -> Handler RepJson
putShareFilePermissionForUserR = putPermission SHARE

deleteShareFilePermissionForUserR :: Username -> Path -> Handler RepJson
deleteShareFilePermissionForUserR = deletePermission SHARE

putPermission :: PermissionType -> Username -> Path -> Handler RepJson
putPermission permissionType username filePath = do
  --verify that the accesskey has the power to share filePath
  accessKey <- requireAccessKey
  {-(_,_,appEntity) <- (accessKey `requirePermission` SHARE) filePath
  Entity uid _ <- runDB $ getBy404 $ UniqueNickname $ username
  
  --add a new permission to file for `username`
  Entity fid file <- runDB $ getBy404 $ UniqueRawPath $ pathToText filePath -- 404 shouldn't be thrown, fp was already verified
  newPermission <- permissionConstructor uid (entityKey appEntity)
  _ <- runDB $ update fid [TFilePermissions =. newPermission : (tFilePermissions file)]  -}
  jsonToRepJson $ show $ "Added  permission" 

deletePermissionJSON :: PermissionType -> Username -> Path -> Handler RepJson
deletePermissionJSON pt username filePath = do

  --verify that the accesskey has the power to delete (share) filePath
  accessKey <- requireAccessKey
  (user,tapp) <- (accessKey `requirePermission` SHARE) filePath  
  
  -- get connection to database
  master <- getYesod
  let conn = redisConnection master
  
  -- delete existing permission to file for username and app
  fileID <- getFileId filePath
  liftIO $ deletePermission pt username filePath
    
  jsonToRepJson $ show $ "Added  permission"
  
-- | This function checks that a given access key has permission `permissionType` on the given file path. It returns a triple, to avoid refetching entities.
requirePermission :: AccessKey -> PermissionType -> Path -> GHandler s m (User,TApplication)
requirePermission ak pt fp = do
  ap@(user,app) <- requireValidAuthPair ak  
  permission <- getPermission pt (uid user) (identifier app) -- Either Reply BS
  
  either (permissionDenied "Not enough permissions on file for that accesskey") (\_ -> return ap) permission
    
  where
    pathToFile = pathToText -- :: Text, with the userid pre            
        
-- | If a user has the power of sharing a file with others, then the
-- powers of writing and reading are certainly implicit
{-permissionTo :: (YesodPersist m, YesodPersistBackend m ~ SqlPersist) => Bool -> Bool -> Bool -> UserId -> TApplicationId -> GHandler s m (Key (PersistEntityBackend Permission) Permission)
permissionTo write' read' share' uid tappid = do
  permission <- runDB $ selectFirst [PermissionUser ==. uid, PermissionApp ==. tappid, PermissionWrite ==. write', PermissionRead ==. read', PermissionShare ==. share'] []
  case permission of
    Just (Entity pid _) -> return pid
    _ -> do
      let p = let
            write = write'
            read = read'
            share = share'
            in Permission uid read write share tappid
      runDB $ insert p
      -}