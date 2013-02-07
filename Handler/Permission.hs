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
-- Permission
--    user UserId
--    read Bool
--    write Bool
--    share Bool
--    app TApplicationId
-----------------------------------------------------------------------------

{-# LANGUAGE DoAndIfThenElse       #-}

module Handler.Permission where

--Prelude
import Data.List(intersect)
import Database.Persist.GenericSql.Raw(SqlPersist(..))
import Import

-- Tersus
import Tersus.Responses(fileDoesNotExistError)
import Tersus.AccessKeys(requireAccessKey,reqValidAuthPair,requireValidAuthPairEntities)
import Tersus.Filesystem

-- | Returns 404 if the username doesn't exist.
-- Returns permissionDenied if filePath is not valid
putReadFilePermissionForUserR :: Username -> Path -> Handler RepJson
putReadFilePermissionForUserR = putPermission permissionToRead
  
deleteReadFilePermissionForUserR :: Username -> Path -> Handler RepJson
deleteReadFilePermissionForUserR = deletePermission permissionToRead

putWriteFilePermissionForUserR :: Username -> Path -> Handler RepJson
putWriteFilePermissionForUserR = putPermission permissionToWrite

deleteWriteFilePermissionForUserR :: Username -> Path -> Handler RepJson
deleteWriteFilePermissionForUserR = deletePermission permissionToWrite

putShareFilePermissionForUserR :: Username -> Path -> Handler RepJson
putShareFilePermissionForUserR = putPermission permissionToShare

deleteShareFilePermissionForUserR :: Username -> Path -> Handler RepJson
deleteShareFilePermissionForUserR = deletePermission permissionToShare

data PermissionType = READ | WRITE | SHARE deriving Show

putPermission :: (YesodPersist m, YesodPersistBackend m ~ SqlPersist, m ~ App, s ~ App) => (UserId -> TApplicationId -> GHandler s m (Key (PersistEntityBackend Permission) Permission)) -> Username -> Path -> Handler RepJson
putPermission permissionConstructor username filePath = do
  --verify that the accesskey has the power to share filePath
  accessKey <- requireAccessKey
  (_, appEntity) <- (accessKey `requirePermission` SHARE) filePath
  Entity uid _ <- runDB $ getBy404 $ UniqueNickname $ username
  
  --add a new permission to file for `username`
  Entity fid file <- runDB $ getBy404 $ UniqueRawPath $ pathToText filePath -- 404 shouldn't be thrown, fp was already verified
  newPermission <- permissionConstructor uid (entityKey appEntity)
  _ <- runDB $ update fid [TFilePermissions =. newPermission : (tFilePermissions file)]  
  jsonToRepJson $ show $ "Added  permission"

deletePermission :: (YesodPersist m, YesodPersistBackend m ~ SqlPersist, m ~ App, s ~ App) => (UserId -> TApplicationId -> GHandler s m (Key (PersistEntityBackend Permission) Permission)) -> Username -> Path -> Handler RepJson
deletePermission permissionConstructor username filePath = do
  --verify that the accesskey has the power to delete (share) filePath
  accessKey <- requireAccessKey
  (_, appEntity) <- (accessKey `requirePermission` SHARE) filePath
  Entity uid _ <- runDB $ getBy404 $ UniqueNickname $ username
  
  --delete existing permission to file for `username`
  Entity fid file <- runDB $ getBy404 $ UniqueRawPath $ pathToText filePath -- 404 shouldn't be thrown, fp was already verified
  permissionToDelete <- permissionConstructor uid (entityKey appEntity)
  _ <- runDB $ update fid [TFilePermissions =. filter (/= permissionToDelete) (tFilePermissions file)]  
  jsonToRepJson $ show $ "Added  permission"
  
-- | This function checks that a given access key has permission `permissionType` on the given file path. It returns a tuple to avoid refetching entities
requirePermission :: (YesodPersist m, YesodPersistBackend m ~ SqlPersist) => AccessKey -> PermissionType -> Path -> GHandler s m (Entity (UserGeneric SqlPersist), Entity (TApplicationGeneric SqlPersist))
--access key, permission type and file path
requirePermission ak pt fp = do
  (userEntity,appEntity) <- requireValidAuthPairEntities ak -- goes to login otherwise
  let 
    uid = entityKey userEntity
    aid = entityKey appEntity
    
  permissionEntities <- getPermissions pt uid aid -- permissions that **contain** this permission
  
  maybeFile <- runDB $ getBy $ UniqueRawPath $ pathToText fp      
        
  case maybeFile of
    Just file -> 
      let 
        filePermissions = tFilePermissions . entityVal $ file -- [PermissionId]
        permissionKeys = map entityKey permissionEntities
      in 
       if (not . null $ permissionKeys `intersect` filePermissions)
       then return $ (userEntity,appEntity)
       else do
         liftIO . putStrLn . show $ filePermissions
         permissionDenied "Not enough permissions on file for that accesskey"
    Nothing -> permissionDenied "TODO!!! fileDoesNotExistErrorResponse - AND LOGIN REQUIRED"
  where
    getPermissions READ uid aid = runDB $ selectList [PermissionRead ==. True, PermissionUser ==. uid, PermissionApp ==. aid] []
    getPermissions WRITE uid aid = runDB $ selectList [PermissionWrite ==. True, PermissionUser ==. uid, PermissionApp ==. aid] []
    getPermissions SHARE uid aid = runDB $ selectList [PermissionShare ==. True, PermissionUser ==. uid, PermissionApp ==. aid] []

-- | Permission that only allows to read
permissionToRead :: (YesodPersist m, YesodPersistBackend m ~ SqlPersist) => UserId -> TApplicationId -> GHandler s m (Key (PersistEntityBackend Permission) Permission)
permissionToRead uid tappid = do
  permission <- runDB $ selectFirst [PermissionUser ==. uid, PermissionApp ==. tappid, PermissionWrite ==. False, PermissionRead ==. True, PermissionShare ==. False] []
  case permission of
    Just (Entity pid _) -> return pid
    _ -> do
      let p = let
            write = False
            read = True
            share = False
            in Permission uid read write share tappid
      runDB $ insert p
      
-- | Permission that allows to read and write
permissionToWrite :: (YesodPersist m, YesodPersistBackend m ~ SqlPersist) => UserId -> TApplicationId -> GHandler s m (Key (PersistEntityBackend Permission) Permission)
permissionToWrite = permissionTo share read write where
  share = False
  read = True
  write = True

-- | Permission that allows to read and write
permissionToShare :: (YesodPersist m, YesodPersistBackend m ~ SqlPersist) => UserId -> TApplicationId -> GHandler s m (Key (PersistEntityBackend Permission) Permission)
permissionToShare = permissionTo share read write where
  share = True
  read = True
  write = True
        
-- | If a user has the power of sharing a file with others, then the
-- powers of writing and reading are certainly implicit
permissionTo :: (YesodPersist m, YesodPersistBackend m ~ SqlPersist) => Bool -> Bool -> Bool -> UserId -> TApplicationId -> GHandler s m (Key (PersistEntityBackend Permission) Permission)
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
            