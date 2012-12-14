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

module Handler.Permission where

import           Import
import Tersus.AccessKeys(requireAccessKey)
import Database.Persist.GenericSql.Raw(SqlPersist(..))
import Tersus.AccessKeys(reqValidAuthPair,requireValidAuthPairEntities)
import Tersus.Filesystem

-- | Returns 404 if the username doesn't exist.
-- Returns permissionDenied if filePath is not valid
putReadFilePermissionForUserR :: Username -> Path -> Handler RepJson
putReadFilePermissionForUserR username filePath = do
  --verify that the accesskey has the power to share filePath
  accessKey <- requireAccessKey
  (_,_,appEntity) <- (accessKey `requirePermission` SHARE) filePath
  Entity uid _ <- runDB $ getBy404 $ UniqueNickname $ username
  
  --add a new permission to file for `username`
  Entity fid file <- runDB $ getBy404 $ UniqueRawPath $ pathToText filePath -- 404 shouldn't be thrown, fp was already verified
  newPermission <- permissionToRead uid (entityKey appEntity)
  _ <- runDB $ update fid [TFilePermissions =. newPermission : (tFilePermissions file)]
  
  jsonToRepJson $ show "Added read permission"

    
    --permissionDenied $ " You don't have permissions to share this file"

-- | This function checks that a given access key has permission `permissionType` on the given file path. It returns a triple, to avoid refetching entities.
requirePermission :: (YesodPersist m, YesodPersistBackend m ~ SqlPersist) => AccessKey -> PermissionType -> Path -> GHandler s m (Entity (PermissionGeneric SqlPersist),Entity (UserGeneric SqlPersist), Entity (TApplicationGeneric SqlPersist))
requirePermission ak pt fp = do
  (userEntity,appEntity) <- requireValidAuthPairEntities ak
  let 
    uid = entityKey userEntity
    aid = entityKey appEntity
  permission <- getPermission pt uid aid
  file <- runDB $ getBy $ UniqueRawPath $ pathToText fp -- :: Maybe TFile
  let 
    filePermissions = file >>= Just . tFilePermissions . entityVal -- :: Maybe [PermissionId]
    ifElemJust x xs = case x `elem` xs of
      True -> Just x
      _ -> Nothing
    requiredPermission = filePermissions >>= \fps -> permission >>= \p -> case (entityKey p) `elem` fps of
      True -> Just p
      _ -> Nothing
  let response = requiredPermission >>= \rp -> Just (rp,userEntity,appEntity)
  maybe (permissionDenied "Not enough permissions on file for that accesskey") return response where
  
    getPermission READ uid aid = runDB $ selectFirst [PermissionRead ==. True, PermissionUser ==. uid, PermissionApp ==. aid] []
    getPermission WRITE uid aid = runDB $ selectFirst [PermissionWrite ==. True, PermissionUser ==. uid, PermissionApp ==. aid] []
    getPermission SHARE uid aid = runDB $ selectFirst [PermissionShare ==. True, PermissionUser ==. uid, PermissionApp ==. aid] []

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

data PermissionType = READ | WRITE | SHARE

-- | If a user has the power of sharing a file with others, then the
-- powers of writing and reading are certainly implicit
permissionToShare :: UserId -> TApplicationId -> Permission
permissionToShare uid tappid = 
  let
    write = True
    read = True
    share = True
  in Permission uid read write share tappid

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
      
    
permissionToShareId :: (YesodPersist m, YesodPersistBackend m ~ SqlPersist) => UserId -> TApplicationId -> GHandler s m PermissionId
permissionToShareId uid tappid = runDB $ insert $ permissionToShare uid tappid
