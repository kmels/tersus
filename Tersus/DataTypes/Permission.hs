module Tersus.DataTypes.Permission where

import           qualified Data.ByteString.Char8 as Char8
import qualified Data.Text as T
import           Data.Text.Encoding
import           Database.Redis
import           Prelude
import           Tersus.DataTypes.TypeSynonyms
import           Tersus.Database((.>),(<.)) -- Ts for Tersus
import           Tersus.Filesystem (pathToText)

data Permission = Permission {
  userid :: UserId
  , permissionType :: PermissionType
  , appid :: ApplicationIdentifier
  } 
  
data PermissionType = READ | WRITE | SHARE deriving Show

deletePermission :: PermissionType -> Username -> FileId -> IO ()
deletePermission pt u fi = return ()

{- 
runRedis () $ do
    --fileId <- getFileId filePath
    let fileId = (identifier app) -- this is obviously wrong
    del $ "f:" .> (decodeUtf8 fileId) <. (identifier app) <. (uid user)
    -}
    
getPermission :: Connection -> PermissionType -> FileId -> ApplicationIdentifier -> UserId -> IO Bool
getPermission conn pt fid aid uid = 
  let
    fileID = Char8.pack . show $ fid
  in 
   runRedis conn $ do  
     eFileId <- get $ "f:" .> fileID <. "id" -- Either Reply BS 
     get $ "f:" .> fileID <. (show $ pt) 
     return False
     