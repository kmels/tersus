{- |
Module      :  Handler.User
Copyright   :  (C) 2012 Carlos López-Camey, Ernesto Rodríguez
License     :  GNU 2

Maintainer  :  <c.lopez@kmels.net>
Stability   :  stable

 - API calls that are related to user
 - A set of monadic and non monadic functions for handling users in requests
-}

module Handler.User(
  --API calls
  getLoggedUserR, --maybe json logged user
  getUserAccessKeyR, --(request) plain access key request for given username,appkey  
  --GHandler function helpers  
  verifyUserKey, --validate accesskey for given username
  verifyUserKeyM, --monadic version of verifyUserKey
  getValidUser --match authkey with username
  ) where
  
import Import
import Tersus.AccessKeys(decompose)
import Yesod.Json(jsonToRepJson)
import Yesod.Auth
import Model.User
import Data.Aeson(encode)
import Database.Persist.GenericSql.Raw(SqlPersist(..))
import Tersus.AccessKeys
import           Data.Text                as T

-- | Returns a JSON representation of the logged user. Returns a 412 status code (Precondition failed) with an empty string
getLoggedUserR :: Handler RepJson
getLoggedUserR = do
  maybeUserId <- maybeAuth
  case maybeUserId of
       Just (Entity _ u) -> jsonToRepJson u
       Nothing -> error "Result: Empty user. TODO: implement return response"

-- | Returns the access key of a logged user. Returns a 412 status code (Precondition failed) with an empty string
-- TODO: This request *must* be secure (with https)
-- TODO: Change the parameter from ApplicationIdentifier to ApplicationKey (implies modifying newHexRandomAccessKey)
getUserAccessKeyR :: ApplicationIdentifier -> Handler RepJson
getUserAccessKeyR appId = do
  maybeUserId <- maybeAuth
  case maybeUserId of
    Just (Entity _ u) -> do
      accessKey <- liftIO $ newAccessKey (userNickname u) appId
      jsonToRepJson $ accessKey
    Nothing -> error "Result: Empty user"
      
-- | Returns Nothing iff the access key doesn't correspond to the given username. Returns a user if the access key is valid for the given username.
getValidUser :: (YesodPersist m, YesodPersistBackend m ~ SqlPersist) => Username -> AccessKey -> GHandler s m (Maybe User)
getValidUser u ak = do  
  liftIO $ putStrLn $ "Recibido u:"++(show u)
  userMaybe <- runDB $ getBy $ UniqueNickname $ u --query db
  liftIO $ putStrLn $ show $ ak `verifyUserKey` u
  case ak `verifyUserKey` u of
    Just uname -> do  
      case userMaybe of
        Just (Entity _ user') -> return $ Just user'
        Nothing -> return Nothing
    _ -> return Nothing

-- | Monadic version of `verifyUser`
verifyUserKeyM :: AccessKey -> Username -> GHandler s m (Maybe Username)
verifyUserKeyM ak u = return $ ak `verifyUserKey` u

-- | Returns Nothing iff the access key doesn't correspond to the given username. Returns the given username if the access key belongs to him.
verifyUserKey :: AccessKey -> Username -> Maybe Username
verifyUserKey key uname = decompose key >>= \(u',_) -> if (u'==uname) 
                                                     then Just uname
                                                     else Nothing
                                                                                                          