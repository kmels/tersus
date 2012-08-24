{- |
Module      :  Tersus.User
Copyright   :  (C) 2012 Carlos López-Camey, Ernesto Rodríguez
License     :  GNU 2

Maintainer  :  <c.lopez@kmels.net>
Stability   :  stable

A set of monadic and non monadic functions for handling users in requests.
-}

module Tersus.User(
  getValidUser
  ) where

import Import
import Tersus.AccessKeys(decompose)
import Database.Persist.GenericSql.Raw(SqlPersist(..))
-- | Returns Nothing iff the access key doesn't correspond to the given username. Returns a user if the access key is valid for the given username.

getValidUser :: (YesodPersist m, YesodPersistBackend m ~ SqlPersist) => Username -> AccessKey -> GHandler s m (Maybe User)
getValidUser u ak = do  
  userMaybe <- runDB $ getBy $ UniqueNickname $ u --query db
  case validateAccessKeyUsername u ak of
    Just uname -> do  
      case userMaybe of
        Just (Entity _ user) -> return $ Just user
        Nothing -> return Nothing
    _ -> return Nothing

-- | -- | Returns Nothing iff the access key doesn't correspond to the given username. Returns the given username if the access key belongs to him.
validateAccessKeyUsername :: AccessKey -> Username -> Maybe Username
validateAccessKeyUsername ak u = case decompose ak of
    Nothing -> Nothing
    Just ((username',_)) -> if (username' == u)
                          then Just u
                          else Nothing