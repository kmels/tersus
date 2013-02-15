----------------------------------------------------------------------------
-- |
-- Module      :  Tersus.Yesod.Persistent
-- Copyright   :  (c) Carlos López-Camey, Ernesto Rodriguez
-- License     :  
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
--
-- Helper functions for persistent transactions
-----------------------------------------------------------------------------

module Tersus.Yesod.Persistent where

import Import

--haskell platform
import Data.Maybe(isJust)


{--- | Given a unique, determine if it already exists. 
uniqueExists :: GHandler s m Boolean
uniqueExists = do 
  ent <- runDB $ getBy $ UniqueIdentifier $ appidfier
  
existsOne :: (PersistEntity val, PersistEntityBackend val ~ b) => [Filter val] -> [SelectOpt val] -> b m Bool
existsOne f s = do
  f <- runDB $ selectFirst f s
  return (isJust f)-}
     
-- | Given a UserApplication, return the user. See example in Handler.TApplication.fetchAdminsOf
userAppToUser :: (YesodPersist m, YesodPersistBackend m ~ SqlPersist) => Entity UserApplication -> GHandler s m (Maybe User)
userAppToUser (Entity _ (UserApplication userkey tappkey isadmin)) = runDB $ get $ userkey