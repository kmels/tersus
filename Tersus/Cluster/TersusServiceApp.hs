{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Tersus.Cluster.TersusServiceApp where

import Model
import Prelude
-- import Database.Persist.Postgresql
import Control.Monad.Maybe
import Control.Monad.Trans          (liftIO)
import Data.Aeson                   (encode, toJSON)
import Data.Text                    (Text)
import Data.Text.Lazy.Encoding      (decodeUtf8)
import Data.Time.Clock              (getCurrentTime)
import Database.Persist             ((<-.))
import Database.Persist.Query.Internal
import Database.Persist.Store
import Model.User()
import System.IO.Unsafe             (unsafePerformIO)
import Tersus.Cluster.TersusService
import Tersus.Global

tersusServiceAppName :: Text
tersusServiceAppName = "tersus"

tersusServiceApp' :: TApplication
tersusServiceApp' = TApplication tersusServiceAppName tersusServiceAppName "Application that provides the service messaging system for system functions" (Just "http://tersusland.com/tersus") "neto@netowork.me" (unsafePerformIO getCurrentTime) "tersusAppKey"

tersusServiceUsername :: Text
tersusServiceUsername = "tersus"

tersusServiceUser :: User
tersusServiceUser = User "tersus@tersusland.com" tersusServiceUsername (Just "") False

tersusServiceRecv :: TMessage -> TersusServiceM ()
tersusServiceRecv (TMessage uSender uReceiver aSender aReceiver _ _) = do
    currTime <- liftIO $ getCurrentTime
    users <- runQuery $ getAppUsersQuery aSender --selectList [] []) :: TersusServiceM [Entity User]
    sendMessage $ TMessage uReceiver uSender aReceiver aSender ((collapseLazyText.decodeUtf8.encode.toJSON) users) currTime
    return ()

getAppUsersQuery :: forall (m :: * -> *) (backend :: (* -> *) -> * -> *).
                    (PersistUnique backend m, PersistQuery backend m) =>
                    Text -> backend m [UserGeneric backend]
getAppUsersQuery app' = (runMaybeT $ getAppUsersQuery' app') >>= \res -> case res of
                                                                           Nothing -> return []
                                                                           Just a -> return a
getAppUsersQuery' :: forall (m :: * -> *) (backend :: (* -> *) -> * -> *).
                     (PersistUnique backend m,
                      PersistQuery backend m) => Text -> MaybeT (backend m) [UserGeneric backend]
getAppUsersQuery' applicationIdentifier = do
  Entity key _ <- maybeGetBy $ UniqueIdentifier applicationIdentifier
  appUsers <- maybeSelectList [UserApplicationApplication <-. [key]] [] -- :: MaybeT
  --TODO Very inefficient, do a join instead.
  mapM (\(Entity _ (UserApplication u _ _)) ->  maybeGet u) appUsers
  --return [ (\(Entity _ (UserApplication u _)) -> u) x | x <- appUsers]
  --maybeSelectList [UserId <-. [ filterKey u | u <- appUsers]] []

  where
    --TODO: remove, this is not used (Warning) filterKey (Entity k _) = k

tersusServiceApp :: TersusServerApp
tersusServiceApp = TersusServerApp tersusServiceApp' tersusServiceUser tersusServiceRecv Nothing


