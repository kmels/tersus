{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Tersus.Cluster.TersusServiceApp where

import Prelude
import Control.Monad.Maybe
import Control.Monad.Trans          (liftIO)
import Data.Text                    (Text)
import Data.Time.Clock              (getCurrentTime)
import Tersus.DataTypes.User
import System.IO.Unsafe             (unsafePerformIO)
--import Tersus.Cluster.TersusService (TersusServerApp(..),TersusServiceM,sendMessage,maybeGetBy,maybeSelectList,maybeGet,runQuery)
import Tersus.Cluster.TersusService (TersusServerApp(..),TersusServiceM,sendMessage)
import Tersus.DataTypes
import Tersus.Global
import qualified Tersus.Global as Tersus.Global
tersusServiceAppName :: Text
tersusServiceAppName = "tersus"

-- | Tersus service application. This application can be messaged to obtain lists of users
tersusServiceApp' :: TApplication
tersusServiceApp' = TApp 1 tersusServiceAppName tersusServiceAppName "Application that provides the service messaging system for system functions" "http://tersusland.com/tersus" "neto@netowork.me" (unsafePerformIO getCurrentTime) "tersusAppKey" [Tersus.Global.tersusUserId]

tersusServiceUsername :: Text
tersusServiceUsername = "tersus"

tersusServiceUser :: User
tersusServiceUser = User 0 "tersus@tersusland.com" tersusServiceUsername (Just "") False                      

-- | Function that processes a message sent to the tersus service application. It will query
-- the database for all users and return the list of the users
tersusServiceRecv :: TMessage -> TersusServiceM AppInstance ()
tersusServiceRecv (TMessage uSender uReceiver aSender aReceiver _ _) = do
  
    {-currTime <- liftIO $ getCurrentTime
    users <- runQuery $ getAppUsersQuery aSender --selectList [] []) :: TersusServiceM [Entity User]
    sendMessage $ TMessage uReceiver uSender aReceiver aSender (encodeAsText users) currTime-}
    return ()

{-getAppUsersQuery :: forall (m :: * -> *) (backend :: (* -> *) -> * -> *).
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
    --TODO: remove, this is not used (Warning) filterKey (Entity k _) = k-}

tersusServiceApp :: TersusServerApp AppInstance
tersusServiceApp = TersusServerApp tersusServiceApp' tersusServiceUser tersusServiceRecv Nothing Nothing Nothing
