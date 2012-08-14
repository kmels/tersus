{-# LANGUAGE OverloadedStrings #-}
module Tersus.Cluster.TersusServiceApp where

import Prelude
import Model
-- import Database.Persist.Postgresql
import Tersus.Cluster.TersusService
import Data.Text (pack)
import System.IO.Unsafe (unsafePerformIO)
import Data.Time.Clock (getCurrentTime)
import Control.Monad.Trans (liftIO)
import Database.Persist (insert,selectList,(>.),(<-.))
import Database.Persist.Store --(Entity)
import Data.Aeson (toJSON,encode)
import Tersus.Global
import Model.User
import Data.Text
import Data.Text.Lazy.Encoding (decodeUtf8)
import Control.Monad.Maybe
import Control.Monad (foldM)

tersusServiceApp' :: TApplication
tersusServiceApp' = TApplication "tersus" "tersus" "Application that provides the service messaging system for system functions" (Just "http://tersusland.com/tersus") "neto@netowork.me" (unsafePerformIO getCurrentTime) "tersusAppKey"

tersusServiceUser = User "tersus" (Just "") []

tersusServiceRecv (TMessage uSender uReceiver aSender aReceiver content timestamp) = do
    currTime <- liftIO $ getCurrentTime
    users <- runQuery $ getAppUsersQuery aSender --selectList [] []) :: TersusServiceM [Entity User]
    sendMessage $ TMessage uReceiver uSender aReceiver aSender ((collapseLazyText.decodeUtf8.encode.toJSON) users) currTime
    return ()

getAppUsersQuery app = (runMaybeT $ getAppUsersQuery' app) >>= \res -> case res of
                                                                   Nothing -> return []
                                                                   Just a -> return a

getAppUsersQuery' (TApplication name id desc url email date appkey) = do 
  Entity key _ <- maybeGetBy $ UniqueIdentifier id
  appUsers <- maybeSelectList [UserApplicationApplication <-. [key]] [] -- :: MaybeT
  mapM (\(Entity _ (UserApplication u _)) ->  maybeGet u) appUsers
  --return [ (\(Entity _ (UserApplication u _)) -> u) x | x <- appUsers]
  --maybeSelectList [UserId <-. [ filterKey u | u <- appUsers]] []

  where
    filterKey (Entity k _) = k
    
tersusServiceApp = TersusServerApp tersusServiceApp' tersusServiceUser tersusServiceRecv Nothing