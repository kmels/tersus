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
import Database.Persist (insert,selectList,(>.))
import Database.Persist.Store (Entity)
import Data.Aeson (toJSON)
import Tersus.Global
import Model.User
import Data.Text

tersusServiceApp' :: TApplication
tersusServiceApp' = TApplication "tersus" "tersus" "Application that provides the service messaging system for system functions" (Just "http://tersusland.com/tersus") "neto@netowork.me" (unsafePerformIO getCurrentTime) "tersusAppKey"

tersusServiceUser = User "tersus" (Just "") []

tmpPsqlConn = "Server=127.0.0.1;Port=5432;Database=tersus;User Id=tersus;Password=tersus!;"

tersusServiceRecv (TMessage uSender uReceiver aSender aReceiver content timestamp) = do
    currTime <- liftIO $ getCurrentTime
    users <- (runQuery $ selectList [] []) :: TersusServiceM [Entity User]
    sendMessage $ TMessage uReceiver uSender aReceiver aSender ((pack.show.toJSON) users) currTime
    return ()
    
tersusServiceApp = TersusServerApp tersusServiceApp' tersusServiceUser tersusServiceRecv Nothing