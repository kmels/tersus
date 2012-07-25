{-# LANGUAGE OverloadedStrings #-}
module Tersus.Cluster.TersusServiceApp where

import Prelude
import Model
-- import Database.Persist.Postgresql
import Tersus.Cluster.TersusService
import Data.Text (append)
import System.IO.Unsafe (unsafePerformIO)
import Data.Time.Clock (getCurrentTime)
import Control.Monad.Trans (liftIO)

tersusServiceApp' :: TApplication
tersusServiceApp' = TApplication "tersus" "tersus" "Application that provides the service messaging system for system functions" (Just "http://tersusland.com/tersus") "neto@netowork.me" (unsafePerformIO getCurrentTime) "tersusAppKey"

tersusServiceUser = User "tersus" (Just "") []

tmpPsqlConn = "Server=127.0.0.1;Port=5432;Database=tersus;User Id=tersus;Password=tersus!;"

tersusServiceRecv (TMessage uSender uReceiver aSender aReceiver content timestamp) = do
    currTime <- liftIO $ getCurrentTime
    sendMessage $ TMessage uReceiver uSender aReceiver aSender ("Ok, I got: " `append` content) currTime
    return ()
    
tersusServiceApp = TersusServerApp tersusServiceApp' tersusServiceUser tersusServiceRecv Nothing