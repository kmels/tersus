module Tersus.Cluster.MessageFrontend where

import Import
import Control.Distributed.Process.Binder
import qualified Control.Distributed.Process as P 
import Data.IORef
import Control.Concurrent.STM (atomically,readTVar)
import Tersus.DataTypes
import qualified Data.HashTable.IO as HT
import Tersus.Cluster.Types
import Data.Typeable (Typeable)

-- | Run a process action in the GHandler Monad and
-- get the result
runProcess :: Typeable a => P.Process a -> GHandler s Tersus a
runProcess process = do
  tersus <- getYesod
  liftIO $ runProcessIO (processRunner tersus) process
  
runProcessIO :: Typeable a => ProcessBinder -> P.Process a -> IO a
runProcessIO pRunner proc = runAction' Nothing
  where
    runAction' Nothing = runAction pRunner proc >>= runAction'
    runAction' (Just r) = return r
            
broadcastNotificationsProcess :: [TersusNotification] -> TersusClusterList -> P.Process ()
broadcastNotificationsProcess notifications nodes = do
  ns <- liftIO $ atomically $ readTVar nodes
  mapM_ (\(nSendPort,_) -> P.sendChan nSendPort notifications) ns

broadcastNotificationsIO :: ProcessBinder -> [TersusNotification] -> TersusClusterList -> IO ()
broadcastNotificationsIO pRunner notifications nodes = runProcessIO pRunner $ broadcastNotificationsProcess notifications nodes

broadcastNotifications :: [TersusNotification] -> GHandler s Tersus ()
broadcastNotifications notifications = do
  tersus <- getYesod
  -- nodes <- liftIO $ atomically $ readTVar (tersusNodes tersus)
  runProcess $ broadcastNotificationsProcess notifications $ tersusNodes tersus
    
-- | Get channels to receive messages to a particular app
getRecvChannelsIO :: Addressable a => RecvAddressTable -> a -> IO (Maybe MessageRecvChannels)
getRecvChannelsIO channels addr = HT.lookup channels $ getAppInstance addr

-- | Get channels to send messages to a particular app
getSendChannelsIO :: Addressable a => SendAddressTable -> a -> IO (Maybe MessageSendChannels)
getSendChannelsIO channels addr = HT.lookup channels $ getAppInstance addr

-- | Get the channels to send messages for given app instance
getSendChannels :: Addressable a => a -> GHandler s Tersus (Maybe MessageSendChannels)
getSendChannels addr = do
  tersus <- getYesod
  liftIO $ getSendChannelsIO (appsSendChannels tersus) addr
  
-- | Get the channels to receive messages for the given app instance
getRecvChannels :: Addressable a => a -> GHandler s Tersus (Maybe MessageRecvChannels)
getRecvChannels addr = do
  tersus <- getYesod
  liftIO $ getRecvChannelsIO (appsRecvChannels tersus) addr

-- | Insert the send channels for the given app instance
insertSendChannels :: Addressable a => a -> MessageSendChannels -> GHandler s Tersus ()
insertSendChannels addr channels = do
  tersus <- getYesod
  liftIO $ HT.insert (appsSendChannels tersus) (getAppInstance addr) channels
  
-- | Insert the receive channels for the given app instance  
insertRecvChannels :: Addressable a => a -> MessageRecvChannels -> GHandler s Tersus ()
insertRecvChannels addr channels = do
  tersus <- getYesod
  liftIO $ HT.insert (appsRecvChannels tersus) (getAppInstance addr) channels
