module Tersus.Cluster where

-- Cluster
-- Author: ERnesto Rodriguez
-- Contains all the CloudHaskell and Yesod initialization
-- functions.

import Prelude
import Control.Concurrent (forkIO,threadDelay)
import Remote
import Remote.Call (mkClosure)
import Remote.Process (forkProcess)
import Model
import Control.Monad.Trans (liftIO)
import Data.HashTable as H
import Network.Wai.Handler.Warp (runSettings, defaultSettings, settingsPort)
import Application
import Settings (parseExtra)
import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main (defaultMain)
import Control.Concurrent.STM (newTChan,atomically)
import Tersus.Cluster.Types
import Tersus.Cluster.MessageBackend
import GHC.Int
import System.Exit (exitSuccess)
import System.Posix.Signals (sigTERM,signalProcess)
import System.Posix.Process (getProcessID)
import System.Directory (doesFileExist)
import Control.Concurrent.STM.TVar (newTVar)
import Tersus.Cluster.TersusService (makeTersusService)
import Tersus.Cluster.TersusServiceApp (tersusServiceApp)

-- Hash function for a user application combo instance
hashUserApp :: AppInstance -> GHC.Int.Int32
hashUserApp (AppInstance name tApplication)  = H.hashString $ name ++ tApplication

-- Produce all the data structures needed to communicate data between Tersusland and Clould Haskell
-- addresses: Matches (user,app) to the processId where it's running
-- mailBoxes: Matches (user,app) to the MVar where the messages are delivered
-- msgStatusTable: Matches the md5 hash of a message to the MVar containing it's status. This MVar is written once the message delivery status is known.
-- sChannel: Channel where messgaes queues for delivery are written
-- rChannel: Channel where messages acknowledge as received are placed. Note that this channel is NOT USED TO DELIVER MESSAGES is just the pipeline were Tersus/Yesod informs CloudHaskell that a delivery status for that channel can be sent
-- aChannel: Channel were (user/app) actions are written by tersus so CloudHaskell can communicate them to other processes. Actions are app is opened, app is closed
-- messagePorts: A send and receive channel (called ports in cloud haskell) dedicated for sending and receiving messages. The send port is distributed to all other tersus instances so they can send messages to this instance
-- acknowledgementPorts: A send and receive channel dedicated to receive message delivery status. They are usually packed in the message envelope so once a message is received, tersus can immediately send the result to the corresponding process
initDataStructures :: ProcessM (TMessageQueue,TMessageQueue,NotificationsChannel,MailBoxTable,AddressTable,TMessageStatusTable,MessagingPorts,AcknowledgementPorts,NotificationsPorts,TersusClusterList)
initDataStructures = (liftIO $ H.new (==) hashUserApp) >>= \addresses ->
                     (liftIO $ H.new (==) hashUserApp) >>= \mailBoxes ->
                     (liftIO $ H.new (==) hashUserApp) >>= \msgStatusTable ->
                     (liftIO $ atomically newTChan) >>= \sChannel ->
                     (liftIO $ atomically newTChan) >>= \rChannel ->
                     (liftIO $ atomically newTChan) >>= \nChannel ->
                     newChannel >>= \messagePorts ->
                     newChannel >>= \acknowledgementPorts ->
                     newChannel >>= \(nSendPort,nRecvPort) ->
                     (liftIO $ atomically $ newTVar []) >>= \clusterList ->
                     return (sChannel,rChannel,nChannel,mailBoxes,addresses,msgStatusTable,messagePorts,acknowledgementPorts,(nSendPort,nRecvPort),clusterList)

-- Process that runs Tersus and Yesod in development mode
-- This is what yesod devel executes
-- For the moment it initializes a dummy address and mailbox, but this will be 
-- discarded once the registration services exist
createTersusDevelInstance :: ProcessM ()
createTersusDevelInstance = do
  (sChannel,rChannel,aChannel,mailBoxes,addresses,msgStatusTable,(mSendPort,mRecvPort),(aSendPort,aRecvPort),(nSendPort,nRecvPort),clusterList) <- initDataStructures

  _ <- runTersusMessaging (mSendPort,mRecvPort) (aSendPort,aRecvPort) (nSendPort,nRecvPort) sChannel rChannel aChannel mailBoxes addresses clusterList msgStatusTable 1
  _ <- forkProcess $ makeTersusService tersusServiceApp sChannel clusterList
  liftIO $ do (port, app') <- getApplicationDev (sChannel,rChannel,aChannel,mailBoxes,msgStatusTable,aSendPort)
              runSettings defaultSettings
                              { settingsPort = port
                              } app'

-- Function for Tersus devel that constantly checks if
-- the source changed and kills Tersus if the case
-- is so.
loop :: IO ()
loop = do
  threadDelay 100000
  e <- doesFileExist "dist/devel-terminate"
  if e then  getProcessID >>= signalProcess sigTERM >> terminateDevel else loop

terminateDevel :: IO ()
terminateDevel = exitSuccess

-- Process that runs Tersus and Yesod in producction mode
-- This will be executed when Tersus is deployed
createTersusInstance :: ProcessM ()
createTersusInstance = do
  (sChannel,rChannel,nChannel,mailBoxes,addresses,msgStatusTable,(mSendPort,mRecvPort),(aSendPort,aRecvPort),(nSendPort,nRecvPort),clusterList) <- initDataStructures
  _ <- liftIO $ forkIO $ defaultMain (fromArgs parseExtra) $ makeApplicationWrapper (sChannel,rChannel,nChannel,mailBoxes,msgStatusTable,aSendPort)
  _ <- runTersusMessaging (mSendPort,mRecvPort) (aSendPort,aRecvPort) (nSendPort,nRecvPort) sChannel rChannel nChannel mailBoxes addresses clusterList msgStatusTable 1
  return ()

-- Make the functions that initialize a tersus process remotable
remotable ['createTersusInstance,'createTersusDevelInstance]

-- Functions to initialize all the tersus nodes and distribute the ProcessId of such nodes
-- Development and producction version of the function
initTersusCluster :: String -> ProcessM ()
initTersusCluster "T1" = do 
  peers <- getPeers
--  t2s <- return $ findPeerByRole peers tersusClusterRole
  t2s <- getSelfNode >>= return . return
  pids <- mapM (\p -> (spawn p $(mkClosure 'createTersusInstance)))  t2s
--  mapM_ (\p -> send p pids) pids
  receiveWait []

initTersusCluster _ = return ()

-- ProcessM function that initializes a single tersus node in development
-- mode. This Function uses forkProcess to fork a ProcessM instance with
-- Tersus running.
initTersusClusterDevel :: String -> ProcessM ()
initTersusClusterDevel "T1" = do 
  peers <- getPeers
  -- t2s <- return $ findPeerByRole peers tersusClusterRole
  t2s <- getSelfNode >>= return . return
  pids <- mapM (\p -> (spawn p $(mkClosure 'createTersusDevelInstance)))  t2s
  -- mapM_ (\p -> send p pids) pids
  -- forkProcess createTersusDevelInstance
  liftIO $ loop
  return ()

initTersusClusterDevel _ = return ()

-- Initialize Tersus in producction mode running on top of CloudHaskell
-- called by main
tersusProducction :: IO ()
tersusProducction = do 
  remoteInit (Just "config/servers") [Tersus.Cluster.__remoteCallMetaData] initTersusCluster
  return ()

-- Initialize Tersus in development mode running on top of CloudHaskell
-- called by main
tersusDevel :: IO ()
tersusDevel = do 
  remoteInit (Just "config/servers") [Tersus.Cluster.__remoteCallMetaData] initTersusClusterDevel
  return ()