module Tersus.Cluster where

-- Cluster
-- Author: ERnesto Rodriguez
-- Contains all the CloudHaskell and Yesod initialization
-- functions.

import           Application
import           Control.Concurrent                                 (threadDelay)
import           Control.Concurrent.STM                             (atomically,
                                                                     newTChan)
import           Control.Concurrent.STM.TVar                        (newTVar)
import           Control.Distributed.Process
import           Control.Distributed.Process.Backend.SimpleLocalnet
import           Control.Distributed.Process.Node                  
                                                                     (initRemoteTable,
                                                                     runProcess)
-- import           Control.Monad.Trans                                (liftIO)
import           Data.HashTable                                     as H
import           GHC.Int
import           Model
import           Network.Wai.Handler.Warp                          
                                                                     (defaultSettings,
                                                                     runSettings,
                                                                     settingsPort)
import           Prelude
import           Settings                                           (parseExtra)
import           System.Directory                                  
                                                                     (doesFileExist)
import           System.Exit                                       
                                                                     (exitSuccess)
import           System.Posix.Process                              
                                                                     (getProcessID)
import           System.Posix.Signals                               (sigTERM,
                                                                     signalProcess)
import           Tersus.Cluster.MessageBackend
import           Tersus.Cluster.TersusService                      
                                                                     (makeTersusService)
import           Tersus.Cluster.TersusServiceApp                   
                                                                     (tersusServiceApp)
import           Tersus.Cluster.Types
import           Tersus.Global
import           Yesod.Default.Config                               (fromArgs)
import           Yesod.Default.Main                                
                                                                     (defaultMain)

-- |Hash function for a user application combo instance
hashUserApp :: AppInstance -> GHC.Int.Int32
hashUserApp (AppInstance name tApplication)  = H.hashString $ name ++ tApplication

-- |Produce all the data structures needed to communicate data between Tersusland and Clould Haskell
-- addresses: Matches (user,app) to the processId where it's running
-- mailBoxes: Matches (user,app) to the MVar where the messages are delivered
-- msgStatusTable: Matches the md5 hash of a message to the MVar containing it's status. This MVar is written once the message delivery status is known.
-- sChannel: Channel where messgaes queues for delivery are written
-- rChannel: Channel where messages acknowledge as received are placed. Note that this channel is NOT USED TO DELIVER MESSAGES is just the pipeline were Tersus/Yesod informs CloudHaskell that a delivery status for that channel can be sent
-- aChannel: Channel were (user/app) actions are written by tersus so CloudHaskell can communicate them to other processes. Actions are app is opened, app is closed
-- messagePorts: A send and receive channel (called ports in cloud haskell) dedicated for sending and receiving messages. The send port is distributed to all other tersus instances so they can send messages to this instance
-- acknowledgementPorts: A send and receive channel dedicated to receive message delivery status. They are usually packed in the message envelope so once a message is received, tersus can immediately send the result to the corresponding process
initDataStructures :: Process (TMessageQueue,TMessageQueue,NotificationsChannel,AppInstanceTable,AddressTable,MessagingPorts,AcknowledgementPorts,NotificationsPorts,TersusClusterList)
initDataStructures = (liftIO $ H.new (==) hashUserApp) >>= \addresses ->
                     (liftIO $ H.new (==) hashUserApp) >>= \appEnvs ->
--                     (liftIO $ H.new (==) hashUserApp) >>= \msgStatusTable ->
                     (liftIO $ atomically newTChan) >>= \sChannel ->
                     (liftIO $ atomically newTChan) >>= \rChannel ->
                     (liftIO $ atomically newTChan) >>= \nChannel ->
                     newChan >>= \messagePorts ->
                     newChan >>= \acknowledgementPorts ->
                     newChan >>= \(nSendPort,nRecvPort) ->
                     (liftIO $ atomically $ newTVar []) >>= \clusterList ->
                     return (sChannel,rChannel,nChannel,appEnvs,addresses,messagePorts,acknowledgementPorts,(nSendPort,nRecvPort),clusterList)

-- |Process that runs Tersus and Yesod in development mode
-- This is what yesod devel executes
-- For the moment it initializes a dummy address and mailbox, but this will be
-- discarded once the registration services exist
createTersusDevelInstance :: Backend -> Process ()
createTersusDevelInstance back = do
  (sChannel,rChannel,aChannel,appEnvs,addresses,(mSendPort,mRecvPort),(aSendPort,aRecvPort),(nSendPort,nRecvPort),clusterList) <- initDataStructures

  _ <- runTersusMessaging back (mSendPort,mRecvPort) (aSendPort,aRecvPort) (nSendPort,nRecvPort) sChannel rChannel aChannel appEnvs addresses clusterList 1
  _ <- spawnLocal $ makeTersusService tersusServiceApp sChannel clusterList Development
  liftIO $ do (port, app') <- getApplicationDev (sChannel,rChannel,aChannel,appEnvs,aSendPort)
              runSettings defaultSettings
                              { settingsPort = port
                              } app'

-- |Function for Tersus devel that constantly checks if
-- the source changed and kills Tersus if the case
-- is so.
loop :: IO ()
loop = do
  threadDelay 100000
  e <- doesFileExist "dist/devel-terminate"
  if e then  getProcessID >>= signalProcess sigTERM >> terminateDevel else loop

terminateDevel :: IO ()
terminateDevel = exitSuccess

-- |Process that runs Tersus and Yesod in producction mode
-- This will be executed when Tersus is deployed
createTersusInstance :: Backend -> Process ()
createTersusInstance back = do
  (sChannel,rChannel,nChannel,appEnvs,addresses,(mSendPort,mRecvPort),(aSendPort,aRecvPort),(nSendPort,nRecvPort),clusterList) <- initDataStructures
  _ <- runTersusMessaging back (mSendPort,mRecvPort) (aSendPort,aRecvPort) (nSendPort,nRecvPort) sChannel rChannel nChannel appEnvs addresses clusterList 1
  _ <- spawnLocal $ makeTersusService tersusServiceApp sChannel clusterList Development
  liftIO $ defaultMain (fromArgs parseExtra) $ makeApplicationWrapper (sChannel,rChannel,nChannel,appEnvs,aSendPort)
  return ()

-- |Functions to initialize all the tersus nodes and distribute the ProcessId of such nodes
-- Development and producction version of the function
initTersusCluster :: Backend -> Process ()
initTersusCluster back = do
  createTersusInstance back
  receiveWait []

-- | Initialize a Tersus instance running on top of Cloud Haskell
-- the initialization is done by calling the createTersusDevelInstance
initTersusClusterDevel :: Backend -> Process ()
initTersusClusterDevel back = do
  _ <- spawnLocal $ createTersusDevelInstance back
  liftIO $ loop
  return ()

-- |Initialize Tersus in producction mode running on top of CloudHaskell
-- called by main
tersusProducction :: IO ()
tersusProducction = do
  putStrLn "Init Production"
  back <- initializeBackend "127.0.0.1" "8000" initRemoteTable
  node <- newLocalNode back
  runProcess node $ initTersusCluster back
  return ()

-- |Initialize Tersus in development mode running on top of CloudHaskell
-- called by main
tersusDevel :: IO ()
tersusDevel = do
  back <- initializeBackend "127.0.0.1" "8000" initRemoteTable
  node <- newLocalNode back
  runProcess node $ initTersusClusterDevel back
  return ()
