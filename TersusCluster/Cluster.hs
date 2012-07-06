module TersusCluster.Cluster where

-- Cluster
-- Author: ERnesto Rodriguez
-- Contains all the CloudHaskell and Yesod initialization
-- functions.

import Prelude
import Control.Concurrent (forkIO,threadDelay)
import Remote
import Remote.Call (mkClosure)
import Model
import Data.Text as T
import System.IO.Unsafe (unsafePerformIO)
import Data.Time.Clock (getCurrentTime)
import Control.Monad.Trans (liftIO)
import Data.HashTable as H
import Network.Wai.Handler.Warp (runSettings, defaultSettings, settingsPort)
import Application
import Control.Concurrent.MVar 
import Settings (parseExtra)
import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main (defaultMain)
import Control.Concurrent.STM (newTChan,atomically)
import Control.Concurrent.STM.TChan (TChan)
import TersusCluster.Types
import TersusCluster.MessageBackend
import GHC.Int
import System.Directory (doesFileExist, removeFile)
import System.Exit (exitSuccess)

dummyUser = User (T.pack "neto") (Just (T.pack "1234")) []
                       
-- This is a dummy datatype only to show that this works
-- It will be removed and never used
-- unsafePerformIO is there just because it's simpler and
-- this will not be part of tersus
dummyApp = TApplication (T.pack "emacs") (T.pack "identifier") (T.pack "description dummy") (Just (T.pack "url")) (T.pack "mail@place.com") (unsafePerformIO getCurrentTime)  (T.pack "appkey")

dummyMsg = TMessage dummyUser dummyUser dummyApp dummyApp (T.pack "Alonso") (unsafePerformIO getCurrentTime)

dummyAddress = Address dummyUser dummyApp

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
initDataStructures :: ProcessM (TMessageQueue,TMessageQueue,ActionsChannel,MailBoxTable,AddressTable,TMessageStatusTable,MessagingPorts,AcknowledgementPorts)
initDataStructures = (liftIO $ H.new (==) hashUserApp) >>= \addresses ->
                     (liftIO $ H.new (==) hashUserApp) >>= \mailBoxes ->
                     (liftIO $ H.new (==) hashUserApp) >>= \msgStatusTable ->
                     (liftIO $ atomically $ newTChan) >>= \sChannel ->
                     (liftIO $ atomically $ newTChan) >>= \rChannel ->
                     (liftIO $ atomically $ newTChan) >>= \aChannel ->
                     newChannel >>= \messagePorts ->
                     newChannel >>= \acknowledgementPorts ->
                     return (sChannel,rChannel,aChannel,mailBoxes,addresses,msgStatusTable,messagePorts,acknowledgementPorts)
                                      

-- Process that runs Tersus and Yesod in development mode
-- This is what yesod devel executes
-- For the moment it initializes a dummy address and mailbox, but this will be 
-- discarded once the registration services exist
createTersusDevelInstance :: ProcessM ()
createTersusDevelInstance = do
  (sChannel,rChannel,aChannel,mailBoxes,addresses,msgStatusTable,(mSendPort,mRecvPort),(aSendPort,aRecvPort)) <- initDataStructures
--  testMailBox <- liftIO $ newEmptyMVar
  _ <- runTersusMessaging (mSendPort,mRecvPort) (aSendPort,aRecvPort) sChannel rChannel aChannel mailBoxes addresses msgStatusTable 1
  _ <- liftIO $ H.insert addresses (getAppInstance dummyAddress) (mSendPort,"dummyHash")
--  liftIO $ H.insert mailBoxes(getAppInstance dummyAddress) testMailBox
--  liftIO $ H.insert addresses (getAppInstance dummyAddress) (mSendPort,"")
  liftIO $ do (port, app) <- getApplicationDev (sChannel,rChannel,aChannel,mailBoxes,msgStatusTable,aSendPort)
              forkIO $ runSettings defaultSettings
                            { settingsPort = port
                            } app
  liftIO $ loop

loop :: IO ()
loop = do
  threadDelay 100000
  e <- doesFileExist "dist/devel-terminate"
  if e then terminateDevel else loop

terminateDevel :: IO ()
terminateDevel = exitSuccess

-- Process that runs Tersus and Yesod in producction mode
-- This will be executed when Tersus is deployed
createTersusInstance :: ProcessM ()
createTersusInstance = do
  (sChannel,rChannel,aChannel,mailBoxes,addresses,msgStatusTable,(mSendPort,mRecvPort),(aSendPort,aRecvPort)) <- initDataStructures
  liftIO $ forkIO $ defaultMain (fromArgs parseExtra) $ makeApplicationWrapper (sChannel,rChannel,aChannel,mailBoxes,msgStatusTable,aSendPort)
  runTersusMessaging (mSendPort,mRecvPort) (aSendPort,aRecvPort) sChannel rChannel aChannel mailBoxes addresses msgStatusTable 1
  return ()

-- Make the functions that initialize a tersus process remotable
remotable ['createTersusInstance,'createTersusDevelInstance]

-- Functions to initialize all the tersus nodes and distribute the ProcessId of such nodes
-- Development and producction version of the function
initTersusCluster :: String -> ProcessM ()
initTersusCluster "T2" = receiveWait []
initTersusCluster "T1" = do 
                            peers <- getPeers
                            t2s <- return $ findPeerByRole peers "T2"
                            pids <- mapM (\p -> (spawn p $(mkClosure 'createTersusInstance)))  t2s
                            mapM_ (\p -> send p pids) pids

initTersusClusterDevel :: String -> ProcessM ()
initTersusClusterDevel "T2" = receiveWait []
initTersusClusterDevel "T1" = do peers <- getPeers
                                 t2s <- return $ findPeerByRole peers "T2"
                                 pids <- mapM (\p -> (spawn p $(mkClosure 'createTersusDevelInstance)))  t2s
                                 mapM_ (\p -> send p pids) pids

-- Initialize Tersus in producction mode running on top of CloudHaskell
-- called by main
tersusProducction :: IO ()
tersusProducction = do 
  _ <- forkIO $ remoteInit (Just "config/servers") [TersusCluster.Cluster.__remoteCallMetaData] initTersusCluster
  remoteInit (Just "config/servers2") [TersusCluster.Cluster.__remoteCallMetaData] initTersusCluster
  return ()

-- Initialize Tersus in development mode running on top of CloudHaskell
-- called by main
tersusDevel :: IO ()
tersusDevel = do 
  _ <- forkIO $ remoteInit (Just "config/servers") [TersusCluster.Cluster.__remoteCallMetaData] initTersusClusterDevel
  remoteInit (Just "config/servers2") [TersusCluster.Cluster.__remoteCallMetaData] initTersusClusterDevel
  return ()