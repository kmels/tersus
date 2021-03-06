module Tersus.Cluster where

-- | Cluster
-- Author: Ernesto Rodriguez
-- Contains all the CloudHaskell and Yesod initialization
-- functions.

import           Application
import           Control.Concurrent                                 (threadDelay)
import           Control.Concurrent.STM.TVar                        (newTVarIO)
import           Control.Distributed.Process.Binder
import           Control.Distributed.Process                 hiding (newChan)
import           Control.Distributed.Process.Backend.SimpleLocalnet
import           Control.Distributed.Process.Node                   (initRemoteTable,
                                                                     runProcess)
import           Data.HashTable.IO                                     as H
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
--                                                                      (makeTersusService,TersusServerApp)
-- import           Tersus.Cluster.TersusServiceApp                   
--                                                                     (tersusServiceApp)
-- import           Tersus.Cluster.TersusNotificationsApp               (tersusNotificationsApp)
import           Tersus.Cluster.Types
import           Yesod.Default.Config                               (fromArgs)
import           Yesod.Default.Main                                
                                                                     (defaultMain)

initDataStructures :: Process (SendAddressTable,RecvAddressTable,TersusClusterList)
initDataStructures = do
  sendTable <- liftIO $ H.new
  recvTable <- liftIO $ H.new
  clusterList <- liftIO $ newTVarIO []
  return (sendTable,recvTable,clusterList)

-- |Process that runs Tersus and Yesod in development mode
-- This is what yesod devel executes
-- For the moment it initializes a dummy address and mailbox, but this will be
-- discarded once the registration services exist
createTersusDevelInstance :: ProcessBinder -> Backend -> Process ()
createTersusDevelInstance node back = do
  (sendTable,recvTable,clusterList) <- initDataStructures
  _ <- runTersusMessaging back sendTable clusterList 1
--  _ <- initTersusServices sChannel clusterList Development
  liftIO $ do (port, app') <- getApplicationDev (node,sendTable,recvTable,clusterList)
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

-- | Container for all tersus server applications
-- data ServerAppT where
--   ServerAppT :: SafeCopy store => TersusServerApp store -> ServerAppT

-- tersusServices :: [ServerAppT]
-- tersusServices = [ServerAppT tersusNotificationsApp,ServerAppT tersusServiceApp]

-- initTersusServices :: TMessageQueue -> TersusClusterList -> TersusEnvoiernment -> Process [ProcessId]
-- initTersusServices sChannel clusterList env = mapM (\(ServerAppT app) -> spawnLocal $ makeTersusService app sChannel clusterList env) tersusServices

-- |Process that runs Tersus and Yesod in producction mode
-- This will be executed when Tersus is deployed
createTersusInstance :: ProcessBinder ->  Backend -> Process ()
createTersusInstance node back = do
  (sendTable,recvTable,clusterList) <- initDataStructures
  _ <- runTersusMessaging back sendTable clusterList 1
--  _ <- initTersusServices sChannel clusterList Development
  liftIO $ defaultMain (fromArgs parseExtra) $ makeApplicationWrapper (node,sendTable,recvTable,clusterList)
  return ()

-- |Functions to initialize all the tersus nodes and distribute the ProcessId of such nodes
-- Development and producction version of the function
initTersusCluster :: ProcessBinder -> Backend -> Process ()
initTersusCluster node back = do
  createTersusInstance node back
  receiveWait []

-- | Initialize a Tersus instance running on top of Cloud Haskell
-- the initialization is done by calling the createTersusDevelInstance
initTersusClusterDevel :: ProcessBinder -> Backend -> Process ()
initTersusClusterDevel node back = do
  _ <- spawnLocal $ createTersusDevelInstance node back
  liftIO $ loop
  return ()

-- |Initialize Tersus in producction mode running on top of CloudHaskell
-- called by main
tersusProducction :: IO ()
tersusProducction = do
  putStrLn "Init Production"
  back <- initializeBackend "127.0.0.1" "8000" initRemoteTable
  node <- newLocalNode back
  pq <- newProcessQueue node
  runProcess node $ initTersusCluster pq back
  return ()

-- |Initialize Tersus in development mode running on top of CloudHaskell
-- called by main
tersusDevel :: IO ()
tersusDevel = do
  back <- initializeBackend "127.0.0.1" "8000" initRemoteTable
  node <- newLocalNode back
  pq <- newProcessQueue node
  runProcess node $ initTersusClusterDevel pq back
  return ()
