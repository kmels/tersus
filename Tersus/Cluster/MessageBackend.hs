{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes         #-}
module Tersus.Cluster.MessageBackend where

-- Tersus Message Backend
-- Author: Ernesto Rodriguez
-- Description: Contains all the services that run behind
-- to send, receive and acknowledge Tersus Messages.
-- It will also contain services to register and unregister
-- AppInstances accross TersusClusters

import           Control.Concurrent.MVar                            (MVar,
                                                                     newEmptyMVar,
                                                                     putMVar,
                                                                     takeMVar)
import           Control.Concurrent.STM                             (atomically)
import           Control.Concurrent.STM.TMVar                       (isEmptyTMVar,
                                                                     putTMVar,
                                                                     takeTMVar)
import           Control.Concurrent.STM.TVar                        (modifyTVar,
                                                                     readTVar)
import           Control.Distributed.Process                        as P
import           Control.Distributed.Process.Backend.SimpleLocalnet
import           Control.Monad                                      (forever)
import qualified Data.HashTable.IO                                  as H
import           Prelude
import           Tersus.Cluster.Types

-- | This is the name that the process used to establish communication with other
-- Tersus instances will be called.
processBinderName :: String
processBinderName = "TersusProcessBinder"

-- | Function that handles messaging among the multiple Tersus instances
-- There are various services that work together to provide messaging
-- this function initializes all of them. For specific details view
-- each service individually
runTersusMessaging :: Backend -> SendAddressTable -> TersusClusterList -> Int -> Process [ProcessId]
runTersusMessaging backend addresses clusterList numThreads = do
  (nSendPort,nRecvPort) <- newChan
  p6 <- initNotificationsDispatchService addresses nRecvPort numThreads
  p7 <- initProcessBinderService backend nSendPort clusterList
  _<- spawnLocal $ do
    mapM_ (monitor) $ p6++p7
    n <- expect :: Process ProcessMonitorNotification
    liftIO $ putStrLn $ "\n\nProcess Died \n" ++ (show n) ++ "\n\n"
  
  return []

initProcessBinderService :: Backend -> NotificationsSendPort -> TersusClusterList -> Process [ProcessId]
initProcessBinderService backend nSendPort clusterList = do
  initLock <- liftIO $ newEmptyMVar
  p <- spawnLocal $ processBinderService backend nSendPort clusterList initLock
  _ <- liftIO $ takeMVar initLock
  return [p]


-- | Process that registers new tersus instances once they are started and
-- ready to start messaging. The registration process is as follows:
-- 1. A tersus instance is created and starts the processBinderService (PBS).
-- 2. The PBS gets a list of all nodes and sends it's processId and it's corresponding notifications port
-- 3. When a NotificatiionsPort is received, this port is registered in the list of Nodes, therefore
-- notifications about applications in this Node are communicated to the newly created node.
-- 4. When a processId is received, this indicates that this process is requesting the NotificationsPort
-- of the process, therefore the port is sent so the process can start notifing about the activities in the
-- of the applications running in that Node.
-- Note: The PBS has a special name for other nodes to query. This name is held in the value of processBinderName
processBinderService :: Backend -> NotificationsSendPort -> TersusClusterList -> MVar Int -> Process ()
processBinderService backend nSendPort clusterList lock = do
  myPid <- getSelfPid
  register processBinderName myPid
  peers <- liftIO $ findPeers backend peerSearch
  mapM_ (\n -> whereisRemoteAsync n processBinderName) peers
  handleNotificationsPortRegistration (nSendPort,myPid)
  _ <- spawnLocal $ forever $ receiveWait [matchWhereis] >>= \_ -> return ()
  liftIO $ putMVar lock 1
  forever receiveSendChannels

    where
      matchWhereis = match $ \(WhereIsReply _ pId) -> maybeSendPid pId
      maybeSendPid (Just pid) = do
        myPid <- getSelfPid
        send pid nSendPort
        if pid == myPid then return () else send pid myPid
      -- This case would indicate that a node has been initialized but the processBinderService is not started yet
      -- So it will be ignored since the other cluster will register this cluster once it's processBinderService
      -- is started.
      maybeSendPid Nothing = return ()

      addTersusSendPort :: (NotificationsSendPort,ProcessId) -> [(NotificationsSendPort,ProcessId)] -> [(NotificationsSendPort,ProcessId)]
      addTersusSendPort nSendPort' sendPorts = nSendPort' : sendPorts

      handleNotificationsPortRegistration :: (NotificationsSendPort,ProcessId) -> Process ()
      handleNotificationsPortRegistration nSendPort' = liftIO $ atomically $ modifyTVar clusterList $ addTersusSendPort nSendPort'

      handleNodeRegistration :: ProcessId -> Process ()
      handleNodeRegistration pid = do
        myPid <- getSelfPid
        if myPid == pid then return () else send pid (nSendPort,myPid)

      receiveSendChannels = receiveWait [match handleNodeRegistration, match handleNotificationsPortRegistration]

-- | Forks the given Process (proc) numThread times and returns a list with the process id of the forked processes
forkProcFun :: Int -> Process () -> Process [ProcessId]
forkProcFun numThreads proc = mapM (\_-> spawnLocal proc) [1 .. numThreads]

initNotificationsDispatchService :: SendAddressTable -> NotificationsRecvPort -> Int -> Process [ProcessId]
initNotificationsDispatchService addressTable notificationsRecvPort numThreads = forkProcFun numThreads $ notificationsDispatchService addressTable notificationsRecvPort

notificationsDispatchService :: SendAddressTable -> NotificationsRecvPort -> Process ()
notificationsDispatchService addressTable notificationsRecvPort = forever $ receiveChan notificationsRecvPort >>= dispatchNotifications
    where
      dispatchNotifications :: [TersusNotification] -> Process ()
      dispatchNotifications notifications = mapM_ (liftIO . processNotification) notifications

      processNotification :: TersusNotification -> IO ()
      -- Application instance regitered notificaiton, add the address to the addresstable
      processNotification (Initialized appInstance (msgSendPort,ackPort,hash)) = liftIO $ H.insert addressTable appInstance $ MessageSendChannels {messageSendPort=msgSendPort,acknowledgementSendPort=ackPort,hashCode="someHashCode"}
      -- Application instance closed remove form address table if hashes match
      processNotification (Closed (appInstance,hash)) = H.lookup addressTable appInstance >>= handleEntry hash appInstance
      -- Handle unknown notifications, should not happen and logging should be used here since this suggests a
      -- bug in Data.Binary package or a bug in Tersus
      processNotification NotificationUnknown = return ()
      -- Helper to handle entry lookup and delete if necesary
      handleEntry hash appInstance (Just channels)
                  | (hashCode channels) == hash = H.delete addressTable appInstance
                  | otherwise = return ()
      handleEntry _ _ _ = return ()

deleteNode :: Eq b => b -> [(a,b)] -> [(a,b)]
deleteNode _ [] = []
deleteNode pid ((nSendPort,pid'):t)
    | pid == pid' = deleteNode pid t
    | otherwise = (nSendPort,pid') : (deleteNode pid t)