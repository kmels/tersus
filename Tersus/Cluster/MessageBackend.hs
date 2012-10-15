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
import           Control.Concurrent.STM                             (STM,
                                                                     atomically)
import           Control.Concurrent.STM.TChan                      
                                                                     (isEmptyTChan,
                                                                     readTChan,
                                                                     writeTChan)
import           Control.Concurrent.STM.TMVar                       (TMVar,
                                                                     isEmptyTMVar,
                                                                     putTMVar,
                                                                     takeTMVar)
import           Control.Concurrent.STM.TVar                        (modifyTVar,
                                                                     readTVar)
import           Control.Distributed.Process                        as P
import           Control.Distributed.Process.Backend.SimpleLocalnet
import           Control.Monad                                      (forever)
import           Control.Monad                                      (foldM)
import           Data.Array.IO                                      (readArray)
import           Data.HashTable                                     as H
import           Model                                             
                                                                     (MessageResult (Delivered, ENoAppInstance),
                                                                     getAppInstance,
                                                                     getSendAppInstance)
import           Prelude
import           Tersus.Cluster.Types

-- This is the name that the process used to establish communication with other
-- Tersus instances will be called.
processBinderName :: String
processBinderName = "TersusProcessBinder"

-- Function that handles messaging among the multiple Tersus instances
-- There are various services that work together to provide messaging
-- this function initializes all of them. For specific details view
-- each service individually
runTersusMessaging :: Backend -> MessagingPorts -> AcknowledgementPorts -> NotificationsPorts -> TMessageQueue -> TMessageQueue -> NotificationsChannel -> AppInstanceTable -> AddressTable -> TersusClusterList -> Int -> Process [ProcessId]
runTersusMessaging backend (mSendPort,mRecvPort) (_,aRecvPort) (nSendPort,nRecvPort) sendChannel recvChannel notificationsChan appEnvs addresses clusterList numThreads = do
  _ <- initDispatchMessageAcknowledgementService aRecvPort appEnvs numThreads
  _ <- initMessageDeliveryService sendChannel addresses numThreads
  _ <- initMessageReceiveService mRecvPort appEnvs numThreads
  _ <- initMessageAcknowledgementService recvChannel numThreads
  _ <- initNotificationsService mSendPort clusterList notificationsChan numThreads
  _ <- initNotificationsDispatchService addresses nRecvPort numThreads
  _ <- initProcessBinderService backend nSendPort clusterList
  return []

initProcessBinderService :: Backend -> NotificationsSendPort -> TersusClusterList -> Process [ProcessId]
initProcessBinderService backend nSendPort clusterList = do
  initLock <- liftIO $ newEmptyMVar
  p <- spawnLocal $ processBinderService backend nSendPort clusterList initLock
  _ <- liftIO $ takeMVar initLock
  return [p]


-- Process that registers new tersus instances once they are started and
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





-- Forks the given Process (proc) numThread times and returns a list with the process id of the forked processes
forkProcFun :: Int -> Process () -> Process [ProcessId]
forkProcFun numThreads proc = mapM (\_-> spawnLocal proc) [1 .. numThreads]

initNotificationsDispatchService :: AddressTable -> NotificationsRecvPort -> Int -> Process [ProcessId]
initNotificationsDispatchService addressTable notificationsRecvPort numThreads = forkProcFun numThreads $ notificationsDispatchService addressTable notificationsRecvPort

notificationsDispatchService :: AddressTable -> NotificationsRecvPort -> Process ()
notificationsDispatchService addressTable notificationsRecvPort = forever $ receiveChan notificationsRecvPort >>= dispatchNotifications
    where
      dispatchNotifications :: [TersusNotification] -> Process ()
      dispatchNotifications notifications = mapM_ (liftIO . processNotification) notifications

      processNotification :: TersusNotification -> IO ()
      -- Application instance regitered notificaiton, add the address to the addresstable
      processNotification (Initialized appInstance (msgSendPort,hash)) = liftIO $ H.insert addressTable appInstance (msgSendPort,hash)
      -- Application instance closed remove form address table if hashes match
      processNotification (Closed (appInstance,hash)) = H.lookup addressTable appInstance >>= handleEntry hash appInstance
      -- Handle unknown notifications, should not happen and logging should be used here since this suggests a
      -- bug in Data.Binary package or a bug in Tersus
      processNotification NotificationUnknown = return ()
      -- Helper to handle entry lookup and delete if necesary
      handleEntry hash appInstance (Just (_,hash'))
                  | hash' == hash = H.delete addressTable appInstance
                  | otherwise = return ()
      handleEntry _ _ _ = return ()




-- Create numThreads instances of notificationsServices
initNotificationsService :: MessageSendPort -> TersusClusterList -> NotificationsChannel -> Int -> Process [ProcessId]
initNotificationsService msgSendPort clusterList notificationsChan numThreads = forkProcFun numThreads $ notificationsService msgSendPort clusterList notificationsChan

-- Process that notifies all tersus instances about notifications occuring to app instances running in this server
-- Theese notifications can be an app instance being created or being destroyed. They are written in the Yesod
-- side to the notificationsChan and this process reads notifications (10 by 10) and sends them to all tersus instances
-- known to CloudHaskell.
notificationsService :: MessageSendPort -> TersusClusterList -> NotificationsChannel -> Process ()
notificationsService msgSendPort clusterList notificationsChan = forever $ do
                                                       notifications <- liftIO $ atomically $ getNumNotifications 10
                                                       -- The list of available clusters is read on every iteration in case
                                                       -- new clusters become available. It could be configured to be updated
                                                       -- every particular defined period
                                                       clusters <- liftIO $ atomically $ readTVar clusterList
                                                       mapM_ (sendNotifications notifications clusterList) clusters
    where
      -- Try to read at most n notifications from the notifications channel
      -- note that this function takes constant time to execute and dosen't break
      -- if the channel gets empty since other notifications could arrive
      -- a little bit later
      getNumNotifications n = foldM (getNotification) [] [1 .. n]

      getNotification :: [TersusNotification] -> Int -> STM [TersusNotification]
      getNotification prev x
                      | x < 2 = readTChan notificationsChan >>= \n -> return ((packNotification n):prev)
                      | otherwise = isEmptyTChan notificationsChan >>= \empty ->
                                    case empty of
                                      True -> return prev
                                      False -> getNotification prev 1
      -- Todo, have a real hash function.
      packNotification :: TersusSimpleNotification -> TersusNotification
      packNotification (Initialized' appInstance) = Initialized appInstance (msgSendPort,"HashLoco")
      packNotification (Closed' appInstance) = Closed (appInstance,"HashLoco")


-- Transmit the notification to a particular node, if that node is no longer available
-- it's removed from the list using exception handling
sendNotifications :: [TersusNotification] -> TersusClusterList -> (NotificationsSendPort,ProcessId) -> Process ()
sendNotifications notifications clusterList (nSendPort',pid) = P.catch sendData handleSendError
  where
    sendData = sendChan nSendPort' notifications
    handleSendError :: ProcessLinkException -> Process ()
    handleSendError _ = liftIO (atomically $ modifyTVar clusterList $ deleteNode pid)

deleteNode :: Eq b => b -> [(a,b)] -> [(a,b)]
deleteNode _ [] = []
deleteNode pid ((nSendPort,pid'):t)
    | pid == pid' = deleteNode pid t
    | otherwise = (nSendPort,pid') : (deleteNode pid t)

-- Start numThreads concurrent Message Acknowledgement Dispatch Services
initDispatchMessageAcknowledgementService :: AcknowledgementRecvPort -> AppInstanceTable -> Int -> Process [ProcessId]
initDispatchMessageAcknowledgementService aRecvPort appEnvs numThreads = forkProcFun numThreads $ dispatchMessageAcknowledgementService aRecvPort appEnvs

-- Message Acknowledgement Dispatch Service: This service listens to
-- the receive port of the message acknowledgement channel of this
-- cluster and writes in the message result table of this server the
-- result of sending a message from this server to another Tersus
-- instance. In other words inform an application if the message could
-- be delivered or not.
dispatchMessageAcknowledgementService :: AcknowledgementRecvPort -> AppInstanceTable -> Process ()
dispatchMessageAcknowledgementService aRecvPort appEnvs = forever $ receiveChan aRecvPort >>= dispatchAcknowledgement
    where
      dispatchAcknowledgement :: MessageResultEnvelope -> Process ()
      dispatchAcknowledgement (hashCode,result,address) = do
        Just (mappings,statusVars,_) <- liftIO $ getMessageBuffer appEnvs address
        -- add error handling
        (Just (_,index)) <- liftIO $ lookupIndex hashCode mappings
        statusVar <- liftIO $ readArray statusVars index
        case (Just statusVar) of
          Just statusVar' -> liftIO $ atomically $ putTMVar statusVar' result
          Nothing -> return () -- Todo: log, this shouldn't happen. Means that a message acknowledgement was received from a message never sent. Obviously this is impossible so this case implies that messages are being wrongly acknowledged

-- Start numThreads concurrent Message Acknowledgement Services
initMessageAcknowledgementService :: TMessageQueue -> Int -> Process [ProcessId]
initMessageAcknowledgementService recvChan numThreads = mapM (\_ -> spawnLocal $ messageAcknowledgementService recvChan) [1 .. numThreads]

-- Message Acknowledgement Service: When a message is successfully
-- delivered to a AppInstance in this server, it is written to
-- the message receive queue. This service reads messages
-- that arrive to this queue and sends a message delivered
-- status to the Node from where this message came.
messageAcknowledgementService :: TMessageQueue -> Process ()
messageAcknowledgementService recvChan = forever $ do
                                           (msg,aPort) <- liftIO $ atomically $ readTChan recvChan
                                           sendChan aPort (generateHash msg, Delivered , getSendAppInstance msg)

-- Start numThreads concurrent Message Delivery Services
initMessageDeliveryService :: TMessageQueue -> AddressTable -> Int -> Process [ProcessId]
initMessageDeliveryService sendQueue addresses numThreads = mapM (\_ -> spawnLocal $ messageDeliveryService sendQueue addresses) [1 .. numThreads]

-- Message Delivery Service: Whenever a AppInstance wants to send a message,
-- it writes the message to the SendMessage queue (sendQueue) packed with the
-- sendPort of the acknowledgement channel of it's server (so the other server
-- can reply with the result of sending the message). This process reads messages
-- (packed as MessageEnvelopes) from the sendQueue, obtains the sendPort of
-- the channel where this message should be sent from the address table
-- and writes the message to that port
messageDeliveryService :: TMessageQueue -> AddressTable -> Process ()
messageDeliveryService sendQueue addresses = forever $ do
  liftIO $ putStrLn "Running deliver service"
  (msg,aPort) <- liftIO $ atomically $ readTChan sendQueue
  let
    handleMsgError :: ProcessLinkException -> Process ()
    handleMsgError _ = (liftIO $ H.delete addresses $ getAppInstance msg) >> deliverMsg (msg,aPort) Nothing
  destPort <- liftIO $ H.lookup addresses $ getAppInstance msg
  P.catch (deliverMsg (msg,aPort) destPort) handleMsgError

    where
      -- This function is given the port where the message is delivered
      -- obtained from the address table. If no port exists, it means
      -- that the AppInstance for whom this message is does not exist.
      -- In that case, a No app instance delivery status is sent to
      -- the sender
      deliverMsg :: TMessageEnvelope -> Maybe TersusProcessM -> Process ()
      deliverMsg envelope (Just (destPort,_)) = sendChan destPort envelope >> return ()
      deliverMsg (msg,aPort) _ =  sendChan aPort (generateHash msg,ENoAppInstance,getSendAppInstance msg) >> return ()

-- Create numThreads concurrent Message Receive Services
initMessageReceiveService :: MessageRecvPort -> AppInstanceTable -> Int -> Process [ProcessId]
initMessageReceiveService mRecvPort appEnvs numThreads = mapM (\_ -> spawnLocal $ messageReceiveService mRecvPort appEnvs) [1 .. numThreads]

-- Message Receive Service: Every tersus instance has a channel to receive messages.
-- The send port of the channel is distributed to all other tersus instances so
-- they can send messages to this instance. The receive port is read by this
-- process. Whenever a new TersusMessage for a AppInstance in this TersusCluster
-- is sent, it is read from the mRecvPort. This process reads the messages and
--- writes them to the corresponding mailbox
messageReceiveService :: MessageRecvPort -> AppInstanceTable -> Process ()
messageReceiveService mRecvPort appEnvs = forever $ receiveChan mRecvPort >>= writeToMailBox
    where
      writeToMailBox :: TMessageEnvelope -> Process ()
      writeToMailBox (msg,aSendPort) = do
        addr <- return $ getAppInstance msg
        mBox <- liftIO $ getMailBox appEnvs addr
        case mBox of
          Just mBox' -> liftIO $ atomically $ writeTChan mBox' (msg,aSendPort)
          Nothing -> sendChan aSendPort (generateHash msg, ENoAppInstance, getSendAppInstance msg) >> return ()