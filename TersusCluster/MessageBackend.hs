{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
module TersusCluster.MessageBackend where

-- Tersus Message Backend
-- Author: Ernesto Rodriguez
-- Description: Contains all the services that run behind
-- to send, receive and acknowledge Tersus Messages.
-- It will also contain services to register and unregister
-- AppInstances accross TersusClusters

import Prelude
import Control.Monad (forever)
import Remote.Process (forkProcess)
import Remote
import TersusCluster.Types
import Model (MessageResult(Delivered,ENoAppInstance),getAppInstance,getSendAppInstance)
import Data.HashTable as H
import Control.Monad.Trans (liftIO)
import Control.Concurrent.STM.TMVar (TMVar, putTMVar, isEmptyTMVar,takeTMVar)
import Control.Concurrent.STM.TChan (readTChan,isEmptyTChan)
import Control.Concurrent.STM.TVar (readTVar)
import Control.Concurrent.STM (atomically, STM)
import Data.Array.IO (readArray)
import Control.Monad (foldM)

-- Function that handles messaging among the multiple Tersus instances
-- There are various services that work together to provide messaging
-- this function initializes all of them. For specific details view
-- each service individually
runTersusMessaging :: MessagingPorts -> AcknowledgementPorts -> NotificationsPorts -> TMessageQueue -> TMessageQueue -> NotificationsChannel -> MailBoxTable -> AddressTable -> TersusClusterList -> TMessageStatusTable -> Int -> ProcessM [ProcessId]
runTersusMessaging (mSendPort,mRecvPort) (aSendPort,aRecvPort) (nSendPort,nRecvPort) sendChan recvChan notificationsChan mailBoxes addresses clusterList statusTable numThreads = do
  _ <- initDispatchMessageAcknowledgementService aRecvPort statusTable numThreads
  _ <- initMessageDeliveryService sendChan addresses numThreads 
  _ <- initMessageReceiveService mRecvPort mailBoxes numThreads 
  _ <- initMessageAcknowledgementService recvChan numThreads
  _ <- initNotificationsService mSendPort clusterList notificationsChan numThreads
  _ <- initNotificationsDispatchService addresses nRecvPort numThreads
  return []

-- Forks the given Process (proc) numThread times and returns a list with the process id of the forked processes
forkProcFun :: Int -> ProcessM () -> ProcessM [ProcessId]
forkProcFun numThreads proc = mapM (\_-> forkProcess proc) [1 .. numThreads]

initNotificationsDispatchService :: AddressTable -> NotificationsRecvPort -> Int -> ProcessM [ProcessId]
initNotificationsDispatchService addressTable notificationsRecvPort numThreads = forkProcFun numThreads $ notificationsDispatchService addressTable notificationsRecvPort

notificationsDispatchService :: AddressTable -> NotificationsRecvPort -> ProcessM ()
notificationsDispatchService addressTable notificationsRecvPort = forever $ receiveChannel notificationsRecvPort >>= dispatchNotifications
    where
      dispatchNotifications :: [TersusNotification] -> ProcessM ()
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
initNotificationsService :: MessageSendPort -> TersusClusterList -> NotificationsChannel -> Int -> ProcessM [ProcessId]
initNotificationsService msgSendPort clusterList notificationsChan numThreads = forkProcFun numThreads $ notificationsService msgSendPort clusterList notificationsChan

-- Process that notifies all tersus instances about notifications occuring to app instances running in this server
-- Theese notifications can be an app instance being created or being destroyed. They are written in the Yesod
-- side to the notificationsChan and this process reads notifications (10 by 10) and sends them to all tersus instances
-- known to CloudHaskell.
notificationsService :: MessageSendPort -> TersusClusterList -> NotificationsChannel -> ProcessM ()
notificationsService msgSendPort clusterList notificationsChan = forever $ do
                                                       notifications <- liftIO $ atomically $ getNumNotifications 10
                                                       -- The list of available clusters is read on every iteration in case 
                                                       -- new clusters become available. It could be configured to be updated
                                                       -- every particular defined period
                                                       clusters <- liftIO $ atomically $ readTVar clusterList 
                                                       mapM_ (\clusterPort-> sendChannel clusterPort notifications) clusters
                                                       return ()
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

-- Start numThreads concurrent Message Acknowledgement Dispatch Services
initDispatchMessageAcknowledgementService :: AcknowledgementRecvPort -> TMessageStatusTable -> Int -> ProcessM [ProcessId]
initDispatchMessageAcknowledgementService aRecvPort messageStatusTable numThreads = forkProcFun numThreads $ dispatchMessageAcknowledgementService aRecvPort messageStatusTable

-- Message Acknowledgement Dispatch Service: This service listens to
-- the receive port of the message acknowledgement channel of this
-- cluster and writes in the message result table of this server the
-- result of sending a message from this server to another Tersus
-- instance. In other words inform an application if the message could
-- be delivered or not.
dispatchMessageAcknowledgementService :: AcknowledgementRecvPort -> TMessageStatusTable -> ProcessM ()
dispatchMessageAcknowledgementService aRecvPort messageStatusTable = forever $ receiveChannel aRecvPort >>= dispatchAcknowledgement 
    where
      dispatchAcknowledgement :: MessageResultEnvelope -> ProcessM ()
      dispatchAcknowledgement (hashCode,result,address) = do
        Just (mappings,statusVars,availableBuff) <- liftIO $ H.lookup messageStatusTable address
        -- add error handling
        (Just (_,index)) <- liftIO $ lookupIndex hashCode mappings
        statusVar <- liftIO $ readArray statusVars index
        case (Just statusVar) of
          Just statusVar' -> liftIO $ atomically $ putTMVar statusVar' result
          Nothing -> return () -- Todo: log, this shouldn't happen. Means that a message acknowledgement was received from a message never sent. Obviously this is impossible so this case implies that messages are being wrongly acknowledged

-- Start numThreads concurrent Message Acknowledgement Services
initMessageAcknowledgementService :: TMessageQueue -> Int -> ProcessM [ProcessId]
initMessageAcknowledgementService recvChan numThreads = mapM (\_ -> forkProcess $ messageAcknowledgementService recvChan) [1 .. numThreads]

-- Message Acknowledgement Service: When a message is successfully
-- delivered to a AppInstance in this server, it is written to
-- the message receive queue. This service reads messages
-- that arrive to this queue and sends a message delivered
-- status to the Node from where this message came.
messageAcknowledgementService :: TMessageQueue -> ProcessM ()
messageAcknowledgementService recvChan = forever $ do 
                                           (msg,aPort) <- liftIO $ atomically $ readTChan recvChan
                                           sendChannel aPort (generateHash msg, Delivered , getSendAppInstance msg)

-- Start numThreads concurrent Message Delivery Services
initMessageDeliveryService :: TMessageQueue -> AddressTable -> Int -> ProcessM [ProcessId]
initMessageDeliveryService sendQueue addresses numThreads = mapM (\_ -> forkProcess $ messageDeliveryService sendQueue addresses) [1 .. numThreads]

-- Message Delivery Service: Whenever a AppInstance wants to send a message,
-- it writes the message to the SendMessage queue (sendQueue) packed with the
-- sendPort of the acknowledgement channel of it's server (so the other server
-- can reply with the result of sending the message). This process reads messages
-- (packed as MessageEnvelopes) from the sendQueue, obtains the sendPort of
-- the channel where this message should be sent from the address table
-- and writes the message to that port
messageDeliveryService :: TMessageQueue -> AddressTable -> ProcessM () 
messageDeliveryService sendQueue addresses = forever $ do
                                                 liftIO $ putStrLn "Running deliver service"
                                                 (msg,aPort) <- liftIO $ atomically $ readTChan sendQueue
                                                 destPort <- liftIO $ H.lookup addresses $ getAppInstance msg
                                                 deliverMsg (msg,aPort) destPort
    where
      -- This function is given the port where the message is delivered
      -- obtained from the address table. If no port exists, it means
      -- that the AppInstance for whom this message is does not exist.
      -- In that case, a No app instance delivery status is sent to
      -- the sender
      deliverMsg :: TMessageEnvelope -> Maybe TersusProcessM -> ProcessM ()
      deliverMsg envelope (Just (destPort,_)) = sendChannel destPort envelope >> return ()
      deliverMsg (msg,aPort) _ =  sendChannel aPort (generateHash msg,ENoAppInstance,getSendAppInstance msg) >> return ()

-- Create numThreads concurrent Message Receive Services
initMessageReceiveService :: MessageRecvPort -> MailBoxTable -> Int -> ProcessM [ProcessId]
initMessageReceiveService mRecvPort mailBoxes numThreads = mapM (\_ -> forkProcess $ messageReceiveService mRecvPort mailBoxes) [1 .. numThreads]

-- Message Receive Service: Every tersus instance has a channel to receive messages.
-- The send port of the channel is distributed to all other tersus instances so 
-- they can send messages to this instance. The receive port is read by this
-- process. Whenever a new TersusMessage for a AppInstance in this TersusCluster
-- is sent, it is read from the mRecvPort. This process reads the messages and
--- writes them to the corresponding mailbox
messageReceiveService :: MessageRecvPort -> MailBoxTable -> ProcessM ()
messageReceiveService mRecvPort mailBoxes = forever $ receiveChannel mRecvPort >>= writeToMailBox
    where
      writeToMailBox :: TMessageEnvelope -> ProcessM ()
      writeToMailBox (msg,aSendPort) = do
        addr <- return $ getAppInstance msg
        mBox <- liftIO $ H.lookup mailBoxes addr
        case mBox of
          Just mBox' -> liftIO $ insertMessage mBox' (msg,aSendPort)
          Nothing -> sendChannel aSendPort (generateHash msg, ENoAppInstance, getSendAppInstance msg) >> return ()
      insertMessage :: TMVar [TMessageEnvelope] -> TMessageEnvelope -> IO ()
      insertMessage mBox envelope = atomically $ do 
                                      empty <- isEmptyTMVar mBox
                                      if empty then putTMVar mBox [envelope] else modifyTMVar mBox (\msgs -> return (envelope:msgs))

modifyTMVar :: TMVar a -> (a -> STM a) -> STM ()
modifyTMVar var f = do
                val <- takeTMVar var
                val' <- f val
                putTMVar var (val')