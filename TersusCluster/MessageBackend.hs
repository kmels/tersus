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
import Model (TMessage,MessageResult(Delivered,ENoAppInstance),getAppInstance)
import Data.HashTable as H
import TersusCluster.Types
import Control.Monad.Trans (liftIO)
import Control.Concurrent.MVar (MVar, putMVar, modifyMVar_,isEmptyMVar)
import Control.Concurrent.Chan (Chan, readChan)

-- Function that handles messaging among the multiple Tersus instances
-- There are various services that work together to provide messaging
-- this function initializes all of them. For specific details view
-- each service individually
runTersusMessaging :: MessagingPorts -> AcknowledgementPorts -> TMessageQueue -> TMessageQueue -> ActionsChannel -> MailBoxTable -> AddressTable -> TMessageStatusTable -> Int -> ProcessM [ProcessId]
runTersusMessaging (mSendPort,mRecvPort) (aSendPort,aRecvPort) sendChan recvChan actionsChan mailBoxes addresses statusTable numThreads = do
  _ <- initDispatchMessageAcknowledgementService aRecvPort statusTable numThreads
  _ <- initMessageDeliveryService sendChan addresses numThreads 
  _ <- initMessageReceiveService mRecvPort mailBoxes numThreads 
  _ <- initMessageAcknowledgementService recvChan numThreads
  return []

-- Start numThreads concurrent Message Acknowledgement Dispatch Services
initDispatchMessageAcknowledgementService :: AcknowledgementRecvPort -> TMessageStatusTable -> Int -> ProcessM [ProcessId]
initDispatchMessageAcknowledgementService aRecvPort messageStatusTable numThreads = mapM (\_ -> forkProcess $ dispatchMessageAcknowledgementService aRecvPort messageStatusTable) [1 .. numThreads]

-- Message Acknowledgement Dispatch Service: This service listens to
-- the receive port of the message acknowledgement channel of this
-- cluster and writes in the message result table of this server the
-- result of sending a message from this server to another Tersus
-- instance.
dispatchMessageAcknowledgementService :: AcknowledgementRecvPort -> TMessageStatusTable -> ProcessM ()
dispatchMessageAcknowledgementService aRecvPort messageStatusTable = forever $ receiveChannel aRecvPort >>= dispatchAcknowledgement 
    where
      dispatchAcknowledgement :: MessageResultEnvelope -> ProcessM ()
      dispatchAcknowledgement (hashCode,result) = do
        statusVar <- liftIO $ H.lookup messageStatusTable hashCode
        case statusVar of
          Just statusVar' -> liftIO $ putMVar statusVar' result
          Nothing -> return () -- Todo: log, this shouldn't happen. Means that a message acknowledgement was received from a message never sent

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
                                           (msg,aPort) <- liftIO $ readChan recvChan
                                           sendChannel aPort (generateHash msg, Delivered)

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
                                                 (msg,aPort) <- liftIO $ readChan sendQueue
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
      deliverMsg (msg,aPort) _ =  sendChannel aPort (generateHash msg,ENoAppInstance) >> return ()

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
          Nothing -> sendChannel aSendPort (generateHash msg, ENoAppInstance) >> return ()
      insertMessage :: MVar [TMessageEnvelope] -> TMessageEnvelope -> IO ()
      insertMessage mBox envelope= do 
        empty <- isEmptyMVar mBox 
        if empty then putMVar mBox [envelope] else modifyMVar_ mBox (\msgs -> return (envelope:msgs))