{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Handler.Messages where

--Author: Ernesto Rodriguez

--Description: Functions to handle the messaging system of Tersus.

import Data.Aeson as D
import Data.HashTable as H
import Import
import Model()
import Model.TMessage()
import Model.TersusResult()
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM (atomically,modifyTVar,newTVar)
import Tersus.Cluster.Types
import Data.Maybe (fromJust)
import Control.Monad (mapM)
import Data.Array.MArray
import Data.Array.IO (IOArray)
import Tersus.Cluster.DummyImports

bufferSize :: Int
bufferSize = 2

-- Create a buffer which an app instance uses to queue messages
-- for delivery. This function should be called whenever a
-- user starts an application
createMessageDeliveryBuffer :: AppInstance -> GHandler sub App ()
createMessageDeliveryBuffer appInstance = do
  master <- getYesod
  statusTable <- return $ getStatusTable master
  (statusVars,availableBuff,mappings) <- liftIO $ atomically $ do 
                                           statusVars' <- mapM (\i -> (newEmptyTMVar >>= \v -> return (i,v))) [0 .. (bufferSize - 1)]
                                           availableBuff' <- newTChan
                                           mapM_ (writeTChan availableBuff') statusVars'
                                           mappings <- newTVar []
                                           return (statusVars',availableBuff',mappings)
  varsArray <- liftIO $ (newArray_ (0,bufferSize) :: IO (IOArray Int (TMVar MessageResult)))
  liftIO $ mapM_ (\(i,var) -> writeArray varsArray i var) statusVars
  liftIO $ H.insert statusTable appInstance (mappings,varsArray,availableBuff)


createMessageMailBox :: AppInstance -> GHandler sub App ()
createMessageMailBox appInstance = do
  master <- getYesod
  mailBoxes <- return $ getMailBoxes master
  mailBox <- liftIO $ atomically $ newEmptyTMVar
  liftIO $ H.insert mailBoxes appInstance mailBox
  return ()

registerApplication :: AppInstance -> GHandler sub App ()
registerApplication appInstance = do
  master <- getYesod  
  nChannel <- return $ getNotificationsChannel master
  sPort <- return $ getSendPort master
  liftIO $ atomically $ writeTChan nChannel (Initialized' appInstance)
  return ()

initApplication :: AppInstance -> GHandler sub App ()
initApplication appInstance = do
  createMessageDeliveryBuffer appInstance
  createMessageMailBox appInstance
  registerApplication appInstance
  return ()
  

-- Deliver a message, block until it's delivery
-- status is known. The delivery status will be
-- writen on the MVar which is returned by
-- queueMessage once the destinatary creates
-- a receive request. The status MVar is deleted
-- after the message delivery
deliverTMessage :: AppInstance -> TMessage -> GHandler sub App MessageResult
deliverTMessage appInstance message = do 
                        master <- getYesod
                        channel <- return $ getSendChannel master
                        port <- return $ getSendPort master
                        statusTable <- return $ getStatusTable master
                        msgBuffer <- liftIO $ H.lookup statusTable appInstance
                        writeMsgInBuffer channel port  msgBuffer

                        where
                          writeMsgInBuffer _ _ Nothing = return EInvalidAppKey
                          writeMsgInBuffer channel port (Just msgBuffer) = do
                                        status <- liftIO $ queueMessage msgBuffer channel (message,port)
                                        waitMessage appInstance (generateHash message)                                        

-- Given an appInstance and the hashcode of a particular message,
-- blocks until the delivery status of the message is known
waitMessage :: AppInstance -> THashCode -> GHandler sub App MessageResult
waitMessage appInstance hashCode = do
  master <- getYesod
  statusTable <- return $ getStatusTable master
  msgBuffer <- liftIO $ H.lookup statusTable appInstance
  liftIO $ hashCodeLookup msgBuffer

  where
    rmHash hash ((hash',msgIndex):t) 
           | hash == hash' = t
           | otherwise = (hash',msgIndex) : (rmHash hash t)
    rmHash hash _ = []
    -- Lookup the hashcode in the message buffer if such
    -- buffer could be obtained from the app key
    hashCodeLookup Nothing = return EInvalidAppKey
    hashCodeLookup (Just (mappings,statusVars,availableBuff)) = let
                                       readMsgStatus Nothing = return EInvalidHashCode
                                       readMsgStatus (Just (_,msgIndex)) = do
                                                                  statusVar <- readArray statusVars msgIndex
                                                                  atomically $ do
                                                                           result <- takeTMVar statusVar
                                                                           mapping <- modifyTVar mappings (rmHash hashCode)
                                                                           writeTChan availableBuff (msgIndex,statusVar)
                                                                           return result
                                  in 
                                    lookupIndex hashCode mappings >>= readMsgStatus
                 

-- Add a message to the privided message queue channel. Create a
-- MVar to write the status code of the received message and
-- Save that MVar in the message status table so the receive request
-- can write the status once the message is processed
queueMessage :: TMessageSendBuff -> TMessageQueue -> TMessageEnvelope -> IO (TMVar MessageResult)
queueMessage msgBuffer channel (message,port) = atomically $ do                                                    
                                                    (msgIndex,statusVar) <- readTChan availableBuffs
                                                    modifyTVar mappings (addIndex msgIndex)
                                                    writeTChan channel (message,port)
                                                    return statusVar
                                                  where
                                                    (mappings,_,availableBuffs) = msgBuffer
                                                    addIndex msgIndex indexes = (generateHash message,msgIndex):indexes
                                                    

-- Get a list of all messages, block until at least one message has arrived
-- Note that a pattern match failure should not happe. If it happens it means
-- that the app key generaiton method is not secure
-- receieveMessage :: Address -> IO [TMessage]
receiveMessages :: AppInstance -> GHandler sub App [TMessageEnvelope]
receiveMessages appInstance = do
                master <- getYesod
                mailBoxes <- return $ getMailBoxes master                
                deliveryChan <- return $ getDeliveryChannel master
                -- Todo: what happens if the appInstance dosent exist? What should be returned?
                mailBox <- liftIO (H.lookup mailBoxes appInstance >>= return . fromJust)
                messages <- liftIO $ atomically $ do 
                                messages' <- takeTMVar mailBox
                                mapM_ (\m -> writeTChan deliveryChan m) messages'
                                return messages'
                return messages


-- TEsting funcitons 
getSendMessageR :: Handler RepJson
getSendMessageR = do
  resp <- deliverTMessage (getAppInstance dummyAddress) dummyMsg
  jsonToRepJson $ encode $ (show resp)

getRecvMessageR :: Handler RepJson
getRecvMessageR = do
  msgs <- receiveMessages (getAppInstance dummyAddress2)
  msgs' <- mapM (\(msg,_) -> return msg) msgs
  jsonToRepJson $ encode $ msgs'

getInitMessagesR :: Handler RepJson
getInitMessagesR = do
  initApplication $ getAppInstance dummyAddress
  jsonToRepJson $ encode $ ("Done" :: String)

getInitMessagesR2 :: Handler RepJson
getInitMessagesR2 = do
  initApplication $ getAppInstance dummyAddress2
  jsonToRepJson $ encode $ ("Done" :: String)

