{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Handler.Messages where

--Author: Ernesto Rodriguez

--Description: Functions to handle the messaging system of Tersus.

import Data.Aeson as D
import Data.HashTable as H
import Data.Text as T
import Import
import Model
import Model.TMessage
import Model.TersusResult
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM (atomically,newEmptyTMVar,takeTMVar,putTMVar,modifyTVar,newTVar)
import TersusCluster.Types
import Data.Maybe (fromJust)
import System.IO.Unsafe (unsafePerformIO)
import Data.Time.Clock (getCurrentTime)
import Control.Monad (mapM)
import Data.List (find)
import Data.Array.MArray
import Data.Array.IO (IOArray)

bufferSize = 20

-- Create a buffer which an app instance uses to queue messages
-- for delivery. This function should be called whenever a
-- user starts an application
createMessageDeliveryBuffer :: AppInstance -> GHandler sub App ()
createMessageDeliveryBuffer app = do
  master <- getYesod
  statusTable <- return $ getStatusTable master
  (statusVars,availableBuff,mappings) <- liftIO $ atomically $ do 
                                           statusVars' <- mapM (\i -> (newEmptyTMVar >>= \v -> return (i,v))) [0 .. bufferSize]
                                           availableBuff' <- newTChan
                                           mapM_ (writeTChan availableBuff') statusVars'
                                           mappings <- newTVar []
                                           return (statusVars',availableBuff',mappings)
  varsArray <- liftIO $ (newArray_ (0,bufferSize) :: IO (IOArray Int (TMVar MessageResult)))
  liftIO $ mapM_ (\(i,var) -> writeArray varsArray i var) statusVars
  liftIO $ H.insert statusTable app (mappings,varsArray,availableBuff)


createMessageMailBox :: AppInstance -> GHandler sub App ()
createMessageMailBox appInstance = do
  master <- getYesod
  mailBoxes <- return $ getMailBoxes master
  mailBox <- liftIO $ atomically $ newEmptyTMVar
  liftIO $ H.insert mailBoxes appInstance mailBox
  return ()

initApplication :: AppInstance -> GHandler sub App ()
initApplication appInstance = do
  createMessageDeliveryBuffer appInstance
  createMessageMailBox appInstance
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
                        -- Todo: properly process the maybe
                        Just msgBuffer <- liftIO $ H.lookup statusTable appInstance
                        status <- liftIO $ queueMessage msgBuffer channel (message,port)
                        waitMessage appInstance (generateHash message)

waitMessage :: AppInstance -> THashCode -> GHandler sub App MessageResult
waitMessage appInstance hashCode = do
  master <- getYesod
  statusTable <- return $ getStatusTable master
  Just (mappings,statusVars,availableBuff) <- liftIO $ H.lookup statusTable appInstance
  -- Todo: Error Handling for Maybe monad
  Just (_,index) <- liftIO $ lookupIndex hashCode mappings
  statusVar <- liftIO $ readArray statusVars index
  liftIO $ atomically $ do
    result <- takeTMVar statusVar
    map <- modifyTVar mappings (rmHash hashCode)
    writeTChan availableBuff (index,statusVar)
    return result

  where
    rmHash hash ((hash',index):t) 
           | hash == hash' = t
           | otherwise = (hash',index) : (rmHash hash t)
    rmHash hash _ = []                          
                         

-- Add a message to the privided message queue channel. Create a
-- MVar to write the status code of the received message and
-- Save that MVar in the message status table so the receive request
-- can write the status once the message is processed
queueMessage :: TMessageSendBuff -> TMessageQueue -> TMessageEnvelope -> IO (TMVar MessageResult)
queueMessage msgBuffer channel (message,port) = atomically $ do                                                    
                                                    (index,statusVar) <- readTChan availableBuffs
                                                    modifyTVar mappings (addIndex index)
                                                    writeTChan channel (message,port)
                                                    return statusVar
                                                  where
                                                    (mappings,_,availableBuffs) = msgBuffer
                                                    addIndex index indexes = (generateHash message,index):indexes
                                                    

-- Get a list of all messages, block until at least one message has arrived
-- Note that a pattern match failure should not happe. If it happens it means
-- that the app key generaiton method is not secure
-- receieveMessage :: Address -> IO [TMessage]
receiveMessages :: AppInstance -> GHandler sub App [TMessageEnvelope]
receiveMessages appInstance = do
                master <- getYesod
                mailBoxes <- return $ getMailBoxes master                
                deliveryChan <- return $ getDeliveryChannel master
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
  msgs <- receiveMessages (getAppInstance dummyAddress)
  msgs' <- mapM (\(msg,_) -> return msg) msgs
  jsonToRepJson $ encode $ msgs'

getInitMessagesR :: Handler RepJson
getInitMessagesR = do
  initApplication $ getAppInstance dummyAddress
  jsonToRepJson $ encode $ ("Done" :: String)

dummyUser = User (T.pack "neto") (Just (T.pack "1234")) []
                       
-- This is a dummy datatype only to show that this works
-- It will be removed and never used
-- unsafePerformIO is there just because it's simpler and
-- this will not be part of tersus
dummyApp = TApplication (T.pack "emacs") (T.pack "identifier") (T.pack "description dummy") (Just (T.pack "url")) (T.pack "mail@place.com") (unsafePerformIO getCurrentTime)  (T.pack "appkey")

dummyMsg = TMessage dummyUser dummyUser dummyApp dummyApp (T.pack "Alonso") (unsafePerformIO getCurrentTime)

dummyAddress = Address dummyUser dummyApp
