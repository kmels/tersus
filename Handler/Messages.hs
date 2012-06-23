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
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import TersusCluster.Types
import Data.Maybe (fromJust)
import System.IO.Unsafe (unsafePerformIO)
import Data.Time.Clock (getCurrentTime)
import Control.Monad (mapM)

-- Deliver a message, block until it's delivery
-- status is known. The delivery status will be
-- writen on the MVar which is returned by
-- queueMessage once the destinatary creates
-- a receive request. The status MVar is deleted
-- after the message delivery
deliverTMessage :: TMessage -> GHandler sub App MessageResult
deliverTMessage message = do 
                        master <- getYesod
                        channel <- return $ getSendChannel master
                        port <- return $ getSendPort master
                        statusTable <- return $ getStatusTable master
                        status <- liftIO $ queueMessage statusTable channel (message,port)
                        --Todo: exception handling if the message
                        --is never received. A timeout or something
                        --similar. Also if the request is 
                        --aborted, the MVar should be removed
                        result <- liftIO $ takeMVar status
                        liftIO $ H.delete statusTable (generateHash message)
                        return result

-- Add a message to the privided message queue channel. Create a
-- MVar to write the status code of the received message and
-- Save that MVar in the message status table so the receive request
-- can write the status once the message is processed
queueMessage :: TMessageStatusTable -> TMessageQueue -> TMessageEnvelope -> IO (MVar MessageResult)
queueMessage statusTable channel (message,port) = do 
                              status <- newEmptyMVar
                              H.insert statusTable (generateHash message) status                                                            
                              writeChan channel (message,port)
                              return status
                              


-- Get a list of all messages, block until at least one message has arrived
-- Note that a pattern match failure should not happe. If it happens it means
-- that the app key generaiton method is not secure
-- receieveMessage :: Address -> IO [TMessage]
receiveMessages :: Addressable a => a -> GHandler sub App [TMessageEnvelope]
receiveMessages address = do
                master <- getYesod
                mailBoxes <- return $ getMailBoxes master                
                deliveryChan <- return $ getDeliveryChannel master
                mailBox <- liftIO (H.lookup mailBoxes (getAppInstance address) >>= return . fromJust)
                messages <- liftIO $ takeMVar mailBox
                liftIO $ mapM_ (writeChan deliveryChan) messages
                return messages


-- TEsting funcitons
getSendMessageR :: Handler RepJson
getSendMessageR = do
  resp <- deliverTMessage dummyMsg
  jsonToRepJson $ encode $ (show resp)

getRecvMessageR :: Handler RepJson
getRecvMessageR = do
  msgs <- receiveMessages dummyAddress
  msgs' <- mapM (\(msg,_) -> return msg) msgs
  jsonToRepJson $ encode $ msgs'

dummyUser = User (T.pack "neto") (Just (T.pack "1234")) []
                       
-- This is a dummy datatype only to show that this works
-- It will be removed and never used
-- unsafePerformIO is there just because it's simpler and
-- this will not be part of tersus
dummyApp = TApplication (T.pack "emacs") (T.pack "identifier") (T.pack "description dummy") (Just (T.pack "url")) (T.pack "mail@place.com") (unsafePerformIO getCurrentTime)  (T.pack "appkey")

dummyMsg = TMessage dummyUser dummyUser dummyApp dummyApp (T.pack "Alonso") (unsafePerformIO getCurrentTime)

dummyAddress = Address dummyUser dummyApp
