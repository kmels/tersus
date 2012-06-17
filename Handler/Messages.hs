{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Handler.Messages where

--Author: Ernesto Rodriguez

--Description: Functions to handle the messaging system of Tersus.

import           Control.Concurrent (MVar, isEmptyMVar, modifyMVar_, newEmptyMVar, putMVar, takeMVar)
import           Data.Aeson         as D
import           Data.HashTable     as H
import           Data.Text          as T
import           Data.Time.Clock    (getCurrentTime)
import           Import
import           Model
import           Model.TMessage
import           Model.TersusResult
import           System.IO.Unsafe   (unsafePerformIO)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import TersusCluster.Types
import Data.Maybe (fromJust)


-- Deliver a message, block until it's delivery
-- status is known
deliverTMessage message = do 
                        master <- getYesod
                        channel <- return $ getSendChannel master
                        pid <- return $ getProcessId master
                        statusTable <- return $ getStatusTable master
                        status <- liftIO $ queueMessage statusTable channel (message,pid)
                        return $ liftIO $ takeMVar status

-- Add a message to the message queue channel
queueMessage :: TMessageStatusTable -> TMessageQueue -> TMessageEnvelope -> IO (MVar MessageResult)
queueMessage statusTable channel (message,pid) = do                            
                              writeChan channel (message,pid)
                              status <- newEmptyMVar
                              H.insert statusTable (generateHash message) status
                              return status
                              


-- Get a list of all messages, block until at least one message has arrived
-- Note that a pattern match failure should not happe. If it happens it means
-- that the app key generaiton method is not secure
-- receieveMessage :: Address -> IO [TMessage]
receiveMessages address = do
                master <- getYesod
                mailBoxes <- return $ getMailBoxes master                
                deliveryChan <- return $ getDeliveryChannel master
                mailBox <- liftIO (H.lookup mailBoxes (getAppInstance address) >>= return . fromJust)
                messages <- liftIO $ takeMVar mailBox
                liftIO $ mapM_ (writeChan deliveryChan) messages
                return messages

-- Dummy test functions, will not exist in future releases
getInitMVarR = do
            -- master <- getYesod
            -- liftIO $ createMailbox master $ Address dummyUser dummyApp
            jsonToRepJson $ encode $ TersusResult 3 Import.Success

getMessageR :: Handler RepJson
getMessageR = do
            -- master <- getYesod
            -- Just msgs <- liftIO $ retrieveMessages master $ Address dummyUser dummyApp
            jsonToRepJson $ encode ("tmp resp" :: String) --msgs

getSetMVarR :: Handler RepJson
getSetMVarR = do
            -- master <- getYesod
            -- liftIO $ writeMessage master dummyMsg
            jsonToRepJson $ encode $ TersusResult 3 Import.Success
