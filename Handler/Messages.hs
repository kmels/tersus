{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Handler.Messages where

--Author: Ernesto Rodriguez

--Description: Functions to handle the messaging system of Tersus.

import           Blaze.ByteString.Builder     (fromLazyByteString)
import           Control.Concurrent.STM       (atomically, modifyTVar,STM)
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TMVar
import           Data.Aeson                   as D
import           Data.Array.MArray
import           Data.HashTable               as H
import           Data.Maybe                   (fromJust)
import           Data.Text                    as T
import           Data.Text.Lazy               (fromChunks)
import           Data.Text.Lazy.Encoding      (encodeUtf8)
import           Data.Time.Clock              (getCurrentTime)
import           Import
import           System.Timeout               (timeout)
import           Tersus.AccessKeys            (decompose)
import           Tersus.Cluster.Types
import Tersus.DataTypes.Messaging
import Network.Wai.EventSource (ServerEvent (..), eventSourceAppIO)

-- | Creates an appInstance envoiernment and registers that
-- envoiernment with the provided appInstance
initApplication :: AppInstance -> GHandler sub App ()
initApplication appInstance = do
  master <- getYesod
  let
    nChannel = getNotificationsChannel master
    appEnvs = getAppEnvs master
  liftIO $ newAppInstanceEnv >>= H.insert appEnvs appInstance
  liftIO $ atomically $ writeTChan nChannel (Initialized' appInstance)
  return ()


-- | Attempt to decrypt the access key of an AuthMessage and convert
  -- It to a TMessage for delivery. If decryption fails return an
  -- invalid app key error
deliverAuthMessage :: AuthMessage -> GHandler sub App MessageResult
deliverAuthMessage (AuthMessage accessKey rUser appId body) = deliverTMessage' $ decompose accessKey
  where
    deliverTMessage' Nothing = return EInvalidAppKey
    deliverTMessage' (Just (sUsername,sAppId)) = do
      currTime <- liftIO $ getCurrentTime
      deliverTMessage $ TMessage sUsername rUser sAppId appId body currTime


-- | Deliver a message, block until it's delivery
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
  appEnvs <- return $ getAppEnvs master
  msgBuffer <- liftIO $ getMessageBuffer appEnvs (getSendAppInstance message)
  writeMsgInBuffer channel port  msgBuffer

  where
    writeMsgInBuffer _ _ Nothing = return EInvalidAppKey
    writeMsgInBuffer channel port (Just msgBuffer) = do
      _ <- liftIO $ queueMessage msgBuffer channel (message,port)
      let
        appInstance = getSendAppInstance message
      waitMessage appInstance (generateHash message)

-- | Given an appInstance and the hashcode of a particular message,
-- blocks until the delivery status of the message is known
waitMessage :: AppInstance -> THashCode -> GHandler sub App MessageResult
waitMessage appInstance hashCode = do
  master <- getYesod
  let
    appEnvs = getAppEnvs master
  msgBuffer <- liftIO $ getMessageBuffer appEnvs appInstance
  liftIO $ hashCodeLookup msgBuffer

  where
    rmHash hash ((hash',msgIndex):t)
           | hash == hash' = t
           | otherwise = (hash',msgIndex) : (rmHash hash t)
    rmHash _ _ = []
    -- Lookup the hashcode in the message buffer if such
    -- buffer could be obtained from the app key
    hashCodeLookup Nothing = return EInvalidAppKey
    hashCodeLookup (Just (mappings,statusVars,availableBuff)) = let
      readMsgStatus Nothing = return EInvalidHashCode
      readMsgStatus (Just (_,msgIndex)) = do
        statusVar <- readArray statusVars msgIndex
        atomically $ do
          result <- takeTMVar statusVar
          modifyTVar mappings (rmHash hashCode)
          writeTChan availableBuff (msgIndex,statusVar)
          return result
      in
       lookupIndex hashCode mappings >>= readMsgStatus


-- | Add a message to the privided message queue channel. Create a
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


receiveMessageTimeout :: Int
receiveMessageTimeout = 30 * 1000000 -- 30 Seconds

-- | Load all the messages from a mailbox and acknowledge them
-- on the given delivery channel
loadMessages :: TChan TMessageEnvelope -> TChan TMessageEnvelope -> STM [TMessageEnvelope]
loadMessages mailBox deliveryChan = do
  msg <- readTChan mailBox
  rest <- loadAll
  mapM_ (\m -> writeTChan deliveryChan m) (msg:rest)
  return (msg:rest)

  where
    loadAll = isEmptyTChan mailBox >>= \isEmpty -> case isEmpty of
      True -> return []
      False -> do
        msg <- readTChan mailBox
        otherMsgs <- loadAll
        return $ (msg:otherMsgs)

-- | Get a list of all messages, block until at least one message has arrived
-- Note that a pattern match failure should not happen. If it happens it means
-- that the app key generaiton method is not secure
-- receieveMessage :: Address -> IO [TMessage]
receiveMessages :: AppInstance -> GHandler sub App (Maybe [TMessageEnvelope])
receiveMessages appInstance = do
  master <- getYesod
  let
    appEnvs = getAppEnvs master
    deliveryChan = getDeliveryChannel master
  -- Todo: what happens if the appInstance dosent exist? What should be returned?
  mailBox <- liftIO (getMailBox appEnvs appInstance >>= return . fromJust)
  messages <- liftIO $ timeout receiveMessageTimeout $ atomically $ loadMessages mailBox deliveryChan
  return messages

authMessagesPostParam :: Text
authMessagesPostParam = "messages"

appKeyGet :: Text
appKeyGet = "appkey"

-- | Send a batch of messages to tersus applications.
-- The messages are received through post and are decoded using Aeson
postSendAuthMessagesR :: Handler RepJson
postSendAuthMessagesR = do
  msgs <- lookupPostParam authMessagesPostParam
  -- Try to decode a maybe text. Must be converted to a lazy bytestring beforehand
  case msgs >>= decode . encodeUtf8 . fromChunks . return of
    Just msgs' -> mapM deliverAuthMessage msgs' >>= jsonToRepJson . show
    Nothing -> jsonToRepJson $ show InvalidMsgFormat

-- | Try to decrypt the given encryption key given as a get parameter,
-- return the user and app or nothing if decryption fails
receiveMessageAuth :: Handler (Maybe (Username,ApplicationIdentifier))
receiveMessageAuth = do
  key <- lookupGetParam appKeyGet
  return $ key >>= decompose

-- | Create a server event from a MailBox which also acknowledges
-- the messages as received in the given message queue
messagesEventSource :: MailBox -> TMessageQueue -> IO ServerEvent
messagesEventSource mailBox deliveryChan = do
  msgs' <- atomically $ loadMessages mailBox deliveryChan
  let
    msgs = Import.map (\(m,_) -> m) msgs'
  return $ ServerEvent Nothing Nothing $ return $ fromLazyByteString $ encode msgs

-- | Used to create a channel that lets an application receive messages
-- using EventSources
createEventPipe :: Addressable a => a -> Handler ()
createEventPipe appInstance = do
  master <- getYesod
  let
    appEnvs = getAppEnvs master
    deliveryChan = getDeliveryChannel master
  maybeMailBox <- liftIO $ getMailBox appEnvs appInstance
  case maybeMailBox of
    Nothing -> return ()
    Just mailBox -> do
      nMailBox <- liftIO $ atomically $ dupTChan mailBox
      req <- waiRequest
      res <- lift $ eventSourceAppIO (messagesEventSource nMailBox deliveryChan) req
      sendWaiResponse res
      

-- | Request to receive messages using Event Sources. Event sources are
-- only available in html5 so this function only works for powerful browsers
getReceiveMessagesEventR :: Handler ()
getReceiveMessagesEventR = do
  key <- receiveMessageAuth
  case key of
    Just (appUsername,appIdentifier) -> createEventPipe $ AppInstance appUsername appIdentifier
    Nothing -> return ()
  
-- | Request to load all the messages sent to a particular app instance
getReceiveMessagesR :: Handler RepJson
getReceiveMessagesR = do
  key <- receiveMessageAuth
  case key of
    Just (appUsername,appIdentifier) -> receiveMessages' $ AppInstance appUsername appIdentifier
    Nothing -> jsonToRepJson $ show EInvalidAppKey

  where
    receiveMessages' appInstance = do
      msgs <- receiveMessages appInstance
      case msgs >>= return . (Import.map (\(msg,_) -> msg)) of
        Just (msgs') -> jsonToRepJson msgs'
        Nothing -> jsonToRepJson $ show MsgTimeout
