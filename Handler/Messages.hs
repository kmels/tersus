{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Handler.Messages where

--Author: Ernesto Rodriguez

--Description: Functions to handle the messaging system of Tersus.

import           Blaze.ByteString.Builder     (fromLazyByteString)
import           Control.Distributed.Process.Binder (newChan,ProcessBinder)
import           Control.Distributed.Process  (sendChan,receiveChan,receiveChanTimeout)
import qualified Control.Distributed.Process.Node as N
import           Data.Aeson                   as D
import           Data.Maybe                   (fromMaybe)
import           Data.Text                    as T
import           Data.Text.Lazy               (fromChunks)
import           Data.Text.Lazy.Encoding      (encodeUtf8)
import           Data.Time.Clock              (getCurrentTime)
import           Import
import           System.Timeout               (timeout)
import           Tersus.AccessKeys            (decompose)
import           Tersus.Cluster.Types
import Tersus.DataTypes.Messaging
import Tersus.DataTypes.TypeSynonyms
import Tersus.Cluster.MessageFrontend
import Network.Wai.EventSource (ServerEvent (..), eventSourceAppIO)

-- | Creates an appInstance envoiernment and registers that
-- envoiernment with the provided appInstance
initApplication :: AppInstance -> GHandler sub Tersus ()
initApplication appInstance = do
  master <- getYesod
  let
    pb = processRunner master
  (msgSend,msgRecv) <- liftIO $ newChan pb
  (ackSend,ackRecv) <- liftIO $ newChan pb
  let
    sendChannels = MessageSendChannels{messageSendPort=msgSend,acknowledgementSendPort=ackSend,hashCode="someHashCode"}
    recvChannels = MessageRecvChannels{messageRecvPort=msgRecv,acknowledgementRecvPort=ackRecv}
    -- Todo add a real hash code
    initNotification = Initialized appInstance (msgSend,ackSend,"hashPelado")
    
  insertSendChannels appInstance sendChannels
  insertRecvChannels appInstance recvChannels
  broadcastNotifications [initNotification]
  return ()


-- | Attempt to decrypt the access key of an AuthMessage and convert
  -- It to a TMessage for delivery. If decryption fails return an
  -- invalid app key error
deliverAuthMessage :: AuthMessage -> GHandler sub Tersus TMessageResponse
deliverAuthMessage (AuthMessage accessKey rUser appId body) = deliverTMessage' $ decompose accessKey
  where
    deliverTMessage' Nothing = return $ InvalidAppKey accessKey
    deliverTMessage' (Just (sUsername,sAppId)) = do
      currTime <- liftIO $ getCurrentTime
      deliverTMessage $ TMessage sUsername rUser sAppId appId body currTime


-- | Deliver a message, return the result of attempting to queue
-- the message in the cloud haskell channels
deliverTMessage :: TMessage -> GHandler sub Tersus TMessageResponse
deliverTMessage message = do
  sendChannels <- getSendChannels (getAppInstance message)
  case sendChannels of
    Just sendChannels' -> do
      runProcess $ sendChan (messageSendPort sendChannels') (message,acknowledgementSendPort sendChannels')
      return $ MsgQueued $ generateHash message
    Nothing -> return $ NoAppInstance $ getAppInstance message
      

receiveTimeout :: Int
receiveTimeout = 1000

-- | Get a list of all messages, block until at least one message has arrived
-- Note that a pattern match failure should not happen. If it happens it means
-- that the app key generaiton method is not secure
-- receieveMessage :: Address -> IO [TMessage]
receiveMessages :: AppInstance -> GHandler sub Tersus (Maybe [TMessageEnvelope])
receiveMessages appInstance = do
  recvChannels <- getRecvChannels appInstance
  case recvChannels of
    Just recvChannels' -> receiveMessages'' (messageRecvPort recvChannels')
    Nothing -> return Nothing
    
  where
    receiveMessages'' chan = let
      receiveMessages' (Just msg) = do
        next <- runProcess $ receiveChanTimeout receiveTimeout chan
        rest <- receiveMessages' next
        return (msg:rest)
      receiveMessages' Nothing = return []
      in
       runProcess (receiveChan chan) >>= receiveMessages'.Just >>= return . Just
  
  
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
    Nothing -> jsonToRepJson $ InvalidFormat "Parse error."

-- | Try to decrypt the given encryption key given as a get parameter,
-- return the user and app or nothing if decryption fails
receiveMessageAuth :: Handler (Maybe (Username,ApplicationIdentifier))
receiveMessageAuth = do
  key <- lookupGetParam appKeyGet
  return $ key >>= decompose

messageEventSource :: ProcessBinder -> MessageRecvPort -> IO ServerEvent
messageEventSource tersusNode msgRecvPort = do
  msgs <- runProcessIO tersusNode $ do
    fst <- receiveChan msgRecvPort
    rest <- receiveChanCond
    return $ fst:rest
  return $ ServerEvent Nothing Nothing $ return $ fromLazyByteString $ encode $ Import.map (\(m,_)->m) msgs
  
  where
    receiveChanCond = do
      mNext <- receiveChanTimeout receiveTimeout msgRecvPort
      case mNext of
        Nothing -> return []
        Just next -> receiveChanCond >>= return . (next:)

-- | Used to create a channel that lets an application receive messages
-- using EventSources
createEventPipe :: Addressable a => a -> Handler ()
createEventPipe appInstance = do
  master <- getYesod
  recvChannels <- getRecvChannels appInstance
  case recvChannels of
    Just recvChannels' -> do
      req <- waiRequest
      res <- lift $ eventSourceAppIO (messageEventSource (processRunner master) (messageRecvPort recvChannels')) req
      sendWaiResponse res
    Nothing -> return ()
      

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
  accessKey <- lookupGetParam appKeyGet
  key <- receiveMessageAuth
  case key of
    Just (appUsername,appIdentifier) -> receiveMessages' $ AppInstance appUsername appIdentifier
    Nothing -> jsonToRepJson $ InvalidAppKey $ fromMaybe "" accessKey

  where
    receiveMessages' appInstance = do
      msgs <- receiveMessages appInstance
      case msgs >>= return . (Import.map (\(msg,_) -> msg)) of
        Just (msgs') -> jsonToRepJson msgs'
        Nothing -> jsonToRepJson $ show MsgTimeout
