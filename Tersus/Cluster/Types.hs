{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes         #-}
module Tersus.Cluster.Types where

import           Control.Distributed.Process
import           Control.Concurrent.STM (TVar)
import           Data.Array.IO
import qualified Data.Binary                  as B
import           Data.Hash.MD5
import           Data.HashTable.IO            as HT
import qualified Data.List                    as L
import           Data.Text                    as T
import           Data.Typeable.Internal       (Typeable)
import           Prelude
import Tersus.DataTypes
import           Yesod

type THashCode = String

-- Hash code was introduced to handle uncoordinated registration
-- and un-registration of User/App Instances, for example
-- if we get two register requests and one un-register request
-- the app should remain registered

-- Envelope for sending message results. The
-- hash code of this envelope is the hash code
-- produced from the messages for which this result is sent.
-- This hash code can be used to determine the message that
-- should be acknowledge with the MessageResult
type MessageResultEnvelope = (THashCode,MessageResult,AppInstance)


-- The send/receive port of the channel for message acknowledgements
type AcknowledgementSendPort = SendPort MessageResultEnvelope
type AcknowledgementRecvPort = ReceivePort MessageResultEnvelope
type AcknowledgementPorts = (AcknowledgementSendPort,AcknowledgementRecvPort)


-- Contains a Tersus Messages packed with the port to which
-- acknowledgements for this message shuld be sent
type TMessageEnvelope = (TMessage,AcknowledgementSendPort)


-- The send/receive port of the channel for messages
type MessageSendPort = SendPort TMessageEnvelope
type MessageRecvPort = ReceivePort TMessageEnvelope
type MessagingPorts = (MessageSendPort,MessageRecvPort)

data MessageSendChannels = MessageSendChannels{
  messageSendPort :: MessageSendPort,
  acknowledgementSendPort :: AcknowledgementSendPort,
  hashCode :: THashCode
  }
                           
data MessageRecvChannels = MessageRecvChannels{
  messageRecvPort :: MessageRecvPort,  
  acknowledgementRecvPort :: AcknowledgementRecvPort 
}

-- This type represents the address of a TersusCluster for
-- message delivery. Also has a hash code which will be used
-- to determine how to register and unregister addresses
type TersusProcessM = (SendPort TMessageEnvelope,THashCode)

-- | HashTable indexed by appInstances that contains all
--  known AppInstances with their address
type SendAddressTable = HT.BasicHashTable (AppInstance) MessageSendChannels

-- | Hash table that contains all the channels to receive messages
-- for the appInstances running in the current server
type RecvAddressTable = HT.BasicHashTable (AppInstance) MessageRecvChannels

-- Represents the datatypse for which a hashcode can be computed
class Hashable a where
      generateHash :: a -> THashCode

-- Implementation of the hashcode generation mechanisms of
-- TersusMessages
instance Hashable TMessage where
    generateHash msg = md5s $ Str ((show msgTime) ++ (T.unpack $ T.concat [un1,un2,id1,id2,body]))
        where
          TMessage un1 un2 id1 id2 body msgTime = msg

type NotificationsSendPort = SendPort [TersusNotification]
type NotificationsRecvPort = ReceivePort [TersusNotification]
type NotificationsPorts = (NotificationsSendPort,NotificationsRecvPort)

-- | Notifications about the ongoing activity in Tersus Application instances
-- this usually means an application is started and an application is stopped
-- since is possible that an application is started on a server, then stopped
-- then started on another server before all theese notifications are dispatched
-- to all tersus clusters (althogh unlikely) we add a random hash code which
-- will be used to match the notifications
-- Note that this type is used by Cloud Haskell for communication since it
-- has information which is abstracted away to Yesod, the
-- Yesod side user the TersusSimpleNotification
data TersusNotification = Initialized AppInstance (MessageSendPort,AcknowledgementSendPort,THashCode)
                        | Closed (AppInstance,THashCode)
                        | NotificationUnknown deriving (Typeable)


-- The Application Instance notifications for activities that they
-- undergo. Usually to indicate an app was initialized by a
-- user or stopped. This datatype is used in the Yesod
-- side since it's much simpler than the TersusNotification which
-- is used by CloudHaskell
data TersusSimpleNotification = Initialized' AppInstance
                              | Closed' AppInstance deriving Show

-- Mutable variable that holds a list of all Tersus instances
-- to whom the registration of a new app instance should
-- be informed.
type TersusClusterList = TVar [(NotificationsSendPort,ProcessId)]

-- | Time that will be used for the getPeers Function
peerSearch :: Int
peerSearch = 5000

tersusClusterRole :: String
tersusClusterRole = "T1"

-- Binary instance for Tersus notification so
-- the notifications can be sent throughout Cloud Haskell
-- Please use positive numbers
instance B.Binary TersusNotification where
    put (Initialized appInstance (msgSendPort,ackPort,hash)) = B.put (1 :: Int) >> B.put (appInstance,(msgSendPort,ackPort,hash))
    put (Closed (appInstance,hash)) = B.put (2 :: Int) >> B.put (appInstance,hash)
    put NotificationUnknown = B.put (-1 :: Int) -- No reason to be sent, but will be matched anyway

    get = do
      notificationNum <- (B.get :: B.Get Int)
      case notificationNum of
        1 -> B.get >>= \(appInstance,(msgSendPort,ackPort,hash)) -> return $ Initialized appInstance (msgSendPort,ackPort,hash)
        2 -> B.get >>= \(appInstance,hash) -> return $ Closed (appInstance,hash)
        _ -> return NotificationUnknown
