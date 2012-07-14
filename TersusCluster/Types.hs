{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
module TersusCluster.Types where

import Prelude
import Remote
import Data.HashTable
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TMVar
import Model
import Data.Hash.MD5
import Data.Text as T
import Control.Concurrent.STM.TVar (readTVar,TVar)
import Control.Concurrent.STM (atomically)
import Data.Array.IO
import qualified Data.List as L
import qualified Data.Binary as B
import Data.Typeable.Internal (Typeable)

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



-- This type represents the address of a TersusCluster for
-- message delivery. Also has a hash code which will be used
-- to determine how to register and unregister addresses
type TersusProcessM = (SendPort TMessageEnvelope,THashCode)

-- HashTable indexed by appInstances that contains all
-- known AppInstances with their address
type AddressTable = HashTable (AppInstance) TersusProcessM

-- Contains all the Mailboxes for the AppInstances running in this
-- TersusCluster. 
type MailBoxTable = HashTable AppInstance (TMVar [TMessageEnvelope])

-- Contains a table where the status of each message send from
-- AppInstances of this server are written once they are
-- acknowledge by the target TersusCluster
type TMessageSendBuff = (TVar [(THashCode,Int)], IOArray Int (TMVar MessageResult), TChan (Int,TMVar MessageResult))
type TMessageStatusTable = HashTable AppInstance TMessageSendBuff


type TMessageQueue = TChan TMessageEnvelope
type AcknowledgementQueue = TChan (TMessageEnvelope,AppInstance)

-- Represents the datatypse for which a hashcode can be computed
class Hashable a where
      generateHash :: a -> THashCode

-- Implementation of the hashcode generation mechanisms of 
-- TersusMessages
instance Hashable TMessage where
         generateHash msg = let
                      TMessage u1 u2 a1 a2 body msgTime= msg
                      User un1 _ _ = u1
                      User un2 _ _ = u2
                      TApplication _ id1 _ _ _ _ _ = a1
                      TApplication _ id2 _ _ _ _ _ = a2
                      in
                        md5s $ Str ((show msgTime) ++ (T.unpack $ T.concat [un1,un2,id1,id2,body]))

lookupIndex :: THashCode -> TVar [(THashCode,Int)] -> IO (Maybe (THashCode,Int))
lookupIndex hashCode mappings = do
  mapping <- atomically $ readTVar mappings :: IO [(THashCode,Int)]
  return $ L.find (\(h,_) -> h == hashCode) mapping


type NotificationsSendPort = SendPort [TersusNotification]
type NotificationsRecvPort = ReceivePort [TersusNotification]
type NotificationsPorts = (NotificationsSendPort,NotificationsRecvPort)

-- Notifications about the ongoing activity in Tersus Application instances
-- this usually means an application is started and an application is stopped
-- since is possible that an application is started on a server, then stopped
-- then started on another server before all theese notifications are dispatched
-- to all tersus clusters (althogh unlikely) we add a random hash code which
-- will be used to match the notifications
-- Note that this type is used by Cloud Haskell for communication since it
-- has information which is abstracted away to Yesod, the
-- Yesod side user the TersusSimpleNotification
data TersusNotification = Initialized AppInstance (MessageSendPort,THashCode)
                        | Closed (AppInstance,THashCode)
                        | NotificationUnknown deriving (Typeable)


-- The Application Instance notifications for activities that they
-- undergo. Usually to indicate an app was initialized by a 
-- user or stopped. This datatype is used in the Yesod
-- side since it's much simpler than the TersusNotification which
-- is used by CloudHaskell
data TersusSimpleNotification = Initialized' AppInstance
                              | Closed' AppInstance


-- Channel that will be used to communicate what is happening with
-- the AppInstances
type NotificationsChannel = TChan TersusSimpleNotification


type TersusClusterList = TVar [NotificationsSendPort]

-- Binary instance for Tersus notification so
-- the notifications can be sent throughout Cloud Haskell
instance B.Binary TersusNotification where
    put (Initialized appInstance (msgSendPort,hash)) = B.put (1 :: Int) >> B.put (appInstance,(msgSendPort,hash))
    put (Closed (appInstance,hash)) = B.put (2 :: Int) >> B.put (appInstance,hash)
                                            
    get = do
      notificationNum <- (B.get :: B.Get Int)
      case notificationNum of
        1 -> B.get >>= \(appInstance,(msgSendPort,hash)) -> return $ Initialized appInstance (msgSendPort,hash)
        2 -> B.get >>= \(appInstance,hash) -> return $ Closed (appInstance,hash)
        _ -> return NotificationUnknown