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
import Data.Array.MArray
import Data.Array.IO
import qualified Data.List as L
import Data.Typeable.Internal (Typeable)
import Data.Binary (Binary)

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

-- Channel that will be used to communicate what is happening with
-- the AppInstances
type ActionsChannel = TChan (AppInstanceActions)

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
                      TMessage u1 u2 a1 a2 body time= msg
                      User un1 _ _ = u1
                      User un2 _ _ = u2
                      TApplication _ id1 _ _ _ _ _ = a1
                      TApplication _ id2 _ _ _ _ _ = a1
                      in
                        md5s $ Str ((show time) ++ (T.unpack $ T.concat [un1,un2,id1,id2,body]))

lookupIndex :: THashCode -> TVar [(THashCode,Int)] -> IO (Maybe (THashCode,Int))
lookupIndex hashCode mappings = do
  mapping <- atomically $ readTVar mappings :: IO [(THashCode,Int)]
  return $ L.find (\(h,_) -> h == hashCode) mapping
