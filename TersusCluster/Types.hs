module TersusCluster.Types where

import Prelude
import Remote
import Data.HashTable
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Model
import Data.Hash.MD5
import Data.Text as T

type THashCode = String

-- Hash code was introduced to handle uncoordinated registration
-- and un-registration of User/App Instances, for example
-- if we get two register requests and one un-register request
-- the app should remain registered
type TersusProcessM = (ProcessId,THashCode)

-- type TMessageChannel = TChan (TMessage, TMVar (MessageResult))
type TMessageEnvelope = (TMessage,ProcessId)
type AddressTable = HashTable (AppInstance) TersusProcessM
type ActionsChannel = Chan (AppInstanceActions)
type MailBoxTable = HashTable AppInstance (MVar [TMessageEnvelope])
type TMessageStatusTable = HashTable THashCode (MVar (MessageResult))
type TMessageQueue = Chan TMessageEnvelope

class Hashable a where
      generateHash :: a -> THashCode

instance Hashable TMessage where
         generateHash msg = let
                      TMessage u1 u2 a1 a2 body time= msg
                      User un1 _ _ = u1
                      User un2 _ _ = u2
                      TApplication _ id1 _ _ _ _ _ = a1
                      TApplication _ id2 _ _ _ _ _ = a1
                      in
                        md5s $ Str ((show time) ++ (T.unpack $ T.concat [un1,un2,id1,id2,body]))
