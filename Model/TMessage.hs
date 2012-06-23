module Model.TMessage where

import Import
import Data.Aeson as J
import Control.Monad

-- Instance to convert a message into it's json representation
instance ToJSON TMessage where
         toJSON (TMessage sender receiver senderApp receiverApp msgBody timestamp) = (J.String msgBody)
