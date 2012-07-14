{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module Model.TMessage where

import Import
import Data.Aeson as J
import Data.Text as T
import Model.User ()
import Model.TApplication ()

-- Instance to convert a message into it's json representation, this instance is defined
-- according to the Tersus Developers Api.
instance ToJSON TMessage where
         toJSON (TMessage sender receiver senderApp receiverApp msgBody timestamp) = J.object [
                                                                                      ("userSender",toJSON sender),
                                                                                      ("userReceiver",toJSON receiver),
                                                                                      ("appSender",toJSON senderApp),
                                                                                      ("appReceiver",toJSON receiverApp),
                                                                                      ("content",String msgBody)
                                                                                     ]
